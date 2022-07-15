module T = Types

type level =
  | TOPLEVEL
  | NONTOP of {uniq: unit ref; parent: level; frame: Frame.frame}
[@@deriving show]

type access = level * Frame.access
[@@deriving show]

type exp =
  | Ex of Ir.exp
  | Nx of Ir.stm
  | Cx of ((Temp.label * Temp.label) -> Ir.stm)
[@@deriving show]

let fraglist : Frame.frag list ref = ref []
;;

let outermost = TOPLEVEL
;;

let nil = Ex(Ir.CONST 0)
;;

let newlevel (parent, name, formals) = NONTOP({uniq = ref (); parent=parent; frame=Frame.newframe(name, true::formals)})
;;

let formals = function
  | TOPLEVEL -> []
  | NONTOP{uniq; parent; frame} ->
    let addlevel acc faccess = (NONTOP{uniq=uniq; parent=parent; frame=frame}, faccess)::acc
    in List.fold_left addlevel [] (Frame.formals(frame))
;;

let alloclocal (level, escape) =
  match level with
  | NONTOP{uniq; parent; frame} -> (NONTOP{uniq=uniq; parent=parent; frame=frame}, Frame.alloclocal frame escape)
  | TOPLEVEL -> (print_endline "impossible"; (outermost, Frame.alloclocal (Frame.newframe(Temp.newlabel(), [])) escape))
;;

let unEx = function
  | Ex e -> e
  | Cx genstm ->
    let r = Temp.newtemp() in
    let t = Temp.newlabel() in
    let f = Temp.newlabel() in
    Ir.ESEQ(
      Ir.SEQ[
        Ir.MOVE(Ir.TEMPLOC r, Ir.CONST 1);
        genstm(t, f);
        Ir.LABEL f;
        Ir.MOVE(Ir.TEMPLOC r, Ir.CONST 0);
        Ir.LABEL t;
      ],
      Ir.TEMP r
    )
  | Nx s -> Ir.ESEQ(s, Ir.CONST 0)
;;

let unCx = function
  | Cx c -> c
  | Ex (Ir.CONST 0) -> (fun (_, flabel) -> Ir.JUMP(Ir.NAME(flabel), [flabel]))
  | Ex (Ir.CONST 1) -> (fun (tlabel, _) -> Ir.JUMP(Ir.NAME(tlabel), [tlabel]))
  | Ex e -> (fun (tlabel, flabel) -> Ir.CJUMP(Ir.EQ, Ir.CONST 1, e, tlabel, flabel))
  | Nx _ -> (print_endline "compiler error: unCx an Nx"; (fun (_, _) -> Ir.LABEL(Temp.newlabel())))
;;

let rec unNx = function
  | Ex e -> Ir.EXP(e)
  | Nx n -> n
  | c -> unNx(Ex(unEx(c)))
;;

let rec follow_staticlinks = function
  | (TOPLEVEL, TOPLEVEL, bestguess) -> (print_endline "following static links failed"; bestguess)
  | (TOPLEVEL, _, bestguess) -> (print_endline "following static links failed"; bestguess)
  | (_, TOPLEVEL, bestguess) -> (print_endline "following static links failed"; bestguess)
  | ((NONTOP{uniq=uniqdec; parent=_; frame=_} as declevel), (NONTOP{uniq=uniquse; parent=useparent; frame=_}), bestguess) ->
    if uniqdec = uniquse
    then bestguess
    else follow_staticlinks (declevel, useparent, Ir.MEM(bestguess))
;;

let simpleVarIR ((declevel, fraccess), uselevel) = Ex(Frame.exp (fraccess, follow_staticlinks(declevel, uselevel, Ir.TEMP(Frame.fp))))
;;

let binopIR (binop, left, right) = Ex(Ir.BINOP(binop, unEx(left), unEx(right)))
;;

let relopIR (relop, left, right, ty) =
  match (ty,relop) with
  | (T.STRING, Ir.EQ) -> Ex(Frame.external_call("stringEqual", [unEx left; unEx right]))
  | (T.STRING, Ir.LE) -> Ex(Frame.external_call("stringLE", [unEx left; unEx right]))
  | (T.STRING, Ir.LT) -> Ex(Frame.external_call("stringLT", [unEx left; unEx right]))
  | (T.STRING, Ir.GE) -> Ex(Frame.external_call("stringGE", [unEx left; unEx right]))
  | (T.STRING, Ir.GT) -> Ex(Frame.external_call("stringGT", [unEx left; unEx right]))
  | _                 -> Cx(fun (t, f) -> Ir.CJUMP(relop, unEx left, unEx right, t, f))
;;

let ifIR (test, then', else') =
  let genstm = unCx(test) in
  let e2 = unEx(then') in
  let e3 = unEx(else') in
  let resulttemp = Temp.newtemp() in
  let t = Temp.newlabel() in
  let f = Temp.newlabel() in
  let join = Temp.newlabel() in
  Ex(Ir.ESEQ(
    Ir.SEQ[
      genstm(t, f);
      Ir.LABEL t;
      Ir.MOVE(Ir.TEMPLOC resulttemp, e2);
      Ir.JUMP(Ir.NAME join, [join]);
      Ir.LABEL f;
      Ir.MOVE(Ir.TEMPLOC resulttemp, e3);
      Ir.JUMP(Ir.NAME join, [join]);
    ],
    Ir.TEMP(resulttemp)
  ))
;;

let exp_to_loc = function
  | Ir.MEM exp -> Ir.MEMLOC exp
  | Ir.TEMP temp -> Ir.TEMPLOC temp
  | Ir.TODO -> (print_endline "todo"; Ir.TEMPLOC(Temp.newtemp()))
  | Ir.ESEQ(stm, exp) -> Ir.ESEQLOC(stm, exp)
  | _ -> (print_endline "can't convert exp to loc"; Ir.TEMPLOC(Temp.newtemp()))
;;

let assignIR (left, right) = Nx(Ir.MOVE(exp_to_loc(unEx left), unEx right))
;;

let whileIR (test, body, breaklabel) =
  let testlabel = Temp.newlabel() in
  let bodylabel = Temp.newlabel() in
  let test = unCx test in
  let body = unNx body in
  Nx(Ir.SEQ[
    Ir.LABEL testlabel;
    test (bodylabel, breaklabel);
    Ir.LABEL bodylabel;
    body;
    Ir.JUMP(Ir.NAME testlabel, [testlabel]);
    Ir.LABEL breaklabel;
  ])
;;

let breakIR breaklabel = Nx(Ir.JUMP(Ir.NAME breaklabel, [breaklabel]))
;;

let forIR (varEx, _, loEx, hiEx, bodyNx, breaklabel) =
  let var = unEx varEx in
  let lo = unEx loEx in
  let hi = unEx hiEx in
  let body = unNx bodyNx in
  let bodylabel = Temp.newlabel() in
  let updatelabel = Temp.newlabel() in
  Nx(Ir.SEQ[
    Ir.MOVE(exp_to_loc var, lo);
    Ir.CJUMP(Ir.LE, var, hi, bodylabel, breaklabel);
    Ir.LABEL bodylabel;
    body;
    Ir.CJUMP(Ir.LT, var, hi, updatelabel, breaklabel);
    Ir.LABEL updatelabel;
    Ir.MOVE(exp_to_loc var, Ir.BINOP(Ir.PLUS, var, Ir.CONST 1));
    Ir.JUMP(Ir.NAME bodylabel, [bodylabel]);
    Ir.LABEL breaklabel;
  ])
;;

let arrayIR (sizeEx, initEx) = Ex(Frame.external_call("initArray", [unEx sizeEx; unEx initEx]))
;;

let subscriptIR (arrEx, indexEx) =
  let addr = Temp.newtemp() in
  let arr = unEx arrEx in
  let index = unEx indexEx in
  Ex(Ir.ESEQ(
    Ir.MOVE(Ir.TEMPLOC addr,
            Ir.BINOP(Ir.PLUS,
                     arr,
                     Ir.BINOP(Ir.MUL, index, Ir.CONST(Frame.wordsize)))),
    Ir.MEM(Ir.TEMP addr)
  ))
;;

let fieldIR (nameEx, elem) = Ex(Ir.MEM(Ir.BINOP(Ir.PLUS, unEx nameEx, Ir.BINOP(Ir.MUL, Ir.CONST elem, Ir.CONST(Frame.wordsize)))))
;;

let nilIR () = Ex(Ir.CONST 0)
;;

let intIR n = Ex(Ir.CONST n)
;;

let callexpIR = function
  | (TOPLEVEL, _, _, _) -> Ex(Ir.TEMP(Frame.fp))
  | (NONTOP{uniq=_; parent=parent; frame=_}, calllevel, label, args) ->
    let sl = follow_staticlinks(parent, calllevel, Ir.TEMP(Frame.fp)) in
    let unExedArgs = List.map unEx args in
    Ex(Ir.CALL(Ir.NAME label, sl::unExedArgs))
;;

let rec sequencingIR = function
  | [] -> Ex(Ir.CONST 0)
  | [exp] -> exp
  | h::t -> Ex(Ir.ESEQ(unNx h, unEx (sequencingIR t)))
;;

let recordIR exps =
  let n = List.length exps in
  let r = Temp.newtemp() in
  let recordInit = Ir.MOVE(Ir.TEMPLOC r, Frame.external_call("initRecord", [Ir.CONST n])) in
  let setField (exp, elem) =
    Ir.MOVE(
      Ir.MEMLOC(Ir.BINOP(Ir.PLUS, Ir.TEMP r, Ir.CONST((Frame.wordsize) * elem))),
      unEx exp
    )
  in
  let rec instantiateFields = function
    | [] -> [recordInit]
    | h::t -> (setField(h, List.length t))::(instantiateFields t)
  in
  let rec convert = function
    | [] -> Ir.EXP(Ir.CONST 0)
    | [s] -> s
    | h::t -> Ir.SEQ([h; convert(t)])
  in
    Ex(Ir.ESEQ(
      convert(List.rev(instantiateFields(exps))),
      Ir.TEMP(r)
    ))
;;

let stringIR lit =
  let checkFragLit = function
    | Frame.PROC(_) -> false
    | Frame.STRING(_, lit') -> lit' = lit
  in
  let getFragLabel () =
    match List.find_opt checkFragLit (!fraglist) with
    | Some(Frame.STRING(lab', _)) -> lab'
    | _ ->
      let lab' = Temp.newlabel() in
      let _ = fraglist := Frame.STRING(lab', lit)::(!fraglist) in
      lab'
  in
  let lab = getFragLabel()
  in
  Ex(Ir.NAME(lab))
;;

let getResult () = !fraglist
;;

let resetFragList () = fraglist := []
;;

let concatExpList(expList, exp) =
  let rec createExpListStm = function
    | h::t -> unNx(h)::createExpListStm(t)
    | [] -> []
  in
    Ex(Ir.ESEQ(Ir.SEQ(createExpListStm(expList)), unEx exp))
;;

let proc_entry_exit (level, body) =
  let levelFrame = match level with
    | TOPLEVEL -> (print_endline "fundec should not happen in outermost"; Frame.newframe(Temp.newlabel(), []))
    | NONTOP{uniq=_; parent=_; frame=frame} -> frame
  in
  let treeBody = unNx body
  in
  fraglist := Frame.PROC{body=treeBody; frame=levelFrame}::(!fraglist)
;;