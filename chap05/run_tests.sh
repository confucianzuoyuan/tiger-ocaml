#! /bin/bash

for f in ../testcases/*.tig ../additonalcases/*.tig ;do 
    echo -n $f ": "; ./_build/default/bin/main.exe < $f; done > results.log 2>&1