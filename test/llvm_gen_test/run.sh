#!/bin/bash

for file in ./*.hl; do
    rm -f ./heaplang_llvm_*.ll
    tmpfile=./heaplang_llvm_XXXXXX.ll
    result=$(HeaplangToLLVM --llvm $file > $tmpfile)
    if lli $tmpfile; then
        rm $tmpfile
        echo "Successfully executed $file"
    else
        echo "Error running lli on $file"
        echo $result
        cat  $tmpfile
        exit 1
    fi
done