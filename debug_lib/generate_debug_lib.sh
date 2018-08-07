#!/bin/bash

set -e -u -o pipefail

for f in ../src/*.ml{,i}; do
    b=$(basename $f)
    cat $f >$b.tmp
    case $b in
        incremental*)
            target=$(echo $b | sed -r 's/incremental/incremental_debug/')
            rm -f $target
            sed <$b.tmp >$target -r \
                -e 's/Incremental_intf/Incremental_debug_intf/g' \
                -e 's/Incremental_kernel/Incremental_kernel_debug/g'
            ;;
        *)
            target=$b
            rm -f $target
            sed <$b.tmp >$target -r \
                -e 's/Incremental_kernel/Incremental_kernel_debug/'
            ;;
    esac        
    chmod -w $target
    rm -f $b.tmp
done
