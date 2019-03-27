#!/bin/bash

set -e -u -o pipefail

for f in ../src/*.ml{,i}; do
    b=$(basename $f)
    cat $f >$b.tmp
    case $b in
        incremental.*|incremental_intf*)
            target=$(echo $b | sed -r 's/incremental/incremental_debug/')
            rm -f $target
            sed <$b.tmp >$target -r 's/Incremental_intf/Incremental_debug_intf/g'
            ;;
        *)
            target=$b
            mv $b.tmp $target
            ;;
    esac        
    chmod -w $target
    rm -f $b.tmp
done
