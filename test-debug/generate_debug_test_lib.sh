#!/bin/bash

set -e -u -o pipefail

cp ../test/*.ml{,i} .
echo 'module Incremental = Incremental_debug' >>import.ml
chmod a-w *.ml{,i}
