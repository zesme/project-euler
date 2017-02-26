#!/bin/sh
p=`primesieve $1 -p | awk 1 ORS=','`
p=${p%?}
echo \"[$p]\"
