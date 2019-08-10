#!/bin/bash
# This script runs time_xtx.cpp several times for different N values.
# The output of the executions is dumped to times.dat.
make time_xtx
END=10
let p=2
for ((i=1;i<=END;i++)); do
    ./time_xtx $p
		let p="$p*2"
done

