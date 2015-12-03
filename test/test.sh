#!/bin/sh
# Copyright © 2015 Bart Massey
# Create regression test against known-good echo server.
# Tests should be hand-checked after creation for validity.
TAUNET_SEND=../dist/build/taunet-send/taunet-send
ECHO_SERVER=barton.cs.pdx.edu
if [ ! -x $TAUNET_SEND ]
then
    echo "no taunet-send at $TAUNET_SEND" >&2
    exit 1
fi
TMP=/tmp/test.$$
trap "rm -rf $TMP" 0 1 2 3 15
mkdir $TMP
[ -d outputs ] || mkdir outputs
for i in inputs/*.txt
do
    N=`basename $i`
    $TAUNET_SEND $ECHO_SERVER <inputs/$N >$TMP/$N
    if ! cmp $TMP/$N outputs/$N
    then
        echo "$N failed" >&2
        [ -d failures ] || mkdir failures
        mv $TMP/$N failures/
    fi
done
