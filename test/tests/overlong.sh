#!/bin/sh
# Copyright © 2015 Bart Massey
# [This program is licensed under the GPL version 3 or later.]
# Please see the file COPYING in the source
# distribution of this software for license terms.

# Test that an overlong source message is properly truncated for receipt.

TMP=/tmp/test-overlong.$$
trap "rm -rf $TMP" 0 1 2 3 15

if [ ! -x $TAUNET_SEND ]
then
    echo "no taunet-send at $TAUNET_SEND" >&2
    exit 1
fi
$TAUNET_SEND -c $ECHO_SERVER <misc/overlong.txt >$TMP
if [ `wc -c <$TMP` -ne 1024 ]
then
    [ -d failures ] || mkdir failures
    mv $TMP failures/overlong.txt
    echo "length of overlong response is wrong" >&2
    exit 1
fi
sed -i -e '1,5d' $TMP
if ! cmp -n `wc -c <$TMP` -s misc/overlong.txt $TMP
then
    [ -d failures ] || mkdir failures
    mv $TMP failures/overlong.txt
    echo "content of overlong response is wrong" >&2
    exit 1
fi
exit 0
