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
$TAUNET_SEND -n 0 $ECHO_SERVER <misc/replies0.txt >/dev/null
$TAUNET_SEND -n 3 $ECHO_SERVER <misc/replies3.txt >$TMP
sed -i -e '1,5d' $TMP
if ! cmp -s misc/replies3.txt $TMP
then
    [ -d failures ] || mkdir failures
    mv $TMP failures/replies3.txt
    echo "content of replies3 response is wrong" >&2
    exit 1
fi
exit 0
