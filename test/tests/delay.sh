#!/bin/sh
# Copyright © 2015 Bart Massey
# [This program is licensed under the GPL version 3 or later.]
# Please see the file COPYING in the source
# distribution of this software for license terms.

# Test that the delay command does something reasonable.

if [ ! -x $TAUNET_SEND ]
then
    echo "no taunet-send at $TAUNET_SEND" >&2
    exit 1
fi
echo -n 'delay test (2.5s): ' >&2
T=`/usr/bin/time -f '%e' $TAUNET_SEND $ECHO_SERVER \
     <misc/delay.txt 2>&1 >/dev/null`
if expr $T '<' 2.1 >/dev/null
then
    echo "short timeout" $T >&2
    exit 1
fi
if expr $T '>' 2.9 >/dev/null
then
    echo "long timeout" $T >&2
    exit 1
fi
echo '.' >&2
exit 0
