#!/bin/sh
# Copyright © 2015 Bart Massey
# [This program is licensed under the GPL version 3 or later.]
# Please see the file COPYING in the source
# distribution of this software for license terms.

# Test the ping functionality of the server and client.
if [ ! -x $TAUNET_SEND ]
then
    echo "no taunet-send at $TAUNET_SEND" >&2
    exit 1
fi
$TAUNET_SEND -P $ECHO_SERVER
