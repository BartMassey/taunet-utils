#!/bin/sh
# Copyright © 2015 Bart Massey
# [This program is licensed under the GPL version 3 or later.]
# Please see the file COPYING in the source
# distribution of this software for license terms.

# Driver for echo server.

cd /etc/taunet-echo
while true
do
    /usr/local/sbin/taunet-echo </dev/null >>echo.err 2>&1
    echo "`date`: server restarted" >>echo.err
    sleep 5
done
