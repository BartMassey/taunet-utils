#!/bin/sh
# Copyright © 2015 Bart Massey
# [This program is licensed under the GPL version 3 or later.]
# Please see the file COPYING in the source
# distribution of this software for license terms.

# Upgrade the installed taunet-echo server.

git pull &&
cabal build &&
sudo rm -f /usr/local/sbin/taunet-echo &&
sudo cp dist/build/taunet-echo/taunet-echo /usr/local/sbin/ &&
sudo service taunet-echo restart
