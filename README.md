# TauNet Utilities
Copyright (c) 2015 Bart Massey

TauNet is a project for Portland State University's CS 300
Principles of Software Engineering class in Fall 2015.  The
goal of the project is to provide software for a "secure
communications node" for a Raspberry Pi that allows it to
communicate securely with other TauNet nodes.

This code comprises a set of sample utilities for
interacting with TauNet.

* `taunet-echo.hs`: This server will listen for a TauNet
  message and echo it back as the body of a message sent to
  the TauNet server presumptively at the source address of
  the original.

* `taunet-send.hs`: This client will send its standard input
  to a TauNet server and listen for a return message, which
  it will write to standard output.

See the manual pages in the `man/` subdirectory for details
of invocation.

These programs are buildable with Haskell Cabal and the
appropriate packages. Say `cabal configure`, use `cabal
install` to install requested packages, then say `cabal
build`. You will need at least GHC 2.7, because of the
tangle of Hackage dependencies.

This work is licensed under the GPL version 3 or later.
Please see the file COPYING in the source distribution of
this software for license terms.

Here is the release announcement:

---

I now have a TauNet echo server running on
barton.cs.pdx.edu.

The server uses key ******** as per usual. If you send a
TauNet message to this host, you will get back an
instantaneous response whose body is your message as
received by the echo server. The response will go back to
whatever IP address you sent from. The echo server doesn't
care about usernames, and will happily respond as whatever
you called it to whatever you called yourself. If the echo
server cannot process your message, it will attempt to send
a failure report back anyhow, using usernames TEST-TO and
TEST-FROM.

Let me know if there are issues. I will try to keep this
server up indefinitely. It may go down for a few seconds
once in a while for maintenance.

The source code is at
<http://github.com/PSU-CS-300-Fall2015/taunet-utils>. It
includes a test client that will send a message and listen
for the response. It is vaguely ugly Haskell and may be hard
to build and harder to work on.
