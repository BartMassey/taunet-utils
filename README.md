# TauNet Utilities
Copyright (c) 2015 Bart Massey

TauNet is a project for Portland State University's CS 300
Principles of Software Engineering class in Fall 2015.  The
goal of the project is to provide software for a "secure
communications node" for a Raspberry Pi that allows it to
communicate securely with other TauNet nodes.

This code comprises a set of sample utilities interacting
with TauNet.

* `taunet-echo.hs`: This server will listen for a TauNet
  message and echo it back as the body of a message sent to
  the TauNet server presumptively at the source address of
  the original.

  This code is currently a work-in-progress: not complete
  and ready for use. However, it may serve as an example of
  TauNet interaction.

The Haskell code requires the packages `ciphersaber2`,
`bytestring` and `network` from Hackage. The build
infrastructure is not yet in place.

This work is licensed under the GPL version 3 or later.
Please see the file COPYING in the source distribution of
this software for license terms.
