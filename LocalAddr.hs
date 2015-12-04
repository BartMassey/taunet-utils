-- Copyright © 2015 Bart Massey
-- [This program is licensed under the GPL version 3 or later.]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- | This module provides functions for manipulating
-- IP addresses and for finding out whether an IP address
-- is on a local interface.
module LocalAddr (AddressData(..), getLocalAddresses, hostAddr, portAddr)
where

import Data.Word
import Network.Info
import Network.Socket

-- | Yet another IP address datatype. This proved necessary
-- because of limitations of the underlying datatypes, and
-- to provide some portability opportunities. No attempt is
-- made to preserve IPv6 flow IDs here, and an assumption is
-- made that the only scopes are 0x2 (local) and 0xe (global).
data AddressData = AddressDataIPv4 !Word32
                 | AddressDataIPv6 !(Word32, Word32, Word32, Word32)
                 | AddressDataIPv6Local !(Word32, Word32, Word32, Word32)
                 deriving (Ord, Eq)

class AddressClass a where
    fromAddress :: a -> AddressData

instance AddressClass IPv4 where
    fromAddress (IPv4 w) = AddressDataIPv4 w

instance AddressClass IPv6 where
    fromAddress (IPv6 w1 w2 w3 w4) = AddressDataIPv6 (w1, w2, w3, w4)

instance Show AddressData where
    show (AddressDataIPv4 w) = show $ IPv4 w
    show (AddressDataIPv6Local (w1, w2, w3, w4)) = show $ IPv6 w1 w2 w3 w4
    show (AddressDataIPv6 (w1, w2, w3, w4)) = show $ IPv6 w1 w2 w3 w4

-- | Given a 'Network.Socket' 'SockAddr, return the IP address
-- portion as 'AddressData'.
hostAddr :: SockAddr -> AddressData
hostAddr (SockAddrInet _ ha) =
    AddressDataIPv4 ha
hostAddr (SockAddrInet6 _ scope ha _) | scope <= 0x2 =
    AddressDataIPv6Local ha
hostAddr (SockAddrInet6 _ _ ha _) =
    AddressDataIPv6 ha
hostAddr _ =
    error "hostAddr: unsupported address type"

-- | Given a 'Network.Socket' 'PortNumber' and an 'AddressData' IP address,
-- return a 'Network.Socket' 'SockAddr'.
portAddr :: PortNumber -> AddressData -> SockAddr
portAddr p (AddressDataIPv4 ha) =
    SockAddrInet p ha
portAddr p (AddressDataIPv6Local ha) =
    SockAddrInet6 p 0x2 ha 0x0
portAddr p (AddressDataIPv6 ha) =
    SockAddrInet6 p 0xe ha 0x0

-- | Get a list of IP addresses for all the interfaces on
-- the local machine.
getLocalAddresses :: IO [AddressData]
getLocalAddresses = do
  ifaces <- getNetworkInterfaces
  let translate f = map (fromAddress . f) ifaces
  return $ translate ipv4 ++ translate ipv6
