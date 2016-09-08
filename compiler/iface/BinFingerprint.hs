{-# LANGUAGE CPP #-}

module BinFingerprint
  ( -- * Computing fingerprints
    fingerprintBinMem
  , computeFingerprint
  , putNameLiterally
  ) where

#include "HsVersions.h"

import Fingerprint
import Binary
import Name
import Panic
import Util
import Outputable

fingerprintBinMem :: BinHandle -> IO Fingerprint
fingerprintBinMem bh =
  withBinBuffer bh (return . fingerprintByteString)

computeFingerprint :: (Binary a)
                   => (BinHandle -> Name -> IO ())
                   -> a
                   -> IO Fingerprint
computeFingerprint put_nonbinding_name a = do
  bh <- openBinMem (3*1024) -- just less than a block
  bh <- return $ setUserData bh $ newWriteState put_nonbinding_name putNameLiterally putFS
  put_ bh a
  fp <- fingerprintBinMem bh
  withBinBuffer bh (\x -> -- pprTrace "computeFingerprint" (ppr fp $$ text (show x)) $
                          return a)
  return fp

-- | Used when we want to fingerprint a structure without depending on the
-- fingerprints of external Names that it refers to.
putNameLiterally :: BinHandle -> Name -> IO ()
putNameLiterally bh name = ASSERT( isExternalName name )
  do
    put_ bh $! nameModule name
    put_ bh $! nameOccName name
