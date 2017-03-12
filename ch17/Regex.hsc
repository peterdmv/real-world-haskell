{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Regex where

import Foreign
import Foreign.C.Types

#include <pcre.h>

-- | A type for PCRE compile-time options. These are newtyped CInts,
-- which can be bitwise-or'd together, using '(Data.Bits..|.)'
--
newtype PCREOption = PCREOption { unPCREOption :: CInt }
    deriving (Eq,Show)

-- caseless       :: PCREOption
-- caseless       = PCREOption #const PCRE_CASELESS

-- dollar_endonly :: PCREOption
-- dollar_endonly = PCREOption #const PCRE_DOLLAR_ENDONLY

-- dotall         :: PCREOption
-- dotall         = PCREOption #const PCRE_DOTALL

-- PCRE compile options
#{enum PCREOption, PCREOption
  , caseless             = PCRE_CASELESS
  , dollar_endonly       = PCRE_DOLLAR_ENDONLY
  , dotall               = PCRE_DOTALL
  }

combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0

newtype PCREInfo = PCREInfo {unPCREInfo :: CInt}
    deriving (Eq,Show)

#{enum PCREInfo, PCREInfo
  , info_capturecount = PCRE_INFO_CAPTURECOUNT
}



newtype PCREExecOption = PCREExecOption { unPCREExecOption :: CInt }
    deriving (Eq,Show)


combineExecOptions :: [PCREExecOption] -> PCREExecOption
combineExecOptions = PCREExecOption . foldr ((.|.) . unPCREExecOption) 0
