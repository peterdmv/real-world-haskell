{-# LINE 1 "Regex.hsc" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LINE 2 "Regex.hsc" #-}

module Regex where

import Foreign
import Foreign.C.Types


{-# LINE 9 "Regex.hsc" #-}

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
caseless              :: PCREOption
caseless              = PCREOption 1
dollar_endonly        :: PCREOption
dollar_endonly        = PCREOption 32
dotall                :: PCREOption
dotall                = PCREOption 4

{-# LINE 31 "Regex.hsc" #-}

combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0

newtype PCREInfo = PCREInfo {unPCREInfo :: CInt}
    deriving (Eq,Show)

info_capturecount  :: PCREInfo
info_capturecount  = PCREInfo 2

{-# LINE 41 "Regex.hsc" #-}



newtype PCREExecOption = PCREExecOption { unPCREExecOption :: CInt }
    deriving (Eq,Show)


combineExecOptions :: [PCREExecOption] -> PCREExecOption
combineExecOptions = PCREExecOption . foldr ((.|.) . unPCREExecOption) 0
