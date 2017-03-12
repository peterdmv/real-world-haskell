module RegexExec where

import Regex
import PCRECompile

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Internal (toForeignPtr)
import Data.ByteString.Unsafe
import Foreign
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)

newtype PCREExtra = PCREExtra (Ptr PCREExtra)

foreign import ccall "pcre.h pcre_exec"
    c_pcre_exec     :: Ptr PCRE
                    -> Ptr PCREExtra
                    -> Ptr Word8
                    -> CInt
                    -> CInt
                    -> PCREExecOption
                    -> Ptr CInt
                    -> CInt
                    -> IO CInt

foreign import ccall "pcre.h pcre_fullinfo"
    c_pcre_fullinfo :: Ptr PCRE
                    -> Ptr PCREExtra
                    -> PCREInfo
                    -> Ptr a
                    -> IO CInt

capturedCount :: Ptr PCRE -> IO Int
capturedCount regex_ptr =
    alloca $ \n_ptr -> do
         c_pcre_fullinfo regex_ptr nullPtr info_capturecount n_ptr
         return . fromIntegral =<< peek (n_ptr :: Ptr CInt)

match :: Regex -> B.ByteString -> [PCREExecOption] -> Maybe [B.ByteString]
match (Regex pcre_fp _) subject os = unsafePerformIO $ do
  withForeignPtr pcre_fp $ \pcre_ptr -> do
    n_capt <- capturedCount pcre_ptr
    -- https://linux.die.net/man/3/pcreapi
    -- How pcre_exec() returns captured substrings
    let ovec_size = (n_capt + 1) * 3
        ovec_bytes = ovec_size * sizeOf (undefined :: CInt)

    allocaBytes ovec_bytes $ \ovec -> do

        let (str_fp, off, len) = toForeignPtr subject
        withForeignPtr str_fp $ \cstr -> do
            r <- c_pcre_exec
                         pcre_ptr
                         nullPtr
                         (cstr `plusPtr` off)
                         (fromIntegral len)
                         0
                         (combineExecOptions os)
                         ovec
                         (fromIntegral ovec_size)
            if r < 0
                then return Nothing
                else let loop n o acc =
                            if n == r
                              then return (Just (reverse acc))
                              else do
                                    i <- peekElemOff ovec o
                                    j <- peekElemOff ovec (o+1)
                                    let s = substring i j subject
                                    loop (n+1) (o+2) (s : acc)
                     in loop 0 0 []

  where
    substring :: CInt -> CInt -> B.ByteString -> B.ByteString
    substring x y _ | x == y = B.empty
    substring a b s = end
        where
            start = unsafeDrop (fromIntegral a) s
            end   = unsafeTake (fromIntegral (b-a)) start
