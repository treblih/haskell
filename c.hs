{-# LANGUAGE ForeignFunctionInterface #-}
import Foreign
import Foreign.C.Types

foreign import ccall unsafe "stdlib.h rand"
	c_rand :: CUInt
