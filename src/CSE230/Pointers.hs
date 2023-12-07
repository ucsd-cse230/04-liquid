module CSE230.Pointers where

{-@ LIQUID "--no-termination" @-}

import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import Data.ByteString.Internal (c2w, w2c)


-- | See Ch 11 of https://ucsd-progsys.github.io/liquidhaskell-tutorial/book.pdf

--------------------------------------------------------------------------------------------------
-- | We assume the `malloc` function returns a pointer to a region of size `n`
--------------------------------------------------------------------------------------------------
{-@ assume mallocForeignPtrBytes :: n:Nat -> IO (ForeignPtrN a n) @-}

{- In the LH standard library, we have the following types for
   * `plusPtr`  -- pointer arithmetic accessing
   * `peek`     -- reading a raw pointer
   * `poke`     -- writing a raw pointer

plusPtr :: p:Ptr a
        -> o:{Nat | o <= plen p}   -- offset in bounds
        -> PtrN b {plen b - o}     -- remainder

peek :: {v:Ptr a | 0 < plen v} -> IO a

poke :: {v:Ptr a | 0 < plen v} -> a -> IO ()

-}

--------------------------------------------------------------------------------------------------
{-@ fail zero4 @-}      -- what happens if you delete this line, why?
zero4 :: IO (ForeignPtr a)
zero4 = do fp <- mallocForeignPtrBytes 4
           withForeignPtr fp $ \p -> do
             poke (p `plusPtr` 0) zero
             poke (p `plusPtr` 1) zero
             poke (p `plusPtr` 2) zero
             poke (p `plusPtr` 5) zero
           return fp
        where
           zero = 0 :: Word8

--------------------------------------------------------------------------------------------------
-- | A datatype to represent "Strings" as contiguous blocks of bytes in memory
--------------------------------------------------------------------------------------------------
data ByteString = BS {
    bPtr :: ForeignPtr Word8
  , bOff :: !Int
  , bLen :: !Int
  }

{-@ data ByteString = BS {
      bPtr :: ForeignPtr Word8
    , bOff :: {v:Nat| v        <= fplen bPtr}
    , bLen :: {v:Nat| v + bOff <= fplen bPtr}
    }                                       @-}

-- A bytestring of size n
{-@ type ByteStringN N = {v:ByteString| bLen v = N} @-}

----------------------------------------------------------------------------------
-- | Problem 1 : Fix the type of `create` so `bsTaco` typechecks
----------------------------------------------------------------------------------

{-@ create :: n:Nat -> (Ptr Word8 -> IO ()) -> ByteString @-}
create :: Int -> (Ptr Word8 -> IO ()) -> ByteString
create n fill = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes n
  withForeignPtr fp fill
  return (BS fp 0 n)

{-@ bsTaco :: ByteStringN 4 @-}
bsTaco = create 4 $ \p -> do
  poke (p `plusPtr` 0) (c2w 'T')
  poke (p `plusPtr` 1) (c2w 'a')
  poke (p `plusPtr` 2) (c2w 'c')
  poke (p `plusPtr` 3) (c2w 'o')

----------------------------------------------------------------------------------
-- | Problem 2 : Fix the type of `create` and `packLoop` so `pack` typechecks
----------------------------------------------------------------------------------

{-@ pack :: str:String -> ByteStringN {len str} @-}
pack :: String -> ByteString
pack str = create n $ \p -> packLoop p xs
  where
    n    = length str
    xs   = map c2w str

{-@ packLoop :: p:Ptr Word8 -> [Word8] -> IO () @-}
packLoop :: Ptr Word8 -> [Word8] -> IO ()
packLoop p (x:xs) = poke p x >> packLoop (plusPtr p 1) xs
packLoop _ []     = return ()

----------------------------------------------------------------------------------
-- | Problem 3 : Fix the types of `unsafeTake` and `unsafeDrop` so that
--               bsRocket and bsShip type check, and
--               bsRocket' and bsShip' are rejected.
----------------------------------------------------------------------------------

{-@ unsafeTake :: n:Nat -> ByteString -> ByteString @-}
unsafeTake :: Int -> ByteString -> ByteString
unsafeTake n (BS x s _) = BS x s n

{-@ unsafeDrop :: n:Nat -> ByteString -> ByteStringN n @-}
unsafeDrop :: Int -> ByteString -> ByteString
unsafeDrop n (BS x s l) = BS x (s + n) (l - n)

{-@ bsRocket :: ByteStringN 6 @-}
bsRocket :: ByteString
bsRocket = unsafeTake 6 (pack "RocketShip")

{-@ bsShip :: ByteStringN 4 @-}
bsShip :: ByteString
bsShip = unsafeDrop 6 (pack "RocketShip")

-- When you are done, the below should be *rejected*
{-@ fail bsRocket' @-}
bsRocket' :: ByteString
bsRocket' = unsafeTake 60 (pack "RocketShip")

{-@ fail bsShip' @-}
bsShip' :: ByteString
bsShip' = unsafeDrop 60 (pack "RocketShip")

----------------------------------------------------------------------------------
-- | Problem 4 (HARD?): Fix the types of `unpackLoop` so that `unpack` verifies
----------------------------------------------------------------------------------

{-@ unpack :: b:ByteString -> {v:String | len v == bLen b } @-}
unpack :: ByteString -> String
unpack (BS _ _ 0) = []
unpack (BS ps s l) = unsafePerformIO
                        $ withForeignPtr ps
                        $ \p -> unpackLoop (p `plusPtr` s) (l - 1) []

{-@ unpackLoop :: p:Ptr Word8 -> n:Int  -> acc:String -> IO String @-} -- TODO
unpackLoop :: Ptr Word8 -> Int -> String -> IO String
unpackLoop p 0 acc = peekAt p 0 >>= \e -> return (w2c e : acc)
unpackLoop p n acc = peekAt p n >>= \e -> unpackLoop p (n-1) (w2c e : acc)

{-@ peekAt :: p:Ptr a -> {n:Nat | n < plen p} -> IO a @-}
peekAt :: (Storable a) => Ptr a -> Int -> IO a
peekAt p n = peek (p `plusPtr` n)
