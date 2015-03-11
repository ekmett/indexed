{-# LANGUAGE TypeOperators, GADTs, KindSignatures, RankNTypes, DataKinds #-}

module FH where

import System.FilePath
import System.IO
import Control.Exception 
import System.Environment
import Indexed.Functor
import Indexed.Types
import Indexed.Monad.Free

data FHState = FOpen | FClosed
data FHSTATE :: FHState -> * where
  FOPEN    :: FHSTATE FOpen
  FCLOSED  :: FHSTATE FClosed

data FH :: (FHState -> *) -> FHState -> * where
  FHOpen   :: FilePath -> (FHSTATE ~> q) -> FH q FClosed
  FHClose  :: q FClosed -> FH q FOpen
  FHRead   :: (Maybe Char -> q FOpen) -> FH q FOpen

instance IFunctor FH where
  imap f (FHOpen s k) = FHOpen s (f . k)
  imap f (FHClose q) = FHClose (f q)
  imap f (FHRead k) = FHRead (f . k)

fhOpen :: FilePath -> (Free FH FHSTATE) FClosed
fhOpen f = Free $ FHOpen f Return

fhClose :: Free FH (At () FClosed) FOpen
fhClose = Free . FHClose . Return $ At ()

fhRead :: Free FH (At (Maybe Char) FOpen) FOpen
fhRead = Free . FHRead $ \ c -> Return (At c)

runFH :: Free FH (At a FClosed) FClosed -> IO a
runFH (Return (At a)) = return a
runFH (Free (FHOpen s f)) = catch
   (openFile s ReadMode >>= openFH (f FOPEN))
   (\(SomeException _) -> runFH (f FCLOSED))
  where openFH (Free (FHClose k)) h = hClose h >> runFH k
        openFH (Free (FHRead f)) h = catch
          (hGetChar h >>= \ c -> openFH (f (Just c)) h)
          (\(SomeException _) -> openFH (f Nothing) h)

myReadFile :: FilePath -> IO (Maybe String)
myReadFile s = runFH $ ibind check (fhOpen s)  where
  check :: FHSTATE ~> (Free FH (At (Maybe String) FClosed))
  check FCLOSED = Return (At Nothing)
  check FOPEN = grab !>= \ s -> fhClose !>= \ _ -> Return (At (Just s))
  grab :: (Free FH (At String FOpen)) FOpen
  grab = fhRead !>= \ x -> case x of
    Nothing -> Return (At "")
    Just c -> grab !>= \ cs -> Return (At (c : cs))
