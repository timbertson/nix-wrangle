{-# LANGUAGE LambdaCase #-}

module Wrangle.Util where

import Prelude hiding (error)
import Control.Monad.Catch
import Data.String (IsString, fromString)
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Environment as Env

-- Awkward workaround for not knowing the type of a string literal
s :: String -> String
s = id

abort :: String -> IO a
abort msg = do
  putStrLn msg
  exitFailure

liftEither :: MonadThrow m => Exception a => Either a b -> m b
liftEither (Left err) = throwM err
liftEither (Right x) = return x

liftMaybe :: MonadThrow m => Exception a => a -> Maybe b -> m b
liftMaybe err Nothing = throwM err
liftMaybe _ (Just x) = return x

-- XXX this should just use `liftMaybe`, but that has a weird
-- TypeFamily restriction that e ~ SomeError, which breaks things
toRight :: a -> Maybe b -> Either a b
toRight err Nothing = Left err
toRight _ (Just x) = Right x

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft fn (Left x) = Left (fn x)
mapLeft _ (Right x) = Right x

orElse (Just x) _ = x
orElse Nothing x = x

orEither (Right x) _ = x
orEither (Left _) dfl = dfl

orTry (Right x) _ = (Right x)
orTry (Left _) alt = alt

debugLn :: String -> IO ()
debugLn = case unsafePerformIO (Env.lookupEnv "DEBUG") of
  (Just "true") -> putStrLn . ("[debug]: " <>)
  (Just _) -> noop
  Nothing -> noop
  where
    noop _ = return ()

infoLn :: String -> IO ()
infoLn = putStrLn

errorLn :: String -> IO ()
errorLn = putStrLn . ("[error]: " <>)

tap :: (a -> IO ()) -> IO a -> IO a
tap action x = x >>= (\x -> action x >> return x)

newtype AppError = AppError String
instance Show AppError where show (AppError s) = s
instance Semigroup AppError where (<>) (AppError a) (AppError b) = AppError $ a <> b
instance Exception AppError
instance IsString AppError where fromString = AppError

prefixAppError :: String -> Either AppError a -> Either AppError a
prefixAppError p = mapLeft ((AppError p) <>)
