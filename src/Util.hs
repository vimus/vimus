module Util (withMPDEx_, maybeRead) where

import Data.Maybe
import Control.Monad (liftM)
import System.Environment (getEnv)
import System.IO.Error (isDoesNotExistError)

import Network.MPD

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

withMPDEx_ :: Maybe String -> Maybe Port -> MPD a -> IO (Response a)
withMPDEx_ h p m = do
    port       <- fromMaybeM (read `liftM` getEnvDefault "MPD_PORT" "6600") p
    (host, pw) <- parseHost `liftM` (fromMaybeM (getEnvDefault "MPD_HOST" "localhost") h)
    withMPDEx host port pw m
    where
        getEnvDefault x dflt =
            catch (getEnv x) (\e -> if isDoesNotExistError e
                                    then return dflt else ioError e)
        parseHost s = case breakChar '@' s of
                          (host, "") -> (host, "")
                          (pw, host) -> (host, pw)

fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM a x = fromMaybe a (fmap return x)

-- Break a string by character, removing the separator.
breakChar :: Char -> String -> (String, String)
breakChar c s = let (x, y) = break (== c) s in (x, drop 1 y)
