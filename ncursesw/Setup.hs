import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo

import System.Directory

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
    preBuild = \_ _ -> mkHookedBuildInfo
  , preConf  = \_ _ -> mkHookedBuildInfo
  , postConf = postConfHook
  }
  where
    postConfHook :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
    postConfHook args flags pd lbi = do
      bi <- mkHookedBuildInfo
      postConf simpleUserHooks args flags (updatePackageDescription bi pd)  lbi

    mkHookedBuildInfo :: IO (Maybe BuildInfo, [a])
    mkHookedBuildInfo = do
      r <- doesFileExist "/usr/include/ncursesw/ncurses.h"
      if r then
        return (Just emptyBuildInfo {cppOptions = ["-DDEBIAN"], ccOptions = ["-DDEBIAN"], includes = ["/usr/include/ncursesw/ncurses.h"]}, [])
      else
        return (Just emptyBuildInfo {includes = ["/usr/include/ncurses.h"]}, [])
