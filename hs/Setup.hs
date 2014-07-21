{-# OPTIONS_GHC -Wall #-}

import Data.List ( intercalate )
import Distribution.Simple
import Distribution.Simple.Setup
import qualified Distribution.PackageDescription as PD
import qualified Distribution.Simple.LocalBuildInfo as LBI
import qualified Distribution.ModuleName as MN
import System.Directory ( getDirectoryContents, doesDirectoryExist, doesFileExist )

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks { confHook = myConfHook }

myConfHook :: (PD.GenericPackageDescription, PD.HookedBuildInfo)
           -> ConfigFlags -> IO LBI.LocalBuildInfo
myConfHook (gpd,hbi) flags = do
  lbi0 <- confHook simpleUserHooks (gpd,hbi) flags

  modules <- fmap (MN.fromString "Messages" :) $ handleEntry [] "Messages"
  let lpd0 = LBI.localPkgDescr lbi0
      newlib = case PD.library lpd0 of
        Nothing -> error "no library"
        Just lib -> Just (lib {PD.exposedModules = PD.exposedModules lib ++ modules})
  return $ lbi0 { LBI.localPkgDescr = lpd0 { PD.library = newlib } }


getModules :: [String] -> IO [MN.ModuleName]
getModules pfx = do
  contents <- getDirectoryContents (prefixToPath pfx)
  fmap concat (mapM (handleEntry pfx) contents)

handleEntry :: [String] -> String -> IO [MN.ModuleName]
handleEntry _ "." = return []
handleEntry _ ".." = return []
handleEntry pfx entry = do
  dir <- doesDirectoryExist (prefixToPath (pfx ++ [entry]))
  file <- doesFileExist (prefixToPath (pfx ++ [entry]))
  case (dir,file) of
    (True,_) -> getModules (pfx ++ [entry])
    (_,True) -> case reverse entry of
      ('s':'h':'.':nm) -> return [MN.fromString $ (intercalate "." pfx) ++ "." ++ reverse nm]
      _ -> return []
    _ -> return []

prefixToPath :: [String] -> FilePath
prefixToPath pfx = intercalate "/" ("src":pfx)
