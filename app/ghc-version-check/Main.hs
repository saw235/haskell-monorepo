import System.Info (compilerName, compilerVersion)

main :: IO ()
main = do
    putStrLn $ "Compiler: " ++ compilerName
    putStrLn $ "Version: " ++ show compilerVersion
    putStrLn "GHC Version Check Complete" 