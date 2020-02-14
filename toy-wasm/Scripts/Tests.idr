module Scripts.Tests

import System

export
goldTests : IO ()
goldTests = do
    system "idris --build toy.ipkg"
    n <- system "Scripts/diff_test_gold.sh"
    if n /= 0 then do
        putStrLn "Gold tests failed"
        exitFailure
    else putStrLn "Gold tests passed"
