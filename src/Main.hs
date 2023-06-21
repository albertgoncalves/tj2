import qualified Data.Map as M
import Infer (infer)
import Parse (parse)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Printf (printf)

rowCol :: Int -> Int -> String -> Int -> (Int, Int)
rowCol y x _ 0 = (y, x)
rowCol y _ ('\n' : cs) i = rowCol (y + 1) 1 cs (i - 1)
rowCol y x (_ : cs) i = rowCol y (x + 1) cs (i - 1)
rowCol _ _ _ _ = undefined

exit :: String -> String -> String -> Int -> IO a
exit path source message offset = do
  putStrLn $ printf "%s:%d:%d: %s" path row col message
  exitFailure
  where
    (row, col) = rowCol 1 1 source (length source - offset)

main :: IO ()
main = do
  [path] <- getArgs
  source <- readFile path
  (bindings, program) <-
    either (exit path source "Invalid syntax") return $ parse source
  let types = mapM (infer $ M.fromList bindings) program

  putChar '\n'
  mapM_ (print . (snd <$>)) bindings

  putChar '\n'
  either (uncurry $ exit path source) (mapM_ print . zip program) types
