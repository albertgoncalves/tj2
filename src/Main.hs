import Ast (Expr (..), Stmt (..), Type (..))
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Infer (infer)
import Parse (parse)
import System.Environment (getArgs)

extractBinding :: Stmt -> Maybe (String, Type)
extractBinding (StmtBinding _ (ExprCall {})) = undefined
extractBinding (StmtBinding label (ExprFunc args returnType _)) =
  Just (label, TypeFunc (map snd args) returnType)
extractBinding (StmtBinding _ (ExprLabel _)) = undefined
extractBinding (StmtBinding _ (ExprObj _)) = undefined
extractBinding (StmtBinding label (ExprSymbol symbol)) =
  Just (label, TypeSymbol symbol)
extractBinding (StmtVoid _) = Nothing

extractExpr :: Stmt -> Expr
extractExpr (StmtBinding _ expr) = expr
extractExpr (StmtVoid expr) = expr

main :: IO ()
main = do
  [pathIn] <- getArgs
  source <- readFile pathIn

  let ast = parse source
  putChar '\n'
  mapM_ print ast

  let bindings = M.fromList $ mapMaybe extractBinding ast
  putChar '\n'
  mapM_ print $ M.toList bindings

  let program = map extractExpr ast
  putChar '\n'
  mapM_ print $ zip (map (infer bindings) program) program
