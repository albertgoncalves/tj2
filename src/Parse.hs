module Parse where

import Ast
  ( Expr (..),
    ExprOffset,
    Stmt (..),
    StmtOffset,
    StringOffset,
    Type (..),
    TypeOffset,
  )
import Control.Monad (when)
import Data.Char (isAlphaNum, isLower, isSpace, isUpper)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    get,
    look,
    munch,
    munch1,
    pfail,
    readP_to_S,
    satisfy,
    string,
    (<++),
  )
import Text.Printf (printf)

duplicates :: Eq a => [a] -> Bool
duplicates [] = False
duplicates (x : xs)
  | x `elem` xs = True
  | otherwise = duplicates xs

unpack :: [((a, b), c)] -> [b]
unpack = map (snd . fst)

many :: ReadP a -> ReadP [a]
many p = many1 p <++ return []

many1 :: ReadP a -> ReadP [a]
many1 p = (:) <$> p <*> many p

sepBy :: ReadP a -> ReadP sep -> ReadP [a]
sepBy p sep = sepBy1 p sep <++ return []

sepBy1 :: ReadP a -> ReadP sep -> ReadP [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

choice :: [ReadP a] -> ReadP a
choice [] = pfail
choice [p] = p
choice (p : ps) = p <++ choice ps

space :: ReadP ()
space = do
  rest <- look
  case rest of
    ('#' : _) -> do
      _ <- get
      munch (/= '\n') *> ((char '\n' *> space) <++ eof)
    (c : _) | isSpace c -> do
      _ <- get
      space
    _ -> return ()

token :: ReadP a -> ReadP (Int, a)
token p = do
  _ <- space
  offset <- length <$> look
  (offset,) <$> p

lowerIdent :: ReadP StringOffset
lowerIdent = token $ (:) <$> satisfy isLower <*> munch isAlphaNum

upperIdent :: ReadP StringOffset
upperIdent = token $ (:) <$> satisfy isUpper <*> munch isAlphaNum

comma :: ReadP (Int, Char)
comma = token $ char ','

keyValues :: ReadP a -> ReadP [(StringOffset, a)]
keyValues p = sepBy ((,) <$> (lowerIdent <* token (char ':')) <*> p) comma

leftParen :: ReadP (Int, Char)
leftParen = token $ char '('

rightParen :: ReadP (Int, Char)
rightParen = token $ char ')'

leftBrace :: ReadP (Int, Char)
leftBrace = token $ char '{'

rightBrace :: ReadP (Int, Char)
rightBrace = token $ char '}'

arrow :: ReadP StringOffset
arrow = token $ string "->"

typeFunc :: ReadP TypeOffset
typeFunc = do
  (offset, _) <- leftParen
  args <- sepBy type' comma
  _ <- rightParen
  _ <- arrow
  (offset,) . TypeFunc args <$> type'

typeSymbol :: ReadP TypeOffset
typeSymbol = (TypeSymbol <$>) <$> upperIdent

typeObj :: ReadP TypeOffset
typeObj = do
  (offset, _) <- leftBrace
  pairs <- sepBy1 ((,) <$> (lowerIdent <* token (char ':')) <*> type') comma
  when (duplicates $ unpack pairs) pfail
  _ <- rightBrace
  return (offset, TypeObj pairs)

typeVar :: ReadP TypeOffset
typeVar = do
  (offset, _) <- token (char '\'')
  var <- munch1 isLower
  return (offset, TypeVar var 0)

type' :: ReadP TypeOffset
type' = choice [typeFunc, typeSymbol, typeObj, typeVar]

exprCall :: ReadP ExprOffset
exprCall = do
  (offset, _) <- leftParen
  func <- expr
  args <- many expr
  _ <- rightParen
  return (offset, ExprCall func args)

exprFunc :: ReadP ExprOffset
exprFunc = do
  (offset, _) <- token $ char '\\'
  _ <- leftParen
  args <- keyValues type'
  _ <- rightParen
  _ <- arrow
  returnType <- type'
  _ <- token $ char '='
  (offset,) . ExprFunc args returnType <$> expr

exprLabel :: ReadP ExprOffset
exprLabel = (ExprLabel <$>) <$> lowerIdent

exprObj :: ReadP ExprOffset
exprObj = do
  (offset, _) <- leftBrace
  pairs <- keyValues expr
  when (duplicates $ unpack pairs) pfail
  _ <- rightBrace
  return (offset, ExprObj pairs)

exprSymbol :: ReadP ExprOffset
exprSymbol = (ExprSymbol <$>) <$> upperIdent

expr :: ReadP ExprOffset
expr = choice [exprCall, exprFunc, exprLabel, exprObj, exprSymbol]

stmtBinding :: ReadP StmtOffset
stmtBinding = do
  (offset, label) <- lowerIdent
  _ <- token $ char '='
  (offset,) . StmtBinding label <$> expr

stmtVoid :: ReadP StmtOffset
stmtVoid = do
  expr'@(offset, _) <- expr
  return (offset, StmtVoid expr')

stmt :: ReadP StmtOffset
stmt = choice [stmtBinding, stmtVoid]

enumerateType :: Int -> Type -> Type
enumerateType k (TypeFunc argTypes returnType) =
  TypeFunc
    (map (enumerateType k <$>) argTypes)
    (enumerateType k <$> returnType)
enumerateType _ existing@(TypeSymbol _) = existing
enumerateType k (TypeObj pairs) =
  TypeObj $ map ((enumerateType k <$>) <$>) pairs
enumerateType k (TypeVar var _) = TypeVar var k

enumerateExpr :: Int -> Expr -> Expr
enumerateExpr k (ExprCall func args) =
  ExprCall (enumerateExpr k <$> func) (map (enumerateExpr k <$>) args)
enumerateExpr k (ExprFunc argTypes returnType returnExpr) =
  ExprFunc
    (map ((enumerateType k <$>) <$>) argTypes)
    (enumerateType k <$> returnType)
    (enumerateExpr k <$> returnExpr)
enumerateExpr _ expr'@(ExprLabel _) = expr'
enumerateExpr k (ExprObj pairs) =
  ExprObj $ map ((enumerateExpr k <$>) <$>) pairs
enumerateExpr _ expr'@(ExprSymbol _) = expr'

enumerateStmt :: Int -> Stmt -> Stmt
enumerateStmt k (StmtBinding label expr') =
  StmtBinding label $ enumerateExpr k <$> expr'
enumerateStmt k (StmtVoid expr') = StmtVoid $ enumerateExpr k <$> expr'

extractBinding ::
  StmtOffset -> Either (String, Int) (Maybe (StringOffset, TypeOffset))
extractBinding (offset, StmtBinding _ (_, expr'@(ExprCall {}))) =
  Left (message, offset)
  where
    message :: String
    message = printf "Unable to bind expression `%s`" $ show expr'
extractBinding
  (offsetLabel, StmtBinding label (offsetExpr, ExprFunc args returnType _)) =
    Right $
      Just
        ( (offsetLabel, label),
          (offsetExpr, TypeFunc (map snd args) returnType)
        )
extractBinding (_, StmtBinding _ (offset, ExprLabel label)) =
  Left (message, offset)
  where
    message :: String
    message = printf "Unable to bind label `%s`" label
extractBinding (_, StmtBinding _ (offset, expr'@(ExprObj _))) =
  Left (message, offset)
  where
    message :: String
    message = printf "Unable to bind object `%s`" $ show expr'
extractBinding
  (offsetLabel, StmtBinding label (offsetExpr, ExprSymbol symbol)) =
    Right $ Just ((offsetLabel, label), (offsetExpr, TypeSymbol symbol))
extractBinding (_, StmtVoid _) = Right Nothing

extractExpr :: StmtOffset -> ExprOffset
extractExpr (_, StmtBinding _ expr') = expr'
extractExpr (_, StmtVoid expr') = expr'

intoBindings ::
  M.Map String TypeOffset ->
  [(StringOffset, TypeOffset)] ->
  Either (String, Int) (M.Map String TypeOffset)
intoBindings bindings [] = Right bindings
intoBindings bindings (((offset, label), typeOffset) : pairs)
  | M.notMember label bindings =
      intoBindings (M.insert label typeOffset bindings) pairs
  | otherwise = Left (message, offset)
  where
    message :: String
    message = printf "Identifier `%s` shadows existing binding" label

parse ::
  String -> Either (String, Int) (M.Map String TypeOffset, [ExprOffset])
parse source =
  case readP_to_S (many1 stmt <* token (pure ())) source of
    ((stmts, []) : _) -> do
      let ast = zipWith (\k -> (enumerateStmt k <$>)) [0 ..] stmts
      pairs <- catMaybes <$> mapM extractBinding ast
      bindings <- intoBindings M.empty pairs
      return (bindings, map extractExpr ast)
    ((_, remaining) : _) -> Left ("Invalid syntax", length remaining)
    _ -> Left ("Invalid syntax", length source)
