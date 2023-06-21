module Infer where

import Ast (Expr (..), ExprOffset, Type (..), TypeOffset, commaDelim)
import Control.Monad (zipWithM)
import Data.Bifunctor (first)
import Data.List (sortOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Text.Printf (printf)

merge ::
  TypeOffset ->
  TypeOffset ->
  Either (String, Int) [(TypeOffset, TypeOffset)]
merge (_, childType) (_, parentType)
  | childType == parentType = Right []
merge childTypeOffset parentTypeOffset@(_, TypeVar _ _) =
  Right [(childTypeOffset, parentTypeOffset)]
merge
  (offset, TypeFunc childArgs childReturn)
  (_, TypeFunc parentArgs parentReturn)
    | lengthChild /= lengthParent =
        let message =
              printf
                "Function requires %d argument(s), but %d were provided"
                lengthParent
                lengthChild ::
                String
         in Left (message, offset)
    | otherwise =
        (++)
          <$> (concat <$> zipWithM merge childArgs parentArgs)
          <*> merge childReturn parentReturn
    where
      lengthChild = length childArgs
      lengthParent = length parentArgs
merge (offset, TypeObj childPairs) (_, TypeObj parentPairs)
  | childKeys /= parentKeys =
      let message =
            printf
              "Object requires field(s) [%s], but field(s) [%s] were provided"
              (commaDelim childKeys)
              (commaDelim parentKeys) ::
              String
       in Left (message, offset)
  | otherwise = concat <$> zipWithM merge childValues parentValues
  where
    (childKeyOffsets, childValues) = unzip $ sortOn (snd . fst) childPairs
    (parentKeyOffsets, parentValues) = unzip $ sortOn (snd . fst) parentPairs
    childKeys = map snd childKeyOffsets
    parentKeys = map snd parentKeyOffsets
merge (offset, leftType) (_, rightType) = Left (message, offset)
  where
    message :: String
    message =
      printf "Expected `%s` but received `%s`" (show rightType) (show leftType)

deref ::
  M.Map (String, Int) TypeOffset ->
  (String, Int) ->
  Maybe TypeOffset
deref replacements key = do
  value <- M.lookup key replacements
  case value of
    (_, TypeVar var k) -> deref (M.delete key replacements) (var, k)
    replacementTypeOffset -> return replacementTypeOffset

unify ::
  M.Map (String, Int) TypeOffset ->
  [(TypeOffset, TypeOffset)] ->
  Either (String, Int) (M.Map (String, Int) TypeOffset)
unify replacements [] = Right replacements
unify replacements (((_, childType), (_, parentType)) : pairs)
  | childType == parentType = unify replacements pairs
unify
  replacements
  ((childTypeOffset@(_, TypeObj _), parentTypeOffset@(_, TypeObj _)) : pairs) =
    merge childTypeOffset parentTypeOffset
      >>= (unify replacements . (++ pairs))
unify _ (((offset, childType@(TypeVar _ childK)), (_, parentType@(TypeVar _ parentK))) : _)
  | childK == parentK =
      let message =
            printf
              "Unable to unify `%s` with `%s`"
              (show childType)
              (show parentType) ::
              String
       in Left (message, offset)
unify
  replacements
  ((childTypeOffset, (_, TypeVar parentVar parentK)) : pairs) =
    case deref replacements parentKey of
      -- NOTE: It is very unclear to me both _how_ and _why_ this works.
      -- Sometimes you just get lucky.
      Just replacementTypeOffset ->
        unify replacements ((replacementTypeOffset, childTypeOffset) : pairs)
      Nothing -> unify (M.insert parentKey childTypeOffset replacements) pairs
    where
      parentKey = (parentVar, parentK)
unify _ (((offset, childType), (_, parentType)) : _) = Left (message, offset)
  where
    message :: String
    message =
      printf
        "Unable to unify `%s` with `%s`"
        (show childType)
        (show parentType)

swap :: M.Map (String, Int) TypeOffset -> TypeOffset -> TypeOffset
swap replacements funcOffset@(_, TypeFunc argTypes returnTypeOffset) =
  funcType <$ funcOffset
  where
    funcType =
      TypeFunc (map (swap replacements) argTypes) $
        swap replacements returnTypeOffset
swap _ existing@(_, TypeSymbol _) = existing
swap replacements (offset, TypeObj pairs) =
  (offset, TypeObj $ map (swap replacements <$>) pairs)
swap replacements existing@(_, TypeVar var k) =
  fromMaybe existing $ M.lookup (var, k) replacements

infer ::
  M.Map String TypeOffset ->
  ExprOffset ->
  Either (String, Int) TypeOffset
infer bindings (offset, ExprCall funcOffset@(_, func) args) =
  case infer bindings funcOffset of
    Right (_, TypeFunc argTypes _)
      | length args /= length argTypes ->
          let message =
                printf
                  "Function requires %d argument(s), but %d were provided"
                  (length argTypes)
                  (length args) ::
                  String
           in Left (message, offset)
    Right (_, TypeFunc argTypes returnTypeOffset) -> do
      callTypes <- mapM (infer bindings) args
      pairs <- zipWithM merge callTypes argTypes
      replacements <- unify M.empty $ concat pairs
      return $
        first (const offset) $
          if M.size replacements == 0
            then returnTypeOffset
            else swap replacements returnTypeOffset
    _ -> Left (printf "Unable to call `%s` as a function" (show func), offset)
infer
  bindings
  (_, ExprFunc argTypes returnTypeOffset@(_, returnType) returnExpr) = do
    (offset, exprType) <-
      infer
        (M.union (M.fromList $ map (first snd) argTypes) bindings)
        returnExpr
    if exprType == returnType
      then Right (offset, TypeFunc (map snd argTypes) returnTypeOffset)
      else
        let message =
              printf
                "Expected `%s` but received `%s`"
                (show returnType)
                (show exprType) ::
                String
         in Left (message, offset)
infer bindings (offset, ExprLabel label) =
  maybe (Left (printf "Identifier `%s` not defined" label, offset)) Right $
    M.lookup label bindings
infer bindings (offset, ExprObj pairs) =
  (offset,) . TypeObj . zip fields <$> mapM (infer bindings) types
  where
    (fields, types) = unzip pairs
infer _ exprOffset@(_, ExprSymbol symbol) =
  Right $ TypeSymbol symbol <$ exprOffset
