module Infer where

import Ast (Expr (..), Type (..))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

merge :: Type -> Type -> [(Type, Type)]
merge childType parentType
  | childType == parentType = []
merge childType parentType@(TypeVar _ _) = [(childType, parentType)]
merge (TypeFunc childArgs childReturn) (TypeFunc parentArgs parentReturn)
  | length childArgs /= length parentArgs = undefined
  | otherwise =
      concat
        (zipWith merge childArgs parentArgs)
        ++ merge childReturn parentReturn
merge (TypeObj _) (TypeObj _) = undefined
merge _ _ = undefined

deref :: M.Map (String, Int) Type -> (String, Int) -> Maybe Type
deref replacements key = do
  value <- M.lookup key replacements
  case value of
    TypeVar var k -> deref (M.delete key replacements) (var, k)
    replacementType -> return replacementType

unify :: M.Map (String, Int) Type -> [(Type, Type)] -> M.Map (String, Int) Type
unify replacements [] = replacements
unify _ ((TypeVar _ childK, TypeVar _ parentK) : _)
  | childK == parentK = undefined
unify replacements ((childType, TypeVar parentVar parentK) : pairs) =
  case deref replacements parentKey of
    -- NOTE: It is very unclear to me both _how_ and _why_ this works.
    -- Sometimes you just get lucky.
    Just replacementType ->
      unify replacements ((replacementType, childType) : pairs)
    Nothing -> unify (M.insert parentKey childType replacements) pairs
  where
    parentKey = (parentVar, parentK)
unify replacements ((childType, parentType) : pairs)
  | childType == parentType = unify replacements pairs
unify _ _ = undefined

swap :: M.Map (String, Int) Type -> Type -> Type
swap replacements (TypeFunc argTypes returnType) =
  TypeFunc (map (swap replacements) argTypes) $ swap replacements returnType
swap _ existing@(TypeSymbol _) = existing
swap replacements (TypeObj pairs) = TypeObj $ map (swap replacements <$>) pairs
swap replacements existing@(TypeVar var k) =
  fromMaybe existing $ M.lookup (var, k) replacements

infer :: M.Map String Type -> Expr -> Type
infer bindings (ExprCall func args) =
  case infer bindings func of
    TypeFunc argTypes _ | length args /= length argTypes -> undefined
    TypeFunc argTypes returnType ->
      let callTypes = map (infer bindings) args
          replacements =
            unify M.empty $ concat $ zipWith merge callTypes argTypes
       in if M.size replacements == 0
            then returnType
            else swap replacements returnType
    _ -> undefined
infer bindings (ExprFunc argTypes returnType returnExpr)
  | infer (M.union (M.fromList argTypes) bindings) returnExpr == returnType =
      TypeFunc (map snd argTypes) returnType
  | otherwise = undefined
infer bindings (ExprLabel label) =
  fromMaybe undefined $ M.lookup label bindings
infer bindings (ExprObj pairs) = TypeObj $ map (infer bindings <$>) pairs
infer _ (ExprSymbol symbol) = TypeSymbol symbol
