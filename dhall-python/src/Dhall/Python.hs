{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-| This library only exports a single `dhallToPython` function for translating a
    Dhall syntax tree to a Python syntax tree (i.e. a `Value`)

    See the @dhall@ package if you would like to transform Dhall source code
    into a Dhall syntax tree.  Similarly, see the @language-python@ package if you would
    like to translate a Python syntax tree into Python.

    Dhall @Bool@s translate to Python bools:

> $ dhall-to-python <<< 'True'
> true
> $ dhall-to-python <<< 'False'
> false

    Dhall numbers translate to Python numbers:

> $ dhall-to-python <<< '+2'
> 2
> $ dhall-to-python <<< '2'
> 2
> $ dhall-to-python <<< '2.3'
> 2.3

    Dhall @Text@ translates to Python text:

> $ dhall-to-python <<< '"ABC"'
> "ABC"

    Dhall @List@s translate to Python lists:

> $ dhall-to-python <<< '[1, 2, 3] : List Integer'
> [1,2,3]

    Dhall @Optional@ values translate to @null@ if absent and the unwrapped
    value otherwise:

> $ dhall-to-python <<< '[] : Optional Integer'
> null
> $ dhall-to-python <<< '[1] : Optional Integer'
> 1

    Dhall records translate to Python records:

> $ dhall-to-python <<< '{ foo = 1, bar = True }'
> {"foo":1,"bar":true}

    Dhall unions translate to the wrapped value:

> $ dhall-to-python <<< "< Left = +2 | Right : Natural>"
> 2
> $ cat config
> [ < Person = { age = +47, name = "John" }
>   | Place  : { location : Text }
>   >
> , < Place  = { location = "North Pole" }
>   | Person : { age : Natural, name : Text }
>   >
> , < Place  = { location = "Sahara Desert" }
>   | Person : { age : Natural, name : Text }
>   >
> , < Person = { age = +35, name = "Alice" }
>   | Place  : { location : Text }
>   >
> ]
> $ dhall-to-python <<< "./config"
> [{"age":47,"name":"John"},{"location":"North Pole"},{"location":"Sahara Desert"},{"age":35,"name":"Alice"}]

    You can preserve the name of the alternative if you wrap the value in a
    record with three fields:

    * @contents@: The union literal that you want to preserve the tag of

    * @field@: the name of the field that will store the name of the
      alternative

    * @nesting@: A value of type @\< Inline : {} | Nested : Text \>@.

    If @nesting@ is set to @Inline@ and the union literal stored in @contents@
    contains a record then the name of the alternative is stored inline within
    the same record.  For example, this code:

>     let Example = < Left : { foo : Natural } | Right : { bar : Bool } >
>
> in  let example = constructors Example
>
> in  let Nesting = < Inline : {} | Nested : Text >
>
> in  let nesting = constructors Nesting
>
> in  { field    = "name"
>     , nesting  = nesting.Inline {=}
>     , contents = example.Left { foo = 2 }
>     }

    ... produces this Python:

> {
>   "foo": 2,
>   "name": "Left"
> }

    If @nesting@ is set to @Nested nestedField@ then the union is store
    underneath a field named @nestedField@.  For example, this code:

>     let Example = < Left : { foo : Natural } | Right : { bar : Bool } >
>
> in  let example = constructors Example
>
> in  let Nesting = < Inline : {} | Nested : Text >
>
> in  let nesting = constructors Nesting
>
> in  { field    = "name"
>     , nesting  = nesting.Nested "value"
>     , contents = example.Left { foo = 2 }
>     }

    ... produces this Python:

> {
>   "name": "Left",
>   "value": {
>     "foo": 2
>   }
> }

    Also, all Dhall expressions are normalized before translation to Python:

> $ dhall-to-python <<< "True == False"
> false

-}

module Dhall.Python (
    -- * Dhall to Python
      dhallToPython
    -- , omitNull
    , Conversion(..)
    , convertToHomogeneousMaps
    , parseConversion
    , codeToValue

    -- * Exceptions
    , CompileError(..)
    ) where

import Control.Applicative (empty, (<|>))
import Control.Monad (guard)
import Control.Exception (Exception, throwIO)
import qualified Language.Python.Common.AST as Py
import qualified Language.Python.Common.SrcLocation as Py
import Data.Monoid ((<>), mempty)
import Data.Text (Text, unpack)
import Data.Typeable (Typeable)
import Data.Bifunctor (first)
import Dhall.Core (Expr)
import Dhall.TypeCheck (X)
import Dhall.Map (Map, toList)
import Options.Applicative (Parser)

import qualified Data.Foldable
import qualified Data.HashMap.Strict
import qualified Data.List
import qualified Data.Ord
import qualified Data.Text
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.Map
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified Options.Applicative

{-| This is the exception type for errors that might arise when translating
    Dhall to Python

    Because the majority of Dhall language features do not translate to Python
    this just returns the expression that failed
-}
data CompileError = Unsupported (Expr X X) deriving (Typeable)

instance Show CompileError where
    show (Unsupported e) =
        Data.Text.unpack $
            "" <> _ERROR <> ": Cannot translate to Python                                     \n\
            \                                                                               \n\
            \Explanation: Only primitive values, records, unions, ❰List❱s, and ❰Optional❱   \n\
            \values can be translated from Dhall to Python                                    \n\
            \                                                                               \n\
            \The following Dhall expression could not be translated to Python:                \n\
            \                                                                               \n\
            \↳ " <> txt <> "                                                                "
      where
        txt = Dhall.Core.pretty e

_ERROR :: Data.Text.Text
_ERROR = "\ESC[1;31mError\ESC[0m"

instance Exception CompileError

{-| Convert a Dhall expression to the equivalent Python expression

>>> :set -XOverloadedStrings
>>> :set -XOverloadedLists
>>> import Dhall.Core
>>> dhallToPython (RecordLit [("foo", IntegerLit 1), ("bar", TextLit "ABC")])
Right (Object (fromList [("foo",Number 1.0),("bar",String "ABC")]))
>>> fmap Data.Aeson.encode it
Right "{\"foo\":1,\"bar\":\"ABC\"}"
-}

dhallToPython :: Expr s X -> Either CompileError (Py.Expr Py.SrcSpan)
dhallToPython e0 = loop (Dhall.Core.normalize e0)
  where
    loop e = case e of
        -- -- | > Const c                                  ~  c
        -- = Const Const
        -- -- | > Var (V x 0)                              ~  x
        -- --   > Var (V x n)                              ~  x@n
        -- | Var Var
        -- -- | > Lam x     A b                            ~  λ(x : A) -> b
        -- | Lam Text (Expr s a) (Expr s a)
        -- -- | > Pi "_" A B                               ~        A  -> B
        -- --   > Pi x   A B                               ~  ∀(x : A) -> B
        -- | Pi  Text (Expr s a) (Expr s a)
        -- -- | > App f a                                  ~  f a
        -- | App (Expr s a) (Expr s a)
        -- -- | > Let [Binding x Nothing  r] e             ~  let x     = r in e
        -- --   > Let [Binding x (Just t) r] e             ~  let x : t = r in e
        -- | Let (NonEmpty (Binding s a)) (Expr s a)
        -- -- | > Annot x t                                ~  x : t
        -- | Annot (Expr s a) (Expr s a)
        -- -- | > Bool                                     ~  Bool
        -- | Bool
        -- -- | > BoolLit b                                ~  b
        -- | BoolLit Bool
        Dhall.Core.BoolLit a -> do
          return (Py.Bool { bool_value = a, expr_annot = Py.SpanEmpty })
        -- -- | > BoolAnd x y                              ~  x && y
        -- | BoolAnd (Expr s a) (Expr s a)
        Dhall.Core.BoolAnd a b -> do
            a' <-  loop a
            b' <-  loop b
            return (Py.BinaryOp { operator = Py.And Py.SpanEmpty, left_op_arg = a', right_op_arg = b', expr_annot = Py.SpanEmpty })
        -- -- | > BoolOr  x y                              ~  x || y
        -- | BoolOr  (Expr s a) (Expr s a)
        Dhall.Core.BoolAnd a b -> do
            a' <-  loop a
            b' <-  loop b
            return (Py.BinaryOp { operator = Py.Or Py.SpanEmpty, left_op_arg = a', right_op_arg = b', expr_annot = Py.SpanEmpty })
        -- -- | > BoolEQ  x y                              ~  x == y
        -- | BoolEQ  (Expr s a) (Expr s a)
        Dhall.Core.BoolEQ a b -> do
            a' <-  loop a
            b' <-  loop b
            return (Py.BinaryOp { operator = Py.Equality Py.SpanEmpty, left_op_arg = a', right_op_arg = b', expr_annot = Py.SpanEmpty })
        -- -- | > BoolNE  x y                              ~  x != y
        -- | BoolNE  (Expr s a) (Expr s a)
        Dhall.Core.BoolEQ a b -> do
            a' <-  loop a
            b' <-  loop b
            return (Py.BinaryOp { operator = Py.NotEqualsV2 Py.SpanEmpty, left_op_arg = a', right_op_arg = b', expr_annot = Py.SpanEmpty })
        -- -- | > BoolIf x y z                             ~  if x then y else z
        -- | BoolIf (Expr s a) (Expr s a) (Expr s a)
        -- -- | > Natural                                  ~  Natural
        -- | Natural
        -- -- | > NaturalLit n                             ~  n
        -- | NaturalLit Natural
        Dhall.Core.NaturalLit a -> do
            return (Py.Int { int_value = toInteger a, expr_literal = show a, expr_annot = Py.SpanEmpty })
        -- -- | > NaturalFold                              ~  Natural/fold
        -- | NaturalFold
        -- -- | > NaturalBuild                             ~  Natural/build
        -- | NaturalBuild
        -- -- | > NaturalIsZero                            ~  Natural/isZero
        -- | NaturalIsZero
        -- -- | > NaturalEven                              ~  Natural/even
        -- | NaturalEven
        -- -- | > NaturalOdd                               ~  Natural/odd
        -- | NaturalOdd
        -- -- | > NaturalToInteger                         ~  Natural/toInteger
        -- | NaturalToInteger
        -- -- | > NaturalShow                              ~  Natural/show
        -- | NaturalShow
        -- -- | > NaturalPlus x y                          ~  x + y
        -- | NaturalPlus (Expr s a) (Expr s a)
        Dhall.Core.NaturalPlus a b -> do
            a' <-  loop a
            b' <-  loop b
            return (Py.BinaryOp { operator = Py.Plus Py.SpanEmpty, left_op_arg = a', right_op_arg = b', expr_annot = Py.SpanEmpty })
        -- -- | > NaturalTimes x y                         ~  x * y
        -- | NaturalTimes (Expr s a) (Expr s a)
        Dhall.Core.NaturalPlus a b -> do
            a' <-  loop a
            b' <-  loop b
            return (Py.BinaryOp { operator = Py.Multiply Py.SpanEmpty, left_op_arg = a', right_op_arg = b', expr_annot = Py.SpanEmpty })
        -- -- | > Integer                                  ~  Integer
        -- | Integer
        -- -- | > IntegerLit n                             ~  ±n
        -- | IntegerLit Integer
        Dhall.Core.IntegerLit a -> do
            return (Py.Int { int_value = a, expr_literal = show a, expr_annot = Py.SpanEmpty })
        -- -- | > IntegerShow                              ~  Integer/show
        -- | IntegerShow
        -- -- | > IntegerToDouble                          ~  Integer/toDouble
        -- | IntegerToDouble
        -- -- | > Double                                   ~  Double
        -- | Double
        -- -- | > DoubleLit n                              ~  n
        -- | DoubleLit Double
        Dhall.Core.DoubleLit a
            | isInfinite a && a > 0 -> return (Py.Float { float_value = (1.7976931348623157e308 :: Double), expr_literal = "", expr_annot = Py.SpanEmpty})
            | isInfinite a && a < 0 -> return (Py.Float { float_value = (-1.7976931348623157e308 :: Double), expr_literal = "", expr_annot = Py.SpanEmpty})
            | otherwise -> return (Py.Float { float_value = a, expr_literal = show a, expr_annot = Py.SpanEmpty })
        -- -- | > DoubleShow                               ~  Double/show
        -- | DoubleShow
        -- -- | > Text                                     ~  Text
        -- | Text
        -- -- | > TextLit (Chunks [(t1, e1), (t2, e2)] t3) ~  "t1${e1}t2${e2}t3"
        -- | TextLit (Chunks s a)
        Dhall.Core.TextLit (Dhall.Core.Chunks [] a) -> do
            return (Py.Strings { strings_strings = [unpack a], expr_annot = Py.SpanEmpty})
        -- -- | > TextAppend x y                           ~  x ++ y
        -- | TextAppend (Expr s a) (Expr s a)
        Dhall.Core.TextAppend a b -> do
            a' <-  loop a
            b' <-  loop b
            return (Py.BinaryOp { operator = Py.Plus Py.SpanEmpty, left_op_arg = a', right_op_arg = b', expr_annot = Py.SpanEmpty })
        -- -- | > List                                     ~  List
        -- | List
        -- -- | > ListLit (Just t ) [x, y, z]              ~  [x, y, z] : List t
        -- --   > ListLit  Nothing  [x, y, z]              ~  [x, y, z]
        -- | ListLit (Maybe (Expr s a)) (Seq (Expr s a))
        Dhall.Core.ListLit _ a -> do
            a' <- traverse loop a
            return (Py.List { list_exprs = Data.Foldable.toList a', expr_annot = Py.SpanEmpty })
        -- -- | > ListAppend x y                           ~  x # y
        -- | ListAppend (Expr s a) (Expr s a)
        -- -- | > ListBuild                                ~  List/build
        -- | ListBuild
        -- -- | > ListFold                                 ~  List/fold
        -- | ListFold
        -- -- | > ListLength                               ~  List/length
        -- | ListLength
        -- -- | > ListHead                                 ~  List/head
        -- | ListHead
        -- -- | > ListLast                                 ~  List/last
        -- | ListLast
        -- -- | > ListIndexed                              ~  List/indexed
        -- | ListIndexed
        -- -- | > ListReverse                              ~  List/reverse
        -- | ListReverse
        -- -- | > Optional                                 ~  Optional
        -- | Optional
        -- -- | > OptionalLit t (Just e)                   ~  [e] : Optional t
        -- --   > OptionalLit t Nothing                    ~  []  : Optional t
        -- | OptionalLit (Expr s a) (Maybe (Expr s a))
        Dhall.Core.OptionalLit a _ -> do
            a' <- loop a
            return a'
        -- -- | > Some e                                   ~  Some e
        -- | Some (Expr s a)
        Dhall.Core.Some a -> do
            a' <- loop a
            return a'
        -- -- | > None                                     ~  None
        -- | None
        Dhall.Core.App Dhall.Core.None _ -> do
            return (Py.None { expr_annot = Py.SpanEmpty })
        -- -- | > OptionalFold                             ~  Optional/fold
        -- | OptionalFold
        -- -- | > OptionalBuild                            ~  Optional/build
        -- | OptionalBuild
        -- -- | > Record       [(k1, t1), (k2, t2)]        ~  { k1 : t1, k2 : t1 }
        -- | Record    (Map Text (Expr s a))
        -- -- | > RecordLit    [(k1, v1), (k2, v2)]        ~  { k1 = v1, k2 = v2 }
        -- | RecordLit (Map Text (Expr s a))
        Dhall.Core.RecordLit a ->
            case toOrderedList a of
            --     [ (   "contents"
            --       ,   Dhall.Core.UnionLit alternativeName contents _
            --       )
            --      ,(   "field"
            --       ,   Dhall.Core.TextLit
            --                 (Dhall.Core.Chunks [] field)
            --       )
            --      ,(   "nesting"
            --       ,   Dhall.Core.UnionLit
            --                 "Nested"
            --                 (Dhall.Core.TextLit
            --                     (Dhall.Core.Chunks [] nestedField)
            --                 )
            --                 [ ("Inline", Dhall.Core.Record []) ]
            --       )
            --      ] -> do
            --         contents' <- loop contents
            --
            --         let taggedValue =
            --                 Dhall.Map.fromList
            --                     [   (   field
            --                         ,   loop alternativeName
            --                         )
            --                     ,   (   nestedField
            --                         ,   contents'
            --                         )
            --                     ]
            --         let mapTagged = Dhall.Map.toMap taggedValue
            --         return Py.Dictionary { dict_mappings = [DictKeyDatumList annot], expr_annot = Py.SpanEmpty }
            --
            --     [   (   "contents"
            --         ,   Dhall.Core.UnionLit
            --                 alternativeName
            --                 (Dhall.Core.RecordLit contents)
            --                 _
            --         )
            --      ,  (   "field"
            --         ,   Dhall.Core.TextLit
            --                 (Dhall.Core.Chunks [] field)
            --         )
            --      ,  (   "nesting"
            --         ,   Dhall.Core.UnionLit
            --                 "Inline"
            --                 (Dhall.Core.RecordLit [])
            --                 [ ("Nested", Dhall.Core.Text) ]
            --         )
            --      ] -> do
            --         let contents' =
            --                 Dhall.Map.insert
            --                     field
            --                     (Dhall.Core.TextLit
            --                         (Dhall.Core.Chunks
            --                             []
            --                             alternativeName
            --                         )
            --                     )
            --                     contents
            --
            --         loop (Dhall.Core.RecordLit contents')
                _ -> do
                    a' <- (traverse loop a) :: Either CompileError (Map Text (Py.Expr Py.SrcSpan))
                    -- Py.DictKeyDatumList Py.SrcSpan
                    -- data DictKeyDatumList annot =
                    --  DictMappingPair (Expr annot) (Expr annot)
                    --  | DictUnpacking (Expr annot)
                    --  deriving (Eq,Ord,Show,Typeable,Data,Functor)
                    let mappings = map (\(a, b) -> Py.DictMappingPair a b) $ map (first (\a -> Py.Strings { strings_strings = [unpack a], expr_annot = Py.SpanEmpty})) $ toList a'
                    return Py.Dictionary { dict_mappings = mappings, expr_annot = Py.SpanEmpty }

        -- -- | > Union        [(k1, t1), (k2, t2)]        ~  < k1 : t1 | k2 : t2 >
        -- | Union     (Map Text (Expr s a))
        -- -- | > UnionLit k v [(k1, t1), (k2, t2)]        ~  < k = v | k1 : t1 | k2 : t2 >
        -- | UnionLit Text (Expr s a) (Map Text (Expr s a))
        -- -- | > Combine x y                              ~  x ∧ y
        -- | Combine (Expr s a) (Expr s a)
        -- -- | > CombineTypes x y                         ~  x ⩓ y
        -- | CombineTypes (Expr s a) (Expr s a)
        -- -- | > Prefer x y                               ~  x ⫽ y
        -- | Prefer (Expr s a) (Expr s a)
        -- -- | > Merge x y (Just t )                      ~  merge x y : t
        -- --   > Merge x y  Nothing                       ~  merge x y
        -- | Merge (Expr s a) (Expr s a) (Maybe (Expr s a))
        -- -- | > Constructors e                           ~  constructors e
        -- | Constructors (Expr s a)
        -- -- | > Field e x                                ~  e.x
        -- | Field (Expr s a) Text
        -- -- | > Project e xs                             ~  e.{ xs }
        -- | Project (Expr s a) (Set Text)
        -- -- | > Note s x                                 ~  e
        -- | Note s (Expr s a)
        -- -- | > ImportAlt                                ~  e1 ? e2
        -- | ImportAlt (Expr s a) (Expr s a)
        -- -- | > Embed import                             ~  import
        -- | Embed a

        -- Dhall.Core.UnionLit _ b _ -> loop b
        _ -> Left (Unsupported e)

toOrderedList :: Ord k => Map k v -> [(k, v)]
toOrderedList =
        Data.List.sortBy (Data.Ord.comparing fst)
    .   Dhall.Map.toList

-- | Omit record fields that are @null@
-- omitNull :: Value -> Value
-- omitNull (Object object) =
--     Object (fmap omitNull (Data.HashMap.Strict.filter (/= Null) object))
-- omitNull (Array array) =
--     Array (fmap omitNull array)
-- omitNull (String string) =
--     String string
-- omitNull (Number number) =
--     Number number
-- omitNull (Bool bool) =
--     Bool bool
-- omitNull Null =
--     Null

{-| Specify whether or not to convert association lists of type
    @List { mapKey: Text, mapValue : v }@ to records
-}
data Conversion
    = NoConversion
    | Conversion { mapKey :: Text, mapValue :: Text }

{-| Convert association lists to homogeneous maps

    This converts an association list of the form:

    > [ { mapKey = k0, mapValue = v0 }, { mapKey = k1, mapValue = v1 } ]

    ... to a record of the form:

    > { k0 = v0, k1 = v1 }
-}
convertToHomogeneousMaps :: Conversion -> Expr s X -> Expr s X
convertToHomogeneousMaps NoConversion e0 = e0
convertToHomogeneousMaps (Conversion {..}) e0 = loop (Dhall.Core.normalize e0)
  where
    loop e = case e of
        Dhall.Core.Const a ->
            Dhall.Core.Const a

        Dhall.Core.Var v ->
            Dhall.Core.Var v

        Dhall.Core.Lam a b c ->
            Dhall.Core.Lam a b' c'
          where
            b' = loop b
            c' = loop c

        Dhall.Core.Pi a b c ->
            Dhall.Core.Pi a b' c'
          where
            b' = loop b
            c' = loop c

        Dhall.Core.App a b ->
            Dhall.Core.App a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.Let as b ->
            Dhall.Core.Let as' b'
          where
            f (Dhall.Core.Binding x y z) = Dhall.Core.Binding x y' z'
              where
                y' = fmap loop y
                z' =      loop z

            as' = fmap f as

            b' = loop b

        Dhall.Core.Annot a b ->
            Dhall.Core.Annot a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.Bool ->
            Dhall.Core.Bool

        Dhall.Core.BoolLit a ->
            Dhall.Core.BoolLit a

        Dhall.Core.BoolAnd a b ->
            Dhall.Core.BoolAnd a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.BoolOr a b ->
            Dhall.Core.BoolOr a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.BoolEQ a b ->
            Dhall.Core.BoolEQ a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.BoolNE a b ->
            Dhall.Core.BoolNE a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.BoolIf a b c ->
            Dhall.Core.BoolIf a' b' c'
          where
            a' = loop a
            b' = loop b
            c' = loop c

        Dhall.Core.Natural ->
            Dhall.Core.Natural

        Dhall.Core.NaturalLit a ->
            Dhall.Core.NaturalLit a

        Dhall.Core.NaturalFold ->
            Dhall.Core.NaturalFold

        Dhall.Core.NaturalBuild ->
            Dhall.Core.NaturalBuild

        Dhall.Core.NaturalIsZero ->
            Dhall.Core.NaturalIsZero

        Dhall.Core.NaturalEven ->
            Dhall.Core.NaturalEven

        Dhall.Core.NaturalOdd ->
            Dhall.Core.NaturalOdd

        Dhall.Core.NaturalToInteger ->
            Dhall.Core.NaturalToInteger

        Dhall.Core.NaturalShow ->
            Dhall.Core.NaturalShow

        Dhall.Core.NaturalPlus a b ->
            Dhall.Core.NaturalPlus a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.NaturalTimes a b ->
            Dhall.Core.NaturalTimes a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.Integer ->
            Dhall.Core.Integer

        Dhall.Core.IntegerLit a ->
            Dhall.Core.IntegerLit a

        Dhall.Core.IntegerShow ->
            Dhall.Core.IntegerShow

        Dhall.Core.IntegerToDouble ->
            Dhall.Core.IntegerToDouble

        Dhall.Core.Double ->
            Dhall.Core.Double

        Dhall.Core.DoubleLit a ->
            Dhall.Core.DoubleLit a

        Dhall.Core.DoubleShow ->
            Dhall.Core.DoubleShow

        Dhall.Core.Text ->
            Dhall.Core.Text

        Dhall.Core.TextLit (Dhall.Core.Chunks a b) ->
            Dhall.Core.TextLit (Dhall.Core.Chunks a' b)
          where
            a' = fmap (fmap loop) a

        Dhall.Core.TextAppend a b ->
            Dhall.Core.TextAppend a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.List ->
            Dhall.Core.List

        Dhall.Core.ListLit a b ->
            case transform of
                Just c  -> loop c
                Nothing -> Dhall.Core.ListLit a' b'
          where
            elements = Data.Foldable.toList b

            toKeyValue :: Expr s X -> Maybe (Text, Expr s X)
            toKeyValue (Dhall.Core.RecordLit m) = do
                guard (Data.Foldable.length m == 2)

                key   <- Dhall.Map.lookup mapKey   m
                value <- Dhall.Map.lookup mapValue m

                keyText <- case key of
                    Dhall.Core.TextLit (Dhall.Core.Chunks [] keyText) ->
                        return keyText

                    _ ->
                        empty

                return (keyText, value)
            toKeyValue _ = do
                empty

            transform =
                case elements of
                    [] ->
                        case a of
                            Just (Dhall.Core.Record m) -> do
                                guard (Data.Foldable.length m == 2)
                                guard (Dhall.Map.member mapKey   m)
                                guard (Dhall.Map.member mapValue m)
                                return (Dhall.Core.RecordLit mempty)
                            _ -> do
                                empty

                    _  -> do
                        keyValues <- traverse toKeyValue elements

                        let recordLiteral =
                                Dhall.Map.fromList keyValues

                        return (Dhall.Core.RecordLit recordLiteral)

            a' = fmap loop a
            b' = fmap loop b

        Dhall.Core.ListAppend a b ->
            Dhall.Core.ListAppend a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.ListBuild ->
            Dhall.Core.ListBuild

        Dhall.Core.ListFold ->
            Dhall.Core.ListFold

        Dhall.Core.ListLength ->
            Dhall.Core.ListLength

        Dhall.Core.ListHead ->
            Dhall.Core.ListHead

        Dhall.Core.ListLast ->
            Dhall.Core.ListLast

        Dhall.Core.ListIndexed ->
            Dhall.Core.ListIndexed

        Dhall.Core.ListReverse ->
            Dhall.Core.ListReverse

        Dhall.Core.Optional ->
            Dhall.Core.Optional

        Dhall.Core.OptionalLit a b ->
            Dhall.Core.OptionalLit a' b'
          where
            a' =      loop a
            b' = fmap loop b

        Dhall.Core.Some a ->
            Dhall.Core.Some a'
          where
            a' = loop a

        Dhall.Core.None ->
            Dhall.Core.None

        Dhall.Core.OptionalFold ->
            Dhall.Core.OptionalFold

        Dhall.Core.OptionalBuild ->
            Dhall.Core.OptionalBuild

        Dhall.Core.Record a ->
            Dhall.Core.Record a'
          where
            a' = fmap loop a

        Dhall.Core.RecordLit a ->
            Dhall.Core.RecordLit a'
          where
            a' = fmap loop a

        Dhall.Core.Union a ->
            Dhall.Core.Union a'
          where
            a' = fmap loop a

        Dhall.Core.UnionLit a b c ->
            Dhall.Core.UnionLit a b' c'
          where
            b' =      loop b
            c' = fmap loop c

        Dhall.Core.Combine a b ->
            Dhall.Core.Combine a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.CombineTypes a b ->
            Dhall.Core.CombineTypes a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.Prefer a b ->
            Dhall.Core.Prefer a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.Merge a b c ->
            Dhall.Core.Merge a' b' c'
          where
            a' =      loop a
            b' =      loop b
            c' = fmap loop c

        Dhall.Core.Constructors a ->
            Dhall.Core.Constructors a'
          where
            a' = loop a

        Dhall.Core.Field a b ->
            Dhall.Core.Field a' b
          where
            a' = loop a

        Dhall.Core.Project a b ->
            Dhall.Core.Project a' b
          where
            a' = loop a

        Dhall.Core.ImportAlt a b ->
            Dhall.Core.ImportAlt a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.Note a b ->
            Dhall.Core.Note a b'
          where
            b' = loop b

        Dhall.Core.Embed a ->
            Dhall.Core.Embed a

parseConversion :: Parser Conversion
parseConversion =
        conversion
    <|> noConversion
  where
    conversion = do
        mapKey   <- parseKeyField
        mapValue <- parseValueField
        return (Conversion {..})
      where
        parseKeyField =
            Options.Applicative.strOption
                (   Options.Applicative.long "key"
                <>  Options.Applicative.help "Reserved key field name for association lists"
                <>  Options.Applicative.value "mapKey"
                <>  Options.Applicative.showDefaultWith Data.Text.unpack
                )

        parseValueField =
            Options.Applicative.strOption
                (   Options.Applicative.long "value"
                <>  Options.Applicative.help "Reserved value field name for association lists"
                <>  Options.Applicative.value "mapValue"
                <>  Options.Applicative.showDefaultWith Data.Text.unpack
                )

    noConversion =
        Options.Applicative.flag'
            NoConversion
            (   Options.Applicative.long "noMaps"
            <>  Options.Applicative.help "Disable conversion of association lists to homogeneous maps"
            )

type Value = Py.Expr Py.SrcSpan

{-| Convert a piece of Text carrying a Dhall inscription to an equivalent Python Value


>>> :set -XOverloadedStrings
>>> import Dhall.Core
>>> Dhall.Python.codeToValue "(stdin)" "{ a = 1 }"
>>> Object (fromList [("a",Number 1.0)])
-}
codeToValue
  :: Conversion
  -> Text  -- ^ Describe the input for the sake of error location.
  -> Text  -- ^ Input text.
  -> IO (Value)
codeToValue conversion name code = do
    parsedExpression <- case Dhall.Parser.exprFromText (Data.Text.unpack name) code of
      Left  err              -> Control.Exception.throwIO err
      Right parsedExpression -> return parsedExpression

    resolvedExpression <- Dhall.Import.load parsedExpression

    case Dhall.TypeCheck.typeOf resolvedExpression  of
      Left  err -> Control.Exception.throwIO err
      Right _   -> return ()

    let convertedExpression =
            convertToHomogeneousMaps conversion resolvedExpression

    case dhallToPython convertedExpression of
      Left  err  -> Control.Exception.throwIO err
      Right python -> return python
