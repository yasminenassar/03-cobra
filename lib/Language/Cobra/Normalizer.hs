--------------------------------------------------------------------------------
-- | This module contains the code for converting an `Expr` to a "A-Normal" form.
--------------------------------------------------------------------------------

module Language.Cobra.Normalizer ( anormal ) where

import           Language.Cobra.Types

type Binds a = [(Bind a, (AnfExpr a, a))]

--------------------------------------------------------------------------------
-- | Convert an Expr into A-Normal Form
--------------------------------------------------------------------------------
anormal :: Expr SourceSpan -> AnfExpr SourceSpan
--------------------------------------------------------------------------------
anormal = snd . (anf 0)


--------------------------------------------------------------------------------
-- | `anf i e` takes as input a "start" counter `i` and expression `e` and
--   returns an output `(i', e')` where
--   * `i'` is the output counter (i.e. i' - i) anf-variables were generated,
--   * `e'` is equivalent to `e` but is in A-Normal Form.
--------------------------------------------------------------------------------
anf :: Int -> Expr a -> (Int, AnfExpr a)
--------------------------------------------------------------------------------
anf i (Number n l)      = (i, Number n l)

anf i (Boolean b l)     = (i, Boolean b l)

anf i (Id     x l)      = (i, Id     x l)

anf i (Let x e b l)     = (i2, Let x e1 e2 l)
  where
    (i1, e1)            = anf i e
    (i2, e2)            = anf i1 b

anf i (Prim1 o e l)     = (i', stitch bs  (Prim1 o ae l))
  where
    (i', bs, ae)        = imm i e

anf i (Prim2 o e1 e2 l) = (i'', stitch (bs2++bs1) (Prim2 o v1 v2 l))
  where
    (i', bs1, v1)  = imm i e1
    (i'', bs2, v2) = imm i' e2

anf i (If c e1 e2 l)    = (i''', stitch bs  (If c' e1' e2' l))
  where
    (i'  , bs, c')      = imm i   c
    (i'' ,     e1') = anf i'  e1
    (i''',     e2') = anf i'' e2

--------------------------------------------------------------------------------
-- | `stitch bs e` takes a "context" `bs` which is a list of temp-vars and their
--   definitions, and an expression `e` that uses the temp-vars in `bs` and glues
--   them together into a `Let` expression.
--   NOTE: the binders are in reverse order.
--------------------------------------------------------------------------------
stitch :: Binds a -> AnfExpr a -> AnfExpr a
--------------------------------------------------------------------------------
stitch bs e = bindsExpr [ (x, xe) | (x, (xe, _)) <- reverse bs] e (getLabel e)

--------------------------------------------------------------------------------
-- | `imms i es` takes as input a "start" counter `i` and expressions `es`, and
--   and returns an output `(i', bs, es')` where
--   * `i'` is the output counter (i.e. i'- i) anf-variables were generated
--   * `bs` are the temporary binders needed to convert `es` to immediate vals
--   * `es'` are the immediate values  equivalent to es
--------------------------------------------------------------------------------
imms :: Int -> [Expr a] -> (Int, Binds a, [ImmExpr a])
--------------------------------------------------------------------------------
imms i []           = (i, [], [])
imms i (e:es)       = (i'', bs' ++ bs, e' : es' )
  where
    (i' , bs , e' ) = imm  i  e
    (i'', bs', es') = imms i' es

--------------------------------------------------------------------------------
-- | `imm i e` takes as input a "start" counter `i` and expression `e` and
--   returns an output `(i', bs, e')` where
--   * `i'` is the output counter (i.e. i' - i) anf-variables were generated,
--   * `bs` are the temporary binders needed to render `e` in ANF, and
--   * `e'` is an `imm` value (Id, Boolean, or Number) equivalent to `e`.
--------------------------------------------------------------------------------
imm :: Int -> Expr a -> (Int, Binds a, ImmExpr a)
--------------------------------------------------------------------------------
imm i (Number n l)      = (i, [], Number n l)

imm i (Id x l)          = (i, [], Id x l)

imm i (Boolean b  l)    = (i, [], Boolean b l)


imm i (Prim1 o e1 l)    = (i'', bs, mkId v l)
  where
    (i' , b1s, v1)      = imm i e1
    (i'', v)            = fresh l i'
    bs                  = (v, (Prim1 o v1 l, l)) : b1s

imm i (Prim2 o e1 e2 l) = (i''', (newBind, (Prim2 o v1 v2 l, l)):bs2++bs1, 
                           mkId newBind l)
  where
    (i', bs1, v1)       = imm i e1
    (i'', bs2, v2)      = imm i' e2
    (i''', newBind)     = fresh l i''  

imm i e@(If _ _ _  l)   = immExp i e l

imm i e@(Let _ _ _ l)   = immExp i e l


immExp :: Int -> Expr a -> a -> (Int, Binds a, ImmExpr a)
immExp i e l  = (i'', bs, mkId v l)
  where
    (i' , e') = anf i e
    (i'', v)  = fresh l i'
    bs        = [(v, (e', l))]

mkId :: Bind a -> a -> Expr a
mkId x l = Id (bindId x) l

--------------------------------------------------------------------------------
-- | `fresh i` returns a temp-var named `i` and "increments" the counter
--------------------------------------------------------------------------------
fresh :: a -> Int -> (Int, Bind a)
--------------------------------------------------------------------------------
fresh l i = (i + 1, Bind x l)
  where
    x     = "anf" ++ show i
