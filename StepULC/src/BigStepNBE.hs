{-# language Strict, DeriveGeneric #-}

module BigStepNBE where

import GHC.Generics
import Control.DeepSeq

data Tm  = Var Int | App Tm Tm | Lam Tm | Fix Tm deriving (Show, Eq, Generic)
data Val = VVar Int | VApp Val Val | VLam Env Tm | VFix Env Tm
data Env = Nil | (:>) Env ~Val
infixl 3 :>

instance NFData Tm

var :: Env -> Int -> Val
var (_  :> v) 0 = v
var (vs :> _) x = var vs (x - 1)
var _         _ = undefined

isCanonical :: Val -> Bool
isCanonical VLam{} = True
isCanonical _      = False

eval :: Env -> Tm -> Val
eval vs (Var x)   = var vs x
eval vs (App t u) = case (eval vs t, eval vs u) of
                      (VLam vs t, ~u)                 -> eval (vs :> u) t
                      (VFix vs t,  u) | isCanonical u -> eval (vs :> VFix vs t :> u) t
                      (t,          u)                 -> VApp t u
eval vs (Lam t)   = VLam vs t
eval vs (Fix t)   = VFix vs t

quote :: Int -> Val -> Tm
quote l (VVar x)    = Var (l - x - 1)
quote l (VApp t u)  = App (quote l t) (quote l u)
quote l (VLam vs t) = Lam (quote (l + 1) (eval (vs :> VVar l) t))
quote l (VFix vs t) = Fix (quote (l + 2) (eval (vs :> VVar l :> VVar (l + 1)) t))

nf :: Tm -> Tm
nf = quote 0 . eval Nil

--------------------------------------------------------------------------------

instance Num Tm where
  fromInteger = Var . fromInteger
  (+) = undefined; (*) = undefined
  abs = undefined; signum = undefined; (-) = undefined

($$) = App
infixl 7 $$

-- Church natural numbers
z   = Lam $ Lam $ 0
s   = Lam $ Lam $ Lam $ 1 $$ (2 $$ 1 $$ 0)
add = Lam $ Lam $ Lam $ Lam $ 3 $$ 1 $$ (2 $$ 1 $$ 0)
mul = Lam $ Lam $ Lam $ 2 $$ (1 $$ 0)

n5   = s $$ (s $$ (s $$ (s $$ (s $$ z))))
n10  = add $$ n5 $$ n5
n100 = mul $$ n10 $$ n10
n10k = mul $$ n100 $$ n100

intToChurch :: Int -> Tm
intToChurch n = Lam (Lam (go n (Var 0))) where
  go 0 t = t
  go n t = go (n - 1) (App (Var 1) t)

churchToInt :: Tm -> Int
churchToInt (Lam (Lam t)) = go 0 t where
  go acc (Var 0)   = acc
  go acc (App _ t) = go (acc + 1) t
  go _   _         = undefined

pred'  = Lam $ Lam $ Lam $ 2 $$ (Lam $ Lam $ 0 $$ (1 $$ 3)) $$ (Lam 1) $$ (Lam 0)
true   = Lam $ Lam $ 1
false  = Lam $ Lam $ 0
isZero = Lam $ Lam $ Lam $ 2 $$ Lam 1 $$ 1
fac    = Fix $ isZero $$ 0 $$ (s$$z) $$ (mul $$ 0 $$ (1 $$ (pred' $$ 0)))
facN n = fac $$ intToChurch n
