module MTL where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

type Name = String

data Exp = Lit Integer
            | Var Name
            | Plus Exp Exp
            | Abs Name Exp
            | App Exp Exp
                deriving Show

data Value = IntVal Integer 
            | FunVal Env Name Exp 
                deriving Show

type Env = Map.Map Name Value

eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2 ) = let IntVal i1 = eval0 env e1
                              IntVal i2 = eval0 env e2
                          in IntVal (i1 + i2 )
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2 ) = let val1 = eval0 env e1
                             val2 = eval0 env e2
                         in case val1 of
                             FunVal env0 n body -> eval0 (Map.insert n val2 env0) body

-- 12 + ((L x -> x)(4 + 2))
exampleExp1 = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

-- ((Lx -> x + 2 + x)(2)) + ((Ly -> y + y)(3))
exampleExp2 = (App (Abs "x" ((Var "x") `Plus` (Lit 2) `Plus` (Var "x")))(Lit 2)) `Plus` (App (Abs "y" ((Var "y") `Plus` (Var "y"))) (Lit 3))



factorial :: Integer -> Integer 
factorial n = let (fact, _) = go (1, n)
            in fact
    where
        go (part, 0) = (part, 0)
        go (part, counter) = go (part * counter, counter - 1)


factorial2 :: Integer -> Integer 
factorial2 n = go n
    where
        go 0 = 1
        go n = n * go (n-1)

factorial3 :: Integer -> State Integer ()
factorial3 n = do
    current <- get
    _ <- put $ n * current
    factorial3 $ n - 1