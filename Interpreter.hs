{- Programacao Funcional - Trabalho 2 - 
   Prazo de entrega: 19/02/2023 por Tarefa no Aprender 3
   O trabalho deve ser feito individualmente

  ** ENUNCIADO DO TRABALHO:   
 Evolua o interpretador abaixo para prover avaliacao preguicosa (lazy evaluation).
 Ao fim do codigo abaixo, ha alguns casos de testes. 
 Sugere-se observar as dicas neste arquivo e no AbsLI.hs.
 
 Criterios de avaliacao: 
   1) testCaseSuiteResults deve ser computavel e ser True 
   2) para nota maior ou igual a 9, deve-se usar SYB.

-}

module Interpreter where

import AbsLI
import Prelude
import Data.Maybe
import Memo


type Context k v = [(k,v)]                                
type RContext = (VContext, FContext)
type VContext = Context Ident Exp
type FContext = Context Ident Function


-- remover vars da exp
removeV :: VContext -> Exp -> Exp
removeV context (EAdd expR expL) = EAdd (removeV context expR) (removeV context expL)
removeV context (ESub expR expL) = ESub (removeV context expR) (removeV context expL)
removeV context (EMul expR expL) = EMul (removeV context expR) (removeV context expL)
removeV context (EDiv expR expL) = EDiv (removeV context expR) (removeV context expL)
removeV _ (EInt n) = EInt n
removeV context (EVar name) = case lookupMemo name context of
  Just e -> removeV context e
  Nothing -> EVar name
removeV context (EIf e1 e2 e3) = EIf (removeV context e1) (removeV context e2) (removeV context e3)
removeV context (Call func params) = Call func (map (removeV context) params)

-- tentativa de remocao lazy -> primeira tentativa lazy porem esta dando stackoverflow, faz sentido o erro.
-- removeV :: VContext -> Exp -> Exp
-- removeV context exp = case exp of
--   EAdd expR expL -> EAdd (lazyEval expR) (lazyEval expL)
--   ESub expR expL -> ESub (lazyEval expR) (lazyEval expL)
--   EMul expR expL -> EMul (lazyEval expR) (lazyEval expL)
--   EDiv expR expL -> EDiv (lazyEval expR) (lazyEval expL)
--   EInt n -> EInt n
--   EVar name -> case lookupMemo name context of
--     Just e -> lazyEval e
--     Nothing -> EVar name
--   EIf e1 e2 e3 -> EIf (lazyEval e1) (lazyEval e2) (lazyEval e3)
--   Call func params -> Call func (map lazyEval params)
    
-- lazyEval :: Exp -> Exp
-- lazyEval exp = case exp of
--     EAdd expR expL -> EAdd (lazyEval expR) (lazyEval expL)
--     ESub expR expL -> ESub (lazyEval expR) (lazyEval expL)
--     EMul expR expL -> EMul (lazyEval expR) (lazyEval expL)
--     EDiv expR expL -> EDiv (lazyEval expR) (lazyEval expL)
--     EIf e1 e2 e3 -> case lazyEval e1 of
--         EInt 0 -> lazyEval e3
--         _      -> lazyEval e2
--     _ -> exp

evalP :: Program -> Integer
evalP (Prog fs) = eval ([],(updatecF [] fs)) (Call (Ident "main") [])   

-- eval lazy evaluation com removeV
eval :: RContext -> Exp -> Integer
eval context x = case x of
    EAdd exp0 exp  -> let val0 = eval context exp0 in
                        if val0 == 0 then 0 else val0 + eval context exp
    ESub exp0 exp  -> let val0 = eval context exp0 in
                        if val0 == 0 then 0 else val0 - eval context exp
    EMul exp0 exp  -> let val0 = eval context exp0 in
                        if val0 == 0 then 0 else val0 * eval context exp
    EDiv exp0 exp  -> let val0 = eval context exp0 in
                        if val0 == 0 then 0 else val0 `div` eval context exp
    EInt n         -> n

    EVar id        -> val where
      varExp = fromJust (lookupMemo id (fst context))
      val = eval context varExp
      (vContext, fContext) = context

    EIf e1 e2 e3   -> let cond = eval context e1 in
                        if cond /= 0 then eval context e2 else eval context e3

    Call func params -> val where
      (prevVCont, prevFCont) = context
      Fun _ funcParams funcExp = fromJust (lookupMemo func prevFCont)
      paramBindings = zip funcParams (map (removeV prevVCont) params)
      val = eval (paramBindings, prevFCont) funcExp
  

updatecF :: FContext -> [Function] -> FContext
updatecF ctx [] = ctx
updatecF ctx (f@(Fun id _ _):fs) = updatecF (updateMemo ctx id f) fs


{-
  main () {
    fat (5)
  }
  
  fat (n) {
    if (n) 
      then n * fat (n - 1) 
    else 1
  }
-}

fat = 
  Prog [
    Fun (Ident "main") [] (Call (Ident "fat") [EInt 5]),

    Fun (Ident "fat") [Ident "n"] (
      EIf (EVar (Ident "n"))
        (EMul (EVar (Ident "n"))
        (Call (Ident "fat") [ESub (EVar (Ident "n")) (EInt 1)]))
      (EInt 1)
    )
  ]

testCaseFat = evalP fat == 120


{-
 main () {
   fib (8)
}
 fib (n) {
   if (n) then 
      if (n - 1) 
        then fib (n - 1) + fib (n - 2) 
        else 1 
    else 1
}
-}

fibo =  
  Prog [
    Fun (Ident "main") [] (
      Call (Ident "fib") [EInt 8]
    ),
    Fun (Ident "fib") [Ident "n"] (
      EIf (EVar (Ident "n")) 
        (EIf (ESub (EVar (Ident "n")) (EInt 1)) 
          (EAdd (Call (Ident "fib") [ESub (EVar (Ident "n")) (EInt 1)]) 
          (Call (Ident "fib") [ESub (EVar (Ident "n")) (EInt 2)])) 
        (EInt 1)) 
      (EInt 1)
    )
  ]

testCaseFibo = evalP fibo == 34

-- testCaseSuiteResults deve ser true 
testCaseSuiteResults  = testCaseFat && testCaseFibo
