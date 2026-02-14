-- this is the state we'll use for the uniquefy pass, it has
-- an environment which we'll adjust as we enter let expressions
-- and an Integer which will be used for generating symbol names
data UniquifyState = UState (Env String) Integer

-- a uniquify result is a compiler result with our state and an N1 Exp
type UniquifyResult = CompilerResult UniquifyState Exp

-- a helper function to pull out the result from the compiler result
getResult :: CompilerResult UniquifyState N1 -> Result N1
getResult (CState _ (Right p)) = Right p
getResult (CState _ err) = err


{--
  The uniquify pass transforms an N1 program into another
  N1 program, but ensures that every variable name is unique!
--}
uniquify :: N1 -> CompilerResult UniquifyState N1
uniquify (Program exp) =
  case uniquifyExp exp (UState Env.makeEnv 0) of
    CState state (Right exp') -> CState state $ Right $ Program exp'
    CState state (Left msg) -> CState state $ Left msg  



{--
  uniquifyExp transforms an expression in N1 to another
  expression in N1 where the variables have been renamed. We
  pass UniquifyState along with the Exp and get a result. Note
  that this result also contains a UniquifyState, which allows
  us to update it from calls down further in the AST as we 
  recurse through it. 
--}
uniquifyExp :: Exp -> UniquifyState -> UniquifyResult

-- uniquifying an Int is simple, it doesn't change the state,
-- and just returns the int expression
uniquifyExp v@(Int _) state = CState state $ Right v

-- uniquifying Read is also straightforward, the state doesn't change
uniquifyExp r@Read state = CState state $ Right r

-- uniquifying a Negate requires uniquifying its subexpression, which
-- then requires 
uniquifyExp (Negate exp) state =
  case uniquifyExp exp state of
    CState state' (Right res) -> CState state' $ Right $ Negate res
    err -> err

-- You must fill out the following:
-- uniquifyExp (Add x y) state = ...
-- uniquifyExp (Var sym) state = ...
-- uniquifyExp (Let sym exp body) state = ...