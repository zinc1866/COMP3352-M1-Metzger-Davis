module N1Passes where

-- imports for writing our N1 -> N1 passes
import N1
import Env
import CompilerPasses

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
    CState state (Right exp') -> CState state $ Right $ N1 exp'
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

uniquifyExp (Add x y) state =
  case uniquifyExp x state of
    CState state' (Right x') ->
      case uniquifyExp y state' of
        CState state'' (Right y') -> CState state'' $ Right $ Add x' y'
        err -> err
    err -> err

uniquifyExp (Var sym) (UState env counter) = 
  case lookupEnv sym env of  
    Just newName -> CState (UState env counter) $ Right(Var newName)
    Nothing ->  CState (UState env counter) $ Left ("Symbol '" ++ sym ++ "' not found")

uniquifyExp (Let sym exp body) (UState env counter) =
  let freshName = "s" ++ show counter                   
  in case uniquifyExp exp (UState env (counter + 1)) of 
    CState (UState env' counter') (Right exp') ->
      let env'' = extendEnv sym freshName env'           
      in case uniquifyExp body (UState env'' counter') of 
        CState state'' (Right body') -> CState state'' $ Right $ Let freshName exp' body'
        err -> err
    err -> err


  -- we will have slightly different types this time around, but still very similar:
type RCOState = Integer
type RCOResult = CompilerResult RCOState Exp


atm ::= Int Int64 | Var String
exp ::= atm | Read | Negate atm | Add atm atm | Let String exp exp 
N1' ::= Program exp 
-- we're changing our pass slightly to simply take a CompilerResult as an argument
-- and return a CompilerResult
passRemoveComplexOperas :: CompilerResult RCOState N1 -> CompilerResult RCOState N1

-- straightforward since we just have one kind of program that contains an expression
passRemoveComplexOperas (CState symCount (Right (Program expr))) =
    case rcoExp (CState symCount (Right expr)) of
      CState symCount (Right exp') -> CState symCount $ Right $ Program exp'
      CState symCount (Left msg) -> CState symCount $ Left msg

-- rcoExp walks through expressions that can be complex or atomic
rcoExp :: RCOResult -> RCOResult  
rcoExp atm@(CState _ (Right Read)) = atm
rcoExp atm@(CState _ (Right (Int _))) = atm
rcoExp atm@(CState _ (Right (Var _))) = atm
-- let expressions, subexpressions can be atomic or complex
rcoExp (CState state (Right (Let sym expr body))) =
  case rcoExp (CState state (Right expr)) of
    CState sc' (Right expr') ->
      case rcoExp (CState sc' (Right body)) of
        CState sc'' (Right body') ->
          CState sc'' $ Right $ Let sym expr' body'
        err -> err
    err -> err
-- negate expressions, which need atomic subexpressions
rcoExp (CState state (Right (Negate expr))) = 

    let res = rcoExp (CState state (Right expr)) in
    case res of
      CState st (Right e) ->
        let (atom, binds) = rcoAtom st e in
        if null binds
          then CState st (Right (Negate atom))
          else -- introduce let-bindings for any non-atomic subexpressions
            let (name, expr1) = head binds in
            CState st (Right (Let name expr1 (Negate (Var name))))
      CState st (Left msg) -> CState st (Left msg)



<<<<<<< Updated upstream
-- add expressions, subexpressions must be atomicrcpExp
rcoExp (CState state (Right (Add x y)))  = 


  rcoExp (CState state (Right (Let sym expr body))) =
  case rcoExp (CState state (Right expr)) of
    CState sc' (Right expr') ->
      case rcoExp (CState sc' (Right body)) of
        CState sc'' (Right body') ->
          CState sc'' $ Right $ Let sym expr' body'
=======
rcoExp (CState state (Right (Add x y))) =
  case rcoAtm (CState state (Right (x, []))) of
    CState sc' (Right (atom1, bindings1)) ->
      case rcoAtm (CState sc' (Right (y, bindings1))) of
        CState sc'' (Right (atom2, bindings2)) ->
          CState sc'' $ Right $ wrapBindings bindings2 (Add atom1 atom2)
>>>>>>> Stashed changes
        err -> err
    err -> err

-- pass errors up
rcoExp (CState _ (Left msg)) = CState 0 (Left msg)

-- Define a type for our atomic expression operations. This type returns a pair, which
-- is either a new Var, if needed, or the old expression. The list is then a list of name
-- binding pairs, where the binding is an expression. The idea here is that when this function
-- returns to rcoExp, you'll create a bunch of let bindings from this list, and Exp is sub'd
-- wherever this was called from
type AtmResult = CompilerResult RCOState (Exp, [(String, Exp)])
rcoAtm :: AtmResult -> AtmResult
rcoAtm res@(CState _ (Right (Int _, _))) = res 
rcoAtm res@(CState _ (Right (Read, _))) = res
rcoAtm res@(CState _ (Right (Var _, _))) = res

-- negate expressions
rcoAtm (CState symCount (Right (Negate expr, lst))) = 
  case rcoAtm (CState symCount (Right (expr, lst))) of
  CState symCount' (Right (atomExpr, bindings)) ->
    let freshName = "s" ++ show symCount'
    in CState (symCount' + 1) $ Right (Var freshName, bindings ++ [(freshName, Negate atomExpr)])



-- add expressions, here, both subexpressions must be atoms too
rcoAtm (CState symCount (Right (Add e1 e2, lst))) =
  case rcoAtm (CState symCount (Right (e1, lst))) of
    CState sc' (Right (atom1, bindings1)) ->
      case rcoAtm (CState sc' (Right (e2, bindings1))) of
        CState sc'' (Right (atom2, bindings2)) ->
          let fresh = "s" ++ show sc''
          in CState (sc'' + 1) $ Right (Var fresh, bindings2 ++ [(fresh, Add atom1 atom2)])
        err -> err
    err -> err


-- let expressions, either of these expressions, expr or body,
-- can be atomic or complex
rcoAtm (CState symCount (Right (Let sym expr body, lst))) =
  case rcoExp (CState symCount (Right expr)) of
    CState sc' (Right expr') ->
      case rcoAtm (CState sc' (Right (body, lst))) of
        CState sc'' (Right (atomBody, bindings)) ->
          CState sc'' $ Right (atomBody, [(sym, expr')] ++ bindings)
        err -> err
    err -> err  