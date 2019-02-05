module Inference where 
    import SCStructures

    type TVar = String -- type variable
    type TAssumption = [(VName, Type)] -- Contest
    type TAssertion = (TAssumption, Term, Type)
    data TConstraint = TEq Type Type | TSub Type Type


    
    -- fInference :: Term -> Type
    -- fInference M = inference M [] [(A0,M,R)]

    {-
     M is the term
     G is a set of subgoals [TAssertion] "premesse di una regola"
     E is a set of constraints on the type variables
    -}
    inference' :: Term -> Maybe ([TConstraint])
    inference' m = fst (app (inference m [] [([],m,(TVar "r"))]) 0)


    inference :: Term -> [TConstraint] -> [TAssertion] -> ST (Maybe ([TConstraint]))
    -- l'algoritmo fallisce se nel contesto non e' presente una variabile
    inference m e [] = return (Just e) -- no goals, i have done. E is the final constraints lists
    inference m e (g:gs) = do  
                                mr <- actionsTable g
                                case mr of 
                                    Just (pr,cons) -> inference m (cons++e) (pr++gs)
                                    Nothing -> return Nothing
                                


    type State = Int
    newtype ST a = S (State -> (a,State))
    app :: ST a -> State -> (a,State)
    app (S sta) s = sta s

    instance Functor ST where 
        fmap g st = S (\s-> let (x,s')= app st s in (g x, s'))
    instance Applicative ST where
        pure x = S (\s -> (x,s))
        stf <*> sta = S (\s-> 
            let (f,s')= app stf s 
                (x,s'')= app sta s' in (f x , s'') )
    instance Monad ST where
        sta >>= f = S (\s -> let (a,s') = app sta s in app (f a) s')
    

    fresh :: ST Type 
    fresh = S(\s->( (TVar('t':(show s))) ,s+1))

    actionsTable :: TAssertion -> ST (Maybe ([TAssertion],[TConstraint]))
        -- l'algoritmo fallisce se c'e' una variabile libera (fallisce Evar)
        -- lo ST pattern e' usato per avere type varibili fresche.
    actionsTable (a, EVar x, t) = return (do tx <- lookup x a; Just ([],[TEq t tx]))
    -- leggo dal contesto e metto il vincolo
    actionsTable (a, ENum n, t) = return (Just ([], [TEq t TNat]))  
    actionsTable (a, EBool b, t) = return (Just ([], [TEq t  TBool]))
    actionsTable (a, ESum t1 t2, t) = return (Just ( [(a,t1,TNat), (a,t1,TNat)], [TEq t TNat]))
    actionsTable (a, EMin t1 t2, t) = return (Just ( [(a,t1, TNat), (a,t1,TNat)], [TEq t TNat]))
    actionsTable (a, ECond t1 t2 t3, t) = return (Just ([(a,t1, TBool), (a,t2,t), (a,t3,t)], []))
    actionsTable (a, EAp t1 t2, t) = do 
                                        n1 <- fresh 
                                        n2 <- fresh
                                        return (Just ([(a,t1, (TArrow n1 n2 )), (a,t2,n1)], [TEq t n2]))
    actionsTable (a, (Efn var s1 m), t) = return (Just ([(a++[(var,s1)], m, t)], []))
   