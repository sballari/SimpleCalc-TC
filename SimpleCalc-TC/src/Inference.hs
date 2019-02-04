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
    inference :: Term -> [TConstraint] -> [TAssertion] -> [TConstraint]
    inference m e [] = e -- no goals, i have done. E is the final constraints lists
    inference m e (g:gs) = let (pr,cons) = fst (app (actionsTable g) 0) in inference m (cons++e) (pr++gs)


    
    
    
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

    actionsTable :: TAssertion -> ST ([TAssertion],[TConstraint])

    -- actionsTable (a, EVar x, t) = let Maybe r = (lookup x a)  in  ([],[TEq t  r])
    actionsTable (a, ENum n, t) = return ([], [TEq t TNat])
    actionsTable (a, EBool b, t) = return ([], [TEq t  TBool])
    actionsTable (a, ESum t1 t2, t) = return ( [(a,t1,TNat), (a,t1,TNat)], [TEq t TNat])
    actionsTable (a, EMin t1 t2, t) = return ( [(a,t1, TNat), (a,t1,TNat)], [TEq t TNat])
    actionsTable (a, ECond t1 t2 t3, t) = return ([(a,t1, TBool), (a,t2,t), (a,t3,t)], [])
    actionsTable (a, EAp t1 t2, t) = do 
                                        n1 <- fresh 
                                        n2 <- fresh
                                        return ([(a,t1, (TArrow n1 n2 )), (a,t2,n1)], [TEq t n2])
    actionsTable (a, (Efn var s1 m), t) = return ([(a++[(var,s1)], m, t)], [])
   