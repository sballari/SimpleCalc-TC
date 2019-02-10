module Inference where 
    import SCStructures

    type Contest = [(VName, Type)] -- Contest


    type Error = [String]
    newtype ST a = ST (Error -> (a,Error))
    app :: ST a -> Error -> (a,Error)
    app (ST sta) s = sta s

    instance Functor ST where 
        fmap g st = ST (\s-> let (x,s')= app st s in (g x, s'))
    instance Applicative ST where
        pure x = ST (\s -> (x,s))
        stf <*> sta = ST (\s-> 
            let (f,s')= app stf s 
                (x,s'')= app sta s' in (f x , s'') )
    instance Monad ST where
        sta >>= f = ST (\s -> let (a,s') = app sta s in app (f a) s')


    enrichError :: String -> ST ()
    enrichError err = ST(\e-> ((),e++[err]))

    typeOf :: Contest -> Term -> ST (Maybe Type)
    typeOf ctx (EVar x) =
            let mtx = lookup x ctx in      
            if mtx == Nothing then enrichError ("var "++x++"not in the context") >>= (\x -> return mtx)
            else return mtx    
            
    typeOf ctx (Efn x s term) = do  mt <- typeOf ((x,s):ctx) term 
                                    case mt of 
                                        Nothing -> return Nothing
                                        Just t -> return (Just (TArrow s t))
    typeOf ctx (EAp m1 m2) = 
        do 
            mt1 <- typeOf ctx m1 
            mt2 <- typeOf ctx m2
            case mt1 of 
                Nothing -> return Nothing
                Just (TArrow t11 t12) -> case mt2 of 
                                            Nothing -> return Nothing
                                            Just t2 -> if t2 <= t11     
                                                        then return (Just t12)
                                                        else enrichError ("parametrer type mismatch") >>= \x -> return Nothing 
                _ -> enrichError("arrow type expected") >>= \x-> return Nothing
    
    typeOf ctx (ECond bexp m1 m2) = 
        do 
            mt1 <- typeOf ctx m1
            mt2 <- typeOf ctx m2
            case mt1 of 
                Nothing -> enrichError ("first branch typing error") >>= \x -> return Nothing
                Just t1 -> case mt2 of 
                                Nothing -> enrichError ("second branch typing error") >>= \x -> return Nothing
                                Just t2 -> let tif = lub t1 t2 in
                                            if tif == Nothing
                                                then enrichError ("impossible joint of if stm") >>= \x -> return Nothing
                                                else return tif


                                    

    lub :: Type -> Type -> Maybe Type 
    lub TBool TBool = Just TBool
    lub TNat TBool = Just TBool -- added
    lub (TArrow s1 s2) (TArrow t1 t2) = pure(TArrow) <*> glb s1 t1 <*> lub s2 t2 
    lub _ _ = Just Top

    glb :: Type -> Type -> Maybe Type 
    glb Top t = Just t 
    glb TNat TBool  = Just TNat --added
    glb TBool TBool = Just TBool
    glb (TArrow s1 s2) (TArrow t1 t2) = pure(TArrow) <*> lub s1 t1 <*> glb s2 t2 
    glb _ _ = Nothing

    



{-
    type TVar = String -- type variable
    
    data TAssertion = Tass TAssumption Term Type | TSub Type Type deriving (Show, Eq)
    data TConstraint = TEq Type Type  deriving (Show, Eq)


    
    -- fInference :: Term -> Type
    -- fInference M = inference M [] [(A0,M,R)]

    {-
     M is the term
     G is a set of subgoals [TAssertion] "premesse di una regola"
     E is a set of constraints on the type variables
    -}
    inference' :: Term -> Maybe ([TConstraint])
    inference' m = fst (app (inference m [] [(Tass [] m (TVar "r"))]) 0)


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
    actionsTable (Tass a (EVar x) t) = return (do s <- lookup x a; Just ([TSub s t],[]))
    -- leggo dal contesto e metto il vincolo
    actionsTable (Tass a (ENum n) t) = return (Just ([], [TEq t TNat]))  
    actionsTable (Tass a (EBool b) t) = return (Just ([], [TEq t  TBool]))
    actionsTable (Tass a (ESum t1 t2) t) = return (Just ( [(Tass a t1 TNat), (Tass a t1 TNat)], [TEq t TNat]))
    actionsTable (Tass a (EMin t1 t2) t) = return (Just ( [(Tass a t1 TNat), (Tass a t1 TNat)], [TEq t TNat]))
    actionsTable (Tass a (ECond t1 t2 t3) t) = return (Just ([(Tass a t1 TBool), (Tass a t2 t), (Tass a t3 t)], []))
    actionsTable (Tass a (EAp t1 t2) t) = do 
                                        n1 <- fresh 
                                        n2 <- fresh
                                        return (Just ([(Tass a,t1, (TArrow n1 n2 )), (Tass a t2 n1)], [TEq t n2]))
    actionsTable Tass(a, (Efn var s1 m), t) = return (Just ([(a++[(var,s1)], m, t)], []))
   -}