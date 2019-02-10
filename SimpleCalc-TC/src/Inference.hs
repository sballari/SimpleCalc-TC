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
            if mtx == Nothing then enrichError ("var "++x++"not in the context") >>= (\x -> return Nothing)
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


    typeOf ctx (ESum m1 m2) = 
        do 
            mt1 <- typeOf ctx m1
            mt2 <- typeOf ctx m2 
            case pure(\x y -> x <= TNat && y <=TNat) <*> mt1 <*> mt2 of 
                Nothing -> enrichError ("impossible to type a operator of the sum") >>= \x -> return Nothing   
                Just True -> return (Just TNat)
                Just False -> enrichError ("one of the operator is not a subtype of TNat") >>= \x -> return Nothing   

    typeOf ctx (EBool b) = return (Just TBool)
    typeOf ctx (ENum n) = return (Just TNat)

    lub :: Type -> Type -> Maybe Type 
    lub TBool TBool = Just TBool
    lub TNat TNat = Just TNat
    lub TNat TBool = Just TBool -- added
    lub TBool TNat = Just TBool -- added
    lub (TArrow s1 s2) (TArrow t1 t2) = pure(TArrow) <*> glb s1 t1 <*> lub s2 t2 
    lub _ _ = Just Top

    glb :: Type -> Type -> Maybe Type 
    glb Top t = Just t 
    glb TNat TBool = Just TNat --added
    glb TBool TNat = Just TNat --added
    glb TBool TBool = Just TBool
    glb TNat TNat = Just TNat
    glb (TArrow s1 s2) (TArrow t1 t2) = pure(TArrow) <*> lub s1 t1 <*> glb s2 t2 
    glb _ _ = Nothing

    



