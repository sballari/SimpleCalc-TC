module SCStructures where 

    type VName = String
    

    data Term =
        EVar VName -- variabili 
        | ENum Int | EBool Bool -- costanti intere e booleane
        | ESum Term Term | EMin Term Term -- operazioni intere
        | ECond Term Term Term -- condizionale
        | EAp (Term) (Term) -- applicazione di funzione       
        | Efn VName Type (Term) -- dichiarazione di funzione
        deriving (Show, Eq)

    data Type =
        TBool | TNat | TArrow Type Type | Top
        deriving (Show, Eq)

    instance Ord Type where
        --subtyping checker
        _ <= Top = True
        TNat <= TBool = True
        TNat <= TNat = True
        TBool <= TBool = True
        TArrow s1 s2  <= TArrow t1 t2 = (t1 <= s1) && (s2 <= t2)
        _ <= _ = False

        s < t = (s<=t) && (s/=t)
        s > t = t < s 
        s >= t = t <= s

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