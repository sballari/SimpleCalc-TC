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
        TBool | TNat | TArrow Type Type | Top | TVar String
        deriving (Show, Eq)

    instance Ord Type where
        --subtyping checker
        _ < Top = True
        TNat < TBool = True
        TArrow s1 s2  < TArrow t1 t2 = (t1 < s1) && (s2 <t2)
        _ < _ = False

        s <= t = s<t || s==t
        s > t = t < s 
        s >= t = t <= s
