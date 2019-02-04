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
        TBool | TNat | TArrow Type Type |TVar String
        deriving (Show, Eq)