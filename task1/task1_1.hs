data Term = IntConstant{ intValue :: Int }
          | Variable{ varName :: String }
          | UnaryTerm{ op :: Operator, hv :: Term} 
          | BinaryTerm{ lhv :: Term, op :: Operator, rhv :: Term } deriving(Show,Eq)

data Operator = Sum | Minus | Mult deriving(Show, Eq)

infixl 6 <+>, <->
infixl 7 <*>
infixl 8 -

(<+>), (<*>), (<->) :: Term -> Term -> Term
(-) :: Term -> Term

a <+> b = BinaryTerm a Sum b
a <-> b = BinaryTerm a Minus b
a <*> b = BinaryTerm a Mult b
(-) a = UnaryTerm Minus a

replaceVar :: String -> Term -> Term -> Term
replaceVar _ _ ic@(IntConstant _) = ic
replaceVar name term (Variable varName) 
          | name == varName  = term
          | otherwise = Variable varName
replaceVar name term (BinaryTerm lhv op rhv) = let replaceVarWith = replaceVar name term in
          BinaryTerm (replaceVarWith lhv) op (replaceVarWith rhv)
replaceVar name term (UnaryTerm op hv) = UnaryTerm op (replaceVar name term hv)