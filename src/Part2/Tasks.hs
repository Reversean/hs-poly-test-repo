module Part2.Tasks where

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term
  = IntConstant {intValue :: Int} -- числовая константа
  | Variable {varName :: String} -- переменная
  | BinaryTerm {op :: BinaryOp, lhv :: Term, rhv :: Term} -- бинарная операция
  deriving (Show, Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus

infixl 6 |+|

(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus

infixl 6 |-|

(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times

infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement (Variable varName')
  | varName == varName' = replacement
  | otherwise = Variable varName'
replaceVar varName replacement (BinaryTerm op lhv rhv) = BinaryTerm op replaceLhv replaceRhv
  where
    replaceVar' = replaceVar varName replacement
    replaceLhv = replaceVar' lhv
    replaceRhv = replaceVar' rhv
replaceVar _ _ expression = expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm Plus (IntConstant lhv) (IntConstant rhv)) = IntConstant (lhv + rhv)
evaluate (BinaryTerm Minus (IntConstant lhv) (IntConstant rhv)) = IntConstant (lhv - rhv)
evaluate (BinaryTerm Times (IntConstant lhv) (IntConstant rhv)) = IntConstant (lhv * rhv)
evaluate term@(BinaryTerm op lhv@BinaryTerm {} rhv)
  | lhv' == lhv = term
  | otherwise = evaluate (BinaryTerm op lhv' rhv)
  where
    lhv' = evaluate lhv
evaluate term@(BinaryTerm op lhv rhv@BinaryTerm {})
  | rhv' == rhv = term
  | otherwise = evaluate (BinaryTerm op lhv rhv')
  where
    rhv' = evaluate rhv
evaluate expression = expression
