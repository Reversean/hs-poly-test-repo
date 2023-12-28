module Part4.Tasks where

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist lst =
    reversed (reverse lst)
    where reversed [] = REmpty
          reversed (h : t) = reversed t :< h

-- Реализуйте все представленные ниже классы (см. тесты)
instance (Show a) => Show (ReverseList a) where
    showsPrec _ a b = show a ++ b
    show REmpty = "[]"
    show (h :< t) = show' h ++ show t ++ "]"
        where show' REmpty = "["
              show' (h :< t) = show' h ++ show t ++ ","

instance (Eq a) => Eq (ReverseList a) where
    REmpty == REmpty = True
    REmpty == _ = False
    _ == REmpty = False
    (lh :< lt) == (rh :< rt) = (lh == rh) && (lt == rt)
    left /= right = not (left == right)

instance Semigroup (ReverseList a) where
    REmpty <> rightLst = rightLst
    leftLst <> REmpty = leftLst
    leftLst <> (rh :< rt) = leftLst <> rh :< rt

instance Monoid (ReverseList a) where
    mempty = REmpty

instance Functor ReverseList where
    fmap _ REmpty = REmpty
    fmap f (h :< t) = fmap f h :< f t

instance Applicative ReverseList where
    pure a = REmpty :< a
    REmpty <*> _ = REmpty
    _ <*> REmpty = REmpty
    (lh :< lt) <*> rightLst = (lh <*> rightLst) <> fmap lt rightLst

instance Monad ReverseList where
    REmpty >>= _ = REmpty
    (h :< t) >>= f = (h >>= f) <> f t
