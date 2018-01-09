{-Дана следующая структура данных:

newtype PSet a = PSet{ contains :: (a -> Bool) }
представляющая собой предикатную реализацию математического понятия «множество».
Реализуйте столько вариантов реализаций классов Monoid и Functor для данной структуры данных, сколько сможете. Объясните свои решения.-}

newtype PSetOr a = PSetOr{ containsOr :: (a -> Bool) }

instance Monoid (PSetOr a) where
  mempty = PSetOr (\x -> False)
  mappend (PSetOr x) (PSetOr y) = PSetOr $ \a -> x a || y a


instance Functor PSetOr where
  fmap _ _ = PSetOr (\_ -> False)

newtype PSetAnd a = PSetAnd{ containsAnd :: (a -> Bool) }

instance Monoid (PSetAnd a) where
  mempty = PSetAnd (\x -> False)
  mappend (PSetAnd x) (PSetAnd y) = PSetAnd $ \c -> x c && y c

newtype PSetDiff a = PSetDiff{ containsDiff :: (a -> Bool) }

instance Monoid (PSetDiff a) where
  mempty = PSetDiff (\x -> False)
  mappend (PSetDiff x) (PSetDiff y) = PSetDiff $ \a -> ((x a) && (not $ y a)) || ((not $ x a) && (y a))
