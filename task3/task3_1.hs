{-
Дана следующая структура данных:

data WeirdPeanoNumber = Zero | Succ (WeirdPeanoNumber ) | Pred (WeirdPeanoNumber )
где Zero это 0, Succ X это (X + 1), Pred X это (X - 1).
Реализуйте для данного представления чисел все характерные для целых чисел классы типов. Проверьте работу реализованных функций.-}

data WeirdPeanoNumber = Zero | Succ (WeirdPeanoNumber) | Pred (WeirdPeanoNumber)

getInt :: WeirdPeanoNumber -> Integer
getInt Zero = 0
getInt (Succ a) = getInt a + 1
getInt (Pred a) = getInt a - 1

foldWPN :: WeirdPeanoNumber -> WeirdPeanoNumber
foldWPN Zero = Zero
foldWPN (Pred (Succ x)) = foldWPN x
foldWPN (Succ (Pred x)) = foldWPN x
foldWPN (Pred x) = case foldWPN x of
  (Succ y) -> y
  _ -> Pred (foldWPN x)
foldWPN (Succ x) = case foldWPN x of
  (Pred y) -> y
  _ -> Succ (foldWPN x)

instance Eq WeirdPeanoNumber where
  a == b = helper (foldWPN a) (foldWPN b) where
    helper Zero Zero = True
    helper (Succ lv) (Succ rv) = helper lv rv
    helper (Pred lv) (Pred rv) = helper lv rv
    helper _ _ = False

instance Ord WeirdPeanoNumber where
  a <= b = helper (foldWPN a) (foldWPN b) where
    helper (Succ lv) (Succ rv) = helper lv rv
    helper (Pred lv) (Pred rv) = helper lv rv
    helper (Pred _) Zero = True
    helper Zero (Succ _) = True
    helper lv rv = lv == rv

helper _ Zero = Zero
helper 1 (Succ r) = Succ (helper 1 r)
helper 1 (Pred r) = Pred (helper 1 r)
helper (-1) (Succ r) = Pred (helper (-1) r)
helper (-1) (Pred r) = Succ (helper (-1) r)

instance Num WeirdPeanoNumber where
  (+) Zero rv = rv
  (+) lv Zero = lv
  (+) (Succ lv) rv = Succ (lv + rv)
  (+) (Pred lv) rv = Pred (lv + rv)

  (*) Zero rv = Zero
  (*) lv Zero = Zero
  (*) (Succ lv) rv = (helper 1 rv) + (lv * rv)
  (*) (Pred lv) rv = (helper (-1) rv) + (lv * rv)

  signum a = case foldWPN a of
    (Succ _) -> Succ Zero
    (Pred _) -> Pred Zero
    _ -> Zero

  negate Zero = Zero
  negate (Succ num) = Pred (negate num)
  negate (Pred num) = Succ (negate num)

  abs a | a > Zero = a
        | otherwise = negate a

  fromInteger x | x == 0 = Zero
                | x < 0 = Pred (fromInteger (x + 1))
                | otherwise = Succ (fromInteger (x - 1))

instance Show WeirdPeanoNumber where
    show a = show (getInt a)

instance Enum WeirdPeanoNumber where
  toEnum = fromIntegral
  fromEnum = fromInteger . getInt

instance Real WeirdPeanoNumber where
    toRational = toRational . getInt

instance Integral WeirdPeanoNumber where
    toInteger = getInt

    quotRem Zero _ = (Zero, Zero)
    quotRem a Zero = error "Division by zero"
    quotRem a b | signum a == signum b = res
                | otherwise = (negate quot, rem) where
                     res@(quot, rem) = quotRemAbs Zero (abs a) (abs b)
                     quotRemAbs q r b | r >= b = quotRemAbs (q + 1) (r - b) b
                                           | otherwise = (q, r)

a = (Pred (Pred (Succ (Succ Zero))))
b = (Succ (Pred (Pred (Succ Zero))))
c = (Succ (Succ (Succ (Succ Zero))))
