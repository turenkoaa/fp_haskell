{-Рассмотрим следующий тип данных

data FunMonad a = FunMonad{ fun :: () -> a }
Реализуйте для этого типа данных класс Monad. Покажите (на примерах) его работу.
-}
import Control.Monad (liftM, ap)

data FunMonad a = FunMonad{ fun :: () -> a }

instance (Show fun) => Show (FunMonad fun) where
  show (FunMonad fun) = show "FunMonad " ++ (show $ fun ())

instance Functor FunMonad where
    fmap fmapFun funValue = FunMonad (\() -> fmapFun $ (fun funValue) ())

instance Applicative FunMonad where
    pure x = FunMonad (\() -> x)
    (<*>) = ap

instance Monad FunMonad where
    return = pure
    m >>= k = k $ fun m ()

-- "FunMonad "[5,5,5]
test = do
  fm <- FunMonad (\() -> 5)
  return fm >>= (\v -> FunMonad $ \() -> [v, v, v])
