module Free where

data Free m a = Pure a | Free (m (Free m a))

instance (Functor m) => Functor (Free m) where
    fmap f (Pure a) = Pure $ f a
    fmap f (Free mfa) = Free $ (fmap . fmap) f mfa

instance (Applicative m) => Applicative (Free m) where
    pure a = Pure a
    (Free mfa2b) <*> fmfa = Free $ fmap ( <*> fmfa) mfa2b
    (Pure a2b) <*> (Pure a) = Pure (a2b a)
    (Pure a2b) <*> (Free mfa) = Free $ (fmap . fmap) a2b mfa

instance (Monad m) => Monad (Free m) where
    return = pure
    (Pure a) >>= a2fb = a2fb a
    (Free mfa) >>= a2fmfb = Free $ fmap (>>= a2fmfb) mfa