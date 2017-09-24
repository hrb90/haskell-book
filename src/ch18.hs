import Control.Monad

bind :: Monad m => (a -> m b) -> m a -> m b
bind fn = join . fmap fn