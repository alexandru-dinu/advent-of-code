import Data.Monoid (mappend)


data Rules = I | E | W | SE | SW | NE | NW  deriving (Eq, Show)


instance Semigroup Rules where
    (<>) = undefined

instance Monoid Rules where
    mempty = undefined

