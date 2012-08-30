module Kind.Constraint
  ( (|-)(..)
  , Dict(..)
  , (\\)
  , Class(..)
  , (:=>)(..)
  ) where

-- | A dictionary for a constraint
data Dict p where Dict :: p -> Dict p

infixr 0 |-
-- | Entailment of constraints
newtype p |- q = Sub (p => Dict q)

-- | Substitution of constraints
(\\) :: p => (q => r) -> (p |- q) -> r
r \\ Sub Dict = r

-- | Reification of a @class@ declaration
class Class b h | h -> b where
  byClass :: h |- b

-- | Reification of an @instance@ declaration
class b :=> h | b -> h where
  byInstance :: b |- h

instance Class () (Class b a) where byClass = Sub Dict
instance Class () (b :=> a) where byClass = Sub Dict
instance Class b a => () :=> Class b a where byInstance = Sub Dict
instance (b :=> a) => () :=> b :=> a where byInstance = Sub Dict
