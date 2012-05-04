module Algebraic (
    GroupPlus,
    (>+<),
    (>-<),
    gzero,
    gneg,

    GroupElem,
    Ring,
    (>*<)
) where


infixl 6 >+<
infixl 6 >-<

infixl 7 >*<

-- |Specifies an additive group G, which should satisfy the "group axioms,"
-- namely:
--
-- 1) Addition is associative and closed in G
--
-- 2) There exists a unique identity element gzero: gzero + g = g forall g
--
-- 3) Every element has a unique inverse such that (inv g) + g = gzero
--
-- In addition, since this specifies an additive group, instances should be
-- commutative: g1 + g2 = g2 + g1
class (Eq g) => GroupPlus g where
    -- |Group addition
    (>+<) :: g -> g -> g
    -- |Group subtraction. A sensible default is provided.
    (>-<) :: g -> g -> g
    -- |Zero element
    gzero :: g
    -- |Negation operation
    gneg  :: g -> g

    g1 >-< g2 = g1 >+< (gneg g2)


-- |Wrapper for a group element. This is needed because we can't write an
-- instance declaration like:
--
-- instance Num g => GroupPlus g where ...
newtype GroupElem g = GroupElem g
    deriving (Read, Show, Eq, Ord)

-- |The Num class generally specifies a group in a very obvious way,
-- although floating-point arithmetic is NOT associative.
instance Num g => GroupPlus (GroupElem g) where
    (GroupElem g1) >+< (GroupElem g2) = GroupElem (g1 + g2)
    gzero = GroupElem 0
    gneg (GroupElem g) = GroupElem (-g)


-- |Ring class, which is an additive group with an associative multiplication.
-- Should obey the ring axioms, notably that left and right multiplication
-- distribute.
class (GroupPlus r) => Ring r where
    -- |Ring multiplication
    (>*<) :: r -> r -> r

instance Num r => Ring (GroupElem r) where
    (GroupElem r1) >*< (GroupElem r2) = GroupElem (r1 * r2)
