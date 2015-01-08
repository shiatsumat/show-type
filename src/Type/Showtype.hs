{-# LANGUAGE Trustworthy,
    TypeOperators, ScopedTypeVariables,
    PolyKinds, DataKinds,
    FlexibleInstances, UndecidableInstances #-}

module Type.Showtype (
    -- * Showtype
    Showtype(showtype, showtypesPrec)
  ) where

import Data.Typeable
import GHC.TypeLits
import GHC.Exts

-- | Conversion of types to readable 'String's. Analogous to 'Show'.
class Showtype (a :: k) where
  {-# MINIMAL showtype | showtypesPrec #-}
  -- | Convert a type @a@ to a readable 'String'. Analogous to 'show' in 'Show'.
  showtype :: proxy a -> String
  -- | Convert a type @a@ to a readable 'String' with additional arguments. Analogous to 'showsPrec' in 'Show'.
  showtypesPrec :: Int -> proxy a -> String -> String
  showtype p = showtypesPrec 0 p ""
  showtypesPrec _ p s = showtype p ++ s

instance Showtype False where
  showtype _ = "False"
instance Showtype True where
  showtype _ = "True"

instance Showtype LT where
  showtype _ = "LT"
instance Showtype EQ where
  showtype _ = "EQ"
instance Showtype GT where
  showtype _ = "GT"

instance KnownNat n => Showtype (n :: Nat) where
  showtype = show . natVal

instance KnownSymbol s => Showtype (s :: Symbol) where
  showtype = show . symbolVal

instance Showtype Nothing where
  showtype _ = "Nothing"
instance Showtype a => Showtype (Just a) where
  showtypesPrec p _ = showParen (p > 10) $
    showString "Just " .
    showtypesPrec 11 (Proxy :: Proxy a)

instance Showtype a => Showtype (Left a) where
  showtypesPrec p _ = showParen (p > 10) $
    showString "Left " .
    showtypesPrec 11 (Proxy :: Proxy a)
instance Showtype a => Showtype (Right a) where
  showtypesPrec p _ = showParen (p > 10) $
    showString "Right " .
    showtypesPrec 11 (Proxy :: Proxy a)

instance Showtype '[] where
  showtype _ = "[]"
instance (Showtype a, Showlisttype as) => Showtype (a ': as :: [k]) where
  showtype _ = "[" ++ showtype (Proxy :: Proxy a) ++ showlisttype (Proxy :: Proxy as)
class Showlisttype (as :: [k]) where
  showlisttype :: Proxy as -> String
instance Showlisttype '[] where
  showlisttype _ = "]"
instance (Showtype a, Showlisttype as) => Showlisttype (a ': as) where
  showlisttype _ = "," ++ showtype (Proxy :: Proxy a) ++ showlisttype (Proxy :: Proxy as)

showtuple, showtuple' :: [String] -> String
showtuple ss = "(" ++ foldr1 (\s t -> s ++ "," ++ t) ss ++ ")"
showtuple' ss = "'" ++ showtuple ss

instance Showtype '() where
  showtype _ = "'()"
instance (Showtype a, Showtype b) => Showtype '(a,b) where
  showtype _ = showtuple' [
    showtype (Proxy :: Proxy a),
    showtype (Proxy :: Proxy b)]
instance (Showtype a, Showtype b, Showtype c) => Showtype '(a,b,c) where
  showtype _ = showtuple' [
    showtype (Proxy :: Proxy a),
    showtype (Proxy :: Proxy b),
    showtype (Proxy :: Proxy c)]
instance (Showtype a, Showtype b, Showtype c, Showtype d) => Showtype '(a,b,c,d) where
  showtype _ = showtuple' [
    showtype (Proxy :: Proxy a),
    showtype (Proxy :: Proxy b),
    showtype (Proxy :: Proxy c),
    showtype (Proxy :: Proxy d)]
instance (Showtype a, Showtype b, Showtype c, Showtype d, Showtype e) => Showtype '(a,b,c,d,e) where
  showtype _ = showtuple' [
    showtype (Proxy :: Proxy a),
    showtype (Proxy :: Proxy b),
    showtype (Proxy :: Proxy c),
    showtype (Proxy :: Proxy d),
    showtype (Proxy :: Proxy e)]
instance (Showtype a, Showtype b, Showtype c, Showtype d, Showtype e, Showtype f) => Showtype '(a,b,c,d,e,f) where
  showtype _ = showtuple' [
    showtype (Proxy :: Proxy a),
    showtype (Proxy :: Proxy b),
    showtype (Proxy :: Proxy c),
    showtype (Proxy :: Proxy d),
    showtype (Proxy :: Proxy e),
    showtype (Proxy :: Proxy f)]
instance (Showtype a, Showtype b, Showtype c, Showtype d, Showtype e, Showtype f, Showtype g) => Showtype '(a,b,c,d,e,f,g) where
  showtype _ = showtuple' [
    showtype (Proxy :: Proxy a),
    showtype (Proxy :: Proxy b),
    showtype (Proxy :: Proxy c),
    showtype (Proxy :: Proxy d),
    showtype (Proxy :: Proxy e),
    showtype (Proxy :: Proxy f),
    showtype (Proxy :: Proxy g)]
instance (Showtype a, Showtype b, Showtype c, Showtype d, Showtype e, Showtype f, Showtype g, Showtype h) => Showtype '(a,b,c,d,e,f,g,h) where
  showtype _ = showtuple' [
    showtype (Proxy :: Proxy a),
    showtype (Proxy :: Proxy b),
    showtype (Proxy :: Proxy c),
    showtype (Proxy :: Proxy d),
    showtype (Proxy :: Proxy e),
    showtype (Proxy :: Proxy f),
    showtype (Proxy :: Proxy g),
    showtype (Proxy :: Proxy h)]
instance (Showtype a, Showtype b, Showtype c, Showtype d, Showtype e, Showtype f, Showtype g, Showtype h, Showtype i) => Showtype '(a,b,c,d,e,f,g,h,i) where
  showtype _ = showtuple' [
    showtype (Proxy :: Proxy a),
    showtype (Proxy :: Proxy b),
    showtype (Proxy :: Proxy c),
    showtype (Proxy :: Proxy d),
    showtype (Proxy :: Proxy e),
    showtype (Proxy :: Proxy f),
    showtype (Proxy :: Proxy g),
    showtype (Proxy :: Proxy h),
    showtype (Proxy :: Proxy i)]
instance (Showtype a, Showtype b, Showtype c, Showtype d, Showtype e, Showtype f, Showtype g, Showtype h, Showtype i, Showtype j) => Showtype '(a,b,c,d,e,f,g,h,i,j) where
  showtype _ = showtuple' [
    showtype (Proxy :: Proxy a),
    showtype (Proxy :: Proxy b),
    showtype (Proxy :: Proxy c),
    showtype (Proxy :: Proxy d),
    showtype (Proxy :: Proxy e),
    showtype (Proxy :: Proxy f),
    showtype (Proxy :: Proxy g),
    showtype (Proxy :: Proxy h),
    showtype (Proxy :: Proxy i),
    showtype (Proxy :: Proxy j)]

showtypeauto :: proxy a -> String
showtypeauto = showtypeauto
{-# INLINE showtypeauto #-}

instance Typeable a => Showtype (a :: *) where
  showtype = showtypeauto
instance Typeable a => Showtype (a :: k -> *) where
  showtype = showtypeauto
instance Typeable a => Showtype (a :: k1 -> k2 -> *) where
  showtype = showtypeauto
instance Typeable a => Showtype (a :: k1 -> k2 -> k3 -> *) where
  showtype = showtypeauto
instance Typeable a => Showtype (a :: k1 -> k2 -> k3 -> k4 -> *) where
  showtype = showtypeauto
instance Typeable a => Showtype (a :: k1 -> k2 -> k3 -> k4 -> k5 -> *) where
  showtype = showtypeauto
instance Typeable a => Showtype (a :: k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> *) where
  showtype = showtypeauto
instance Typeable a => Showtype (a :: k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> *) where
  showtype = showtypeauto
instance Typeable a => Showtype (a :: k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> k8 -> *) where
  showtype = showtypeauto
instance Typeable a => Showtype (a :: k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> k8 -> k9 -> *) where
  showtype = showtypeauto
instance Typeable a => Showtype (a :: k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> k8 -> k9 -> k10 -> *) where
  showtype = showtypeauto

instance Typeable a => Showtype (a :: Constraint) where
  showtype = showtypeauto
instance Typeable a => Showtype (a :: k -> Constraint) where
  showtype = showtypeauto
instance Typeable a => Showtype (a :: k1 -> k2 -> Constraint) where
  showtype = showtypeauto
instance Typeable a => Showtype (a :: k1 -> k2 -> k3 -> Constraint) where
  showtype = showtypeauto
instance Typeable a => Showtype (a :: k1 -> k2 -> k3 -> k4 -> Constraint) where
  showtype = showtypeauto
instance Typeable a => Showtype (a :: k1 -> k2 -> k3 -> k4 -> k5 -> Constraint) where
  showtype = showtypeauto
instance Typeable a => Showtype (a :: k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> Constraint) where
  showtype = showtypeauto
instance Typeable a => Showtype (a :: k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> Constraint) where
  showtype = showtypeauto
instance Typeable a => Showtype (a :: k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> k8 -> Constraint) where
  showtype = showtypeauto
instance Typeable a => Showtype (a :: k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> k8 -> k9 -> Constraint) where
  showtype = showtypeauto
instance Typeable a => Showtype (a :: k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> k8 -> k9 -> k10 -> Constraint) where
  showtype = showtypeauto
