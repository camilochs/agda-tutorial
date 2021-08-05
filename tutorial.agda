module tutorial where

-- Expresiones lógicas

data Bool : Set where
  true  : Bool
  false : Bool

_||_ : Bool -> Bool -> Bool
_ || true = true
true || _ = true
_ || _    = false

_or_ : Bool -> Bool -> Bool
false or x = x
true or _ = true

infixr 20 _or_

_&&_ : Bool -> Bool -> Bool
true && true = true
_ && _  = false

!_ : Bool -> Bool
! true  = false
! false = true

_⊕_ : Bool -> Bool -> Bool
true ⊕ true   = false
false ⊕ false = false
_ ⊕ _ = true

_==_ : Bool -> Bool -> Bool
true == true = true
false == false = true
_ == _ = false

-- Expresiones aritmeticas
data Nat : Set where
 zero : Nat
 succ : Nat -> Nat

_+_ : Nat -> Nat -> Nat
zero + m = m
succ n + m = succ (n + m)


_*_ : Nat -> Nat -> Nat
zero * n = zero
succ n * m = n * m + m

_-_ : Nat -> Nat -> Nat
zero - m = zero
n - zero = n
(succ n) - (succ m) = n - m

infixl 100 _*_

pred : Nat -> Nat
pred zero = zero
pred (succ n) = n


-- Expresiones lógicas aritmeticas

_>_ : Nat -> Nat -> Bool
zero > zero = false
zero > (succ _) = false
(succ _) > zero = true
(succ n) > (succ m) = n > m

_>=_ : Nat -> Nat -> Bool
zero >= zero = true
zero >= (succ _) = false
(succ _) >= zero = true
(succ n) >= (succ m) = n >= m

_<_ : Nat -> Nat -> Bool
zero < zero = false
zero < (succ _) = true
(succ _) < zero = false
(succ n) < (succ m) = n < m

_<=_ : Nat -> Nat -> Bool
zero <= zero = true
zero <= (succ _) = true
(succ _) <= zero = false
(succ n) <= (succ m) = n <= m


-- Listas
data List (A : Set) : Set where
  Nil : List A
  Cons : A -> List A -> List A

head : {A : Set} -> List A -> List A
head Nil = Nil
head (Cons y y') = (Cons y Nil)

last : {A : Set} -> List A -> List A
last Nil = Nil
last (Cons y Nil) = (Cons y Nil)
last (Cons y y') = (last y')


identity : (A : Set) -> A -> A
identity A x = x


-- Dependencia de Tipos

test : (A : Set) -> (A -> Set)
test = \(A : Set) x -> A

apply : (A : Set) -> (B : A -> Set) -> ((x : A) -> B x) -> (a : A) -> B a
apply A B f a = f a

-- apply Nat (Test Nat) succ (succ zero) -> succ (succ zero)
