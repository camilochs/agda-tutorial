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

infixr 100 _*_

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

-- Tipos dependientes e implicitos

identity : (A : Set) -> A -> A
identity A x = x

-- Operaciones sobre listas

data Vec (A : Set) : Nat -> Set where
 []   : Vec A zero
 _::_ : {n : Nat} -> A -> Vec A n -> Vec A (succ n)

infixr 10 _::_

head : {A : Set}{n : Nat} -> Vec A (succ n) -> A
head (x :: xs) = x

last : {A : Set}{n : Nat} -> Vec A (succ n) -> A
last (x :: []) = x
last (x :: x1 :: x2) = last (x1 :: x2)

tail : {A : Set}{n : Nat} -> Vec A (succ n) -> Vec A n
tail (x :: xs) = xs

prepend : {A : Set}{n : Nat} -> A -> Vec A n -> Vec A (succ n)
prepend e vs = e :: vs

append : {A : Set}{n : Nat} -> Vec A n -> A -> Vec A (succ n)
append (x :: xs) e = x :: (append xs e)
append [] e = e :: []


-- Operaciones con funciones dependientes de tipos

test : (A : Set) -> (A -> Set)
test = \(A : Set) x -> A

apply : (A : Set) -> (B : A -> Set) -> ((x : A) -> B x) -> (a : A) -> B a
apply A B f a = f a

-- apply Nat (Test Nat) succ (succ zero) -> succ (succ zero)
