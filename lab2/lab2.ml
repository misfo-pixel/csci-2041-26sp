(*For this laboratory assignment, you must write OCaml functions that perform rational arithmetic as described in the previous section. Your functions must represent rational numbers as OCaml tuples with two elements. As a result, the rational number n / d is represented as the 2-tuple (n, d), which has the type int ∗ int.

OCaml predefines two functions fst and snd that take 2-tuples as arguments. The function fst returns the left member of a 2-tuple, so it has the type 'a ∗ 'b -> 'a. The function snd returns the right member of a 2-tuple, so it has the type 'a ∗ 'b -> 'b. These make it easy to define the functions num and den, like this:*)

let num = fst ;; 
let den = snd ;;

(*You will also need a function (gcd i j) that returns the greatest common divisor of integers i and j. You can define gcd like this: it has the type int -> int -> int.*)

let rec gcd i j = 
  if i <> 0 
  then if j > i 
       then gcd i (j - i) 
       else gcd (i - j) j 
  else j ;;

(*You must write the following functions yourself. They have short snappy names, because you might need to call them many times. All functions that take rational numbers as arguments must assume that those rational numbers are in lowest terms. All functions that return rational numbers must make sure that those rational numbers are also in lowest terms.*)

let rat n d =
  let common = gcd n d in
  (n/common, d/common);;

(*Return the rational number whose numerator is the integer n and whose denominator is the integer d. The type of rat is int -> int -> int ∗ int. You may assume that n ≥ 0 and d > 0. Hints: use gcd. Do not compute the gcd of n and d more than once.*)

let ratAdd a b =
  let n = num a * den b + num b * den a in
  let d = den a * den b in
  rat n d;;

(*Return the rational number that is the sum of the rational numbers a and b. The type of ratAdd is int ∗ int -> int ∗ int -> int ∗ int. Hint: use rat.*)

let ratMul a b =
  let n = num a * num b in
  let d = den a * den b in
  rat n d;;

(*Return the rational number that is the product of rational numbers a and b. The type of ratMul is int ∗ int -> int ∗ int -> int ∗ int. Hint: use rat.*)

let ratDiv a b =
  let n = num a * den b in
  let d = num b * den a in
  rat n d;;

(*Return the rational number that is the quotient of the rational numbers a and b. The type of ratDiv is int ∗ int -> int ∗ int -> int ∗ int. You may assume that b ≠ 0. Hint: use rat.*)

let ratGt a b =
  num a * den b > num b * den a;;

(*If the rational number a is greater than the rational number b, then return true, otherwise return false. The type of ratGt is int ∗ int -> int ∗ int -> bool. Hint: do not use an if–then–else.*)

let euler() =
  let epsilon = rat 1 100000 in
  let rec euler s t c =
    if ratGt epsilon t
    then s
    else euler (ratAdd s t) (ratDiv t c) (ratAdd c (rat 1 1))
  in euler (rat 0 1) (rat 1 1) (rat 1 1);;

(*(base) misfo@MQ122 lab2 % ./mytests
1 / 2
1 / 2
1 / 1
5 / 6
5 / 1
8 / 15
1 / 10
3 / 2
true
false
109601 / 40320*)