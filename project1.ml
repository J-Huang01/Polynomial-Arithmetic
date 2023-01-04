(*Jiatan Huang CSCI2041 project1*)

exception MakePolyError;;

type poly = PolyEmpty | PolyTerm of int * int * poly;;

let isPolyOk p =
  let rec isPolyOking p = 
  match p 
with PolyEmpty -> true |(*PolyEmpty is a well formed poly*)
    PolyTerm(otherCoef, otherExp, otherPoly) ->(*have all three vals*)
      match otherPoly
    with PolyEmpty -> (*check the otherPoly term*)
      if (otherCoef = 0) || (otherExp < 0)
        then false
    else
      true | 
      PolyTerm(coef, exp, otherPolys) ->
        if (otherCoef = 0) || (otherExp < 0) || (otherExp < exp)(*check if p is well formed*)
          then false
      else
        isPolyOking otherPoly
      in isPolyOking p;;



let makePoly i = 
  let rec making r =
  match r
with [] -> PolyEmpty |(*empty list*)
[a] -> raise MakePolyError |(*only one element in the list*)
a::e::others -> PolyTerm(a, e, making others)
in let p = making i
in if isPolyOk p(*check whether isPolyOk*)
  then p
else
  raise MakePolyError;;




let polyAdd l r =
  let rec polyAdding l r =
  match l, r
with PolyEmpty, PolyEmpty -> PolyEmpty | (*both l and r are empty*)
PolyEmpty, r -> r |(*l is empty*)
l, PolyEmpty -> l |(*r is empty*)
  PolyTerm(lCoef, lExpo, lOther), PolyTerm(rCoef, rExpo, rOther) -> 
    if lExpo = rExpo
      then PolyTerm(lCoef+rCoef, lExpo, (polyAdding lOther rOther) )
  else if lExpo > rExpo(*find the next poly term in l, then check whether have same exp with r*)
    then PolyTerm(lCoef,lExpo, polyAdding lOther r)
else(*find the next poly term in r, then check whether have same exp with l*)
  PolyTerm(rCoef,rExpo, polyAdding l rOther)
in polyAdding l r;;



let polyMinus r =
  let rec polyMinusing i = 
  match i 
with PolyEmpty -> PolyEmpty |
PolyTerm(rCoef, rExp, rOther) ->
  PolyTerm(-rCoef, rExp, polyMinusing rOther)(*make the coef term to be negative*)
in polyMinusing r;;


(* test result *)

(*exception MakePolyError
val isPolyOk : poly -> bool = <fun>
val makePoly : int list -> poly = <fun>
val polyAdd : poly -> poly -> poly = <fun>
val polyMinus : poly -> poly = <fun>*)

(*test isPolyOk*)
(*3 x^5 + 9 x^4 + 2 x^3 - 4 x^1 + 2 x^0*)
isPolyOk (PolyTerm (3, 5, (PolyTerm (9, 4,(PolyTerm (2, 3,(PolyTerm (-4, 1,(PolyTerm (2, 0, PolyEmpty))))))))));;
(*3 x^4 + 9 x^5 + 2 x^3 - 4 x^1 + 2 x^0*)
isPolyOk (PolyTerm (3, 4, (PolyTerm (9, 5,(PolyTerm (2, 3,(PolyTerm (-4, 1,(PolyTerm (2, 0, PolyEmpty))))))))));;(*not decreasing expo*)
(*3 x^5 + 9 x^4 + 2 x^-3 - 4 x^1 + 2 x^0*)
isPolyOk (PolyTerm (3, 5, (PolyTerm (9, 4,(PolyTerm (2, -3,(PolyTerm (-4, 1,(PolyTerm (2, 0, PolyEmpty))))))))));;(*negative expo*)
(*3 x^5 + 9 x^4 + 0 x^3 - 4 x^1 + 2 x^0*)
isPolyOk (PolyTerm (3, 5, (PolyTerm (9, 4,(PolyTerm (0, 3,(PolyTerm (-4, 1,(PolyTerm (2, 0, PolyEmpty))))))))));;(*zero coef*)
(*- : bool = true
- : bool = false
- : bool = false
- : bool = false*)

(*test makePoly*)

printPoly (makePoly [1;5;2;4;3;3]);;
(*1 x^5 + 2 x^4 + 3 x^3*)
(*printPoly (makePoly [1;5;0;4;3;3]);;(*zero coef*)
(*Exception: MakePolyError.*)
printPoly (makePoly [1;5;2;-4;3;3]);;(*negative expo*)
(*Exception: MakePolyError.*)
printPoly(makePoly []);;(*empty list*)
(*0*)
printPoly (makePoly [2]);;(*only 1 element*)
(* Exception: MakePolyError. *)*)


(* test polyAdd *)
let p = makePoly [1;5;2;4;3;3];;
let k = makePoly [3;4;1;3;2;2;4;1];;
printPoly (polyAdd p k);;
(*val p : poly = PolyTerm (1, 5, PolyTerm (2, 4, PolyTerm (3, 3, PolyEmpty)))
val k : poly = PolyTerm (3, 4, PolyTerm (1, 3, PolyTerm (2, 2, PolyTerm (4, 1, PolyEmpty))))
1 x^5 + 5 x^4 + 4 x^3 + 2 x^2 + 4 x^1
- : unit = ()*)
let m = makePoly[3;5;2;4;2;3;-1;2;5;0];;(*test sample in theory part*)
let n = makePoly[7;4;1;2;-4;1;-3;0];;
printPoly(polyAdd m n);;
(*val m : poly =
  PolyTerm (3, 5,
   PolyTerm (2, 4,
    PolyTerm (2, 3, PolyTerm (-1, 2, PolyTerm (5, 0, PolyEmpty)))))
val n : poly =
  PolyTerm (7, 4,
   PolyTerm (1, 2, PolyTerm (-4, 1, PolyTerm (-3, 0, PolyEmpty))))
3 x^5 + 9 x^4 + 2 x^3 + 0 x^2 - 4 x^1 + 2 x^0
- : unit = ()*)

(*test polyMinus*)
printPoly (polyMinus p);;
printPoly (polyMinus k);;
printPoly (polyMinus m);;
printPoly (polyMinus n);;(*test sample in theory part*)
(*-1 x^5 - 2 x^4 - 3 x^3
- : unit = ()
-3 x^4 - 1 x^3 - 2 x^2 - 4 x^1
- : unit = ()
-3 x^5 - 2 x^4 - 2 x^3 + 1 x^2 - 5 x^0
- : unit = ()
-7 x^4 - 1 x^2 + 4 x^1 + 3 x^0
- : unit = ()*)


