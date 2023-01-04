(*
   PRINT POLY. Pretty-print polynomials from CSci 2041 Project 1.

     James Moen
     25 Sep 22
*)

(* PRINTF. Define various predefined printing functions. *)

open Printf ;;

(* POLY. A univariate polynomial, with nonzero integer coefficients and integer
   exponents greater than or equal to zero. *)

(* type poly =
  PolyEmpty |
  PolyTerm of int * int * poly ;; *)

(* PRINT POLY. Print POLY so it's allegedly easy to read. You need not know how
   this works. Maybe it will help with debugging. *)

let printPoly poly =

(* PRINTING POLY. Do all the work for PRINT POLY. *)

  let rec printingPoly poly =
    match poly
    with PolyEmpty ->
          () |

         PolyTerm (coef, expo, other) ->
          printf " %c %i x^%i"
            (if coef < 0 then '-' else '+')
            (abs coef) expo ;
          printingPoly other

(* Lost? This is PRINT POLY's body. *)

  in match poly
     with PolyEmpty ->
            printf "0\n" |

          PolyTerm(coef, expo, other) ->
            printf "%i x^%i" coef expo ;
            printingPoly other ;
            printf "\n" ;;

(*3 x^5 + 9 x^4 + 2 x^3 - 4 x^1 + 2 x^0*)

(*test isPolyOk*)
(* printPoly (isPolyOk (PolyTerm (3, 5,(PolyTerm (9, 4,(PolyTerm (2, 3,(PolyTerm (-4, 1,(PolyTerm (2, 0, PolyEmpty)))))))))));; *)
(*test makePoly*)

printPoly (makePoly [1;5;2;4;3;3]);;
printPoly (makePoly [1;5;0;4;3;3]);;
printPoly (makePoly [1;5;2;-4;3;3]);;
printPoly(makePoly []);;
printPoly (makePoly [2]);;


(*test Polyadd*)
printPoly ()

(*test PolyMinus*)
printPoly ()