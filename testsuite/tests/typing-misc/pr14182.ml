(* TEST
 expect;
*)

(* #14182: function literals passed as arguments are typed after the
   independent arguments that follow them, so that type information flows
   from e.g. a list to the function mapped over it. *)

module T = struct type r = { a : int } let x = { a = 1 } let get_a r = r.a end
type t = A | B
type s = A | B
module M = struct type v = C | D let c = C end
let opt ?(o = 0) x = x + o
let app f x = f x
[%%expect{|
module T : sig type r = { a : int; } val x : r val get_a : r -> int end
type t = A | B
type s = A | B
module M : sig type v = C | D val c : v end
val opt : ?o:int -> int -> int = <fun>
val app : ('a -> 'b) -> 'a -> 'b = <fun>
|}]

(* The examples from the issue *)

let _ = T.x |> (fun r -> r.a);;
[%%expect{|
- : int = 1
|}, Principal{|
Line 1, characters 27-28:
1 | let _ = T.x |> (fun r -> r.a);;
                               ^
Warning 18 [not-principal]: this type-based field disambiguation is not
  principal.

- : int = 1
|}]

let _ = [T.x] |> List.map (fun r -> r.a);;
[%%expect{|
- : int list = [1]
|}, Principal{|
Line 1, characters 38-39:
1 | let _ = [T.x] |> List.map (fun r -> r.a);;
                                          ^
Warning 18 [not-principal]: this type-based field disambiguation is not
  principal.

- : int list = [1]
|}]

let _ =
  [T.x] |> List.filter (fun r -> r.T.a > 0) |> List.map (fun r -> r.a);;
[%%expect{|
- : int list = [1]
|}, Principal{|
Line 2, characters 68-69:
2 |   [T.x] |> List.filter (fun r -> r.T.a > 0) |> List.map (fun r -> r.a);;
                                                                        ^
Warning 18 [not-principal]: this type-based field disambiguation is not
  principal.

- : int list = [1]
|}]

let _ = List.map (fun r -> r.a) [T.x];;
[%%expect{|
- : int list = [1]
|}, Principal{|
Line 1, characters 29-30:
1 | let _ = List.map (fun r -> r.a) [T.x];;
                                 ^
Warning 18 [not-principal]: this type-based field disambiguation is not
  principal.

- : int list = [1]
|}]

(* The function is typed before its argument: still rejected *)
let _ = (fun r -> r.a) T.x;;
[%%expect{|
Line 1, characters 20-21:
1 | let _ = (fun r -> r.a) T.x;;
                        ^
Error: Unbound record field "a"
|}]

(* Other applications *)

let f (l : T.r list) = List.fold_left (fun acc r -> acc + r.a) 0 l;;
[%%expect{|
val f : T.r list -> int = <fun>
|}, Principal{|
Line 1, characters 60-61:
1 | let f (l : T.r list) = List.fold_left (fun acc r -> acc + r.a) 0 l;;
                                                                ^
Warning 18 [not-principal]: this type-based field disambiguation is not
  principal.

val f : T.r list -> int = <fun>
|}]

let f (l : T.r list) = List.iter2 (fun r1 r2 -> ignore (r1.a + r2.a)) l l;;
[%%expect{|
val f : T.r list -> unit = <fun>
|}, Principal{|
Line 1, characters 59-60:
1 | let f (l : T.r list) = List.iter2 (fun r1 r2 -> ignore (r1.a + r2.a)) l l;;
                                                               ^
Warning 18 [not-principal]: this type-based field disambiguation is not
  principal.

Line 1, characters 66-67:
1 | let f (l : T.r list) = List.iter2 (fun r1 r2 -> ignore (r1.a + r2.a)) l l;;
                                                                      ^
Warning 18 [not-principal]: this type-based field disambiguation is not
  principal.

val f : T.r list -> unit = <fun>
|}]

let f (l : M.v list) = List.map (function C -> 0 | D -> 1) l;;
[%%expect{|
val f : M.v list -> int list = <fun>
|}, Principal{|
Line 1, characters 42-43:
1 | let f (l : M.v list) = List.map (function C -> 0 | D -> 1) l;;
                                              ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

Line 1, characters 51-52:
1 | let f (l : M.v list) = List.map (function C -> 0 | D -> 1) l;;
                                                       ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

val f : M.v list -> int list = <fun>
|}]

let f (x : t list) = x |> List.map (function A -> 0 | B -> 1);;
[%%expect{|
val f : t list -> int list = <fun>
|}, Principal{|
Line 1, characters 45-46:
1 | let f (x : t list) = x |> List.map (function A -> 0 | B -> 1);;
                                                 ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

Line 1, characters 54-55:
1 | let f (x : t list) = x |> List.map (function A -> 0 | B -> 1);;
                                                          ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

val f : t list -> int list = <fun>
|}]

let f (o : T.r option) = Option.map (fun r -> r.a) o;;
[%%expect{|
val f : T.r option -> int option = <fun>
|}, Principal{|
Line 1, characters 48-49:
1 | let f (o : T.r option) = Option.map (fun r -> r.a) o;;
                                                    ^
Warning 18 [not-principal]: this type-based field disambiguation is not
  principal.

val f : T.r option -> int option = <fun>
|}]

let f l = List.map (fun r -> r.a) (List.rev (l : T.r list));;
[%%expect{|
Line 1, characters 31-32:
1 | let f l = List.map (fun r -> r.a) (List.rev (l : T.r list));;
                                   ^
Error: Unbound record field "a"
|}]

let f (l : T.r list) = ListLabels.map ~f:(fun r -> r.a) l;;
[%%expect{|
val f : T.r list -> int list = <fun>
|}, Principal{|
Line 1, characters 53-54:
1 | let f (l : T.r list) = ListLabels.map ~f:(fun r -> r.a) l;;
                                                         ^
Warning 18 [not-principal]: this type-based field disambiguation is not
  principal.

val f : T.r list -> int list = <fun>
|}]

let f (l : T.r list) = l |> ListLabels.map ~f:(fun r -> r.a);;
[%%expect{|
val f : T.r list -> int list = <fun>
|}, Principal{|
Line 1, characters 58-59:
1 | let f (l : T.r list) = l |> ListLabels.map ~f:(fun r -> r.a);;
                                                              ^
Warning 18 [not-principal]: this type-based field disambiguation is not
  principal.

val f : T.r list -> int list = <fun>
|}]

let f (l : T.r list) =
  l |> ListLabels.fold_left ~init:0 ~f:(fun acc r -> acc + r.a);;
[%%expect{|
val f : T.r list -> int = <fun>
|}, Principal{|
Line 2, characters 61-62:
2 |   l |> ListLabels.fold_left ~init:0 ~f:(fun acc r -> acc + r.a);;
                                                                 ^
Warning 18 [not-principal]: this type-based field disambiguation is not
  principal.

val f : T.r list -> int = <fun>
|}]

(* The type of the function literal now comes from the list *)
let _ = List.map (fun r -> r.a) [M.c];;
[%%expect{|
Line 1, characters 27-28:
1 | let _ = List.map (fun r -> r.a) [M.c];;
                               ^
Error: This expression has type "M.v" which is not a record type.
|}]

(* Arguments which may be typed differently once the function literal is
   typed are not moved before it. *)

let _ = [{a = 1}] |> List.map (fun (r : T.r) -> r.a);;
[%%expect{|
- : int list = [1]
|}]

let _ = [A] |> List.map (fun (x : t) -> x);;
[%%expect{|
- : t list = [A]
|}]

let _ = List.map (fun r -> r.T.a) [{a = 1}];;
[%%expect{|
- : int list = [1]
|}, Principal{|
Line 1, characters 35-42:
1 | let _ = List.map (fun r -> r.T.a) [{a = 1}];;
                                       ^^^^^^^
Warning 18 [not-principal]: this type-based record disambiguation is not
  principal.

- : int list = [1]
|}]

let _ = List.iter (function M.C -> () | M.D -> ()) [C; D];;
[%%expect{|
- : unit = ()
|}, Principal{|
Line 1, characters 52-53:
1 | let _ = List.iter (function M.C -> () | M.D -> ()) [C; D];;
                                                        ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

Line 1, characters 55-56:
1 | let _ = List.iter (function M.C -> () | M.D -> ()) [C; D];;
                                                           ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

- : unit = ()
|}]

(* The optional argument of [opt] is eliminated using the expected type *)
let _ = List.map (fun f -> f 1) [opt];;
[%%expect{|
- : int list = [1]
|}, Principal{|
Line 1, characters 33-36:
1 | let _ = List.map (fun f -> f 1) [opt];;
                                     ^^^
Warning 19 [non-principal-labels]: eliminated optional argument without principality.

- : int list = [1]
|}]

let _ = app (fun (f : int -> int) -> f 1) opt;;
[%%expect{|
- : int = 1
|}, Principal{|
Line 1, characters 42-45:
1 | let _ = app (fun (f : int -> int) -> f 1) opt;;
                                              ^^^
Warning 19 [non-principal-labels]: eliminated optional argument without principality.

- : int = 1
|}]

(* The string literal is typed as a format *)
let _ = app (fun fmt -> Printf.sprintf fmt 1) "%d";;
[%%expect{|
- : string = "1"
|}, Principal{|
Line 1, characters 46-50:
1 | let _ = app (fun fmt -> Printf.sprintf fmt 1) "%d";;
                                                  ^^^^
Warning 18 [not-principal]: this coercion to format6 is not principal.

- : string = "1"
|}]

(* [x |> f a] is not typed as [f a x] when this would change the parameter
   [x] is passed to *)
let g ~l f = f l;;
let _ = 2 |> g (fun x -> x * 10);;
[%%expect{|
val g : l:'a -> ('a -> 'b) -> 'b = <fun>
Line 2, characters 13-32:
2 | let _ = 2 |> g (fun x -> x * 10);;
                 ^^^^^^^^^^^^^^^^^^^
Warning 6 [labels-omitted]: label "l" was omitted in the application of this
  function.

- : int = 20
|}]

(* Evaluation order is preserved *)
let _ =
  let log = ref [] in
  let trace s v = log := s :: !log; v in
  let r =
    (trace "x" [1; 2] : int list)
    |> List.fold_left (fun acc x -> acc + x) (trace "init" 0)
  in
  r, List.rev !log;;
[%%expect{|
- : int * string list = (3, ["x"; "init"])
|}]

(* [f a @@ x] is typed as [f a x] too *)
let _ = List.map (fun r -> r.a) @@ [T.x];;
[%%expect{|
- : int list = [1]
|}, Principal{|
Line 1, characters 29-30:
1 | let _ = List.map (fun r -> r.a) @@ [T.x];;
                                 ^
Warning 18 [not-principal]: this type-based field disambiguation is not
  principal.

- : int list = [1]
|}]

let _ =
  let log = ref [] in
  let trace s v = log := s :: !log; v in
  let r =
    List.fold_left (fun acc x -> acc + x) (trace "init" 0)
    @@ (trace "x" [1; 2] : int list)
  in
  r, List.rev !log;;
[%%expect{|
- : int * string list = (3, ["x"; "init"])
|}]

(* Arguments whose typing depends on identifiers that the function literal
   may refine are not moved before it *)
module U = struct type r = { a : int; b : int } end
open U
let k (f : unit -> unit) (n : int) = f (); n;;
let h x = k (fun () -> ignore x.T.a) x.a;;
let h x = x.a |> k (fun () -> ignore x.T.a);;
let h g = k (fun () -> ignore (g : ?o:int -> int -> int)) (g 1);;
[%%expect{|
module U : sig type r = { a : int; b : int; } end
val k : (unit -> unit) -> int -> int = <fun>
val h : T.r -> int = <fun>
val h : T.r -> int = <fun>
val h : (?o:int -> int -> int) -> int = <fun>
|}, Principal{|
module U : sig type r = { a : int; b : int; } end
val k : (unit -> unit) -> int -> int = <fun>
Line 4, characters 39-40:
4 | let h x = k (fun () -> ignore x.T.a) x.a;;
                                           ^
Warning 18 [not-principal]: this type-based field disambiguation is not
  principal.

val h : T.r -> int = <fun>
Line 5, characters 12-13:
5 | let h x = x.a |> k (fun () -> ignore x.T.a);;
                ^
Warning 18 [not-principal]: this type-based field disambiguation is not
  principal.

val h : T.r -> int = <fun>
Line 6, characters 59-60:
6 | let h g = k (fun () -> ignore (g : ?o:int -> int -> int)) (g 1);;
                                                               ^
Warning 19 [non-principal-labels]: eliminated optional argument without principality.

val h : (?o:int -> int -> int) -> int = <fun>
|}]

(* No reordering under local constraints *)
type _ ty = Int : int ty | Bool : bool ty
let f (type a) (w : a ty) (x : a) =
  match w with
  | Int -> List.map (fun y -> if true then y else 1) [x]
  | Bool -> [];;
[%%expect{|
type _ ty = Int : int ty | Bool : bool ty
Line 4, characters 11-56:
4 |   | Int -> List.map (fun y -> if true then y else 1) [x]
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "int list"
       but an expression was expected of type "'a"
       This instance of "int" is ambiguous:
       it would escape the scope of its equation
       Hint (manual section 7.2): A type annotation may resolve the ambiguity,
       either on this expression or the whole function.
|}]

(* Type errors may now be reported in the function literal *)
let f (l : string list) = List.map (fun x -> x + 1) l;;
[%%expect{|
Line 1, characters 45-46:
1 | let f (l : string list) = List.map (fun x -> x + 1) l;;
                                                 ^
Error: The value "x" has type "string" but an expression was expected of type "int"
|}]
