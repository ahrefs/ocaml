(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                     Mark Shinwell, Jane Street Europe                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Mutable state used by [Cmmgen]. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type constant =
  | Const_closure of Cmm.is_global * Clambda.ufunction list
                     * Clambda.uconstant list
  | Const_table of Cmm.is_global * Cmm.data_item list

val add_constant : Misc.Stdlib.String.t -> constant -> unit

val add_data_items : Cmm.data_item list -> unit

val add_function : Clambda.ufunction -> unit

val get_and_clear_constants : unit -> constant Misc.Stdlib.String.Map.t

val get_and_clear_data_items : unit -> Cmm.data_item list

val next_function : unit -> Clambda.ufunction option

val no_more_functions : unit -> bool

(* Whether the function has been added with [add_function] since the last
   call to [clear_function_names], that is, whether it is defined in the
   current compilation unit. *)
val is_local_function : Clambda.function_label -> bool

val clear_function_names : unit -> unit

(* The structured constants defined in the current compilation unit *)
val set_local_structured_constants :
  Clambda.preallocated_constant list -> unit

(* Record a structured constant defined in another compilation unit,
   unless it is already known *)
val add_global_structured_constant :
  string -> Clambda.ustructured_constant -> unit

val get_structured_constant :
  string -> (Cmm.is_global * Clambda.ustructured_constant) option

(* Also looks up using Compilenv.structured_constant_of_symbol *)
val structured_constant_of_sym : string -> Clambda.ustructured_constant option
