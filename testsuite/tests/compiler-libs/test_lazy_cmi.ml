(* TEST
 flags = "-I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/parsing \
          -I ${ocamlsrcdir}/utils \
          -I ${ocamlsrcdir}/file_formats -I ${ocamlsrcdir}/toplevel";
 expect;
*)

(* Check that a signature read lazily from a .cmi file, then forced piece by
   piece, is the same as the signature that was written. *)

module M = struct
  type t = A | B of int
  let x = 1
  let f (y : t) = [y]
  external id : 'a -> 'a = "%identity"
  exception E of t
  module type S = sig type u val g : u -> t end
  module N = struct
    type r = { a : t; b : float }
    let z = ref [x]
    module P = struct let w (r : r) = r.a end
  end
  module F (X : S) = struct let h = X.g end
  class c = object method m = x end
end

[%%expect{|
module M :
  sig
    type t = A | B of int
    val x : int
    val f : t -> t list
    external id : 'a -> 'a = "%identity"
    exception E of t
    module type S = sig type u val g : u -> t end
    module N :
      sig
        type r = { a : t; b : float; }
        val z : int list ref
        module P : sig val w : r -> t end
      end
    module F : (X : S) -> sig val h : X.u -> t end
    class c : object method m : int end
  end
|}]

(* Force the nested signatures and the value types before the enclosing
   signature. *)
let rec force_sig sg =
  List.iter force_item (Subst.Lazy.force_signature_once sg)
and force_item : Subst.Lazy.signature_item -> unit = function
  | SigL_value (_, vd, _) -> ignore (Subst.Lazy.force_value_description vd)
  | SigL_module (_, _, md, _, _) -> force_modtype md.mdl_type
  | SigL_modtype (_, { mtdl_type = Some mty; _ }, _) -> force_modtype mty
  | _ -> ()
and force_modtype : Subst.Lazy.modtype -> unit = function
  | MtyL_signature sg -> force_sig sg
  | MtyL_functor (Named (_, arg), res) -> force_modtype arg; force_modtype res
  | MtyL_functor (Unit, res) -> force_modtype res
  | MtyL_ident _ | MtyL_alias _ -> ()

(* Whether the signatures read eagerly and lazily print like the saved one *)
let same_as_saved =
  let saved =
    match
      Env.find_module_by_name (Longident.Lident "M") !Toploop.toplevel_env
    with
    | _, { md_type = Mty_signature sg; _ } ->
        Subst.signature Make_local (Subst.for_saving Subst.identity) sg
    | _ -> assert false
  in
  let file = Filename.temp_file "test_lazy_cmi" ".cmi" in
  let oc = open_out_bin file in
  let _crc : Digest.BLAKE128.t =
    Cmi_format.output_cmi file oc
      { cmi_name = "M"; cmi_sign = saved; cmi_crcs = []; cmi_flags = [] }
  in
  close_out oc;
  let eager = (Cmi_format.read_cmi file).cmi_sign in
  let forced =
    let sg = (Cmi_format.read_cmi_lazy file).cmi_sign in
    force_sig sg;
    Subst.Lazy.force_signature sg
  in
  Sys.remove file;
  let print sg = Format.asprintf "%a" Printtyp.signature sg in
  print eager = print saved, print forced = print saved

[%%expect{|
val force_sig : Subst.Lazy.signature -> unit = <fun>
val force_item : Subst.Lazy.signature_item -> unit = <fun>
val force_modtype : Subst.Lazy.modtype -> unit = <fun>
val same_as_saved : bool * bool = (true, true)
|}]
