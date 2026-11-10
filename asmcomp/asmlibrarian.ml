(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Build libraries of .cmx files *)

open Misc
open Config
open Cmx_format

type error =
    File_not_found of string
  | Archiver_error of string
  | Link_error of Linkdeps.error

exception Error of error

let read_info name =
  let filename =
    try
      Load_path.find name
    with Not_found ->
      raise(Error(File_not_found name)) in
  let (info, crc) = Compilenv.read_unit_info filename in
  info.ui_force_link <- info.ui_force_link || !Clflags.link_everything;
  filename, (info, crc)

let create_archive file_list lib_name =
  let archive_name = Filename.remove_extension lib_name ^ ext_lib in
  let outchan = open_out_bin lib_name in
  Misc.try_finally
    ~always:(fun () -> close_out outchan)
    ~exceptionally:(fun () -> remove_file lib_name; remove_file archive_name)
    (fun () ->
       output_string outchan cmxa_magic_number;
       let units = List.map read_info file_list in
       let objfiles = List.map (fun (filename,_) ->
           Filename.chop_suffix filename ".cmx" ^ ext_obj)
           units in
       List.iter
         (fun (file_name, (unit, crc)) ->
            Asmlink.check_consistency file_name unit crc)
         units;
       let ldeps = Linkdeps.create ~complete:false in
       List.iter
         (fun (filename, (unit, _crc)) ->
            Linkdeps.add ldeps
              ~filename ~compunit:unit.ui_name
              ~provides:[unit.ui_name]
              ~requires:(List.map fst unit.ui_imports_cmx))
         (List.rev units);
       (match Linkdeps.check ldeps with
        | None -> ()
        | Some e -> raise (Error (Link_error e)));
       (* The imports of all units, without duplicates. A name may appear
          twice, with and without a CRC, so that the imports of each unit
          are kept exactly. *)
       let all_imports imports_of =
         let index = Hashtbl.create 42 in
         List.iter
           (fun (_, (unit, _)) ->
              List.iter (fun imp -> Hashtbl.replace index imp 0)
                (imports_of unit))
           units;
         let arr = Array.of_seq (Hashtbl.to_seq_keys index) in
         Array.sort compare arr;
         Array.iteri (fun i imp -> Hashtbl.replace index imp i) arr;
         arr, index
       in
       let cmis, cmi_index = all_imports (fun u -> u.ui_imports_cmi) in
       let cmxs, cmx_index = all_imports (fun u -> u.ui_imports_cmx) in
       let bitmap arr index imports =
         let b = Misc.Bitmap.make (Array.length arr) in
         List.iter (fun imp -> Misc.Bitmap.set b (Hashtbl.find index imp))
           imports;
         b
       in
       let genfns = Cmm_helpers.Generic_fns_tbl.make () in
       let lib_units =
         List.map
           (fun (_, (unit, crc)) ->
              Cmm_helpers.Generic_fns_tbl.add_unit genfns unit;
              { li_name = unit.ui_name;
                li_symbol = unit.ui_symbol;
                li_crc = crc;
                li_defines = unit.ui_defines;
                li_force_link = unit.ui_force_link;
                li_need_stdlib = unit.ui_need_stdlib;
                li_imports_cmi = bitmap cmis cmi_index unit.ui_imports_cmi;
                li_imports_cmx = bitmap cmxs cmx_index unit.ui_imports_cmx })
           units
       in
       let infos =
         { lib_imports_cmi = cmis;
           lib_imports_cmx = cmxs;
           lib_units;
           lib_generic_fns = Cmm_helpers.Generic_fns_tbl.entries genfns;
           lib_ccobjs = !Clflags.ccobjs;
           lib_ccopts = !Clflags.all_ccopts } in
       output_value outchan infos;
       if Ccomp.create_archive archive_name objfiles <> 0
       then raise(Error(Archiver_error archive_name));
    )

module Style = Misc.Style
open Format_doc

let report_error_doc ppf = function
  | File_not_found name ->
      fprintf ppf "Cannot find file %a" Style.inline_code name
  | Archiver_error name ->
      fprintf ppf "Error while creating the library %a" Style.inline_code name
  | Link_error e ->
      Linkdeps.report_error_doc ~print_filename:Location.Doc.filename ppf e

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error_doc err)
      | _ -> None
    )

let report_error = Format_doc.compat report_error_doc
