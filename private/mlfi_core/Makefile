all:
	ocamlc -c mlfi_types.mli mlfi_types.ml mlfi_type_path.mli mlfi_type_path.ml mlfi_xtypes.mli mlfi_xtypes.ml mlfi_json.mli mlfi_json.ml
	ocamlc -a -o mlfi.cma mlfi_types.cmo mlfi_type_path.cmo mlfi_xtypes.cmo mlfi_json.cmo
	ocamlopt -c -o mlfi_types.cmx mlfi_types.ml
	ocamlopt -c -o mlfi_type_path.cmx mlfi_type_path.ml
	ocamlopt -c -o mlfi_xtypes.cmx mlfi_xtypes.ml
	ocamlopt -c -o mlfi_json.cmx mlfi_json.ml
	ocamlopt -a -o mlfi.cmxa mlfi_types.cmx mlfi_type_path.cmx mlfi_xtypes.cmx mlfi_json.cmx
	ocamlopt -shared -linkall -o mlfi.cmxs mlfi.cmxa
