(** stanc console application *)

open Core_kernel
open Frontend
open Stan_math_backend
open Analysis_and_optimization
open Middle

(** The main program. *)
let version = "%%NAME%%3 %%VERSION%%"

let name = "%%NAME%%"

(** The usage message. *)
let usage = "Usage: " ^ name ^ " [option] ... <model_file.stan>"

let model_file = ref ""
let pretty_print_program = ref false
let canonicalize_program = ref false
let print_model_cpp = ref false
let dump_mir = ref false
let dump_mir_pretty = ref false
let dump_tx_mir = ref false
let dump_tx_mir_pretty = ref false
let dump_opt_mir = ref false
let dump_opt_mir_pretty = ref false
let dump_stan_math_sigs = ref false
let opt_lvl = ref Optimize.O0
let output_file = ref ""
let generate_data = ref false
let warn_uninitialized = ref false

(** Some example command-line options here *)
let options =
  Arg.align
    [ ( "--debug-lex"
      , Arg.Set Debugging.lexer_logging
      , " For debugging purposes: print the lexer actions" )
    ; ( "--debug-parse"
      , Arg.Set Debugging.grammar_logging
      , " For debugging purposes: print the parser actions" )
    ; ( "--debug-ast"
      , Arg.Set Debugging.ast_printing
      , " For debugging purposes: print the undecorated AST, before semantic \
         checking" )
    ; ( "--debug-decorated-ast"
      , Arg.Set Debugging.typed_ast_printing
      , " For debugging purposes: print the decorated AST, after semantic \
         checking" )
    ; ( "--debug-generate-data"
      , Arg.Set generate_data
      , " For debugging purposes: generate a mock dataset to run the model on"
      )
    ; ( "--debug-mir"
      , Arg.Set dump_mir
      , " For debugging purposes: print the MIR as an S-expression." )
    ; ( "--debug-mir-pretty"
      , Arg.Set dump_mir_pretty
      , " For debugging purposes: pretty-print the MIR." )
    ; ( "--debug-optimized-mir"
      , Arg.Set dump_opt_mir
      , " For debugging purposes: print the MIR after it's been optimized. \
         Only has an effect when optimizations are turned on." )
    ; ( "--debug-optimized-mir-pretty"
      , Arg.Set dump_opt_mir_pretty
      , " For debugging purposes: pretty print the MIR after it's been \
         optimized. Only has an effect when optimizations are turned on." )
    ; ( "--debug-transformed-mir"
      , Arg.Set dump_tx_mir
      , " For debugging purposes: print the MIR after the backend has \
         transformed it." )
    ; ( "--debug-transformed-mir-pretty"
      , Arg.Set dump_tx_mir_pretty
      , " For debugging purposes: pretty print the MIR after the backend has \
         transformed it." )
    ; ( "--dump-stan-math-signatures"
      , Arg.Set dump_stan_math_sigs
      , "Dump out the list of supported type signatures for Stan Math backend."
      )
    ; ( "--warn-uninitialized"
      , Arg.Set warn_uninitialized
      , " Emit warnings about uninitialized variables to stderr. Currently an \
         experimental feature." )
    ; ( "--auto-format"
      , Arg.Set pretty_print_program
      , " Pretty prints the program to the console" )
    ; ( "--print-canonical"
      , Arg.Set canonicalize_program
      , " Prints the canonicalized program to the console" )
    ; ( "--version"
      , Arg.Unit
          (fun _ ->
            print_endline (version ^ " " ^ "(" ^ Sys.os_type ^ ")") ;
            exit 0 )
      , " Display stanc version number" )
    ; ( "--name"
      , Arg.Set_string Semantic_check.model_name
      , " Take a string to set the model name (default = \
         \"$model_filename_model\")" )
    ; ( "-O0"
      , Arg.Unit (fun () -> opt_lvl := Optimize.O0)
      , "Do not apply optimizations to the Stan code." )
    ; ( "-O1"
      , Arg.Unit (fun () -> opt_lvl := Optimize.O1)
      , "Apply level 1 compiler optimizations (only basic optimizations)." )
    ; ( "-O2"
      , Arg.Unit (fun () -> opt_lvl := Optimize.O2)
      , "Apply level 2 compiler optimizations (all numerically stable optimizations)." )
    ; ( "-O3"
      , Arg.Unit (fun () -> opt_lvl := Optimize.O3)
      , "Apply level 3 compiler optimizations (all optimizations)." )
    ; ( "--O"
      , Arg.Unit (fun () -> opt_lvl := Optimize.O3)
      , "Apply level 3 compiler optimizations (all optimizations)." )
    ; ( "--o"
      , Arg.Set_string output_file
      , " Take the path to an output file for generated C++ code (default = \
         \"$name.hpp\")" )
    ; ( "--print-cpp"
      , Arg.Set print_model_cpp
      , " If set, output the generated C++ Stan model class to stdout." )
    ; ( "--allow_undefined"
      , Arg.Clear Semantic_check.check_that_all_functions_have_definition
      , " Do not fail if a function is declared but not defined" )
    ; ( "--include_paths"
      , Arg.String
          (fun str ->
            Preprocessor.include_paths := String.split_on_chars ~on:[','] str
            )
      , " Takes a comma-separated list of directories that may contain a file \
         in an #include directive (default = \"\")" )
    ; ( "--use-opencl"
      , Arg.Set Transform_Mir.use_opencl
      , " If set, try to use matrix_cl signatures." ) ]

let print_warn_uninitialized
    (uninit_vars : (Location_span.t * string) Set.Poly.t) =
  let show_location_span Location_span.({begin_loc; end_loc; _}) =
    let begin_line = string_of_int begin_loc.line_num in
    let begin_col = string_of_int begin_loc.col_num in
    let end_line = string_of_int end_loc.line_num in
    let end_col = string_of_int end_loc.col_num in
    let char_range =
      if begin_line = end_line then
        "line " ^ begin_line ^ ", character(s) " ^ begin_col ^ "-" ^ end_col
      else
        "line " ^ begin_line ^ ", character " ^ begin_col ^ " to line "
        ^ end_line ^ ", character " ^ end_col
    in
    "File \"" ^ begin_loc.filename ^ "\", " ^ char_range
  in
  let show_var_info (span, var_name) =
    show_location_span span ^ ":\n" ^ "  Warning: The variable '" ^ var_name
    ^ "' may not have been initialized.\n"
  in
  let filtered_uninit_vars =
    Set.Poly.filter
      ~f:(fun (span, _) -> span <> Location_span.empty)
      uninit_vars
  in
  Set.Poly.iter filtered_uninit_vars ~f:(fun v_info ->
      Out_channel.output_string stderr (show_var_info v_info) )

let model_file_err () =
  Arg.usage options ("Please specify one model_file.\n\n" ^ usage) ;
  exit 127

let model_file_start_char_err () =
  eprintf "%s" "Model name must not start with a number or symbol other than underscore.\n";
  exit 127

let add_file filename =
  if !model_file = "" then model_file := filename else model_file_err ()

(** ad directives from the given file. *)
let use_file filename =
  let ast =
    if !canonicalize_program then
      Canonicalize.repair_syntax
        (Errors.without_warnings Frontend_utils.get_ast_or_exit filename)
    else Frontend_utils.get_ast_or_exit filename
  in
  Debugging.ast_logger ast ;
  if !pretty_print_program then
    print_endline (Pretty_printing.pretty_print_program ast) ;
  let typed_ast = Frontend_utils.type_ast_or_exit ast in
  if !canonicalize_program then
    print_endline
      (Pretty_printing.pretty_print_typed_program
         (Canonicalize.canonicalize_program typed_ast)) ;
  if !generate_data then
    print_endline (Debug_data_generation.print_data_prog typed_ast) ;
  Debugging.typed_ast_logger typed_ast ;
  if not (!pretty_print_program || !canonicalize_program) then (
    let mir = Ast_to_Mir.trans_prog filename typed_ast in
    if !dump_mir then
      Sexp.pp_hum Format.std_formatter [%sexp (mir : Middle.Program.Typed.t)] ;
    if !dump_mir_pretty then Program.Typed.pp Format.std_formatter mir ;
    ( if !warn_uninitialized then
      let uninitialized_vars =
        Dependence_analysis.mir_uninitialized_variables mir
      in
      print_warn_uninitialized uninitialized_vars ) ;
    let tx_mir = Transform_Mir.trans_prog mir in
    if !dump_tx_mir then
      Sexp.pp_hum Format.std_formatter
        [%sexp (tx_mir : Middle.Program.Typed.t)] ;
    if !dump_tx_mir_pretty then Program.Typed.pp Format.std_formatter tx_mir ;
    let opt_mir =
      if !opt_lvl <> Optimize.O0 then (
        let opt =
          Optimize.optimization_suite ~settings:(Optimize.level_optimizations !opt_lvl) tx_mir
        in
        if !dump_opt_mir then
          Sexp.pp_hum Format.std_formatter
            [%sexp (opt : Middle.Program.Typed.t)] ;
        if !dump_opt_mir_pretty then Program.Typed.pp Format.std_formatter opt ;
        opt )
      else tx_mir
    in
    let cpp = Fmt.strf "%a" Stan_math_code_gen.pp_prog opt_mir in
    Out_channel.write_all !output_file ~data:cpp ;
    if !print_model_cpp then print_endline cpp )

let remove_dotstan s = String.drop_suffix s 5

let model_name_check_regex = Str.regexp "^[a-zA-Z_].*$"

let main () =
  (* Parse the arguments. *)
  Arg.parse options add_file usage ;
  (* Deal with multiple modalities *)
  if !dump_stan_math_sigs then (
    Stan_math_signatures.pretty_print_all_math_sigs Format.std_formatter () ;
    exit 0 ) ;
  (* Just translate a stan program *)
  if !model_file = "" then model_file_err () ;
  if !Semantic_check.model_name = "" then
    Semantic_check.model_name :=
      remove_dotstan List.(hd_exn (rev (String.split !model_file ~on:'/')))
      ^ "_model" ;
  if not (Str.string_match model_name_check_regex !Semantic_check.model_name 0) then
    model_file_start_char_err () ;
  if !output_file = "" then output_file := remove_dotstan !model_file ^ ".hpp" ;
  use_file !model_file

let () = main ()
