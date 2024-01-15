open Encoding
(* TODO: Do we need this datastructure? *)

type typemap = (string, Ty.t) Hashtbl.t

type varmap = { (* sym : Counter.t; *)
                ord : string Stack.t; typemap : typemap }

type t = varmap

let create () : t =
  {
    (* sym = Counter.create (); *)
    ord = Stack.create ();
    typemap = Hashtbl.create 16;
  }

(* let to_store (varmap : t) (binds : (Symbol.t * Value.t) list) : Concolic.Store.t *)
(*     = *)
(*   let sym = varmap.sym in *)
(*   let ord = varmap.ord in *)
(*   let map = Hashtbl.create Interpreter.Flags.hashtbl_default_size in *)
(*   let store = Concolic.Store.from_parts sym ord map in *)
(*   Concolic.Store.init store binds; *)
(*   store *)

let next (_ : t) (x : string) : string =
  let id = 0 in
  (* let id = Counter.get_and_inc varmap.sym x in *)
  if id = 0 then x else x ^ "_" ^ string_of_int id

let get_vars_by_type (t : Ty.t) (varmap : t) : string list =
  Hashtbl.fold
    (fun k v acc -> if v = t then k :: acc else acc)
    varmap.typemap []

let binds (varmap : t) : Symbol.t list =
  Hashtbl.fold (fun k v acc -> Symbol.mk_symbol v k :: acc) varmap.typemap []

let copy (varmap : t) : t =
  (* let sym = Counter.copy varmap.sym *)
  let ord = Stack.copy varmap.ord in
  let typemap = Hashtbl.copy varmap.typemap in
  { ord; typemap }

let add (varmap : t) (name : string) (ty : Ty.t) : unit =
  Stack.push name varmap.ord;
  Hashtbl.replace varmap.typemap name ty
