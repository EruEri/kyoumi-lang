(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kyoumi                                                                *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* Kyoumi is free software: you can redistribute it and/or modify it under the terms          *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* Kyoumi is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;        *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with Kyoumi.       *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

open Util.Position
open KyoumiAst.KExpresssion
open KyoumiError

module KyoTypeConstraintSet = KyoumiUtil.KyoTypeConstraintSet

type kyo_tying_env = (string * KyoumiAst.KyoType.kyo_type) list
type kyo_tying_constraints = KyoumiUtil.KyoTypeConstraintSet.t

type kyo_env = {
  program: KyoumiAst.kyo_program;
  opened_module: KyoumiAst.kyo_module list;
  variable_env: kyo_tying_env;
  kyo_tying_constraints: kyo_tying_constraints
}

let merge_env base rhs = 
  {
    base with kyo_tying_constraints = KyoTypeConstraintSet.union base.kyo_tying_constraints rhs.kyo_tying_constraints
  }
let fresh_variable reset =
  let counter = ref 0 in
  fun () -> 
    let () = match reset with
      | false -> ()
      | true -> counter := 0 
    in
    let n = !counter in
    let () = incr counter in
    Printf.sprintf "'t%u" n

let fresh_variable_type ?(reset = false) () = 
  KyoumiAst.KyoType.TyPolymorphic(KyTyPolymorphic (fresh_variable reset ()))

let add_constraint ~lhs ~rhs env =
  let constrai = KyoumiAst.KyoType.{cstr_lhs = lhs; cstr_rhs = rhs} in
  { env with kyo_tying_constraints = KyoumiUtil.KyoTypeConstraintSet.add constrai env.kyo_tying_constraints}

let add_module kyo_module kyo_env =
  { kyo_env with opened_module = kyo_module::kyo_env.opened_module } 

let rec typeof_expr' kyo_env expr = typeof_expr kyo_env @@ value expr

and typeof_expr kyo_env = 
let open KyoumiAst.KyoType in
function
| EUnit -> kyo_env, TyUnit
| ECmpLess | ECmpEqual | ECmpGreater ->
  kyo_env, TyOredered
| EInteger _ -> 
  kyo_env, TyInteger
| EFloat _ -> 
  kyo_env, TyFloat
| EString _ -> 
  kyo_env, TyString
| EOpen {module_resolver; next} ->
  let kyo_module = 
    match KyoumiUtil.Module.find_module module_resolver kyo_env.program with
    | Some kyo_module -> kyo_module
    | None -> raise @@ unbound_module module_resolver
  in
  let kyo_env = add_module kyo_module kyo_env in
  let next_type = typeof_expr' kyo_env next in
  next_type
| ETuple kyo_exprs ->
  let kyo_extented_env, kyo_types = 
  List.fold_left_map typeof_expr' kyo_env kyo_exprs
  in
  kyo_extented_env, TyTuple (kyo_types)
| EWhile {w_condition; w_body} ->
  let (env, c) = typeof_expr' kyo_env w_condition in
  let kyo_env = merge_env kyo_env env in
  let (env, b) = typeof_expr' env w_body in
  let kyo_env = merge_env kyo_env env in
  let kyo_env = 
    kyo_env
    |> add_constraint ~lhs:c ~rhs:TyBool
    |> add_constraint ~lhs:b ~rhs:TyUnit
  in
  kyo_env, TyUnit
| _ -> failwith ""