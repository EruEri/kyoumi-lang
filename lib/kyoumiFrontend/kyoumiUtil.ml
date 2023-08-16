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



module Compare = struct
  let mcompare list =
    match list with
    | [] -> failwith "No_comparaison"
    | list -> 
      list
      |> List.fold_left (fun acc (fn, lhs, rhs) ->
        if acc = 0 then fn lhs rhs
        else acc
      ) 0

  let compare_string lhs rhs =  
    String.compare lhs.value rhs.value
  
  let module_resolver_compare lhs rhs = 
    List.compare compare_string lhs rhs
end

module KyTypeEffect = struct
  type kyo_type = KyoumiAst.KyoLocType.kyoloc_type

  (* let rec compare_type lhs rhs = 
    let open KyoumiAst.KyoType in 
    match lhs, rhs with
    | TyParametricIdentifier {module_resolver = lmr; parametrics_type = lpt; name = ln},
    TyParametricIdentifier {module_resolver = rmr; parametrics_type = rpt; name = rn} ->
      let module_compare = (Compare.module_resolver_compare, lmr, rmr) in
      let type_compare = (List.compare compare_type), lpt, rpt in
      let name_compare = Compare.compare_string, ln, rn in
     Compare.(mcompare [module_compare; type_compare; name_compare])
    | _ -> failwith "" *)



end

module Pattern = struct
  open KyoumiAst.KExpresssion
  let rec flatten_por pattern =
    match pattern.value with
    | POr patterns ->
        patterns |> List.map flatten_por |> List.flatten
    | _ ->
        pattern :: []
end