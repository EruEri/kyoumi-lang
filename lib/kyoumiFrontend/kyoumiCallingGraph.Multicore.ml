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
 
module T = Domainslib.Task
module KyoFunctionGraph = KyoumiUtil.KyoFunctionGraph
open KyoumiAst

let calling_graph _kyo_program kyo_module = 
  kyo_module
  |> List.fold_left (fun graph -> function
  | KNFunction _declaration -> failwith ""
  | KNEffect _| KNType _ ->
    graph
  ) KyoFunctionGraph.empty

let calling_graph kyo_program = 
  let open KyoumiAst in
  let nb_core = Domain.recommended_domain_count () - 1 in
  let pool = T.setup_pool ~num_domains:nb_core () in
  kyo_program 
    |> List.map (fun {filename = _; kyo_module} -> 
      T.async pool (fun _ -> calling_graph kyo_program kyo_module)
    )
    |> List.fold_left (fun graph promise ->
      let await = T.await pool promise in
      KyoFunctionGraph.merge graph await
    ) KyoFunctionGraph.empty