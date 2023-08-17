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


type suppoted_file = 
    | CFile
    | COjectFile
    | KyoumiFile

let extension_list = 
    [ (CFile, ".c"); (COjectFile, ".o"); (KyoumiFile, ".kyoumi") ]

let rev_extension_list = 
    let swap (a,b) = b,a in
    List.map swap extension_list


let rec split_file ~kyoumi ~c ~co =
    function
    | [] -> Ok ((List.rev kyoumi), (List.rev c), (List.rev co))
    | t::q -> 
        let (let*) = Result.bind in
        let filekind = List.assoc_opt (Filename.extension t) rev_extension_list in
        let* kind =  match filekind with
            | None -> Error t
            | Some kind -> Ok kind
        in
        let kyoumi, c, co = match kind with
            | CFile -> kyoumi, t::c, co
            | COjectFile -> kyoumi, c, t::co
            | KyoumiFile -> t::kyoumi, c, co
        in
        split_file ~kyoumi ~c ~co q




let split_file files = 
    match split_file ~kyoumi:[] ~c:[] ~co:[] files with
    | Ok (kyoumi, c , co) -> Ok (`Kyofile kyoumi, `Cfile c, `CObjfile co)
    | Error _ as e -> e  

let version = 
    match Build_info.V1.version () with
    | None ->
        "n/a"
    | Some v ->
        Build_info.V1.Version.to_string v


  