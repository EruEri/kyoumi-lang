open Util.Position


module Pattern = struct
  open KyoumiAst.KExpresssion
  let rec flatten_por pattern =
    match pattern.value with
    | POr patterns ->
        patterns |> List.map flatten_por |> List.flatten
    | _ ->
        pattern :: []
end