type position = {
  start_position : Lexing.position;
  end_position : Lexing.position;
}

type 'a location = { value : 'a; position : position }

let map f location = { location with value = f location.value }
let map_use f location = { value = f location; position = location.position }
let value { value; _ } = value
let position { position; _ } = position