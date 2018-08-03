/* Read from taf file and extracts content */
open Core;
open Lib.Util;

let headers = [
  "nombre",
  "min_peso",
  "max_peso",
  "kg_adicional",
  "precio_normal",
  "precio_mismo_dia",
  "tipo_vehiculo",
  "hub",
  "region_origen",
  "region_destino",
  "zona",
  "tipo",
];

let getKey = (key, map) =>
  switch (String.Map.find(map, key)) {
  | Some(res) => res
  | None => ""
  };

let getFromMap = map => (
  getKey("zona", map),
  getKey("hub", map),
  getKey("precio_normal", map) |> int_of_string,
);

let csvTransform = getFromMap << String.Map.of_alist_exn << Csv.Row.to_assoc;

let readFile = path => {
  let csv = Csv.Rows.load(~has_header=true, ~header=headers, path);
  List.map(~f=csvTransform, csv);
};

let getPath = () =>
  switch (Sys.argv) {
  | [|_, path|] => Some(path)
  | _ => None
  };

let () = {
  print_string("executing...\n");
  getPath()
  |> Option.bind(~f=path => {
       print_string $ "Reading file " ++ path ++ "\n";
       Some(readFile(path));
     })
  |> Option.bind(~f=csv => {
       printf("Result---CSV---------------------------------\n");
       List.iter(
         ~f=
           ((zona, hub, price)) =>
             print_string
             $ zona
             ++ ","
             ++ hub
             ++ ","
             ++ string_of_int(price)
             ++ "\n",
         csv,
       )
       |> Option.some;
     })
  |> ignore;
};
