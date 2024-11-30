open Js_of_ocaml
open Js_of_ocaml_tyxml

let create_results_table data =
  let open Tyxml_js.Html in
  let headers = 
    [ "Date (UT) HR:MN"; "Date JDUT"; "R.A. (ICRF)"; "DEC"; "Azi (r-app)"; "Elev"; "L Ap Sid Time" ]
  in
  let header_row = 
    tr (List.map (fun header -> th [txt header]) headers)
  in
  let rows = List.map (fun (date_ut, date_jdut, ra, dec, azi, elev, l_ap_sid_time) ->
    tr [
      td [txt date_ut];
      td [txt date_jdut];
      td [txt ra];
      td [txt dec];
      td [txt azi];
      td [txt elev];
      td [txt l_ap_sid_time];
    ]
    ) data in
  tablex ~a:[
    a_id "results-table";
    a_style "border-collapse: collapse; width: 100%;"
  ] ( tbody ( header_row :: rows ) :: [] )

(* Example data *)
let data = [
  ("2024-11-26 12:34", "2451545.0", "10h 00m 00s", "-30° 00' 00\"", "120.0", "45.0", "23h 59m 59s");
  ("2024-11-26 13:34", "2451545.1", "11h 00m 00s", "-31° 00' 00\"", "121.0", "46.0", "00h 00m 00s")
]

(* Usage *)
let table_element = create_results_table data

let update_table_row ?(index=0) ~date_ut ~date_jdut ~ra ~dec ~azi ~elev ~l_ap_sid_time () =
  try
    (* Debug: Print the index being updated *)
    Printf.printf "Attempting to update row at index %d\n" index;

    (* Attempt to get the table *)
    let table = 
      try Js_of_ocaml.Dom_html.getElementById "results-table" 
      with Not_found -> 
        Printf.printf "Table not found by ID\n";
        failwith "Table not found"
    in

    (* Debug: Check if table is found *)
    Printf.printf "Table found successfully\n";

    (* Try to get tbody *)
    let tbody = 
      Js.Opt.to_option (table##querySelector (Js.string "tbody"))
    in

    match tbody with
    | Some tbody_elem ->
        (* Debug: Print number of rows *)
        let rows = tbody_elem##querySelectorAll (Js.string "tr") in
        Printf.printf "Total rows: %d\n" rows##.length;

        if index < rows##.length then
          let row = rows##item index in
          Js.Opt.iter row (fun row ->
            let cells = row##querySelectorAll (Js.string "td") in
            
            (* Debug: Print number of cells *)
            Printf.printf "Total cells in row: %d\n" cells##.length;

            let update_cell idx value =
              if idx < cells##.length then
                Js.Opt.iter (cells##item idx) (fun cell ->
                  cell##.textContent := Js.some (Js.string value);
                  Printf.printf "Updated cell %d with value: %s\n" idx value
                )
            in
            update_cell 0 date_ut;
            update_cell 1 date_jdut;
            update_cell 2 ra;
            update_cell 3 dec;
            update_cell 4 azi;
            update_cell 5 elev;
            update_cell 6 l_ap_sid_time
          )
        else 
          Printf.printf "Index %d is out of bounds\n" index
    | None -> 
        Printf.printf "No tbody found in table\n"
  with 
  | exn -> 
      Printf.printf "Exception occurred: %s\n" (Printexc.to_string exn)

(*
let update_table_row ?(index=0) ~date_ut ~date_jdut ~ra ~dec ~azi ~elev ~l_ap_sid_time () =
  let table = Js_of_ocaml.Dom_html.getElementById "results-table" in
  let tbody = 
    Js.Opt.to_option (table##querySelector (Js.string "tbody"))
  in
  match tbody with
  | Some tbody_elem ->
      let rows = tbody_elem##querySelectorAll (Js.string "tr") in
      if index < rows##.length then
        let row = rows##item index in
        Js.Opt.iter row (fun row ->
          let cells = row##querySelectorAll (Js.string "td") in
          let update_cell idx value =
            if idx < cells##.length then
              Js.Opt.iter (cells##item idx) (fun cell ->
                cell##.textContent := Js.some (Js.string value)
              )
          in
          update_cell 0 date_ut;
          update_cell 1 date_jdut;
          update_cell 2 ra;
          update_cell 3 dec;
          update_cell 4 azi;
          update_cell 5 elev;
          update_cell 6 l_ap_sid_time
        )
  | None -> ()
*)
