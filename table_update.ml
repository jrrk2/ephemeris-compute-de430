open Js_of_ocaml
open Js_of_ocaml_tyxml

let table_element =
  let open Tyxml_js.Html in
  let headers = [ "Date (UT) HR:MN"; "Date JDUT"; "R.A. (ICRF)"; "DEC"; "Azi (r-app)"; "Elev"; "L Ap Sid Time" ] in
  let hwidths = [ 150; 100; 100; 100; 100; 100; 100 ] in
  let header_row =
    tr ~a:[
      a_style "background-color: #f8f9fa; border-bottom: 2px solid #dee2e6;"
    ] (
      List.map2 (fun header wid -> 
        th ~a:[
          a_style ("min-width: " ^ string_of_int wid ^ "px; " ^
                  "padding: 12px 16px; " ^
                  "text-align: left; " ^
                  "font-weight: 600; " ^
                  "color: #495057; " ^
                  "border-right: 1px solid #dee2e6;")
        ] [txt header]
      ) headers hwidths
    )
  in
  tablex 
    ~a:[
      a_id "results-table";
      a_style ("border-collapse: collapse; " ^
               "width: 100%; " ^
               "border: 1px solid #dee2e6; " ^
               "border-radius: 4px; " ^
               "margin: 16px 0; " ^
               "background-color: white; " ^
               "box-shadow: 0 1px 3px rgba(0,0,0,0.1);")
    ]
    ~thead:(thead [header_row])
    [tbody []]

let update_cell_style cell =
  cell##.style##.padding := Js.string "12px 16px";
  cell##.style##.borderRight := Js.string "1px solid #dee2e6";
  cell##.style##.borderBottom := Js.string "1px solid #dee2e6"

let update_table_row ?(index=0) ~date_ut ~date_jdut ~ra ~dec ~azi ~elev ~l_ap_sid_time () =
  try
    let table = Js_of_ocaml.Dom_html.getElementById "results-table" in
    let tbody = Js.Opt.to_option (table##querySelector (Js.string "tbody")) in
    
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
                  cell##.textContent := Js.some (Js.string value);
                  update_cell_style cell;
                  if idx mod 2 = 0 then
                    cell##.style##.backgroundColor := Js.string "#ffffff"
                  else
                    cell##.style##.backgroundColor := Js.string "#f8f9fa"
                )
            in
            update_cell 0 date_ut;
            update_cell 1 date_jdut;
            update_cell 2 ra;
            update_cell 3 dec;
            update_cell 4 azi;
            update_cell 5 elev;
            update_cell 6 l_ap_sid_time;
            
            (* Add hover effect to row *)
            row##.onmouseover := Dom_html.handler (fun _ ->
              row##.style##.backgroundColor := Js.string "#f2f2f2";
              Js._true
            );
            row##.onmouseout := Dom_html.handler (fun _ ->
              row##.style##.backgroundColor := Js.string "transparent";
              Js._true
            )
          )
    | None -> 
        Printf.printf "No tbody found in table\n"
  with 
  | exn -> 
      Printf.printf "Exception occurred: %s\n" (Printexc.to_string exn)

let append_table_row ~date_ut ~date_jdut ~ra ~dec ~azi ~elev ~l_ap_sid_time () =
  try
    let table = Js_of_ocaml.Dom_html.getElementById "results-table" in
    let tbody_opt = Js.Opt.to_option (table##querySelector (Js.string "tbody")) in
    
    match tbody_opt with
    | Some tbody_elem ->
        let new_row = Js_of_ocaml.Dom_html.createTr Js_of_ocaml.Dom_html.document in
        
        (* Add hover effect to new row *)
        new_row##.onmouseover := Dom_html.handler (fun _ ->
          new_row##.style##.backgroundColor := Js.string "#f2f2f2";
          Js._true
        );
        new_row##.onmouseout := Dom_html.handler (fun _ ->
          new_row##.style##.backgroundColor := Js.string "transparent";
          Js._true
        );

        let cell_values = [
          date_ut; 
          date_jdut; 
          ra; 
          dec; 
          azi; 
          elev; 
          l_ap_sid_time
        ] in

        let maplst = List.mapi (fun i value ->
          let cell = Js_of_ocaml.Dom_html.createTd Js_of_ocaml.Dom_html.document in
          cell##.textContent := Js.some (Js.string value);
          update_cell_style cell;
          if i mod 2 = 0 then
            cell##.style##.backgroundColor := Js.string "#ffffff"
          else
            cell##.style##.backgroundColor := Js.string "#f8f9fa";
          new_row##appendChild (cell :> Dom.node Js.t)
        ) cell_values in

        let _ = tbody_elem##appendChild (new_row :> Dom.node Js.t) in ()

    | None -> 
        Printf.printf "No tbody found in table\n"
  with
  | exn -> 
Printf.printf "Exception occurred: %s\n" (Printexc.to_string exn)
