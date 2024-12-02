(* Location Picker for Tabbed Dialogs *)
open Js_of_ocaml
open Js_of_ocaml_tyxml
open Utils

let city = ref ""
let region = ref ""
let latitude = ref 0.0
let longitude = ref 0.0

let confirm_my_button msg = fun _ ->
  let element = Js_of_ocaml.Dom_html.getElementById msg in
  set_static_text element ("City selected: "^ !city^", "^ !region ^ " (latitude="^ string_of_float !latitude^", longitude="^string_of_float !longitude ^ ")");
  true

let update_city_options selected_timezone =
  print_endline selected_timezone;
  let open Tyxml_js.Html in
  let city_dropdown = Js_of_ocaml.Dom_html.getElementById "city-specific-dropdown" in
  let city_options = 
    match Hashtbl.find_opt Base_locations.loch selected_timezone with
    | None -> []  (* Handle case where timezone isn't found *)
    | Some cities ->
        List.mapi (fun idx (name, region, lat, long) -> 
          option 
            ~a:[a_value (string_of_int idx)] 
            (txt (name ^ " " ^ region))
        ) cities
  in
  print_endline ("city options: "^string_of_int (List.length city_options));

  let city_dropdown_opt = Js.Opt.return city_dropdown in
  Js.Opt.iter city_dropdown_opt (fun dropdown ->
    dropdown##.innerHTML := Js.string "";
    List.iter (fun opt -> 
      let dom_opt = Tyxml_js.To_dom.of_option opt in
      Dom.appendChild dropdown dom_opt
    ) city_options;
    dropdown##.style##.display := Js.string "block"
  )

let create_location_picker tz =
  let open Tyxml_js.Html in  
  let message_div = div ~a:[a_id "city-message"; a_style "padding-top: 15px; font-size: 16px; color: #333; display: none;"] [txt ""] in
  let select_div = div ~a:[a_id "city-select"; a_style "padding-top: 15px; font-size: 16px; color: #333; display: none;"] [txt ""] in
  let button = button ~a:[ a_id "city-button"; a_onclick (confirm_my_button "city-message") ] [ txt "Set City" ] in

  let timezone_dropdown =
    select
      ~a:[ 
        a_id "city-timezone-dropdown";
        a_onchange (fun ev ->
          Js.Opt.case (ev##.target)
            (fun () -> false)
            (fun target -> let select = Dom_html.CoerceTo.select target in
              Js.Opt.case select
                (fun () -> false)
                (fun select -> update_city_options (Js.to_string (select##.value)); true)
            )
        )
      ]
      (Hashtbl.fold 
        (fun timezone _ acc -> 
          option 
            ~a:(a_value timezone :: if timezone=tz then [a_selected ()] else []) 
            (txt timezone)
          :: acc
        ) 
        Base_locations.loch
        []
      |> List.sort compare)
  in
(* In location.ml, modify the city dropdown's onchange handler *)

  let city_dropdown =
    select
      ~a:[ 
        a_id "city-specific-dropdown";
        a_onchange (fun ev ->
          Js.Opt.case (ev##.target)
            (fun () -> false)
            (fun target ->
              let select = Dom_html.CoerceTo.select target in
              Js.Opt.case select
                (fun () -> false)
                (fun select ->
                  let selected_index = int_of_string (Js.to_string (select##.value)) in
                  let element = Js_of_ocaml.Dom_html.getElementById "city-select" in
                  let selected_timezone =
                    let select_element = Js_of_ocaml.Dom_html.getElementById "city-timezone-dropdown" in
                    Js.Opt.case (Dom_html.CoerceTo.select select_element)
                    ( fun () -> "")  (* Handle the case where the element is not a select *)
                    ( fun select -> Js.to_string select##.value) in
                  match Hashtbl.find_opt Base_locations.loch selected_timezone with
                  | None -> true  (* Handle case where timezone isn't found *)
                  | Some cities ->
                      let (name, regn, lat, long) = List.nth cities selected_index in
                      city := name;
                      region := regn;
                      latitude := lat;
                      longitude := long;
 		      (* Save to cookies *)
		      Cookie.set "city" name;
		      Cookie.set "area" regn;
		      Cookie.set "latitude" (string_of_float lat);
		      Cookie.set "longitude" (string_of_float long);
		      Cookie.set "timezone" selected_timezone;
                     true
                )
            )
        )
      ]
      [] in

  let output = div ~a:[ a_id "city-output"; a_style "margin-top: 20px;" ] [] in
  div [ button; timezone_dropdown; br (); city_dropdown; br (); output; message_div; select_div ]

let restore() = 
 (* Restore saved values if they exist *)
  begin match Cookie.get "city", Cookie.get "area", Cookie.get "latitude", Cookie.get "longitude" with
  | Some saved_city, Some saved_area, Some saved_lat, Some saved_long ->
      city := saved_city;
      region := saved_area;
      latitude := float_of_string saved_lat;
      longitude := float_of_string saved_long;
      
      (* Update timezone dropdown *)
      let tz_dropdown = Js_of_ocaml.Dom_html.getElementById "city-timezone-dropdown" in
      let city_dropdown = Js_of_ocaml.Dom_html.getElementById "city-specific-dropdown" in
      begin match Cookie.get "timezone" with
      | Some saved_tz ->
          Js.Opt.iter (Dom_html.CoerceTo.select tz_dropdown) (fun dropdown ->
            dropdown##.value := Js.string saved_tz;
            (* Update city dropdown *)
            update_city_options saved_tz;
            (* After city options are updated, set the correct city *)
            Js.Opt.iter (Dom_html.CoerceTo.select city_dropdown) (fun citydrop ->
              let cities = Hashtbl.find Base_locations.loch saved_tz in
              let city_index = 
                let rec find_index i = function
                  | [] -> raise Not_found
                  | (name, region, _, _)::_ when name = saved_city && region = saved_area -> i
                  | _::rest -> find_index (i+1) rest
                in
                find_index 0 cities
              in
              citydrop##.value := Js.string (string_of_int city_index)
            )
          )
      | None -> ()
      end;

      let element = Js_of_ocaml.Dom_html.getElementById "city-message" in
      set_static_text element ("City selected: "^ saved_city ^", "^ saved_area ^ 
                              " (latitude="^ saved_lat ^
                              ", longitude="^ saved_long ^ ")")
  | _ -> ()
  end
