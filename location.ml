(* Location Picker for Tabbed Dialogs *)
open Js_of_ocaml
open Js_of_ocaml_tyxml
open Utils

let city = ref ""
let region = ref ""
let latitude = ref 0.0
let longitude = ref 0.0

let grouped_cities =
  let grouped = ref [] in
  Hashtbl.iter (fun k x -> grouped := (k,x) :: !grouped) Base_locations.loch;
  List.sort compare !grouped

let confirm_my_button msg = fun _ ->
  let element = Js_of_ocaml.Dom_html.getElementById msg in
  set_static_text element ("City selected: "^ !city^", "^ !region ^ " (latitude="^ string_of_float !latitude^", longitude="^string_of_float !longitude ^ ")");
  true

let update_city_options selected_timezone =
  print_endline selected_timezone;
  let open Tyxml_js.Html in
  let city_dropdown = Js_of_ocaml.Dom_html.getElementById "city-specific-dropdown" in
  let city_options = 
    List.find (fun (timezone, _) -> timezone = selected_timezone) grouped_cities 
    |> snd 
    |> List.mapi (fun idx (name, region, lat, long) -> 
      option 
        ~a:[a_value (string_of_int idx)] 
        (txt (name ^ " " ^ region))
    )
  in
  print_endline ("city options: "^string_of_int (List.length city_options));

  (* Convert to Js.opt before using Js.Opt.iter *)
  let city_dropdown_opt = Js.Opt.return city_dropdown in
  Js.Opt.iter city_dropdown_opt (fun dropdown ->
    (* Clear existing options *)
    dropdown##.innerHTML := Js.string "";

    (* Add new options *)
    List.iter (fun opt -> 
      let dom_opt = Tyxml_js.To_dom.of_option opt in
      Dom.appendChild dropdown dom_opt
    ) city_options;

    (* Show city dropdown *)
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
      (List.map 
        (fun (timezone, _) -> 
          option 
            ~a:(a_value timezone :: if timezone=tz then [a_selected ()] else []) 
            (txt timezone)
        ) 
        grouped_cities)
  in
  
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
                  let (name, regn, lat, long) = 
                    List.nth (snd (List.find (fun (timezone, _) -> timezone = selected_timezone) grouped_cities)) selected_index
                    in
		  city := name;
		  region := regn;
		  latitude := lat;
		  longitude := long;
                  true
                )
            )
        )
      ]
      []
  in
  
  let output = div ~a:[ a_id "city-output"; a_style "margin-top: 20px;" ] [] in
  div [ button; timezone_dropdown; br (); city_dropdown; br (); output; message_div; select_div ]
