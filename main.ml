(* main.ml *)

   external _main : unit -> unit = "_main"

   let () =
     (* Initialize and call your Emscripten functions *)
     _main () (* Call the main function *)
