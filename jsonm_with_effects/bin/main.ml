open Effect
open Effect.Deep

(* Adds 2 effects to Jsonm:
   - `Await` when there is no more input to read
   - `End` when a valid value has been parsed and there is no more input *)

type _ Effect.t += Await : unit t | End : unit t

let wrapper (decoder : Jsonm.decoder) =
  let rec loop decoder =
    match Jsonm.decode decoder with
    | `Lexeme l ->
      print_string (Format.asprintf "%a " Jsonm.pp_lexeme l);
      loop decoder
    | `End -> perform End
    | `Error e -> print_string (Format.asprintf "%a\n" Jsonm.pp_error e)
    | `Await ->
      perform Await;
      loop decoder
  in
  loop decoder
;;

let () =
  let list = [ "[\"Hell"; "o worl"; "d\"]" ] in
  let index = ref 0 in
  let decoder = Jsonm.decoder ~encoding:`UTF_8 `Manual in
  try_with
    wrapper
    decoder
    { effc =
        (fun (type a) (eff : a t) ->
          match eff with
          | Await ->
            Some
              (fun (k : (a, _) continuation) ->
                if !index = List.length list
                then (
                  Jsonm.Manual.src decoder Bytes.empty 0 0;
                  continue k ())
                else (
                  let new_str = List.nth list !index in
                  index := !index + 1;
                  Jsonm.Manual.src
                    decoder
                    (Bytes.of_string new_str)
                    0
                    (String.length new_str);
                  continue k ()))
          | End ->
            Some
              (fun (k : (a, _) continuation) ->
                print_string "EOF\n";
                continue k ())
          | _ -> None)
    }
;;
