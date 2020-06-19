open Base
open Stdio

(* val extract: In_channel -> (string,string,string) list *)
(* lit chaque ligne du fichier (In_channel est équivalent du FILE* du langage C) *)
(* et crée une liste de triplets (forme,catégorie,lemme) *)
(* pour transformer le ic en string, vous utiliserez les fonctions input_line ou fold_lines *)
(* cf. la documentation https://ocaml.janestreet.com/ocaml-core/latest/doc/stdio/Stdio/In_channel/index.html *)
(* vous appellerez ensuite extract_line *)

let extract_line l = 
 let ll= String.split ~on:'\t' l in 
  List.hd_exn ll,List.nth_exn ll 2,List.hd_exn (String.split ~on:'_' (List.nth_exn ll 4))
    
let extract ic = 
    let read () =
      try In_channel.input_line ic with End_of_file -> None in
      let rec loop acc = match read () with
        | Some s -> loop ((extract_line s) :: acc)
        | None -> In_channel.close ic; List.rev acc in
    loop []

let () =
  In_channel.create (Sys.get_argv()).(1)
  |> extract
  |> List.iter ~f:(fun (f,c,l) -> printf "%s %s %s\n" f c l)
