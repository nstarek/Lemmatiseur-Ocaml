open Libtrie
open Base
open Stdio

let extract_line l = 
 let ll= String.split ~on:'\t' l in 
 Trie.string_to_word(List.hd_exn ll),Trie.string_to_word(List.nth_exn ll 2),Trie.string_to_word(List.hd_exn (String.split ~on:'_' (List.nth_exn ll 4)))
    
let extract ic = 
    let read () =
      try In_channel.input_line ic with End_of_file -> None in
      let rec loop acc = match read () with
        | Some s -> loop ((extract_line s) :: acc)
        | None -> In_channel.close ic; List.rev acc in
    loop []

    
let () =
  let lexicon = In_channel.create Sys.argv.(1)
                |> extract
                |> List.fold ~init:(Trie.empty []) ~f:(fun acc (f,_,_) -> Trie.insert acc f ()) in
  let lwords = Trie.extract lexicon in
  let () = List.iter lwords ~f:(fun (s,_) -> printf "%s\n" (Trie.word_to_string s)) in
  let oc = Stdlib.open_out_bin "lexicon.bin" in
  Caml.Marshal.to_channel oc lexicon []
