open Libtrie
open Base
open Stdio

type word_diff = int * char list [@@deriving eq]
type word_diff_cat = {diff: word_diff; cat:string} [@@deriving eq]
type t = word_diff_cat Trie.t

(* code for diff and word_patch *)
let diff (l:Trie.word) (f:Trie.word) : word_diff = 
 let rec loop ll ff (a,b) = 
  match (ll,ff) with
      |[],[]->(a,b)
      |_,[]-> (a,ll)
      |[],_->(List.length(ff),b)
      |h::q,l::r-> if (Char.equal h l) then loop q r (a,b) else (List.length(ff),ll)
  in loop l f (0,[])


let word_patch w wd = 
  let rec loop ww acc (a,b) = 
      match ww with 
      |[]->[]
      |h::q->if((List.length(w))-acc=a) then b else h::loop q (1+acc) (a,b) 
  in loop w 0 wd

let extract_line l = 
 let ll= String.split ~on:'\t' l in 
 let a = (List.hd_exn ll) in 
         let b=(List.hd_exn (String.split ~on:'_' (List.nth_exn ll 4)))
          in
            let c=(List.nth_exn ll 2) in 
                   (a,c,b)
    
let extract ic = 
    let read () =
      try In_channel.input_line ic with End_of_file -> None in
      let rec loop acc = match read () with
        | Some s -> loop ((extract_line s) :: acc)
        | None -> In_channel.close ic; List.rev acc in
    loop []

let lexicon =
  In_channel.create Sys.argv.(1)
  |> extract
  |> List.fold
    ~init:(Trie.empty [])
    ~f:(fun acc (f,c,l) ->
        let d = diff (Trie.string_to_word l) (Trie.string_to_word f) in
        let wc = {diff=d;cat=c} in
        if List.mem (Trie.find acc (Trie.string_to_word f)) wc ~equal:equal_word_diff_cat
        then acc else Trie.insert acc (Trie.string_to_word f) wc)



let () = printf "created trie %d\n%!" (Trie.size lexicon);
  let oc = Stdlib.open_out_bin "./lexicon.bin" in
  Caml.Marshal.to_channel oc lexicon []



let make_lemmatize t = fun s -> Trie.find t s |> List.map ~f:(fun {diff;cat} -> (word_patch s diff,cat))
let lemmatizer = make_lemmatize lexicon

let rec loop () =
 let () = printf "Entrer forme à lemmatiser:\n%!" in
  match Stdio.In_channel.input_line Stdio.stdin with
  | None -> ()
  | Some s ->
    let l = Trie.string_to_word s |> lemmatizer in
    let () = List.iter ~f:(fun (l,c)-> printf "%s %s\n" (Trie.word_to_string l) c) l
    in loop ()


let () = loop ()
