open Base
open Stdio

type 'a stream = Nil | Cons of 'a * 'a stream thunk and 'a thunk = unit -> 'a

let extract_line l = 
 let ll= String.split ~on:'\t' l in 
  List.hd_exn ll,List.nth_exn ll 2,List.hd_exn (String.split ~on:'_' (List.nth_exn ll 4))

(* val extract : In_channel -> (string*string*string) stream *)
let rec extract ic = 
   let st= In_channel.input_line ic in 
   match st with 
   |None -> Nil
   |Some s-> 
      Cons(extract_line s, fun() -> extract ic)

(* val iter_stream : 'a stream -> ('a -> unit) -> unit *)
let rec iter_stream st ~f = 
  match st with 
  |Nil -> ()
  |Cons(h,t)-> f h ; iter_stream (t()) ~f

  (*|Cons(h,t)-> let _= f h in iter_stream (t()) ~f  : on a essayé comme ca mais on a toujours un petit probléme de type 
  val iter_stream : 'a stream -> f:('a -> 'b) -> unit = <fun>    
  a part ca tout marche parfaitement *)

let () =
  In_channel.create (Sys.get_argv()).(1)
  |> extract
  |> iter_stream ~f:(fun (f,c,l) -> printf "%s %s %s\n" f c l)


