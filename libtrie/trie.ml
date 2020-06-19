open Base
 
type word = char list
type 'a t = Node of 'a list * 'a arc list [@@deriving show] and 'a arc = char * 'a t [@@deriving show]

let empty l = Node (l,[])

let string_to_word (st:string) : word = 
  let rec loop acc i =
    if(i<0) then acc else loop ((String.get st i) ::acc) (i-1) in 
    loop [] (String.length st-1) 


let word_to_string (w:word) : string =
  String.concat ~sep:"" (List.map ~f:(fun l->String.make 1 l)  w)


let word_to_trie w l =
  let rec loop t s = 
    match s with 
    |[]->t
    |h::q-> Node([],[(h,loop t q)]) 
  in
  loop (empty l) w
 

let rec size (t: 'a t) : int = 
  match t with
  |Node([],[])->0
  |Node(a,l) ->
                let x =(List.map ~f:(fun(_,b)->b) l) in 
                  let rec sum_size ~f l =
                    match l with
                    | [] -> 0
                    | h::q -> f h + sum_size ~f q
                  in
                    if ((List.length a)=0) then sum_size ~f:(fun s ->size s) x else 1+sum_size ~f:(fun s -> size s) x                      


let rec arc_size (t: 'a t) : int  = 
  match t with
  |Node([],[])->0
  |Node(_,l) ->
                let x =(List.map ~f:(fun(_,b)->b) l) 
                in 
                  let rec sum_size ~f l =
                    match l with
                    | [] -> 0
                    | h::q -> f h + sum_size ~f q
                  in
                    List.length l + sum_size ~f:(fun s ->arc_size s) x 



let rec find (t:'a t) (w:word) : 'a list = 
  match (t,w) with 
  |Node([],[]),[]->[]
  |Node([],[]), _ -> []
  |Node(a,_),[]-> if ((List.length a)=0) then [] else a
  |Node(_,l) , h::q-> let rec find_letter ch ll=
                                match ll with 
                                |[]-> Node([],[])
                                |(x,y)::z -> if (Char.equal x ch) then y else find_letter ch z
                                in 
                                  let tr = find_letter h l 
                                  in 
                                    match tr with 
                                    |Node([],[])-> []
                                    |_-> find tr q   

               

let rec mem (t:'a t) (w:word) : bool = 
  match (t,w) with 
  |Node([],[]),[]->false
  |Node([],[]), _ -> false
  |Node(a,_),[]-> if ((List.length a)=0) then false else true
  |Node(_,l) , h::q-> let rec find_letter ch ll=
                                match ll with 
                                |[]-> Node([],[])
                                |(x,y)::z -> if (Char.equal x ch) then y else find_letter ch z
                                in 
                                  let tr= find_letter h l 
                                  in 
                                    match tr with 
                                    |Node([],[])-> false
                                    |_-> mem tr q   



let extract (t: 'a t): (word * 'a list) list =
  let rec loop acc (t:'a t) = 
    match t with 
      |Node([],[])->[]
      |Node(a,l) ->    
                      let rec sum_size ~f acc l =
                        match l with
                        | [] -> []
                        | (x,y)::z  -> f (acc@[x]) y @sum_size ~f acc z
                      in
                      if ((List.length a)=0) then
                        sum_size ~f:(fun s -> loop s) acc l 
                      else 
                        ((acc),a)::sum_size ~f:(fun s -> loop s) acc l                        
  in loop [] t 


(* Zipper functions implementation *)

exception Up
exception Down
exception Left
exception Right

type 'a path = 
  | Top
  | Context of ('a list * char) * ('a arc ) list * 'a path * ('a arc ) list  


type 'a zipper = Zipper of 'a path * 'a t

(* Renvoie un zipper avec un focus sur la racine *)
let trie_to_zipper (t:'a t) : 'a zipper =   Zipper(Top,t)
     
(* déplace le focus vers le noeud pere *)
(* lève l'exception Up si le focus est la racine de l'arbre *)
let zip_up_exn (Zipper(p,Node(a,b)): 'a zipper) : 'a zipper = 
  match p with 
    | Top -> raise Up 
    | Context((info,char),left,up,right) -> Zipper(up,Node(info,(List.rev left)@[(char,Node(a,b))]@right))

(* déplace le focus vers le noeud fils lève l'exception Down si le noeud n'a pas de noeud fils *)
let zip_down_exn (Zipper(p,Node(a,b)): 'a zipper) : 'a zipper = 
  match b with 
  | []-> raise Down
  |(c,d)::l -> Zipper(Context((a,c),[],p,l),d)

let zip_right_exn (Zipper(p,t): 'a zipper) : 'a zipper =  
  match p with 
  |Top -> raise Right
  |Context(_,_,_,[]) -> raise Right
  |Context((info,char),left,up,(x,y)::right) -> Zipper(Context((info,x),(char,t)::left,up,right),y)


let zip_left_exn (Zipper(p,t): 'a zipper) : 'a zipper =  
  match p with 
  |Top -> raise Left
  |Context(_,[],_,_) -> raise Right
  |Context((info,char),(x,y)::left,up,right) -> Zipper(Context((info,x),left,up,(char,t)::right),y)

let rec zip_up_until (f : ('a zipper -> bool)) (z : 'a zipper) : 'a zipper = 
    if (Bool.to_int (f z) = 1)
        then z 
    else zip_up_until f (zip_up_exn z)


let rec zip_down_until (f : ('a zipper -> bool))  (z : 'a zipper) : 'a zipper = 
    if (Bool.to_int (f z) = 1)
        then z 
    else zip_down_until f (zip_down_exn z)
    

let rec zip_left_until (f : ('a zipper -> bool)) (z : 'a zipper) : 'a zipper = 
    if (Bool.to_int (f z) = 1)
        then z 
    else zip_left_until f (zip_left_exn z)


let rec zip_right_until (f : ('a zipper -> bool)) (z : 'a zipper) : 'a zipper = 
    if (Bool.to_int (f z) = 1)
        then z 
    else zip_right_until f (zip_right_exn z)


let zipper_to_trie (z:'a zipper) : 'a t = 
  let (Zipper(_,t)) = zip_up_until (fun (Zipper(p,_))-> match p with |Top-> true |_->false) z
          in t


let zip_insert_right (Zipper(p,t):'a zipper) (c:char) (tr : 'a t) : 'a zipper = 
      match p with 
      |Top -> raise Right
      |Context((info,char),left,up,right) -> Zipper(Context((info,char),left,up,(c,tr)::right),t)


let zip_insert_left (Zipper(p,t):'a zipper) (c:char) (tr : 'a t) : 'a zipper = 
      match p with 
      |Top -> raise Left
      |Context((info,char),left,up,right) -> Zipper(Context((info,char),(c,tr)::left,up,right),t)


let insert  (t:'a t) (w : word) (d:'a) : 'a t = 
   match t,w with  
   |_,[]->t
   |Node([],[]),_-> word_to_trie w [d]
   |_ -> 
        let zip= trie_to_zipper t     
        in 
          let insertletters (z:'a zipper) (w:word): 'a zipper = 
                match w with 
                |[]->z
                |_-> let Node(_,b) = word_to_trie w [d] in   
                              match b with 
                              |[]->z
                              |(x,y)::_->  zip_insert_right z x y
          in 
              let rec find_same_letters (Zipper(p,t):'a zipper) (w:word)=
              match w with
                |[]-> Zipper(p,t),w
                |h::q-> 
                    let l=
                      try (zip_down_exn (Zipper(p,t)))  with |Down-> Zipper(Top,t) in
                      (* On considére le cas d'erreur comme un Top pour faciliter son traitement pour pouvoir renvoyer le meme zipper *)
                        match l with   
                          |Zipper(Top,_)-> Zipper(p,t),w
                          |Zipper(Context((_,char),_,_,_),_) -> (
                          if(Char.equal h char) then find_same_letters l q 
                                else 
                            let ll= 
                            try  (zip_right_until (fun (Zipper(u,_))-> 
                                match u with 
                                  |Top->false
                                  |Context((_,char),_,_,_) -> if (Char.equal h char) then true else false) l)
                            with  
                                  |Right-> Zipper(Top,t)
                            in 
                                match ll with 
                                  |Zipper(Top,_)-> l,w
                                  |Zipper(x,y)-> find_same_letters (Zipper(x,y)) q 
                                )     
            in 
                let ((Zipper(n,Node(x,y))),b)= find_same_letters zip w in 
                  match b with 
                    |[] -> zipper_to_trie ((Zipper(n,Node([d],y))))
                    |_-> zipper_to_trie (insertletters (Zipper(n,Node(x,y))) b)
