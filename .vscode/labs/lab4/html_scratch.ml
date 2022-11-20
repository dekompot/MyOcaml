(*
type content = TEXT of string | 
A of string * content | ITALIC of content;;

(P(C(A("http://", ITALIC(TEXT("Hello"))))));;*)
let indent = " ";;
let nl = "\n";

type head_tag = None | Title of string * head_tag | Meta of string * head_tag;;

type body_tag = 
END 
| TEXT of string * body_tag  
| A of string * body_tag * body_tag  
| I of body_tag * body_tag  
| BR of body_tag 
| P of body_tag * body_tag 
| DIV of body_tag * body_tag 
| H1 of body_tag * body_tag  
| H2 of body_tag * body_tag  
| H3 of body_tag * body_tag  
| H4 of body_tag * body_tag 
| H5 of body_tag * body_tag  
| H6 of body_tag * body_tag 
| OL of list_element * body_tag  
| UL of list_element * body_tag  
and 
list_element = LI' of body_tag | LI of body_tag * list_element;;


(*lists can be nested
type list_tags =  
  OL of list_element | 
  UL of list_element;;

type btag = T of body_tag | L of list_tags | R of {t: btag; next: btag};;*)
type btag = B of body_tag | B' of {c: btag; next: btag};;
type htag = H of head_tags | H' of {c: htag; next: htag};;


let rec build_tag ind' = function
  | TEXT s -> s
  | A (link, c) -> ind'^"<a href="^link^">"^build_tag (ind' ^ indent) c^"</a>"^nl
  | I c -> "<i>"^build_tag (ind' ^ indent) c^"</i>"
  | BR -> "<\br>"^nl
  | DIV tag -> ind'^"<div>"^nl^build_tag (ind' ^ indent) tag^indent'^"</div>"^nl
  | P tag -> ind'^"<p>"^build_tag (ind' ^ indent) tag^"</p>"^nl
  | H1 tag -> ind'^"<h1>"^build_tag (ind' ^ indent) tag^"</h1>"^nl
  | H2 tag -> ind'^"<h2>"^build_tag (ind' ^ indent) tag^"</h2>"^nl
  | H3 tag -> ind'^"<h3>"^build_tag (ind' ^ indent) tag^"</h3>"^nl
  | H4 tag -> ind'^"<h4>"^build_tag (ind' ^ indent) tag^"</h4>"^nl
  | H5 tag -> ind'^"<h5>"^build_tag (ind' ^ indent) tag^"</h5>"^nl
  | H6 tag -> ind'^"<h6>"^build_tag (ind' ^ indent) tag^"</h6>"^nl
  | OL (tags, next) -> ind'^"<ol>"^nl^build_list_items (ind' ^ indent) tags^nl^ind'^"</ol>"^nl  
  | UL (tags, next) -> ind'^"<ul>"^nl^build_list_items (ind' ^ indent) tags^nl^ind'^"</ul>"^nl
and build_list_items ind' = function 
  | LI' tag -> ind'^"<li>"^build_tag ind' tag^"</li>"
  | LI (tag, next) -> ind'^"<li>"^build_tag ind' tag^"</li>"^nl^build_list_items ind' t

(*
let rec build_list = function 
  | OL tags -> "<ol>"^build_list_items tags^"</ol>"  
  | UL tags -> "<ul>"^build_list_items tags^"</ul>"*)

let rec build_head_tags indent' = function 
| Title s -> indent'^"<title>"^s^"</title>"^nl
| Meta s -> indent'^"<meta charset="^s^">"^nl;;
(*
let rec build_head = function
  | H tag -> build_head_tags tag
  | H' {c=tag; next=next} -> (build_head tag)^build_head next;; 

let rec build_body = function
  | B tag -> build_tag tag 
  | B' {c=tag; next=next} -> (build_body tag)^build_body next;;

let build (head, body) = 
  "<head>"^build_head head^"</head>"^nl^"<body>"^build_body body^"</body>"^nl;;

build (H'{c=H(Title("Hello World")); next=H(Meta("UTF-8"))}, 
      B'{c=B(H2(TEXT"Neural Networks")); next=B'{c=B(TEXT"A"); next=B'{c=B(A("https://en.wikipedia.org/wiki/Neural_network", TEXT"A neural network")); next=B(TEXT"is a network or circuit of biological neurons, or,
    in a modern sense, an artificial neural network, composed of artificial neurons or nodes")}}});;*)

let rec build_head' = function  
| h::t -> build_head_tags indent h ^ build_head' t 
| [] -> "";;

let rec build_body' = function 
| h::t -> build_tag indent h ^ build_body' t 
| [] -> "";;  

let rec build' (head, body) = 
  "<head>"^nl^build_head' head^"</head>"^nl^"<body>"^nl^build_body' body^"</body>";;

print_string (build' ([Meta("UTF-8"); Title("Hello World")], 
  [H2(A("https://en.wikipedia.org/wiki/Neural_network",TEXT("Neural Networks")));
   P(TEXT("A neural network is a network or circuit of biological neurons, or,
  in a modern sense, an artificial neural network, composed of artificial neurons or nodes")); 
  H5(TEXT("List of content")); 
  UL(LI{c=TEXT("History"); next=LI{c=TEXT("Artificial Intelligence"); next=LI'(TEXT("Applications"))}}); 
  DIV(P(TEXT("Neural networks architecture is inspired by brain structure. When activities are repeated,
  the connections between those neurons are strengthened"))); 
  H4(TEXT("The most important applications")); 
  OL(LI{c=TEXT("Computer Vision"); next=LI{c=TEXT("Speech Recognition"); next=LI'(TEXT("Self-driving cars"))}})]));;