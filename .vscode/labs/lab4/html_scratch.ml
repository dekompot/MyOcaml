(*
type content = TEXT of string | 
A of string * content | ITALIC of content;;

(P(C(A("http://", ITALIC(TEXT("Hello"))))));;*)
let indent = " ";;
let nl = "\n";

type head_tags = Title of string | Meta of string;;

type body_tag = 
TEXT of string 
| A of string * body_tag 
| I of body_tag 
| BR
| P of body_tag
| DIV of body_tag
| H1 of body_tag 
| H2 of body_tag 
| H3 of body_tag 
| H4 of body_tag
| H5 of body_tag 
| H6 of body_tag
| OL of list_element 
| UL of list_element 
and 
list_element = LI' | LI of {c: body_tag; next: list_element};;

(*lists can be nested
type list_tags =  
  OL of list_element | 
  UL of list_element;;

type btag = T of body_tag | L of list_tags | R of {t: btag; next: btag};;*)
type btag = B of body_tag | B' of {c: btag; next: btag};;
type htag = H of head_tags | H' of {c: htag; next: htag};;


let rec build_tag = function
  | TEXT s -> indent^s^nl
  | A (link, c) -> indent^"<a href="^link^">"^build_tag c^"</a>"^nl
  | I c -> "<i>"^build_tag c^"</i>"
  | BR -> "<\br>"^nl
  | DIV tag -> indent^"<div>"^build_tag tag^"</div>"^nl
  | P tag -> indent^"<p>"^build_tag tag^"</p>"^nl
  | H1 tag -> indent^"<h1>"^build_tag tag^"</h1>"^nl
  | H2 tag -> indent^"<h2>"^build_tag tag^"</h2>"^nl
  | H3 tag -> indent^"<h3>"^build_tag tag^"</h3>"^nl
  | H4 tag -> indent^"<h4>"^build_tag tag^"</h4>"^nl
  | H5 tag -> indent^"<h5>"^build_tag tag^"</h5>"^nl
  | H6 tag -> indent^"<h6>"^build_tag tag^"</h6>"^nl
  | OL tags -> indent^"<ol>"^build_list_items tags^"</ol>"^nl  
  | UL tags -> indent^"<ul>"^build_list_items tags^"</ul>"^nl

and build_list_items = function 
  | LI' -> ""
  | LI {c= tag; next= t} -> indent^"<li>"^build_tag tag^"\n"^build_list_items t^"</li>"^nl;;

(*
let rec build_list = function 
  | OL tags -> "<ol>"^build_list_items tags^"</ol>"  
  | UL tags -> "<ul>"^build_list_items tags^"</ul>"*)

let rec build_head_tags = function 
| Title s -> indent^"<title>"^s^"<\title>"^nl
| Meta s -> indent^"<meta charset="^s^">"^nl;;

let rec build_head = function
  | H tag -> build_head_tags tag
  | H' {c=tag; next=next} -> (build_head tag)^build_head next;; 

let rec build_body = function
  | B tag -> build_tag tag 
  | B' {c=tag; next=next} -> (build_body tag)^build_body next;;

let build (head, body) = 
  "<head>"^build_head head^"</head>"^nl^"<body>"^build_body body^"</body>"^nl;;

build (H'{c=H(Title("Hello World")); next=H(Meta("UTF-8"))}, 
      B'{c=B(H2(TEXT"Neural Networks")); next=B'{c=B(TEXT"A"); next=B'{c=A("https://en.wikipedia.org/wiki/Neural_network", TEXT"A neural network"); next=B(TEXT"is a network or circuit of biological neurons, or,
    in a modern sense, an artificial neural network, composed of artificial neurons or nodes")}}})