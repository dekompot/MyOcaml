let indent = "    ";;
let nl = "\n";

type head_tag = None | Title of string * head_tag | Meta of string * head_tag;;

type body_tag = 
END 
| TEXT of string * body_tag
| I of string * body_tag
| IMG of string * body_tag
| A of string * body_tag * body_tag  
| BR of body_tag 
| P of body_tag * body_tag 
| DIV of body_tag * body_tag 
| H1 of string * body_tag  
| H2 of string * body_tag  
| H3 of string * body_tag  
| H4 of string * body_tag 
| H5 of string * body_tag  
| H6 of string * body_tag 
| OL of list_element * body_tag  
| UL of list_element * body_tag  
and 
list_element = LI' of body_tag | LI of body_tag * list_element;;

let get_tag = function
| END -> ""
| TEXT(_, _) -> ""
| I(_, _) -> "i"
| IMG(_, _) -> "img"
| A(_, _, _) -> "a"
| BR _ -> "br"
| P(_, _) -> "p"
| DIV(_, _) -> "div"
| H1 (_, _) -> "h1"
| H2 (_, _) -> "h2"
| H3 (_, _) -> "h3"
| H4 (_, _) -> "h4"
| H5 (_, _) -> "h5"
| H6 (_, _) -> "h6"
| OL(_, _) -> "ol"
| UL(_, _) -> "ul";;

let format (bt, open_ind, text, close_ind) = 
  let str_tag = get_tag bt 
  in open_ind^"<"^str_tag^">"^text^close_ind^"</"^str_tag^">";;

let rec build_tag ind bt = 
  match bt with
  | END -> "" 
  | TEXT (s, next) -> s ^ build_tag ind next
  | IMG (src, next) -> nl^ind^"<img src="^src^" alt=\"laptop image\">" ^ build_tag ind next
  | A (link, tag, next) -> nl^ind^"<a href="^link^">"^build_tag (ind ^ indent) tag^"</a>" ^ build_tag ind next
  | I (text, next) -> format(bt, "", text, "") ^ build_tag ind next
  | BR next -> nl^ind^"<\br>" ^ build_tag ind next
  | DIV (tag, next) -> format(bt, nl^ind, build_tag (ind ^ indent) tag, nl^ind) ^ build_tag ind next
  | P (tag, next) -> format(bt, nl^ind, build_tag (ind ^ indent) tag, "") ^ build_tag ind next
  | H1 (text, next) | H2 (text, next) | H3 (text, next) | H4 (text, next) | H5 (text, next) | H6 (text, next) -> format(bt, nl^ind, text, "") ^ build_tag ind next
  | OL (tags, next) | UL (tags, next) -> format(bt, nl^ind, build_list_items (ind ^ indent) tags, nl^ind) ^ build_tag ind next
and build_list_items ind = function 
  | LI' tag -> nl^ind^"<li>"^build_tag ind tag^"</li>"
  | LI (tag, next) -> nl^ind^"<li>"^build_tag ind tag^"</li>"^build_list_items ind next
  
let rec build_head_tags ind = function 
| Title (s, next) -> nl^ind^"<title>"^s^"</title>" ^ build_head_tags ind next
| Meta (s, next) -> nl^ind^"<meta charset="^s^"/>" ^ build_head_tags ind next
| None -> "";;

let double_indent = indent ^ indent;;

let build (head, body) = 
  "<html>"^nl^indent^"<head>"^build_head_tags double_indent head^nl^indent^"</head>"^nl^indent^"<body>"^build_tag double_indent body^nl^indent^"</body>"^nl^"</html>";;
  
print_string (build (Meta("UTF-8", Title("Hello World", None)), 
                    A("https://en.wikipedia.org/wiki/Neural_network", H2("Neural Networks", END),
                    P(TEXT("A neural network is a network or circuit of biological neurons, or, in a modern sense, an artificial neural network, composed of artificial neurons or nodes", END), 
                    H5("List of content", 
                    UL(
                      LI(TEXT("History", END), 
                      LI(TEXT("Artificial Intelligence", END), 
                      LI'(TEXT("Applications", END)))), 
                    DIV(
                      P(
                        TEXT("Neural networks architecture is inspired by brain structure. When activities are repeated, the connections between those neurons are ", 
                        I("strengthen", TEXT(".", END))), END
                      ), 
                    H4(
                      "The most important applications",  
                    OL(
                      LI(TEXT("Computer Vision", END), 
                      LI(TEXT("Speech Recognition", END), 
                      LI'(TEXT("Self-driving cars", END)))), 
                    DIV(IMG("some_image.png", END), 
                    END))
                    ))))))));;
                    
print_string("\n\n");;

print_string (build(Meta("UTF-8", Title("Hello World", None)), 
                    OL(
                      LI(TEXT("First Element", END),
                      LI(
                        UL(
                          LI(TEXT("Coffee", END), LI'(TEXT("Tea", END))), END
                          ), 
                      LI'(TEXT("Third Element", END))
                        )), END)));;
