let indent = "  ";;
let nl = "\n";

type head_tag = None | Title of string * head_tag | Meta of string * head_tag;;

type body_tag = 
END 
| TEXT of string * body_tag
| IMG of string * body_tag
| A of string * body_tag * body_tag  
| I of body_tag * body_tag  
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

let rec build_tag ind = function
  | END -> "" 
  | TEXT (s, next) -> s ^ build_tag ind next
  | IMG (src, next) -> nl^ind^"<img src="^src^" alt=\"laptop image\">" ^ build_tag ind next
  | A (link, tag, next) -> "<a href="^link^">"^build_tag (ind ^ indent) tag^"</a>" ^ build_tag ind next
  | I (tag, next) -> "<i>"^build_tag (ind ^ indent) tag^"</i>" ^ build_tag ind next
  | BR next -> nl^ind^"<\br>" ^ build_tag ind next
  | DIV (tag, next) -> nl^ind^"<div>"^build_tag (ind ^ indent) tag^nl^ind^"</div>" ^ build_tag ind next
  | P (tag, next) -> nl^ind^"<p>"^build_tag (ind ^ indent) tag^"</p>" ^ build_tag ind next
  | H1 (text, next) -> nl^ind^"<h1>"^text^"</h1>" ^ build_tag ind next
  | H2 (text, next) -> nl^ind^"<h2>"^text^"</h2>" ^ build_tag ind next
  | H3 (text, next) -> nl^ind^"<h3>"^text^"</h3>" ^ build_tag ind next
  | H4 (text, next) -> nl^ind^"<h4>"^text^"</h4>" ^ build_tag ind next
  | H5 (text, next) -> nl^ind^"<h5>"^text^"</h5>" ^ build_tag ind next
  | H6 (text, next) -> nl^ind^"<h6>"^text^"</h6>" ^ build_tag ind next
  | OL (tags, next) -> nl^ind^"<ol>"^build_list_items (ind ^ indent) tags^nl^ind^"</ol>" ^ build_tag ind next
  | UL (tags, next) -> nl^ind^"<ul>"^build_list_items (ind ^ indent) tags^nl^ind^"</ul>" ^ build_tag ind next
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
                        I(TEXT("strengthen", END), TEXT(".", END))), END
                      ), 
                    H4(
                      "The most important applications",  
                    OL(
                      LI(TEXT("Computer Vision", END), 
                      LI(TEXT("Speech Recognition", END), 
                      LI'(TEXT("Self-driving cars", END)))), 
                    DIV(IMG("some_image.png", END), 
                    END))
                    ))))))))
