type logic = 
	OR of logic * logic
	| AND of logic * logic
	| IMPLIES of logic * logic
	| NOT of logic
	| LETTER of string
	| true
	| false;;
	
	
let rec removeImplies = function
	| IMPLIES(a, b) -> OR(NOT(removeImplies(a)), removeImplies(b))
	| LETTER(a) -> LETTER(a)
	| NOT(a) -> NOT(removeImplies(a))
	| AND(a, b) -> AND(removeImplies(a), removeImplies(b))
	| OR(a, b) -> OR(removeImplies(a), removeImplies(b));;
	

let rec pushNegsIn = function
	| NOT(NOT(a)) -> pushNegsIn(a)
	| NOT(AND(a, b)) -> OR(pushNegsIn(NOT(a)), pushNegsIn(NOT(b)))
	| NOT(OR(a, b)) -> AND(pushNegsIn(NOT(a)), pushNegsIn(NOT(b)))
	| OR(a, b) -> OR(pushNegsIn(a), pushNegsIn(b))
	| AND(a, b) -> AND(pushNegsIn(a), pushNegsIn(b))
	| LETTER(a) -> LETTER(a)
	| NOT(a) -> NOT(a);;

let distrib = function
	| OR(a, AND(b, c)) -> AND(OR(a, b), OR(a, c))
	| OR(AND(b, c), a) -> AND(OR(a, b), OR(a, c))
	| OR(a, b) -> OR(a, b)

let rec pushDisjIn = function
	| OR(a, AND(b, c)) -> AND(pushDisjIn(OR(a, b)), pushDisjIn(OR(a, c)))
	| OR(AND(b, c), a) -> AND(pushDisjIn(OR(a, b)), pushDisjIn(OR(a, c)))
	| OR(a, b) -> distrib(OR(pushDisjIn(a), pushDisjIn(b)))
	| AND(a, b) -> AND(pushDisjIn(a), pushDisjIn(b))
	| LETTER(a) -> LETTER(a)
	| NOT(a) -> NOT(a);;


let toCNF = function
	| a -> let b = removeImplies(a) in
					let c = pushNegsIn(b) in
					let d = pushDisjIn(c) in
						d;;
						
let rec toStr = function
	| OR(a, b) -> String.concat "" ["(OR "; toStr(a); " "; toStr(b); ")"]
	| AND(a, b) -> String.concat "" ["(AND "; toStr(a); " "; toStr(b); ")"]
	| NOT(LETTER(a)) -> String.concat "" ["(NOT "; a; ")"]
	| LETTER(a) -> a;;
	
let () = print_string (toStr(toCNF( OR(AND(LETTER("P"),LETTER("Q")),LETTER("R")) )))
let () = print_string "\n"
let () = print_string (toStr(toCNF( NOT(OR(AND(LETTER("P"),LETTER("Q")),LETTER("R"))) )))
let () = print_string "\n"
let () = print_string (toStr(toCNF( OR(LETTER("P"),OR(LETTER("Q"), AND(LETTER("R"), LETTER("V")))) )))
let () = print_string "\n"
let () = print_string (toStr(toCNF( OR(LETTER("P"),OR(LETTER("Q"), OR(LETTER("Z"), AND(LETTER("R"), LETTER("V")) ))) )))
let () = print_string "\n";;
	
	
	