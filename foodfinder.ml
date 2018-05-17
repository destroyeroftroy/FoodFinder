type dining = EatIn | TakeOut | Both

(*foodplace is the type for data about restaurants
first string is the name
second string is the location 
(stadium village, west bank, dinkytown, coffman)
type dining is whether it is eat in, take out, or both
string list is the tags associated with that restaurant
*)

type foodplace = string * string * dining * string list

(*param is the type for the parameters data
the string is the desired location
the first bool is desired sit down status
the second bool is desired take out status
the string list is any desired tags for food*)

type param = string * dining * string list

(*this function takes a list and an item
then goes through a list
and returns true if the item is in the list
it is called tagcheckele because it helps check if an element
is within some list of tags*)

let rec tagcheckele (h : string) (rtags : string list) : bool=
	match rtags with
	|[] -> false
	|hd::tl -> if (h = hd)
		then true
		else tagcheckele h tl

(*this helper function takes two lists
iterates through the first list
and calling tagcheckele on each item in the list
with the second list
it is called tagcheckor because it looks at two tag lists
and returns true if they share any elements*)

let rec tagcheckor (l1 : string list) (l2 : string list) : bool=
	match l1 with
	|[] -> true 
	|hd::tl -> (tagcheckele hd l2) || (tagchecklst tl l2)

let rec tagcheckand (l1 : string list) (l2 : string list) : bool=
	match l1 with
	|[] -> true 
	|hd::tl -> (tagcheckele hd l2) && (tagchecklst tl l2)

let idealLoc (place : foodplace) (params : param) : bool=
	let (pname, ploc, pdining, ptags) = place
	in let (iloc, idining, itags) = params
	in (iloc = ploc)

let idealDining (place : foodplace) (params : param) : bool=
	let (pname, ploc, pdining, ptags) = place
	in let (iloc, idining, itags) = params
	in 
	match idining with
	|Both -> true
	|EatIn -> (pdining = Both || pdining = EatIn)
	|TakeOut -> (pdining = Both || pdining = TakeOut)

let anyTags (place : foodplace) (params : param) : bool=
	let (pname, ploc, pdining, ptags) = place
	in let (iloc, idining, itags) = params
	in tagcheckor ptags itags

let allTags (place : foodplace) (params : param) : bool=
	let (pname, ploc, pdining, ptags) = place
	in let (iloc, idining, itags) = params
	in tagcheckand ptags itags	

let isAcceptable (place : foodplace) (params : param) : bool=
	idealLoc place params ||
	idealDining place params ||
	anyTags place params

let isIdeal (place : foodplace) (params : param) : bool=
	idealLoc place params &&
	idealDining place params &&
	anyTags place params

let isPerfect (place : foodplace) (params: param) : bool=
	isIdeal place params && allTags place params

let rec acceptablePlaces (placelist : foodplace list) (params : param)
: foodplace list=
	match placelist with
	|[] -> []
	|hd::tl -> if isAcceptable hd params
		then hd :: acceptablePlaces tl params
		else acceptablePlaces tl params

let rec idealPlaces (placelist : foodplace list) (params : param)
: foodplace list=
	match placelist with
	|[] -> []
	|hd::tl -> if isIdeal hd params
		then hd :: idealPlaces tl params
		else idealPlaces tl params

let rec perfectPlaces (placelist : foodplace list) (params : param)
: foodplace list=
	match placelist with
	|[] -> []
	|hd::tl -> if isPerfect hd params
		then hd :: perfectPlaces tl params
		else perfectPlaces tl params 

let justName (place : foodplace) : string=
	let (name, loc, dining, tags) = place
	in name

let rec justNames (placelist : foodplace list) : string list=
	match placelist with
	|[] -> []
	|hd::tl -> justName hd :: justNames tl

let noodles : foodplace = ("Noodles and Company", "stadium village", Both, ["noodles"; "asian"; "american"])

let canes1 : foodplace = ("Raising Canes", "stadium village", Both, ["chicken"])

let canes2 : foodplace = ("Raising Canes", "dinkytown", Both, ["chicken"])

let sotarol : foodplace = ("Sotarol", "stadium village", Both, ["asian"; "ramen"; "chinese"; "japanese"; "korean"])

let dpdough : foodplace = ("DP Dough", "dinkytown", Both, ["italian"; "calzone"])

let foodlst : foodplace list = [noodles; canes1; canes2; sotarol; dpdough]

let params : param = ("stadium village", TakeOut, ["asian"])

(* feedback
take out vs eat in tags
_ instead of named variables for unused things
databases hmmmm
a lot of the functions are pre-existing
automatic in databases - tag to restaurant lookup indexes - sqlite 
*)
