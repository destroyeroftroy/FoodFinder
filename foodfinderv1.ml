(*this type replaces my original idea for boolean values
that indicate whether a restaurant can handle take out or eat in
this is an improvement on that system because it allows one
to more efficiently look at the data
in a future update I will likely just move these to the tag
section, as tags are just descriptors*)

type dining = EatIn | TakeOut | Both

(*foodplace is the type for data about restaurants
first string is the name
second string is the location 
(eg: stadium village, west bank, dinkytown, coffman)
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

(*this function takes a list and an element
then goes through that list
and returns true if the element is in the list
it is called tagcheckele because it helps check if an element
is within some list of tags*)

let rec tagcheckele (h : string) (rtags : string list) : bool=
	match rtags with
	|[] -> false
	|hd::tl -> if (h = hd)
		then true
		else tagcheckele h tl

(*this helper function takes two lists
goes through the first list
and calling tagcheckele on each item in the list
with the second list
it is called tagcheckor because it looks at two tag lists
and returns true if they share any elements
this is essentially two nested for loops*)

let rec tagcheckor (l1 : string list) (l2 : string list) : bool=
	match l1 with
	|[] -> true 
	|hd::tl -> (tagcheckele hd l2) || (tagcheckor tl l2)
(*this helper function takes two lists
goes through the first list and calls tagcheckele on each element
this is equivalent to tagcheckor but returns true only if all 
elements in each list are shared*)

let rec tagcheckand (l1 : string list) (l2 : string list) : bool=
	match l1 with
	|[] -> true 
	|hd::tl -> (tagcheckele hd l2) && (tagcheckand tl l2)

(*this function checks if a place has the ideal location*)

let idealLoc (place : foodplace) (params : param) : bool=
	let (_, ploc, _, _) = place
	in let (iloc, _, _) = params
	in (iloc = ploc)

(*this function checks if a place has the ideal dining type
eg: eat in or take out*)

let idealDining (place : foodplace) (params : param) : bool=
	let (_, _, pdining, _) = place
	in let (_, idining, _) = params
	in 
	match idining with
	|Both -> true
	|EatIn -> (pdining = Both || pdining = EatIn)
	|TakeOut -> (pdining = Both || pdining = TakeOut)

(*this function takes a foodplace and some parameters and returns
true if the place shares any tags with the parameters*)

let anyTags (place : foodplace) (params : param) : bool=
	let (_, _, _, ptags) = place
	in let (_, _, itags) = params
	in tagcheckor ptags itags

(*this function checks if a foodplace shares all tags with some
parameters*)

let allTags (place : foodplace) (params : param) : bool=
	let (_, _, _, ptags) = place
	in let (_, _, itags) = params
	in tagcheckand ptags itags	

(*this function takes a place and some parameters and returns true
if the place is acceptable given those parameters. a place is 
acceptable if it has the ideal location OR the ideal dining type
OR any matching tags*)

let isAcceptable (place : foodplace) (params : param) : bool=
	idealLoc place params ||
	idealDining place params ||
	anyTags place params

(*this function takes a place and some parameters and returns true
if the place is ideal given those parameters. a place is ideal if
it has the ideal location AND the ideal dining type AND at least
one matching tag*)

let isIdeal (place : foodplace) (params : param) : bool=
	idealLoc place params &&
	idealDining place params &&
	anyTags place params

(*this function takes a place and some parameters and returns true
if the place is perfect given those parameters. a place is perfect
if it has the ideal location AND the ideal dining type AND all tags
match*)

let isPerfect (place : foodplace) (params: param) : bool=
	isIdeal place params && allTags place params

(*this function takes a list of foodplaces and some parameters
and returns a list of all the acceptable foodplaces in that list*)

let rec acceptablePlaces (placelist : foodplace list) (params : param)
: foodplace list=
	match placelist with
	|[] -> []
	|hd::tl -> if isAcceptable hd params
		then hd :: acceptablePlaces tl params
		else acceptablePlaces tl params

(*this function takes a list of foodplaces and some parameters and
returns a list of all the ideal foodplaces in that list*)

let rec idealPlaces (placelist : foodplace list) (params : param)
: foodplace list=
	match placelist with
	|[] -> []
	|hd::tl -> if isIdeal hd params
		then hd :: idealPlaces tl params
		else idealPlaces tl params

(*this function takes a list of foodplaces and some parameters 
and returns a list of all the perfect foodplaces in that list*)

let rec perfectPlaces (placelist : foodplace list) (params : param)
: foodplace list=
	match placelist with
	|[] -> []
	|hd::tl -> if isPerfect hd params
		then hd :: perfectPlaces tl params
		else perfectPlaces tl params 

(*this function takes a foodplace and returns just its name*)

let justName (place : foodplace) : string=
	let (name, _, _, _) = place
	in name

(*this function takes a list of foodplaces and returns just their 
names*)

let rec justNames (placelist : foodplace list) : string list=
	match placelist with
	|[] -> []
	|hd::tl -> justName hd :: justNames tl

(*here are some foodplaces and a list of them
as well as some parameters
for testing!*)

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
