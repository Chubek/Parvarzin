type genealogyDSL =
  | Parvarzin of string * statement list
and statement =
  | PersonStatement of string * attribute list
  | RelationshipStatement of string * relationshipValue list
  | SyntacticComment of string
and relationshipValue =
  | RelationshipValue of relationshipModifier option * relationshipType * string
and relationshipModifier =
  | Maternal | Paternal | Male | Female
and relationshipType =
  | Parent | Child | Spouse | Sibling | Grandparent | Grandchild
  | Aunt | Uncle | Cousin | Niece | Nephew | InLaw | HalfSibling
  | StepParent | StepChild
and attribute =
  | Attribute of string * attributeValue
and attributeValue =
  | Identifier of string
  | String of string
  | Number of string
  | Date of int * int * int
  | Day of int
  | Month of int
  | Year of int

(* Lexer *)
let rec lex_ident acc input =
  match input with
  | c :: rest when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || (c >= '0' && c <= '9') ->
      lex_ident (acc ^ String.make 1 c) rest
  | _ -> acc, input

let rec lex_string acc input =
  match input with
  | '"' :: rest -> acc ^ "\"", rest
  | c :: rest -> lex_string (acc ^ String.make 1 c) rest
  | _ -> acc, input

let rec lex_number acc input =
  match input with
  | c :: rest when c >= '0' && c <= '9' -> lex_number (acc ^ String.make 1 c) rest
  | _ -> acc, input

let rec lex_year acc input =
  match input with
  | c1 :: c2 :: c3 :: c4 :: rest when c1 >= '0' && c1 <= '9' && c2 >= '0' && c2 <= '9' && c3 >= '0' && c3 <= '9' && c4 >= '0' && c4 <= '9' ->
      lex_year (acc ^ String.make 1 c1 ^ String.make 1 c2 ^ String.make 1 c3 ^ String.make 1 c4) rest
  | _ -> acc, input

let rec lexer input =
  match input with
  | '\n' :: rest -> lexer rest
  | ' ' :: rest -> lexer rest
  | '\t' :: rest -> lexer rest
  | ";;" :: rest -> SyntacticComment(""), rest
  | "Parvarzin" :: rest -> lex_ident "" rest |> fun (id, rest') -> Parvarzin(id, lexer rest')
  | "Person" :: rest -> lex_ident "" rest |> fun (id, rest') -> PersonStatement(id, lex_attribute_list rest')
  | "Relationship" :: rest -> lex_ident "" rest |> fun (id, rest') -> RelationshipStatement(id, lex_relationship_list rest')
  | _ -> failwith "Lexer error"
and lex_attribute_list input =
  match input with
  | '|' :: rest -> lex_attribute_list rest
  | "identifier" :: rest -> lex_ident "" rest |> fun (id, rest') -> Attribute(id, Identifier(id))
  | "string" :: rest -> lex_string "" rest |> fun (s, rest') -> Attribute(s, String(s))
  | "number" :: rest -> lex_number "" rest |> fun (n, rest') -> Attribute(n, Number(n))
  | "date" :: rest -> lex_date "" rest |> fun (date, rest') -> Attribute(date, date)
  | _ -> []
and lex_relationship_list input =
  match input with
  | '|' :: rest -> lex_relationship_list rest
  | "[" :: rest -> lex_relationship_modifier "" rest |> fun (modifier, rest') ->
      lex_relationship_type rest' |> fun (relType, rest'') ->
      match rest'' with
      | "of" :: rest''' -> lex_ident "" rest''' |> fun (id, rest'''') ->
          RelationshipValue(modifier, relType, id) :: lex_relationship_list rest''''
      | _ -> failwith "Lexer error"
  | _ -> []
and lex_relationship_modifier acc input =
  match input with
  | "maternal" :: rest -> Some Maternal, rest
  | "paternal" :: rest -> Some Paternal, rest
  | "male" :: rest -> Some Male, rest
  | "female" :: rest -> Some Female, rest
  | _ -> None, input
and lex_relationship_type input =
  match input with
  | "parent" :: rest -> Parent, rest
  | "child" :: rest -> Child, rest
  | "spouse" :: rest -> Spouse, rest
  | "sibling" :: rest -> Sibling, rest
  | "grandparent" :: rest -> Grandparent, rest
  | "grandchild" :: rest -> Grandchild, rest
  | "aunt" :: rest -> Aunt, rest
  | "uncle" :: rest -> Uncle, rest
  | "cousin" :: rest -> Cousin, rest
  | "niece" :: rest -> Niece, rest
  | "nephew" :: rest -> Nephew, rest
  | "inLaw" :: rest -> InLaw, rest
  | "halfSibling" :: rest -> HalfSibling, rest
  | "stepParent" :: rest -> StepParent, rest
  | "stepChild" :: rest -> StepChild, rest
  | _ -> failwith "Lexer error"
and lex_date acc input =
  mach input with
  | digit1 :: digit2 :: '-' :: rest when digit1 >= '0' && digit1 <= '9' && digit2 >= '0' && digit2 <= '9' -> lex_date (acc ^ String.make 1 digit1 ^ String.make 1 digit2) rest
  | digit1 :: '-' :: digit2 :: rest when digit1 >= '0' && digit1 <= '9' && digit2 >= '0' && digit2 <= '9' -> lex_date (acc ^ String.make 1 digit1 ^ String.make 1 digit2) rest
  | digit1 :: digit2 :: digit3 :: digit4 :: '-' :: rest when digit1 >= '0' && digit1 <= '9' && digit2 >= '0' && digit2 <= '9' && digit3 >= '0' && digit3 <= '9' && digit4 >= '0' && digit4 <= '9' ->
      lex_date (acc ^ String.make 1 digit1 ^ String.make 1 digit2 ^ String.make 1 digit3 ^ String.make 1 digit4) rest
  | digit1 :: digit2 :: '-' :: digit3 :: digit4 :: rest when digit1 >= '0' && digit1 <= '9' && digit2 >= '0' && digit2 <= '9' && digit3 >= '0' && digit3 <= '9' && digit4 >= '0' && digit4 <= '9' ->
      lex_date (acc ^ String.make 1 digit1 ^ String.make 1 digit2 ^ String.make 1 digit3 ^ String.make 1 digit4) rest
  | digit1 :: '-' :: digit2 :: digit3 :: digit4 :: rest when digit1 >= '0' && digit1 <= '9' && digit2 >= '0' && digit2 <= '9' && digit3 >= '0' && digit3 <= '9' && digit4 >= '0' && digit4 <= '9' ->
      lex_date (acc ^ String.make 1 digit1 ^ String.make 1 digit2 ^ String.make 1 digit3 ^ String.make 1 digit4) rest
  | _ -> acc, input

(* Parser *)
let rec parse_genealogyDSL input =
  match input with
  | "Parvarzin" :: rest -> parse_genealogyDSL_rest rest
  | _ -> failwith "Parser error"
and parse_genealogyDSL_rest input =
  match input with
  | ident :: "{" :: rest -> lex_ident "" rest |> fun (id, rest') -> let statements, rest'' = parse_statements rest' in Parvarzin(id, statements), rest''
  | _ -> failwith "Parser error"
and parse_statements input =
  match input with
  | "}" :: rest -> [], rest
  | _ -> let statement, rest' = parse_statement input in
         let statements, rest'' = parse_statements rest' in
         statement :: statements, rest''
and parse_statement input =
  match input with
  | "Person" :: rest -> lex_ident "" rest |> fun (id, rest') -> let attributes, rest'' = parse_attribute_list rest' in PersonStatement(id, attributes), rest''
  | "Relationship" :: rest -> lex_ident "" rest |> fun (id, rest') -> let relationships, rest'' = parse_relationship_list rest' in RelationshipStatement(id, relationships), rest''
  | ";;" :: rest -> lex_string "" rest |> fun (comment, rest') -> SyntacticComment(comment), rest'
  | _ -> failwith "Parser error"
and parse_relationship_list input =
  match input with
  | "=" :: rest -> parse_relationship_value_list rest
  | _ -> [], input
and parse_relationship_value_list input =
  match input with
  | "[" :: rest -> parse_relationship_value rest |> fun (value, rest') -> let values, rest'' = parse_relationship_value_list rest'' in value :: values, rest''
  | _ -> [], input
and parse_relationship_value input =
  match input with
  | "]" :: rest -> None, Parent, "", rest
  | ident :: "of" :: rest -> lex_ident "" rest |> fun (id, rest') ->
      parse_relationship_type rest' |> fun (relType, rest'') ->
      parse_relationship_modifier rest'' |> fun (modifier, rest''') ->
      modifier, relType, id, rest'''
  | _ -> failwith "Parser error"
and parse_relationship_modifier input =
  match input with
  | "maternal" :: rest -> Some Maternal, rest
  | "paternal" :: rest -> Some Paternal, rest
  | "male" :: rest -> Some Male, rest
  | "female" :: rest -> Some Female, rest
  | _ -> None, input
and parse_relationship_type input =
  match input with
  | "parent" :: rest -> Parent, rest
  | "child" :: rest -> Child, rest
  | "spouse" :: rest -> Spouse, rest
  | "sibling" :: rest -> Sibling, rest
  | "grandparent" :: rest -> Grandparent, rest
  | "grandchild" :: rest -> Grandchild, rest
  | "aunt" :: rest -> Aunt, rest
  | "uncle" :: rest -> Uncle, rest
  | "cousin" :: rest -> Cousin, rest
  | "niece" :: rest -> Niece, rest
  | "nephew" :: rest -> Nephew, rest
  | "inLaw" :: rest -> InLaw, rest
  | "halfSibling" :: rest -> HalfSibling, rest
  | "stepParent" :: rest -> StepParent, rest
  | "stepChild" :: rest -> StepChild, rest
  | _ -> failwith "Parser error"
and parse_attribute_list input =
  match input with
  | "|" :: rest -> parse_attribute_list rest
  | "identifier" :: rest -> lex_ident "" rest |> fun (id, rest') -> let attr, rest'' = parse_attribute rest' in Attribute(id, attr), rest''
  | _ -> [], input
and parse_attribute input =
  match input with
  | "=" :: rest -> parse_attribute_value rest
  | _ -> failwith "Parser error"
and parse_attribute_value input =
  match input with
  | "identifier" :: rest -> lex_ident "" rest |> fun (id, rest') -> Identifier(id), rest'
  | "string" :: rest -> lex_string "" rest |> fun (s, rest') -> String(s), rest'
  | "number" :: rest -> lex_number "" rest |> fun (n, rest') -> Number(n), rest'
  | "date" :: rest -> lex_date "" rest |> fun (date, rest') -> Date(date), rest'
  | "day" :: rest -> lex_number "" rest |> fun (day, rest') -> Day(int_of_string day), rest'
  | "month" :: rest -> lex_number "" rest |> fun (month, rest') -> Month(int_of_string month), rest'
 
