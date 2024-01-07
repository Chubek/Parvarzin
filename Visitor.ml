type 'a genealogy_visitor = {
  visit_parvarzin : string -> 'a list -> 'a;
  visit_person_statement : string -> 'a attribute list -> 'a;
  visit_relationship_statement : string -> 'a relationship_value list -> 'a;
  visit_syntactic_comment : string -> 'a;
  visit_relationship_value :
    'a relationship_modifier option -> relationship_type -> string -> 'a;
  visit_attribute : string -> 'a attribute_value -> 'a;
}

and 'a attribute_value =
  | Identifier of string
  | String of string
  | Number of string
  | Date of int * int * int
  | Day of int
  | Month of int
  | Year of int

and relationship_modifier = Maternal | Paternal | Male | Female

and relationship_type =
  | Parent
  | Child
  | Spouse
  | Sibling
  | Grandparent
  | Grandchild
  | Aunt
  | Uncle
  | Cousin
  | Niece
  | Nephew
  | InLaw
  | HalfSibling
  | StepParent
  | StepChild

and 'a relationship_value =
  | RelationshipValue of
      relationship_modifier option * relationship_type * string

and 'a attribute = Attribute of string * 'a attribute_value

and 'a genealogy_dsl =
  | Parvarzin of string * 'a list
  | PersonStatement of string * 'a attribute list
  | RelationshipStatement of string * 'a relationship_value list
  | SyntacticComment of string

let rec accept_genealogy_visitor visitor ast =
  match ast with
  | Parvarzin (id, statements) ->
      visitor.visit_parvarzin id
        (List.map (accept_genealogy_visitor visitor) statements)
  | PersonStatement (id, attributes) ->
      visitor.visit_person_statement id
        (List.map (accept_genealogy_visitor_attribute visitor) attributes)
  | RelationshipStatement (id, relationships) ->
      visitor.visit_relationship_statement id
        (List.map
           (accept_genealogy_visitor_relationship_value visitor)
           relationships)
  | SyntacticComment comment -> visitor.visit_syntactic_comment comment

and accept_genealogy_visitor_attribute visitor attribute =
  match attribute with
  | Attribute (name, value) ->
      visitor.visit_attribute name
        (accept_genealogy_visitor_attribute_value visitor value)

and accept_genealogy_visitor_attribute_value visitor attribute_value =
  match attribute_value with
  | Identifier id -> visitor.visit_attribute_value_identifier id
  | String s -> visitor.visit_attribute_value_string s
  | Number n -> visitor.visit_attribute_value_number n
  | Date (year, month, day) -> visitor.visit_attribute_value_date year month day
  | Day d -> visitor.visit_attribute_value_day d
  | Month m -> visitor.visit_attribute_value_month m
  | Year y -> visitor.visit_attribute_value_year y

and accept_genealogy_visitor_relationship_value visitor relationship_value =
  match relationship_value with
  | RelationshipValue (modifier, relType, id) ->
      visitor.visit_relationship_value modifier relType id
