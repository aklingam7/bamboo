open Syntax
open Tree
open Dictionary

exception ValidationError of string

let rec as_constituent node category =
  match node with
  | Leaf { pair; _ } ->
      {
        category;
        children =
          L
            {
              category = get_lexeme_category pair;
              child = get_lexeme_word pair;
            };
      }
  | NodeOne { t; category = _; _ } -> (
      match t with
      | Leaf { pair; _ } ->
          {
            category;
            children =
              L
                {
                  category = get_lexeme_category pair;
                  child = get_lexeme_word pair;
                };
          }
      | NodeOne { category = child_cat; _ } | NodeTwo { category = child_cat; _ } ->
          let c = as_constituent t child_cat in
          { category; children = C c })
  | NodeTwo { t1; t2; category = _; _ } -> (
      match (t1, t2) with
      | Leaf { pair; _ }, NodeOne { category = cat2; _ }
      | Leaf { pair; _ }, NodeTwo { category = cat2; _ } ->
          let c2 = as_constituent t2 cat2 in
          {
            category;
            children =
              LC
                ( {
                    category = get_lexeme_category pair;
                    child = get_lexeme_word pair;
                  },
                  c2 );
          }
      | NodeOne { category = cat1; _ }, Leaf { pair; _ }
      | NodeTwo { category = cat1; _ }, Leaf { pair; _ } ->
          let c1 = as_constituent t1 cat1 in
          {
            category;
            children =
              CL
                ( c1,
                  {
                    category = get_lexeme_category pair;
                    child = get_lexeme_word pair;
                  } );
          }
      | NodeOne { category = cat1; _ }, NodeOne { category = cat2; _ }
      | NodeOne { category = cat1; _ }, NodeTwo { category = cat2; _ }
      | NodeTwo { category = cat1; _ }, NodeOne { category = cat2; _ }
      | NodeTwo { category = cat1; _ }, NodeTwo { category = cat2; _ } ->
          let c1 = as_constituent t1 cat1 in
          let c2 = as_constituent t2 cat2 in
          { category; children = CC (c1, c2) }
      | Leaf _, Leaf _ -> failwith "RI violation: NodeTwo cannot have two Leafs")

let validate_lexeme category word syntax dict =
  check_lexeme syntax { category; child = word } && mem (create_lexeme category word) dict

let validate_constituent node category syntax =
  check_constituent syntax (as_constituent node category)