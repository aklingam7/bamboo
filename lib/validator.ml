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
          let c = as_constituent t category in
          {
            category;
            children = L { category = c.category; child = get_lexeme_word pair };
          }
      | _ ->
          let c = as_constituent t category in
          { category; children = C c })
  | NodeTwo { t1; t2; category = _; _ } -> (
      let c1 = as_constituent t1 category in
      let c2 = as_constituent t2 category in
      match (t1, t2) with
      | Leaf { pair; _ }, _ ->
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
      | _, Leaf { pair; _ } ->
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
      | _ -> { category; children = CC (c1, c2) })