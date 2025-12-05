open Dictionary

exception NonexistentNodeID of int
exception NonTopLevelNodeID of int

(**Abstraction function: [Leaf {pair = (a,b); id = x}] represents a node
   containing a lexeme with lexical category [a] and word [b], with an
   associated id number [x]. [NodeOne {t = tr; category = n; id = x}] represents
   a recursive tree containing a constituent node of category [n], which
   branches down into subtree [tr]. The tree has an associated id number [x].
   [NodeTwo {t1 = tr1; t2 = tr2; category = n; id = x}] represents a recursive
   tree containing a constituent branching node of category [n], which branches
   down into subtrees [tr1] and [tr2]. The tree has an associated id number [x].

   The [tree] type represents a workspace containing a list of top-level nodes
   [nodes] and the next available ID [next_id].

   Representation Invariants:
   - [id] >= 0 for all nodes
   - [next_id] > maximum ID in [nodes]
   - Both [t1] and [t2] in [NodeTwo] cannot have lexical categories *)
type tree_node =
  | Leaf of {
      pair : lexeme;
      id : int;
    }
  | NodeOne of {
      t : tree_node;
      category : string;
      id : int;
    }
  | NodeTwo of {
      t1 : tree_node;
      t2 : tree_node;
      category : string;
      id : int;
    }

type tree = {
  nodes : tree_node list;
  next_id : int;
}

let empty = { nodes = []; next_id = 1 }

let add t lexeme =
  if t.nodes = [] then
    let id = 1 in
    let node = Leaf { pair = lexeme; id } in
    ({ nodes = t.nodes @ [ node ]; next_id = id + 1 }, id)
  else
    let id = t.next_id in
    let node = Leaf { pair = lexeme; id } in
    ({ nodes = t.nodes @ [ node ]; next_id = id + 1 }, id)

let rec get t id =
  let rec find_in_node node target_id =
    match node with
    | Leaf { id = nid; _ } when nid = target_id -> Some node
    | Leaf _ -> None
    | NodeOne { t; id = nid; _ } when nid = target_id -> Some node
    | NodeOne { t; _ } -> find_in_node t target_id
    | NodeTwo { t1; t2; id = nid; _ } when nid = target_id -> Some node
    | NodeTwo { t1; t2; _ } -> (
        match find_in_node t1 target_id with
        | Some n -> Some n
        | None -> find_in_node t2 target_id)
  in
  match List.find_map (fun node -> find_in_node node id) t.nodes with
  | Some n -> n
  | None -> raise (NonexistentNodeID id)

let remove t removal_id =
  let rec claimed_ids node =
    match node with
    | NodeOne { t; id; _ } -> id :: claimed_ids t
    | NodeTwo { t1; t2; id; _ } -> id :: (claimed_ids t1 @ claimed_ids t2)
    | Leaf { id; _ } -> [ id ]
  in
  let start_claim node =
    match node with
    | NodeOne { t; _ } -> claimed_ids t
    | NodeTwo { t1; t2; _ } -> claimed_ids t1 @ claimed_ids t2
    | Leaf _ -> []
  in
  let node_id node =
    match node with
    | NodeOne { id; _ } -> id
    | NodeTwo { id; _ } -> id
    | Leaf { id; _ } -> id
  in
  let rec remove_from_list prev lst =
    match lst with
    | [] -> raise (NonTopLevelNodeID removal_id)
    | h :: rest ->
        if node_id h = removal_id then
          let children =
            match h with
            | NodeOne { t; _ } -> [ t ]
            | NodeTwo { t1; t2; _ } -> [ t1; t2 ]
            | Leaf _ -> []
          in
          let disallowed_ids = List.flatten (List.map start_claim children) in
          List.filter
            (fun node -> not (List.mem (node_id node) disallowed_ids))
            (List.rev prev @ children @ rest)
        else remove_from_list (h :: prev) rest
  in
  { t with nodes = remove_from_list [] t.nodes }

let connect2 t n1 n2 category =
  let new_id = t.next_id in
  let new_node = NodeTwo { t1 = n1; t2 = n2; category; id = new_id } in
  let node_id node =
    match node with
    | NodeOne { id; _ } -> id
    | NodeTwo { id; _ } -> id
    | Leaf { id; _ } -> id
  in
  let id1 = node_id n1 in
  let id2 = node_id n2 in
  let nodes' =
    List.map (fun n -> if node_id n = id1 then new_node else n) t.nodes
  in
  let nodes' = List.filter (fun n -> node_id n <> id2) nodes' in
  ({ nodes = nodes'; next_id = new_id + 1 }, new_id)

let connect1 t node category =
  let new_id = t.next_id in
  let new_node = NodeOne { t = node; category; id = new_id } in
  let node_id n =
    match n with
    | NodeOne { id; _ } -> id
    | NodeTwo { id; _ } -> id
    | Leaf { id; _ } -> id
  in
  let id = node_id node in
  let nodes' =
    List.map (fun n -> if node_id n = id then new_node else n) t.nodes
  in
  ({ nodes = nodes'; next_id = new_id + 1 }, new_id)

let to_list t = t.nodes
