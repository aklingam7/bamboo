open Bamboo
open Bamboo.Validator

let clear_screen () =
  let () = print_string "\027[2J" in
  flush stdout

let print_commands () =
  let () = print_endline "Commands:" in
  let () = print_endline "\tquit -> Exits the program" in
  let () =
    print_endline
      "\tsyntax -> Displays the full syntax, including lexical categories, \
       constituent categories, and constituent formation rules"
  in
  let () =
    print_endline
      "\tadd <lexical-category> <word> -> Adds a node containing the described \
       lexeme to the syntax tree"
  in
  let () =
    print_endline
      "\tconnect <constituent-category> <node> -> Adds a constituent node \
       parenting the given node to the syntax tree"
  in
  let () =
    print_endline
      "\tconnect <constituent-category> <node1> <node2> -> Adds a constituent \
       node parenting the two given nodes in order to the syntax tree"
  in
  let () =
    print_endline
      "\tremove <node> -> Removes the given node and all its connections from \
       the tree; can only remove top-level nodes (i.e., nodes with no parents)"
  in
  let () =
    print_endline
      "\texpand <lexical-category> <word> -> Expands the current dictionary \
       with a word containing the described lexeme. Note: this also modifies \
       the dictionary file passed in"
  in
  let () =
    print_endline
      "\treview -> Provides the user with a description\n\
      \  of how syntax trees work"
  in
  ()

let print_review () =
  let ex_tree =
    let t, id1 = Tree.add Tree.empty (Dictionary.create_lexeme "N" "boy") in
    let t, id2 = Tree.add t (Dictionary.create_lexeme "V" "walks") in
    let node1 = Tree.get t id1 in
    let t, id3 = Tree.connect1 t node1 "NP" in
    let t, id4 = Tree.add t (Dictionary.create_lexeme "D" "the") in
    let node4 = Tree.get t id4 in
    let node3 = Tree.get t id3 in
    let t, id5 = Tree.connect2 t node4 node3 "DP" in
    let node2 = Tree.get t id2 in
    let t, id6 = Tree.connect1 t node2 "VP" in
    let node5 = Tree.get t id5 in
    let node6 = Tree.get t id6 in
    let t, id7 = Tree.connect2 t node5 node6 "TP" in
    Tree_printer.to_fancy_string t
  in

  let () =
    print_endline
      ("Syntax trees represent the ways to visualize sentence structures. \n\
       \  The top of a tree, the root, typically contains a TP, which \
        represents the sentence as a whole. The rule, TP -> DP VP, indicates \
        that a TP node branches into two nodes, a DP and VP. The visualization \
        is shown below,\n\
       \   TP\n\
       \  / \\\n\
       \ DP VP.\n\n\
        A DP represents a noun phrase with a determiner (i.e, \"the \", \" my \
        \", \" his \", \" one \", \" four\n\
       \      \" etc.), while a VP represents a verb phrase. For the sentence, \
        \" The boy\n\
       \      walks\", \n\
        \the simple structure is shown below,\n" ^ ex_tree
     ^ "\n\
       \ It is important to note that we can not connect two lexical nodes \
        together (i.e, N, V, D, P, etc.). For example, for a DP to be formed, \
        we must first form an NP first. We can not connect a D with an N as \
        those are both lexical nodes.\n\n\
        Right now, to connect nodes, use the rightmost id number as that \
        represents the id number of the topmost node of the subtree.\n\n\
        Here are the lexical node representations:\n\
        N-Noun\n\
        V-Verb\n\
        D-Determiner\n\
        P-Preposition (not originally given)\n\
        Adj-Adjective\n\n\
        Here are some general nodes that are not originally given:\n\
        PP-> P DP\n\
        VP->V PP\n\
        AdjP->Adj\n\
        NP-> AdjP N")
  in
  ()

let print_welcome () =
  let () = print_endline "╔════════════════════════════════════════╗" in
  let () = print_endline "║        Welcome to Bamboo!              ║" in
  let () = print_endline "╚════════════════════════════════════════╝" in
  let _ = print_review () in
  ()

let next_command_helper tree extra =
  let tree_str = Tree_printer.to_fancy_string tree in
  let () = if tree_str <> "" then print_endline ("\n\n" ^ tree_str ^ "\n") in
  let () = if extra <> "" then print_endline ("\n\n" ^ extra ^ "\n") in
  let () = print_commands () in
  let () = print_string "\n> " in
  flush stdout

let next_command_prompt tree review_opt extra =
  let () = clear_screen () in
  if review_opt = "review" then
    let _ = print_review () in
    next_command_helper tree extra
  else next_command_helper tree extra

let usage_msg =
  "Correct usage: dune exec bin/main.exe -s <syntax-file> [-d <dict-file>]"

let dict_file = ref ""
let syntax_file = ref ""
let anon_fun excess_argument = failwith "Excess argument"

let speclist =
  [
    ("-d", Arg.Set_string dict_file, "Set dict file name");
    ("-s", Arg.Set_string syntax_file, "Set syntax file name");
  ]

let () = Arg.parse speclist anon_fun usage_msg
let starting_info = ref ""

let dictionary =
  ref
    (if !dict_file <> "" then
       try Some (Dictionary.build_dictionary (Csv.load !dict_file))
       with _ ->
         let () = print_endline "Error in loading dictionary" in
         exit 1
     else
       let () =
         starting_info :=
           "\n\
            Dictionary not defined; running with dictionary suppressed (all \
            words permitted without checking)"
       in
       None)

let syntax =
  if !syntax_file <> "" then
    try Syntax.build_syntax (BatFile.with_file_in !syntax_file BatIO.read_all)
    with _ ->
      let () = print_endline "Error in loading syntax" in
      exit 1
  else
    let () =
      starting_info :=
        !starting_info ^ "\nSyntax not defined; running with empty syntax"
    in
    Syntax.build_syntax "lexical categories:\nconstituent categories:\nrules:\n"

let main_loop =
  let tree = ref Tree.empty in
  let () = clear_screen () in
  let () = print_welcome () in
  let () =
    let tree_str = Tree_printer.to_fancy_string !tree in
    if tree_str <> "" then print_endline (tree_str ^ "\n");
    if !starting_info <> "" then print_endline (!starting_info ^ "\n");
    print_commands ();
    print_string "\n> ";
    flush stdout
  in
  let exit = ref false in
  while not !exit do
    let command = String.split_on_char ' ' (read_line ()) in
    try
      match List.hd command with
      | "quit" ->
          let () =
            print_endline "Goodbye!";
            exit := true
          in
          ()
      | "syntax" ->
          if !syntax_file <> "" then
            next_command_prompt !tree ""
              (BatFile.with_file_in !syntax_file BatIO.read_all)
          else next_command_prompt !tree "" "Syntax is empty"
      | "add" ->
          let lexical_category = List.nth command 1 in
          let word = List.nth command 2 in
          if validate_lexeme lexical_category word syntax !dictionary then (
            let lexeme = Dictionary.create_lexeme lexical_category word in
            let new_tree, _ = Tree.add !tree lexeme in
            tree := new_tree;
            next_command_prompt !tree "" "Added node")
          else next_command_prompt !tree "" "Error: Invalid lexeme"
      | "connect" ->
          let constituent_category = List.nth command 1 in
          let node1 = Tree.get !tree (int_of_string (List.nth command 2)) in
          if List.length command <= 3 then
            let test_node =
              Tree.NodeOne
                { t = node1; category = constituent_category; id = 0 }
            in
            if validate_constituent test_node constituent_category syntax then (
              let new_tree, _ =
                Tree.connect1 !tree node1 constituent_category
              in
              tree := new_tree;
              next_command_prompt !tree "" "Added connecting node")
            else next_command_prompt !tree "" "Error: Invalid constituent"
          else
            let node2 = Tree.get !tree (int_of_string (List.nth command 3)) in
            let test_node =
              Tree.NodeTwo
                {
                  t1 = node1;
                  t2 = node2;
                  category = constituent_category;
                  id = 0;
                }
            in
            if validate_constituent test_node constituent_category syntax then (
              let new_tree, _ =
                Tree.connect2 !tree node1 node2 constituent_category
              in
              tree := new_tree;
              next_command_prompt !tree "" "Added connecting node")
            else next_command_prompt !tree "" "Error: Invalid constituent"
      | "remove" ->
          let node = List.nth command 1 in
          let new_tree = Tree.remove !tree (int_of_string node) in
          tree := new_tree;
          next_command_prompt !tree "" "Removed node"
      | "expand" ->
          let lexical_category = List.nth command 1 in
          let word = List.nth command 2 in

          (* Adds to the abstract dictionary representation *)
          let lexeme = Dictionary.create_lexeme lexical_category word in
          (dictionary :=
             match !dictionary with
             | None -> None
             | Some dict -> Some (Dictionary.augment_dictionary lexeme dict));

          (* Adds to the physical dictionary file user passed in *)
          Csv.save !dict_file ([ lexical_category; word ] :: Csv.load !dict_file);

          next_command_prompt !tree "" "Added word to dictionary"
      | "review" -> next_command_prompt !tree "review" "Review displayed"
      | _ -> next_command_prompt !tree "" "Unknown command"
    with
    | Tree.Wrong -> next_command_prompt !tree "" "Error: Invalid tree operation"
    | Failure msg -> next_command_prompt !tree "" ("Error: " ^ msg)
    | Invalid_argument msg ->
        next_command_prompt !tree "" ("Invalid argument: " ^ msg)
    | _ ->
        next_command_prompt !tree "" "Unknown error occurred. Please try again."
  done
