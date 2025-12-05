open Bamboo

let fancy_strings_enabled = ref true
let review_opt = ref true
let dictionary_exists = ref true

let clear_screen () =
  let () = print_string "\027[2J" in
  flush stdout

let print_commands () =
  let print_header =
    ANSITerminal.print_string
      [ ANSITerminal.cyan; ANSITerminal.Bold; ANSITerminal.on_black ]
  in
  let print_text =
    ANSITerminal.print_string
      [ ANSITerminal.white; ANSITerminal.Bold; ANSITerminal.on_black ]
  in

  let () = print_header "Commands:\n" in
  let () =
    print_header "\tquit -> ";
    print_text "Exits the program\n"
  in
  let () =
    print_header "\tsyntax -> ";
    print_text
      "Displays the full syntax, including lexical categories, constituent \
       categories, and constituent formation rules\n"
  in
  let () =
    print_header "\tadd <lexical-category> <word> -> ";
    print_text
      "Adds a node containing the described lexeme to the syntax tree\n"
  in
  let () =
    print_header "\tconnect <constituent-category> <node> -> ";
    print_text
      "Adds a constituent node parenting the given node to the syntax tree\n"
  in
  let () =
    print_header "\tconnect <constituent-category> <node1> <node2> -> ";
    print_text
      "Adds a constituent node parenting the two given nodes in order to the \
       syntax tree\n"
  in
  let () =
    print_header "\tremove <node> -> ";
    print_text
      "Removes the given node and all its connections from the tree; can only \
       remove top-level nodes (i.e., nodes with no parents)\n"
  in
  let () =
    if !dictionary_exists then (
      print_header "\texpand-dict <lexical-category> <word> -> ";
      print_text
        "Expands the current dictionary (including the file) with a word \
         containing the described lexeme.\n")
    else ()
  in
  let () =
    print_header "\texpand-syntax <rule-category> <rule> -> ";
    print_text
      "Expands the current syntax with (including the file) the rule passed \
       in. The rules categories for this REPL are 'lexical-categories', \
       'constituent-categories', and 'rules'. Please ensure complete \
       compliance with the syntax format, including spacing. If you need to \
       add a rule, please put quotes around the rule to ensure it is read \
       properly.\n"
  in
  let () =
    print_header "\treview -> ";
    print_text "Provides the user with a description of how syntax trees work\n"
  in
  let () =
    print_header "\ttoggle_fancy -> ";
    print_text
      "Toggle the display of trees between nested brackets and tree structures\n"
  in
  let () =
    print_header "\treset_tree -> ";
    print_text "Resets the tree and ID numbers back to an empty tree\n"
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
        The top of a tree, the root, typically contains a TP, which represents \
        the sentence as a whole. The rule, TP -> DP VP, indicates that a TP \
        node branches into two nodes, a DP and VP. The visualization is shown \
        below:\n\n\
       \   TP\n\
       \  / \\\n\
       \ DP VP\n\n\
        A DP represents a noun phrase with a determiner (i.e, \"the\", \"my\", \
        \"his\", \"one\", \"four\", etc.), while a VP represents a verb \
        phrase. For the sentence:\n\n\
       \ \t\"The boy walks.\" \n\n\
        The simple structure is shown below:\n\n" ^ ex_tree
     ^ "\n\n\
        It is important to note that we can not connect two lexical nodes \
        together (i.e, N, V, D, P, etc.). For example, for a DP to be formed, \
        we must first form an NP first. We can not connect a D with an N as \
        those are both lexical nodes.\n\n\
        Right now, to connect nodes, use the rightmost id number as that \
        represents the id number of the topmost node of the subtree.\n\n\
        Here are the lexical node representations:\n\
        N\t- Noun\n\
        V\t- Verb\n\
        D\t- Determiner\n\
        P\t- Preposition (not originally given)\n\
        Adj\t- Adjective\n\n\
        Here are some general nodes that are not originally given:\n\
        PP\t-> P DP\n\
        VP\t-> V PP\n\
        AdjP\t-> Adj\n\
        NP\t-> AdjP N\n\n")
  in
  ()

let print_welcome () =
  let () =
    ANSITerminal.print_string
      [ ANSITerminal.green; ANSITerminal.on_black ]
      "╔════════════════════════════════════════╗\n"
  in
  let () =
    ANSITerminal.print_string
      [ ANSITerminal.green; ANSITerminal.on_black ]
      "║        Welcome to Bamboo!              ║\n"
  in
  let () =
    ANSITerminal.print_string
      [ ANSITerminal.green; ANSITerminal.on_black ]
      "╚════════════════════════════════════════╝\n"
  in
  let _ = print_review () in
  ()

(** [repl_state] is the state of the REPL on executing the last typed in
    command. *)
type repl_state =
  | Ok
  | Warning
  | Error

let next_command_prompt tree extra prev_type =
  let () = clear_screen () in
  let () =
    if !review_opt then
      let () = review_opt := false in
      print_review ()
    else ()
  in
  let tree_str =
    if !fancy_strings_enabled then Tree_printer.to_fancy_string tree
    else Tree_printer.to_string tree
  in
  let () = if tree_str <> "" then print_endline ("\n\n" ^ tree_str ^ "\n") in
  let () =
    if extra <> "" then
      let print_fn =
        match prev_type with
        | Ok ->
            ANSITerminal.print_string
              [ ANSITerminal.green; ANSITerminal.on_black ]
        | Warning ->
            ANSITerminal.print_string
              [ ANSITerminal.yellow; ANSITerminal.on_black ]
        | Error ->
            ANSITerminal.print_string
              [ ANSITerminal.red; ANSITerminal.on_black ]
      in

      print_fn ("\n\n" ^ extra ^ "\n\n")
  in
  let () = print_commands () in
  let () = print_string "\n> " in
  flush stdout

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
         let () =
           ANSITerminal.print_string
             [ ANSITerminal.red; ANSITerminal.on_black ]
             "Error in loading dictionary\n"
         in
         exit 1
     else
       let () =
         starting_info :=
           "\n\
            Dictionary not defined; running with dictionary suppressed (all \
            words permitted without checking)"
       in
       let () = dictionary_exists := false in
       None)

let syntax =
  ref
    (if !syntax_file <> "" then
       try
         let file = BatFile.with_file_in !syntax_file BatIO.read_all in
         Syntax.build_syntax file
       with _ ->
         let () =
           ANSITerminal.print_string
             [ ANSITerminal.red; ANSITerminal.on_black ]
             "Error in loading syntax\n"
         in
         exit 1
     else
       let () =
         starting_info :=
           !starting_info ^ "\nSyntax not defined; running with empty syntax"
       in
       Syntax.build_syntax
         "lexical categories:\nconstituent categories:\nrules:\n")

(* This logic has been abstracted out in case we need a more powerful command
   reader/parser later *)
let read_command () = String.split_on_char ' ' (read_line ())

let main_loop =
  let tree = ref Tree.empty in
  let () = clear_screen () in
  let () = print_welcome () in

  let () =
    let tree_str = Tree_printer.to_fancy_string !tree in
    if tree_str <> "" then print_endline (tree_str ^ "\n");

    if !starting_info <> "" then
      ANSITerminal.print_string
        [ ANSITerminal.yellow; ANSITerminal.on_black ]
        (!starting_info ^ "\n\n");

    print_commands ();
    print_string "\n> ";
    flush stdout
  in

  let exit = ref false in
  while not !exit do
    let command = read_command () in
    try
      match List.hd command with
      | "quit" ->
          let () =
            print_endline "Goodbye! Thanks for using Bamboo!";
            ANSITerminal.print_string
              [ ANSITerminal.green; ANSITerminal.on_black ]
              (* Art credit: https://emojicombos.com/bamboo-ascii-art *)
              "⠀⠀⠀⣀⣀⡀⠀⠀⠀⠀⠀⠀⠀⢀⣀⣀⡀⠀⠀⠀⣀⣀⣀⠀⠀⠀⠀⠀⠀⠀\n\
               ⠀⠀⠀⢹⣿⡇⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⠇⠀⠀⠀⢸⣿⣿⠀⠀⠀⠀⠀⠀⠀\n\
               ⠀⠀⠀⢸⣿⡇⠀⠀⠀⠀⠀⠀⠀⠈⣿⣿⠀⠀⠀⠀⢸⣿⣿⠀⣶⡿⠃⠀⠀⠀\n\
               ⠀⠀⠀⢸⣿⡇⠀⠀⠀⠀⠰⣄⠀⠀⣿⣿⠀⠀⠀⠀⠚⠛⠛⠀⣤⣶⣦⣤⡄⠀\n\
               ⠀⠀⠀⠛⠛⠃⠀⠀⠀⠀⠀⠹⠗⢀⣿⣿⡀⠀⠀⠀⢹⣿⣿⠀⣈⡉⠉⠀⠀⠀\n\
               ⠀⠀⠀⢹⣿⡇⠀⠀⠉⠻⠿⣿⡆⠘⣛⣛⠃⠀⠀⠀⢸⣿⡏⠀⠻⢷⠀⠀⠀⠀\n\
               ⠀⠀⠀⢸⣿⡇⠀⠀⣀⠀⠀⠀⠀⢸⣿⣿⡇⠀⠀⠀⢸⣿⡇⠀⠀⠀⠀⠀⠀⠀\n\
               ⠀⠀⠀⢸⣿⡇⢠⣾⠏⠀⠀⠀⠀⠘⣿⣿⠀⠀⠀⠀⢸⣿⣷⠀⠀⠀⠀⠀⠀⠀\n\
               ⠀⠀⠀⠺⠿⠇⠈⣁⣀⠀⠀⠀⠀⠀⣿⣿⠀⠀⠀⠀⣉⣉⣉⠀⠀⠀⠀⠀⠀⠀\n\
               ⠀⠀⠀⢶⣶⡆⢈⡛⠛⠿⠦⠀⠀⢸⣿⣿⡀⠀⠀⠀⢿⣿⡿⠀⠀⠀⠀⠀⠀⠀\n\
               ⠀⠀⠀⢸⣿⡇⠘⠿⣷⡀⠀⠀⠀⠘⠛⠛⠃⠀⠀⠀⢸⣿⡇⠀⠀⣀⣤⡄⠀⠀\n\
               ⠀⠀⠀⢸⣿⡇⠀⠀⠀⠉⠀⠀⠀⢸⣿⣿⠃⠀⠀⠀⣾⣿⣷⠀⣿⠿⠋⠀⠀⠀\n\
               ⠀⠀⠀⢸⣿⡇⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⠀⠀⠀⠀⣤⣤⣤⠠⣤⣄⡀⠀⠀⠀\n\
               ⠀⠀⠀⣾⣿⡇⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⡇⠀⠀⠀⢸⣿⣏⠀⠈⠉⠉⠀⠀⠀\n\
               ⠀⠀⠀⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠈⠉⠉⠁⠀⠀⠀⠉⠉⠉⠀⠀⠀⠀⠀⠀⠀\n";
            exit := true
          in
          ()
      | "syntax" -> next_command_prompt !tree (Syntax.to_string !syntax) Ok
      | "add" ->
          let lexical_category = List.nth command 1 in
          let word = List.nth command 2 in
          if Validator.validate_lexeme lexical_category word !syntax !dictionary
          then
            (let lexeme = Dictionary.create_lexeme lexical_category word in
             let new_tree, _ = Tree.add !tree lexeme in
             tree := new_tree;
             next_command_prompt !tree "Added node")
              Ok
          else next_command_prompt !tree "Error: Invalid lexeme" Error
      | "connect" ->
          let constituent_category = List.nth command 1 in
          let node1 = Tree.get !tree (int_of_string (List.nth command 2)) in
          if List.length command <= 3 then
            let test_node =
              Tree.NodeOne
                { t = node1; category = constituent_category; id = 0 }
            in
            if
              Validator.validate_constituent test_node constituent_category
                !syntax
            then
              (let new_tree, _ =
                 Tree.connect1 !tree node1 constituent_category
               in
               tree := new_tree;
               next_command_prompt !tree "Added connecting node")
                Ok
            else next_command_prompt !tree "Error: Invalid constituent" Error
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
            if
              Validator.validate_constituent test_node constituent_category
                !syntax
            then
              (let new_tree, _ =
                 Tree.connect2 !tree node1 node2 constituent_category
               in
               tree := new_tree;
               next_command_prompt !tree "Added connecting node")
                Ok
            else next_command_prompt !tree "Error: Invalid constituent" Error
      | "remove" ->
          let node = List.nth command 1 in
          let new_tree = Tree.remove !tree (int_of_string node) in
          tree := new_tree;
          next_command_prompt !tree "Removed node" Ok
      | "expand-dict" ->
          if !dictionary_exists then
            (let lexical_category = List.nth command 1 in
             let word = List.nth command 2 in

             (* Adds to the abstract dictionary representation *)
             let lexeme = Dictionary.create_lexeme lexical_category word in
             (dictionary :=
                match !dictionary with
                | None -> None
                | Some dict -> Some (Dictionary.augment_dictionary lexeme dict));

             (* Adds to the physical dictionary file user passed in *)
             Csv.save !dict_file
               ([ lexical_category; word ] :: Csv.load !dict_file);

             next_command_prompt !tree "Added word to dictionary")
              Ok
          else
            next_command_prompt !tree
              "This command is disabled when the dictionary file is not passed \
               as a program argument."
              Warning
      | "expand-syntax" -> (
          let category = List.nth command 1 in
          let rule = List.nth command 2 in

          let file_opt, message =
            if !syntax_file = "" then
              ( None,
                "No syntax file was provided. New syntax element was added to \
                 working syntax" )
            else
              ( Some !syntax_file,
                "Added new syntax element to syntax and modified syntax file" )
          in
          try
            syntax := Syntax.modify_syntax category rule !syntax file_opt;
            next_command_prompt !tree message Ok
          with _ ->
            (next_command_prompt !tree
               "Error: The format of the command given does not match the \
                syntax specification exactly")
              Error)
      | "toggle_fancy" ->
          let () = fancy_strings_enabled := not !fancy_strings_enabled in
          next_command_prompt !tree "Toggled tree printing type" Ok
      | "reset_tree" ->
          let () = tree := Tree.empty in
          next_command_prompt !tree "Reset tree" Ok
      | "review" ->
          let () = review_opt := true in
          next_command_prompt !tree "Review displayed" Ok
      | _ -> next_command_prompt !tree "Unknown command" Warning
    with
    | Tree.NonexistentNodeID id ->
        next_command_prompt !tree
          ("Error: Node with ID " ^ string_of_int id ^ " does not exist")
          Error
    | Tree.NonTopLevelNodeID id ->
        next_command_prompt !tree
          ("Error: Node with ID " ^ string_of_int id
         ^ " does not exist or is not a top-level node")
          Error
    | Failure msg -> next_command_prompt !tree ("Error: " ^ msg) Error
    | Invalid_argument msg ->
        next_command_prompt !tree ("Invalid argument: " ^ msg) Warning
    | _ ->
        next_command_prompt !tree "Unknown error occurred. Please try again."
          Error
  done
