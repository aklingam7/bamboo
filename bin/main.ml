open Bamboo

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
  ()

let next_command_prompt extra =
  let () = clear_screen () in
  let tree_strings = List.map Tree.to_string !Tree.treelist in
  let () = List.iter print_endline tree_strings in
  let () = print_endline ("\n" ^ extra ^ "\n") in
  print_commands ()

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
       try Dictionary.build_dictionary (Csv.load !dict_file)
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
       Dictionary.suppressed_dictionary)

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
  let () = clear_screen () in
  let () = next_command_prompt !starting_info in
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
            next_command_prompt
              (BatFile.with_file_in !syntax_file BatIO.read_all)
          else next_command_prompt "Syntax is empty"
      | "add" ->
          let lexical_category = List.nth command 1 in
          let word = List.nth command 2 in
          let () = Tree.add lexical_category word syntax !dictionary in
          next_command_prompt "Added node"
      | "connect" ->
          let constituent_category = List.nth command 1 in
          let node1 =
            Tree.get (int_of_string (List.nth command 2)) !Tree.treelist
          in
          let () =
            if List.length command <= 3 then
              Tree.connect1 node1 constituent_category syntax
            else
              let node2 =
                Tree.get (int_of_string (List.nth command 3)) !Tree.treelist
              in
              Tree.connect2 node1 node2 constituent_category syntax
          in
          next_command_prompt "Added connecting node"
      | "remove" ->
          let node = List.nth command 1 in
          let () = Tree.remove (int_of_string node) !Tree.treelist in
          next_command_prompt "Removed node"
      | "expand" ->
          let lexical_category = List.nth command 1 in
          let word = List.nth command 2 in

          (* Adds to the abstract dictionary representation *)
          let lexeme = Dictionary.create_lexeme lexical_category word in
          dictionary := Dictionary.augment_dictionary lexeme !dictionary;

          (* Adds to the physical dictionary file user passed in *)
          Csv.save !dict_file ([ lexical_category; word ] :: Csv.load !dict_file);

          next_command_prompt "Added word to dictionary"
      | _ -> next_command_prompt "Unknown command"
    with _ -> next_command_prompt "Invalid command"
  done
