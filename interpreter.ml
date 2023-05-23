type stackValue = BOOL of bool | INT of int | ERROR | STRING of string | NAME of string | UNIT
type command = ADD | SUB | MUL | DIV | PUSH of stackValue | POP | NEG | SWAP | REM | QUIT
| TOSTRING | PRINTLN | CAT | AND | OR | NOT | EQUAL | LESSTHAN | IF | BIND | LET | END
let rec interpreter((input: string), (output: string)): unit =
  let ic = open_in input in
  let oc = open_out output in
  let rec loop_read acc =
    try
      (* Removes unneccesary whitespace from input *)
      let l = String.trim(input_line ic) in loop_read (l::acc)
    with
    | End_of_file -> List.rev acc
  in

  let strList = loop_read [] in
  (* Takes a string and returns a command *)
  
  let str2sv s = 
    (* Names start with either an underscore or character
       Integers can be converted to strings using int_of_string function
       Strings begin with quotations
       Booleans can be defined as the literals True or False *)
       let n = String.get s 0 in
       match s with
       |":unit:" -> UNIT
       |":error:" -> ERROR
       |":true:" -> BOOL true
       |":false:" -> BOOL false
       | _ -> if((n) = '"') then STRING(String.sub s 1 (String.length s -2)) else
        if((n)= '0') then INT(int_of_string s) else
          if((n)= '1') then INT(int_of_string s) else
            if((n)= '2') then INT(int_of_string s) else
              if((n)= '3') then INT(int_of_string s) else
                if((n)= '4') then INT(int_of_string s) else
                  if((n)= '5') then INT(int_of_string s) else
                    if((n)= '6') then INT(int_of_string s) else
                      if((n)= '7') then INT(int_of_string s) else
                        if((n)= '8') then INT(int_of_string s) else
                          if((n)= '9') then INT(int_of_string s) else
                            if((n) = '-') then INT(int_of_string s) else
            NAME(s)
          in

(* String value returns a command *)
  let str2com s =
    match s with
    | "add" -> ADD
    | "sub" -> SUB
    | "mul" -> MUL
    | "div" -> DIV
    | "pop" -> POP
    | "neg" -> NEG
    | "swap" -> SWAP
    | "rem" -> REM
    | "quit" -> QUIT
    | "toString" -> TOSTRING
    | "println" -> PRINTLN
    | "cat" -> CAT
    | "and" -> AND
    | "or" -> OR
    | "not" -> NOT
    | "equal" -> EQUAL
    | "lessThan" -> LESSTHAN
    | "if" -> IF
    | "bind" -> BIND
    | "let" -> LET
    | "end" -> END
    | _ -> PUSH (str2sv (String.sub s 5 (String.length s - 5)))

  in

(*Stack value returns a string *)
  let sv2str sv =
    match sv with
    | BOOL i -> (
      match i with
      | true -> ":true:"
      | false -> ":false:"
     )
    
    | INT i -> string_of_int i
    | STRING i -> i
    | NAME i -> i
    | ERROR -> ":error:"
    | UNIT -> ":unit:"

  in
(* Command returns a string value *) (*
  let com2str c =
    match c with
    | ADD -> "add"
    | SUB -> "sub"
    | MUL -> "mul"
    | DIV -> "div"
    | POP -> "pop"
    | NEG -> "neg"
    | SWAP -> "swap"
    | REM -> "rem"
    | QUIT -> "quit"
    | TOSTRING -> "toString"
    | PRINTLN -> "println"
    | CAT -> "cat"
    | AND -> "and"
    | OR -> "or"
    | NOT -> "not"
    | EQUAL -> "equal"
    | LESSTHAN -> "lessThan"
    | IF -> "if"
    | BIND -> "bind"
    | LET -> "let"
    | END -> "end"
    | PUSH sv -> "push " ^ (sv2str sv)

  in *)
(* Creats a mapping of commands and strings *)
  let comList = List.map str2com strList in

  let file_write bool_val = Printf.fprintf oc "%s\n" bool_val in

  let rec processor comList stack =
    match (comList, stack) with
    | (ADD::restOfCommands, INT(a)::INT(b)::restOfStack) -> processor restOfCommands (INT (a+b)::restOfStack)
    | (ADD::restOfCommands, stack) -> processor restOfCommands (ERROR::stack)
    | (SUB::restOfCommands, INT(b)::INT(a)::restOfStack) -> processor restOfCommands (INT (a-b)::restOfStack)
    | (SUB::restOfCommands, stack) -> processor restOfCommands (ERROR::stack)
    | (MUL::restOfCommands, INT(a)::INT(b)::restOfStack) -> processor restOfCommands (INT (a*b)::restOfStack)
    | (MUL::restOfCommands, stack) -> processor restOfCommands (ERROR::stack)
    | (DIV::restOfCommands, INT(0)::INT(a)::restOfStack) -> processor restOfCommands (ERROR::stack)
    | (DIV::restOfCommands, INT(b)::INT(a)::restOfStack) -> processor restOfCommands (INT (a/b)::restOfStack)
    | (DIV::restOfCommands, stack) -> processor restOfCommands (ERROR::stack)
    | (PUSH (i)::restOfCommands, stack) -> processor restOfCommands ((i)::stack)
    | (POP::restOfCommands, i::restOfStack) -> processor restOfCommands (restOfStack)
    | (POP::restOfCommands, []) -> processor restOfCommands (ERROR::[])
    | (NEG::restOfCommands, INT(i)::restOfStack) -> processor restOfCommands (INT (i-(2*i))::restOfStack)
    | (NEG::restOfCommands, stack) -> processor restOfCommands (ERROR::stack)
    | (SWAP::restOfCommands, (a)::(b)::restOfStack) -> processor restOfCommands (b::a::restOfStack)
    | (SWAP::restOfCommands, stack) -> processor restOfCommands (ERROR::stack)
    | (REM::restOfCommands, INT(0)::INT(a)::restOfStack) -> processor restOfCommands (ERROR::stack)
    | (REM::restOfCommands, INT(b)::INT(a)::restOfStack) -> processor restOfCommands (INT (a mod b)::restOfStack)
    | (REM::restOfCommands, stack) -> processor restOfCommands (ERROR::stack)
    | (PRINTLN::restOfCommands, STRING i::restOfStack) -> (file_write i ;processor restOfCommands (restOfStack))
    | (PRINTLN::restOfCommands, stack) -> processor restOfCommands (ERROR::stack)
    | (TOSTRING::restOfCommands, i::restOfStack) -> processor restOfCommands (STRING (sv2str(i))::restOfStack)
    | (TOSTRING::restOfCommands, []) -> processor restOfCommands (ERROR::[])
    | (CAT::restOfCommands, STRING(b)::STRING(a)::restOfStack) -> processor restOfCommands (STRING (a^b)::restOfStack)
    | (CAT::restOfCommands, stack) -> processor restOfCommands (ERROR::stack)
    | (AND::restOfCommands, BOOL(a)::BOOL(b)::restOfStack) -> processor restOfCommands (BOOL (a&&b)::restOfStack)
    | (AND::restOfCommands, stack) -> processor restOfCommands (ERROR::stack)
    | (OR::restOfCommands, BOOL(a)::BOOL(b)::restOfStack) -> processor restOfCommands (BOOL (a||b)::restOfStack)
    | (OR::restOfCommands, stack) -> processor restOfCommands (ERROR::stack)
    | (NOT::restOfCommands, BOOL(i)::restOfStack) -> processor restOfCommands (BOOL (not i)::restOfStack)
    | (NOT::restOfCommands, stack) -> processor restOfCommands (ERROR::stack)
    | (EQUAL::restOfCommands, INT(a)::INT(b)::restOfStack) -> processor restOfCommands (BOOL (a = b)::restOfStack)
    | (EQUAL::restOfCommands, stack) -> processor restOfCommands (ERROR::stack)
    | (LESSTHAN::restOfCommands, INT(b)::INT(a)::restOfStack) -> processor restOfCommands (BOOL (a < b)::restOfStack)
    | (LESSTHAN::restOfCommands, stack) -> processor restOfCommands (ERROR::stack)
    | (IF::restOfCommands, (a)::(b)::BOOL(c)::restOfStack) -> (if c then processor restOfCommands ((a)::restOfStack)
        else processor restOfCommands ((b)::restOfStack))
    | (IF::restOfCommands, stack) -> processor restOfCommands (ERROR::stack) 
    | (QUIT::restOfCommands, stack) -> ()
    | _ -> ()
  in processor comList [];;

(interpreter("input.txt","output.txt"))