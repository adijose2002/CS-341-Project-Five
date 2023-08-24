//
// Analyzer for simple C programs.  This component performs
// type checking.  The analyzer returns a string denoting
// success or failure. The string "success" if the input 
// program is legal, otherwise the string "type_error: ..." 
// is returned denoting an invalid simple C program.
//
// Modified by:
//   << Adithya Jose >>
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module checker =
  //
  // NOTE: all functions in the module must be indented.
  //


  //
  // match_token
  //
  let private match_token expected_token (tokens: string list) =

    let next_token = List.head tokens

    if expected_token = "identifier" && next_token.StartsWith("identifier") then
      List.tail tokens
    elif expected_token = "int_literal" && next_token.StartsWith("int_literal") then
      List.tail tokens
    elif expected_token = "str_literal" && next_token.StartsWith("str_literal") then
     List.tail tokens
    elif expected_token = "real_literal" && next_token.StartsWith("real_literal") then
      List.tail tokens
    elif expected_token = next_token then  
      List.tail tokens
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)


  let rec varName s v = 
    let search = List.filter (fun (name, varType) -> name = v) s
    if search.Length > 0 then
      true
    else
      false


  let rec declaredName s v = 
    match s with
    | [] -> (false, "")
    | (name, varType)::tail when name = v ->(true, varType)
    | head::tail -> declaredName tail v


  //
  // <expr-value> -> identifier
  //               | int_literal
  //               | str_literal
  //               | true
  //               | false
  //
  let rec private expr_value tokens s =
    let next_token = List.head tokens
    if next_token = "false" then
      let T2 = match_token "false" tokens
      (T2, "bool")
    elif next_token = "true" then
      let T2 = match_token "true" tokens
      (T2, "bool")
    elif next_token.StartsWith("identifier") then
      let v = next_token.[11..]
      if (varName s v) then
        let T2 = match_token "identifier" tokens
        let (result, varType) = declaredName s v
        (T2, varType)
      else
        failwith ("variable '" + v + "' undefined")
    elif next_token.StartsWith("int_literal") then
      let T2 = match_token "int_literal" tokens
      (T2, "int")
    elif next_token.StartsWith("str_literal") then
      let T2 = match_token "str_literal" tokens
      (T2, "str")
    elif next_token.StartsWith("real_literal") then
      let T2 = match_token "real_literal" tokens
      (T2, "real")
    else
      failwith ("expecting identifier or literal, but found " + next_token)


  //
  // <expr-op> -> +
  //            | -
  //            | *
  //            | /
  //            | ^
  //            | <
  //            | <=
  //            | >
  //            | >=
  //            | ==
  //            | !=
  //
  let rec private expr_op tokens = 
    let next_token = List.head tokens

    if next_token = "+"  ||
       next_token = "-"  ||
       next_token = "*"  ||
       next_token = "/"  ||
       next_token = "^"  ||
       next_token = "<"  ||
       next_token = "<=" ||
       next_token = ">"  ||
       next_token = ">=" ||
       next_token = "==" ||
       next_token = "!=" then
      let T2 = match_token next_token tokens
      (T2, next_token)
    else
      failwith ("expecting expression operator, but found " + next_token)


  //
  // <expr> -> <expr-value> <expr-op> <expr-value>
  //         | <expr-value>
  //
  let rec private expr tokens s = 
    let (T2, firstType) = expr_value tokens s
    let next_token = List.head T2

    if next_token = "+"  ||
       next_token = "-"  ||
       next_token = "*"  ||
       next_token = "/"  ||
       next_token = "^"  ||
       next_token = "<"  ||
       next_token = "<=" ||
       next_token = ">"  ||
       next_token = ">=" ||
       next_token = "==" ||
       next_token = "!=" then
      let (T3, opp) = expr_op T2
      let (T4, secondType) = expr_value T3 s
      if opp = "+" ||
         opp = "-" ||
         opp = "*" ||
         opp = "/" ||
         opp = "^" then  
        if (firstType = "int" || firstType = "real") && secondType = firstType then
          (T4, firstType)
        else  
          failwith ("operator " + opp + " must involve 'int' or 'real'")
      else  
        if opp = "==" && (firstType = "real" || secondType = "real") then 
          printfn "warning: comparing real numbers with == may never be true"
          (T4, "bool")
        elif firstType = secondType then  
          (T4, "bool")
        else
          failwith ("type mismatch '" + firstType + "' " + opp + " '" + secondType + "'")
    else
      (T2, firstType)


  //
  // <empty> -> ;
  //
  let rec private empty tokens = 
    let T2 = match_token ";" tokens
    T2


  //
  // <vardecl> -> int identifier ;
  // 
  let rec private vardecl tokens = 
    let next_token = List.head tokens
    if next_token = "int" then
      let T2 = match_token "int" tokens
      let T3 = match_token "identifier" T2
      let T4 = match_token ";" T3
      T4
    else
      let T2 = match_token "real" tokens
      let T3 = match_token "identifier" T2
      let T4 = match_token ";" T3
      T4


  //
  // <input> -> cin >> identifier ;
  //
  let rec private input tokens s = 
    let T2 = match_token "cin" tokens
    let T3 = match_token ">>" T2
    let v = (List.head T3).[11..]
    
    if (varName s v) then
      let T4 = match_token "identifier" T3
      let T5 = match_token ";" T4
      T5
    else
      failwith ("variable '" + v + "' undefined")


  //
  // <output-value> -> <expr-value>
  //                 | endl
  //
  let rec private output_value tokens s = 
    let next_token = List.head tokens
    if next_token = "endl" then
      let T2 = match_token "endl" tokens
      T2
    else
      let (T2, exprType) = expr_value tokens s
      T2


  //
  // <output> -> cout << <output-value> ;
  //
  let rec private output tokens s = 
    let T2 = match_token "cout" tokens
    let T3 = match_token "<<" T2
    let T4 = output_value T3 s
    let T5 = match_token ";" T4
    T5


  //
  // <assignment> -> identifier = <expr> ;
  //
  let rec private assignment (tokens: string list) s = 
    let next_token = List.head tokens
    let v = next_token.Substring(11)
    let (result, varType) = declaredName s v
    if result = true then
      let T2 = match_token "identifier" tokens
      let T3 = match_token "=" T2
      let (T4, exprType) = expr T3 s
      if (varType = "real" && exprType.StartsWith("int") || exprType.StartsWith(varType)) then
        let T5 = match_token ";" T4
        T5
      else 
        let exprPos = 
          if exprType.StartsWith("int") then
            "int"
          elif exprType.StartsWith("real") then
            "real"
          elif exprType.StartsWith("bool") then
            "bool"
          else "str"
        failwith ("cannot assign '" + exprPos + "' to variable of type '" + varType + "'")
    else 
        failwith ("variable '" + v + "' undefined")


  //
  // <stmt> -> <empty>
  //         | <vardecl>
  //         | <input>
  //         | <output>
  //         | <assignment>
  //         | <ifstmt>
  //
  let rec private stmt tokens s = 
    let next_token = List.head tokens

    if next_token = ";" then
      let T2 = empty tokens
      T2
    elif next_token = "int" || next_token = "real" then
      let T2 = vardecl tokens 
      T2
    elif next_token = "cin" then
      let T2 = input tokens s
      T2
    elif next_token = "cout" then
      let T2 = output tokens s
      T2
    elif next_token.StartsWith("identifier") then
      let T2 = assignment tokens s
      T2
    elif next_token = "if" then
      let T2 = ifstmt tokens s 
      T2
    else
      failwith ("expecting statement, but found " + next_token)


  //
  // <ifstmt> -> if ( <condition> ) <then-part> <else-part>
  //
  and private ifstmt tokens s = 
    let T2 = match_token "if" tokens
    let T3 = match_token "(" T2
    let T4 = condition T3 s
    let T5 = match_token ")" T4
    let T6 = then_part T5 s
    let T7 = else_part T6 s
    T7


  //
  // <condition> -> <expr>
  //
  and private condition tokens s = 
    let (T2, exprType) = expr tokens s
    if exprType = "bool" then
      T2
    else
      failwith ("if condition must be 'bool', but found '" + exprType + "'")


  //
  // <then-part> -> <stmt>
  //
  and private then_part tokens s = 
    let T2 = stmt tokens s
    T2


  //
  // <else-part> -> else <stmt>
  //              | EMPTY
  //
  and private else_part tokens s = 
    let next_token = List.head tokens
    
    if next_token = "else" then
      let T2 = match_token "else" tokens
      let T3 = stmt T2 s
      T3
    else
      tokens


  //
  // <morestmts> -> <stmt> <morestmts>
  //              | EMPTY
  //
  let rec private morestmts tokens s = 
    let next_token = List.head tokens
  
    if next_token = ";"    ||
       next_token = "int"  ||
       next_token = "cin"  ||
       next_token = "cout" ||
       next_token.StartsWith("identifier") ||
       next_token = "if" ||
       next_token = "real" then
    
      let T2 = stmt tokens s
      let T3 = morestmts T2 s
      T3

    else 
      tokens


  //
  // <stmts> -> <stmt> <morestmts>
  // 
  let rec private stmts tokens s = 
    let T2  = stmt tokens s
    let T3 = morestmts T2 s
    T3


  //
  // <simpleC> -> void main ( ) { <stmts> } $
  //
  let private simpleC tokens s = 
    let T2 = match_token "void" tokens
    let T3 = match_token "main" T2
    let T4 = match_token "(" T3
    let T5 = match_token ")" T4
    let T6 = match_token "{" T5
    let T7 = stmts T6 s
    let T8 = match_token "}" T7
    let T9 = match_token "$" T8
    T9


  //
  // typecheck tokens symboltable
  //
  // Given a list of tokens and a symbol table, type-checks 
  // the program to ensure program's variables and expressions
  // are type-compatible. If the program is valid, returns 
  // the string "success". If the program contains a semantic
  // error or warning, returns a string of the form
  // "type_error: ...".
  //
  let typecheck tokens symboltable = 
    try
      let T2 = simpleC tokens symboltable
      "success"
    with 
      | ex -> "type_error: " + ex.Message
