open Str


let  read_j_file  file = 
    let ic = open_in file in
    let rec read_j_file_ ls =
        try 
            let char1 = input_char ic in 
            read_j_file_ (char1::ls)                  
        with
        | End_of_file  ->  ls
    in

    List.rev (read_j_file_ [])

exception Str of string 
exception Char of string

let get_next_lex ls  =
    let rec get_next_lexem ls  buffer s_quote d_quote = 
(*        
        (match (ls) with 
            |x::xs ->Printf.printf "%c,%b\n" x  d_quote  ;    
            |_-> ()
        );
*)
        match ls,buffer with
        |('\'' as x::xs),[] when not s_quote  ->  
              get_next_lexem xs (x::buffer) true d_quote
    
        |('\'' as x::xs),_ when s_quote ->  
              xs,(x::buffer)  
        |('\"' as x::xs),[] when not d_quote  ->  
              get_next_lexem xs (x::buffer) s_quote true
    
        |('\"' as x::xs),_ when d_quote ->  
               xs, (x::buffer)
        |([],_)when (d_quote ) -> 
                raise (Str "quote non matched" )
        |(x::xs,_)when (d_quote ) -> 
            get_next_lexem xs (x::buffer) s_quote d_quote
        |([],_)when (s_quote ) -> 
                raise (Char "quote non matched" )
        |(x::xs,_)when (s_quote ) -> 
            get_next_lexem xs (x::buffer) s_quote d_quote
        |' '::xs,[] ->
            get_next_lexem xs buffer d_quote s_quote   
        |' '::xs,_ ->
               xs, buffer
        |'{'::xs,[] ->
                xs,['{']
        |'{'::xs,_ ->  
                ls, buffer
        |'}'::xs,[] ->
                xs,['.']
        |'}'::xs,_ ->  
                ls, buffer
        |'.'::xs,[] ->
                xs,['.']
        |'.'::xs,_ ->  
                ls, buffer
        |'('::xs,[] ->
                xs,['(']
        |'('::xs,_ ->  
                ls, buffer
        |')'::xs,[] ->
                xs,[')']
        |')'::xs,_ ->  
                ls, buffer
        |'>'::'='::xs,[] ->
                xs,['=';'>']
        |'>'::'='::xs,_ ->  
                ls, buffer
        |'<'::'='::xs,[] ->
                xs,['=';'<']
        |'<'::'='::xs,_ ->  
                ls, buffer
        |'|'::'>'::xs,[] ->
                xs,['>';'|']
        |'|'::'>'::xs,_ ->  
                ls, buffer
        |'<'::'|'::xs,[] ->
                xs,['|';'<']
        |'<'::'|'::xs,_ ->  
                ls, buffer
        |':'::'='::xs,[] ->
                xs,[':';'=']
        |':'::'='::xs,_ ->  
                ls, buffer
        |'<'::xs,[] ->
                xs,['<']
        |'<'::xs,_ ->  
                ls, buffer
        |'>'::xs,[] ->
                xs,['>']
        |'>'::xs,_ ->  
                ls, buffer
                
        |'+'::xs,[] ->
                xs,['+']
        |'+'::xs,_ ->  
                ls, buffer
        |'='::xs,[] ->
                xs,['=']
        |'='::xs,_ ->  
                ls, buffer
        |';'::xs,[] ->
                xs,[';']
        |';'::xs,_ ->  
                ls, buffer
        |('\n'::xs,_) -> 
               xs, buffer
        |(x::xs,_) -> 
               get_next_lexem xs (x::buffer) d_quote s_quote 
        | [],_ ->  ls, buffer
    in
    let stream,buf= (get_next_lexem ls [] false false) in
    stream, String.of_seq (List.to_seq(List.rev buf))

type tokens = 
    | IF  | LPARAM | RPARAM | COMMA | LBRAC | RBRAC  | RPIPE | LPIPE  
    | ARROW | ASSIGN   
    | LEQ | GEQ | EQ | LT | GT 
    | PLUS | MINUS | SLASH | TIMES
    | FLOAT of float | INTEGER of int | STRING of string | IDEN of string | CHAR of char 

        

let match_reg reg str= Str.string_match (Str.regexp reg)  str 0 
let typify str = 
        match  (str) with
            | "if" ->  IF
            | "("  ->  LPARAM
            | ")"  ->  RPARAM
            | "{"  ->  LBRAC
            | "}"  ->  RBRAC
            | "=>" ->  ARROW
            | ">=" ->  GEQ
            | "=" ->  EQ
            | "<=" ->  LEQ
            | ">"  ->  GT
            | "<"  ->  LT 
            | ":=" -> ASSIGN
            |"|>" -> RPIPE
            |"<|" -> LPIPE
            |"+" -> PLUS
            |"-" -> MINUS
            |"\\" -> SLASH
            |"*" -> TIMES 
            |"," -> COMMA
            |int_value when match_reg "^[0-9]+$" int_value -> 
                
                INTEGER ( Stdlib.int_of_string int_value)

            |float_value when match_reg "^[0-9]+\\.[0-9]+$" float_value -> 
                FLOAT (Stdlib.float_of_string float_value)
            |string_value when match_reg "^\".*\"$" string_value ->
                STRING (Str.replace_first (Str.regexp "\"\\(.*\\)\"" ) "\\1" string_value)

            |string_value when match_reg "^\'.\'$" string_value ->
                CHAR (String.get  (Str.replace_first (Str.regexp "\'\\(.\\)\'" ) "\\1" string_value) 0)
            | id_value -> IDEN(id_value) 

let stringify token = 
    match(token) with
        |IF      ->  "if"
        |LPARAM  ->  "(" 
        |RPARAM  ->  ")" 
        |LBRAC   ->  "{" 
        |RBRAC   ->  "}" 
        |ARROW   ->  "=>"
        |GEQ     ->  ">="
        |LEQ     ->  "<="
        |GT      ->  ">" 
        |LT      ->  "<" 
        |RPIPE   ->  "|>" 
        |LPIPE   ->  "<|" 
        |EQ      ->  "="
        |COMMA -> ","
        |ASSIGN -> ":="
        |PLUS -> "+"
        |MINUS -> "-"
        |SLASH -> "\\"
        |TIMES -> "*"
        |INTEGER(v) -> string_of_int v
        |FLOAT(v) -> string_of_float v
        |STRING(v) -> Printf.sprintf "\"%s\"" v 
        |CHAR(v) -> Printf.sprintf "%c" v
        |IDEN(v) -> v


let get_raw_lexes char_list = 
    let rec recur_lex ch_ls buf = 
        match (get_next_lex ch_ls) with 
        |([],end_val) -> end_val::buf
        |(stream,lex) -> recur_lex stream (lex::buf)
    in

    List.rev (recur_lex char_list [])
    

let extract_lexems str_list = List.map typify str_list 
exception Symbol

type exp = ADD of exp * exp | MULTI of exp * exp | PIPE of exp * exp | COMP of exp * exp  | CAT of exp * exp | PARAM of exp * exp
            | INT of int |FLOAT of float | STR of string| CHAR of char |IDEN of string  | EMPTY 



type term_type = OP of tokens 
            | NUMBER of tokens 
            | FUNCTION of tokens
            | ERROR 
            | LP | RP


type symbol = ENTRY of string * term_type  * symbol list

let parser_f terminals_list = 
    let ordering op = match op with
        |TIMES->3
        |SLASH->3
        |PLUS->2
        |MINUS->2
        |_-> -1
    in
    let term_type term = match term with 
        |TIMES|SLASH|PLUS|MINUS as x -> OP(x)
        | _ -> ERROR

    in
    let rec shunting_yard  terms output ops = match terms,output,ops with
        |(INTEGER(v) as top)::rterms,_,_ -> 
                shunting_yard rterms (top::output) ops 
        |STRING(v) as top::rterms,_,_ -> 
                shunting_yard rterms (top::output) ops 
 
        |FLOAT(v) as top::rterms,_,_ -> 
                shunting_yard rterms (top::output) ops 
        |CHAR(v) as top::rterms,_,_ -> 
                shunting_yard rterms (top::output) ops 
        |_->[] 

    in
    let rec expression terms output = match terms,output  with
        |RPARAM::xs,exp ->
            terms,output
        |LPARAM::xs,exp ->  
            let (rest_of_terms,paran_exp) = expression xs EMPTY in
            expression rest_of_terms (PARAM (exp,paran_exp))
        |INTEGER(v)::xs,EMPTY -> expression xs (INT v) 
        |FLOAT(v)::xs,EMPTY -> expression xs (FLOAT v) 
        |STRING(v)::xs,EMPTY -> expression xs (STR v) 
        |PLUS::INTEGER(v)::xs,exp  -> expression xs (ADD(exp, INT v)) 
        |PLUS::STRING(v)::xs,exp -> expression xs (CAT(exp,STR v)) 
        |PLUS::FLOAT(v)::xs,exp ->  expression xs (ADD(exp,FLOAT v))
        |MINUS::INTEGER(v)::xs,exp  -> expression xs (ADD( exp,INT (v * -1)))
        |MINUS::FLOAT(v)::xs,exp ->  expression xs (ADD( exp,FLOAT (Float.mul v (-1.0) )))
        |_->terms,output
    
    in
    let rec print_exp exp = match exp with 
        |ADD(left,right)-> 
            print_exp left;

            Printf.printf "+ ";
            print_exp right; 
            
        |MULTI(left,right)-> 

            print_exp left;
            print_exp right;
        |PIPE(left,right)-> 

            Printf.printf " | ";
            print_exp left;
            print_exp right;
        |COMP(left,right)-> 

            Printf.printf " >= ";
            print_exp left;
            print_exp right;
        |CAT(left,right)-> 

            Printf.printf " cat ";
            print_exp left;
            print_exp right;
        |PARAM(left,right)-> 
            print_exp left;
            print_exp right;
        |INT(v)-> 
            Printf.printf "%i " v 
        |FLOAT(v)-> 
            Printf.printf "%f " v 
        |IDEN(v) ->
            Printf.printf "%s " v
        |STR(v) ->
            Printf.printf "%s " v
        |CHAR(v) ->
            Printf.printf "%c" v
        |EMPTY -> ()

    in
   (* let rec closure terms prev = match terms,prev with 
        |IDEN(v)::xs,IDEN(u) ->   ""
    in *)

    let (_,y)= expression terminals_list EMPTY   
    in 
    print_exp y 

let () =
     read_j_file "file" |> get_raw_lexes  
        |> extract_lexems |> parser_f 
        
