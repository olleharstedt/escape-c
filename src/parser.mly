/**************************************************************************/
/*                                                                        */
/*  Menhir                                                                */
/*                                                                        */
/*  François Pottier, INRIA Rocquencourt                                  */
/*  Yann Régis-Gianas, PPS, Université Paris Diderot                      */
/*                                                                        */
/*  Copyright 2005-2008 Institut National de Recherche en Informatique    */
/*  et en Automatique. All rights reserved. This file is distributed      */
/*  under the terms of the Q Public License version 1.0, with the change  */
/*  described in file LICENSE.                                            */
/*                                                                        */
/**************************************************************************/

%{
  open Ast
%}

%token <int> INT
%token <string> NAME
%token CONSTANT STRING_LITERAL
%token PLUS "+"
%token MINUS "-"
(*%token TIMES DIV*)
%token COLON ":"
%token SEMICOLON ";"
%token EOF
%token EQEQ "=="
%token EQ "="
%token LT "<"
%token GT ">"
%token LPAREN "("
%token RPAREN ")"
%token LBRACE "{"
%token RBRACE "}"
%token LBRACK "["
%token RBRACK "]"
%token RETURN "return"
%token NEW "new"
%token LET "let"
%token LOCAL "local"
%token REG "reg"
%token WITH "with"
%token STRUCT "struct"
%token FUNCTION "function"

%left PLUS MINUS        /* lowest precedence */
(*%left TIMES DIV         /* medium precedence */*)
%nonassoc UMINUS        /* highest precedence */

%type <declaration> decl

/* changed the type, because the script does not return one value, but all
 * results which are calculated in the file */
%start<Ast.program> program
%%

program:
| decl=list(decl); EOF {Declaration_list decl}

(*NAME int NAME main LPAREN RPAREN LBRACE RETURN INT0 SEMICOLON RBRACE*)
(*int main() { return 0; }*)
(*decl: t=NAME n=NAME LPAREN RPAREN LBRACE RBRACE {Function (n, [], [], Int)}*)
decl: "function" f=NAME "(" ")" ":" t=NAME {Function (f, [], [], type_of_string t)}
