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
%token CONSTANT STRING_LITERAL
%token PLUS MINUS
(*%token TIMES DIV*)
%token LPAREN RPAREN
%token SEMICOLON
%token EOF
%token EQEQ "=="
%token EQ "="
%token LT "<"
%token GT ">"
%token LBRACE "{"
%token RBRACE "}"
%token LBRACK "["
%token RBRACK "]"

%left PLUS MINUS        /* lowest precedence */
(*%left TIMES DIV         /* medium precedence */*)
%nonassoc UMINUS        /* highest precedence */

/* changed the type, because the script does not return one value, but all
 * results which are calculated in the file */
%start <int list> main

%%

/* the calculated results are accumalted in an OCaml int list */
main:
| stmt = statement EOF { [stmt] }
| stmt = statement m = main { stmt :: m}

/* expressions end with a semicolon, not with a newline character */
statement:
| e = expr SEMICOLON { e }

expr:
| i = INT
    { i }
| LPAREN e = expr RPAREN
    { e }
| e1 = expr PLUS e2 = expr
    { e1 + e2 }
| e1 = expr MINUS e2 = expr
    { e1 - e2 }
    (*
| e1 = expr TIMES e2 = expr
    { e1 * e2 }
| e1 = expr DIV e2 = expr
    { e1 / e2 }
    *)
| MINUS e = expr %prec UMINUS
    { - e }
