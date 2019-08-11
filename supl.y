/*------------------------------------------------------------------------------

  HEIG-Vd - CoE@SNU              Summer University             August 5-16, 2019

  The Art of Compiler Construction


  suPL parser skeleton


  @brief snPL parser
  @author Bernhard Egger <bernhard@csap.snu.ac.kr>
  @section changelog Change Log
  2016/07/10 bernhard created
  2019/08/01 bernhard adapted to long ints

  @section license_section License
  Copyright (c) 2016-2019, Computer Systems and Platforms Laboratory, SNU
  All rights reserved.

  Redistribution and use in source and binary forms,  with or without modifi-
  cation, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,  BUT NOT LIMITED TO,  THE
  IMPLIED WARRANTIES OF MERCHANTABILITY  AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDER  OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT,  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSE-
  QUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF  SUBSTITUTE
  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN  CONTRACT, STRICT
  LIABILITY, OR TORT  (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
  DAMAGE.

------------------------------------------------------------------------------*/


%locations

%code top{
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>

#define YYDEBUG 1

extern char *yytext;
}

%code requires {
#include "supllib.h"
}

%union {
  long int lval;
  char     *str;
  IDlist   *idl;
  EType    t;
}

%code {
  Stack   *stack = NULL;
  Symtab *symtab = NULL;
  Funclist  *fnl = NULL;
  CodeBlock  *cb = NULL;

  char *fn_pfx   = NULL;
  EType rettype  = tVoid;
}

%start program

%token LONG VOID
%token INTVAL
%token IDENT
%token IF WHILE ELSE
%token CALL RETURN READ WRITE PRINT
%token STRING QUOTES
%token COMPARATOR
%token ASSIGN
%token OPERATOR


%type<lval> INTVAL
%type<str>  ident IDENT
%type<t>    type
%type<idl>  identl vardecl

%%

program     :                                 { stack = init_stack(NULL); symtab = init_symtab(stack, NULL); 
                                                fnl = NULL;
                                              }
              decll                           { cb = init_codeblock("");
                                                stack = init_stack(stack); symtab = init_symtab(stack, symtab);
                                                rettype = tVoid;
                                              }
              stmtblock                       { add_op(cb, opHalt, NULL);
                                                dump_codeblock(cb); save_codeblock(cb, fn_pfx);
                                                Stack *pstck = stack; stack = stack->uplink; delete_stack(pstck);
                                                Symtab *pst = symtab; symtab = symtab->parent; delete_symtab(pst);

                                                delete_stack(stack); delete_symtab(symtab);
                                                delete_funclist(fnl); delete_codeblock(cb);
                                              }
            ;

decll       : %empty
            | decll vardecl ';'               { delete_idlist($vardecl); }
            | decll fundecl
            ;

vardecl     : type identl                     {
                                                IDlist *l = $identl;
                                                while (l) {
                                                  if (insert_symbol(symtab, l->id, $type) == NULL) {
                                                    char *error = NULL;
                                                    asprintf(&error, "Duplicated identifier '%s'.", l->id);
                                                    yyerror(error);
                                                    free(error);
                                                    YYABORT;
                                                  }
                                                  l = l->next;
                                                }
                                                $$ = $identl;
                                              }
            ;

type        : LONG                            { $$ = tLong; }
            | VOID                            { $$ = tVoid; }
            ;

identl      : ident                           { $$ = (IDlist*)calloc(1, sizeof(IDlist)); $$->id = $ident; }
            | identl ',' ident                { $$ = (IDlist*)calloc(1, sizeof(IDlist)); $$->id = $ident; $$->next = $1; }
            ;

ident       : IDENT
            ;

stmtblock   : '{' stmts '}'
            ;

read        : READ ident ';'
            ;

write       : WRITE expression ';'
            ;

print       : PRINT string ';'
            ;

expressions : expression 
            | expressions ',' expression
            ;

expression  : number 
            | ident
            | expression OPERATOR expression
            | '(' expression ')'
            | call 
            ;

number      : INTVAL    
            ;

string      : STRING
            ;

fundecl     : type ident '(' vardecl ')' stmtblock
            | type ident '(' ')' stmtblock
            ;

stmtblock   : '{' stmts '}'
            ;

stmt        : vardecl ';'
            | assign
            | if
            | while
            | call ';'
            | return
            | read
            | write
            | print
            ;

stmts       : stmt
            | stmts stmt
            ;

assign      : ident '=' expression ';'
            ;

if          : IF '(' condition ')' stmtblock 
            | IF '(' condition ')' stmtblock ELSE stmtblock
            ;

while       : WHILE '(' condition ')' stmtblock
            ;

call        : ident '(' expressions ')'
            | ident '(' ')'
            ;

return      : RETURN expression ';'
            | RETURN ';'
            ;

condition   : expression COMPARATOR expression
            ;

%%

int main(int argc, char *argv[])
{
  extern FILE *yyin;
  argv++; argc--;

  while (argc > 0) {
    // prepare filename prefix (cut off extension)
    fn_pfx = strdup(argv[0]);
    char *dot = strrchr(fn_pfx, '.');
    if (dot != NULL) *dot = '\0';

    // open source file
    yyin = fopen(argv[0], "r");
    yydebug = 0;

    // parse
    yyparse();

    // next input
    free(fn_pfx);
    argv++; argc--;
  }

  return EXIT_SUCCESS;
}

int yyerror(const char *msg)
{
  printf("Parse error at %d:%d: %s\n", yylloc.first_line, yylloc.first_column, msg);
  return EXIT_FAILURE;
}

