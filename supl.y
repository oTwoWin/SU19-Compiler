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
  BPrecord *bp;
  EOpcode opc;
}

%code {
  Stack   *stack = NULL;
  Symtab *symtab = NULL;
  Funclist  *fnl = NULL;
  CodeBlock  *cb = NULL;

  char *fn_pfx   = NULL;
  EType rettype  = tVoid;

  int requiresCheck = 0;
}

%start program

%token LONG VOID
%token INTVAL
%token IDENT
%token IF WHILE ELSE
%token CALL RETURN READ WRITE PRINT
%token STRING QUOTES
%token EQ LE LT


%type<lval> INTVAL paramlist number
%type<str>  ident IDENT string STRING
%type<t>    type
%type<idl>  identl vardecl paramdecl
%type<bp>   IF WHILE
%type<opc>  condition
 

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

vardecl     : type identl                     {	if($type == tVoid){
							                        char *error = NULL;
							                        asprintf(&error, "Void type for declaration of variable '%s'.", $identl->id);
							                        yyerror(error);
                                                	YYABORT;
						                        }
                                                IDlist *l = $identl;
                                                while (l) {
                                                  if (insert_symbol(symtab, l->id, $type) == NULL) {
                                                    char *error = NULL;
                                                    asprintf(&error, "Duplicated identifier '%s'.", l->id);
                                                    yyerror(error);
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

read        : READ ident ';'                { Symbol *s = find_symbol(symtab, $ident, sGlobal);
                                                if (s == NULL) { 
					                              	char *error = NULL;
								                    asprintf(&error, "Variable '%s' hasn't been declared.", $ident);
								                    yyerror(error);
								                    YYABORT;
									            }
                                                add_op(cb, opRead, (void*)s); 
                                            }
            ;

write       : WRITE expression ';'          { add_op(cb, opWrite, NULL); }
            ;

print       : PRINT string ';'              {
                                                add_op(cb, opPrint, $string);
                                            }
            ;

expression  : number                    { add_op(cb, opPush, (void*)$number); }
            | ident						{ Symbol *s = find_symbol(symtab, $ident, sGlobal);
                                            if (s == NULL) { 
					                          	char *error = NULL;
								                asprintf(&error, "Variable '%s' hasn't been declared.", $ident);
								                yyerror(error);
								                YYABORT;
									        }
                                            add_op(cb, opLoad, s); 
							      	    }			
            | expression '+' expression  { add_op(cb, opAdd, NULL); } 
            | expression '-' expression  { add_op(cb, opSub, NULL); } 
            | expression '*' expression  { add_op(cb, opMul, NULL); } 
            | expression '/' expression  { add_op(cb, opDiv, NULL); } 
            | expression '%' expression  { add_op(cb, opMod, NULL); } 
            | expression '^' expression  { add_op(cb, opPow, NULL); } 
            | '(' expression ')'
            | call 
            ;

number      : INTVAL    
            ;

string      : STRING                { $$ = $STRING; }
            ;

fundecl     : type ident           { Funclist *f = find_func(fnl, $ident);
                                     if(f != NULL) {
					printf("Function '%s' already exists.\n	", $ident);
                                        //yyerror("Function '%s' already exists.", f->id);
                                        YYABORT;
                                     }
                                     cb = init_codeblock($ident);
                                     stack = init_stack(stack); symtab = init_symtab(stack, symtab);    
                                   }
              '(' paramdecl ')'    {
                                        int narg = 0;
                                        IDlist *l = $paramdecl;
                                        while (l){
                                            narg++;
                                            l = l->next;
                                            add_op(cb, opStore, find_symbol(symtab,l->id, sGlobal));
                                        }
                                        Funclist *f = (Funclist*)calloc(1, sizeof(Funclist));
					                    f->rettype = $type;
                                        f->id = $ident;
                                        f->narg = narg;
                                        f->next = fnl;
                                        fnl = f;
                                        rettype = $type;
					                    printf("function %s added\n", $ident);
                                        delete_idlist($paramdecl);
                                   }
                stmtblock          {
                                        dump_codeblock(cb);
                                        save_codeblock(cb, fn_pfx);
                                        Stack *pstck = stack; stack = stack->uplink; delete_stack(pstck);
                                        Symtab *pst = symtab; symtab = symtab->parent; delete_symtab(pst);
                                   }
	    ;

paramdecl   : %empty { $$ = NULL; }
	        | vardecl
	        ;

stmtblock   : '{'           { symtab = init_symtab(stack, symtab); }
                 stmts 
                            { Symtab *pst = symtab; symtab = symtab->parent; }
               '}'
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
	        | %empty
            ;

assign      : ident '=' expression ';'		{ const char* i = $ident;
                                             Symbol* sym = find_symbol(symtab, i, sGlobal);
									            if(sym == NULL){
				                                  	char *error = NULL;
							                        asprintf(&error, "Variable '%s' hasn't been declared.", i);
							                        yyerror(error);
							                        YYABORT;
									            }
                                                add_op(cb, opStore, sym);
							      	        }
            ;

if          : IF '(' condition ')' {    $IF = (BPrecord*)calloc(1, sizeof(BPrecord));
                                        Operation *tb = add_op(cb, $condition, NULL);
                                        Operation *fb = add_op(cb, opJump, NULL);
                                        $IF->ttrue = add_backpatch($IF->ttrue, tb);
                                        $IF->tfalse = add_backpatch($IF->tfalse, fb);
                                        pending_backpatch(cb, $IF->ttrue);
                                    }
             stmtblock              {   Operation *next = add_op(cb, opJump, NULL);
                                        $IF->end = add_backpatch($IF->end, next);
                                        pending_backpatch(cb, $IF->tfalse);
                                    }
              else                  {   pending_backpatch(cb, $IF->end);
                                    }
            ;

else        : %empty
            | ELSE stmtblock
            ;

while       : WHILE                 { $WHILE = (BPrecord*)calloc(1, sizeof(BPrecord));
                                      $WHILE->pos = cb->nops;                                    
                                    } 
                 '(' condition ')' 
                                    {   Operation *tb = add_op(cb, $condition, NULL);
                                        Operation *fb = add_op(cb, opJump, NULL);
                                        $WHILE->ttrue = add_backpatch($WHILE->ttrue, tb);
                                        $WHILE->tfalse = add_backpatch($WHILE->tfalse, fb);
                                        pending_backpatch(cb, $WHILE->ttrue);
                                    }
                    stmtblock       {   add_op(cb, opJump,$WHILE->pos);
                                        pending_backpatch(cb, $WHILE->tfalse);
                                    }
            ;

call        : ident '(' paramlist ')' 	{	//check existence of function
					        Funclist* fl = find_func(fnl, $ident);
                            if(fl == NULL) {
                                yyerror("Call to an undeclared function.");
				                YYABORT;   
                            }
					        if(fl->narg != $paramlist){
						        char *error = NULL;
						        asprintf(&error, "Bad number of arguments for call to '%s'.", $ident);
		                        yyerror(error);
		                        YYABORT;
					        }
                            add_op(cb, opCall, $ident);
				     	}
            ;

paramlist   : %empty      { $$ = 0;  }
      	    | expression    { $$ = 1;  }
      	    | paramlist ',' expression  { $$ = $1+1;  }
      	    ;

return      : RETURN expression ';' 	{ if(rettype == tVoid){
					                       	yyerror("Void expected");
					                            YYABORT;
					                      }
                                          add_op(cb, opReturn, NULL);
					                    }
            | RETURN ';'		{ if(rettype != tVoid){
						            yyerror("Expression expected");
					                    YYABORT;
					              }
                                  add_op(cb, opReturn, NULL);
					            }
            ;

condition   : expression EQ expression { $$ = opJeq; }
            | expression LE expression { $$ = opJle; }
            | expression LT expression { $$ = opJlt; }
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
  /*while(fnl != NULL){
	printf("%s\n",fnl->id);
	fnl = fnl->next;
  }*/
  return EXIT_FAILURE;
}

