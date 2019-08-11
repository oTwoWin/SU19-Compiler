/*------------------------------------------------------------------------------

  HEIG-Vd - CoE@SNU              Summer University             August 5-16, 2019

  The Art of Compiler Construction


  suPL support routines for the parser and the VM


  @brief snPL support routines for parser/suVM
  @author Bernhard Egger <bernhard@csap.snu.ac.kr>
  @section changelog Change Log
  2016/07/10 bernhard created
  2019/08/01 bernhard adapted to SU19

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

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "supllib.h"

#define DBG_SYMTAB  0
#define DBG_STACK   0
#define DBG_CODEBLK 0

#define FALSE 0

//------------------------------------------------------------------------------
// various lists
//

void delete_idlist(IDlist *l)
{
  IDlist *p;
  while ((p = l) != NULL) {
    l = l->next;
    free(p);
  }
}

Funclist* find_func(Funclist *fl, const char *id)
{
  Funclist *f = fl;
  while ((f != NULL) && (strcmp(id, f->id) != 0)) f = f->next;
  return f;
}

void delete_funclist(Funclist *l)
{
  Funclist *p;
  while ((p = l) != NULL) {
    l = l->next;
    free(p);
  }
}

//------------------------------------------------------------------------------
// string functions
//
char* escape(const char *s)
{
  // compute length of escaped string
  int l = 0;
  const char *p = s;
  while (*p != '\0') {
    if (*p++ < ' ') l++;
    l++;
  }
  l++;  // terminating \0

  // get memory & copy
  char *e = (char*)malloc(l);
  char *ep = e;

  p = s;
  while (*p != '\0') {
    char c = *p++;
    if ((c >= ' ') && (c != '\"')) *ep++ = c;
    else {
      *ep++ = '\\';
      switch (c) {
        case '\n': *ep++ = 'n'; break;
        case '\t': *ep++ = 't'; break;
        case '\"': *ep++ = '"'; break;
        default:   *ep++ = '?';
      }
    }
  }
  *ep = '\0';

  return e;
}


//------------------------------------------------------------------------------
// stack management
//

Stack* init_stack(Stack *uplink)
{
  Stack *s = (Stack*)calloc(1, sizeof(Stack));
  s->elem = 16;
  s->uplink = uplink;
  resize_stack(s);

  return s;
}

void delete_stack(Stack *s)
{
  if (s->elem > 0) free(s->values);
  free(s);
}

void resize_stack(Stack *s)
{
  s->values = (long*)realloc(s->values, s->elem*sizeof(long));
}

int num_values(Stack *s)
{
  return s->top;
}

long push_value(Stack *s, long val)
{
  if (s->top == s->elem) {
    s->elem += 16;
    resize_stack(s);
  }

  long offset = ((long)(&s->values[s->top] - &s->values[0])) * sizeof(long);
  s->values[s->top++] = val;

  return offset;
}

long pop_value(Stack *s)
{
  if (s->top == 0) {
    printf("ERROR: pop_value called, but stack is empty!\n");
    exit(EXIT_FAILURE);
  }

  return s->values[--s->top];
}

void store_value(Stack *s, long offset, long val)
{
  long idx = offset / sizeof(long);

  // increase size of stack if necessary
  if (idx >= s->elem) {
    s->elem = (idx+1 + 15) / 16 * 16;
    resize_stack(s);
  }

  // adjust top of stack if needed
  if (idx >= s->top) s->top = idx+1;

  // store value
  s->values[idx] = val;
}

long load_value(Stack *s, long offset)
{
  long idx = offset / sizeof(long);

  // increase size of stack if necessary
  if (idx >= s->elem) {
    s->elem = (idx+1 + 15) / 16 * 16;
    resize_stack(s);
  }

  // adjust top of stack if needed
  if (idx >= s->top) s->top = idx+1;

  // load value
  return s->values[idx];
}

void dump_stack(Stack *s)
{
  int idx;

  for (idx=0; idx<s->top; idx++) {
    printf("  %4x: %16lx\n", idx, s->values[idx]);
  }
}

//------------------------------------------------------------------------------
// symbol management
//

Symtab* init_symtab(Stack *stack, Symtab *parent)
{
  if (DBG_SYMTAB) printf("creating new symbol table with parent %p...\n", parent);
  Symtab *st = (Symtab*)calloc(1, sizeof(Symtab));
  st->stack = stack;
  st->parent = parent;

  return st;
}

void delete_symtab(Symtab *st)
{
  if (DBG_SYMTAB) printf("deleting symbol table %p including all symbols...\n", st);
  Symbol *s;
  while ((s = st->s) != NULL) {
    st->s = s->next;
    free(s);
  }
  free(st);
}

Symbol* find_symbol(Symtab *st, const char *id, EScope scope)
{
  if (DBG_SYMTAB) printf("%p: finding symbol with id '%s'...\n", st, id);

  // find symbol in local symbol table
  Symbol *s = st->s;
  while (s != NULL) {
    if (strcmp(s->id, id) == 0) break;
    s = s->next;
  }

  if ((s != NULL) || (scope == sLocal) || (st->parent == NULL)) return s;

  // find symbol in parent symbol table
  return find_symbol(st->parent, id, scope);
}

Symbol* insert_symbol(Symtab *st, const char *id, EType type)
{
  if (DBG_SYMTAB) printf("%p: inserting symbol with id '%s'...\n", st, id);

  // check if a symbol with the same ID already exists in the local symbol table
  Symbol *s = find_symbol(st, id, sLocal);
  if (s != NULL) return NULL;

  // install symbol
  s = (Symbol*)calloc(1, sizeof(Symbol));
  s->id = strdup(id);
  s->type = type;
  s->offset = push_value(st->stack, 0);
  s->next = st->s;
  s->symtab = st;
  st->s = s;
  st->nsym++;

  if (DBG_SYMTAB) printf("  '%s' is located at offset %x\n", s->id, s->offset);

  return s;
}


//------------------------------------------------------------------------------
// string table management
//

#define STRTAB_MAGIC 0x20130413

typedef struct __strtab {
  char *data;                   ///< string data

  int pos;                      ///< current position in data array
  int elem;                     ///< max. number of characters (size of data)
} Strtab;

void resize_strtab(Strtab *st);

Strtab* init_strtab(void)
{
  Strtab *st = (Strtab*)calloc(1, sizeof(Strtab));
  st->elem = 128;

  resize_strtab(st);

  return st;
}

void delete_strtab(Strtab *st)
{
  if (st->elem > 0) free(st->data);
  free(st);
}

void resize_strtab(Strtab *st)
{
  st->data = (char*)realloc(st->data, st->elem*sizeof(char));
}

int add_string(Strtab *st, const char *s)
{
  int l = strlen(s)+1;
  int ofs = st->pos;

  while (st->pos + l >= st->elem) {
    st->elem += 128;
    resize_strtab(st);
  }

  strcpy(&st->data[st->pos], s);
  st->pos += l;

  return ofs;
}

const char* get_string(Strtab *st, int ofs)
{
  if (ofs < st->pos) return &st->data[ofs];
  else return NULL;
}

void dump_strtab(Strtab *st)
{
  printf("string table:\n");
  int p = 0;
  while (p < st->pos) {
    char *str = NULL, *estr = NULL;

    // this will print up to the next \0; increase position by that much
    // plus skip the terminating \0
    p += asprintf(&str, "%s", &st->data[p]) + 1;

    estr = escape(str);
    printf("  %s\n", estr);

    free(str);
    free(estr);
  }
}

void save_strtab(Strtab *st, FILE *f)
{
  unsigned int v;

  // header: Strtab magic marker & number of characters
  v = STRTAB_MAGIC;
  fwrite(&v, sizeof(v), 1, f);
  v = st->pos;
  fwrite(&v, sizeof(v), 1, f);

  // write data
  fwrite(st->data, sizeof(char), st->pos, f);
}

Strtab* load_strtab(FILE *f)
{
  unsigned int v;

  // header: check magic and get number of characters
  fread(&v, sizeof(v), 1, f);
  if (v != STRTAB_MAGIC) return NULL;

  fread(&v, sizeof(v), 1, f);

  // initialize new string table
  Strtab *st = init_strtab();
  st->elem = v;
  resize_strtab(st);

  // read data
  fread(st->data, sizeof(char), v, f);
  st->pos = v;

  return st;
}


//------------------------------------------------------------------------------
// code block management
//

CodeBlock* init_codeblock(const char *label)
{
  CodeBlock *cb = (CodeBlock*)calloc(1, sizeof(CodeBlock));
  cb->label = label;
  cb->elem = 64;
  resize_codeblock(cb);

  return cb;
}

void delete_codeblock(CodeBlock *cb)
{
  if (cb->elem > 0) free(cb->code);
  free(cb);
}

void resize_codeblock(CodeBlock *cb)
{
  cb->code = (Operation*)realloc(cb->code, cb->elem*sizeof(Operation));
}

Operation* add_op(CodeBlock *cb, EOpcode opc, void *opa)
{
  if (cb->nops == cb->elem) {
    cb->elem += 64;
    resize_codeblock(cb);
  }

  Operation *op = &cb->code[cb->nops++];
  op->id = cb->nops-1;
  op->bin = 0;
  op->opc = opc;
  op->operand = opa;

  // process pending backpatches for this operation
  while (cb->bp_pending != NULL) {
    if (DBG_CODEBLK) {
      printf("  processing backpatch for op %d (to op %d)...\n", 
             cb->bp_pending->op->id, op->id);
    }
    cb->bp_pending->op->operand = (void*)(long int)op->id;
    cb->bp_pending = cb->bp_pending->next;
  }

  return op;
}

Operation* get_op(CodeBlock *cb, int idx)
{
  if (idx < cb->nops) return &cb->code[idx];
  else return NULL;
}

Operation* find_op(CodeBlock *cb, int id)
{
  int pos = 0;
  while (pos < cb->nops) {
    if (cb->code[pos].id == id) return &cb->code[pos];
    pos++;
  }
  return NULL;
}

void pending_backpatch(CodeBlock *cb, Oplist *bpl)
{
  // inserts bpl list at the head of the pending list
  Oplist *p = NULL, *c = bpl;
  while (c != NULL) { p = c; c = c->next; }

  p->next = cb->bp_pending;
  cb->bp_pending = bpl;
}

Oplist* add_backpatch(Oplist *list, Operation *op)
{
  Oplist *l = (Oplist*)malloc(sizeof(Oplist));
  l->op = op;
  l->next = list;

  return l;
}

void delete_backpatchlist(BPrecord *bpr)
{
  Oplist *l, *n;
  l = bpr->ttrue;  while (l) { n = l->next; free(l); l = n; }
  l = bpr->tfalse; while (l) { n = l->next; free(l); l = n; }
  l = bpr->end;    while (l) { n = l->next; free(l); l = n; }
  free(bpr);
}

char opc_str[opMax][12] = {
  "opHalt",
  "opAdd", "opSub", "opMul", "opDiv", "opMod", "opPow",
  "opPush", "opPop",
  "opLoad", "opStore",
  "opCall", "opReturn",
  "opJump", "opJeg", "opJle", "opJlt",
  "opRead", "opWrite", "opPrint",
};

void dump_operation(Operation *op)
{
  Symbol *s;
  char   bp[4];
  char   *e;

  printf(" %3d: ", op->id);
  printf(" %-10s ", opc_str[op->opc]);

  union { void *p; long l; double d; } operand;
  operand.p = op->operand;

  long oplong = operand.l;

  switch (op->opc) {
    case opPush:
      printf("%ld ", oplong);
      break;

    case opLoad:
    case opStore:
    case opRead:
      if (op->bin) {
        if (oplong & (1L << 63)) strcpy(bp, "gp");
        else strcpy(bp, "sp");

        printf("%s+%ld", bp, oplong & 0x7fffffffffffffffL);
      } else {
        s = (Symbol*)operand.p;
        assert(s != NULL);
        if (s->symtab->stack->uplink == NULL) strcpy(bp, "gp");
        else strcpy(bp, "sp");

        printf("%-4s at %s+%d", s->id, bp, s->offset);
      }
      break;

    case opCall:
      assert(operand.p != NULL);
      printf("%s ", (char*)operand.p);
      break;

    case opJump:
    case opJeq:
    case opJle:
    case opJlt:
      if (op->bin) {
        printf("%ld ", oplong);
      } else {
        if (oplong != OPID_INVALID) {
          printf("%ld ", oplong);
        } else {
          printf("%s ", "???");
        }
      }
      break;

    case opPrint:
      assert(operand.p != NULL);
      e = escape((char*)operand.p);
      printf("%s ", e);
      free(e);
      break;

    default:
      printf("%s ", " ");
  }

  printf("\n");
}

void dump_codeblock(CodeBlock *cb)
{
  printf("Codeblock '%s'\n", cb->label);
  printf("  #operations: %d\n", cb->nops);

  int i = 0;
  while (i < cb->nops) {
    printf(" %3d: ", i);
    dump_operation(&cb->code[i++]);
  }
}

void save_operation(CodeBlock *cb, FILE *f, Operation *op, Strtab *st)
{
  Symbol *s;
  Operation *t;

  unsigned int opcode = op->opc;
  unsigned long operand = 0;

  switch (op->opc) {
    case opPush:
      // operand: 64-bit constant
      operand = (long int)op->operand;
      break;

    case opLoad:
    case opStore:
    case opRead:
      s = (Symbol*)op->operand;
      assert(s != NULL);

      // operand:
      //   bit  63: global/local flag
      //   bits 62-0: offset
      operand = (s->offset & 0x7fffffffffffffffL);
      if (s->symtab->stack->uplink == NULL) operand |= 1L << 63;

      break;

    case opCall:
      // operand is an offset into the string table
      operand = add_string(st, (char*)op->operand);
      break;

    case opJump:
    case opJeq:
    case opJle:
    case opJlt:
      // operand: relative offset in code array
      t = find_op(cb, (long int)op->operand);
      assert(t != NULL);
      operand = t - op;
      break;

    case opPrint:
      // operand is an offset into the string table
      operand = add_string(st, (char*)op->operand);
      break;
  }

  // write opcode & operand
  fwrite(&opcode, sizeof(opcode), 1, f);
  fwrite(&operand, sizeof(operand), 1, f);
}

void save_codeblock(CodeBlock *cb, const char *fn_prefix)
{
  FILE *f;
  unsigned int v;
  char *fn = NULL;

  // open file for writing
  if (strcmp(cb->label, "") == 0) asprintf(&fn, "%s.sux", fn_prefix);
  else asprintf(&fn, "%s.%s.sux", fn_prefix, cb->label);
  f = fopen(fn, "w");
  assert(f != NULL);

  Strtab *strtab = init_strtab();

  // file header: magic + number of operations
  v = SUX_MAGIC;
  fwrite(&v, sizeof(v), 1, f);
  v = cb->nops;
  fwrite(&v, sizeof(v), 1, f);

  // write operations (12 bytes each)
  int i = 0;
  while (i < cb->nops) {
    save_operation(cb, f, &cb->code[i++], strtab);
  }

  // save string table
  save_strtab(strtab, f);

  // cleanup
  delete_strtab(strtab);
  fclose(f);
  free(fn);
}

Operation* load_operation(FILE *f, Strtab *st)
{
  unsigned int opcode;
  unsigned long operand;

  fread(&opcode, sizeof(opcode), 1, f);
  fread(&operand, sizeof(operand), 1, f);

  Operation *op = (Operation*)calloc(1, sizeof(Operation));
  op->opc = opcode;
  op->bin = 1;

  switch (op->opc) {
    case opCall:
    case opPrint:
      // operand is an offset into the string table
      op->operand = (void*)strdup(get_string(st, operand));
      break;

    default:
      op->operand = (void*)operand;
  }

  return op;
}

CodeBlock* load_codeblock(const char *label, const char *fn_prefix)
{
  FILE *f;
  unsigned int v, nops;
  char *fn = NULL;

  // open file for reading
  if (strcmp(label, "") == 0) asprintf(&fn, "%s.sux", fn_prefix);
  else asprintf(&fn, "%s.%s.sux", fn_prefix, label);
  f = fopen(fn, "r");
  if (f == NULL) return NULL;

  // file header: check magic and get number of operations
  fread(&v, sizeof(v), 1, f);
  if (v != SUX_MAGIC) return NULL;
  fread(&nops, sizeof(v), 1, f);

  // first read string table
  // string table is located after code
  fseek(f, nops*12, SEEK_CUR);

  Strtab *strtab = load_strtab(f);
  if (strtab == NULL) return NULL;

  // initialize & load codeblock
  CodeBlock *cb = init_codeblock(label);
  cb->elem = nops;
  resize_codeblock(cb);

  fseek(f, 8, SEEK_SET);  // reset file pos to beginning of code
  while (cb->nops < nops) {
    Operation *op = load_operation(f, strtab);
    op->id = cb->nops;
    cb->code[cb->nops++] = *op;
    free(op);
  }

  // cleanup
  fclose(f);
  delete_strtab(strtab);

  return cb;
}

