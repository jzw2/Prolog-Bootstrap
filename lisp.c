

#include <ctype.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
const int Lex_lparen = 1;
const int Lex_rparen = 2;
const int Lex_atom = 3;
struct Lex {
  int type;
  char *atom;
};

struct Lex* Lex_new(int type, char * atom) {
  struct Lex* ret = malloc(sizeof *ret);
  ret->type = type;
  ret->atom = atom;
  return ret;
}

struct LexArray {
  int length;
  struct Lex** data;
};

typedef union Exp {
  int type;
  struct Atom {
    int type;
    char* name;
  } atom;
  struct List {
    int type;

    union Exp* car;
    union Exp* cdr;
  } list;
} Exp;
const int Exp_Atom = 1;
const int Exp_List = 2;

struct Atom Atom_new(char* name) {
  struct Atom ret;
  ret.type = Exp_Atom;
  ret.name = name;
  return ret;
}

struct List List_new(union Exp* car, union Exp* cdr) {
  struct List ret;
  ret.type = Exp_List;
  ret.cdr = cdr;
  ret.car = car;
  return ret;
}


union Exp* parse_exp(struct LexArray* stream, int *index);
union Exp* parse_list(struct LexArray* stream, int *index);

union Exp* parse_exp(struct LexArray* stream, int *index) {
  struct Lex* current = stream->data[*index];
  union Exp* ret = malloc(sizeof *ret);
  if (current->type == Lex_atom) {
    ret->atom = Atom_new(current->atom);
    (*index)++;
  } else if (current->type == Lex_lparen) {
    (*index)++;
    ret->list = List_new(NULL, NULL);
    union Exp* current = ret;
    while(stream->data[*index]->type != Lex_rparen) {
      union Exp* inner = parse_exp(stream, index);
      current->list.car = inner;
      union Exp* new = malloc(sizeof *new);
      (new->list) = List_new(NULL, NULL);
      current->list.cdr = new;
      current = new;
    }
    (*index)++;
  }
  return ret;
  
}

struct LexArray* lex(char *string) {
  struct LexArray* ret = malloc(sizeof *ret);
  ret->length = 0;
  // this seems like enough lol
  ret->data = malloc((sizeof *(ret->data) * 10000));
  
  int length = strlen(string);
  int token_start = 0;
  int prev_non_atom = 1;
  for (int i = 0; i < length; i++) {
    if (isalnum(string[i])) {
      if (prev_non_atom) {
        token_start = i;
        prev_non_atom = 0;
      }
      continue;
    }

    if (!prev_non_atom) {
      prev_non_atom = 1;
      int length = i - token_start;
      char * token  =  malloc(length + 1);
     memcpy(token, &string[token_start], length);
     token[length] = '\0';
      ret->data[ret->length] = Lex_new(Lex_atom, token);
      ret->length++;
    }


    if (string[i] == '(') {
      prev_non_atom = 1;
      ret->data[ret->length] = Lex_new(Lex_lparen, NULL);
      ret->length++;
    } else if (string[i] == ')') {
      prev_non_atom = 1;
      ret->data[ret->length] = Lex_new(Lex_rparen, NULL);
      ret->length++;
    }
  }
  
  return ret;
}

void Exp_print(union Exp* exp) {
  int current = 0;
  if (exp->type == Exp_Atom) {
    printf("%s ", exp->atom.name);
  } else if (exp->type == Exp_List) {
    printf("(");
    for (union Exp* e = exp; e->list.cdr; e = e->list.cdr) {
      Exp_print(e->list.car);
    }
    printf(")");
  }
}


Exp* car(Exp* s) {
  if (s->list.type == Exp_Atom) {
    printf("Not a a list");
    exit(1);
  } 
  return s->list.car;
}

Exp* cdr(Exp* s) {
  if (s->list.type == Exp_Atom) {
    printf("Not a a list");
    exit(1);
    // do evaluation
  } 
  return s->list.cdr;
}

Exp* cons(Exp* s1, Exp* s2) {
  Exp* ret = malloc(sizeof *ret);
  ret->type = Exp_List;
  ret->list.car = s1;
  ret->list.cdr = s2;
  return ret;
}

int main() {
  char *program1 = "(hi (1 2 3 1) )";
  struct LexArray* program1_struct = lex(program1);

  int index = 0;
  union Exp* p1_parse = parse_exp(program1_struct, &index);

  
  char *program2 = "(second(struct))";
  struct LexArray* program2_struct = lex(program1);

  int index2 = 0;
  union Exp* p2_parse = parse_exp(program2_struct, &index2);
  printf("parsing\n");
  Exp_print(p1_parse);

  
  printf("\ntesting car p1\n");
  Exp_print(car(p1_parse));
  printf("\ntesting cdr p1\n");
  Exp_print(cdr(p1_parse));
  printf("\ntesting car(cdr(p1)) p1\n");
  Exp_print(car(cdr(p1_parse)));
  
  printf("\ntesting cons(p1, p2)\n");
  Exp_print(cons(p1_parse, p2_parse));
  printf("\ntesting car(car(p1)) p1\n");
  Exp_print(car(car(p1_parse)));
}
