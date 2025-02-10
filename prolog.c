#include <stdio.h>
#include <stdlib.h>
#include <string.h>



#define MAX_ARGS 5



struct Term {
  int variable;
  struct Functor* functor;
};


struct Functor {
  unsigned int arity;
  char* name;
  struct Term * args[MAX_ARGS];
};

struct Term* substitute(struct Term* outer, int variable, struct Term* inner) {
  if (outer->variable) {
    if (variable == outer->variable) {
      return inner;
    } else {
      struct Term* new = malloc(sizeof (*new));
      new->variable = variable;
      new->functor = NULL;
      return new;
    }
  } else if (outer->functor) {
    struct Term* new = malloc(sizeof (*new));
    new->variable = 0;
    new->functor = NULL;
    struct Functor* new_functor = malloc(sizeof (*new_functor));
    new->functor = new_functor;
    for (int i = 0; i < outer->functor->arity; i++) {
      struct Term* subbed = substitute(outer->functor->args[i], variable, inner);
      (new_functor->args[i]) = subbed;
      
    }
    return new;
  }
  return 0;
}

char* to_string(struct Term* term) {
  char* s = malloc(1000);
  if (term->variable) {
    sprintf(s, "%d", term->variable);
  } else if (term->functor) {
    sprintf(s, "%s(", term->functor->name);
    for (int i = 0; i < term->functor->arity; i++) {
      char* rec = to_string(term->functor->args[i]);
      strcat(s, rec);
      if (i != term->functor->arity - 1) {
        strcat(s, ", ");
      }
      free(rec);
    }
    strcat(s, ")");
  }
  return s;
}

struct Term * create_variable(int i) {
  
  struct Term *t1 = malloc(sizeof *t1);
  t1->variable = i;
  t1->functor = NULL;

  return t1;
}

struct Term * create_functor(char* name, int arity) {
  struct Term *t3 = malloc(sizeof *t3);
  t3->functor = malloc(sizeof (struct Functor));
  t3->functor->arity = arity;
  t3->functor->name = name;
  return t3;
  
}


struct Term* unify(struct Term* a, struct Term* b) {

  return 0;
}

int main() {
  struct Term *t1 = create_variable(1);
  struct Term *t2 = create_variable(2);
  struct Term *t3 = create_functor("my_name", 2);
  t3->functor->args[0] = t1;
  t3->functor->args[1] = t1;
  

  char* s = to_string(t3);

  printf("%s", s);
  
}
