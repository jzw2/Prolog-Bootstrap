#include <stdio.h>
#include <stdlib.h>



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

struct Term* unify(struct Term* a, struct Term* b) {

  return 0;
}

int main() {
  
}
