#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>



#define MAX_ARGS 5


struct Term * create_variable(int i);
struct Term * create_functor(char* name, int arity);

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
    new_functor->name = outer->functor->name; //pls no memeory leak
    new_functor->arity = outer->functor->arity;
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

struct Term* parse_with_index(char * s, int start, int* end) {
  if (!s[start] || s[start] == ')') {
    return NULL;
  }
  if (isdigit(s[start])) {
    char name[100];
    int i;
    for (i = 0; s[start + i] && isdigit(s[start + i]); i++) {
      name[i] = s[start + i];
    }
    name[i] = '\0';
    *end = start + i;
    char * end2;
    int new_long = strtol(name, &end2, 10);
    return create_variable(new_long);
    
  } else {
    char name[100];
    int i;
    for (i = 0; s[start + i] && s[start + i] != '('; i++) {
      name[i] = s[start + i];
    }
    name[i] = '\0';
    assert(s[start + i] == '(');
    i++;
    // it's a functor
    int length = strlen(name);
    char *new_string = malloc(length + 1);
    strcpy(new_string, &name[0]);
    struct Term* new = create_functor(new_string,0);
    int arity = 0;
    start = start + i;
    while (1) {
      if (s[start] == ')') {
        break;
      }
      struct Term* child = parse_with_index(s, start, end);
      new->functor->args[arity] = child;
      arity++;
      start = *end;
      if (s[start] == ',') {
        start++;
      }
    }
    start++;
    *end = start;
    new->functor->arity = arity;
    return new;
  }
  return 0;


}

struct Term* parse(char * s) {

  int end;
  return parse_with_index(s, 0, &end);
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

  printf("%s\n", s);


  char* a = "12";
  struct Term *a_term = parse(a);
  printf("%s\n", to_string(a_term));

  
  printf("%s\n", to_string(parse("hi(1)")));
  printf("%s\n", to_string(parse("hi(a(1),2)")));
  printf("%s\n", to_string(parse("hi(a(1),hello(3))")));


  struct Term* sub_test = parse("hi(1)");
  struct Term* var = parse("2");
  struct Term* subbed = substitute(sub_test, 1, var);
  printf("%s\n", to_string(subbed));
  
}
