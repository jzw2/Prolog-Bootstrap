

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

Exp nil();
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
    *current = nil();
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
  
    if (!prev_non_atom) {
      int length1 = length - token_start;
      char * token  =  malloc(length1 + 1);
     memcpy(token, &string[token_start], length1);
     token[length1] = '\0';
      ret->data[ret->length] = Lex_new(Lex_atom, token);
      ret->length++;
    }
  return ret;
}

void Exp_print(union Exp* exp) {
  int current = 0;
  if (exp->type == Exp_Atom) {
    printf("%s ", exp->atom.name);
  } else if (exp->type == Exp_List) {
    printf("(");
    Exp* e = exp;
    for (e = exp; e->type == Exp_List && e->list.cdr; e = e->list.cdr) {
      Exp_print(e->list.car);
    }
    if (e->type != Exp_Atom) {
      // printf("Somehow ended up with a non atom type in the end");
      printf(". ");
      

    } else if (e->type == Exp_Atom && strcmp(e->atom.name, "nil")) {
      printf(". %s", e->atom.name);
    } else {
    }


    printf(")");
  }
}


Exp* car(Exp* s) {
  if (s->list.type == Exp_Atom) {
    printf("%s is not a list", s->atom.name);
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

Exp* eq(Exp* s1, Exp* s2) {
  if (s1->type == Exp_Atom && s2->type == Exp_Atom) {
    Exp* ret = malloc(sizeof *ret);
    if (!strcmp(s1->atom.name, s2->atom.name)) {
      ret->atom = Atom_new("true");
    } else {
      ret->atom = Atom_new("false");
    }
    return ret;
  }

  printf("Error in equality, don't compare lists");
  return NULL;
}


// the nil value
Exp nil() {
  Exp ret;
  ret.atom = Atom_new("nil");
  return ret;
}

Exp* get_value(Exp* atom, Exp* env) {
  if (atom->type == Exp_Atom && env->type== Exp_List) {
    Exp* current = env;
    while (current->type == Exp_List && current->list.car) {
      Exp* left = car(current->list.car);
      Exp* right = cdr(current->list.car);
      if (!strcmp(left->atom.name, atom->atom.name)) {
        return right;
      }
      current = current->list.cdr;
    }
  }
  return NULL;
}

Exp* add_mapping(Exp* env, Exp* key, Exp* value) {
  Exp* entry = cons(key, value);
  return cons(entry, env);
}

Exp* eval(Exp** env, Exp* exp);
// takes the start and keeps going linking up the new environments with each other
Exp* eval_list(Exp** env, Exp* list) {
  Exp* ret = malloc(sizeof(*ret));
  *ret = nil();
  for (Exp* current_list = list; current_list && current_list->type == Exp_List; current_list = cdr(current_list)) {
    Exp* evaluated = eval(env, car(current_list));
    Exp* new_list = malloc(sizeof(*new_list));
    new_list->list = List_new(evaluated, ret);
    ret = new_list;
  }
  // in reverse order
  return ret;
}

Exp* eval(Exp** env, Exp* exp) {
  if (!exp) {
    return NULL;
  }

  if (exp->type == Exp_Atom) {
    // hope it's not a number

    printf("Evaluating atom\n");
    Exp* value = get_value(exp, *env);

      if (value) {
        //found a vlue
        printf("found value\n");
        return value;
      } else {
        printf("evaluating %s to self\n", exp->atom.name);
        return exp;
      }
  } else if (exp->type == Exp_List) {
    printf("getting head\n");
    Exp_print(exp);
    printf("\n");
    Exp* head = eval(env, car(exp));
    printf("getting tail\n");
    Exp* tail = cdr(exp);

    if (head->type == Exp_Atom) {
      char* name = head->atom.name;
      if (!strcmp(name, "car")) {
        printf("It was car!!!\n");
        return car(eval(env, car(tail)));
      } else if (!strcmp(name, "cdr")) {
        return cdr(eval(env, car(tail)));
      } else if (!strcmp(name, "cons")) {
        printf("Found function cons!!!\n");
        return cons(eval(env, car(tail)), eval(env, car(cdr(tail))));
      } else if (!strcmp(name, "quote")) {
        return car(tail);
      } else if (!strcmp(name, "if")) {
        Exp* evaluated = eval(env, car(tail));
        if (evaluated->type == Exp_Atom && !strcmp(evaluated->atom.name, "nil")) {
          return eval(env, car(cdr(cdr(tail))));
        } else {
          return eval(env, car(cdr(tail)));
        }
      } else if (!strcmp(name, "def")) {
        Exp* var = car(tail);
        if (var->type == Exp_Atom) {
          Exp* rhs_val = eval(env, car(cdr(tail)));
          Exp* pair = malloc(sizeof (*pair));
          pair->list = List_new(var, rhs_val);

          Exp* old_entry = *env;
          *env = malloc(sizeof **env);
          (*env)->list = List_new(pair, old_entry);
          return eval(env, var);
        } else {
          printf("Can't assign a list\n");
        }
      } else if (!strcmp(name, "lambda")) {
        return exp;
      } else {
        // look it up in the environment
        // do application
        printf("some funciton appicatoin\n");
      }
    } else if (head->type == Exp_List){
      //do applicatoin
      // assuming its a lambda
      Exp* lambda_head = car(head);
      if (lambda_head->type != Exp_Atom || strcmp(lambda_head->atom.name, "lambda")) {
        printf("Very bad, evaluated list, but was not a lambda\n");
        exit(1);
      }

      Exp* lambda_tail = cdr(head);
      Exp* param = car(lambda_tail);
      Exp* param_tail = cdr(lambda_tail);
      Exp* body = car(param_tail);
      Exp* replaced = eval_list(env, tail);

      if (param->type == Exp_Atom ) {
        //have to reverse the list
        Exp* ret = malloc(sizeof *ret);
        *ret = nil();
        //modify in place
        for (Exp* current = replaced; current && current->type == Exp_List; ) {
          Exp* temp = cdr(current); //save the current
          current->list.cdr = ret; //modify it so that it is now on the the return stack
          ret = current;
          current = temp;
        }
        //leak the last nil
	printf("\n");
	Exp_print(ret);
	printf("\n");

	printf("Old Env\n");
	Exp_print(*env);
	printf("\n");
        //bind the list to the new variable
        Exp* new_env = add_mapping(*env, param, ret);
	printf("New Env\n");
	Exp_print(new_env);
	printf("\n");
        //lets hope they don't define somethig new here
        return eval(&new_env, body);
      } else if (param->type == Exp_List) {
        // pattern match on the list
        
        printf("Didn't implement pattern matching yet\n");
        exit(1);
      } else {
        printf("Very bad\n");
        exit(1);
        
      }
      //pattern match with the argument list


      printf("somehow ended up with a list\n");
      
    } else {
      //bad
      printf("very bad");
    }

  } else {
    //bad
      printf("very bad");
  }
  return NULL;
}

Exp* empty_list() {
  Exp* nilp = malloc(sizeof *nil);
  *nilp = nil();
  return nilp;
}

char* tests[] = {
  "(car (cons 1 2) )",
  "(cdr (cons 1 2))",
  "(cons 1 2)",
  "(cons x 2)",
  "x",
  "(quote (a b c))",
  "(if nil x 2)",
  "(if 5 x 2)",
  "(def y 2)",
  "y",
  "((lambda z z) 1)",
  "finaltest"
};

int main() {

  int index = 0;
  Exp x;
  x.atom =  Atom_new("x");

  Exp three;
  three.atom = Atom_new("3");

  Exp pair;
  pair.list = List_new(&x, &three);

  Exp env;
  env.list = List_new(&pair, empty_list());
  Exp *envp = &env;
  for (int i = 0; i < sizeof(tests) / sizeof (char *); i++) {
    index = 0;
    printf("\nTest #%d\n", i);
    printf("\nEnv is currently\n");
    Exp_print(envp);
    printf("\n");

    struct LexArray* program_struct = lex(tests[i]);
    union Exp* parse = parse_exp(program_struct, &index);
    Exp_print(eval(&envp, parse));
    printf("\n");
    printf("\nEnv is currently\n");
    Exp_print(envp);
    printf("\n");
  }
}
