%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

int yylex();
void yyerror(const char *s);

/* --- ESTRUTURAS DA AST --- */

typedef struct vars {
    char name[50];
    double valor;
    struct vars *prox;
} VARI;

VARI *l1 = NULL; /* Lista global de variáveis */

/* Função para inserir nova variável na lista */
VARI *ins(VARI *l, char n[]) {
    VARI *new = (VARI*)malloc(sizeof(VARI));
    strcpy(new->name, n);
    new->valor = 0;
    new->prox = l;
    return new;
}

/* Função para buscar uma variável na lista */
VARI *srch(VARI *l, char n[]) {
    VARI *aux = l;
    while(aux != NULL) {
        if(strcmp(n, aux->name) == 0)
            return aux;
        aux = aux->prox;
    }
    return NULL; 
}

/* Estruturas da Árvore (AST) */
typedef struct ast { /* Nó genérico para operadores (+, -, *) */
    int nodetype;
    struct ast *l;
    struct ast *r;
} Ast;

typedef struct numval { /* Nó para números */
    int nodetype;
    double number;
} Numval;

typedef struct varval { /* Nó para variáveis */
    int nodetype;
    char var[50];
} Varval;

/* IF/ELSE/WHILE  */
typedef struct flow {
    int nodetype;     /* Tipo I (if) ou W (while) */
    Ast *cond;        /* Condição */
    Ast *tl;          /* Then list (Bloco Verdadeiro) */
    Ast *el;          /* Else list (Bloco Falso) */
} Flow;

/* Protótipos */
Ast *newast(int nodetype, Ast *l, Ast *r);
Ast *newnum(double d);
Ast *newvar(char *s);
Ast *newcmp(int cmptype, Ast *l, Ast *r);
Ast *newflow(int nodetype, Ast *cond, Ast *tl, Ast *el); /* NOVO  */
double eval(Ast *a);

%}

%union {
    double flo;
    int fn;
    char str[50];
    Ast *a;
}

%token PROGRAMA
%token TYPE_INT TYPE_FLOAT TYPE_STRING
%token ESCREVA LEIA
%token SE SENAO ENQUANTO PARA
%token PTVIRG VIRGULA
%token ABRE_P FECHA_P ABRE_C FECHA_C
%token LITERAL_STR ATRIB

/* Tokens com valor */
%token <flo> NUM_INT NUM_FLOAT
%token <str> VAR
%token <fn> CMP

/* Precedência para resolver o "Dangling Else" (conflito do Se/Senao) */
%nonassoc IFX
%nonassoc SENAO

%left CMP %left CMP /* Menor precedência: compara depois de somar. Igual a C, Python e Java */
%left MAIS MENOS
%left VEZES DIV

%type <a> expressao atribuicao comando declaracao comandos fluxo

%%

/* --- GRAMÁTICA --- */

inicio:
    comandos { eval($1); }
    ;

comandos:
    /* vazio */ { $$ = NULL; }
    | comandos comando { 
        if ($1 != NULL)
            $$ = newast('L', $1, $2);
        else    
            $$ = $2;
    }
    ;
    
comando:
    ESCREVA ABRE_P expressao FECHA_P PTVIRG { $$ = newast('P', $3, NULL); }
    | declaracao { $$ = $1; }
    | atribuicao { $$ = $1; }
    | fluxo { $$ = $1; }
    ;

declaracao:
    TYPE_INT VAR PTVIRG    { $$ = newast('D', newvar($2), NULL); }
    | TYPE_FLOAT VAR PTVIRG  { $$ = newast('D', newvar($2), NULL); }
    | TYPE_STRING VAR PTVIRG { $$ = newast('D', newvar($2), NULL); }
    ;

atribuicao:
    VAR ATRIB expressao PTVIRG { 
        $$ = newast('=', newvar($1), $3); 
    }
    ;

/* SE e ENQUANTO  */
fluxo:
    SE ABRE_P expressao FECHA_P ABRE_C comandos FECHA_C %prec IFX {
        $$ = newflow('I', $3, $6, NULL); /* IF sem ELSE */
    }
    | SE ABRE_P expressao FECHA_P ABRE_C comandos FECHA_C SENAO ABRE_C comandos FECHA_C {
        $$ = newflow('I', $3, $6, $10);  /* IF com ELSE */
    }
    | ENQUANTO ABRE_P expressao FECHA_P ABRE_C comandos FECHA_C {
        $$ = newflow('W', $3, $6, NULL); /* WHILE */
    }
    ;

expressao:
    NUM_INT             { $$ = newnum($1); }
    | NUM_FLOAT         { $$ = newnum($1); }
    | VAR               { $$ = newvar($1); }
    | expressao MAIS expressao  { $$ = newast('+', $1, $3); }
    | expressao MENOS expressao { $$ = newast('-', $1, $3); }
    | expressao VEZES expressao { $$ = newast('*', $1, $3); }
    | expressao DIV expressao   { $$ = newast('/', $1, $3); }
    | expressao CMP expressao   { $$ = newcmp($2, $1, $3); } 
    | ABRE_P expressao FECHA_P  { $$ = $2; }
    ;

%%

/* --- FUNÇÕES AUXILIARES --- */

Ast *newast(int nodetype, Ast *l, Ast *r) {
    Ast *a = (Ast*) malloc(sizeof(Ast));
    if(!a) { printf("Sem memoria"); exit(0); }
    a->nodetype = nodetype;
    a->l = l;
    a->r = r;
    return a;
}

Ast *newnum(double d) {
    Numval *a = (Numval*) malloc(sizeof(Numval));
    if(!a) { printf("Sem memoria"); exit(0); }
    a->nodetype = 'K'; 
    a->number = d;
    return (Ast*)a;
}

Ast *newvar(char *s) {
    Varval *a = (Varval*) malloc(sizeof(Varval));
    if(!a) { printf("Sem memoria"); exit(0); }
    a->nodetype = 'N'; 
    strcpy(a->var, s);
    return (Ast*)a;
}

Ast *newcmp(int cmptype, Ast *l, Ast *r) {
    Ast *a = (Ast*) malloc(sizeof(Ast));
    if(!a) { printf("Sem memoria"); exit(0); }

    /* Para não colidir com letras ASCII, somamos o tipo com '0'. Ex: Se cmptype for 1 (>), nodetype vira '1' */
    /*Para pegar o tipe de teste, definido no arquivo.l e utilizar na função eval()*/
    a->nodetype = '0' + cmptype; 
    a->l = l;
    a->r = r;
    return a;
}

/* criar nós de fluxo  */
Ast *newflow(int nodetype, Ast *cond, Ast *tl, Ast *el) {
    Flow *a = (Flow*)malloc(sizeof(Flow));
    if(!a) { printf("Sem memoria"); exit(0); }
    a->nodetype = nodetype;
    a->cond = cond;
    a->tl = tl;
    a->el = el;
    return (Ast *)a;
}

double eval(Ast *a) {
    double v;
    VARI *aux;

    if(!a) return 0.0;

    switch(a->nodetype) {
        case 'K': v = ((Numval *)a)->number; break;

        case 'N': 
            aux = srch(l1, ((Varval *)a)->var);
            if(aux) v = aux->valor;
            else { printf("Erro: Variavel '%s' nao declarada.\n", ((Varval *)a)->var); v = 0; }
            break;

        case 'D': 
            l1 = ins(l1, ((Varval *)a->l)->var);
            v = 0;
            break;

        case '=': 
            v = eval(a->r);
            aux = srch(l1, ((Varval *)a->l)->var);
            if(aux) aux->valor = v;
            else printf("Erro: Tentando atribuir a variavel nao declarada.\n");
            break;

        case '+': v = eval(a->l) + eval(a->r); break;
        case '-': v = eval(a->l) - eval(a->r); break;
        case '*': v = eval(a->l) * eval(a->r); break;
        case '/': v = eval(a->l) / eval(a->r); break;
        
        /* Lógica de Comparação. "árv esq   >   árv dir" (Retorna 1.0 se True, 0.0 se False) */
        case '1': v = (eval(a->l) > eval(a->r)) ? 1 : 0; break;
        case '2': v = (eval(a->l) < eval(a->r)) ? 1 : 0; break;
        case '3': v = (eval(a->l) != eval(a->r)) ? 1 : 0; break;
        case '4': v = (eval(a->l) == eval(a->r)) ? 1 : 0; break;
        case '5': v = (eval(a->l) >= eval(a->r)) ? 1 : 0; break;
        case '6': v = (eval(a->l) <= eval(a->r)) ? 1 : 0; break;

        /* Lógica do IF/ELSE*/
        case 'I':
            if (eval(((Flow *)a)->cond) != 0) { /* se condicao verdadeira */
                if (((Flow *)a)->tl)
                    v = eval(((Flow *)a)->tl);
                else
                    v = 0.0;
            } else {
                if (((Flow *)a)->el) {
                    v = eval(((Flow *)a)->el);
                } else
                    v = 0.0;
            }
            break;

        /* Lógica do WHILE */
        case 'W':
            v = 0.0;
            if (((Flow *)a)->tl) {
                while (eval(((Flow *)a)->cond) != 0) {
                    v = eval(((Flow *)a)->tl);
                }
            }
            break;

        case 'L': 
            eval(a->l); 
            v = eval(a->r); 
            break;

        case 'P': 
            v = eval(a->l);
            printf("%.2f\n", v);
            break;

        default: 
            printf("Erro interno: no desconhecido %c\n", a->nodetype);
            v = 0;
            break;
    }
    return v;
}

#include "lex.yy.c"

void yyerror(const char *s) {
    fprintf(stderr, "Erro sintatico: %s\n", s);
}

int main() {
    yyparse();
    return 0;
}