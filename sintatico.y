%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

int yylex();
void yyerror(const char *s);

/* ========================================================================== */
/* ESTRUTURAS DE DADOS (CABEÇALHO)                      */
/* ========================================================================== */

/* 1. Tabela de Símbolos (Lista Encadeada) */
typedef struct vars {
    char name[50];
    int type;       /* 0=NUMERO, 1=STRING */
    double valor;   /* Valor se for numero */
    char str[100];  /* Valor se for string */
    struct vars *prox;
} VARI;

VARI *l1 = NULL; /* Lista global de variáveis */

/* Função para inserir nova variável na lista */
VARI *ins(VARI *l, char n[]) {
    VARI *new = (VARI*)malloc(sizeof(VARI));
    strcpy(new->name, n);
    new->valor = 0;
    new->type = 0;
    new->str[0] = '\0';
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

/* 2. Estruturas da Árvore (AST) */

/* Nó genérico (Operadores) */
typedef struct ast {
    int nodetype;
    struct ast *l;
    struct ast *r;
} Ast;

/* Nó Numérico */
typedef struct numval {
    int nodetype;
    double number;
} Numval;

/* Nó String */
typedef struct strval {
    int nodetype;
    char str[100];
} Strval;

/* Nó Variável */
typedef struct varval {
    int nodetype;
    char var[50];
} Varval;

/* Nó de Fluxo (IF/WHILE) */
typedef struct flow {
    int nodetype;
    Ast *cond;
    Ast *tl;
    Ast *el;
} Flow;

/* Estrutura de Retorno do EVAL (Para suportar Num e String) */
typedef struct {
    int type; /* 0=NUM, 1=STR */
    double val;
    char str[100];
} Data;

/* Protótipos das Funções Auxiliares */
Ast *newast(int nodetype, Ast *l, Ast *r);
Ast *newnum(double d);
Ast *newstr(char *s);
Ast *newvar(char *s);
Ast *newcmp(int cmptype, Ast *l, Ast *r);
Ast *newflow(int nodetype, Ast *cond, Ast *tl, Ast *el);
Data eval(Ast *a);

%}

/* ========================================================================== */
/* DEFINIÇÕES DO BISON                                 */
/* ========================================================================== */

%union {
    double flo;
    int fn;
    char str[100];
    Ast *a;
}

%token PROGRAMA
%token TYPE_INT TYPE_FLOAT TYPE_STRING
%token ESCREVA LEIA
%token SE SENAO ENQUANTO PARA
%token PTVIRG VIRGULA
%token ABRE_P FECHA_P ABRE_C FECHA_C
%token ATRIB

%token <flo> NUM_INT NUM_FLOAT
%token <str> VAR LITERAL_STR
%token <fn> CMP

%nonassoc IFX
%nonassoc SENAO

%left CMP /* Menor precedência: compara depois de somar. Igual ao C, Python e Java */
%left MAIS MENOS
%left VEZES DIV

%type <a> expressao atribuicao comando declaracao comandos fluxo

%%

/* ========================================================================== */
/* GRAMÁTICA (REGRAS)                               */
/* ========================================================================== */

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
    | LEIA ABRE_P VAR FECHA_P PTVIRG { $$ = newast('R', newvar($3), NULL); }
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
    | LITERAL_STR       { $$ = newstr($1); }
    | VAR               { $$ = newvar($1); }
    | expressao MAIS expressao  { $$ = newast('+', $1, $3); }
    | expressao MENOS expressao { $$ = newast('-', $1, $3); }
    | expressao VEZES expressao { $$ = newast('*', $1, $3); }
    | expressao DIV expressao   { $$ = newast('/', $1, $3); }
    | expressao CMP expressao   { $$ = newcmp($2, $1, $3); } 
    | ABRE_P expressao FECHA_P  { $$ = $2; }
    ;

%%

/* ========================================================================== */
/* FUNÇÕES AUXILIARES E MAIN                            */
/* ========================================================================== */

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

Ast *newstr(char *s) {
    Strval *a = (Strval*) malloc(sizeof(Strval));
    if(!a) { printf("Sem memoria"); exit(0); }
    a->nodetype = 'S'; 
    strcpy(a->str, s);
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

/* INTERPRETADOR (EVAL) */
Data eval(Ast *a) {
    Data v = {0, 0, ""}; 
    Data v1, v2;
    VARI *aux;

    if(!a) return v;

    switch(a->nodetype) {
        case 'K': /* Numero */
            v.type = 0;
            v.val = ((Numval *)a)->number; 
            break;

        case 'S': /* String */
            v.type = 1;
            strcpy(v.str, ((Strval *)a)->str);
            break;

        case 'N': /* Variavel */
            aux = srch(l1, ((Varval *)a)->var);
            if(aux) {
                v.type = aux->type;
                if(v.type == 0) v.val = aux->valor;
                else strcpy(v.str, aux->str);
            } else { 
                printf("Erro: Variavel '%s' nao declarada.\n", ((Varval *)a)->var); 
            }
            break;

        case 'D': /* Declaracao */
            l1 = ins(l1, ((Varval *)a->l)->var);
            break;

        case 'R': /* LEIA */
            aux = srch(l1, ((Varval *)a->l)->var);
            if(aux) {
                //printf("Digite valor para %s: ", aux->name);
                fflush(stdout);
                if(scanf("%lf", &v.val) == 1) {
                    aux->type = 0;
                    aux->valor = v.val;
                }
            } else {
                printf("Erro: Variavel nao declarada.\n");
            }
            break;

        case '=': /* Atribuicao */
            v = eval(a->r);
            aux = srch(l1, ((Varval *)a->l)->var);
            if(aux) {
                aux->type = v.type;
                if(v.type == 0) aux->valor = v.val;
                else strcpy(aux->str, v.str);
            } else {
                printf("Erro: Variavel nao declarada.\n");
            }
            break;

        /* Operacoes Matematicas (Simplificadas para numeros) */
        case '+': v1 = eval(a->l); v2 = eval(a->r); v.val = v1.val + v2.val; break;
        case '-': v1 = eval(a->l); v2 = eval(a->r); v.val = v1.val - v2.val; break;
        case '*': v1 = eval(a->l); v2 = eval(a->r); v.val = v1.val * v2.val; break;
        case '/': v1 = eval(a->l); v2 = eval(a->r); v.val = v1.val / v2.val; break;
        
        /* Comparacoes */
         /* Lógica de Comparação. "árv esq   >   árv dir" (Retorna 1.0 se True, 0.0 se False) */
        case '1': v1 = eval(a->l); v2 = eval(a->r); v.val = (v1.val > v2.val) ? 1 : 0; break;
        case '2': v1 = eval(a->l); v2 = eval(a->r); v.val = (v1.val < v2.val) ? 1 : 0; break;
        case '3': v1 = eval(a->l); v2 = eval(a->r); v.val = (v1.val != v2.val) ? 1 : 0; break;
        case '4': v1 = eval(a->l); v2 = eval(a->r); 
                  if(v1.type==1 && v2.type==1) v.val = (strcmp(v1.str, v2.str) == 0);
                  else v.val = (v1.val == v2.val);
                  break;
        case '5': v1 = eval(a->l); v2 = eval(a->r); v.val = (v1.val >= v2.val) ? 1 : 0; break;
        case '6': v1 = eval(a->l); v2 = eval(a->r); v.val = (v1.val <= v2.val) ? 1 : 0; break;

        /* IF / ELSE */
        case 'I':
            if (eval(((Flow *)a)->cond).val != 0) { 
                if (((Flow *)a)->tl) v = eval(((Flow *)a)->tl);
            } else {
                if (((Flow *)a)->el) v = eval(((Flow *)a)->el);
            }
            break;

        /* WHILE */
        case 'W':
            if (((Flow *)a)->tl) {
                while (eval(((Flow *)a)->cond).val != 0) {
                    v = eval(((Flow *)a)->tl);
                }
            }
            break;

        /* Lista de Comandos */
        case 'L': 
            eval(a->l); 
            v = eval(a->r); 
            break;

        /* ESCREVA */
        case 'P': 
            v = eval(a->l);
            if(v.type == 0) printf("%.2f\n", v.val);
            else printf("%s\n", v.str);
            break;

        default: 
            printf("Erro interno: no desconhecido %c\n", a->nodetype);
            break;
    }
    return v;
}

#include "lex.yy.c"

void yyerror(const char *s) {
    fprintf(stderr, "Erro sintatico: %s\n", s);
}

int main(int argc, char *argv[]) {
    if (argc > 1) {
        FILE *arquivo = fopen(argv[1], "r");
        if (!arquivo) {
            printf("Erro ao abrir arquivo.\n");
            return 1;
        }
        yyin = arquivo;
    }
    yyparse();
    if (argc > 1) fclose(yyin);
    return 0;
}