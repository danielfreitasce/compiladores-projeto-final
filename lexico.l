%{
#include <stdio.h>
%}


DIGITO  [0-9]
LETRA   [a-zA-Z]
ESPACO  [ \t]
QUEBRA  [\n]
VARIAVEL {LETRA}({LETRA}|{DIGITO})*
FLOAT  {DIGITO}+\.{DIGITO}+
STRING   \"[^\"]*\"
COMENTARIO   "//".*
%%

{ESPACO}+   { /* Ignora espaços */ }
{QUEBRA}    { /* Ignora quebras de linha */ }
{COMENTARIO}    { /* Ignora comentários */ }

"programa"  { return PROGRAMA; }
"int"       { return TYPE_INT; }
"float"     { return TYPE_FLOAT; }
"string"    { return TYPE_STRING; }
"escreva"   { return ESCREVA; }
"leia"      { return LEIA; }
"se"        { return SE; }
"senao"     { return SENAO; }
"enquanto"  { return ENQUANTO; }
"para"      { return PARA; }

"+"         { return MAIS; }
"-"         { return MENOS; }
"*"         { return VEZES; }
"/"         { return DIV; }
"="         { return ATRIB; }
";"         { return PTVIRG; }
"("         { return ABRE_P; }
")"         { return FECHA_P; }
"{"         { return ABRE_C; }
"}"         { return FECHA_C; }
","         { return VIRGULA; }
"<"         { return MENOR; }
">"         { return MAIOR; }

{FLOAT}     { return NUM_FLOAT; }
{DIGITO}+   { return NUM_INT; }
{STRING}    { return LITERAL_STR; }
{VARIAVEL}  { return VAR; }

.           { printf("Erro lexico: %s\n", yytext); }

%%

int yywrap() { return 1; }