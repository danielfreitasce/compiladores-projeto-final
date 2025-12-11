%{
#include <stdio.h>
#include <stdlib.h> 
#include <string.h> 
%}

DIGITO   [0-9]
LETRA    [a-zA-Z]
ESPACO   [ \t]
QUEBRA   [\n]
VARIAVEL {LETRA}({LETRA}|{DIGITO})*
FLOAT    {DIGITO}+\.{DIGITO}+
STRING   \"[^\"]*\"
COMENTARIO   "//".*

%%

{ESPACO}+       { /* Ignora espaços */ }
{QUEBRA}        { /* Ignora quebras de linha */ }
{COMENTARIO}    { /* Ignora comentários */ }

"programa"      { return PROGRAMA; }
"int"           { return TYPE_INT; }
"float"         { return TYPE_FLOAT; }
"string"        { return TYPE_STRING; }
"escreva"       { return ESCREVA; }
"leia"          { return LEIA; }
"se"            { return SE; }
"senao"         { return SENAO; }
"enquanto"      { return ENQUANTO; }
"para"          { return PARA; }

"+"             { return MAIS; }
"-"             { return MENOS; }
"*"             { return VEZES; }
"/"             { return DIV; }
"="             { return ATRIB; }
";"             { return PTVIRG; }
"("             { return ABRE_P; }
")"             { return FECHA_P; }
"{"             { return ABRE_C; }
"}"             { return FECHA_C; }
","             { return VIRGULA; }

">"             { yylval.fn = 1; return CMP; }
"<"             { yylval.fn = 2; return CMP; }
"!="            { yylval.fn = 3; return CMP; }
"=="            { yylval.fn = 4; return CMP; }
">="            { yylval.fn = 5; return CMP; }
"<="            { yylval.fn = 6; return CMP; }

{FLOAT}         { yylval.flo = atof(yytext); return NUM_FLOAT; }
{DIGITO}+       { yylval.flo = atof(yytext); return NUM_INT; }
{STRING}        { return LITERAL_STR; }
{VARIAVEL}      { strcpy(yylval.str, yytext); return VAR; }

.               { printf("Erro lexico: %s\n", yytext); } 

%%

int yywrap() { return 1; }