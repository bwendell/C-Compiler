%{
#include "y.tab.h"
#include "driver.h"
%}



alphabet[a-zA-Z]
digit (-)?[0-9]


whiteSpace [ ]+
tab[\t]+
carriage[\n]
null[\0]
character \'.\'
string \".\"

%%
{carriage}              {rowNum++; column = 0;}
{tab}                   {column += tabSize;}
{whiteSpace}            {column += yyleng;}



"auto"                  {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "AUTO\n\n");}  return AUTO;}
"break"                 {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "BREAK\n\n");}  return BREAK;}
"case"                  {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "CASE\n\n");}  return CASE;}
"char"                  {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "CHAR\n\n");}  return CHAR;}
"const"                 {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "CONST\n\n");}  return CONST;}
"continue"              {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "CONTINUE\n\n");}  return CONTINUE;}   
"default"               {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "DEFAULT\n\n");}  return DEFAULT;}
"do"                    {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "DO\n\n");}  return DO;}
"double"                {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "DOUBLE\n\n");}  return DOUBLE;}
"else"                  {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "ELSE\n\n");}  return ELSE;}
"enum"                  {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "ENUM\n\n");}  return ENUM;}
"extern"                {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "EXTERN\n\n");}  return EXTERN;}
"float"                 {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "FLOAT\n\n");}  return FLOAT;}
"for"                   {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "FOR\n\n");}  return FOR;}
"goto"                  {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "GOTO\n\n");}  return GOTO;}
"if"                    {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "IF\n\n");}  return IF;}
"int"                   {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "INT\n\n");} return INT;}
"long"                  {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "LONG\n\n");}  return LONG;}
"register"              {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "REGISTER\n\n");}  return REGISTER;}
"return"                {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "RETURN\n\n");}  return RETURN;}
"short"                 {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "SHORT\n\n");}  return SHORT;}
"signed"                {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "SIGNED\n\n");}  return SIGNED;}
"sizeof"                {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "SIZEOF\n\n");}  return SIZEOF;}
"static"                {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "STATIC\n\n");}  return STATIC;}
"struct"                {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "STRUCT\n\n");}  return STRUCT;}
"switch"                {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "SWITCH\n\n");}  return SWITCH;}
"typedef"               {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "TYPEDEF\n\n");}  return TYPEDEF;}
"union"                 {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "UNION\n\n");}  return UNION;}
"unsigned"              {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "UNSIGNED\n\n");}  return UNSIGNED;}
"void"                  {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "VOID\n\n");}  return VOID;}
"volatile"              {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "VOLATILE\n\n");}  return VOLATILE;}
"while"                 {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "WHILE\n\n");}  return WHILE;}

{character}             {if(yyleng > 8){printf("Over Flow \n");} yylval.sVal = yytext; column += yyleng;if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "CHARACTER_CONSTANT\n\n");}  return CHARACTER_CONSTANT;}
{string}                {column += yyleng;if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "ENUMERATION_CONSTANT\n\n");}  return ENUMERATION_CONSTANT;}
{digit}+                {if(yyleng > 10){printf("Over Flow \n");}column += yyleng;if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "INTEGER_CONSTANT\n\n");}  yylval.iVal = atoi(yytext); return INTEGER_CONSTANT;}
{digit}+"."{digit}*     {if(yyleng > 32){printf("Over Flow \n");}column += yyleng;if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "FLOATING_CONSTANT\n\n");}  yylval.dVal = atof(yytext); return FLOATING_CONSTANT;}
{alphabet}+{digit}?     {column += yyleng;if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "IDENTIFIER\n\n");} yylval.identifierName = strdup(yytext); return IDENTIFIER;}

"("			            {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "(\n\n");} return('('); }
")"			            {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, ")\n\n");} return(')');}
";"                 {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, ";\n\n");} return ';';}
"{"			            {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "{\n\n");} return('{');}
"}"			            {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "}\n\n");} return('}');}
"["			            {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "[\n\n");} return('[');}
"]"			            {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "]\n\n");} return(']');}
"+"			            {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "+\n\n");} return('+');}
"-"			            {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "-\n\n");} return('-');}
"="			            {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "=\n\n");} return('=');}
"*"			            {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "*\n\n");} return('*');}
"/"			            {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "/\n\n");} return('/');}
"&"			            {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "&\n\n");} return('&');}
"!"			            {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "!\n\n");} return('!');}
"~"			            {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "~\n\n");} return('~');}
">"			            {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, ">\n\n");} return('>');}
"<"			            {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "<\n\n");} return('<');}
","			            {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, ",\n\n");} return(',');}
"?"			            {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "?\n\n");} return('?');}
"^"			            {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "^\n\n");} return('^');}
"|"			            {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "|\n\n");} return('|');}
":"			            {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, ":\n\n");} return(':');}
"."			            {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, ".\n\n");} return('.');}
">="			        {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, ">\n\n");} return GE_OP;}
"<="			        {column += yyleng; if(lexDebug){fprintf(lexFile,"//%s\n", yytext);fprintf(lexFile, "<\n\n");} return LE_OP;}

"!!S"               {if(symbolTableDebug){PrintTable();}else{printf("Set symbol flag table debugging to dump table\n");}}

.                   {scanner_error = true; yyerror("Unrecognized Token");}
%%




