%{
#include "y.tab.h"
#include <stdio.h>
unsigned int charCount = 1, lineCount = 1, correct = 1;
%}

%x sc
space [ ]
eol \r\n|\n
id [_a-zA-Z][_a-zA-Z0-9]{0,14}
integer [+-]?[0-9]+
fraction [0-9]+
real {integer}(\.{fraction})?([Ee]{integer})?
string \'[^\n\r\n]*\'
comment \(\*({eol}|[^\*]|(\*+[^\*\)]))*\*+\)({eol})*
/* You should write your own regular expression. */


%%
<*>[Aa][Bb][Ss][Oo][Ll][Uu][Tt][Ee] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return ABSOLUTE;
}
<*>[Aa][Nn][Dd] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return AND;
}
<*>[Bb][Ee][Gg][Ii][Nn] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return BEGINTOKEN;
}
<*>[Bb][Rr][Ee][Aa][Kk] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return BREAK;
}
<*>[Cc][Aa][Ss][Ee] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return CASE;
}
<*>[Cc][Oo][Nn][Ss][Tt] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return CONST;
}
<*>[Cc][Oo][Nn][Tt][Ii][Nn][Uu][Ee] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return CONTINUE;
}
<*>[Dd][Oo] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return DO;
}
<*>[Ee][Ll][Ss][Ee] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return ELSE;
}
<*>[Ee][Nn][Dd] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return END;
}
<*>[Ff][Oo][Rr] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return FOR;
}
<*>[Ff][Uu][Nn][Cc][Tt][Ii][Oo][Nn] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return FUNCTION;
}
<*>[Ii][Ff] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return IF;
}
<*>[Mm][Oo][Dd] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return MOD;
}
<*>[Nn][Ii][Ll] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return NIL;
}
<*>[Nn][Oo][Tt] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return NOT;
}
<*>[Oo][Bb][Jj][Ee][Cc][Tt] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return OBJECT;
}
<*>[Oo][Ff] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return OF;
}
<*>[Oo][Rr] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return OR;
}
<*>[Pp][Rr][Oo][Gg][Rr][Aa][Mm] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return PROGRAM;
}
<*>[Tt][Hh][Ee][Nn] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return THEN;
}
<*>[Tt][Oo] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return TO;
}
<*>[Vv][Aa][Rr] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return VAR;
}
<*>[Ww][Hh][Ii][Ll][Ee] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return WHILE;
}
<*>[Aa][Rr][Rr][Aa][Yy] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return ARRAY;
}
<*>[Ii][Nn][Tt][Ee][Gg][Ee][Rr] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return INTEGER;
}
<*>[Dd][Oo][Uu][Bb][Ll][Ee] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return DOUBLE;
}
<*>[Ww][Rr][Ii][Tt][Ee] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return WRITE;
}
<*>[Ww][Rr][Ii][Tt][Ee][Ll][Nn] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return WRITELN;
}
<*>[Cc][Hh][Aa][Rr] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return CHAR;
}
<*>[Ss][Tt][Rr][Ii][Nn][Gg] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return STRING;
}
<*>[Ff][Ll][Oo][Aa][Tt] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return FLOAT;
}
<*>[Rr][Ee][Aa][Ll] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return REAL;
}
<*>[Rr][Ee][Aa][Dd] {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return READ;
}
<*>{space} {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf(" ");
	charCount++;
}
<*>[ ]*{eol} {
	if(charCount != 1) printf("\n");
	lineCount++; 
	charCount = 1;
	correct = 1;
}
{integer} {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN sc;
	return INT;
}
{real} {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN sc;
	return REALNUM;
}
{string} {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return STR;
}
<*>":" {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return COLON;
}
<*>";" {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return SEMICOLON;
}
<*>"(" {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return LP;
}
<*>")" {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return RP;
}
<*>"[" {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return LB;
}
<*>"]" {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return RB;
}
<*>":=" {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return ASSIGN;
}
<*>">" {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return GT;
}
<*>"<" {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return LT;
}
<*>">=" {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return GE;
}
<*>"<=" {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return LE;
}
<*>"=" {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return EQ;
}
<*>"<>" {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return NE;
}
<sc>"+" {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return PLUS;
}
<sc>"-" {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return MINUS;
}
<*>"*" {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return STAR;
}
<*>"/" {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return SLASH;
}
<*>"," {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return COMMA;
}
<*>"." {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN 0;
	return DOT;
}
<*>{id} {
	if(charCount == 1 && correct == 1) printf("Line %d: ", lineCount);
	if(correct == 1) printf("%s", yytext);
	yylval.value.name=strdup(yytext);
	yylval.value.place=charCount;
	charCount += yyleng;
	BEGIN sc;
	return ID;
}
<*>[ ]*{comment} {
	for(int i=0;i<yyleng;i++) 
	{
		if (yytext[i] == 10)
		{
			lineCount++; 
			charCount = 1;
		} 
		else 
			charCount++;
	}
	BEGIN 0;
}

%%
