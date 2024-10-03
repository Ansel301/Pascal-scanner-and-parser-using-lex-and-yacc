%{
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
extern int lineCount, charCount, correct;
bool dup=false, temp_is_array=false, is_integer=false;
int table_idx=0, temp_num=0, temp_array_size=0;
int temp_id_place[200];
char* temp_id[200];
char* temp_type;
char* ltype;
char* rtype;
int yylex();
double ans = 0;
void yyerror(const char* msg)
{
    correct=0;
    printf("\33[2K\rError at line %d: %s", lineCount, msg);
};
struct symbol_table
{
    char* id;
    char* type;
    bool is_array;
    int array_size;
}table[200];

void insert_table()
{
    for(int i=0;i<temp_num;i++)
    {
        for(int j=0;j<table_idx;j++)
        {
            if(table[j].id == temp_id[i])
            {
                dup=true;
                break;
            }
        }
        if(dup)
        {
            correct=0;
            printf("\33[2K\rLine %d, at char %d: Duplicate identifier \"%s\"", lineCount, temp_id_place[i], temp_id[i]);
            dup=false;
            break;
        }
        else 
        {
            if(temp_is_array)
            {
                table[table_idx].is_array=true;
                table[table_idx].array_size=temp_array_size;
            }
            for(int l=0;l<strlen(temp_id[i]);l++) 
	            temp_id[i][l]=tolower(temp_id[i][l]);
            table[table_idx].id=temp_id[i];
            table[table_idx].type=temp_type;
            table_idx++;
        }
        temp_id[i]='\0';
        temp_id_place[i]=-1;
    }
    temp_num=0;
    temp_is_array=false;
};

char* search(char* sid)         //check types by looking up in the symbol if it's an ID, or manually checking the number
{
    is_integer=true;
    if(strcmp(sid,"DIFF_TYPE") == 0) return "DIFF_TYPE";
    if(strcmp(sid,"NOT_FOUND") == 0) return "NOT_FOUND";

    for(int i=0;i<strlen(sid);i++) 
        if(sid[i] == '.' || sid[i] == 'E' || sid[i] == 'e') return "real";
    for(int i=0;i<strlen(sid);i++) 
    {
        if(sid[i] < 48 || sid[i] > 57)
        {
            if(i == 0 && (sid[i] == 43 || sid[i] == 45)) continue;
            else {is_integer=false;break;}
        }
    }
    if(is_integer) return "integer";

    for(int l=0;l<strlen(sid);l++) 
        sid[l]=tolower(sid[l]);
    for(int i=0;i<table_idx;i++) 
        if(strcmp(table[i].id,sid) == 0) 
            return table[i].type;
    return "NOT_FOUND";
};
%}
%define parse.error verbose
%union{
    float floatVal;
    int intVal;
    char* name;
    struct tokenvalue
    {
        char* name;
        int place;
    }value;
}
%type <value> prog prog_name dec_list dec type standtype arraytype id_list stmt_list stmt assign ifstmt exp relop simpexp term factor read write rw_list for index_exp varid body error
%token <value> ABSOLUTE AND BEGINTOKEN BREAK CASE CONST CONTINUE DO ELSE END FOR FUNCTION IF MOD NIL NOT OBJECT OF OR PROGRAM THEN TO VAR WHILE ARRAY INTEGER DOUBLE WRITE WRITELN CHAR STRING FLOAT REAL READ
%token <value> COLON SEMICOLON LP RP LB RB ASSIGN GT LT GE LE EQ NE PLUS MINUS STAR SLASH COMMA DOT
%token <value> ID INT REALNUM STR

%%
prog: PROGRAM prog_name SEMICOLON VAR dec_list SEMICOLON BEGINTOKEN stmt_list SEMICOLON END DOT
    | PROGRAM prog_name SEMICOLON VAR dec_list SEMICOLON BEGINTOKEN stmt_list SEMICOLON END         //error if not DOT
      {
          correct=0;
          printf("\33[2K\rLine %d, at char 4: Syntax error, \".\" expected but \"end of file\" found", lineCount);
      }
    | error;

prog_name: ID;

dec_list: dec
        | dec_list SEMICOLON dec;

dec: id_list COLON type {insert_table();}       //insert all temporarily-stored ID into the symbol table
   | id_list ASSIGN type                        //wrong symbol
     {
        correct=0;
        printf("\33[2K\rLine %d, at char %d: \":\" expected but \"%s\" found", lineCount, $2.place, $2.name);
     }
   | error;

type: standtype
    | arraytype;

standtype: INTEGER {temp_type="integer";}           //temporarily store the ID type
         | DOUBLE {temp_type="double";}
         | CHAR {temp_type="char";}
         | STRING {temp_type="string";}
         | FLOAT {temp_type="double";}
         | REAL {temp_type="real";};

arraytype: ARRAY LB INT DOT DOT INT RB OF standtype
           {
               temp_array_size=$6.name-$3.name-91;
               temp_is_array=true;
           };

id_list: ID
         {
            temp_id[temp_num]=$1.name;              //temporarily store the ID name and its starting place
            temp_id_place[temp_num]=$1.place;
            temp_num++;
         }
	   | id_list COMMA ID
         {
            temp_id[temp_num]=$3.name;
            temp_id_place[temp_num]=$3.place;
            temp_num++;
         }
       | error;

stmt_list: stmt
         | stmt_list SEMICOLON stmt;

stmt: assign
	| read
	| write
	| for
	| ifstmt
    | WRITELN;

//integer-type ID can only store integer
//double- and real-type ID can store integer/double/real
//char-type ID can only store char
//string-type ID can store char/string
assign: varid ASSIGN simpexp        //check the types of both left and right token
        {
            if(strcmp(search($1.name), "NOT_FOUND") == 0)       //"NOT_FOUND" means the token is an ID that isn't in the symbol table(undeclared)
            {
                correct=0;
                printf("\33[2K\rLine %d, at char %d: Identifier not found \"%s\"", lineCount, $1.place, $1.name);
            }
            else if(strcmp(search($3.name), "NOT_FOUND") == 0)
            {
                correct=0;
                printf("\33[2K\rLine %d, at char %d: Identifier not found \"%s\"", lineCount, $3.place, $3.name);
            }
            else if(strcmp(search($1.name), "DIFF_TYPE") == 0 || strcmp(search($3.name), "DIFF_TYPE") == 0) correct=0;      //"DIFF_TYPE" means the token is a wrong-format expression, e.g. int + string
            else if(strcmp(search($1.name), "double") == 0)
            {
                if(strcmp(search($3.name), "integer") != 0 && strcmp(search($3.name), "double") != 0 && strcmp(search($3.name), "real") != 0)
                {
                    correct=0;
                    printf("\33[2K\rLine %d, at char %d: Incompatible types: got \"%s\" expected \"%s\"", lineCount, $3.place, search($3.name), search($1.name));
                }
            }
            else if(strcmp(search($1.name), "real") == 0)
            {
                if(strcmp(search($3.name), "integer") != 0 && strcmp(search($3.name), "double") != 0 && strcmp(search($3.name), "real") != 0)
                {
                    correct=0;
                    printf("\33[2K\rLine %d, at char %d: Incompatible types: got \"%s\" expected \"%s\"", lineCount, $3.place, search($3.name), search($1.name));
                }
            }
            else if(strcmp(search($3.name), "DIFF_TYPE") != 0 && strcmp(search($3.name), search($1.name)) != 0)
            {
                correct=0;
                printf("\33[2K\rLine %d, at char %d: Incompatible types: got \"%s\" expected \"%s\"", lineCount, $3.place, search($3.name), search($1.name));
            }
        }
      | varid relop simpexp
        {
            correct=0;
            if(strcmp(search($1.name), "NOT_FOUND") == 0)
            {
                printf("\33[2K\rLine %d, at char %d: Identifier not found \"%s\"", lineCount, $1.place, $1.name);
            }
            else
            {
                printf("\33[2K\rLine %d, at char %d: \":=\" expected but \"%s\" found", lineCount, $2.place, $2.name);
            }
        }
      | varid ASSIGN STR
        {
            if(strcmp(search($1.name), "NOT_FOUND") == 0)
            {
                correct=0;
                printf("\33[2K\rLine %d, at char %d: Identifier not found \"%s\"", lineCount, $1.place, $1.name);
            }
            else if(strcmp(search($1.name), "char") == 0 && strlen($3.name) != 3)
            {
                correct=0;
                printf("\33[2K\rLine %d, at char %d: Incompatible types: got \"string\" expected \"char\"", lineCount, $3.place);
            }
            else if(strcmp(search($1.name), "string") != 0 && strcmp(search($1.name), "char") != 0)
            {
                correct=0;
                printf("\33[2K\rLine %d, at char %d: Incompatible types: got \"string\" expected \"%s\"", lineCount, $3.place, search($1.name));
            }
        }
      | varid relop STR
        {
            correct=0;
            if(strcmp(search($1.name), "NOT_FOUND") == 0)
            {
                printf("\33[2K\rLine %d, at char %d: Identifier not found \"%s\"", lineCount, $1.place, $1.name);
            }
            else
            {
                printf("\33[2K\rLine %d, at char %d: \":=\" expected but \"%s\" found", lineCount, $2.place, $2.name);
            }
        }
      | error;

ifstmt: IF LP exp RP THEN body
      | IF LP exp RP THEN body ELSE body;

exp: simpexp
     {
        if(strcmp(search($1.name), "NOT_FOUND") == 0)
        {
            correct=0;
            printf("\33[2K\rLine %d, at char %d: Identifier not found \"%s\"", lineCount, $1.place, $1.name);
        }
     }
   | exp relop simpexp
     {
        if(strcmp(search($3.name), "NOT_FOUND") == 0)
        {
            correct=0;
            printf("\33[2K\rLine %d, at char %d: Identifier not found \"%s\"", lineCount, $3.place, $3.name);
        }
     };

relop: GT
     | LT
     | GE
     | LE
     | EQ
     | NE;

//All legal expression shown below:
// [int || double || real] [+ || - || * || /] [int || double || real]
// [char || string] + [char || string] (string concatenation)
// int mod int
simpexp: term
       | simpexp PLUS term
         {
            ltype=strdup(search($1.name));
            rtype=strdup(search($3.name));
            if(strcmp(ltype,rtype) == 0) $$.name=$1.name;
            else if(strcmp(ltype,"DIFF_TYPE") == 0 || strcmp(rtype,"DIFF_TYPE") == 0) $$.name=strdup("DIFF_TYPE");
            else if(strcmp(ltype,"integer"))
            {
                if(strcmp(rtype,"integer") == 0 || strcmp(rtype,"double") == 0 || strcmp(rtype,"real") == 0) $$.name=$1.name;
                else
                {
                    correct=0;
                    printf("\33[2K\rLine %d, at char %d: Operator is not overloaded: \"%s\" + \"%s\"", lineCount, $2.place, ltype, rtype);
                    $$.name=strdup("DIFF_TYPE");
                }
            }
            else if(strcmp(ltype,"double"))
            {
                if(strcmp(rtype,"integer") == 0 || strcmp(rtype,"double") == 0 || strcmp(rtype,"real") == 0) $$.name=$1.name;
                else
                {
                    correct=0;
                    printf("\33[2K\rLine %d, at char %d: Operator is not overloaded: \"%s\" + \"%s\"", lineCount, $2.place, ltype, rtype);
                    $$.name=strdup("DIFF_TYPE");
                }
            }
            else if(strcmp(ltype,"char"))
            {
                if(strcmp(rtype,"char") == 0 || strcmp(rtype,"string") == 0) $$.name=$1.name;
                else
                {
                    correct=0;
                    printf("\33[2K\rLine %d, at char %d: Operator is not overloaded: \"%s\" + \"%s\"", lineCount, $2.place, ltype, rtype);
                    $$.name=strdup("DIFF_TYPE");
                }
            }
            else if(strcmp(ltype,"string"))
            {
                if(strcmp(rtype,"char") == 0 || strcmp(rtype,"string") == 0) $$.name=$1.name;
                else
                {
                    correct=0;
                    printf("\33[2K\rLine %d, at char %d: Operator is not overloaded: \"%s\" + \"%s\"", lineCount, $2.place, ltype, rtype);
                    $$.name=strdup("DIFF_TYPE");
                }
            }
            else if(strcmp(ltype,"real"))
            {
                if(strcmp(rtype,"integer") == 0 || strcmp(rtype,"double") == 0 || strcmp(rtype,"real") == 0) $$.name=$1.name;
                else
                {
                    correct=0;
                    printf("\33[2K\rLine %d, at char %d: Operator is not overloaded: \"%s\" + \"%s\"", lineCount, $2.place, ltype, rtype);
                    $$.name=strdup("DIFF_TYPE");
                }
            }
         }
       | simpexp MINUS term
         {
            ltype=strdup(search($1.name));
            rtype=strdup(search($3.name));
            if(strcmp(ltype,"char") == 0 || strcmp(ltype,"string") == 0 || strcmp(rtype,"char") == 0 || strcmp(rtype,"string") == 0)
            {
                correct=0;
                printf("\33[2K\rLine %d, at char %d: Operator is not overloaded: \"%s\" - \"%s\"", lineCount, $2.place, ltype, rtype);
                $$.name=strdup("DIFF_TYPE");
            }
            else if(strcmp(ltype,rtype) == 0) $$.name=$1.name;
            else if(strcmp(ltype,"DIFF_TYPE") == 0 || strcmp(rtype,"DIFF_TYPE") == 0) $$.name=strdup("DIFF_TYPE");
            else if(strcmp(ltype,"integer"))
            {
                if(strcmp(rtype,"integer") == 0 || strcmp(rtype,"double") == 0 || strcmp(rtype,"real") == 0) $$.name=$1.name;
                else
                {
                    correct=0;
                    printf("\33[2K\rLine %d, at char %d: Operator is not overloaded: \"%s\" - \"%s\"", lineCount, $2.place, ltype, rtype);
                    $$.name=strdup("DIFF_TYPE");
                }
            }
            else if(strcmp(ltype,"double"))
            {
                if(strcmp(rtype,"integer") == 0 || strcmp(rtype,"double") == 0 || strcmp(rtype,"real") == 0) $$.name=$1.name;
                else
                {
                    correct=0;
                    printf("\33[2K\rLine %d, at char %d: Operator is not overloaded: \"%s\" - \"%s\"", lineCount, $2.place, ltype, rtype);
                    $$.name=strdup("DIFF_TYPE");
                }
            }
            else if(strcmp(ltype,"real"))
            {
                if(strcmp(rtype,"integer") == 0 || strcmp(rtype,"double") == 0 || strcmp(rtype,"real") == 0) $$.name=$1.name;
                else
                {
                    correct=0;
                    printf("\33[2K\rLine %d, at char %d: Operator is not overloaded: \"%s\" - \"%s\"", lineCount, $2.place, ltype, rtype);
                    $$.name=strdup("DIFF_TYPE");
                }
            }
         };

term: factor
    | term STAR factor
      {
        ltype=strdup(search($1.name));
        rtype=strdup(search($3.name));
        if(strcmp(ltype,"char") == 0 || strcmp(ltype,"string") == 0 || strcmp(rtype,"char") == 0 || strcmp(rtype,"string") == 0)
        {
            correct=0;
            printf("\33[2K\rLine %d, at char %d: Operator is not overloaded: \"%s\" * \"%s\"", lineCount, $2.place, ltype, rtype);
            $$.name=strdup("DIFF_TYPE");
        }
        else if(strcmp(ltype,rtype) == 0) $$.name=$1.name;
        else if(strcmp(ltype,"DIFF_TYPE") == 0 || strcmp(rtype,"DIFF_TYPE") == 0) $$.name=strdup("DIFF_TYPE");
        else if(strcmp(ltype,"integer"))
        {
            if(strcmp(rtype,"integer") == 0 || strcmp(rtype,"double") == 0 || strcmp(rtype,"real") == 0) $$.name=$1.name;
            else
            {
                correct=0;
                printf("\33[2K\rLine %d, at char %d: Operator is not overloaded: \"%s\" * \"%s\"", lineCount, $2.place, ltype, rtype);
                $$.name=strdup("DIFF_TYPE");
            }
        }
        else if(strcmp(ltype,"double"))
        {
            if(strcmp(rtype,"integer") == 0 || strcmp(rtype,"double") == 0 || strcmp(rtype,"real") == 0) $$.name=$1.name;
            else
            {
                correct=0;
                printf("\33[2K\rLine %d, at char %d: Operator is not overloaded: \"%s\" * \"%s\"", lineCount, $2.place, ltype, rtype);
                $$.name=strdup("DIFF_TYPE");
            }
        }
        else if(strcmp(ltype,"real"))
        {
            if(strcmp(rtype,"integer") == 0 || strcmp(rtype,"double") == 0 || strcmp(rtype,"real") == 0) $$.name=$1.name;
            else
            {
                correct=0;
                printf("\33[2K\rLine %d, at char %d: Operator is not overloaded: \"%s\" * \"%s\"", lineCount, $2.place, ltype, rtype);
                $$.name=strdup("DIFF_TYPE");
            }
        }
      }
    | term SLASH factor
      {
        ltype=strdup(search($1.name));
        rtype=strdup(search($3.name));
        if(strcmp(ltype,"char") == 0 || strcmp(ltype,"string") == 0 || strcmp(rtype,"char") == 0 || strcmp(rtype,"string") == 0)
        {
            correct=0;
            printf("\33[2K\rLine %d, at char %d: Operator is not overloaded: \"%s\" / \"%s\"", lineCount, $2.place, ltype, rtype);
            $$.name=strdup("DIFF_TYPE");
        }
        else if(strcmp(ltype,rtype) == 0) $$.name=$1.name;
        else if(strcmp(ltype,"DIFF_TYPE") == 0 || strcmp(rtype,"DIFF_TYPE") == 0) $$.name=strdup("DIFF_TYPE");
        else if(strcmp(ltype,"integer"))
        {
            if(strcmp(rtype,"integer") == 0 || strcmp(rtype,"double") == 0 || strcmp(rtype,"real") == 0) $$.name=$1.name;
            else
            {
                correct=0;
                printf("\33[2K\rLine %d, at char %d: Operator is not overloaded: \"%s\" / \"%s\"", lineCount, $2.place, ltype, rtype);
                $$.name=strdup("DIFF_TYPE");
            }
        }
        else if(strcmp(ltype,"double"))
        {
            if(strcmp(rtype,"integer") == 0 || strcmp(rtype,"double") == 0 || strcmp(rtype,"real") == 0) $$.name=$1.name;
            else
            {
                correct=0;
                printf("\33[2K\rLine %d, at char %d: Operator is not overloaded: \"%s\" / \"%s\"", lineCount, $2.place, ltype, rtype);
                $$.name=strdup("DIFF_TYPE");
            }
        }
        else if(strcmp(ltype,"real"))
        {
            if(strcmp(rtype,"integer") == 0 || strcmp(rtype,"double") == 0 || strcmp(rtype,"real") == 0) $$.name=$1.name;
            else
            {
                correct=0;
                printf("\33[2K\rLine %d, at char %d: Operator is not overloaded: \"%s\" / \"%s\"", lineCount, $2.place, ltype, rtype);
                $$.name=strdup("DIFF_TYPE");
            }
        }
      }
    | term MOD factor
      {
        ltype=strdup(search($1.name));
        rtype=strdup(search($3.name));
        if(strcmp(ltype,"integer") != 0 || strcmp(rtype,"integer") != 0)
        {
            correct=0;
            printf("\33[2K\rLine %d, at char %d: Operator is not overloaded: \"%s\" mod \"%s\"", lineCount, $2.place, ltype, rtype);
            $$.name=strdup("DIFF_TYPE");
        }
        else if(strcmp(ltype,rtype) == 0) $$.name=$1.name;
        else if(strcmp(ltype,"DIFF_TYPE") == 0 || strcmp(rtype,"DIFF_TYPE") == 0) $$.name=strdup("DIFF_TYPE");
      }

factor: varid
      | INT
      | REALNUM
      | LP simpexp RP;

read: READ LP rw_list RP;

write: WRITE LP rw_list RP;
     | WRITELN LP rw_list RP;

rw_list: varid
         {
            if(strcmp(search($1.name),"NOT_FOUND") == 0)
            {
                correct=0;
                printf("\33[2K\rLine %d, at char %d: Identifier not found \"%s\"", lineCount, $1.place, $1.name);
            }
         }
       | STR
       | rw_list COMMA varid
         {
            if(strcmp(search($3.name),"NOT_FOUND") == 0)
            {
                correct=0;
                printf("\33[2K\rLine %d, at char %d: Identifier not found \"%s\"", lineCount, $3.place, $3.name);
            }
         }
       | rw_list COMMA STR;

for: FOR index_exp DO body;

index_exp: varid ASSIGN simpexp TO exp;

varid: ID
     | ID LB simpexp RB
       {
           if(strcmp(search($3.name),"NOT_FOUND") == 0)
           {
               correct=0;
               printf("\33[2K\rLine %d, at char %d: Identifier not found \"%s\"", lineCount, $3.place, $3.name);
           }
           else if(strcmp(search($3.name),"integer") != 0)
           {
               correct=0;
               printf("\33[2K\rLine %d, at char %d: Incompatible types: got \"%s\" expected \"integer\"", lineCount, $3.place, search($3.name));
           }
       };

body: stmt
    | BEGINTOKEN stmt_list SEMICOLON END;

%%

int main(){
    yyparse();
    return 0;
}
