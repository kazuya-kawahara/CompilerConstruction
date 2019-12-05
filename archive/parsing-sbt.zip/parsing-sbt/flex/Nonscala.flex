%%

%type Tokens.Token

ALPHA=[A-Za-z]
DIGIT=[0-9]
WHITE_SPACE_CHAR=[\ \t\b\012]

%% 

"if"     { Tokens.IF }
"else"   { Tokens.ELSE }
"def"    { Tokens.DEF }
"Nil"    { Tokens.NIL }
"("      { Tokens.LPAREN }
")"      { Tokens.RPAREN }
"["      { Tokens.LBRACKET }
"]"      { Tokens.RBRACKET }
","      { Tokens.COMMA }
"+"      { Tokens.PLUS }
"-"      { Tokens.MINUS }
"*"      { Tokens.TIMES }
"/"      { Tokens.DIV }
"."      { Tokens.DOT }
":"      { Tokens.COLON }
"::"     { Tokens.COLONCOLON }
"="      { Tokens.EQ }
"=="     { Tokens.EQEQ }
"<"      { Tokens.LESS }

{WHITE_SPACE_CHAR}        { yylex() }
{ALPHA}({ALPHA}|{DIGIT})* { Tokens.ID(yytext()) }
{DIGIT}+                  { Tokens.INT(yytext().toInt) }
<<EOF>>                   { Tokens.EOF }