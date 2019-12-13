%%

%type Tokens.Token   // トークンの型
DIGITS=[0-9]+

%% 

`[a-z][a-z0-9]*`                     { Tokens.ID(yytext()) }  //わからない部分
"if"                                 { Tokens.IF }
[a-z][a-z0-9]*                       { Tokens.ID(yytext()) }
0|[1-9][0-9]*                        { Tokens.NUM(yytext().toInt) }  //revised
0{DIGITS}                            { Base.error() }  //added
{DIGITS}"."[0-9]*|[0-9]*"."{DIGITS}  { Tokens.REAL(yytext().toDouble) }
[\ \t\n]+                            { yylex() }
<<EOF>>                              { Tokens.EOF }
