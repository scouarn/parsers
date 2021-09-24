

litt [0-9]+
plus \+
mult \*

expr {litt}|{term}plus{term}
term {litt}|{litt}mult{litt}



%%

ab|abbbb printf("\n%s\n", yytext);


%%



int main()
{
  yylex();
}

