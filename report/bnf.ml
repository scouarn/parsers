type bnf_tok 	= Def  of bnf_tok * bnf_tok
			 	| Rule of string
			 	| Litt of string
			 	| Alt  of bnf_tok * bnf_tok
			 	| Seq  of bnf_tok list
			 	| Star of bnf_tok
			 	| Plus of bnf_tok
			 	| Ques of bnf_tok

;;