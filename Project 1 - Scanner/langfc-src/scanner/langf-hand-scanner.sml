(* langfc-src/scanner-parser/langf-hand-scanner.sml
 *
 * COPYRIGHT (c) 2011-2015 Matthew Fluet (http://www.cs.rit.edu/~mtf)
 * All rights reserved.
 *
 * Rochester Institute of Technology
 * 4005-711,CSCI-742
 * Q20112,Q20122,S20135,S20145
 *
 * COPYRIGHT (c) 2009 Matthew Fluet (http://tti-c.org/fluet)
 * All rights reserved.
 *
 * University of Chicago
 * CMSC 22610
 * Winter 2009
 *
 * Hand-written LangF scanner as a StringCvt.reader.
 *)

structure LangFHandScanner: LANGF_HAND_SCANNER =
struct
   structure T = Tokens
   fun scan {reportError: string -> unit} 
            (charRdr: (char, 'strm) StringCvt.reader) :
            (Tokens.token, 'strm) StringCvt.reader =
     let
        fun badChar c =
           reportError (concat ["bad character '", Char.toString c, "'"])
		   
		fun badEscapeChar c =
           reportError (concat ["bad escape character '", Char.toString c, "'"])
		   
		fun badCommentChar c =
           reportError (concat ["unclosed comment found at the end of file '", Char.toString c, "'"])
		   
		fun badTypeNameChar c =
           reportError (concat ["bad type name  '", Char.toString c, "'"])

		fun reverseList strList =
			case strList of
				hd::tl => reverseList tl@[hd]
				| [] => []
		
		(* used to build Strings *)
		fun stringStringBuilder strList strm0 =
			case charRdr strm0 of
				SOME (#"\"", strm1) => (strList, strm1)
				| SOME (#"\a", strm1) => (badEscapeChar #"\a" ; stringStringBuilder strList strm1)
				| SOME (#"\b", strm1) => (badEscapeChar #"\b" ; stringStringBuilder strList strm1)
				| SOME (#"\f", strm1) => (badEscapeChar #"\f" ; stringStringBuilder strList strm1)
				| SOME (#"\n", strm1) => (badEscapeChar #"\n" ; stringStringBuilder strList strm1)
				| SOME (#"\r", strm1) => (badEscapeChar #"\r" ; stringStringBuilder strList strm1)
				| SOME (#"\t", strm1) => (badEscapeChar #"\t" ; stringStringBuilder strList strm1)
				| SOME (#"\v", strm1) => (badEscapeChar #"\v" ; stringStringBuilder strList strm1)
				(* Escape characters *)
				| SOME (#"\\", strm1) => 
					(case charRdr strm1 of
						SOME (#"a", strm2) => stringStringBuilder (#"\a"::strList) strm2
						| SOME (#"b", strm2) => stringStringBuilder (#"\b"::strList) strm2
						| SOME (#"f", strm2) => stringStringBuilder (#"\f"::strList) strm2
						| SOME (#"n", strm2) => stringStringBuilder (#"\n"::strList) strm2
						| SOME (#"r", strm2) => stringStringBuilder (#"\r"::strList) strm2
						| SOME (#"t", strm2) => stringStringBuilder (#"\t"::strList) strm2
						| SOME (#"v", strm2) => stringStringBuilder (#"\v"::strList) strm2
						| SOME (#"\\", strm2) => stringStringBuilder (#"\\"::strList) strm2
						| SOME (#"\"", strm2) => stringStringBuilder (#"\""::strList) strm2
						| SOME (c1, strm2) => if Char.isDigit c1
												then 
													(case charRdr strm2 of
														SOME (c2, strm3) => if Char.isDigit c2
																			 then 
																				(case charRdr strm3 of
																					SOME (c3, strm4) => if Char.isDigit c3
																										 then
																											let
																												val number = String.implode ([c1, c2, c3])
																											in
																												if number > "255"
																													then (badEscapeChar c3; stringStringBuilder strList strm4)
																												else
																													case Char.scan charRdr strm0 of
																														NONE => (strList , strm4)
																														| SOME(c, strm) => stringStringBuilder (c::strList) strm4
																											end
																										else (badEscapeChar c3; stringStringBuilder strList strm2))
																			else (badEscapeChar c2; stringStringBuilder strList strm2))
											   else (badEscapeChar c1; stringStringBuilder strList strm2))
				| SOME (c, strm1) => stringStringBuilder (c::strList) strm1
				| NONE => (badEscapeChar #"0"; (strList,strm0))
				
		(* used to build variable names *)
		fun stringBuilder strList strm0 =
			case charRdr strm0 of
				SOME (c, strm1) => if Char.isAlphaNum c orelse c=(#"_") orelse c=(#"'")
									then stringBuilder (c::strList) strm1
								   else (strList, strm0)
				| _ => (strList, strm0)

		(* used to build integers *)
		fun integerBuilder intList strm0 =
			case charRdr strm0 of
				 SOME (c, strm1) => if Char.isDigit c
									then integerBuilder (c::intList) strm1
									else (intList, strm0)
				| _ => (intList, strm0)
					
		(* used to build Constructor names *)
		fun conNameBuilder strList strm0 =
			case charRdr strm0 of
				SOME (c,strm1) => if Char.isAlphaNum c orelse c=(#"_") orelse c=(#"'")
									then conNameBuilder (c::strList) strm1
								 else (strList, strm0)
				| _ => (strList, strm0)
		
		fun typeNameBuilder strList strm0 =
			case charRdr strm0 of
				SOME (c,strm1) => if Char.isAlphaNum c orelse c=(#"_") orelse c=(#"'")
									then conNameBuilder (c::strList) strm1
								 else (strList, strm0)
				| _ => (strList, strm0)
				
		fun commentCount count strm0 =
			case charRdr strm0 of
				 SOME (#"(", strm1) =>
					(case charRdr strm1 of
						SOME (#"*", strm2) => commentCount (count+1) strm2
						| _ => commentCount count strm1)
				| SOME (#"*", strm1) =>
					(case charRdr strm1 of
						SOME (#")", strm2) => if count-1 = 0
												then ([], strm2)
											  else
												commentCount (count-1) strm2
						| _ => commentCount count strm1)
				| SOME(c,strm1) => commentCount count strm1
				| NONE => (badCommentChar #"0"; ([], strm0))

		(* add more scan??? functions for specific complex tokens *)
		
		fun scanTok strm0 =
           case charRdr strm0 of
			(* delimiter and operator symbols *)
             SOME (#"+", strm1) => SOME (T.PLUS, strm1)
            | SOME (#"-", strm1) => 
                 (case charRdr strm1 of
                     SOME (#">", strm2) => SOME (T.MINUS_ARROW, strm2)
                   | _ => SOME (T.MINUS, strm1))
            | SOME (#"*", strm1) => SOME (T.ASTERISK, strm1)
			| SOME (#"/", strm1) => SOME (T.SLASH, strm1)
			| SOME (#"%", strm1) => SOME (T.PERCENT, strm1)
			| SOME (#"~", strm1) => 
				(case charRdr strm1 of 
					SOME (c, strm2) => if Char.isDigit c
										then let 
											val (num, strm) = integerBuilder [] strm1
											val str2 = reverseList num
											val str3 = #"~" :: str2
											in
												(case IntInf.fromString(String.implode str3) of
													NONE => NONE
													|SOME(i) => SOME (T.INTEGER i, strm))
											end
										else SOME (T.TILDE, strm1)
					| _ =>	SOME (T.TILDE, strm1))
			| SOME (#"=", strm1) => 
				 (case charRdr strm1 of
					 SOME (#"=", strm2) => SOME (T.EQEQ, strm2)
					| SOME (#">", strm2) => SOME (T.EQ_ARROW, strm2) 
					| _ => SOME (T.EQ, strm1))
			| SOME (#"<", strm1) => 
				 (case charRdr strm1 of
					 SOME (#">", strm2) => SOME (T.LTGT, strm2)
					| SOME (#"=", strm2) => SOME (T.LTEQ, strm2)
					| _ => SOME (T.LT, strm1))
			| SOME (#">", strm1) =>
				 (case charRdr strm1 of
					 SOME (#"=", strm2) => SOME (T.GTEQ, strm2)
					| _ => SOME (T.GT, strm1))
			| SOME (#"^", strm1) => SOME (T.CARET, strm1)
			| SOME (#"!", strm1) => SOME (T.BANG, strm1)
			| SOME (#":", strm1) =>
				 (case charRdr strm1 of
					 SOME (#"=", strm2) => SOME (T.COLON_EQ, strm2)
					| _ => SOME (T.COLON, strm1))
			| SOME (#"#", strm1) => SOME (T.HASH, strm1)
			| SOME (#"(", strm1) => 
				(case charRdr strm1 of
					SOME (#"*", strm2) => let
											 val (comment, strm) = commentCount 0 strm0
										   in
											 scanTok strm
										   end
					| _ => SOME (T.LPAREN, strm1))
			| SOME (#")", strm1) => SOME (T.RPAREN, strm1)
			| SOME (#"[", strm1) => SOME (T.LSBRACK, strm1)
			| SOME (#"]", strm1) => SOME (T.RSBRACK, strm1)
			| SOME (#"{", strm1) => SOME (T.LCBRACK, strm1)
			| SOME (#"}", strm1) => SOME (T.RCBRACK, strm1)
			| SOME (#",", strm1) => SOME (T.COMMA, strm1)
			| SOME (#";", strm1) => SOME (T.SEMI, strm1)
			| SOME (#"|", strm1) => SOME (T.VBAR, strm1)
			| SOME (#"_", strm1) => SOME (T.UNDERSCORE, strm1)
			(* keywords *)
			| SOME (#"a", strm1) =>
				(case charRdr strm1 of
					SOME (#"n", strm2) =>
						(case charRdr strm2 of
							 SOME (#"d", strm3) =>
								(case charRdr strm3 of
									SOME(#"a", strm4) =>
										(case charRdr strm4 of
											 SOME (#"l", strm5) =>
												(case charRdr strm5 of
													 SOME (#"s", strm6) =>
														(case charRdr strm6 of
															 SOME (#"o", strm7) => 
																(case charRdr strm7 of	
																	 SOME(c,strm8) => if Char.isAlphaNum c orelse c=(#"_") orelse c=(#"'")
																					  then let
					 																		 val (str, strm) = stringBuilder [] strm0
																							 val str2 = reverseList str
																							 val value = Atom.atom(String.implode str2)
					  																	   in
																							 SOME (T.VAR_NAME value, strm)
																						   end
																					  else
																						 SOME (T.KW_andalso, strm7))
																| _ => let
																	val (str,strm) = stringBuilder [] strm0
																	val str2 = reverseList str
																	val value = Atom.atom(String.implode str2)
																   in
																	SOME (T.VAR_NAME value, strm)
																   end)
													| _ => let
															val (str, strm) = stringBuilder [] strm0
															val str2 = reverseList str
															val value = Atom.atom(String.implode str2)
														   in
															SOME(T.VAR_NAME value, strm)
														   end)
											| _ => let
													val (str, strm) = stringBuilder [] strm0
													val str2 = reverseList str
													val value = Atom.atom(String.implode str2)
												   in
													SOME (T.VAR_NAME value, strm)
												   end)
									| SOME (c, strm4) => if Char.isAlphaNum c orelse c=(#"_") orelse c=(#"'")
														then let
																val (str, strm) = stringBuilder [] strm0
																val str2 = reverseList str
																val value = Atom.atom(String.implode str2)
															in	
																SOME (T.VAR_NAME value, strm)
															end
														else
															SOME (T.KW_and , strm3)
									| _ => let
											 val (str, strm) = stringBuilder [] strm0
											 val str2 = reverseList str
											 val value = Atom.atom(String.implode str2)
										   in
											 SOME(T.VAR_NAME value, strm)
										   end)
							| _ => let
									 val (str, strm) = stringBuilder [] strm0
									 val str2 = reverseList str
									 val value = Atom.atom(String.implode str2)
								   in
								     SOME(T.VAR_NAME value, strm)
								   end)
					| _ => let
							 val (str, strm) = stringBuilder [] strm0
							 val str2 = reverseList str
							 val value = Atom.atom(String.implode str2)
						   in
							 SOME(T.VAR_NAME value, strm)
						   end)
			| SOME (#"c", strm1) =>
				 (case charRdr strm1 of
					SOME (#"a", strm2) =>
						(case charRdr strm2 of
							 SOME (#"s", strm3) =>
								 (case charRdr strm3 of
									 SOME (#"e", strm4) => 
										(case charRdr strm4 of
											SOME (c, strm5) => if Char.isAlphaNum c orelse c=(#"_") orelse c=(#"'")
															   then let
																	 val (str, strm) = stringBuilder [] strm0
																	 val str2 = reverseList str
																	 val value = Atom.atom(String.implode str2)
																	in
																	 SOME (T.VAR_NAME value, strm)
																	end
															   else
																	SOME (T.KW_case , strm4))
									| _ => let
											 val (str, strm) = stringBuilder [] strm0
											 val str2 = reverseList str
											 val value = Atom.atom(String.implode str2)
										   in
											 SOME (T.VAR_NAME value, strm)
										   end)
							| _ => let
									 val (str, strm) = stringBuilder [] strm0
									 val str2 = reverseList str
									 val value = Atom.atom(String.implode str2)
								   in
									 SOME (T.VAR_NAME value, strm)
								   end)
					| _ => let
							 val (str, strm) = stringBuilder [] strm0
							 val str2 = reverseList str
							 val value = Atom.atom(String.implode str2)
						   in
						    SOME (T.VAR_NAME value, strm)
						   end)
			| SOME (#"d", strm1) =>
				(case charRdr strm1 of
					SOME (#"a", strm2) =>
						(case charRdr strm2 of
							 SOME (#"t", strm3) =>
								(case charRdr strm3 of
									 SOME (#"a", strm4) =>
										(case charRdr strm4 of
											 SOME (#"t", strm5) =>
												(case charRdr strm5 of
													 SOME (#"y", strm6) =>
														(case charRdr strm6 of
														 SOME (#"p", strm7) =>
															(case charRdr strm7 of
																 SOME (#"e", strm8) => 
																	(case charRdr strm8 of
																		SOME(c,strm9) => if Char.isAlphaNum c orelse c=(#"_") orelse c=(#"'")
																						 then let
																								val (str, strm) = stringBuilder [] strm0
																								val str2 = reverseList str
																								val value = Atom.atom(String.implode str2)
																							  in
																								SOME (T.VAR_NAME value, strm)
																							  end
																						 else
																							SOME (T.KW_datatype , strm8))
																| _ => let
																		val (str, strm) = stringBuilder [] strm0
																		val str2 = reverseList str
																		val value = Atom.atom(String.implode str2)
																	   in
																		SOME (T.VAR_NAME value, strm)
																	   end)
														| _ => let
																 val (str, strm) = stringBuilder [] strm0
																 val str2 = reverseList str
																 val value = Atom.atom(String.implode str2)
															   in
																 SOME (T.VAR_NAME value, strm)
															   end)
													| _ => let
															 val (str, strm) = stringBuilder [] strm0
															 val str2 = reverseList str
															 val value = Atom.atom(String.implode str2)
														   in
															 SOME (T.VAR_NAME value, strm)
														   end)
											| _ => let
													 val (str, strm) = stringBuilder [] strm0
													 val str2 = reverseList str
													 val value = Atom.atom(String.implode str2)
												   in
												     SOME (T.VAR_NAME value, strm)
												   end) 
									| _ => let
											 val (str, strm) = stringBuilder [] strm0
											 val str2 = reverseList str
											 val value = Atom.atom(String.implode str2)
										   in
											 SOME (T.VAR_NAME value, strm)
										   end)
							| _ => let
									 val (str, strm) = stringBuilder [] strm0
									 val str2 = reverseList str
									 val value = Atom.atom(String.implode str2)
								   in
									 SOME (T.VAR_NAME value, strm)
								   end)
					| _ => let
							 val (str, strm) = stringBuilder [] strm0
							 val str2 = reverseList str
							 val value = Atom.atom(String.implode str2)
						   in
							SOME (T.VAR_NAME value, strm)
						   end)
			| SOME (#"e", strm1) =>
				(case charRdr strm1 of
					SOME (#"l", strm2) =>
						(case charRdr strm2 of
							 SOME (#"s", strm3) =>
								(case charRdr strm3 of
									 SOME (#"e", strm4) => 
										(case charRdr strm4 of
											SOME(c, strm5) => if Char.isAlphaNum c orelse c=(#"_") orelse c=(#"'")
															  then let
																	 val (str, strm) = stringBuilder [] strm0
																	 val str2 = reverseList str
																	 val value = Atom.atom(String.implode str2)
																   in
																	 SOME (T.VAR_NAME value, strm)
																   end
															  else
																SOME (T.KW_else , strm4))
									| _ => let
											 val (str, strm) = stringBuilder [] strm0
											 val str2 = reverseList str
											 val value = Atom.atom(String.implode str2)
										   in	
											 SOME (T.VAR_NAME value, strm)
										   end)
							| _ => let
									 val (str, strm) = stringBuilder [] strm0
									 val str2 = reverseList str
									 val value = Atom.atom(String.implode str2)
								   in
									 SOME (T.VAR_NAME value, strm)
								   end)
					| SOME (#"n", strm2) =>
						(case charRdr strm2 of
							 SOME (#"d", strm3) =>
								(case charRdr strm3 of
									 SOME (c, strm4) => if Char.isAlphaNum c orelse c=(#"_") orelse c=(#"'")
														then let
																val (str, strm) = stringBuilder [] strm0
																val str2 = reverseList str
																val value = Atom.atom(String.implode str2)
															 in
																SOME (T.VAR_NAME value, strm)
															 end
														else
															SOME (T.KW_end, strm3))
							| _ => let
									 val (str, strm) = stringBuilder [] strm0
									 val str2 = reverseList str
									 val value = Atom.atom(String.implode str2)
								   in
									 SOME (T.VAR_NAME value, strm)
								   end)
					| _ => let
							 val (str, strm) = stringBuilder [] strm0
							 val str2 = reverseList str
							 val value = Atom.atom(String.implode str2)
						   in
							 SOME (T.VAR_NAME value, strm)
						   end)
			| SOME (#"f", strm1) =>
				(case charRdr strm1 of
					 SOME (#"n", strm2) => 
						(case charRdr strm2 of
							SOME (c, strm3) => if Char.isAlphaNum c orelse c=(#"_") orelse c=(#"'")
											   then let
													 val (str, strm) = stringBuilder [] strm0
													 val str2 = reverseList str
													 val value = Atom.atom(String.implode str2)
													in
													 SOME (T.VAR_NAME value, strm)
													end
											   else
												 SOME (T.KW_fn, strm2))
					| SOME (#"u", strm2) =>
						(case charRdr strm2 of
							 SOME (#"n", strm3) =>
								(case charRdr strm3 of
									 SOME (c, strm4) => if Char.isAlphaNum c orelse c=(#"_") orelse c=(#"'")
														then let
																val (str, strm) = stringBuilder [] strm0
																val str2 = reverseList str
																val value = Atom.atom(String.implode str2)
															 in
																SOME (T.VAR_NAME value, strm)
															 end
														else
															SOME (T.KW_fun, strm3))
							| _ => let
									 val (str, strm) = stringBuilder [] strm0
									 val str2 = reverseList str
									 val value = Atom.atom(String.implode str2)
								   in
									 SOME (T.VAR_NAME value, strm)
								   end)
					| _ => let
							 val (str, strm) = stringBuilder [] strm0
							 val str2 = reverseList str
							 val value = Atom.atom(String.implode str2)
						   in
							 SOME (T.VAR_NAME value, strm)
						   end)
			| SOME (#"i", strm1) =>
				(case charRdr strm1 of
					SOME (#"f", strm2) =>
						(case charRdr strm2 of
							SOME (c, strm3) => if Char.isAlphaNum c orelse c=(#"_") orelse c=(#"'")
												then let
														val (str, strm) = stringBuilder [] strm0
														val str2 = reverseList str
														val value = Atom.atom(String.implode str2)
													 in
														SOME (T.VAR_NAME value, strm)
													 end
												else
													SOME (T.KW_if, strm2))
					| SOME (#"n", strm2) => 
						(case charRdr strm2 of
							 SOME (c, strm3) => if Char.isAlphaNum c orelse c=(#"_") orelse c=(#"'")
												then let
														val (str, strm) = stringBuilder [] strm0
														val str2 = reverseList str
														val value = Atom.atom(String.implode str2)
													 in
														SOME (T.VAR_NAME value, strm)
													 end
												else
													SOME (T.KW_in, strm2))
					| _ => let
							 val (str, strm) = stringBuilder [] strm0
							 val str2 = reverseList str
							 val value = Atom.atom(String.implode str2)
						   in
							 SOME (T.VAR_NAME value, strm)
						   end)
			| SOME (#"l", strm1) =>
				 (case charRdr strm1 of
					 SOME (#"e", strm2) =>
						(case charRdr strm2 of
							 SOME (#"t", strm3) =>
								(case charRdr strm3 of
									 SOME (c, strm4) => if Char.isAlphaNum c orelse c=(#"_") orelse c=(#"'")
														then let
																val (str, strm) = stringBuilder [] strm0
																val str2 = reverseList str
																val value = Atom.atom(String.implode str2)
															 in
																SOME (T.VAR_NAME value, strm)
															 end
														else
															 SOME (T.KW_let, strm3))
							| _ => let
									 val (str, strm) = stringBuilder [] strm0
									 val str2 = reverseList str
									 val value = Atom.atom(String.implode str2)
								   in
									 SOME (T.VAR_NAME value, strm)
								   end)
					| _ => let
							 val (str, strm) = stringBuilder [] strm0
							 val str2 = reverseList str
							 val value = Atom.atom(String.implode str2)
						   in
							 SOME (T.VAR_NAME value, strm)
						   end)
			| SOME (#"o", strm1) =>
				 (case charRdr strm1 of
					 SOME (#"f", strm2) =>
						(case charRdr strm2 of
							 SOME (c, strm3) => if Char.isAlphaNum c orelse c=(#"_") orelse c=(#"'")
												then let
														val (str, strm) = stringBuilder [] strm0
														val str2 = reverseList str
														val value = Atom.atom(String.implode str2)
													 in	
														SOME (T.VAR_NAME value, strm)
													 end
												else
													SOME (T.KW_of, strm2))
					| SOME (#"r", strm2) =>
						(case charRdr strm2 of
							 SOME (#"e", strm3) =>
								(case charRdr strm3 of
									 SOME (#"l", strm4) =>
										(case charRdr strm4 of
											 SOME (#"s", strm5) =>
												(case charRdr strm5 of
													SOME (#"e", strm6) =>
														(case charRdr strm6 of
															SOME (c, strm7) => if Char.isAlphaNum c orelse c=(#"_") orelse c=(#"'")
																			   then let
																						val (str, strm) = stringBuilder [] strm0
																						val str2 = reverseList str
																						val value = Atom.atom(String.implode str2)
																					in
																						SOME (T.VAR_NAME value, strm)
																					end
																				else
																					SOME (T.KW_orelse, strm6))
													| _ => let
															 val (str, strm) = stringBuilder [] strm0
															 val str2 = reverseList str
															 val value = Atom.atom(String.implode str2)
														   in
															 SOME (T.VAR_NAME value, strm)
														   end)
											| _ => let
													 val (str, strm) = stringBuilder [] strm0
													 val str2 = reverseList str
													 val value = Atom.atom(String.implode str2)
												   in
													 SOME (T.VAR_NAME value, strm)
												   end)
									| _ => let
											 val (str, strm) = stringBuilder [] strm0
											 val str2 = reverseList str
											 val value = Atom.atom(String.implode str2)
										   in
											 SOME (T.VAR_NAME value, strm)
										   end)
							| _ => let
									val (str, strm) = stringBuilder [] strm0
									val str2 = reverseList str
									val value = Atom.atom(String.implode str2)
								   in
									SOME (T.VAR_NAME value, strm)
								   end)
					| _ => let
							 val (str, strm) = stringBuilder [] strm0
							 val str2 = reverseList str
							 val value = Atom.atom(String.implode str2)
						   in
							 SOME (T.VAR_NAME value, strm)
						   end)
			| SOME (#"t", strm1) =>
				(case charRdr strm1 of
					 SOME (#"h", strm2) =>
						(case charRdr strm2 of
							 SOME (#"e", strm3) =>
								(case charRdr strm3 of
									 SOME (#"n", strm4) => 
										(case charRdr strm4 of
											SOME (c, strm5) => if Char.isAlphaNum c orelse c=(#"_") orelse c=(#"'")
															   then let
																		val (str, strm) = stringBuilder [] strm0
																		val str2 = reverseList str
																		val value = Atom.atom(String.implode str2)
																	in
																		SOME (T.VAR_NAME value, strm)
																	end
																else
																	SOME (T.KW_then, strm4))
									| _ => let
											 val (str, strm) = stringBuilder [] strm0
											 val str2 = reverseList str
											 val value = Atom.atom(String.implode str2)
										   in
											 SOME (T.VAR_NAME value, strm)
										   end)
							| _ => let
									val (str, strm) = stringBuilder [] strm0
									val str2 = reverseList str
									val value = Atom.atom(String.implode str2)
								   in
									SOME (T.VAR_NAME value, strm)
								   end)
					| SOME (#"y", strm2) =>
						(case charRdr strm2 of
							 SOME (#"p", strm3) =>
								(case charRdr strm3 of
									 SOME (#"e", strm4) =>
										(case charRdr strm4 of
											SOME (c, strm5) => if Char.isAlphaNum c orelse c=(#"_") orelse c=(#"'")
															   then let
																		val (str, strm) = stringBuilder [] strm0
																		val str2 = reverseList str
																		val value = Atom.atom(String.implode str2)
																	in
																		SOME (T.VAR_NAME value, strm)
																	end
																else
																	SOME (T.KW_type, strm4))
									| _ => let
											 val (str, strm) = stringBuilder [] strm0
											 val str2 = reverseList str
											 val value = Atom.atom(String.implode str2)
										   in
											 SOME (T.VAR_NAME value, strm)
										   end)
							| _ => let
									 val (str, strm) = stringBuilder [] strm0
									 val str2 = reverseList str
									 val value = Atom.atom(String.implode str2)
								   in	
									 SOME (T.VAR_NAME value, strm)
								   end)
					| _ => let
							val (str, strm) = stringBuilder [] strm0
							val str2 = reverseList str
							val value = Atom.atom(String.implode str2)
						   in
							SOME (T.VAR_NAME value, strm)
						   end)
			| SOME (#"v", strm1) =>
				(case charRdr strm1 of
					 SOME (#"a", strm2) =>
						(case charRdr strm2 of
							 SOME (#"l", strm3) =>
								(case charRdr strm3 of
									SOME (c, strm4) => if Char.isAlphaNum c orelse c=(#"_") orelse c=(#"'")
													   then let
																val (str, strm) = stringBuilder [] strm0
																val str2 = reverseList str
																val value = Atom.atom(String.implode str2)
															in
																SOME (T.VAR_NAME value, strm)
															end
														else
															SOME (T.KW_val, strm3))
							| _ => let
									val (str, strm) = stringBuilder [] strm0
									val str2 = reverseList str
									val value = Atom.atom(String.implode str2)
								   in
									SOME (T.VAR_NAME value, strm)
								   end)
					| _ => let
							 val (str, strm) = stringBuilder [] strm0
							 val str2 = reverseList str
							 val value = Atom.atom(String.implode str2)
						   in
							 SOME (T.VAR_NAME value, strm)
						   end)
			(* identifiers and literals *)
			| SOME (#"\"", strm1) => let
										val (str, strm) = stringStringBuilder [] strm1
										val str2 = reverseList str
										val value = (String.implode str2)
									 in
										SOME (T.STRING value, strm)
									 end
			| SOME (c0, strm1) => if Char.isDigit c0
									  then let 
											val (num, strm) = integerBuilder [] strm1
											val str2 = reverseList num
											val strList = c0::str2
										in
											(case IntInf.fromString(String.implode strList) of
												NONE => NONE
												|SOME(i) => SOME (T.INTEGER i, strm))
										end
									  else if Char.isUpper c0
										then let
												val (conName, strm) = conNameBuilder [] strm1
												val str2 = reverseList conName
												val strList = c0::str2
												val value = Atom.atom(String.implode strList)
											 in
												SOME (T.CON_NAME value, strm)
											 end
										else if c0=(#"'")
												then 
													(case charRdr strm1 of
														SOME (c1, strm2) => if Char.isLower c1
																				then let
																						val (typeName, strm) = typeNameBuilder [] strm1
																						val str2 = reverseList typeName
																						val strList = c0::str2
																						val value = Atom.atom(String.implode strList)
																					 in
																						SOME (T.TYVAR_NAME value, strm)
																					 end
																			else
																				(badTypeNameChar c1; scanTok strm1))
											 else if Char.isLower c0
													then let
															val (typeName, strm) = stringBuilder [] strm1
															val str2 = reverseList typeName
															val strList = c0::str2
															val value = Atom.atom(String.implode strList)
														 in
															SOME (T.VAR_NAME value, strm)
														 end
												 else if Char.isSpace c0
														then scanTok strm1
														(* add more 'else if's for other classes of initial character *)
													  else (badChar c0; scanTok strm1) 
														(* add more matches for other specific initial characters *)
                | NONE => NONE
     in
        scanTok
     end
	 
end