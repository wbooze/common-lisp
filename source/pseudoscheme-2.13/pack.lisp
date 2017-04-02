; Package definitions for Pseudoscheme.

; If your Common Lisp doesn't have DEFPACKAGE, you'll have to translate
; this file manually into appropriate IN-PACKAGE and EXPORT forms.

(in-package "USER")  ;stifle warnings from compiler/loader

; The SCHEME package is where Scheme symbols live.

(defpackage "SCHEME"
  (:use )
  (:export ))

; Define the Pseudoscheme package.

(defpackage "PS"
  (:nicknames "PS-LISP" "PSEUDOSCHEME")
  (:use #.(if (find-package "COMMON-LISP") ;Avoid pollution.
	      "COMMON-LISP"
	      "LISP"))
  (:export "SET!-AUX"
	   "SET-FUNCTION-FROM-VALUE"
	   "SET-VALUE-FROM-FUNCTION"
	   "SET-FORWARDING-FUNCTION"
	   "UNSPECIFIC"
	   "UNASSIGNED"
	   "TRUE"			; #t
	   "FALSE"			; #f
	   "TRUE?"			; CL boolean -> Scheme boolean
	   "TRUEP"			; Scheme boolean -> CL boolean
	   "BEGIN-TRANSLATED-FILE"	;prelude
	   "AT-TOP-LEVEL"  		;kludge for symbolics lossage
	   "MAYBE-FIX-&REST-PARAMETER"	;ditto
	   "%DEFINE-SYNTAX!"
	   ;; Additional auxiliaries for Revised^4 builtins
	   "SCHEME-PACKAGE"		;for STRING->SYMBOL
	   "PROCEDUREP"			;for PROCEDURE?
	   "SCHEME-SYMBOL-P"		;for SYMBOL?
	   "SCHEME-EQUAL-P"		;for MEMBER, ASSOC, EQUAL?
	   "BOOLEANP"
	   "CHAR-WHITESPACE-P"
	   "INPUT-PORT-P"
	   "OUTPUT-PORT-P"
	   "REALP"
	   ;; Additional auxiliaries for Revised^4 non-builtins
	   "SCHEME-LOAD"		;forward reference from RTS to EVAL
	   "SCHEME-READTABLE"		;for READ
	   "EOF-OBJECT"			;for READ, READ-CHAR
	   ;; ... and for Revised^5
	   "SCHEME-EVAL"
	   "SCHEME-REPORT-ENVIRONMENT"
	   "*CURRENT-REP-ENVIRONMENT*"
	   ;; Random
	   "SCHEME-ERROR"
	   "SCHEME-WARN"
	   "SCHEME-USER-ENVIRONMENT"
	   "SCHEME-READ-USING-COMMONLISP-READER"
	   "*DEFINE-SYNTAX!*"

	   ;; Invoking the translator
	   "SCHEME-COMPILE"
	   "SCHEME-COMPILE-FILE"
	   "TRANSLATE-FILE"

	   ;; REP loop
	   "SET-REP-ENVIRONMENT!"
	   "SCHEME"
	   "QUIT"

	   ;; Handy
	   "PP"
	   "BENCHMARK-MODE"

	   ;; Symbols in the LISP (or COMMON-LISP) package used by
	   ;; translated programs and by the translator itself.
	   "FRESH-LINE"      ;Added by JAR 10/8/1999
	   "&BODY" ;rts.lisp
	   "&OPTIONAL"
	   "&REST"
	   "*"
	   "*DEFAULT-PATHNAME-DEFAULTS*"
	   "*FEATURES*"
	   "*PACKAGE*"
	   "*PRINT-BASE*"
	   "*PRINT-CASE*"
	   "*QUERY-IO*"
	   "*READTABLE*"
	   "*READ-BASE*" ;rts.lisp
	   "*SHARP-SHARP*" ;rts.lisp
	   "*SCHEME-READ*"
	   "*SCHEME-WRITE*"
	   "*SCHEME-DISPLAY*"
	   "*STANDARD-INPUT*"
	   "*STANDARD-OUTPUT*"
	   "+"
	   "-"
	   "/"
	   "<"
	   "<="
	   "="
	   ">"
	   ">="
	   "ABS"
	   "ACOS"
	   "ALPHA-CHAR-P"
	   "AND"
	   "APPEND"
	   "APPLY"
	   "ASIN"
	   "ASSOC"
	   "ATAN"
	   "BLOCK"
	   "BOUNDP"
	   "CAAAAR"
	   "CAAADR"
	   "CAAAR"
	   "CAADAR"
	   "CAADDR"
	   "CAADR"
	   "CAAR"
	   "CADAAR"
	   "CADADR"
	   "CADAR"
	   "CADDAR"
	   "CADDDR"
	   "CADDR"
	   "CADR"
	   "CAR"
	   "CASE"
	   "CDAAAR"
	   "CDAADR"
	   "CDAAR"
	   "CDADAR"
	   "CDADDR"
	   "CDADR"
	   "CDAR"
	   "CDDAAR"
	   "CDDADR"
	   "CDDAR"
	   "CDDDAR"
	   "CDDDDR"
	   "CDDDR"
	   "CDDR"
	   "CDR"
	   "CEILING"
	   "CHAR"
	   "CHAR-CODE"
	   "CHAR-DOWNCASE"
	   "CHAR-EQUAL"
	   "CHAR-GREATERP"
	   "CHAR-LESSP"
	   "CHAR-NOT-GREATERP"
	   "CHAR-NOT-LESSP"
	   "CHAR-UPCASE"
	   "CHAR<"
	   "CHAR<="
	   "CHAR="
	   "CHAR>"
	   "CHAR>="
	   "CHARACTERP"
	   "CIS"
	   "CLOSE"
	   "CODE-CHAR"
	   "COERCE"
	   "COMPILE"
	   "COMPILE-FILE" ;helps with debugging
	   "COMPILED-FUNCTION-P"
	   "COMPLEX"
	   "COND"  ;?
	   "CONS"
	   "CONSP"
	   "COPY-LIST"
	   "COPY-READTABLE"
	   "COPY-SEQ"
	   "COS"
	   "DECLARE"
	   "DEFMACRO" ;for rts.lisp
	   "DEFPACKAGE"
	   "DEFSTRUCT"
	   "DEFTYPE"
	   "DEFUN"
	   "DEFVAR"  ;read, write
	   "DENOMINATOR"
	   "DIGIT-CHAR-P"
	   "DO"  ;?
	   "DRIBBLE"
	   "EQ"
	   "EQL"
	   "EQUAL"
	   "ERROR"
	   "EVAL"
	   "EVENP"
	   "EXP"
	   "EXPORT"
	   "EXPT"
	   "FBOUNDP"
	   "FILL"
	   "FIND-PACKAGE"
	   "FLET"
	   "FLOAT"
	   "FLOATP"
	   "FLOOR"
	   "FORMAT"
	   "FUNCALL"
	   "FUNCTION"
	   "GCD"
	   "GENSYM"
	   "GET"
	   "GETHASH"
	   "GO"
	   "IF"
	   "IGNORE" ;rts.lisp
	   "IMAGPART"
	   "IMPORT"
	   "IN-PACKAGE"
	   "INTEGERP"
	   "INTERN"
	   "LABELS"
	   "LAMBDA"
	   "LAST"
	   "LCM"
	   "LENGTH"
	   "LET"
	   "LET*"  ;?
	   "LISP-IMPLEMENTATION-TYPE"
	   "LISP-IMPLEMENTATION-VERSION"
	   "LIST"
	   "LISTEN"
	   "LOAD"
	   "LOCALLY"
	   "LOG"
	   "LOWER-CASE-P"
	   "MACRO-FUNCTION"
	   "MAKE-HASH-TABLE"
	   "MAKE-PACKAGE"
	   "MAKE-PATHNAME"
	   "MAKE-STRING" ;?
	   "MAP" ;?
	   "MAPC"
	   "MAPCAR"
	   "MAX"
	   "MEMBER"
	   "MERGE-PATHNAMES"
	   "MIN"
	   "MINUSP"
	   "MOD"
	   "MULTIPLE-VALUE-BIND"
	   "MULTIPLE-VALUE-CALL"
	   "NAMESTRING"
	   "NIL"
	   "NOT"
	   "NTH"
	   "NTHCDR"
	   "NULL"
	   "NUMBERP"
	   "NUMERATOR"
	   "ODDP"
	   "OPEN"
	   "OR"
	   "OTHERWISE"
	   "PACKAGE-NAME"
	   "PACKAGE-USE-LIST"
	   "PATHNAME"
	   "PEEK-CHAR"
	   "PHASE"
	   "PLUSP"
	   "POSITION"
	   "PRIN1"
	   "PRINC"
	   "PROG"
	   "PROGN"
	   "PROGV"
	   "PSETQ"
	   "QUOTE"
	   "RATIONALIZE"
	   "RATIONALP"
	   "READ"
	   "READ-CHAR"
	   "REALPART"
	   "REM"
	   "REMOVE"
	   "RENAME-PACKAGE"
	   "RETURN"
	   "RETURN-FROM"
	   "REVERSE"
	   "ROUND"
	   "SET"
	   "SET-MACRO-CHARACTER"
	   "SETF"
	   "SETQ"
	   "SIMPLE-STRING"
	   "SIMPLE-STRING-P"
	   "SIMPLE-VECTOR"
	   "SIN"
	   "SPECIAL"
	   "SQRT"
	   "STRING" ;?
	   "STRING-CAPITALIZE"
	   "STRING-DOWNCASE"
	   "STRING-EQUAL"
	   "STRING-GREATERP"
	   "STRING-LESSP"
	   "STRING-NOT-GREATERP"
	   "STRING-NOT-LESSP"
	   "STRING-UPCASE"
	   "STRING<"
	   "STRING<="
	   "STRING="
	   "STRING>"
	   "STRING>="
	   "SUBLIS"
	   "SUBSEQ"
	   "SVREF"
	   "SYMBOL-FUNCTION"
	   "SYMBOL-NAME"
	   "SYMBOL-PACKAGE"
	   "SYMBOL-VALUE"
	   "SYMBOLP" ;rts.lisp
	   "T"
	   "TAN"
	   "TERPRI"
	   "THE"
	   "TRUENAME"
	   "TRUNCATE"
	   "TYPE-OF"
	   "UNLESS"
	   "UNUSE-PACKAGE"
	   "UNWIND-PROTECT" ;rts.lisp
	   "UPPER-CASE-P"
	   "USE-PACKAGE"
	   "VALUES"
	   "VALUES-LIST"
	   "VECTOR"
	   "WARN"
	   "WHEN" ;rts.lisp
	   "WITH-OPEN-FILE"
	   "WRITE"
	   "WRITE-CHAR"
	   "WRITE-TO-STRING"
	   "Y-OR-N-P"
	   "ZEROP"
	   ;; rts.lisp:
	   "MAKE-SEQUENCE"
	   "READ-PRESERVING-WHITESPACE"
	   "FIND-IF"
	   "INLINE"
	   "WITH-INPUT-FROM-STRING"
	   "FIND-SYMBOL"
	   "CONCATENATE"
	   "KEYWORDP"
	   "PROCLAIM"
	   "SIMPLE-VECTOR-P"
	   ))

; Lose

(defpackage "CLEVER-LOAD"
  (:use #.(if (find-package "COMMON-LISP") ;Avoid pollution.
	      "COMMON-LISP"
	      "LISP"))
  (:export "CLEVER-LOAD"
	   "*COMPILE-IF-NECESSARY-P*"))
