/* A Bison parser, made by GNU Bison 2.5.  */

/* Bison interface for Yacc-like parsers in C
   
      Copyright (C) 1984, 1989-1990, 2000-2011 Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     INTCONST = 258,
     ELEMENTCONST = 259,
     DOUBLECONST = 260,
     NEWTOKEN = 261,
     CHARCONST = 262,
     STEPTOKEN = 263,
     VAR = 264,
     VARSAMESCOPE = 265,
     FUNCCALL = 266,
     USERFUNCCALL = 267,
     VTYPE = 268,
     ATEN_DO = 269,
     ATEN_WHILE = 270,
     ATEN_FOR = 271,
     ATEN_SWITCH = 272,
     ATEN_CASE = 273,
     ATEN_DEFAULT = 274,
     ATEN_IF = 275,
     ATEN_IIF = 276,
     ATEN_IN = 277,
     ATEN_GLOBAL = 278,
     ATEN_RETURN = 279,
     FILTERBLOCK = 280,
     HELP = 281,
     ATEN_VOID = 282,
     ATEN_CONTINUE = 283,
     ATEN_BREAK = 284,
     ATEN_NEW = 285,
     ATEN_ELSE = 286,
     DEQ = 287,
     TEQ = 288,
     MEQ = 289,
     PEQ = 290,
     OR = 291,
     AND = 292,
     NEQ = 293,
     EQ = 294,
     GEQ = 295,
     LEQ = 296,
     UMINUS = 297,
     UPLUS = 298,
     MINUSMINUS = 299,
     PLUSPLUS = 300
   };
#endif
/* Tokens.  */
#define INTCONST 258
#define ELEMENTCONST 259
#define DOUBLECONST 260
#define NEWTOKEN 261
#define CHARCONST 262
#define STEPTOKEN 263
#define VAR 264
#define VARSAMESCOPE 265
#define FUNCCALL 266
#define USERFUNCCALL 267
#define VTYPE 268
#define ATEN_DO 269
#define ATEN_WHILE 270
#define ATEN_FOR 271
#define ATEN_SWITCH 272
#define ATEN_CASE 273
#define ATEN_DEFAULT 274
#define ATEN_IF 275
#define ATEN_IIF 276
#define ATEN_IN 277
#define ATEN_GLOBAL 278
#define ATEN_RETURN 279
#define FILTERBLOCK 280
#define HELP 281
#define ATEN_VOID 282
#define ATEN_CONTINUE 283
#define ATEN_BREAK 284
#define ATEN_NEW 285
#define ATEN_ELSE 286
#define DEQ 287
#define TEQ 288
#define MEQ 289
#define PEQ 290
#define OR 291
#define AND 292
#define NEQ 293
#define EQ 294
#define GEQ 295
#define LEQ 296
#define UMINUS 297
#define UPLUS 298
#define MINUSMINUS 299
#define PLUSPLUS 300




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 2068 of yacc.c  */
#line 31 "grammar.yy"

	int functionId;			/* Function enum id */
	Dnchar *name;			/* character pointer for names */
	TreeNode *node;			/* node pointer */
	Variable *variable;		/* variable pointer */
	Tree *tree;			/* function (tree) pointer */
	VTypes::DataType vtype;		/* variable type for next declarations */
	int intconst;			/* integer constant value */
	double doubleconst;		/* double constant value */



/* Line 2068 of yacc.c  */
#line 153 "grammar.hh"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE CommandParser_lval;


