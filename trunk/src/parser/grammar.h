
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton interface for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
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
     NEWFUNCTOKEN = 262,
     CHARCONST = 263,
     STEPTOKEN = 264,
     VAR = 265,
     LOCALVAR = 266,
     FUNCCALL = 267,
     USERFUNCCALL = 268,
     VTYPE = 269,
     DO = 270,
     WHILE = 271,
     FOR = 272,
     IF = 273,
     IN = 274,
     RETURN = 275,
     FILTERBLOCK = 276,
     HELP = 277,
     DIOV = 278,
     DUMMY = 279,
     OPTION = 280,
     ELSE = 281,
     OR = 282,
     AND = 283,
     DEQ = 284,
     TEQ = 285,
     MEQ = 286,
     PEQ = 287,
     NEQ = 288,
     EQ = 289,
     LEQ = 290,
     GEQ = 291,
     UMINUS = 292,
     MINUSMINUS = 293,
     PLUSPLUS = 294
   };
#endif
/* Tokens.  */
#define INTCONST 258
#define ELEMENTCONST 259
#define DOUBLECONST 260
#define NEWTOKEN 261
#define NEWFUNCTOKEN 262
#define CHARCONST 263
#define STEPTOKEN 264
#define VAR 265
#define LOCALVAR 266
#define FUNCCALL 267
#define USERFUNCCALL 268
#define VTYPE 269
#define DO 270
#define WHILE 271
#define FOR 272
#define IF 273
#define IN 274
#define RETURN 275
#define FILTERBLOCK 276
#define HELP 277
#define DIOV 278
#define DUMMY 279
#define OPTION 280
#define ELSE 281
#define OR 282
#define AND 283
#define DEQ 284
#define TEQ 285
#define MEQ 286
#define PEQ 287
#define NEQ 288
#define EQ 289
#define LEQ 290
#define GEQ 291
#define UMINUS 292
#define MINUSMINUS 293
#define PLUSPLUS 294




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 1676 of yacc.c  */
#line 30 "grammar.yy"

	int functionId;			/* Function enum id */
	Dnchar *name;			/* character pointer for names */
	TreeNode *node;			/* node pointer */
	Variable *variable;		/* variable pointer */
	Tree *functree;			/* user-defined function (tree) pointer */
	VTypes::DataType vtype;		/* variable type for next declarations */
	int intconst;			/* integer constant value */
	double doubleconst;		/* double constant value */



/* Line 1676 of yacc.c  */
#line 143 "grammar.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE CommandParser_lval;


