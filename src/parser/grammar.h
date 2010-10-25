/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

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
     RETURN = 274,
     FILTERBLOCK = 275,
     HELP = 276,
     DIOV = 277,
     DUMMY = 278,
     OPTION = 279,
     ELSE = 280,
     OR = 281,
     AND = 282,
     DEQ = 283,
     TEQ = 284,
     MEQ = 285,
     PEQ = 286,
     NEQ = 287,
     EQ = 288,
     LEQ = 289,
     GEQ = 290,
     UMINUS = 291,
     MINUSMINUS = 292,
     PLUSPLUS = 293
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
#define RETURN 274
#define FILTERBLOCK 275
#define HELP 276
#define DIOV 277
#define DUMMY 278
#define OPTION 279
#define ELSE 280
#define OR 281
#define AND 282
#define DEQ 283
#define TEQ 284
#define MEQ 285
#define PEQ 286
#define NEQ 287
#define EQ 288
#define LEQ 289
#define GEQ 290
#define UMINUS 291
#define MINUSMINUS 292
#define PLUSPLUS 293




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 29 "grammar.yy"
{
	int functionId;			/* Function enum id */
	Dnchar *name;			/* character pointer for names */
	TreeNode *node;			/* node pointer */
	Variable *variable;		/* variable pointer */
	Tree *functree;			/* user-defined function (tree) pointer */
	VTypes::DataType vtype;		/* variable type for next declarations */
	int intconst;			/* integer constant value */
	double doubleconst;		/* double constant value */
}
/* Line 1489 of yacc.c.  */
#line 136 "grammar.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE CommandParser_lval;

