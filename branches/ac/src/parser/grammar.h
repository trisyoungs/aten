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
     DOUBLECONST = 259,
     NEWTOKEN = 260,
     STEPTOKEN = 261,
     VAR = 262,
     LOCALVAR = 263,
     FUNCCALL = 264,
     USERFUNCCALL = 265,
     VTYPE = 266,
     DO = 267,
     WHILE = 268,
     FOR = 269,
     IF = 270,
     RETURN = 271,
     DUMMY = 272,
     OPTION = 273,
     ELSE = 274,
     OR = 275,
     AND = 276,
     DEQ = 277,
     TEQ = 278,
     MEQ = 279,
     PEQ = 280,
     NEQ = 281,
     EQ = 282,
     LEQ = 283,
     GEQ = 284,
     UMINUS = 285,
     MINUSMINUS = 286,
     PLUSPLUS = 287
   };
#endif
/* Tokens.  */
#define INTCONST 258
#define DOUBLECONST 259
#define NEWTOKEN 260
#define STEPTOKEN 261
#define VAR 262
#define LOCALVAR 263
#define FUNCCALL 264
#define USERFUNCCALL 265
#define VTYPE 266
#define DO 267
#define WHILE 268
#define FOR 269
#define IF 270
#define RETURN 271
#define DUMMY 272
#define OPTION 273
#define ELSE 274
#define OR 275
#define AND 276
#define DEQ 277
#define TEQ 278
#define MEQ 279
#define PEQ 280
#define NEQ 281
#define EQ 282
#define LEQ 283
#define GEQ 284
#define UMINUS 285
#define MINUSMINUS 286
#define PLUSPLUS 287




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
#line 124 "grammar.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE CommandParser_lval;

