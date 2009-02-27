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
     TOKENNAME = 258,
     INTCONST = 259,
     REALCONST = 260,
     CHARCONST = 261,
     VARIABLE = 262,
     FUNCTION = 263,
     INTEGER = 264,
     REAL = 265,
     CHARACTER = 266,
     VECTOR = 267,
     WHILE = 268,
     IF = 269,
     PRINT = 270,
     IFX = 271,
     ELSE = 272,
     NE = 273,
     EQ = 274,
     LE = 275,
     GE = 276,
     UMINUS = 277
   };
#endif
/* Tokens.  */
#define TOKENNAME 258
#define INTCONST 259
#define REALCONST 260
#define CHARCONST 261
#define VARIABLE 262
#define FUNCTION 263
#define INTEGER 264
#define REAL 265
#define CHARACTER 266
#define VECTOR 267
#define WHILE 268
#define IF 269
#define PRINT 270
#define IFX 271
#define ELSE 272
#define NE 273
#define EQ 274
#define LE 275
#define GE 276
#define UMINUS 277




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 17 "grammar.yy"
{
	int functionId;			/* function id */
	Dnchar *name;			/* character pointer for names */
	TreeNode *node;			/* node pointer */
}
/* Line 1489 of yacc.c.  */
#line 99 "grammar.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

