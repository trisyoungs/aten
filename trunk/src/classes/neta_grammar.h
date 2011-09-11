
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
     IF = 258,
     OR = 259,
     AND = 260,
     NEQ = 261,
     EQ = 262,
     LEQ = 263,
     GEQ = 264,
     INTCONST = 265,
     ELEMENT = 266,
     TYPEREF = 267,
     NETAKEY = 268,
     NETAVAL = 269,
     NETARING = 270,
     NETACHAIN = 271,
     NETAGEOM = 272,
     TOKEN = 273
   };
#endif
/* Tokens.  */
#define IF 258
#define OR 259
#define AND 260
#define NEQ 261
#define EQ 262
#define LEQ 263
#define GEQ 264
#define INTCONST 265
#define ELEMENT 266
#define TYPEREF 267
#define NETAKEY 268
#define NETAVAL 269
#define NETARING 270
#define NETACHAIN 271
#define NETAGEOM 272
#define TOKEN 273




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 1676 of yacc.c  */
#line 27 "neta_grammar.yy"

	NetaNode *netanode;			/* Generic node pointer */
	NetaBoundNode *boundnode;		/* Bound node pointer */
	NetaRingNode *ringnode;			/* Ring node pointer */
	NetaChainNode *chainnode;		/* Chain node pointer */
	int intconst;				/* Integer number */
	Neta::NetaKeyword netakey;		/* NETA keyword ID */
	Atom::AtomGeometry atomgeom;		/* NETA geometry ID */
	Neta::NetaValue netaval;		/* NETA value ID */
	Refitem<ForcefieldAtom,int> *typelist;	/* Pointer to head of created element/type list */



/* Line 1676 of yacc.c  */
#line 102 "neta_grammar.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE NetaParser_lval;


