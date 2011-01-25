/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0

/* Substitute the variable and function names.  */
#define yyparse CommandParser_parse
#define yylex   CommandParser_lex
#define yyerror CommandParser_error
#define yylval  CommandParser_lval
#define yychar  CommandParser_char
#define yydebug CommandParser_debug
#define yynerrs CommandParser_nerrs


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
     LOCALVAR = 265,
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
     ATEN_IN = 276,
     ATEN_RETURN = 277,
     FILTERBLOCK = 278,
     HELP = 279,
     ATEN_VOID = 280,
     ATEN_CONTINUE = 281,
     ATEN_BREAK = 282,
     OPTION = 283,
     ATEN_ELSE = 284,
     OR = 285,
     AND = 286,
     DEQ = 287,
     TEQ = 288,
     MEQ = 289,
     PEQ = 290,
     NEQ = 291,
     EQ = 292,
     LEQ = 293,
     GEQ = 294,
     UMINUS = 295,
     MINUSMINUS = 296,
     PLUSPLUS = 297
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
#define LOCALVAR 265
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
#define ATEN_IN 276
#define ATEN_RETURN 277
#define FILTERBLOCK 278
#define HELP 279
#define ATEN_VOID 280
#define ATEN_CONTINUE 281
#define ATEN_BREAK 282
#define OPTION 283
#define ATEN_ELSE 284
#define OR 285
#define AND 286
#define DEQ 287
#define TEQ 288
#define MEQ 289
#define PEQ 290
#define NEQ 291
#define EQ 292
#define LEQ 293
#define GEQ 294
#define UMINUS 295
#define MINUSMINUS 296
#define PLUSPLUS 297




/* Copy the first part of user declarations.  */
#line 4 "grammar.yy"


/* Includes */
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "command/commands.h"
#include "parser/parser.h"
#include "parser/tree.h"

/* Prototypes */
int CommandParser_lex(void);
void CommandParser_error(char *s);

/* Local Variables */
Dnchar tokenName;
List<Dnchar> stepNameStack;
VTypes::DataType declaredType, funcType;
TreeNode *tempNode;



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 30 "grammar.yy"
{
	int functionId;			/* Function enum id */
	Dnchar *name;			/* character pointer for names */
	TreeNode *node;			/* node pointer */
	Variable *variable;		/* variable pointer */
	Tree *tree;			/* function (tree) pointer */
	VTypes::DataType vtype;		/* variable type for next declarations */
	int intconst;			/* integer constant value */
	double doubleconst;		/* double constant value */
}
/* Line 187 of yacc.c.  */
#line 221 "grammar.cc"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 234 "grammar.cc"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  75
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1362

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  63
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  43
/* YYNRULES -- Number of rules.  */
#define YYNRULES  143
/* YYNRULES -- Number of states.  */
#define YYNSTATES  290

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   297

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    51,     2,     2,     2,    47,     2,     2,
      55,    56,    45,    43,    60,    44,    57,    46,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    62,    61,
      38,    32,    37,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    53,     2,    54,    52,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    58,     2,    59,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    33,    34,    35,
      36,    39,    40,    41,    42,    48,    49,    50
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     8,    10,    12,    14,    16,    18,
      20,    26,    32,    37,    40,    42,    46,    49,    54,    56,
      61,    63,    64,    69,    72,    76,    81,    84,    88,    93,
      96,   101,   105,   109,   113,   117,   119,   121,   123,   127,
     131,   135,   139,   142,   145,   148,   151,   154,   156,   160,
     164,   168,   172,   176,   180,   184,   188,   192,   196,   200,
     204,   208,   212,   216,   219,   221,   223,   227,   230,   232,
     234,   236,   238,   240,   242,   245,   249,   253,   260,   267,
     271,   273,   278,   280,   282,   286,   289,   293,   299,   301,
     305,   308,   312,   316,   324,   332,   333,   343,   346,   349,
     352,   355,   358,   360,   363,   365,   367,   370,   374,   377,
     380,   383,   385,   388,   394,   397,   399,   401,   409,   415,
     426,   437,   438,   448,   449,   461,   468,   477,   485,   491,
     494,   496,   499,   502,   507,   514,   521,   522,   523,   524,
     525,   526,   527,   528
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      64,     0,    -1,    65,    -1,    64,    65,    -1,    88,    -1,
      89,    -1,     3,    -1,     5,    -1,     7,    -1,     4,    -1,
       8,   103,    53,    76,    54,    -1,     8,   103,    55,    77,
      56,    -1,     8,   103,    55,    56,    -1,     8,   103,    -1,
      67,    -1,    68,    57,    67,    -1,    68,     1,    -1,     9,
      53,    76,    54,    -1,     9,    -1,    10,    53,    76,    54,
      -1,    10,    -1,    -1,    69,    57,    70,    68,    -1,    69,
      55,    -1,    11,    55,    56,    -1,    11,    55,    77,    56,
      -1,    11,     1,    -1,    12,    55,    56,    -1,    12,    55,
      77,    56,    -1,    12,     1,    -1,    28,    55,    77,    56,
      -1,    58,    77,    59,    -1,    69,    32,    76,    -1,    69,
      32,    74,    -1,    69,    32,     1,    -1,    66,    -1,    71,
      -1,    72,    -1,    69,    36,    76,    -1,    69,    35,    76,
      -1,    69,    34,    76,    -1,    69,    33,    76,    -1,    44,
      76,    -1,    69,    50,    -1,    69,    49,    -1,    50,    69,
      -1,    49,    69,    -1,    69,    -1,    76,    43,    76,    -1,
      76,    44,    76,    -1,    76,    45,    76,    -1,    76,    46,
      76,    -1,    76,    52,    76,    -1,    76,    47,    76,    -1,
      76,    40,    76,    -1,    76,    39,    76,    -1,    76,    37,
      76,    -1,    76,    42,    76,    -1,    76,    38,    76,    -1,
      76,    41,    76,    -1,    76,    31,    76,    -1,    76,    30,
      76,    -1,    55,    76,    56,    -1,    51,    76,    -1,     6,
      -1,    76,    -1,    77,    60,    76,    -1,    77,    76,    -1,
       9,    -1,    11,    -1,    10,    -1,    66,    -1,    12,    -1,
      13,    -1,     6,    98,    -1,    78,    32,    74,    -1,    78,
      32,    73,    -1,    78,    53,    76,    54,    32,    76,    -1,
      78,    53,    76,    54,    32,    74,    -1,    78,    32,    76,
      -1,    78,    -1,    78,    53,    76,    54,    -1,    79,    -1,
      80,    -1,    81,    60,    80,    -1,    81,    80,    -1,    13,
      99,    78,    -1,    13,    99,    78,    32,    76,    -1,    82,
      -1,    83,    60,    82,    -1,    83,    82,    -1,    13,    99,
      81,    -1,    13,    99,     1,    -1,    25,   100,     6,   104,
      55,    56,    89,    -1,    13,    99,     6,   104,    55,    56,
      89,    -1,    -1,    13,    99,     6,   104,    55,    83,    56,
      86,    89,    -1,    75,    61,    -1,    84,    61,    -1,    76,
      61,    -1,    71,    61,    -1,    72,    61,    -1,    91,    -1,
      73,    61,    -1,    85,    -1,    97,    -1,    24,    11,    -1,
      22,    76,    61,    -1,    22,    61,    -1,    26,    61,    -1,
      27,    61,    -1,    87,    -1,    88,    87,    -1,    58,   101,
      88,    59,   102,    -1,    58,    59,    -1,    87,    -1,    89,
      -1,    20,    55,    76,    56,    90,    29,    90,    -1,    20,
      55,    76,    56,    90,    -1,    16,   101,    55,    75,    61,
      76,    61,    76,    56,    90,    -1,    16,   101,    55,    84,
      61,    76,    61,    76,    56,    90,    -1,    -1,    16,   101,
      55,    69,    21,    76,    56,    92,    90,    -1,    -1,    16,
     101,    55,    13,    99,    78,    21,    76,    56,    93,    90,
      -1,    15,   101,    55,    76,    56,    90,    -1,    14,   101,
      89,    15,    55,    76,    56,    61,    -1,    17,    55,    76,
      56,    58,    95,    59,    -1,    18,    55,    76,    56,    62,
      -1,    19,    62,    -1,    94,    -1,    95,    88,    -1,    95,
      94,    -1,     6,    98,    32,    66,    -1,    96,    60,     6,
      98,    32,    66,    -1,    23,   105,    55,    96,    56,    89,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    77,    77,    78,    83,    86,    96,    97,    98,    99,
     108,   112,   116,   120,   128,   129,   130,   135,   139,   143,
     147,   151,   151,   156,   167,   171,   175,   183,   187,   191,
     203,   210,   220,   221,   222,   227,   228,   229,   230,   231,
     232,   233,   234,   235,   236,   237,   238,   239,   240,   241,
     242,   243,   244,   245,   246,   247,   248,   249,   250,   251,
     252,   253,   254,   255,   256,   261,   265,   268,   280,   285,
     290,   295,   300,   305,   310,   319,   323,   327,   331,   335,
     343,   347,   351,   358,   361,   364,   372,   376,   384,   387,
     390,   398,   403,   415,   421,   427,   427,   443,   446,   449,
     452,   455,   458,   461,   464,   467,   470,   473,   476,   479,
     482,   489,   492,   500,   503,   510,   513,   520,   523,   526,
     529,   532,   532,   539,   539,   555,   559,   563,   571,   575,
     583,   586,   589,   600,   604,   612,   624,   628,   632,   636,
     640,   644,   648,   656
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "INTCONST", "ELEMENTCONST",
  "DOUBLECONST", "NEWTOKEN", "CHARCONST", "STEPTOKEN", "VAR", "LOCALVAR",
  "FUNCCALL", "USERFUNCCALL", "VTYPE", "ATEN_DO", "ATEN_WHILE", "ATEN_FOR",
  "ATEN_SWITCH", "ATEN_CASE", "ATEN_DEFAULT", "ATEN_IF", "ATEN_IN",
  "ATEN_RETURN", "FILTERBLOCK", "HELP", "ATEN_VOID", "ATEN_CONTINUE",
  "ATEN_BREAK", "OPTION", "ATEN_ELSE", "OR", "AND", "'='", "DEQ", "TEQ",
  "MEQ", "PEQ", "'>'", "'<'", "NEQ", "EQ", "LEQ", "GEQ", "'+'", "'-'",
  "'*'", "'/'", "'%'", "UMINUS", "MINUSMINUS", "PLUSPLUS", "'!'", "'^'",
  "'['", "']'", "'('", "')'", "'.'", "'{'", "'}'", "','", "';'", "':'",
  "$accept", "programlist", "program", "constant", "step", "steplist",
  "variable", "@1", "function", "userfunction", "widget", "ARRAYCONST",
  "assignment", "expression", "expressionlist", "variablename",
  "assignedvariablename", "variablelistitem", "variablelist",
  "typedvariablelistitem", "typedvariablelist", "declaration",
  "functiondeclaration", "@2", "statement", "statementlist", "block",
  "blockment", "flowstatement", "@3", "@4", "caselabel", "caselist",
  "filteroptions", "filter", "savetokenname", "savetype", "cleartype",
  "pushscope", "popscope", "pushstepname", "pushfunc", "pushfilter", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,    61,   287,   288,   289,   290,    62,    60,   291,
     292,   293,   294,    43,    45,    42,    47,    37,   295,   296,
     297,    33,    94,    91,    93,    40,    41,    46,   123,   125,
      44,    59,    58
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    63,    64,    64,    65,    65,    66,    66,    66,    66,
      67,    67,    67,    67,    68,    68,    68,    69,    69,    69,
      69,    70,    69,    69,    71,    71,    71,    72,    72,    72,
      73,    74,    75,    75,    75,    76,    76,    76,    76,    76,
      76,    76,    76,    76,    76,    76,    76,    76,    76,    76,
      76,    76,    76,    76,    76,    76,    76,    76,    76,    76,
      76,    76,    76,    76,    76,    77,    77,    77,    78,    78,
      78,    78,    78,    78,    78,    79,    79,    79,    79,    79,
      80,    80,    80,    81,    81,    81,    82,    82,    83,    83,
      83,    84,    84,    85,    85,    86,    85,    87,    87,    87,
      87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
      87,    88,    88,    89,    89,    90,    90,    91,    91,    91,
      91,    92,    91,    93,    91,    91,    91,    91,    94,    94,
      95,    95,    95,    96,    96,    97,    98,    99,   100,   101,
     102,   103,   104,   105
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     1,     1,     1,     1,     1,     1,
       5,     5,     4,     2,     1,     3,     2,     4,     1,     4,
       1,     0,     4,     2,     3,     4,     2,     3,     4,     2,
       4,     3,     3,     3,     3,     1,     1,     1,     3,     3,
       3,     3,     2,     2,     2,     2,     2,     1,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     2,     1,     1,     3,     2,     1,     1,
       1,     1,     1,     1,     2,     3,     3,     6,     6,     3,
       1,     4,     1,     1,     3,     2,     3,     5,     1,     3,
       2,     3,     3,     7,     7,     0,     9,     2,     2,     2,
       2,     2,     1,     2,     1,     1,     2,     3,     2,     2,
       2,     1,     2,     5,     2,     1,     1,     7,     5,    10,
      10,     0,     9,     0,    11,     6,     8,     7,     5,     2,
       1,     2,     2,     4,     6,     6,     0,     0,     0,     0,
       0,     0,     0,     0
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     6,     9,     7,    64,     8,    18,    20,     0,     0,
     137,   139,   139,   139,     0,     0,     0,   143,     0,   138,
       0,     0,     0,     0,     0,     0,     0,     0,   139,     0,
       2,    35,    47,    36,    37,     0,     0,     0,     0,   104,
     111,     4,     5,   102,   105,     0,     0,    26,     0,    29,
       0,     0,     0,     0,     0,     0,     0,   108,    47,    36,
      37,     0,     0,   106,     0,   109,   110,     0,    42,    46,
      45,    63,     0,   114,     0,     1,     3,     0,     0,     0,
       0,     0,    44,    43,    23,    21,   100,   101,   103,    97,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    99,    98,   112,     0,     0,    24,
      65,     0,    27,     0,    92,   136,    68,    70,    69,    72,
      73,    71,    80,    82,    83,    91,     0,     0,     0,     0,
       0,   107,     0,   142,     0,    62,     0,    34,     0,    33,
      32,    41,    40,    39,    38,     0,    61,    60,    56,    58,
      55,    54,    59,    57,    48,    49,    50,    51,    53,    52,
      17,    19,    25,     0,    67,    28,    74,     0,     0,     0,
     136,     0,    85,     0,     0,   137,     0,     0,     0,     0,
       0,   136,     0,     0,    30,   140,     0,   141,    14,     0,
      66,     0,    76,    75,    79,     0,    84,     0,     0,     0,
       0,     0,     0,     0,   115,   116,   118,     0,     0,     0,
       0,   113,    31,    13,    16,     0,   137,     0,    88,     0,
      81,     0,   125,    80,     0,     0,     0,     0,     0,   130,
       0,     0,     0,   135,   136,     0,     0,     0,    15,     0,
      94,    95,     0,    90,     0,     0,     0,   121,     0,     0,
       0,   129,   127,   131,   132,   117,   133,     0,    93,     0,
      12,     0,    86,     0,    89,    78,    77,   126,     0,     0,
       0,     0,     0,     0,    10,    11,     0,    96,   123,   122,
       0,     0,     0,   134,    87,     0,   119,   120,   128,   124
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    29,    30,    31,   188,   189,    58,   145,    59,    60,
      35,   139,    36,    37,   111,   122,   123,   124,   125,   218,
     219,    38,    39,   263,   204,    41,   205,   206,    43,   269,
     285,   229,   230,   182,    44,   166,    51,    64,    52,   211,
     213,   167,    62
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -197
static const yytype_int16 yypact[] =
{
     772,  -197,  -197,  -197,  -197,  -197,   -33,   -25,    20,    26,
    -197,  -197,  -197,  -197,   -16,    -7,     7,  -197,    77,  -197,
      58,    63,    40,   360,    50,    50,   360,   360,    76,   416,
    -197,  -197,   213,    66,    81,    82,    84,   958,    96,  -197,
    -197,   936,  -197,  -197,  -197,   360,   360,  -197,   828,  -197,
     864,   308,    59,    86,   103,   360,   360,  -197,   420,  -197,
    -197,   983,   104,  -197,   131,  -197,  -197,   360,   108,    -3,
      -3,   108,   351,  -197,   936,  -197,  -197,    60,   360,   360,
     360,   360,  -197,  -197,  -197,  -197,  -197,  -197,  -197,  -197,
     360,   360,   360,   360,   360,   360,   360,   360,   360,   360,
     360,   360,   360,   360,  -197,  -197,  -197,  1238,  1256,  -197,
    1310,   161,  -197,   561,  -197,   106,  -197,  -197,  -197,  -197,
    -197,  -197,    53,  -197,  -197,   143,   148,   360,   116,  1058,
    1078,  -197,   163,  -197,   574,  -197,   715,  -197,   360,  -197,
    1310,   708,   708,   708,   708,   166,   708,   708,    87,    87,
      87,    87,    87,    87,    56,    56,   108,   108,   108,   108,
    -197,  -197,  -197,   360,  1310,  -197,  -197,   120,   798,   360,
    -197,   962,  -197,   121,  1098,  -197,    65,   117,   118,   119,
     772,  -197,   -20,   126,  -197,  -197,   587,  -197,  -197,   502,
    1310,    31,  -197,  -197,  1310,  1274,  -197,   360,   772,   894,
     360,   360,   360,    80,  -197,  -197,   154,   153,    59,   180,
     133,  -197,  -197,    52,  -197,   166,  -197,    59,  -197,    34,
     155,  1118,  -197,    68,  1138,  1008,  1033,   135,   132,  -197,
     658,   772,   109,  -197,  -197,    59,   360,   882,  -197,   962,
    -197,  -197,   182,  -197,   808,   136,   360,  -197,   360,   360,
     360,  -197,  -197,   936,  -197,  -197,  -197,   164,  -197,  1292,
    -197,   645,   167,    59,  -197,  -197,  1310,  -197,  1158,   772,
    1178,  1198,  1218,   109,  -197,  -197,   360,  -197,  -197,  -197,
     772,   772,   139,  -197,  1310,   772,  -197,  -197,  -197,  -197
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -197,  -197,   173,   -48,   -22,  -197,     0,  -197,     2,     8,
      36,  -161,    79,   249,   -45,  -184,  -197,   -87,  -197,  -196,
    -197,    85,  -197,  -197,     4,   -68,     1,  -189,  -197,  -197,
    -197,   -15,  -197,  -197,  -197,  -155,  -143,  -197,    22,  -197,
    -197,    75,  -197
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -143
static const yytype_int16 yytable[] =
{
      32,    42,    33,   121,    40,   113,   136,   193,    34,   222,
       1,     2,     3,     4,     5,   223,     6,     7,     8,     9,
      45,    47,   134,   243,    69,    70,   207,    49,    46,    32,
      42,    33,   199,    40,    53,    54,   208,    34,   172,    55,
     209,    32,   255,    33,   216,   106,   264,   216,    56,    34,
      74,    23,    84,   126,    85,   262,    24,    25,    26,     6,
       7,   137,    27,     1,     2,     3,     4,     5,    57,     6,
       7,     8,     9,   239,    32,    48,    33,   121,    40,   257,
     279,    50,    34,   265,   196,   168,   200,   217,    63,   246,
     241,   286,   287,   186,   242,    67,   289,    77,   227,   228,
     168,   100,   101,   102,    23,   236,   169,   237,   103,    24,
      25,    26,     1,     2,     3,    27,     5,    28,   138,    65,
      84,   169,    85,   121,    66,     6,     7,    86,   176,   175,
      98,    99,   100,   101,   102,    73,    32,   133,    33,   103,
     106,   127,    87,    88,    34,    89,     1,     2,     3,   170,
       5,   121,   116,   117,   118,   119,   120,   105,   128,   132,
     103,  -142,   253,   173,     1,     2,     3,     4,     5,   181,
       6,     7,     8,     9,   187,   191,   197,   203,   201,   202,
      32,   210,    33,   231,   256,   232,   234,   244,    34,   235,
     250,   121,   261,   238,   251,   216,   273,   267,    32,   276,
      33,   288,    76,   171,   192,    23,    34,   177,   183,   233,
      24,    25,    26,   178,     0,   254,    27,   162,   240,     0,
       0,   163,     0,     0,     0,   283,     0,     0,     0,     0,
      32,    32,    33,    33,    40,     0,   258,     0,    34,    34,
       0,     0,     0,     0,     0,    77,    78,    79,    80,    81,
       0,     0,     0,    32,     0,    33,     0,   106,     0,     0,
       0,    34,    82,    83,   277,    61,     0,     0,    84,    32,
      85,    33,    68,     0,     0,    71,    72,    34,     0,     0,
      32,    32,    33,    33,     0,    32,     0,    33,    34,    34,
       0,     0,     0,    34,   107,   108,     0,   110,     0,   110,
       0,     0,     0,     0,   129,   130,     0,     0,     0,   114,
       0,     1,     2,     3,   115,     5,   110,   116,   117,   118,
     119,   120,     0,     0,     0,     0,   140,   141,   142,   143,
     144,     0,     0,     0,     0,     0,     0,     0,     0,   146,
     147,   148,   149,   150,   151,   152,   153,   154,   155,   156,
     157,   158,   159,     0,     0,     0,     0,     0,     0,     0,
     164,     0,   164,     1,     2,     3,     4,     5,     0,     6,
       7,     8,     9,     0,     0,     0,   174,     0,     0,     0,
       0,    90,    91,   164,     0,     0,     0,   110,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,     0,
       0,     0,     0,   103,    23,     0,     0,   135,     0,    24,
      25,    26,   190,     0,     0,    27,    75,   194,   195,     1,
       2,     3,     4,     5,     0,     6,     7,     8,     9,    10,
      11,    12,    13,    14,     0,   164,    15,     0,    16,    17,
      18,    19,    20,    21,    22,     0,   221,     0,     0,   224,
     225,   226,     0,    78,    79,    80,    81,     0,     0,     0,
      23,     0,     0,     0,     0,    24,    25,    26,     0,    82,
      83,    27,     0,     0,    28,    84,     0,    85,     0,     0,
       0,     0,     0,     0,     0,   259,   110,     0,     0,     0,
       0,     0,     0,   266,     0,   268,     0,   270,   271,   272,
       0,     0,     0,   214,     0,   -22,   -22,   -22,   -22,   -22,
     164,   -22,   -22,   -22,   -22,   -22,     0,     0,     0,     0,
       0,     0,     0,   -22,     0,   284,     0,     0,     0,     0,
       0,     0,   -22,   -22,   -22,   -22,   -22,   -22,   -22,   -22,
     -22,   -22,   -22,   -22,   -22,   -22,   -22,   -22,   -22,   -22,
       0,   -22,   -22,   -22,   -22,     0,   -22,   -22,   -22,   215,
       0,   -22,   -22,   -22,     1,     2,     3,     4,     5,     0,
       6,     7,     8,     9,     0,     0,     0,     1,     2,     3,
       4,     5,     0,     6,     7,     8,     9,     0,     0,     0,
       1,     2,     3,     4,     5,     0,     6,     7,     8,     9,
       0,     0,     0,     0,     0,    23,     0,     0,     0,     0,
      24,    25,    26,     0,     0,     0,    27,   165,    23,     0,
       0,   163,     0,    24,    25,    26,     0,     0,     0,    27,
     184,    23,     0,     0,   163,     0,    24,    25,    26,     0,
       0,     0,    27,     0,     0,     0,   212,   163,     1,     2,
       3,     4,     5,     0,     6,     7,     8,     9,     0,     0,
       0,     1,     2,     3,     4,     5,     0,     6,     7,     8,
       9,    10,    11,    12,    13,    14,   227,   228,    15,     0,
      16,    17,    18,    19,    20,    21,    22,     0,     0,    23,
       0,     0,     0,     0,    24,    25,    26,     0,     0,     0,
      27,   275,    23,     0,     0,   163,     0,    24,    25,    26,
       0,     0,     0,    27,     0,     0,     0,   252,     1,     2,
       3,     4,     5,     0,     6,     7,     8,     9,    10,    11,
      12,    13,    14,     0,     0,    15,     0,    16,    17,    18,
      19,    20,    21,    22,     0,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,     0,     0,     0,    23,
     103,     0,     0,     0,    24,    25,    26,     0,     0,     0,
      27,     0,     0,     0,   185,     1,     2,     3,     4,     5,
       0,     6,     7,     8,     9,    10,    11,    12,    13,    14,
       0,     0,    15,     0,    16,    17,    18,    19,    20,    21,
      22,     1,     2,     3,     4,     5,     0,     6,     7,     8,
       9,     1,     2,     3,     4,     5,    23,     6,     7,     8,
       9,    24,    25,    26,     0,     0,    22,    27,     0,     0,
      28,     1,     2,     3,     4,     5,     0,     6,     7,     8,
       9,     0,    23,     0,     0,     0,     0,    24,    25,    26,
       0,     0,    23,    27,     0,     0,   138,    24,    25,    26,
       0,     0,     0,    27,     0,     0,   138,     1,     2,     3,
       4,     5,    23,     6,     7,     8,     9,    24,    25,    26,
       0,     0,     0,    27,   109,     1,     2,     3,     4,     5,
       0,     6,     7,     8,     9,   114,     0,     1,     2,     3,
     170,     5,     0,   116,   117,   118,   119,   120,    23,     0,
       0,     0,     0,    24,    25,    26,     0,     0,     0,    27,
     112,     0,     0,     0,     0,     0,    23,     0,     0,     0,
       0,    24,    25,    26,     0,     0,     0,    27,   260,     1,
       2,     3,     4,     5,     0,     6,     7,     8,     9,    10,
      11,    12,    13,    14,     0,     0,    15,     0,    16,    17,
      18,    19,    20,    21,    22,     1,     2,     3,   170,     5,
       0,   116,   117,   118,   119,   120,     0,     0,     0,     0,
      23,     0,     0,     0,     0,    24,    25,    26,    90,    91,
       0,    27,     0,     0,     0,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,     0,     0,     0,     0,
     103,     0,     0,    90,    91,     0,     0,     0,     0,   104,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,     0,     0,     0,     0,   103,     0,     0,    90,    91,
       0,     0,     0,     0,   131,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,     0,     0,     0,     0,
     103,     0,     0,    90,    91,     0,     0,     0,     0,   248,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,     0,     0,     0,     0,   103,     0,     0,    90,    91,
       0,     0,     0,     0,   249,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,     0,     0,    90,    91,
     103,     0,     0,     0,   179,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,     0,     0,    90,    91,
     103,     0,     0,     0,   180,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,     0,     0,    90,    91,
     103,     0,     0,     0,   198,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,     0,     0,    90,    91,
     103,     0,     0,     0,   245,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,     0,     0,    90,    91,
     103,     0,     0,     0,   247,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,     0,     0,    90,    91,
     103,     0,     0,     0,   278,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,     0,     0,    90,    91,
     103,     0,     0,     0,   280,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,     0,     0,    90,    91,
     103,     0,     0,     0,   281,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,     0,     0,    90,    91,
     103,     0,     0,     0,   282,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,    90,    91,     0,     0,
     103,     0,   160,    92,    93,    94,    95,    96,    97,    98,
      99,   100,   101,   102,    90,    91,     0,     0,   103,     0,
     161,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,    90,    91,     0,     0,   103,     0,   220,    92,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
      90,    91,     0,     0,   103,     0,   274,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,     0,     0,
       0,     0,   103
};

static const yytype_int16 yycheck[] =
{
       0,     0,     0,    51,     0,    50,    74,   168,     0,   198,
       3,     4,     5,     6,     7,   199,     9,    10,    11,    12,
      53,     1,    67,   219,    24,    25,   181,     1,    53,    29,
      29,    29,   175,    29,    12,    13,    56,    29,   125,    55,
      60,    41,   231,    41,    13,    41,   242,    13,    55,    41,
      28,    44,    55,    52,    57,   239,    49,    50,    51,     9,
      10,     1,    55,     3,     4,     5,     6,     7,    61,     9,
      10,    11,    12,   216,    74,    55,    74,   125,    74,   234,
     269,    55,    74,   244,   171,    32,    21,    56,    11,    21,
      56,   280,   281,   138,    60,    55,   285,    32,    18,    19,
      32,    45,    46,    47,    44,    53,    53,    55,    52,    49,
      50,    51,     3,     4,     5,    55,     7,    58,    58,    61,
      55,    53,    57,   171,    61,     9,    10,    61,   128,    13,
      43,    44,    45,    46,    47,    59,   136,     6,   136,    52,
     136,    55,    61,    61,   136,    61,     3,     4,     5,     6,
       7,   199,     9,    10,    11,    12,    13,    61,    55,    55,
      52,    55,   230,    15,     3,     4,     5,     6,     7,     6,
       9,    10,    11,    12,     8,    55,    55,    58,    61,    61,
     180,    55,   180,    29,   232,    32,     6,    32,   180,    56,
      55,   239,   237,   215,    62,    13,    32,    61,   198,    32,
     198,    62,    29,    60,   168,    44,   198,   128,   133,   208,
      49,    50,    51,   128,    -1,   230,    55,    56,   217,    -1,
      -1,    60,    -1,    -1,    -1,   273,    -1,    -1,    -1,    -1,
     230,   231,   230,   231,   230,    -1,   235,    -1,   230,   231,
      -1,    -1,    -1,    -1,    -1,    32,    33,    34,    35,    36,
      -1,    -1,    -1,   253,    -1,   253,    -1,   253,    -1,    -1,
      -1,   253,    49,    50,   263,    16,    -1,    -1,    55,   269,
      57,   269,    23,    -1,    -1,    26,    27,   269,    -1,    -1,
     280,   281,   280,   281,    -1,   285,    -1,   285,   280,   281,
      -1,    -1,    -1,   285,    45,    46,    -1,    48,    -1,    50,
      -1,    -1,    -1,    -1,    55,    56,    -1,    -1,    -1,     1,
      -1,     3,     4,     5,     6,     7,    67,     9,    10,    11,
      12,    13,    -1,    -1,    -1,    -1,    77,    78,    79,    80,
      81,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     111,    -1,   113,     3,     4,     5,     6,     7,    -1,     9,
      10,    11,    12,    -1,    -1,    -1,   127,    -1,    -1,    -1,
      -1,    30,    31,   134,    -1,    -1,    -1,   138,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    -1,
      -1,    -1,    -1,    52,    44,    -1,    -1,    56,    -1,    49,
      50,    51,   163,    -1,    -1,    55,     0,   168,   169,     3,
       4,     5,     6,     7,    -1,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,   186,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    -1,   197,    -1,    -1,   200,
     201,   202,    -1,    33,    34,    35,    36,    -1,    -1,    -1,
      44,    -1,    -1,    -1,    -1,    49,    50,    51,    -1,    49,
      50,    55,    -1,    -1,    58,    55,    -1,    57,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   236,   237,    -1,    -1,    -1,
      -1,    -1,    -1,   244,    -1,   246,    -1,   248,   249,   250,
      -1,    -1,    -1,     1,    -1,     3,     4,     5,     6,     7,
     261,     9,    10,    11,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    21,    -1,   276,    -1,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      -1,    49,    50,    51,    52,    -1,    54,    55,    56,    57,
      -1,    59,    60,    61,     3,     4,     5,     6,     7,    -1,
       9,    10,    11,    12,    -1,    -1,    -1,     3,     4,     5,
       6,     7,    -1,     9,    10,    11,    12,    -1,    -1,    -1,
       3,     4,     5,     6,     7,    -1,     9,    10,    11,    12,
      -1,    -1,    -1,    -1,    -1,    44,    -1,    -1,    -1,    -1,
      49,    50,    51,    -1,    -1,    -1,    55,    56,    44,    -1,
      -1,    60,    -1,    49,    50,    51,    -1,    -1,    -1,    55,
      56,    44,    -1,    -1,    60,    -1,    49,    50,    51,    -1,
      -1,    -1,    55,    -1,    -1,    -1,    59,    60,     3,     4,
       5,     6,     7,    -1,     9,    10,    11,    12,    -1,    -1,
      -1,     3,     4,     5,     6,     7,    -1,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    -1,    -1,    44,
      -1,    -1,    -1,    -1,    49,    50,    51,    -1,    -1,    -1,
      55,    56,    44,    -1,    -1,    60,    -1,    49,    50,    51,
      -1,    -1,    -1,    55,    -1,    -1,    -1,    59,     3,     4,
       5,     6,     7,    -1,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    -1,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    -1,    -1,    44,
      52,    -1,    -1,    -1,    49,    50,    51,    -1,    -1,    -1,
      55,    -1,    -1,    -1,    59,     3,     4,     5,     6,     7,
      -1,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,     3,     4,     5,     6,     7,    -1,     9,    10,    11,
      12,     3,     4,     5,     6,     7,    44,     9,    10,    11,
      12,    49,    50,    51,    -1,    -1,    28,    55,    -1,    -1,
      58,     3,     4,     5,     6,     7,    -1,     9,    10,    11,
      12,    -1,    44,    -1,    -1,    -1,    -1,    49,    50,    51,
      -1,    -1,    44,    55,    -1,    -1,    58,    49,    50,    51,
      -1,    -1,    -1,    55,    -1,    -1,    58,     3,     4,     5,
       6,     7,    44,     9,    10,    11,    12,    49,    50,    51,
      -1,    -1,    -1,    55,    56,     3,     4,     5,     6,     7,
      -1,     9,    10,    11,    12,     1,    -1,     3,     4,     5,
       6,     7,    -1,     9,    10,    11,    12,    13,    44,    -1,
      -1,    -1,    -1,    49,    50,    51,    -1,    -1,    -1,    55,
      56,    -1,    -1,    -1,    -1,    -1,    44,    -1,    -1,    -1,
      -1,    49,    50,    51,    -1,    -1,    -1,    55,    56,     3,
       4,     5,     6,     7,    -1,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,     3,     4,     5,     6,     7,
      -1,     9,    10,    11,    12,    13,    -1,    -1,    -1,    -1,
      44,    -1,    -1,    -1,    -1,    49,    50,    51,    30,    31,
      -1,    55,    -1,    -1,    -1,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    -1,    -1,    -1,
      52,    -1,    -1,    30,    31,    -1,    -1,    -1,    -1,    61,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    -1,    -1,    -1,    -1,    52,    -1,    -1,    30,    31,
      -1,    -1,    -1,    -1,    61,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    -1,    -1,    -1,
      52,    -1,    -1,    30,    31,    -1,    -1,    -1,    -1,    61,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    -1,    -1,    -1,    -1,    52,    -1,    -1,    30,    31,
      -1,    -1,    -1,    -1,    61,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    -1,    30,    31,
      52,    -1,    -1,    -1,    56,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    -1,    30,    31,
      52,    -1,    -1,    -1,    56,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    -1,    30,    31,
      52,    -1,    -1,    -1,    56,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    -1,    30,    31,
      52,    -1,    -1,    -1,    56,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    -1,    30,    31,
      52,    -1,    -1,    -1,    56,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    -1,    30,    31,
      52,    -1,    -1,    -1,    56,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    -1,    30,    31,
      52,    -1,    -1,    -1,    56,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    -1,    30,    31,
      52,    -1,    -1,    -1,    56,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    -1,    30,    31,
      52,    -1,    -1,    -1,    56,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    30,    31,    -1,    -1,
      52,    -1,    54,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    30,    31,    -1,    -1,    52,    -1,
      54,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    30,    31,    -1,    -1,    52,    -1,    54,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      30,    31,    -1,    -1,    52,    -1,    54,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    -1,    -1,
      -1,    -1,    52
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     5,     6,     7,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    20,    22,    23,    24,    25,
      26,    27,    28,    44,    49,    50,    51,    55,    58,    64,
      65,    66,    69,    71,    72,    73,    75,    76,    84,    85,
      87,    88,    89,    91,    97,    53,    53,     1,    55,     1,
      55,    99,   101,   101,   101,    55,    55,    61,    69,    71,
      72,    76,   105,    11,   100,    61,    61,    55,    76,    69,
      69,    76,    76,    59,   101,     0,    65,    32,    33,    34,
      35,    36,    49,    50,    55,    57,    61,    61,    61,    61,
      30,    31,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    52,    61,    61,    87,    76,    76,    56,
      76,    77,    56,    77,     1,     6,     9,    10,    11,    12,
      13,    66,    78,    79,    80,    81,    89,    55,    55,    76,
      76,    61,    55,     6,    77,    56,    88,     1,    58,    74,
      76,    76,    76,    76,    76,    70,    76,    76,    76,    76,
      76,    76,    76,    76,    76,    76,    76,    76,    76,    76,
      54,    54,    56,    60,    76,    56,    98,   104,    32,    53,
       6,    60,    80,    15,    76,    13,    69,    75,    84,    56,
      56,     6,    96,   104,    56,    59,    77,     8,    67,    68,
      76,    55,    73,    74,    76,    76,    80,    55,    56,    99,
      21,    61,    61,    58,    87,    89,    90,    98,    56,    60,
      55,   102,    59,   103,     1,    57,    13,    56,    82,    83,
      54,    76,    90,    78,    76,    76,    76,    18,    19,    94,
      95,    29,    32,    89,     6,    56,    53,    55,    67,    99,
      89,    56,    60,    82,    32,    56,    21,    56,    61,    61,
      55,    62,    59,    88,    94,    90,    66,    98,    89,    76,
      56,    77,    78,    86,    82,    74,    76,    61,    76,    92,
      76,    76,    76,    32,    54,    56,    32,    89,    56,    90,
      56,    56,    56,    66,    76,    93,    90,    90,    62,    90
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 77 "grammar.yy"
    { }
    break;

  case 3:
#line 78 "grammar.yy"
    { }
    break;

  case 4:
#line 83 "grammar.yy"
    {
		if (((yyvsp[(1) - (1)].node) != NULL) && (!cmdparser.addStatement((yyvsp[(1) - (1)].node)))) YYABORT;
		}
    break;

  case 5:
#line 86 "grammar.yy"
    {
		if (((yyvsp[(1) - (1)].node) != NULL) && (!cmdparser.addStatement((yyvsp[(1) - (1)].node)))) YYABORT;
		}
    break;

  case 6:
#line 96 "grammar.yy"
    { (yyval.node) = cmdparser.addConstant((yyvsp[(1) - (1)].intconst)); }
    break;

  case 7:
#line 97 "grammar.yy"
    { (yyval.node) = cmdparser.addConstant((yyvsp[(1) - (1)].doubleconst)); }
    break;

  case 8:
#line 98 "grammar.yy"
    { (yyval.node) = cmdparser.addConstant((yyvsp[(1) - (1)].name)->get()); }
    break;

  case 9:
#line 99 "grammar.yy"
    { (yyval.node) = cmdparser.addElementConstant((yyvsp[(1) - (1)].intconst)); }
    break;

  case 10:
#line 108 "grammar.yy"
    {
		if (!cmdparser.expandPath(stepNameStack.last(), (yyvsp[(4) - (5)].node))) YYABORT;
		stepNameStack.removeLast();
		}
    break;

  case 11:
#line 112 "grammar.yy"
    {
		if (!cmdparser.expandPath(stepNameStack.last(), NULL, (yyvsp[(4) - (5)].node))) YYABORT;
		stepNameStack.removeLast();
		}
    break;

  case 12:
#line 116 "grammar.yy"
    {
		if (!cmdparser.expandPath(stepNameStack.last(), NULL, NULL)) YYABORT;
		stepNameStack.removeLast();
		}
    break;

  case 13:
#line 120 "grammar.yy"
    {
		if (!cmdparser.expandPath((yyvsp[(1) - (2)].name))) YYABORT;
		stepNameStack.removeLast();
		}
    break;

  case 14:
#line 128 "grammar.yy"
    { }
    break;

  case 15:
#line 129 "grammar.yy"
    { }
    break;

  case 16:
#line 130 "grammar.yy"
    { msg.print("Error formulating path.\n"); YYABORT; }
    break;

  case 17:
#line 135 "grammar.yy"
    {
		(yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (4)].variable),(yyvsp[(3) - (4)].node));
		if ((yyval.node) == NULL) { msg.print("Error in variable expression (code 1)\n"); YYABORT; }
		}
    break;

  case 18:
#line 139 "grammar.yy"
    {
		(yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (1)].variable));
		if ((yyval.node) == NULL) { msg.print("Error in variable expression (code 2)\n"); YYABORT; }
		}
    break;

  case 19:
#line 143 "grammar.yy"
    {
		(yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (4)].variable),(yyvsp[(3) - (4)].node));
		if ((yyval.node) == NULL) { msg.print("Error in variable expression (code 3)\n"); YYABORT; }
		}
    break;

  case 20:
#line 147 "grammar.yy"
    {
		(yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (1)].variable));
		if ((yyval.node) == NULL) { msg.print("Error in variable expression (code 4)\n"); YYABORT; }
		}
    break;

  case 21:
#line 151 "grammar.yy"
    {
		cmdparser.createPath((yyvsp[(1) - (2)].node));
		}
    break;

  case 22:
#line 153 "grammar.yy"
    {
		(yyval.node) = cmdparser.finalisePath();
		}
    break;

  case 23:
#line 156 "grammar.yy"
    {
		msg.print("Can't use a variable as a function. Did you mean '[' instead?\n"); (yyval.node) = NULL;
		}
    break;

  case 24:
#line 167 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction( (Command::Function) (yyvsp[(1) - (3)].functionId));
		msg.print(Messenger::Parse,"PARSER : function : function '%s'\n", commands.data[(Command::Function) (yyvsp[(1) - (3)].functionId)].keyword);
		}
    break;

  case 25:
#line 171 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunctionWithArglist( (Command::Function) (yyvsp[(1) - (4)].functionId),(yyvsp[(3) - (4)].node));
		msg.print(Messenger::Parse,"PARSER : function : function '%s' with exprlist\n", commands.data[(Command::Function) (yyvsp[(1) - (4)].functionId)].keyword);
		}
    break;

  case 26:
#line 175 "grammar.yy"
    {
		msg.print("Error: Missing brackets after function call?\n");
		YYABORT;
		}
    break;

  case 27:
#line 183 "grammar.yy"
    {
		(yyval.node) = cmdparser.addUserFunction((yyvsp[(1) - (3)].tree));
		msg.print(Messenger::Parse,"PARSER : userfunction : function '%s'\n", (yyvsp[(1) - (3)].tree)->name());
		}
    break;

  case 28:
#line 187 "grammar.yy"
    {
		(yyval.node) = cmdparser.addUserFunction((yyvsp[(1) - (4)].tree),(yyvsp[(3) - (4)].node));
		msg.print(Messenger::Parse,"PARSER : userfunction : function '%s' with expressionlist\n", (yyvsp[(1) - (4)].tree)->name());
		}
    break;

  case 29:
#line 191 "grammar.yy"
    {
		msg.print("Error: Missing brackets after function call?\n");
		YYABORT;
		}
    break;

  case 30:
#line 203 "grammar.yy"
    {
		(yyval.node) = cmdparser.addWidget((yyvsp[(3) - (4)].node));
		}
    break;

  case 31:
#line 210 "grammar.yy"
    {
		(yyval.node) = cmdparser.addArrayConstant((yyvsp[(2) - (3)].node));
		}
    break;

  case 32:
#line 220 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignment,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); }
    break;

  case 33:
#line 221 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignment,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); }
    break;

  case 34:
#line 222 "grammar.yy"
    { msg.print("Mangled expression used in assignment.\n"); YYABORT; }
    break;

  case 35:
#line 227 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 36:
#line 228 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 37:
#line 229 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 38:
#line 230 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignmentPlus,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); }
    break;

  case 39:
#line 231 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignmentSubtract,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); }
    break;

  case 40:
#line 232 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignmentMultiply,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); }
    break;

  case 41:
#line 233 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignmentDivide,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); }
    break;

  case 42:
#line 234 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorNegate, (yyvsp[(2) - (2)].node)); }
    break;

  case 43:
#line 235 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPostfixIncrease, (yyvsp[(1) - (2)].node));  }
    break;

  case 44:
#line 236 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPostfixDecrease, (yyvsp[(1) - (2)].node)); }
    break;

  case 45:
#line 237 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPrefixIncrease, (yyvsp[(2) - (2)].node)); }
    break;

  case 46:
#line 238 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPrefixDecrease, (yyvsp[(2) - (2)].node)); }
    break;

  case 47:
#line 239 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 48:
#line 240 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAdd, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 49:
#line 241 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorSubtract, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 50:
#line 242 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorMultiply, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 51:
#line 243 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorDivide, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 52:
#line 244 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPower, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 53:
#line 245 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorModulus, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 54:
#line 246 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 55:
#line 247 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorNotEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 56:
#line 248 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorGreaterThan, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 57:
#line 249 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorGreaterThanEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 58:
#line 250 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorLessThan, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 59:
#line 251 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorLessThanEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 60:
#line 252 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAnd, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 61:
#line 253 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorOr, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 62:
#line 254 "grammar.yy"
    { (yyval.node) = (yyvsp[(2) - (3)].node); }
    break;

  case 63:
#line 255 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorNot, (yyvsp[(2) - (2)].node)); }
    break;

  case 64:
#line 256 "grammar.yy"
    { msg.print("Error: '%s' has not been declared as a function or a variable.\n", yylval.name->get()); YYABORT; }
    break;

  case 65:
#line 261 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		if ((yyval.node) == NULL) YYABORT;
		}
    break;

  case 66:
#line 265 "grammar.yy"
    {
		(yyval.node) = Tree::joinArguments((yyvsp[(3) - (3)].node),(yyvsp[(1) - (3)].node));
		}
    break;

  case 67:
#line 268 "grammar.yy"
    {
		msg.print("Error: Missing comma between items.\n");
		YYABORT;
		}
    break;

  case 68:
#line 280 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : variablename : existing var '%s'\n", tokenName.get());
		tokenName = yylval.variable->name();
		(yyval.name) = &tokenName;
		}
    break;

  case 69:
#line 285 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : variablename : existing built-in function '%s'\n", tokenName.get());
		tokenName = Command::data[yylval.functionId].keyword;
		(yyval.name) = &tokenName;
		}
    break;

  case 70:
#line 290 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : variablename : existing local var '%s'\n", tokenName.get());
		msg.print("Error: Existing variable in local scope cannot be redeclared.\n");
		YYABORT;
		}
    break;

  case 71:
#line 295 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : variablename : constant '%s'\n", tokenName.get());
		msg.print("Error: Constant value found in declaration.\n");
		YYABORT;
		}
    break;

  case 72:
#line 300 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : variablename : existing user function '%s'\n", tokenName.get());
		msg.print("Error: Existing user-defined function name cannot be redeclared.\n");
		YYABORT;
		}
    break;

  case 73:
#line 305 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : variablename : variable type-name '%s'\n", VTypes::dataType( yylval.vtype));
		msg.print("Error: Type-name used in variable declaration.\n");
		YYABORT;
		}
    break;

  case 74:
#line 310 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : variablename : new token '%s'\n", tokenName.get());
		if (declaredType == VTypes::NoData) { msg.print("Token '%s' is undeclared.\n", tokenName.get()); YYABORT; }
		(yyval.name) = (yyvsp[(1) - (2)].name);
		}
    break;

  case 75:
#line 319 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : assignedvariablename : var '%s' with array assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addVariable(declaredType, &tokenName, (yyvsp[(3) - (3)].node));
		}
    break;

  case 76:
#line 323 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : assignedvariablename : var '%s' with widget assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addVariable(declaredType, &tokenName, (yyvsp[(3) - (3)].node));
		}
    break;

  case 77:
#line 327 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : assignedvariablename : array var '%s' with expr assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addArrayVariable(declaredType, &tokenName,(yyvsp[(3) - (6)].node),(yyvsp[(6) - (6)].node));
		}
    break;

  case 78:
#line 331 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : assignedvariablename : array var '%s' with array assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addArrayVariable(declaredType, &tokenName,(yyvsp[(3) - (6)].node),(yyvsp[(6) - (6)].node));
		}
    break;

  case 79:
#line 335 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : assignedvariablename : var '%s' with expr assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addVariable(declaredType, &tokenName, (yyvsp[(3) - (3)].node));
		}
    break;

  case 80:
#line 343 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : assignedvariablename : var '%s'\n", tokenName.get());
		(yyval.node) = cmdparser.addVariable(declaredType, &tokenName);
		}
    break;

  case 81:
#line 347 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : assignedvariablename : array var '%s'\n", tokenName.get());
		(yyval.node) = cmdparser.addArrayVariable(declaredType, &tokenName, (yyvsp[(3) - (4)].node));
		}
    break;

  case 82:
#line 351 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 83:
#line 358 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 84:
#line 361 "grammar.yy"
    {
		(yyval.node) = Tree::joinArguments((yyvsp[(3) - (3)].node),(yyvsp[(1) - (3)].node));
		}
    break;

  case 85:
#line 364 "grammar.yy"
    {
		msg.print("Error: Missing comma between declarations?\n");
		YYABORT;
		}
    break;

  case 86:
#line 372 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : typedvariablelistitem : var '%s'\n", tokenName.get());
		(yyval.node) = cmdparser.addVariable(declaredType, &tokenName);
		}
    break;

  case 87:
#line 376 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : typedvariablelistitem : var '%s' with expr assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addVariable(declaredType, &tokenName, (yyvsp[(5) - (5)].node));
		}
    break;

  case 88:
#line 384 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 89:
#line 387 "grammar.yy"
    {
		(yyval.node) = Tree::joinArguments((yyvsp[(3) - (3)].node),(yyvsp[(1) - (3)].node));
		}
    break;

  case 90:
#line 390 "grammar.yy"
    {
		msg.print("Error: Missing comma between declarations?\n");
		YYABORT;
		}
    break;

  case 91:
#line 398 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : declaration : standard variable declaration list\n");
		(yyval.node) = cmdparser.addDeclarations((yyvsp[(3) - (3)].node));
		declaredType = VTypes::NoData;
		}
    break;

  case 92:
#line 403 "grammar.yy"
    {
		msg.print("Illegal use of reserved word '%s'.\n", VTypes::dataType(declaredType));
		YYABORT;
		}
    break;

  case 93:
#line 415 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : functiondeclaration : user-defined subroutine (VOID return value, no arguments)\n");
		if (!cmdparser.addStatement((yyvsp[(7) - (7)].node))) YYABORT;
		cmdparser.popTree();
		declaredType = VTypes::NoData;
		}
    break;

  case 94:
#line 421 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : functiondeclaration : user-defined function (%s return value, no arguments)\n", VTypes::dataType((yyvsp[(4) - (7)].tree)->returnType()));
		if (!cmdparser.addStatement((yyvsp[(7) - (7)].node))) YYABORT;
		cmdparser.popTree();
		declaredType = VTypes::NoData;
		}
    break;

  case 95:
#line 427 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : functiondeclaration : user-defined function (%s return value, arguments)\n", VTypes::dataType((yyvsp[(4) - (7)].tree)->returnType()));
		if (!(yyvsp[(4) - (7)].tree)->addLocalFunctionArguments((yyvsp[(6) - (7)].node))) YYABORT;
		}
    break;

  case 96:
#line 430 "grammar.yy"
    {
		if (!cmdparser.addStatement((yyvsp[(9) - (9)].node))) YYABORT;
		cmdparser.popTree();
		declaredType = VTypes::NoData;
		}
    break;

  case 97:
#line 443 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (2)].node);
		}
    break;

  case 98:
#line 446 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (2)].node);
		}
    break;

  case 99:
#line 449 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (2)].node);
		}
    break;

  case 100:
#line 452 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (2)].node);
		}
    break;

  case 101:
#line 455 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (2)].node);
		}
    break;

  case 102:
#line 458 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 103:
#line 461 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (2)].node);
		}
    break;

  case 104:
#line 464 "grammar.yy"
    {
		(yyval.node) = NULL;
		}
    break;

  case 105:
#line 467 "grammar.yy"
    {
		(yyval.node) = NULL;
		}
    break;

  case 106:
#line 470 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::Help, cmdparser.addConstant((yyvsp[(2) - (2)].functionId)));
		}
    break;

  case 107:
#line 473 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::Return,(yyvsp[(2) - (3)].node));
		}
    break;

  case 108:
#line 476 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::Return);
		}
    break;

  case 109:
#line 479 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::Continue);
		}
    break;

  case 110:
#line 482 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::Break);
		}
    break;

  case 111:
#line 489 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 112:
#line 492 "grammar.yy"
    {
		if ((yyvsp[(2) - (2)].node) == NULL) (yyval.node) = (yyvsp[(1) - (2)].node);
		else (yyval.node) = cmdparser.joinCommands((yyvsp[(1) - (2)].node), (yyvsp[(2) - (2)].node));
		}
    break;

  case 113:
#line 500 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(3) - (5)].node);
		}
    break;

  case 114:
#line 503 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::NoFunction);
		}
    break;

  case 115:
#line 510 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 116:
#line 513 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 117:
#line 520 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::If,(yyvsp[(3) - (7)].node),(yyvsp[(5) - (7)].node),(yyvsp[(7) - (7)].node));
		}
    break;

  case 118:
#line 523 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::If,(yyvsp[(3) - (5)].node),(yyvsp[(5) - (5)].node));
		}
    break;

  case 119:
#line 526 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (10)].node), cmdparser.addFunction(Command::For, (yyvsp[(4) - (10)].node),(yyvsp[(6) - (10)].node),(yyvsp[(8) - (10)].node),(yyvsp[(10) - (10)].node))); cmdparser.popScope();
		}
    break;

  case 120:
#line 529 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (10)].node), cmdparser.addFunction(Command::For, (yyvsp[(4) - (10)].node),(yyvsp[(6) - (10)].node),(yyvsp[(8) - (10)].node),(yyvsp[(10) - (10)].node))); cmdparser.popScope();
		}
    break;

  case 121:
#line 532 "grammar.yy"
    {
		if ((yyvsp[(4) - (7)].node)->returnType() <= VTypes::VectorData) { msg.print("Error: For/In loop variable must be of pointer type.\n"); YYABORT; }
		if ((yyvsp[(4) - (7)].node)->returnType() != (yyvsp[(6) - (7)].node)->returnType()) { msg.print("Error: For/In loop variable is not being assigned the correct type.\n"); YYABORT; }
		}
    break;

  case 122:
#line 535 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (9)].node), cmdparser.addFunction(Command::ForIn,(yyvsp[(4) - (9)].node),(yyvsp[(6) - (9)].node),(yyvsp[(9) - (9)].node)));
		cmdparser.popScope();
		}
    break;

  case 123:
#line 539 "grammar.yy"
    { 
		if (declaredType <= VTypes::VectorData)
		{
			msg.print("Error: For/In loop variable must be of pointer type.\n");
			YYABORT;
		}
		tempNode = cmdparser.addVariable(declaredType, &tokenName);
		if (declaredType != (yyvsp[(8) - (9)].node)->returnType())
		{
			msg.print("Error: For/In loop variable is not being assigned the correct type.\n");
			YYABORT;
		}
		}
    break;

  case 124:
#line 551 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (11)].node), cmdparser.addFunction(Command::ForIn,tempNode,(yyvsp[(8) - (11)].node),(yyvsp[(11) - (11)].node)));
		cmdparser.popScope();
		}
    break;

  case 125:
#line 555 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (6)].node), cmdparser.addFunction(Command::While, (yyvsp[(4) - (6)].node),(yyvsp[(6) - (6)].node)));
		cmdparser.popScope();
		}
    break;

  case 126:
#line 559 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (8)].node), cmdparser.addFunction(Command::DoWhile, (yyvsp[(3) - (8)].node),(yyvsp[(6) - (8)].node)));
		cmdparser.popScope();
		}
    break;

  case 127:
#line 563 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::Switch, (yyvsp[(3) - (7)].node));
		(yyval.node)->addJoinedArguments((yyvsp[(6) - (7)].node));
		}
    break;

  case 128:
#line 571 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::Case, (yyvsp[(3) - (5)].node));
		if ((yyval.node) == NULL) { msg.print("Error: Invalid case expression.\n"); YYABORT; }
		}
    break;

  case 129:
#line 575 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::Default);
		if ((yyval.node) == NULL) { msg.print("Error: Invalid default case expression.\n"); YYABORT; }
		}
    break;

  case 130:
#line 583 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 131:
#line 586 "grammar.yy"
    {
		(yyval.node) = Tree::joinArguments((yyvsp[(2) - (2)].node),(yyvsp[(1) - (2)].node));
		}
    break;

  case 132:
#line 589 "grammar.yy"
    {
		(yyval.node) = Tree::joinArguments((yyvsp[(2) - (2)].node),(yyvsp[(1) - (2)].node));
		}
    break;

  case 133:
#line 600 "grammar.yy"
    {
		if (!cmdparser.setFilterOption(&tokenName, (yyvsp[(4) - (4)].node))) YYABORT;
		msg.print(Messenger::Parse,"PARSER : filteroptions : filter option '%s'\n", tokenName.get());
		}
    break;

  case 134:
#line 604 "grammar.yy"
    {
		if (!cmdparser.setFilterOption(&tokenName, (yyvsp[(6) - (6)].node))) YYABORT;
		msg.print(Messenger::Parse,"PARSER : filteroptions : filter option '%s'\n", tokenName.get());
		}
    break;

  case 135:
#line 612 "grammar.yy"
    {
		if (((yyvsp[(6) - (6)].node) != NULL) && (!cmdparser.addStatement((yyvsp[(6) - (6)].node)))) YYABORT;
		cmdparser.popTree();
		msg.print(Messenger::Parse,"PARSER : completed filter definition\n");
		}
    break;

  case 136:
#line 624 "grammar.yy"
    { tokenName = *yylval.name; }
    break;

  case 137:
#line 628 "grammar.yy"
    { declaredType = yylval.vtype; }
    break;

  case 138:
#line 632 "grammar.yy"
    { declaredType = VTypes::NoData; }
    break;

  case 139:
#line 636 "grammar.yy"
    { (yyval.node) = cmdparser.pushScope(); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 140:
#line 640 "grammar.yy"
    { if (!cmdparser.popScope()) YYABORT; }
    break;

  case 141:
#line 644 "grammar.yy"
    { stepNameStack.add()->set(yylval.name->get()); }
    break;

  case 142:
#line 648 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : pushfunc : function/statement '%s'\n", yylval.name->get());
		(yyval.tree) = cmdparser.pushFunction(yylval.name->get(), declaredType);
		cmdparser.pushScope();
		}
    break;

  case 143:
#line 656 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : pushfilter : new filter definition\n");
		cmdparser.pushFilter();
		}
    break;


/* Line 1267 of yacc.c.  */
#line 2888 "grammar.cc"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 662 "grammar.yy"


void yyerror(char *s)
{
}

