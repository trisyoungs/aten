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
	Tree *functree;			/* user-defined function (tree) pointer */
	VTypes::DataType vtype;		/* variable type for next declarations */
	int intconst;			/* integer constant value */
	double doubleconst;		/* double constant value */
}
/* Line 187 of yacc.c.  */
#line 215 "grammar.cc"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 228 "grammar.cc"

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
#define YYFINAL  66
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1111

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  59
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  42
/* YYNRULES -- Number of rules.  */
#define YYNRULES  134
/* YYNRULES -- Number of states.  */
#define YYNSTATES  250

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   294

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    48,     2,     2,     2,    44,     2,     2,
      54,    55,    42,    40,    53,    41,    58,    43,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    52,
      35,    29,    34,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    56,     2,    57,    49,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    50,     2,    51,     2,     2,     2,     2,
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
      25,    26,    27,    28,    30,    31,    32,    33,    36,    37,
      38,    39,    45,    46,    47
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     4,     6,     9,    11,    13,    19,    22,
      23,    24,    26,    28,    31,    34,    37,    39,    41,    46,
      53,    60,    61,    63,    65,    68,    71,    73,    76,    78,
      80,    88,    94,   105,   106,   116,   117,   129,   136,   145,
     147,   149,   151,   153,   160,   167,   171,   175,   179,   180,
     182,   186,   192,   196,   198,   202,   205,   210,   214,   218,
     225,   232,   236,   238,   240,   242,   244,   246,   248,   250,
     253,   254,   259,   265,   271,   276,   279,   281,   285,   288,
     290,   295,   297,   302,   304,   305,   310,   313,   315,   319,
     322,   324,   326,   328,   330,   334,   338,   342,   346,   350,
     354,   358,   361,   364,   367,   370,   373,   375,   379,   383,
     387,   391,   395,   399,   403,   407,   411,   415,   419,   423,
     427,   431,   435,   438,   440,   444,   448,   453,   455,   460,
     464,   466,   467,   468,   469
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      60,     0,    -1,    -1,    61,    -1,    60,    61,    -1,    65,
      -1,    68,    -1,    50,    63,    65,    51,    64,    -1,    50,
      51,    -1,    -1,    -1,    66,    -1,    76,    -1,    65,    66,
      -1,    65,    76,    -1,    70,    52,    -1,    62,    -1,    72,
      -1,     6,    97,    29,    75,    -1,    67,    53,     6,    97,
      29,    75,    -1,    21,    69,    54,    67,    55,    62,    -1,
      -1,    71,    -1,    85,    -1,    22,    12,    -1,    20,    92,
      -1,    20,    -1,    70,    71,    -1,    77,    -1,    92,    -1,
      18,    54,    92,    55,    66,    26,    66,    -1,    18,    54,
      92,    55,    66,    -1,    17,    63,    54,    71,    52,    92,
      52,    92,    55,    66,    -1,    -1,    17,    63,    54,    88,
      19,    92,    55,    73,    66,    -1,    -1,    17,    63,    54,
      14,    98,    83,    19,    92,    55,    74,    66,    -1,    16,
      63,    54,    92,    55,    66,    -1,    15,    63,    66,    16,
      54,    92,    55,    52,    -1,     3,    -1,     5,    -1,     8,
      -1,     4,    -1,    23,    99,     7,    84,    78,    62,    -1,
      14,    98,     7,    84,    78,    62,    -1,    14,    98,    81,
      -1,    14,    98,     1,    -1,    54,    79,    55,    -1,    -1,
      80,    -1,    79,    53,    80,    -1,    14,    98,    83,    29,
      92,    -1,    14,    98,    83,    -1,    82,    -1,    81,    53,
      82,    -1,    81,    82,    -1,    83,    56,    92,    57,    -1,
      83,    29,    94,    -1,    83,    29,    85,    -1,    83,    56,
      92,    57,    29,    92,    -1,    83,    56,    92,    57,    29,
      94,    -1,    83,    29,    92,    -1,    83,    -1,    10,    -1,
      12,    -1,    11,    -1,    75,    -1,    13,    -1,    14,    -1,
       6,    97,    -1,    -1,    25,    54,    91,    55,    -1,     9,
     100,    56,    92,    57,    -1,     9,   100,    54,    91,    55,
      -1,     9,   100,    54,    55,    -1,     9,   100,    -1,    86,
      -1,    87,    58,    86,    -1,    87,     1,    -1,    89,    -1,
      10,    56,    92,    57,    -1,    10,    -1,    11,    56,    92,
      57,    -1,    11,    -1,    -1,    89,    58,    90,    87,    -1,
      89,    54,    -1,    92,    -1,    91,    53,    92,    -1,    91,
      92,    -1,    93,    -1,    75,    -1,    95,    -1,    96,    -1,
      88,    29,    92,    -1,    88,    29,    94,    -1,    88,    29,
       1,    -1,    88,    33,    92,    -1,    88,    32,    92,    -1,
      88,    31,    92,    -1,    88,    30,    92,    -1,    41,    92,
      -1,    88,    47,    -1,    88,    46,    -1,    47,    88,    -1,
      46,    88,    -1,    88,    -1,    92,    40,    92,    -1,    92,
      41,    92,    -1,    92,    42,    92,    -1,    92,    43,    92,
      -1,    92,    49,    92,    -1,    92,    44,    92,    -1,    92,
      37,    92,    -1,    92,    36,    92,    -1,    92,    34,    92,
      -1,    92,    39,    92,    -1,    92,    35,    92,    -1,    92,
      38,    92,    -1,    92,    28,    92,    -1,    92,    27,    92,
      -1,    54,    92,    55,    -1,    48,    92,    -1,     6,    -1,
      50,    91,    51,    -1,    12,    54,    55,    -1,    12,    54,
      91,    55,    -1,    12,    -1,    13,    54,    91,    55,    -1,
      13,    54,    55,    -1,    13,    -1,    -1,    -1,    -1,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    70,    70,    71,    72,    76,    77,    83,    84,    88,
      92,    96,    97,    98,    99,   103,   104,   105,   111,   112,
     116,   120,   126,   127,   128,   129,   130,   131,   136,   137,
     141,   143,   145,   147,   147,   153,   153,   160,   162,   169,
     170,   171,   172,   178,   181,   187,   190,   195,   201,   204,
     207,   213,   216,   222,   223,   224,   228,   231,   234,   237,
     240,   243,   246,   252,   255,   258,   261,   264,   267,   270,
     276,   285,   291,   292,   293,   294,   298,   299,   300,   304,
     308,   309,   310,   311,   312,   312,   314,   320,   321,   322,
     326,   330,   331,   332,   333,   334,   335,   336,   337,   338,
     339,   340,   341,   342,   343,   344,   345,   346,   347,   348,
     349,   350,   351,   352,   353,   354,   355,   356,   357,   358,
     359,   360,   361,   362,   368,   374,   375,   376,   380,   381,
     382,   388,   392,   396,   400
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "INTCONST", "ELEMENTCONST",
  "DOUBLECONST", "NEWTOKEN", "NEWFUNCTOKEN", "CHARCONST", "STEPTOKEN",
  "VAR", "LOCALVAR", "FUNCCALL", "USERFUNCCALL", "VTYPE", "DO", "WHILE",
  "FOR", "IF", "IN", "RETURN", "FILTERBLOCK", "HELP", "DIOV", "DUMMY",
  "OPTION", "ELSE", "OR", "AND", "'='", "DEQ", "TEQ", "MEQ", "PEQ", "'>'",
  "'<'", "NEQ", "EQ", "LEQ", "GEQ", "'+'", "'-'", "'*'", "'/'", "'%'",
  "UMINUS", "MINUSMINUS", "PLUSPLUS", "'!'", "'^'", "'{'", "'}'", "';'",
  "','", "'('", "')'", "'['", "']'", "'.'", "$accept", "programlist",
  "program", "block", "pushscope", "popscope", "statementlist",
  "blockment", "optlist", "filter", "pushfilter", "statement", "decexpr",
  "flowstatement", "@1", "@2", "constant", "funcdeclaration",
  "vardeclaration", "arglist", "args", "argdeclaration", "declist",
  "vardec", "newname", "pushfunc", "widget", "step", "steplist", "var",
  "rawvar", "@3", "exprlist", "expr", "rawexpr", "ARRAYCONST", "func",
  "userfunc", "savetokenname", "savetype", "cleartype", "pushstepname", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,    61,
     284,   285,   286,   287,    62,    60,   288,   289,   290,   291,
      43,    45,    42,    47,    37,   292,   293,   294,    33,    94,
     123,   125,    59,    44,    40,    41,    91,    93,    46
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    59,    60,    60,    60,    61,    61,    62,    62,    63,
      64,    65,    65,    65,    65,    66,    66,    66,    67,    67,
      68,    69,    70,    70,    70,    70,    70,    70,    71,    71,
      72,    72,    72,    73,    72,    74,    72,    72,    72,    75,
      75,    75,    75,    76,    76,    77,    77,    78,    79,    79,
      79,    80,    80,    81,    81,    81,    82,    82,    82,    82,
      82,    82,    82,    83,    83,    83,    83,    83,    83,    83,
      84,    85,    86,    86,    86,    86,    87,    87,    87,    88,
      89,    89,    89,    89,    90,    89,    89,    91,    91,    91,
      92,    93,    93,    93,    93,    93,    93,    93,    93,    93,
      93,    93,    93,    93,    93,    93,    93,    93,    93,    93,
      93,    93,    93,    93,    93,    93,    93,    93,    93,    93,
      93,    93,    93,    93,    94,    95,    95,    95,    96,    96,
      96,    97,    98,    99,   100
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     1,     2,     1,     1,     5,     2,     0,
       0,     1,     1,     2,     2,     2,     1,     1,     4,     6,
       6,     0,     1,     1,     2,     2,     1,     2,     1,     1,
       7,     5,    10,     0,     9,     0,    11,     6,     8,     1,
       1,     1,     1,     6,     6,     3,     3,     3,     0,     1,
       3,     5,     3,     1,     3,     2,     4,     3,     3,     6,
       6,     3,     1,     1,     1,     1,     1,     1,     1,     2,
       0,     4,     5,     5,     4,     2,     1,     3,     2,     1,
       4,     1,     4,     1,     0,     4,     2,     1,     3,     2,
       1,     1,     1,     1,     3,     3,     3,     3,     3,     3,
       3,     2,     2,     2,     2,     2,     1,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     2,     1,     3,     3,     4,     1,     4,     3,
       1,     0,     0,     0,     0
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,    39,    42,    40,   123,    41,    81,    83,   127,   130,
     132,     9,     9,     9,     0,    26,    21,     0,   133,     0,
       0,     0,     0,     0,     9,     0,     0,     3,    16,     5,
      11,     6,     0,    22,    17,    91,    12,    28,    23,   106,
      79,    29,    90,    92,    93,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    25,     0,    24,     0,     0,   101,
     105,   104,   122,     8,     0,     0,     1,     4,    13,    14,
     132,    15,    27,     0,     0,     0,     0,     0,   103,   102,
      86,    84,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   125,     0,
      87,   129,     0,    46,   131,    70,    63,    65,    64,    67,
      68,    66,    45,    53,    62,     0,     0,     0,     0,     0,
      70,     0,     0,   121,     0,    96,     0,    94,    95,   100,
      99,    98,    97,     0,   120,   119,   115,   117,   114,   113,
     118,   116,   107,   108,   109,   110,   112,   111,    80,    82,
       0,   126,    89,   128,    69,     0,     0,    55,     0,     0,
       0,     0,   132,     0,   106,     0,   131,     0,     0,    71,
      10,     0,   134,    76,     0,    88,    48,     0,    54,    58,
      61,    57,     0,     0,     0,     0,     0,     0,    31,     0,
       0,     0,     0,     7,   124,    75,    78,     0,   132,     0,
      49,    44,    56,     0,    37,    62,     0,     0,     0,     0,
     131,    20,    43,     0,     0,    77,     0,     0,    47,     0,
       0,     0,     0,    33,    30,    18,     0,    74,     0,     0,
      52,    50,    59,    60,    38,     0,     0,     0,     0,    73,
      72,     0,    35,     0,    34,    19,    51,     0,    32,    36
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    26,    27,    28,    50,   193,    29,    30,   167,    31,
      55,    32,    33,    34,   237,   247,    35,    36,    37,   177,
     199,   200,   112,   113,   114,   155,    38,   173,   174,    39,
      40,   133,    99,    41,    42,   128,    43,    44,   154,    49,
      57,   195
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -177
static const yytype_int16 yypact[] =
{
     494,  -177,  -177,  -177,  -177,  -177,   -43,   -32,   -38,   -17,
    -177,  -177,  -177,  -177,    -9,   760,  -177,    30,  -177,    38,
     760,    18,    18,   760,    -1,   760,   280,  -177,  -177,   598,
    -177,  -177,   673,  -177,  -177,  -177,  -177,  -177,  -177,   131,
     -40,  1036,  -177,  -177,  -177,   760,   760,   388,   420,  1083,
     650,    44,    59,   760,  1036,    64,  -177,    57,   760,    54,
    -177,  -177,    54,  -177,   598,   856,  -177,  -177,  -177,  -177,
    -177,  -177,  -177,   104,   760,   760,   760,   760,  -177,  -177,
    -177,  -177,   760,   760,   760,   760,   760,   760,   760,   760,
     760,   760,   760,   760,   760,   760,   753,   784,  -177,   303,
    1036,  -177,   335,  -177,  -177,  -177,  -177,  -177,  -177,  -177,
    -177,  -177,    43,  -177,   -12,   103,   760,   728,   879,   115,
    -177,   356,   546,  -177,  1097,  -177,   760,   153,  -177,   153,
     153,   153,   153,   116,   153,   153,   415,   415,   415,   415,
     415,   415,    -8,    -8,    54,    54,    54,    54,  -177,  -177,
     760,  -177,  1036,  -177,  -177,    74,   236,  -177,   705,   760,
      82,   902,  -177,    85,   101,   650,  -177,    10,    74,  -177,
    -177,   526,  -177,  -177,   224,  1036,   125,    90,  -177,  -177,
    1036,  -177,   808,   760,   650,  1097,   760,   760,   120,   112,
     136,    90,    90,  -177,  -177,    34,  -177,   116,  -177,    49,
    -177,  -177,   124,   925,  -177,    -4,  1017,   948,   650,   119,
    -177,  -177,  -177,   441,   760,  -177,   236,   125,  -177,   578,
      97,   760,   760,  -177,  -177,  -177,   130,  -177,   367,   832,
     137,  -177,  1036,  -177,  -177,   971,   994,   650,   119,  -177,
    -177,   760,  -177,   650,  -177,  -177,  1036,   650,  -177,  -177
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -177,  -177,   139,  -165,    73,  -177,   105,   -27,  -177,  -177,
    -177,  -177,   -26,  -177,  -177,  -177,   -30,   -22,  -177,     2,
    -177,   -50,  -177,   -90,  -176,    53,    16,   -14,  -177,   -18,
    -177,  -177,   -37,   -15,  -177,  -138,  -177,  -177,  -127,   -69,
    -177,  -177
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -86
static const yytype_int16 yytable[] =
{
      54,   124,    68,    60,    61,    59,    72,    69,    62,   205,
      65,   102,   201,    45,    80,   221,    47,   158,    81,   111,
     181,   121,   157,   115,    46,   158,   211,   212,     6,     7,
      96,    97,   100,   100,    92,    93,    94,    48,   118,   189,
     230,    95,    56,   100,   159,    53,     1,     2,     3,   104,
      63,     5,   159,   106,   107,   108,   109,   110,   127,   129,
     130,   131,   132,   190,   120,   191,   178,   134,   135,   136,
     137,   138,   139,   140,   141,   142,   143,   144,   145,   146,
     147,   233,   111,   226,   152,    51,    52,   152,   213,   171,
     214,   163,    58,   185,   111,    68,   156,    64,   116,   164,
      69,   161,   217,    95,   218,   125,   152,     1,     2,     3,
       4,   100,     5,   117,     6,     7,     8,     9,   119,   160,
     187,   166,     1,     2,     3,   172,   111,     5,   176,   216,
      73,    74,    75,    76,    77,   175,   183,   186,   188,   198,
      24,   209,   210,   180,   182,    20,   208,    78,    79,   234,
      21,    22,    23,   219,   126,   111,   152,   204,    25,   238,
      73,    74,    75,    76,    77,    67,   241,   231,   203,   122,
     192,   206,   207,   168,   179,     0,   228,    78,    79,   225,
       0,   224,     0,   215,     0,     0,   111,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,   100,   229,
       0,     0,    95,     0,   232,     0,   235,   236,   245,     0,
     244,     0,     0,   152,     0,     0,   248,     0,     0,     0,
     249,     0,     0,     0,     0,   196,   246,   -85,   -85,   -85,
     -85,     0,   -85,     0,   -85,   -85,   -85,   -85,   -85,     1,
       2,     3,   104,   -85,     5,     0,   106,   107,   108,   109,
     110,   -85,   -85,   -85,   -85,   -85,   -85,   -85,   -85,   -85,
     -85,   -85,   -85,   -85,   -85,   -85,   -85,   -85,   -85,     0,
     -85,   -85,   -85,   -85,     0,   -85,   -85,   -85,   -85,   -85,
      66,   -85,   197,     1,     2,     3,     4,     0,     5,     0,
       6,     7,     8,     9,    10,    11,    12,    13,    14,     0,
      15,    16,    17,    18,     0,    19,     1,     2,     3,     4,
       0,     5,     0,     6,     7,     8,     9,     0,     0,     0,
       0,    20,     0,     0,     0,     0,    21,    22,    23,     0,
      24,     0,     0,     0,    25,     0,     0,     0,     1,     2,
       3,     4,     0,     5,    20,     6,     7,     8,     9,    21,
      22,    23,     0,     0,     0,     0,   150,    25,   151,     1,
       2,     3,     4,     0,     5,     0,     6,     7,     8,     9,
       1,     2,     3,     4,     0,     5,    20,     6,     7,     8,
       9,    21,    22,    23,     0,     0,     0,     0,   150,    25,
     153,     1,     2,     3,     4,     0,     5,    20,     6,     7,
       8,     9,    21,    22,    23,     0,     0,     0,    20,   150,
      25,   169,     0,    21,    22,    23,     0,     0,     0,     0,
     150,    25,   239,     1,     2,     3,     4,     0,     5,    20,
       6,     7,     8,     9,    21,    22,    23,     0,     0,     0,
       0,     0,    25,    98,     1,     2,     3,     4,     0,     5,
       0,     6,     7,     8,     9,    90,    91,    92,    93,    94,
       0,    20,     0,     0,    95,     0,    21,    22,    23,     0,
       0,     0,     0,     0,    25,   101,     0,     0,     0,     0,
       0,     0,    20,     0,     0,     0,     0,    21,    22,    23,
       0,     0,     0,     0,     0,    25,   227,     1,     2,     3,
       4,     0,     5,     0,     6,     7,     8,     9,    10,    11,
      12,    13,    14,     0,    15,    16,    17,    18,     0,    19,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     1,
       2,     3,     4,     0,     5,    20,     6,     7,     8,     9,
      21,    22,    23,     0,    24,     0,     0,     0,    25,     1,
       2,     3,     4,     0,     5,     0,     6,     7,     8,     9,
      10,    11,    12,    13,    14,     0,    15,    20,    17,    18,
       0,    19,    21,    22,    23,     0,     0,   194,     0,   150,
      25,     1,     2,     3,     4,     0,     5,    20,     6,     7,
       8,     9,    21,    22,    23,     0,    24,   170,     0,     0,
      25,     1,     2,     3,     4,     0,     5,     0,     6,     7,
       8,     9,    10,    11,    12,    13,    14,     0,    15,    20,
      17,    18,     0,    19,    21,    22,    23,     0,   126,     0,
       0,     0,    25,     0,     0,     0,     0,     0,     0,    20,
       0,     0,     0,     0,    21,    22,    23,     0,    24,     0,
       0,     0,    25,     1,     2,     3,     4,     0,     5,     0,
       6,     7,     8,     9,    70,    11,    12,    13,    14,     0,
      15,     0,    17,     0,     0,    19,     1,     2,     3,     4,
       0,     5,     0,     6,     7,     8,     9,    70,     0,     0,
       0,    20,     0,     0,     0,     0,    21,    22,    23,     0,
      24,     0,     0,     0,    25,     0,     0,     0,     1,     2,
       3,     4,     0,     5,    20,     6,     7,     8,     9,    21,
      22,    23,     0,     0,     0,    71,     0,    25,     0,     0,
      19,     1,     2,     3,     4,     0,     5,     0,     6,     7,
       8,     9,   162,     0,     0,     0,    20,     0,     0,     0,
       0,    21,    22,    23,     0,   126,     0,     0,     0,    25,
       0,     0,     0,     1,     2,     3,     4,     0,     5,    20,
       6,     7,     8,     9,    21,    22,    23,     0,     0,     0,
      82,    83,    25,     0,     0,     0,     0,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,     0,     0,
       0,    20,    95,     0,     0,     0,    21,    22,    23,     0,
     148,    82,    83,     0,    25,     0,     0,     0,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,     0,
       0,     0,     0,    95,     0,    82,    83,     0,     0,     0,
       0,   149,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,     0,     0,     0,     0,    95,     0,    82,
      83,     0,     0,     0,     0,   202,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,     0,     0,     0,
       0,    95,     0,    82,    83,     0,     0,     0,     0,   240,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,     0,     0,     0,     0,    95,    82,    83,     0,     0,
       0,   123,     0,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,     0,     0,     0,     0,    95,    82,
      83,     0,     0,     0,   165,     0,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,     0,     0,     0,
       0,    95,    82,    83,     0,     0,     0,   184,     0,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
       0,     0,     0,     0,    95,    82,    83,     0,     0,     0,
     220,     0,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,     0,     0,     0,     0,    95,    82,    83,
       0,     0,     0,   223,     0,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,     0,     0,     0,     0,
      95,    82,    83,     0,     0,     0,   242,     0,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,     0,
       0,     0,     0,    95,    82,    83,     0,     0,     0,   243,
       0,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,     0,    82,    83,     0,    95,     0,     0,   222,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,     0,     0,     0,   103,    95,     1,     2,     3,   104,
     105,     5,     0,   106,   107,   108,   109,   110,   103,     0,
       1,     2,     3,   104,     0,     5,     0,   106,   107,   108,
     109,   110
};

static const yytype_int16 yycheck[] =
{
      15,    70,    29,    21,    22,    20,    32,    29,    23,   185,
      25,    48,   177,    56,    54,    19,    54,    29,    58,    49,
     158,    58,   112,    50,    56,    29,   191,   192,    10,    11,
      45,    46,    47,    48,    42,    43,    44,    54,    53,   166,
     216,    49,    12,    58,    56,    54,     3,     4,     5,     6,
      51,     8,    56,    10,    11,    12,    13,    14,    73,    74,
      75,    76,    77,    53,     7,    55,   156,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,   219,   112,   210,    99,    12,    13,   102,    54,   126,
      56,   117,    54,   162,   124,   122,    53,    24,    54,   117,
     122,   116,    53,    49,    55,     1,   121,     3,     4,     5,
       6,   126,     8,    54,    10,    11,    12,    13,    54,    16,
      19,     6,     3,     4,     5,     9,   156,     8,    54,   198,
      29,    30,    31,    32,    33,   150,    54,    52,   165,    14,
      50,    29,     6,   158,   159,    41,    26,    46,    47,    52,
      46,    47,    48,    29,    50,   185,   171,   184,    54,    29,
      29,    30,    31,    32,    33,    26,    29,   217,   183,    64,
     168,   186,   187,   120,   158,    -1,   213,    46,    47,   209,
      -1,   208,    -1,   197,    -1,    -1,   216,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,   213,   214,
      -1,    -1,    49,    -1,   219,    -1,   221,   222,   238,    -1,
     237,    -1,    -1,   228,    -1,    -1,   243,    -1,    -1,    -1,
     247,    -1,    -1,    -1,    -1,     1,   241,     3,     4,     5,
       6,    -1,     8,    -1,    10,    11,    12,    13,    14,     3,
       4,     5,     6,    19,     8,    -1,    10,    11,    12,    13,
      14,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      46,    47,    48,    49,    -1,    51,    52,    53,    54,    55,
       0,    57,    58,     3,     4,     5,     6,    -1,     8,    -1,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    21,    22,    23,    -1,    25,     3,     4,     5,     6,
      -1,     8,    -1,    10,    11,    12,    13,    -1,    -1,    -1,
      -1,    41,    -1,    -1,    -1,    -1,    46,    47,    48,    -1,
      50,    -1,    -1,    -1,    54,    -1,    -1,    -1,     3,     4,
       5,     6,    -1,     8,    41,    10,    11,    12,    13,    46,
      47,    48,    -1,    -1,    -1,    -1,    53,    54,    55,     3,
       4,     5,     6,    -1,     8,    -1,    10,    11,    12,    13,
       3,     4,     5,     6,    -1,     8,    41,    10,    11,    12,
      13,    46,    47,    48,    -1,    -1,    -1,    -1,    53,    54,
      55,     3,     4,     5,     6,    -1,     8,    41,    10,    11,
      12,    13,    46,    47,    48,    -1,    -1,    -1,    41,    53,
      54,    55,    -1,    46,    47,    48,    -1,    -1,    -1,    -1,
      53,    54,    55,     3,     4,     5,     6,    -1,     8,    41,
      10,    11,    12,    13,    46,    47,    48,    -1,    -1,    -1,
      -1,    -1,    54,    55,     3,     4,     5,     6,    -1,     8,
      -1,    10,    11,    12,    13,    40,    41,    42,    43,    44,
      -1,    41,    -1,    -1,    49,    -1,    46,    47,    48,    -1,
      -1,    -1,    -1,    -1,    54,    55,    -1,    -1,    -1,    -1,
      -1,    -1,    41,    -1,    -1,    -1,    -1,    46,    47,    48,
      -1,    -1,    -1,    -1,    -1,    54,    55,     3,     4,     5,
       6,    -1,     8,    -1,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    21,    22,    23,    -1,    25,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,    -1,     8,    41,    10,    11,    12,    13,
      46,    47,    48,    -1,    50,    -1,    -1,    -1,    54,     3,
       4,     5,     6,    -1,     8,    -1,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    41,    22,    23,
      -1,    25,    46,    47,    48,    -1,    -1,    51,    -1,    53,
      54,     3,     4,     5,     6,    -1,     8,    41,    10,    11,
      12,    13,    46,    47,    48,    -1,    50,    51,    -1,    -1,
      54,     3,     4,     5,     6,    -1,     8,    -1,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    41,
      22,    23,    -1,    25,    46,    47,    48,    -1,    50,    -1,
      -1,    -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,    41,
      -1,    -1,    -1,    -1,    46,    47,    48,    -1,    50,    -1,
      -1,    -1,    54,     3,     4,     5,     6,    -1,     8,    -1,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    -1,    -1,    25,     3,     4,     5,     6,
      -1,     8,    -1,    10,    11,    12,    13,    14,    -1,    -1,
      -1,    41,    -1,    -1,    -1,    -1,    46,    47,    48,    -1,
      50,    -1,    -1,    -1,    54,    -1,    -1,    -1,     3,     4,
       5,     6,    -1,     8,    41,    10,    11,    12,    13,    46,
      47,    48,    -1,    -1,    -1,    52,    -1,    54,    -1,    -1,
      25,     3,     4,     5,     6,    -1,     8,    -1,    10,    11,
      12,    13,    14,    -1,    -1,    -1,    41,    -1,    -1,    -1,
      -1,    46,    47,    48,    -1,    50,    -1,    -1,    -1,    54,
      -1,    -1,    -1,     3,     4,     5,     6,    -1,     8,    41,
      10,    11,    12,    13,    46,    47,    48,    -1,    -1,    -1,
      27,    28,    54,    -1,    -1,    -1,    -1,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    -1,    -1,
      -1,    41,    49,    -1,    -1,    -1,    46,    47,    48,    -1,
      57,    27,    28,    -1,    54,    -1,    -1,    -1,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    -1,    49,    -1,    27,    28,    -1,    -1,    -1,
      -1,    57,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    -1,    49,    -1,    27,
      28,    -1,    -1,    -1,    -1,    57,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    -1,    -1,    -1,
      -1,    49,    -1,    27,    28,    -1,    -1,    -1,    -1,    57,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    -1,    -1,    -1,    -1,    49,    27,    28,    -1,    -1,
      -1,    55,    -1,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    -1,    -1,    49,    27,
      28,    -1,    -1,    -1,    55,    -1,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    -1,    -1,    -1,
      -1,    49,    27,    28,    -1,    -1,    -1,    55,    -1,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      -1,    -1,    -1,    -1,    49,    27,    28,    -1,    -1,    -1,
      55,    -1,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    -1,    49,    27,    28,
      -1,    -1,    -1,    55,    -1,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,
      49,    27,    28,    -1,    -1,    -1,    55,    -1,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    -1,    49,    27,    28,    -1,    -1,    -1,    55,
      -1,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    -1,    27,    28,    -1,    49,    -1,    -1,    52,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    -1,    -1,    -1,     1,    49,     3,     4,     5,     6,
       7,     8,    -1,    10,    11,    12,    13,    14,     1,    -1,
       3,     4,     5,     6,    -1,     8,    -1,    10,    11,    12,
      13,    14
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     5,     6,     8,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    20,    21,    22,    23,    25,
      41,    46,    47,    48,    50,    54,    60,    61,    62,    65,
      66,    68,    70,    71,    72,    75,    76,    77,    85,    88,
      89,    92,    93,    95,    96,    56,    56,    54,    54,    98,
      63,    63,    63,    54,    92,    69,    12,    99,    54,    92,
      88,    88,    92,    51,    63,    92,     0,    61,    66,    76,
      14,    52,    71,    29,    30,    31,    32,    33,    46,    47,
      54,    58,    27,    28,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    49,    92,    92,    55,    91,
      92,    55,    91,     1,     6,     7,    10,    11,    12,    13,
      14,    75,    81,    82,    83,    66,    54,    54,    92,    54,
       7,    91,    65,    55,    98,     1,    50,    92,    94,    92,
      92,    92,    92,    90,    92,    92,    92,    92,    92,    92,
      92,    92,    92,    92,    92,    92,    92,    92,    57,    57,
      53,    55,    92,    55,    97,    84,    53,    82,    29,    56,
      16,    92,    14,    71,    88,    55,     6,    67,    84,    55,
      51,    91,     9,    86,    87,    92,    54,    78,    82,    85,
      92,    94,    92,    54,    55,    98,    52,    19,    66,    97,
      53,    55,    78,    64,    51,   100,     1,    58,    14,    79,
      80,    62,    57,    92,    66,    83,    92,    92,    26,    29,
       6,    62,    62,    54,    56,    86,    98,    53,    55,    29,
      55,    19,    52,    55,    66,    75,    97,    55,    91,    92,
      83,    80,    92,    94,    52,    92,    92,    73,    29,    55,
      57,    29,    55,    55,    66,    75,    92,    74,    66,    66
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
#line 70 "grammar.yy"
    { }
    break;

  case 3:
#line 71 "grammar.yy"
    { }
    break;

  case 4:
#line 72 "grammar.yy"
    { }
    break;

  case 5:
#line 76 "grammar.yy"
    { if (((yyvsp[(1) - (1)].node) != NULL) && (!cmdparser.addStatement((yyvsp[(1) - (1)].node)))) YYABORT; }
    break;

  case 6:
#line 77 "grammar.yy"
    { }
    break;

  case 7:
#line 83 "grammar.yy"
    { (yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (5)].node),(yyvsp[(3) - (5)].node)); }
    break;

  case 8:
#line 84 "grammar.yy"
    { (yyval.node) = cmdparser.addFunction(Command::NoFunction); }
    break;

  case 9:
#line 88 "grammar.yy"
    { (yyval.node) = cmdparser.pushScope(); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 10:
#line 92 "grammar.yy"
    { if (!cmdparser.popScope()) YYABORT; }
    break;

  case 11:
#line 96 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 12:
#line 97 "grammar.yy"
    { (yyval.node) = NULL; }
    break;

  case 13:
#line 98 "grammar.yy"
    { (yyval.node) = cmdparser.joinCommands((yyvsp[(1) - (2)].node), (yyvsp[(2) - (2)].node)); }
    break;

  case 14:
#line 99 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (2)].node); }
    break;

  case 15:
#line 103 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (2)].node); }
    break;

  case 16:
#line 104 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 17:
#line 105 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 18:
#line 111 "grammar.yy"
    { if (!cmdparser.setFilterOption(&tokenName, (yyvsp[(4) - (4)].node))) YYABORT; }
    break;

  case 19:
#line 112 "grammar.yy"
    { if (!cmdparser.setFilterOption(&tokenName, (yyvsp[(6) - (6)].node))) YYABORT; }
    break;

  case 20:
#line 116 "grammar.yy"
    { if (!cmdparser.addStatement((yyvsp[(6) - (6)].node))) YYABORT; cmdparser.popTree(); }
    break;

  case 21:
#line 120 "grammar.yy"
    { cmdparser.pushTree(TRUE); }
    break;

  case 22:
#line 126 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 23:
#line 127 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 24:
#line 128 "grammar.yy"
    { (yyval.node) = cmdparser.addFunction(Command::Help, cmdparser.addConstant((yyvsp[(2) - (2)].functionId))); }
    break;

  case 25:
#line 129 "grammar.yy"
    { (yyval.node) = cmdparser.addFunction(Command::Return,(yyvsp[(2) - (2)].node)); }
    break;

  case 26:
#line 130 "grammar.yy"
    { (yyval.node) = cmdparser.addFunction(Command::Return); }
    break;

  case 27:
#line 131 "grammar.yy"
    { msg.print("Error: Expected ';' before current expression.\n"); YYABORT; }
    break;

  case 28:
#line 136 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 29:
#line 137 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 30:
#line 141 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::If,(yyvsp[(3) - (7)].node),(yyvsp[(5) - (7)].node),(yyvsp[(7) - (7)].node)); }
    break;

  case 31:
#line 143 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::If,(yyvsp[(3) - (5)].node),(yyvsp[(5) - (5)].node)); }
    break;

  case 32:
#line 145 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (10)].node), cmdparser.addFunction(Command::For, (yyvsp[(4) - (10)].node),(yyvsp[(6) - (10)].node),(yyvsp[(8) - (10)].node),(yyvsp[(10) - (10)].node))); cmdparser.popScope(); }
    break;

  case 33:
#line 147 "grammar.yy"
    {
		if ((yyvsp[(4) - (7)].node)->returnType() <= VTypes::VectorData) { msg.print("Error: For/In loop variable must be of pointer type.\n"); YYABORT; }
		if ((yyvsp[(4) - (7)].node)->returnType() != (yyvsp[(6) - (7)].node)->returnType()) { msg.print("Error: For/In loop variable is not being assigned the correct type.\n"); YYABORT; }
		}
    break;

  case 34:
#line 151 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (9)].node), cmdparser.addFunction(Command::ForIn,(yyvsp[(4) - (9)].node),(yyvsp[(6) - (9)].node),(yyvsp[(9) - (9)].node))); cmdparser.popScope(); }
    break;

  case 35:
#line 153 "grammar.yy"
    { 
		if (declaredType <= VTypes::VectorData) { msg.print("Error: For/In loop variable must be of pointer type.\n"); YYABORT; }
		tempNode = cmdparser.addVariable(declaredType, &tokenName);
		if (declaredType != (yyvsp[(8) - (9)].node)->returnType()) { msg.print("Error: For/In loop variable is not being assigned the correct type.\n"); YYABORT; }
		}
    break;

  case 36:
#line 158 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (11)].node), cmdparser.addFunction(Command::ForIn,tempNode,(yyvsp[(8) - (11)].node),(yyvsp[(11) - (11)].node))); cmdparser.popScope(); }
    break;

  case 37:
#line 160 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (6)].node), cmdparser.addFunction(Command::While, (yyvsp[(4) - (6)].node),(yyvsp[(6) - (6)].node))); cmdparser.popScope(); }
    break;

  case 38:
#line 162 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (8)].node), cmdparser.addFunction(Command::DoWhile, (yyvsp[(3) - (8)].node),(yyvsp[(6) - (8)].node))); cmdparser.popScope(); }
    break;

  case 39:
#line 169 "grammar.yy"
    { (yyval.node) = cmdparser.addConstant((yyvsp[(1) - (1)].intconst)); }
    break;

  case 40:
#line 170 "grammar.yy"
    { (yyval.node) = cmdparser.addConstant((yyvsp[(1) - (1)].doubleconst)); }
    break;

  case 41:
#line 171 "grammar.yy"
    { (yyval.node) = cmdparser.addConstant((yyvsp[(1) - (1)].name)->get()); }
    break;

  case 42:
#line 172 "grammar.yy"
    { (yyval.node) = cmdparser.addElementConstant((yyvsp[(1) - (1)].intconst)); }
    break;

  case 43:
#line 178 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : funcdeclaration : user-defined statement\n");
		if (!cmdparser.addStatement((yyvsp[(6) - (6)].node))) YYABORT; cmdparser.popTree(); declaredType = VTypes::NoData; }
    break;

  case 44:
#line 181 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : funcdeclaration : user-defined function\n");
		if (!cmdparser.addStatement((yyvsp[(6) - (6)].node))) YYABORT; cmdparser.popTree(); declaredType = VTypes::NoData; }
    break;

  case 45:
#line 187 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : vardeclaration : standard variable declaration list\n");
		(yyval.node) = cmdparser.addDeclarations((yyvsp[(3) - (3)].node)); declaredType = VTypes::NoData; }
    break;

  case 46:
#line 190 "grammar.yy"
    {
		msg.print("Illegal use of reserved word '%s'.\n", VTypes::dataType(declaredType)); YYABORT; }
    break;

  case 47:
#line 195 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : arglist : bracketed argument list\n");
		(yyval.node) = (yyvsp[(2) - (3)].node); }
    break;

  case 48:
#line 201 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : args : empty\n");
		(yyval.node) = NULL; }
    break;

  case 49:
#line 204 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : args : adding item\n");
		(yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 50:
#line 207 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : args : joining items\n");
		if ((yyvsp[(3) - (3)].node) == NULL) YYABORT; (yyval.node) = Tree::joinArguments((yyvsp[(3) - (3)].node),(yyvsp[(1) - (3)].node)); }
    break;

  case 51:
#line 213 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : argdeclaration : function argument '%s' with expr assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addVariableAsArgument(declaredType, &tokenName, (yyvsp[(5) - (5)].node)); }
    break;

  case 52:
#line 216 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : argdeclaration : function argument '%s'\n", tokenName.get());
		(yyval.node) = cmdparser.addVariableAsArgument(declaredType, &tokenName); }
    break;

  case 53:
#line 222 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyvsp[(1) - (1)].node) == NULL) YYABORT; }
    break;

  case 54:
#line 223 "grammar.yy"
    { if ((yyvsp[(3) - (3)].node) == NULL) YYABORT; (yyval.node) = Tree::joinArguments((yyvsp[(3) - (3)].node),(yyvsp[(1) - (3)].node)); }
    break;

  case 55:
#line 224 "grammar.yy"
    { msg.print("Error: Missing comma between declarations?\n"); YYABORT; }
    break;

  case 56:
#line 228 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : vardec : array var '%s'\n", tokenName.get());
		(yyval.node) = cmdparser.addArrayVariable(declaredType, &tokenName, (yyvsp[(3) - (4)].node)); }
    break;

  case 57:
#line 231 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : vardec : var '%s' with array assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addVariable(declaredType, &tokenName, (yyvsp[(3) - (3)].node)); }
    break;

  case 58:
#line 234 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : vardec : var '%s' with widget assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addVariable(declaredType, &tokenName, (yyvsp[(3) - (3)].node)); }
    break;

  case 59:
#line 237 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : vardec : array var '%s' with expr assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addArrayVariable(declaredType, &tokenName,(yyvsp[(3) - (6)].node),(yyvsp[(6) - (6)].node)); }
    break;

  case 60:
#line 240 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : vardec : array var '%s' with array assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addArrayVariable(declaredType, &tokenName,(yyvsp[(3) - (6)].node),(yyvsp[(6) - (6)].node)); }
    break;

  case 61:
#line 243 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : vardec : var '%s' with expr assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addVariable(declaredType, &tokenName, (yyvsp[(3) - (3)].node)); }
    break;

  case 62:
#line 246 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : vardec : var '%s'\n", tokenName.get());
		(yyval.node) = cmdparser.addVariable(declaredType, &tokenName); }
    break;

  case 63:
#line 252 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : newname : existing var '%s'\n", tokenName.get());
		tokenName = yylval.variable->name(); (yyval.name) = &tokenName; }
    break;

  case 64:
#line 255 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : newname : existing built-in function '%s'\n", tokenName.get());
		tokenName = Command::data[yylval.functionId].keyword; (yyval.name) = &tokenName; }
    break;

  case 65:
#line 258 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : newname : existing local var '%s'\n", tokenName.get());
		msg.print("Error: Existing variable in local scope cannot be redeclared.\n"); YYABORT; }
    break;

  case 66:
#line 261 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : newname : constant '%s'\n", tokenName.get());
		msg.print("Error: Constant value found in declaration.\n"); YYABORT; }
    break;

  case 67:
#line 264 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : newname : existing user function '%s'\n", tokenName.get());
		msg.print("Error: Existing user-defined function name cannot be redeclared.\n"); YYABORT; }
    break;

  case 68:
#line 267 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : newname : variable type-name '%s'\n", VTypes::dataType( yylval.vtype));
		msg.print("Error: Type-name used in variable declaration.\n"); YYABORT; }
    break;

  case 69:
#line 270 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : newname : new token '%s'\n", tokenName.get());
		if (declaredType == VTypes::NoData) { msg.print("Token '%s' is undeclared.\n", tokenName.get()); YYABORT; } (yyval.name) = (yyvsp[(1) - (2)].name); }
    break;

  case 70:
#line 276 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : pushfunc : function/statement '%s'\n", yylval.name->get());
		cmdparser.pushFunction(yylval.name->get(), declaredType); }
    break;

  case 71:
#line 285 "grammar.yy"
    { (yyval.node) = cmdparser.addWidget((yyvsp[(3) - (4)].node)); }
    break;

  case 72:
#line 291 "grammar.yy"
    { if (!cmdparser.expandPath(stepNameStack.last(), (yyvsp[(4) - (5)].node))) YYABORT; stepNameStack.removeLast(); }
    break;

  case 73:
#line 292 "grammar.yy"
    { if (!cmdparser.expandPath(stepNameStack.last(), NULL, (yyvsp[(4) - (5)].node))) YYABORT; stepNameStack.removeLast(); }
    break;

  case 74:
#line 293 "grammar.yy"
    { if (!cmdparser.expandPath(stepNameStack.last(), NULL, NULL)) YYABORT; stepNameStack.removeLast(); }
    break;

  case 75:
#line 294 "grammar.yy"
    { if (!cmdparser.expandPath((yyvsp[(1) - (2)].name))) YYABORT; stepNameStack.removeLast(); }
    break;

  case 76:
#line 298 "grammar.yy"
    { }
    break;

  case 77:
#line 299 "grammar.yy"
    { }
    break;

  case 78:
#line 300 "grammar.yy"
    { msg.print("Error formulating path.\n"); YYABORT; }
    break;

  case 79:
#line 304 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 80:
#line 308 "grammar.yy"
    { (yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (4)].variable),(yyvsp[(3) - (4)].node)); }
    break;

  case 81:
#line 309 "grammar.yy"
    { (yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (1)].variable)); }
    break;

  case 82:
#line 310 "grammar.yy"
    { (yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (4)].variable),(yyvsp[(3) - (4)].node)); }
    break;

  case 83:
#line 311 "grammar.yy"
    { (yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (1)].variable)); }
    break;

  case 84:
#line 312 "grammar.yy"
    { cmdparser.createPath((yyvsp[(1) - (2)].node)); }
    break;

  case 85:
#line 313 "grammar.yy"
    { (yyval.node) = cmdparser.finalisePath(); }
    break;

  case 86:
#line 314 "grammar.yy"
    { msg.print("Can't use a variable as a function. Did you mean '[' instead?\n"); (yyval.node) = NULL; }
    break;

  case 87:
#line 320 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 88:
#line 321 "grammar.yy"
    { (yyval.node) = Tree::joinArguments((yyvsp[(3) - (3)].node),(yyvsp[(1) - (3)].node)); }
    break;

  case 89:
#line 322 "grammar.yy"
    { msg.print("Error: Missing comma between items.\n"); YYABORT; }
    break;

  case 90:
#line 326 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 91:
#line 330 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 92:
#line 331 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 93:
#line 332 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 94:
#line 333 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignment,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); }
    break;

  case 95:
#line 334 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignment,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); }
    break;

  case 96:
#line 335 "grammar.yy"
    { msg.print("Mangled expression used in assignment.\n"); YYABORT; }
    break;

  case 97:
#line 336 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignmentPlus,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); }
    break;

  case 98:
#line 337 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignmentSubtract,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); }
    break;

  case 99:
#line 338 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignmentMultiply,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); }
    break;

  case 100:
#line 339 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignmentDivide,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); }
    break;

  case 101:
#line 340 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorNegate, (yyvsp[(2) - (2)].node)); }
    break;

  case 102:
#line 341 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPostfixIncrease, (yyvsp[(1) - (2)].node));  }
    break;

  case 103:
#line 342 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPostfixDecrease, (yyvsp[(1) - (2)].node)); }
    break;

  case 104:
#line 343 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPrefixIncrease, (yyvsp[(2) - (2)].node)); }
    break;

  case 105:
#line 344 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPrefixDecrease, (yyvsp[(2) - (2)].node)); }
    break;

  case 106:
#line 345 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 107:
#line 346 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAdd, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 108:
#line 347 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorSubtract, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 109:
#line 348 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorMultiply, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 110:
#line 349 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorDivide, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 111:
#line 350 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPower, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 112:
#line 351 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorModulus, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 113:
#line 352 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 114:
#line 353 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorNotEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 115:
#line 354 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorGreaterThan, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 116:
#line 355 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorGreaterThanEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 117:
#line 356 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorLessThan, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 118:
#line 357 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorLessThanEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 119:
#line 358 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAnd, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 120:
#line 359 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorOr, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 121:
#line 360 "grammar.yy"
    { (yyval.node) = (yyvsp[(2) - (3)].node); }
    break;

  case 122:
#line 361 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorNot, (yyvsp[(2) - (2)].node)); }
    break;

  case 123:
#line 362 "grammar.yy"
    { msg.print("Error: '%s' has not been declared as a function or a variable.\n", yylval.name->get()); YYABORT; }
    break;

  case 124:
#line 368 "grammar.yy"
    { (yyval.node) = cmdparser.addArrayConstant((yyvsp[(2) - (3)].node)); }
    break;

  case 125:
#line 374 "grammar.yy"
    { (yyval.node) = cmdparser.addFunction( (Command::Function) (yyvsp[(1) - (3)].functionId)); }
    break;

  case 126:
#line 375 "grammar.yy"
    { (yyval.node) = cmdparser.addFunctionWithArglist( (Command::Function) (yyvsp[(1) - (4)].functionId),(yyvsp[(3) - (4)].node)); }
    break;

  case 127:
#line 376 "grammar.yy"
    { (yyval.node) = cmdparser.addFunction( (Command::Function) (yyvsp[(1) - (1)].functionId)); }
    break;

  case 128:
#line 380 "grammar.yy"
    { (yyval.node) = cmdparser.addUserFunction((yyvsp[(1) - (4)].functree),(yyvsp[(3) - (4)].node)); }
    break;

  case 129:
#line 381 "grammar.yy"
    { (yyval.node) = cmdparser.addUserFunction((yyvsp[(1) - (3)].functree)); }
    break;

  case 130:
#line 382 "grammar.yy"
    { (yyval.node) = cmdparser.addUserFunction((yyvsp[(1) - (1)].functree)); }
    break;

  case 131:
#line 388 "grammar.yy"
    { tokenName = *yylval.name; }
    break;

  case 132:
#line 392 "grammar.yy"
    { declaredType = yylval.vtype; }
    break;

  case 133:
#line 396 "grammar.yy"
    { declaredType = VTypes::NoData; }
    break;

  case 134:
#line 400 "grammar.yy"
    { stepNameStack.add()->set(yylval.name->get()); }
    break;


/* Line 1267 of yacc.c.  */
#line 2557 "grammar.cc"
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


#line 403 "grammar.yy"


void yyerror(char *s)
{
}

