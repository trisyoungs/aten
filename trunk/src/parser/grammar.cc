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
     DO = 269,
     WHILE = 270,
     FOR = 271,
     IF = 272,
     RETURN = 273,
     FILTERBLOCK = 274,
     HELP = 275,
     DIOV = 276,
     DUMMY = 277,
     OPTION = 278,
     ELSE = 279,
     OR = 280,
     AND = 281,
     DEQ = 282,
     TEQ = 283,
     MEQ = 284,
     PEQ = 285,
     NEQ = 286,
     EQ = 287,
     LEQ = 288,
     GEQ = 289,
     UMINUS = 290,
     MINUSMINUS = 291,
     PLUSPLUS = 292
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
#define DO 269
#define WHILE 270
#define FOR 271
#define IF 272
#define RETURN 273
#define FILTERBLOCK 274
#define HELP 275
#define DIOV 276
#define DUMMY 277
#define OPTION 278
#define ELSE 279
#define OR 280
#define AND 281
#define DEQ 282
#define TEQ 283
#define MEQ 284
#define PEQ 285
#define NEQ 286
#define EQ 287
#define LEQ 288
#define GEQ 289
#define UMINUS 290
#define MINUSMINUS 291
#define PLUSPLUS 292




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
VTypes::DataType declaredType;



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
/* Line 187 of yacc.c.  */
#line 210 "grammar.cc"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 223 "grammar.cc"

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
#define YYFINAL  67
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   992

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  57
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  41
/* YYNRULES -- Number of rules.  */
#define YYNRULES  130
/* YYNRULES -- Number of states.  */
#define YYNSTATES  238

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   292

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    46,     2,     2,     2,    42,     2,     2,
      52,    53,    40,    38,    51,    39,    56,    41,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    50,
      33,    27,    32,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    54,     2,    55,    47,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    48,     2,    49,     2,     2,     2,     2,
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
      25,    26,    28,    29,    30,    31,    34,    35,    36,    37,
      43,    44,    45
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     4,     6,     9,    11,    13,    19,    22,
      23,    24,    26,    29,    32,    34,    36,    38,    40,    45,
      52,    59,    60,    62,    65,    68,    70,    73,    75,    77,
      79,    87,    93,   104,   111,   119,   121,   123,   125,   127,
     136,   145,   146,   148,   152,   157,   164,   165,   167,   169,
     173,   176,   181,   185,   189,   193,   200,   207,   209,   211,
     213,   215,   217,   219,   221,   224,   228,   232,   237,   243,
     249,   254,   257,   259,   263,   266,   268,   273,   275,   280,
     282,   283,   288,   291,   293,   297,   300,   302,   304,   306,
     308,   312,   316,   320,   324,   328,   332,   336,   339,   342,
     345,   348,   351,   353,   357,   361,   365,   369,   373,   377,
     381,   385,   389,   393,   397,   401,   405,   409,   413,   416,
     418,   422,   426,   431,   433,   438,   442,   444,   445,   446,
     447
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      58,     0,    -1,    -1,    59,    -1,    58,    59,    -1,    63,
      -1,    66,    -1,    48,    61,    63,    49,    62,    -1,    48,
      49,    -1,    -1,    -1,    64,    -1,    63,    64,    -1,    68,
      50,    -1,    60,    -1,    70,    -1,    72,    -1,    73,    -1,
       6,    94,    27,    71,    -1,    65,    51,     6,    94,    27,
      71,    -1,    19,    67,    52,    65,    53,    60,    -1,    -1,
      69,    -1,    20,    11,    -1,    18,    89,    -1,    18,    -1,
      68,    69,    -1,    81,    -1,    89,    -1,    82,    -1,    17,
      52,    89,    53,    64,    24,    64,    -1,    17,    52,    89,
      53,    64,    -1,    16,    61,    52,    69,    50,    89,    50,
      89,    53,    64,    -1,    15,    61,    52,    89,    53,    64,
      -1,    14,    61,    64,    15,    52,    89,    53,    -1,     3,
      -1,     5,    -1,     7,    -1,     4,    -1,    13,    95,     6,
      52,    76,    74,    53,    60,    -1,    21,    96,     6,    52,
      76,    74,    53,    60,    -1,    -1,    75,    -1,    74,    51,
      75,    -1,    13,    95,     6,    94,    -1,    13,    95,     6,
      94,    27,    89,    -1,    -1,    79,    -1,    77,    -1,    78,
      51,    77,    -1,    78,    77,    -1,    80,    54,    89,    55,
      -1,    80,    27,    89,    -1,    80,    27,    91,    -1,    80,
      27,    82,    -1,    80,    54,    89,    55,    27,    89,    -1,
      80,    54,    89,    55,    27,    91,    -1,    80,    -1,     9,
      -1,    11,    -1,    10,    -1,    71,    -1,    12,    -1,    13,
      -1,     6,    94,    -1,    13,    95,    78,    -1,    13,    95,
       1,    -1,    23,    52,    88,    53,    -1,     8,    97,    54,
      89,    55,    -1,     8,    97,    52,    88,    53,    -1,     8,
      97,    52,    53,    -1,     8,    97,    -1,    83,    -1,    84,
      56,    83,    -1,    84,     1,    -1,    86,    -1,     9,    54,
      89,    55,    -1,     9,    -1,    10,    54,    89,    55,    -1,
      10,    -1,    -1,    86,    56,    87,    84,    -1,    86,    52,
      -1,    89,    -1,    88,    51,    89,    -1,    88,    89,    -1,
      90,    -1,    71,    -1,    92,    -1,    93,    -1,    85,    27,
      89,    -1,    85,    27,    91,    -1,    85,    27,     1,    -1,
      85,    31,    89,    -1,    85,    30,    89,    -1,    85,    29,
      89,    -1,    85,    28,    89,    -1,    39,    89,    -1,    85,
      45,    -1,    85,    44,    -1,    45,    85,    -1,    44,    85,
      -1,    85,    -1,    89,    38,    89,    -1,    89,    39,    89,
      -1,    89,    40,    89,    -1,    89,    41,    89,    -1,    89,
      47,    89,    -1,    89,    42,    89,    -1,    89,    35,    89,
      -1,    89,    34,    89,    -1,    89,    32,    89,    -1,    89,
      37,    89,    -1,    89,    33,    89,    -1,    89,    36,    89,
      -1,    89,    26,    89,    -1,    89,    25,    89,    -1,    52,
      89,    53,    -1,    46,    89,    -1,     6,    -1,    48,    88,
      49,    -1,    11,    52,    53,    -1,    11,    52,    88,    53,
      -1,    11,    -1,    12,    52,    88,    53,    -1,    12,    52,
      53,    -1,    12,    -1,    -1,    -1,    -1,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    69,    69,    70,    71,    75,    76,    82,    83,    87,
      91,    95,    96,   100,   101,   102,   103,   104,   110,   111,
     115,   119,   125,   126,   127,   128,   129,   133,   134,   135,
     139,   140,   141,   142,   143,   149,   150,   151,   152,   159,
     163,   167,   168,   169,   173,   174,   178,   184,   188,   189,
     190,   194,   195,   196,   197,   198,   199,   200,   204,   205,
     206,   207,   208,   209,   210,   214,   215,   221,   227,   228,
     229,   230,   234,   235,   236,   240,   244,   245,   246,   247,
     248,   248,   250,   256,   257,   258,   262,   266,   267,   268,
     269,   270,   271,   272,   273,   274,   275,   276,   277,   278,
     279,   280,   281,   282,   283,   284,   285,   286,   287,   288,
     289,   290,   291,   292,   293,   294,   295,   296,   297,   298,
     304,   310,   311,   312,   316,   317,   318,   324,   328,   332,
     336
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "INTCONST", "ELEMENTCONST",
  "DOUBLECONST", "NEWTOKEN", "CHARCONST", "STEPTOKEN", "VAR", "LOCALVAR",
  "FUNCCALL", "USERFUNCCALL", "VTYPE", "DO", "WHILE", "FOR", "IF",
  "RETURN", "FILTERBLOCK", "HELP", "DIOV", "DUMMY", "OPTION", "ELSE", "OR",
  "AND", "'='", "DEQ", "TEQ", "MEQ", "PEQ", "'>'", "'<'", "NEQ", "EQ",
  "LEQ", "GEQ", "'+'", "'-'", "'*'", "'/'", "'%'", "UMINUS", "MINUSMINUS",
  "PLUSPLUS", "'!'", "'^'", "'{'", "'}'", "';'", "','", "'('", "')'",
  "'['", "']'", "'.'", "$accept", "programlist", "program", "block",
  "pushscope", "popscope", "statementlist", "blockment", "optlist",
  "filter", "pushfilter", "statement", "decexpr", "fstatement", "constant",
  "userfuncdef", "userstatementdef", "args", "arg", "pushfunc",
  "namelistitem", "namelist", "newname", "newvar", "declaration", "widget",
  "step", "steplist", "var", "rawvar", "@1", "exprlist", "expr", "rawexpr",
  "ARRAYCONST", "func", "userfunc", "savetokenname", "savetype",
  "cleartype", "pushstepname", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,    61,   282,   283,
     284,   285,    62,    60,   286,   287,   288,   289,    43,    45,
      42,    47,    37,   290,   291,   292,    33,    94,   123,   125,
      59,    44,    40,    41,    91,    93,    46
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    57,    58,    58,    58,    59,    59,    60,    60,    61,
      62,    63,    63,    64,    64,    64,    64,    64,    65,    65,
      66,    67,    68,    68,    68,    68,    68,    69,    69,    69,
      70,    70,    70,    70,    70,    71,    71,    71,    71,    72,
      73,    74,    74,    74,    75,    75,    76,    77,    78,    78,
      78,    79,    79,    79,    79,    79,    79,    79,    80,    80,
      80,    80,    80,    80,    80,    81,    81,    82,    83,    83,
      83,    83,    84,    84,    84,    85,    86,    86,    86,    86,
      87,    86,    86,    88,    88,    88,    89,    90,    90,    90,
      90,    90,    90,    90,    90,    90,    90,    90,    90,    90,
      90,    90,    90,    90,    90,    90,    90,    90,    90,    90,
      90,    90,    90,    90,    90,    90,    90,    90,    90,    90,
      91,    92,    92,    92,    93,    93,    93,    94,    95,    96,
      97
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     1,     2,     1,     1,     5,     2,     0,
       0,     1,     2,     2,     1,     1,     1,     1,     4,     6,
       6,     0,     1,     2,     2,     1,     2,     1,     1,     1,
       7,     5,    10,     6,     7,     1,     1,     1,     1,     8,
       8,     0,     1,     3,     4,     6,     0,     1,     1,     3,
       2,     4,     3,     3,     3,     6,     6,     1,     1,     1,
       1,     1,     1,     1,     2,     3,     3,     4,     5,     5,
       4,     2,     1,     3,     2,     1,     4,     1,     4,     1,
       0,     4,     2,     1,     3,     2,     1,     1,     1,     1,
       3,     3,     3,     3,     3,     3,     3,     2,     2,     2,
       2,     2,     1,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     2,     1,
       3,     3,     4,     1,     4,     3,     1,     0,     0,     0,
       0
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,    35,    38,    36,   119,    37,    77,    79,   123,   126,
     128,     9,     9,     9,     0,    25,    21,     0,   129,     0,
       0,     0,     0,     0,     9,     0,     0,     3,    14,     5,
      11,     6,     0,    22,    15,    87,    16,    17,    27,    29,
     102,    75,    28,    86,    88,    89,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    24,     0,    23,     0,     0,
      97,   101,   100,   118,     8,     0,     0,     1,     4,    12,
     128,    13,    26,     0,     0,     0,     0,     0,    99,    98,
      82,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   121,     0,
      83,   125,     0,    66,   127,    58,    60,    59,    62,    63,
      61,    48,    65,    47,    57,     0,     0,     0,     0,     0,
       0,     0,     0,   117,     0,    92,     0,    90,    91,    96,
      95,    94,    93,     0,   116,   115,   111,   113,   110,   109,
     114,   112,   103,   104,   105,   106,   108,   107,    76,    78,
       0,   122,    85,   124,    46,    64,   127,     0,    50,     0,
       0,     0,     0,     0,     0,   127,     0,    46,    67,    10,
       0,   130,    72,     0,    84,    41,    49,    54,    52,    53,
       0,     0,     0,     0,    31,     0,     0,     0,    41,     7,
     120,    71,    74,     0,   128,     0,    42,    51,     0,    33,
       0,     0,     0,   127,    20,     0,     0,     0,    73,     0,
       0,     0,     0,    34,     0,    30,    18,     0,     0,    70,
       0,     0,   127,    43,    39,    55,    56,     0,     0,    40,
      69,    68,    44,     0,    19,     0,    32,    45
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    26,    27,    28,    51,   189,    29,    30,   166,    31,
      56,    32,    33,    34,    35,    36,    37,   195,   196,   175,
     111,   112,   113,   114,    38,    39,   172,   173,    40,    41,
     133,    99,    42,    43,   128,    44,    45,   155,    50,    58,
     191
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -177
static const yytype_int16 yypact[] =
{
     475,  -177,  -177,  -177,  -177,  -177,   -42,   -32,   -22,    -9,
    -177,  -177,  -177,  -177,    -6,   743,  -177,    27,  -177,     1,
     743,    19,    19,   743,     5,   743,   259,  -177,  -177,   575,
    -177,  -177,   599,  -177,  -177,  -177,  -177,  -177,  -177,  -177,
     102,   -11,   923,  -177,  -177,  -177,   743,   743,   373,   393,
     546,   575,     4,    37,   743,   923,    38,  -177,    20,   743,
      10,  -177,  -177,    10,  -177,   575,   789,  -177,  -177,  -177,
    -177,  -177,  -177,   280,   743,   743,   743,   743,  -177,  -177,
    -177,  -177,   743,   743,   743,   743,   743,   743,   743,   743,
     743,   743,   743,   743,   743,   743,   148,   417,  -177,   290,
     923,  -177,   311,  -177,    39,  -177,  -177,  -177,  -177,  -177,
    -177,  -177,   109,  -177,   -14,    49,   743,   649,   812,    87,
      43,   342,   525,  -177,   968,  -177,   743,   468,  -177,   468,
     468,   468,   468,    89,   468,   468,   101,   101,   101,   101,
     101,   101,   -23,   -23,    10,    10,    10,    10,  -177,  -177,
     743,  -177,   923,  -177,  -177,  -177,  -177,   979,  -177,   625,
     743,    46,   835,    50,   575,  -177,    12,  -177,  -177,  -177,
     675,  -177,  -177,   205,   923,    86,  -177,  -177,   923,  -177,
     731,   743,   575,   743,    78,    76,    98,    57,    86,  -177,
    -177,   -31,  -177,    89,  -177,    32,  -177,    80,   858,  -177,
     904,   575,    45,  -177,  -177,    35,   424,   743,  -177,   103,
      86,    57,   693,  -177,   743,  -177,  -177,    81,    57,  -177,
     362,   765,  -177,  -177,  -177,   923,  -177,   881,    45,  -177,
    -177,  -177,    83,   575,  -177,   743,  -177,   923
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -177,  -177,    91,  -176,     3,  -177,    58,   -26,  -177,  -177,
    -177,  -177,   -25,  -177,   -30,  -177,  -177,   -64,   -84,   -39,
    -106,  -177,  -177,  -177,  -177,   -10,   -59,  -177,    15,  -177,
    -177,   -45,   -15,  -177,  -157,  -177,  -177,  -156,   -69,  -177,
    -177
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -82
static const yytype_int16 yytable[] =
{
      55,   124,   179,    69,   102,    60,   158,    72,    63,   185,
      66,   204,    46,   159,   121,    52,    53,    92,    93,    94,
     110,   206,    47,   207,    95,   115,   120,    65,     6,     7,
      48,    96,    97,   100,   100,   224,    61,    62,    57,   118,
     160,    80,   229,    49,   100,    81,    54,   217,     1,     2,
       3,   176,     5,    59,    64,   226,   116,    95,   127,   129,
     130,   131,   132,   186,   161,   187,   232,   134,   135,   136,
     137,   138,   139,   140,   141,   142,   143,   144,   145,   146,
     147,   170,   110,   210,   152,   211,   210,   152,   218,   117,
     119,   154,   163,   165,   110,   167,    69,   171,   181,   194,
     183,   162,   201,   202,   203,    24,   152,   212,   228,   222,
     235,   100,     1,     2,     3,   156,     5,    68,   105,   106,
     107,   108,   109,   122,   205,   209,   223,   110,   188,    73,
      74,    75,    76,    77,   208,   174,     0,     0,   184,    90,
      91,    92,    93,    94,   178,   180,    78,    79,    95,   177,
       0,     0,     0,     0,     0,   152,   199,     0,     0,     0,
     157,   220,     0,     0,     0,     0,   198,     0,   200,     0,
       0,     0,   216,    82,    83,   215,     0,     0,     0,     0,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,   100,   221,     0,     0,    95,     0,   225,   234,   227,
       0,     0,     0,   148,     0,   152,   192,   236,   -81,   -81,
     -81,   -81,   -81,     0,   -81,   -81,   -81,   -81,   -81,     0,
     237,     0,     0,     0,     0,     0,     0,     0,   -81,     0,
     -81,   -81,   -81,   -81,   -81,   -81,   -81,   -81,   -81,   -81,
     -81,   -81,   -81,   -81,   -81,   -81,   -81,   -81,     0,   -81,
     -81,   -81,   -81,     0,   -81,   -81,   -81,   -81,   -81,    67,
     -81,   193,     1,     2,     3,     4,     5,     0,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,   125,    19,     1,     2,     3,     4,     5,     0,     6,
       7,     8,     9,     1,     2,     3,     4,     5,    20,     6,
       7,     8,     9,    21,    22,    23,     0,    24,     0,     0,
       0,    25,     0,     0,     1,     2,     3,     4,     5,    20,
       6,     7,     8,     9,    21,    22,    23,     0,   126,    20,
       0,     0,    25,     0,    21,    22,    23,     0,     0,     0,
       0,   150,    25,   151,     0,     1,     2,     3,     4,     5,
      20,     6,     7,     8,     9,    21,    22,    23,     0,     0,
       0,     0,   150,    25,   153,     1,     2,     3,     4,     5,
       0,     6,     7,     8,     9,     0,     1,     2,     3,     4,
       5,    20,     6,     7,     8,     9,    21,    22,    23,     0,
       0,     0,     0,   150,    25,   168,     1,     2,     3,     4,
       5,    20,     6,     7,     8,     9,    21,    22,    23,     0,
       0,     0,    20,   150,    25,   230,     0,    21,    22,    23,
       0,     0,     0,     0,     0,    25,    98,     1,     2,     3,
       4,     5,    20,     6,     7,     8,     9,    21,    22,    23,
       0,     0,    82,    83,     0,    25,   101,     0,     0,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
       0,     0,     0,    20,    95,     0,     0,     0,    21,    22,
      23,     0,   149,     0,     0,     0,    25,   219,     1,     2,
       3,     4,     5,     0,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,     0,     0,     0,    20,    95,     0,     0,     0,    21,
      22,    23,     0,    24,     0,     0,     0,    25,     1,     2,
       3,     4,     5,     0,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,     0,    17,    18,   103,    19,     1,
       2,     3,   104,     5,     0,   105,   106,   107,   108,   109,
       0,     0,     0,     0,    20,     0,     0,     0,     0,    21,
      22,    23,     0,    24,   169,     0,     0,    25,     1,     2,
       3,     4,     5,     0,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,     0,    17,    18,     0,    19,     0,
       0,     0,     1,     2,     3,     4,     5,     0,     6,     7,
       8,     9,    70,     0,    20,     0,     0,     0,     0,    21,
      22,    23,    19,    24,     0,     0,     0,    25,     1,     2,
       3,     4,     5,     0,     6,     7,     8,     9,    20,     0,
       0,     0,     0,    21,    22,    23,     0,     0,    19,    71,
       0,    25,     1,     2,     3,     4,     5,     0,     6,     7,
       8,     9,    70,     0,    20,     0,     0,     0,     0,    21,
      22,    23,    19,   126,     0,     0,     0,    25,     1,     2,
       3,     4,     5,     0,     6,     7,     8,     9,    20,     0,
       0,     0,     0,    21,    22,    23,     1,     2,     3,     4,
       5,    25,     6,     7,     8,     9,     0,     0,     0,     0,
       0,     0,     0,     0,    20,     0,     0,     0,     0,    21,
      22,    23,     0,     0,   190,     0,   150,    25,     0,     0,
       0,     0,    20,     0,     0,     0,     0,    21,    22,    23,
       0,   126,     0,     0,     0,    25,     1,     2,     3,     4,
       5,     0,     6,     7,     8,     9,    82,    83,     0,     0,
       0,     0,     0,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,     0,     0,     0,     0,    95,     0,
       0,     0,    20,     0,     0,     0,   197,    21,    22,    23,
      82,    83,     0,     0,     0,    25,     0,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,     0,     0,
       0,     0,    95,     0,    82,    83,     0,     0,     0,     0,
     231,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,     0,     0,     0,     0,    95,    82,    83,     0,
       0,     0,   123,     0,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,     0,     0,     0,     0,    95,
      82,    83,     0,     0,     0,   164,     0,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,     0,     0,
       0,     0,    95,    82,    83,     0,     0,     0,   182,     0,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,     0,     0,     0,     0,    95,    82,    83,     0,     0,
       0,   213,     0,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,     0,     0,     0,     0,    95,    82,
      83,     0,     0,     0,   233,     0,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,     0,    82,    83,
       0,    95,     0,     0,   214,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,     0,     0,     0,   103,
      95,     1,     2,     3,   156,     5,     0,   105,   106,   107,
     108,   109,     1,     2,     3,   156,     5,     0,   105,   106,
     107,   108,   109
};

static const yytype_int16 yycheck[] =
{
      15,    70,   159,    29,    49,    20,   112,    32,    23,   165,
      25,   187,    54,    27,    59,    12,    13,    40,    41,    42,
      50,    52,    54,    54,    47,    51,     6,    24,     9,    10,
      52,    46,    47,    48,    49,   211,    21,    22,    11,    54,
      54,    52,   218,    52,    59,    56,    52,   203,     3,     4,
       5,   157,     7,    52,    49,   212,    52,    47,    73,    74,
      75,    76,    77,    51,    15,    53,   222,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,   126,   112,    51,    99,    53,    51,   102,    53,    52,
      52,    52,   117,     6,   124,    52,   122,     8,    52,    13,
      50,   116,    24,    27,     6,    48,   121,    27,    27,     6,
      27,   126,     3,     4,     5,     6,     7,    26,     9,    10,
      11,    12,    13,    65,   188,   194,   210,   157,   167,    27,
      28,    29,    30,    31,   193,   150,    -1,    -1,   164,    38,
      39,    40,    41,    42,   159,   160,    44,    45,    47,   159,
      -1,    -1,    -1,    -1,    -1,   170,   182,    -1,    -1,    -1,
      51,   206,    -1,    -1,    -1,    -1,   181,    -1,   183,    -1,
      -1,    -1,   202,    25,    26,   201,    -1,    -1,    -1,    -1,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,   206,   207,    -1,    -1,    47,    -1,   212,   228,   214,
      -1,    -1,    -1,    55,    -1,   220,     1,   233,     3,     4,
       5,     6,     7,    -1,     9,    10,    11,    12,    13,    -1,
     235,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    23,    -1,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    -1,    44,
      45,    46,    47,    -1,    49,    50,    51,    52,    53,     0,
      55,    56,     3,     4,     5,     6,     7,    -1,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,     1,    23,     3,     4,     5,     6,     7,    -1,     9,
      10,    11,    12,     3,     4,     5,     6,     7,    39,     9,
      10,    11,    12,    44,    45,    46,    -1,    48,    -1,    -1,
      -1,    52,    -1,    -1,     3,     4,     5,     6,     7,    39,
       9,    10,    11,    12,    44,    45,    46,    -1,    48,    39,
      -1,    -1,    52,    -1,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    52,    53,    -1,     3,     4,     5,     6,     7,
      39,     9,    10,    11,    12,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    52,    53,     3,     4,     5,     6,     7,
      -1,     9,    10,    11,    12,    -1,     3,     4,     5,     6,
       7,    39,     9,    10,    11,    12,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    52,    53,     3,     4,     5,     6,
       7,    39,     9,    10,    11,    12,    44,    45,    46,    -1,
      -1,    -1,    39,    51,    52,    53,    -1,    44,    45,    46,
      -1,    -1,    -1,    -1,    -1,    52,    53,     3,     4,     5,
       6,     7,    39,     9,    10,    11,    12,    44,    45,    46,
      -1,    -1,    25,    26,    -1,    52,    53,    -1,    -1,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      -1,    -1,    -1,    39,    47,    -1,    -1,    -1,    44,    45,
      46,    -1,    55,    -1,    -1,    -1,    52,    53,     3,     4,
       5,     6,     7,    -1,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    -1,    23,    -1,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    -1,    -1,    -1,    39,    47,    -1,    -1,    -1,    44,
      45,    46,    -1,    48,    -1,    -1,    -1,    52,     3,     4,
       5,     6,     7,    -1,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    21,     1,    23,     3,
       4,     5,     6,     7,    -1,     9,    10,    11,    12,    13,
      -1,    -1,    -1,    -1,    39,    -1,    -1,    -1,    -1,    44,
      45,    46,    -1,    48,    49,    -1,    -1,    52,     3,     4,
       5,     6,     7,    -1,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    21,    -1,    23,    -1,
      -1,    -1,     3,     4,     5,     6,     7,    -1,     9,    10,
      11,    12,    13,    -1,    39,    -1,    -1,    -1,    -1,    44,
      45,    46,    23,    48,    -1,    -1,    -1,    52,     3,     4,
       5,     6,     7,    -1,     9,    10,    11,    12,    39,    -1,
      -1,    -1,    -1,    44,    45,    46,    -1,    -1,    23,    50,
      -1,    52,     3,     4,     5,     6,     7,    -1,     9,    10,
      11,    12,    13,    -1,    39,    -1,    -1,    -1,    -1,    44,
      45,    46,    23,    48,    -1,    -1,    -1,    52,     3,     4,
       5,     6,     7,    -1,     9,    10,    11,    12,    39,    -1,
      -1,    -1,    -1,    44,    45,    46,     3,     4,     5,     6,
       7,    52,     9,    10,    11,    12,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    39,    -1,    -1,    -1,    -1,    44,
      45,    46,    -1,    -1,    49,    -1,    51,    52,    -1,    -1,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    44,    45,    46,
      -1,    48,    -1,    -1,    -1,    52,     3,     4,     5,     6,
       7,    -1,     9,    10,    11,    12,    25,    26,    -1,    -1,
      -1,    -1,    -1,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    -1,    -1,    -1,    -1,    47,    -1,
      -1,    -1,    39,    -1,    -1,    -1,    55,    44,    45,    46,
      25,    26,    -1,    -1,    -1,    52,    -1,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    -1,    -1,
      -1,    -1,    47,    -1,    25,    26,    -1,    -1,    -1,    -1,
      55,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    -1,    -1,    -1,    -1,    47,    25,    26,    -1,
      -1,    -1,    53,    -1,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    -1,    -1,    -1,    -1,    47,
      25,    26,    -1,    -1,    -1,    53,    -1,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    -1,    -1,
      -1,    -1,    47,    25,    26,    -1,    -1,    -1,    53,    -1,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    -1,    -1,    -1,    -1,    47,    25,    26,    -1,    -1,
      -1,    53,    -1,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    -1,    -1,    -1,    -1,    47,    25,
      26,    -1,    -1,    -1,    53,    -1,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    -1,    25,    26,
      -1,    47,    -1,    -1,    50,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    -1,    -1,    -1,     1,
      47,     3,     4,     5,     6,     7,    -1,     9,    10,    11,
      12,    13,     3,     4,     5,     6,     7,    -1,     9,    10,
      11,    12,    13
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     5,     6,     7,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    23,
      39,    44,    45,    46,    48,    52,    58,    59,    60,    63,
      64,    66,    68,    69,    70,    71,    72,    73,    81,    82,
      85,    86,    89,    90,    92,    93,    54,    54,    52,    52,
      95,    61,    61,    61,    52,    89,    67,    11,    96,    52,
      89,    85,    85,    89,    49,    61,    89,     0,    59,    64,
      13,    50,    69,    27,    28,    29,    30,    31,    44,    45,
      52,    56,    25,    26,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    47,    89,    89,    53,    88,
      89,    53,    88,     1,     6,     9,    10,    11,    12,    13,
      71,    77,    78,    79,    80,    64,    52,    52,    89,    52,
       6,    88,    63,    53,    95,     1,    48,    89,    91,    89,
      89,    89,    89,    87,    89,    89,    89,    89,    89,    89,
      89,    89,    89,    89,    89,    89,    89,    89,    55,    55,
      51,    53,    89,    53,    52,    94,     6,    51,    77,    27,
      54,    15,    89,    69,    53,     6,    65,    52,    53,    49,
      88,     8,    83,    84,    89,    76,    77,    82,    89,    91,
      89,    52,    53,    50,    64,    94,    51,    53,    76,    62,
      49,    97,     1,    56,    13,    74,    75,    55,    89,    64,
      89,    24,    27,     6,    60,    74,    52,    54,    83,    95,
      51,    53,    27,    53,    50,    64,    71,    94,    53,    53,
      88,    89,     6,    75,    60,    89,    91,    89,    27,    60,
      53,    55,    94,    53,    71,    27,    64,    89
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
#line 69 "grammar.yy"
    { }
    break;

  case 3:
#line 70 "grammar.yy"
    { }
    break;

  case 4:
#line 71 "grammar.yy"
    { }
    break;

  case 5:
#line 75 "grammar.yy"
    { if (((yyvsp[(1) - (1)].node) != NULL) && (!cmdparser.addStatement((yyvsp[(1) - (1)].node)))) YYABORT; }
    break;

  case 6:
#line 76 "grammar.yy"
    { }
    break;

  case 7:
#line 82 "grammar.yy"
    { (yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (5)].node),(yyvsp[(3) - (5)].node)); }
    break;

  case 8:
#line 83 "grammar.yy"
    { (yyval.node) = cmdparser.addFunction(Command::NoFunction); }
    break;

  case 9:
#line 87 "grammar.yy"
    { (yyval.node) = cmdparser.pushScope(); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 10:
#line 91 "grammar.yy"
    { if (!cmdparser.popScope()) YYABORT; }
    break;

  case 11:
#line 95 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 12:
#line 96 "grammar.yy"
    { if (((yyvsp[(1) - (2)].node) != NULL) && ((yyvsp[(2) - (2)].node) != NULL)) (yyval.node) = cmdparser.joinCommands((yyvsp[(1) - (2)].node), (yyvsp[(2) - (2)].node)); }
    break;

  case 13:
#line 100 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (2)].node); }
    break;

  case 14:
#line 101 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 15:
#line 102 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 16:
#line 103 "grammar.yy"
    { (yyval.node) = NULL; }
    break;

  case 17:
#line 104 "grammar.yy"
    { (yyval.node) = NULL; }
    break;

  case 18:
#line 110 "grammar.yy"
    { if (!cmdparser.setFilterOption(&tokenName, (yyvsp[(4) - (4)].node))) YYABORT; }
    break;

  case 19:
#line 111 "grammar.yy"
    { if (!cmdparser.setFilterOption(&tokenName, (yyvsp[(6) - (6)].node))) YYABORT; }
    break;

  case 20:
#line 115 "grammar.yy"
    { if (!cmdparser.addStatement((yyvsp[(6) - (6)].node))) YYABORT; cmdparser.popTree(); }
    break;

  case 21:
#line 119 "grammar.yy"
    { cmdparser.pushTree(TRUE); }
    break;

  case 22:
#line 125 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 23:
#line 126 "grammar.yy"
    { (yyval.node) = cmdparser.addFunction(Command::Help, cmdparser.addConstant((yyvsp[(2) - (2)].functionId))); }
    break;

  case 24:
#line 127 "grammar.yy"
    { (yyval.node) = cmdparser.addFunction(Command::Return,(yyvsp[(2) - (2)].node)); }
    break;

  case 25:
#line 128 "grammar.yy"
    { (yyval.node) = cmdparser.addFunction(Command::Return); }
    break;

  case 26:
#line 129 "grammar.yy"
    { msg.print("Error: Expected ';' before current expression.\n"); YYABORT; }
    break;

  case 27:
#line 133 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 28:
#line 134 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 29:
#line 135 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 30:
#line 139 "grammar.yy"
    { (yyval.node) = cmdparser.addFunction(Command::If,(yyvsp[(3) - (7)].node),(yyvsp[(5) - (7)].node),(yyvsp[(7) - (7)].node)); }
    break;

  case 31:
#line 140 "grammar.yy"
    { (yyval.node) = cmdparser.addFunction(Command::If,(yyvsp[(3) - (5)].node),(yyvsp[(5) - (5)].node)); }
    break;

  case 32:
#line 141 "grammar.yy"
    { (yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (10)].node), cmdparser.addFunction(Command::For, (yyvsp[(4) - (10)].node),(yyvsp[(6) - (10)].node),(yyvsp[(8) - (10)].node),(yyvsp[(10) - (10)].node))); cmdparser.popScope(); }
    break;

  case 33:
#line 142 "grammar.yy"
    { (yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (6)].node), cmdparser.addFunction(Command::While, (yyvsp[(4) - (6)].node),(yyvsp[(6) - (6)].node))); cmdparser.popScope(); }
    break;

  case 34:
#line 143 "grammar.yy"
    { (yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (7)].node), cmdparser.addFunction(Command::DoWhile, (yyvsp[(3) - (7)].node),(yyvsp[(6) - (7)].node))); cmdparser.popScope(); }
    break;

  case 35:
#line 149 "grammar.yy"
    { (yyval.node) = cmdparser.addConstant((yyvsp[(1) - (1)].intconst)); }
    break;

  case 36:
#line 150 "grammar.yy"
    { (yyval.node) = cmdparser.addConstant((yyvsp[(1) - (1)].doubleconst)); }
    break;

  case 37:
#line 151 "grammar.yy"
    { (yyval.node) = cmdparser.addConstant((yyvsp[(1) - (1)].name)->get()); }
    break;

  case 38:
#line 152 "grammar.yy"
    { (yyval.node) = cmdparser.addElementConstant((yyvsp[(1) - (1)].intconst)); }
    break;

  case 39:
#line 159 "grammar.yy"
    { if (!cmdparser.addStatement((yyvsp[(8) - (8)].node))) YYABORT; cmdparser.popTree(); declaredType = VTypes::NoData; }
    break;

  case 40:
#line 163 "grammar.yy"
    { if (!cmdparser.addStatement((yyvsp[(8) - (8)].node))) YYABORT; cmdparser.popTree(); declaredType = VTypes::NoData; }
    break;

  case 41:
#line 167 "grammar.yy"
    { }
    break;

  case 42:
#line 168 "grammar.yy"
    { }
    break;

  case 43:
#line 169 "grammar.yy"
    { }
    break;

  case 44:
#line 173 "grammar.yy"
    { (yyval.node) = cmdparser.addVariableAsArgument(declaredType, &tokenName); }
    break;

  case 45:
#line 174 "grammar.yy"
    { (yyval.node) = cmdparser.addVariableAsArgument(declaredType, &tokenName, (yyvsp[(6) - (6)].node)); }
    break;

  case 46:
#line 178 "grammar.yy"
    { cmdparser.pushFunction(yylval.name->get(), declaredType); }
    break;

  case 47:
#line 184 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyvsp[(1) - (1)].node) == NULL) YYABORT; }
    break;

  case 48:
#line 188 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 49:
#line 189 "grammar.yy"
    { if ((yyvsp[(3) - (3)].node) == NULL) YYABORT; (yyval.node) = Tree::joinArguments((yyvsp[(3) - (3)].node),(yyvsp[(1) - (3)].node)); }
    break;

  case 50:
#line 190 "grammar.yy"
    { msg.print("Error: Missing comma between declarations?\n"); YYABORT; }
    break;

  case 51:
#line 194 "grammar.yy"
    { (yyval.node) = cmdparser.addArrayVariable(declaredType, &tokenName, (yyvsp[(3) - (4)].node)); }
    break;

  case 52:
#line 195 "grammar.yy"
    { (yyval.node) = cmdparser.addVariable(declaredType, &tokenName, (yyvsp[(3) - (3)].node)); }
    break;

  case 53:
#line 196 "grammar.yy"
    { (yyval.node) = cmdparser.addVariable(declaredType, &tokenName, (yyvsp[(3) - (3)].node)); }
    break;

  case 54:
#line 197 "grammar.yy"
    { (yyval.node) = cmdparser.addVariable(declaredType, &tokenName, (yyvsp[(3) - (3)].node)); }
    break;

  case 55:
#line 198 "grammar.yy"
    { (yyval.node) = cmdparser.addArrayVariable(declaredType, &tokenName,(yyvsp[(3) - (6)].node),(yyvsp[(6) - (6)].node)); }
    break;

  case 56:
#line 199 "grammar.yy"
    { (yyval.node) = cmdparser.addArrayVariable(declaredType, &tokenName,(yyvsp[(3) - (6)].node),(yyvsp[(6) - (6)].node)); }
    break;

  case 57:
#line 200 "grammar.yy"
    { (yyval.node) = cmdparser.addVariable(declaredType, (yyvsp[(1) - (1)].name)); }
    break;

  case 58:
#line 204 "grammar.yy"
    { tokenName = yylval.variable->name(); (yyval.name) = &tokenName; }
    break;

  case 59:
#line 205 "grammar.yy"
    { tokenName = Command::data[yylval.functionId].keyword; (yyval.name) = &tokenName; }
    break;

  case 60:
#line 206 "grammar.yy"
    { msg.print("Error: Existing variable in local scope cannot be redeclared.\n"); YYABORT; }
    break;

  case 61:
#line 207 "grammar.yy"
    { msg.print("Error: Constant value found in declaration.\n"); YYABORT; }
    break;

  case 62:
#line 208 "grammar.yy"
    { msg.print("Error: Existing user-defined function name cannot be redeclared.\n"); YYABORT; }
    break;

  case 63:
#line 209 "grammar.yy"
    { msg.print("Error: Type-name used in variable declaration.\n"); YYABORT; }
    break;

  case 64:
#line 210 "grammar.yy"
    { if (declaredType == VTypes::NoData) { msg.print("Token '%s' is undeclared.\n", tokenName.get()); YYABORT; } (yyval.name) = (yyvsp[(1) - (2)].name); }
    break;

  case 65:
#line 214 "grammar.yy"
    { (yyval.node) = cmdparser.addDeclarations((yyvsp[(3) - (3)].node)); declaredType = VTypes::NoData; }
    break;

  case 66:
#line 215 "grammar.yy"
    { msg.print("Illegal use of reserved word '%s'.\n", VTypes::dataType(declaredType)); YYABORT; }
    break;

  case 67:
#line 221 "grammar.yy"
    { (yyval.node) = cmdparser.addWidget((yyvsp[(3) - (4)].node)); }
    break;

  case 68:
#line 227 "grammar.yy"
    { if (!cmdparser.expandPath(stepNameStack.last(), (yyvsp[(4) - (5)].node))) YYABORT; stepNameStack.removeLast(); }
    break;

  case 69:
#line 228 "grammar.yy"
    { if (!cmdparser.expandPath(stepNameStack.last(), NULL, (yyvsp[(4) - (5)].node))) YYABORT; stepNameStack.removeLast(); }
    break;

  case 70:
#line 229 "grammar.yy"
    { if (!cmdparser.expandPath(stepNameStack.last(), NULL, NULL)) YYABORT; stepNameStack.removeLast(); }
    break;

  case 71:
#line 230 "grammar.yy"
    { if (!cmdparser.expandPath((yyvsp[(1) - (2)].name))) YYABORT; stepNameStack.removeLast(); }
    break;

  case 72:
#line 234 "grammar.yy"
    { }
    break;

  case 73:
#line 235 "grammar.yy"
    { }
    break;

  case 74:
#line 236 "grammar.yy"
    { msg.print("Error formulating path.\n"); YYABORT; }
    break;

  case 75:
#line 240 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 76:
#line 244 "grammar.yy"
    { (yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (4)].variable),(yyvsp[(3) - (4)].node)); }
    break;

  case 77:
#line 245 "grammar.yy"
    { (yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (1)].variable)); }
    break;

  case 78:
#line 246 "grammar.yy"
    { (yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (4)].variable),(yyvsp[(3) - (4)].node)); }
    break;

  case 79:
#line 247 "grammar.yy"
    { (yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (1)].variable)); }
    break;

  case 80:
#line 248 "grammar.yy"
    { cmdparser.createPath((yyvsp[(1) - (2)].node)); }
    break;

  case 81:
#line 249 "grammar.yy"
    { (yyval.node) = cmdparser.finalisePath(); }
    break;

  case 82:
#line 250 "grammar.yy"
    { msg.print("Can't use a variable as a function. Did you mean '[' instead?\n"); (yyval.node) = NULL; }
    break;

  case 83:
#line 256 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 84:
#line 257 "grammar.yy"
    { (yyval.node) = Tree::joinArguments((yyvsp[(3) - (3)].node),(yyvsp[(1) - (3)].node)); }
    break;

  case 85:
#line 258 "grammar.yy"
    { msg.print("Error: Missing comma between items.\n"); YYABORT; }
    break;

  case 86:
#line 262 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 87:
#line 266 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 88:
#line 267 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 89:
#line 268 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 90:
#line 269 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignment,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); }
    break;

  case 91:
#line 270 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignment,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); }
    break;

  case 92:
#line 271 "grammar.yy"
    { msg.print("Mangled expression used in assignment.\n"); YYABORT; }
    break;

  case 93:
#line 272 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignmentPlus,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); }
    break;

  case 94:
#line 273 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignmentSubtract,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); }
    break;

  case 95:
#line 274 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignmentMultiply,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); }
    break;

  case 96:
#line 275 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignmentDivide,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); }
    break;

  case 97:
#line 276 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorNegate, (yyvsp[(2) - (2)].node)); }
    break;

  case 98:
#line 277 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPostfixIncrease, (yyvsp[(1) - (2)].node));  }
    break;

  case 99:
#line 278 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPostfixDecrease, (yyvsp[(1) - (2)].node)); }
    break;

  case 100:
#line 279 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPrefixIncrease, (yyvsp[(2) - (2)].node)); }
    break;

  case 101:
#line 280 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPrefixDecrease, (yyvsp[(2) - (2)].node)); }
    break;

  case 102:
#line 281 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 103:
#line 282 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAdd, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 104:
#line 283 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorSubtract, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 105:
#line 284 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorMultiply, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 106:
#line 285 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorDivide, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 107:
#line 286 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPower, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 108:
#line 287 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorModulus, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 109:
#line 288 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 110:
#line 289 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorNotEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 111:
#line 290 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorGreaterThan, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 112:
#line 291 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorGreaterThanEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 113:
#line 292 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorLessThan, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 114:
#line 293 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorLessThanEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 115:
#line 294 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAnd, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 116:
#line 295 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorOr, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 117:
#line 296 "grammar.yy"
    { (yyval.node) = (yyvsp[(2) - (3)].node); }
    break;

  case 118:
#line 297 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorNot, (yyvsp[(2) - (2)].node)); }
    break;

  case 119:
#line 298 "grammar.yy"
    { msg.print("Error: '%s' has not been declared as a function or a variable.\n", yylval.name->get()); YYABORT; }
    break;

  case 120:
#line 304 "grammar.yy"
    { (yyval.node) = cmdparser.addArrayConstant((yyvsp[(2) - (3)].node)); }
    break;

  case 121:
#line 310 "grammar.yy"
    { (yyval.node) = cmdparser.addFunction( (Command::Function) (yyvsp[(1) - (3)].functionId)); }
    break;

  case 122:
#line 311 "grammar.yy"
    { (yyval.node) = cmdparser.addFunctionWithArglist( (Command::Function) (yyvsp[(1) - (4)].functionId),(yyvsp[(3) - (4)].node)); }
    break;

  case 123:
#line 312 "grammar.yy"
    { (yyval.node) = cmdparser.addFunction( (Command::Function) (yyvsp[(1) - (1)].functionId)); }
    break;

  case 124:
#line 316 "grammar.yy"
    { (yyval.node) = cmdparser.addUserFunction((yyvsp[(1) - (4)].functree),(yyvsp[(3) - (4)].node)); }
    break;

  case 125:
#line 317 "grammar.yy"
    { (yyval.node) = cmdparser.addUserFunction((yyvsp[(1) - (3)].functree)); }
    break;

  case 126:
#line 318 "grammar.yy"
    { (yyval.node) = cmdparser.addUserFunction((yyvsp[(1) - (1)].functree)); }
    break;

  case 127:
#line 324 "grammar.yy"
    { tokenName = *yylval.name; }
    break;

  case 128:
#line 328 "grammar.yy"
    { declaredType = yylval.vtype; }
    break;

  case 129:
#line 332 "grammar.yy"
    { declaredType = VTypes::NoData; }
    break;

  case 130:
#line 336 "grammar.yy"
    { stepNameStack.add()->set(yylval.name->get()); }
    break;


/* Line 1267 of yacc.c.  */
#line 2440 "grammar.cc"
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


#line 339 "grammar.yy"


void yyerror(char *s)
{
}

