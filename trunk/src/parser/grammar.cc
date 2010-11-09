
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C
   
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
#define YYBISON_VERSION "2.4.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0

/* Substitute the variable and function names.  */
#define yyparse         CommandParser_parse
#define yylex           CommandParser_lex
#define yyerror         CommandParser_error
#define yylval          CommandParser_lval
#define yychar          CommandParser_char
#define yydebug         CommandParser_debug
#define yynerrs         CommandParser_nerrs


/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
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



/* Line 189 of yacc.c  */
#line 104 "grammar.cc"

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
     ATEN_DO = 270,
     WHILE = 271,
     FOR = 272,
     ATEN_IF = 273,
     ATEN_IN = 274,
     ATEN_RETURN = 275,
     FILTERBLOCK = 276,
     HELP = 277,
     ATEN_VOID = 278,
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
#define ATEN_DO 270
#define WHILE 271
#define FOR 272
#define ATEN_IF 273
#define ATEN_IN 274
#define ATEN_RETURN 275
#define FILTERBLOCK 276
#define HELP 277
#define ATEN_VOID 278
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

/* Line 214 of yacc.c  */
#line 30 "grammar.yy"

	int functionId;			/* Function enum id */
	Dnchar *name;			/* character pointer for names */
	TreeNode *node;			/* node pointer */
	Variable *variable;		/* variable pointer */
	Tree *functree;			/* user-defined function (tree) pointer */
	VTypes::DataType vtype;		/* variable type for next declarations */
	int intconst;			/* integer constant value */
	double doubleconst;		/* double constant value */



/* Line 214 of yacc.c  */
#line 231 "grammar.cc"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 243 "grammar.cc"

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
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
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
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
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
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  67
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1111

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  59
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  42
/* YYNRULES -- Number of rules.  */
#define YYNRULES  135
/* YYNRULES -- Number of states.  */
#define YYNSTATES  251

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
     147,   149,   151,   153,   160,   167,   169,   173,   177,   181,
     182,   184,   188,   194,   198,   200,   204,   207,   212,   216,
     220,   227,   234,   238,   240,   242,   244,   246,   248,   250,
     252,   255,   256,   261,   267,   273,   278,   281,   283,   287,
     290,   292,   297,   299,   304,   306,   307,   312,   315,   317,
     321,   324,   326,   328,   330,   332,   336,   340,   344,   348,
     352,   356,   360,   363,   366,   369,   372,   375,   377,   381,
     385,   389,   393,   397,   401,   405,   409,   413,   417,   421,
     425,   429,   433,   437,   440,   442,   446,   450,   455,   457,
     462,   466,   468,   469,   470,   471
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
      14,    98,     7,    84,    78,    62,    -1,     7,    -1,    14,
      98,    81,    -1,    14,    98,     1,    -1,    54,    79,    55,
      -1,    -1,    80,    -1,    79,    53,    80,    -1,    14,    98,
      83,    29,    92,    -1,    14,    98,    83,    -1,    82,    -1,
      81,    53,    82,    -1,    81,    82,    -1,    83,    56,    92,
      57,    -1,    83,    29,    94,    -1,    83,    29,    85,    -1,
      83,    56,    92,    57,    29,    92,    -1,    83,    56,    92,
      57,    29,    94,    -1,    83,    29,    92,    -1,    83,    -1,
      10,    -1,    12,    -1,    11,    -1,    75,    -1,    13,    -1,
      14,    -1,     6,    97,    -1,    -1,    25,    54,    91,    55,
      -1,     9,   100,    56,    92,    57,    -1,     9,   100,    54,
      91,    55,    -1,     9,   100,    54,    55,    -1,     9,   100,
      -1,    86,    -1,    87,    58,    86,    -1,    87,     1,    -1,
      89,    -1,    10,    56,    92,    57,    -1,    10,    -1,    11,
      56,    92,    57,    -1,    11,    -1,    -1,    89,    58,    90,
      87,    -1,    89,    54,    -1,    92,    -1,    91,    53,    92,
      -1,    91,    92,    -1,    93,    -1,    75,    -1,    95,    -1,
      96,    -1,    88,    29,    92,    -1,    88,    29,    94,    -1,
      88,    29,     1,    -1,    88,    33,    92,    -1,    88,    32,
      92,    -1,    88,    31,    92,    -1,    88,    30,    92,    -1,
      41,    92,    -1,    88,    47,    -1,    88,    46,    -1,    47,
      88,    -1,    46,    88,    -1,    88,    -1,    92,    40,    92,
      -1,    92,    41,    92,    -1,    92,    42,    92,    -1,    92,
      43,    92,    -1,    92,    49,    92,    -1,    92,    44,    92,
      -1,    92,    37,    92,    -1,    92,    36,    92,    -1,    92,
      34,    92,    -1,    92,    39,    92,    -1,    92,    35,    92,
      -1,    92,    38,    92,    -1,    92,    28,    92,    -1,    92,
      27,    92,    -1,    54,    92,    55,    -1,    48,    92,    -1,
       6,    -1,    50,    91,    51,    -1,    12,    54,    55,    -1,
      12,    54,    91,    55,    -1,    12,    -1,    13,    54,    91,
      55,    -1,    13,    54,    55,    -1,    13,    -1,    -1,    -1,
      -1,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    70,    70,    71,    72,    76,    77,    83,    84,    88,
      92,    96,    97,    98,    99,   103,   104,   105,   111,   112,
     116,   120,   126,   127,   128,   129,   130,   131,   136,   137,
     141,   143,   145,   147,   147,   153,   153,   160,   162,   169,
     170,   171,   172,   178,   181,   184,   189,   192,   197,   203,
     206,   209,   215,   218,   224,   225,   226,   230,   233,   236,
     239,   242,   245,   248,   254,   257,   260,   263,   266,   269,
     272,   278,   287,   293,   294,   295,   296,   300,   301,   302,
     306,   310,   311,   312,   313,   314,   314,   316,   322,   323,
     324,   328,   332,   333,   334,   335,   336,   337,   338,   339,
     340,   341,   342,   343,   344,   345,   346,   347,   348,   349,
     350,   351,   352,   353,   354,   355,   356,   357,   358,   359,
     360,   361,   362,   363,   364,   370,   376,   377,   378,   382,
     383,   384,   390,   394,   398,   402
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "INTCONST", "ELEMENTCONST",
  "DOUBLECONST", "NEWTOKEN", "NEWFUNCTOKEN", "CHARCONST", "STEPTOKEN",
  "VAR", "LOCALVAR", "FUNCCALL", "USERFUNCCALL", "VTYPE", "ATEN_DO",
  "WHILE", "FOR", "ATEN_IF", "ATEN_IN", "ATEN_RETURN", "FILTERBLOCK",
  "HELP", "ATEN_VOID", "DUMMY", "OPTION", "ELSE", "OR", "AND", "'='",
  "DEQ", "TEQ", "MEQ", "PEQ", "'>'", "'<'", "NEQ", "EQ", "LEQ", "GEQ",
  "'+'", "'-'", "'*'", "'/'", "'%'", "UMINUS", "MINUSMINUS", "PLUSPLUS",
  "'!'", "'^'", "'{'", "'}'", "';'", "','", "'('", "')'", "'['", "']'",
  "'.'", "$accept", "programlist", "program", "block", "pushscope",
  "popscope", "statementlist", "blockment", "optlist", "filter",
  "pushfilter", "statement", "decexpr", "flowstatement", "$@1", "$@2",
  "constant", "funcdeclaration", "vardeclaration", "arglist", "args",
  "argdeclaration", "declist", "vardec", "newname", "pushfunc", "widget",
  "step", "steplist", "var", "rawvar", "$@3", "exprlist", "expr",
  "rawexpr", "ARRAYCONST", "func", "userfunc", "savetokenname", "savetype",
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
      75,    75,    75,    76,    76,    76,    77,    77,    78,    79,
      79,    79,    80,    80,    81,    81,    81,    82,    82,    82,
      82,    82,    82,    82,    83,    83,    83,    83,    83,    83,
      83,    84,    85,    86,    86,    86,    86,    87,    87,    87,
      88,    89,    89,    89,    89,    90,    89,    89,    91,    91,
      91,    92,    93,    93,    93,    93,    93,    93,    93,    93,
      93,    93,    93,    93,    93,    93,    93,    93,    93,    93,
      93,    93,    93,    93,    93,    93,    93,    93,    93,    93,
      93,    93,    93,    93,    93,    94,    95,    95,    95,    96,
      96,    96,    97,    98,    99,   100
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     1,     2,     1,     1,     5,     2,     0,
       0,     1,     1,     2,     2,     2,     1,     1,     4,     6,
       6,     0,     1,     1,     2,     2,     1,     2,     1,     1,
       7,     5,    10,     0,     9,     0,    11,     6,     8,     1,
       1,     1,     1,     6,     6,     1,     3,     3,     3,     0,
       1,     3,     5,     3,     1,     3,     2,     4,     3,     3,
       6,     6,     3,     1,     1,     1,     1,     1,     1,     1,
       2,     0,     4,     5,     5,     4,     2,     1,     3,     2,
       1,     4,     1,     4,     1,     0,     4,     2,     1,     3,
       2,     1,     1,     1,     1,     3,     3,     3,     3,     3,
       3,     3,     2,     2,     2,     2,     2,     1,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     2,     1,     3,     3,     4,     1,     4,
       3,     1,     0,     0,     0,     0
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,    39,    42,    40,   124,    45,    41,    82,    84,   128,
     131,   133,     9,     9,     9,     0,    26,    21,     0,   134,
       0,     0,     0,     0,     0,     9,     0,     0,     3,    16,
       5,    11,     6,     0,    22,    17,    92,    12,    28,    23,
     107,    80,    29,    91,    93,    94,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    25,     0,    24,     0,     0,
     102,   106,   105,   123,     8,     0,     0,     1,     4,    13,
      14,   133,    15,    27,     0,     0,     0,     0,     0,   104,
     103,    87,    85,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   126,
       0,    88,   130,     0,    47,   132,    71,    64,    66,    65,
      68,    69,    67,    46,    54,    63,     0,     0,     0,     0,
       0,    71,     0,     0,   122,     0,    97,     0,    95,    96,
     101,   100,    99,    98,     0,   121,   120,   116,   118,   115,
     114,   119,   117,   108,   109,   110,   111,   113,   112,    81,
      83,     0,   127,    90,   129,    70,     0,     0,    56,     0,
       0,     0,     0,   133,     0,   107,     0,   132,     0,     0,
      72,    10,     0,   135,    77,     0,    89,    49,     0,    55,
      59,    62,    58,     0,     0,     0,     0,     0,     0,    31,
       0,     0,     0,     0,     7,   125,    76,    79,     0,   133,
       0,    50,    44,    57,     0,    37,    63,     0,     0,     0,
       0,   132,    20,    43,     0,     0,    78,     0,     0,    48,
       0,     0,     0,     0,    33,    30,    18,     0,    75,     0,
       0,    53,    51,    60,    61,    38,     0,     0,     0,     0,
      74,    73,     0,    35,     0,    34,    19,    52,     0,    32,
      36
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    27,    28,    29,    51,   194,    30,    31,   168,    32,
      56,    33,    34,    35,   238,   248,    36,    37,    38,   178,
     200,   201,   113,   114,   115,   156,    39,   174,   175,    40,
      41,   134,   100,    42,    43,   129,    44,    45,   155,    50,
      58,   196
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -178
static const yytype_int16 yypact[] =
{
     494,  -178,  -178,  -178,  -178,  -178,  -178,   -43,   -32,   -36,
     -25,  -178,  -178,  -178,  -178,   -20,   760,  -178,    38,  -178,
      31,   760,    34,    34,   760,    13,   760,   280,  -178,  -178,
     598,  -178,  -178,   673,  -178,  -178,  -178,  -178,  -178,  -178,
     131,    32,  1036,  -178,  -178,  -178,   760,   760,   388,   420,
    1083,   650,    44,    49,   760,  1036,    59,  -178,    81,   760,
      69,  -178,  -178,    69,  -178,   598,   856,  -178,  -178,  -178,
    -178,  -178,  -178,  -178,   104,   760,   760,   760,   760,  -178,
    -178,  -178,  -178,   760,   760,   760,   760,   760,   760,   760,
     760,   760,   760,   760,   760,   760,   760,   753,   784,  -178,
     303,  1036,  -178,   335,  -178,  -178,  -178,  -178,  -178,  -178,
    -178,  -178,  -178,    43,  -178,   -15,    76,   760,   728,   879,
      91,  -178,   356,   546,  -178,  1097,  -178,   760,   153,  -178,
     153,   153,   153,   153,   116,   153,   153,   415,   415,   415,
     415,   415,   415,    -7,    -7,    69,    69,    69,    69,  -178,
    -178,   760,  -178,  1036,  -178,  -178,    74,   236,  -178,   705,
     760,    82,   902,  -178,    85,   101,   650,  -178,    10,    74,
    -178,  -178,   526,  -178,  -178,   224,  1036,   125,    90,  -178,
    -178,  1036,  -178,   808,   760,   650,  1097,   760,   760,   115,
     113,   140,    90,    90,  -178,  -178,    48,  -178,   116,  -178,
      66,  -178,  -178,   120,   925,  -178,    -4,  1017,   948,   650,
     119,  -178,  -178,  -178,   441,   760,  -178,   236,   125,  -178,
     578,   107,   760,   760,  -178,  -178,  -178,   124,  -178,   367,
     832,   136,  -178,  1036,  -178,  -178,   971,   994,   650,   119,
    -178,  -178,   760,  -178,   650,  -178,  -178,  1036,   650,  -178,
    -178
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -178,  -178,   139,  -166,     3,  -178,   102,   -28,  -178,  -178,
    -178,  -178,   -27,  -178,  -178,  -178,   -31,   -23,  -178,     0,
    -178,   -48,  -178,   -91,  -177,    52,    15,   -18,  -178,   -19,
    -178,  -178,   -38,   -16,  -178,  -139,  -178,  -178,  -128,   -70,
    -178,  -178
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -87
static const yytype_int16 yytable[] =
{
      55,   125,    69,    61,    62,    60,    73,    70,    63,   206,
      66,   103,   202,    46,   159,   222,    52,    53,    48,   112,
     182,   122,   158,   116,    47,   159,   212,   213,    65,    49,
      97,    98,   101,   101,    54,    93,    94,    95,   119,   190,
     231,   160,    96,   101,     7,     8,     1,     2,     3,   105,
      57,     6,   160,   107,   108,   109,   110,   111,   128,   130,
     131,   132,   133,   191,    64,   192,   179,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
     148,   234,   112,   227,   153,    59,    81,   153,   121,   172,
      82,   164,   161,   186,   112,    69,   157,   167,   117,   165,
      70,   162,   214,   118,   215,   126,   153,     1,     2,     3,
       4,   101,     6,   120,     7,     8,     9,    10,    96,   218,
     188,   219,     1,     2,     3,   173,   112,     6,   177,   217,
      74,    75,    76,    77,    78,   176,   184,   187,   189,   199,
      25,   209,   210,   181,   183,    21,   211,    79,    80,   220,
      22,    23,    24,   239,   127,   112,   153,   205,    26,   235,
      74,    75,    76,    77,    78,   242,    68,   123,   204,   193,
     232,   207,   208,   169,   180,     0,   229,    79,    80,   226,
     216,   225,     0,     0,     0,     0,   112,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,   101,   230,
       0,     0,    96,     0,   233,     0,   236,   237,   246,     0,
     245,     0,     0,   153,     0,     0,   249,     0,     0,     0,
     250,     0,     0,     0,     0,   197,   247,   -86,   -86,   -86,
     -86,     0,   -86,     0,   -86,   -86,   -86,   -86,   -86,     1,
       2,     3,   105,   -86,     6,     0,   107,   108,   109,   110,
     111,   -86,   -86,   -86,   -86,   -86,   -86,   -86,   -86,   -86,
     -86,   -86,   -86,   -86,   -86,   -86,   -86,   -86,   -86,     0,
     -86,   -86,   -86,   -86,     0,   -86,   -86,   -86,   -86,   -86,
      67,   -86,   198,     1,     2,     3,     4,     5,     6,     0,
       7,     8,     9,    10,    11,    12,    13,    14,    15,     0,
      16,    17,    18,    19,     0,    20,     1,     2,     3,     4,
       0,     6,     0,     7,     8,     9,    10,     0,     0,     0,
       0,    21,     0,     0,     0,     0,    22,    23,    24,     0,
      25,     0,     0,     0,    26,     0,     0,     0,     1,     2,
       3,     4,     0,     6,    21,     7,     8,     9,    10,    22,
      23,    24,     0,     0,     0,     0,   151,    26,   152,     1,
       2,     3,     4,     0,     6,     0,     7,     8,     9,    10,
       1,     2,     3,     4,     0,     6,    21,     7,     8,     9,
      10,    22,    23,    24,     0,     0,     0,     0,   151,    26,
     154,     1,     2,     3,     4,     0,     6,    21,     7,     8,
       9,    10,    22,    23,    24,     0,     0,     0,    21,   151,
      26,   170,     0,    22,    23,    24,     0,     0,     0,     0,
     151,    26,   240,     1,     2,     3,     4,     0,     6,    21,
       7,     8,     9,    10,    22,    23,    24,     0,     0,     0,
       0,     0,    26,    99,     1,     2,     3,     4,     0,     6,
       0,     7,     8,     9,    10,    91,    92,    93,    94,    95,
       0,    21,     0,     0,    96,     0,    22,    23,    24,     0,
       0,     0,     0,     0,    26,   102,     0,     0,     0,     0,
       0,     0,    21,     0,     0,     0,     0,    22,    23,    24,
       0,     0,     0,     0,     0,    26,   228,     1,     2,     3,
       4,     5,     6,     0,     7,     8,     9,    10,    11,    12,
      13,    14,    15,     0,    16,    17,    18,    19,     0,    20,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     1,
       2,     3,     4,     0,     6,    21,     7,     8,     9,    10,
      22,    23,    24,     0,    25,     0,     0,     0,    26,     1,
       2,     3,     4,     5,     6,     0,     7,     8,     9,    10,
      11,    12,    13,    14,    15,     0,    16,    21,    18,    19,
       0,    20,    22,    23,    24,     0,     0,   195,     0,   151,
      26,     1,     2,     3,     4,     0,     6,    21,     7,     8,
       9,    10,    22,    23,    24,     0,    25,   171,     0,     0,
      26,     1,     2,     3,     4,     5,     6,     0,     7,     8,
       9,    10,    11,    12,    13,    14,    15,     0,    16,    21,
      18,    19,     0,    20,    22,    23,    24,     0,   127,     0,
       0,     0,    26,     0,     0,     0,     0,     0,     0,    21,
       0,     0,     0,     0,    22,    23,    24,     0,    25,     0,
       0,     0,    26,     1,     2,     3,     4,     0,     6,     0,
       7,     8,     9,    10,    71,    12,    13,    14,    15,     0,
      16,     0,    18,     0,     0,    20,     1,     2,     3,     4,
       0,     6,     0,     7,     8,     9,    10,    71,     0,     0,
       0,    21,     0,     0,     0,     0,    22,    23,    24,     0,
      25,     0,     0,     0,    26,     0,     0,     0,     1,     2,
       3,     4,     0,     6,    21,     7,     8,     9,    10,    22,
      23,    24,     0,     0,     0,    72,     0,    26,     0,     0,
      20,     1,     2,     3,     4,     0,     6,     0,     7,     8,
       9,    10,   163,     0,     0,     0,    21,     0,     0,     0,
       0,    22,    23,    24,     0,   127,     0,     0,     0,    26,
       0,     0,     0,     1,     2,     3,     4,     0,     6,    21,
       7,     8,     9,    10,    22,    23,    24,     0,     0,     0,
      83,    84,    26,     0,     0,     0,     0,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,     0,     0,
       0,    21,    96,     0,     0,     0,    22,    23,    24,     0,
     149,    83,    84,     0,    26,     0,     0,     0,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,     0,
       0,     0,     0,    96,     0,    83,    84,     0,     0,     0,
       0,   150,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,     0,     0,     0,     0,    96,     0,    83,
      84,     0,     0,     0,     0,   203,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,     0,     0,     0,
       0,    96,     0,    83,    84,     0,     0,     0,     0,   241,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,     0,     0,     0,     0,    96,    83,    84,     0,     0,
       0,   124,     0,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,     0,     0,     0,     0,    96,    83,
      84,     0,     0,     0,   166,     0,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,     0,     0,     0,
       0,    96,    83,    84,     0,     0,     0,   185,     0,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
       0,     0,     0,     0,    96,    83,    84,     0,     0,     0,
     221,     0,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,     0,     0,     0,     0,    96,    83,    84,
       0,     0,     0,   224,     0,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,     0,     0,     0,     0,
      96,    83,    84,     0,     0,     0,   243,     0,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,     0,
       0,     0,     0,    96,    83,    84,     0,     0,     0,   244,
       0,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,     0,    83,    84,     0,    96,     0,     0,   223,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,     0,     0,     0,   104,    96,     1,     2,     3,   105,
     106,     6,     0,   107,   108,   109,   110,   111,   104,     0,
       1,     2,     3,   105,     0,     6,     0,   107,   108,   109,
     110,   111
};

static const yytype_int16 yycheck[] =
{
      16,    71,    30,    22,    23,    21,    33,    30,    24,   186,
      26,    49,   178,    56,    29,    19,    13,    14,    54,    50,
     159,    59,   113,    51,    56,    29,   192,   193,    25,    54,
      46,    47,    48,    49,    54,    42,    43,    44,    54,   167,
     217,    56,    49,    59,    10,    11,     3,     4,     5,     6,
      12,     8,    56,    10,    11,    12,    13,    14,    74,    75,
      76,    77,    78,    53,    51,    55,   157,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,   220,   113,   211,   100,    54,    54,   103,     7,   127,
      58,   118,    16,   163,   125,   123,    53,     6,    54,   118,
     123,   117,    54,    54,    56,     1,   122,     3,     4,     5,
       6,   127,     8,    54,    10,    11,    12,    13,    49,    53,
      19,    55,     3,     4,     5,     9,   157,     8,    54,   199,
      29,    30,    31,    32,    33,   151,    54,    52,   166,    14,
      50,    26,    29,   159,   160,    41,     6,    46,    47,    29,
      46,    47,    48,    29,    50,   186,   172,   185,    54,    52,
      29,    30,    31,    32,    33,    29,    27,    65,   184,   169,
     218,   187,   188,   121,   159,    -1,   214,    46,    47,   210,
     198,   209,    -1,    -1,    -1,    -1,   217,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,   214,   215,
      -1,    -1,    49,    -1,   220,    -1,   222,   223,   239,    -1,
     238,    -1,    -1,   229,    -1,    -1,   244,    -1,    -1,    -1,
     248,    -1,    -1,    -1,    -1,     1,   242,     3,     4,     5,
       6,    -1,     8,    -1,    10,    11,    12,    13,    14,     3,
       4,     5,     6,    19,     8,    -1,    10,    11,    12,    13,
      14,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      46,    47,    48,    49,    -1,    51,    52,    53,    54,    55,
       0,    57,    58,     3,     4,     5,     6,     7,     8,    -1,
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
       6,     7,     8,    -1,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    21,    22,    23,    -1,    25,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,    -1,     8,    41,    10,    11,    12,    13,
      46,    47,    48,    -1,    50,    -1,    -1,    -1,    54,     3,
       4,     5,     6,     7,     8,    -1,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    41,    22,    23,
      -1,    25,    46,    47,    48,    -1,    -1,    51,    -1,    53,
      54,     3,     4,     5,     6,    -1,     8,    41,    10,    11,
      12,    13,    46,    47,    48,    -1,    50,    51,    -1,    -1,
      54,     3,     4,     5,     6,     7,     8,    -1,    10,    11,
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
       0,     3,     4,     5,     6,     7,     8,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    20,    21,    22,    23,
      25,    41,    46,    47,    48,    50,    54,    60,    61,    62,
      65,    66,    68,    70,    71,    72,    75,    76,    77,    85,
      88,    89,    92,    93,    95,    96,    56,    56,    54,    54,
      98,    63,    63,    63,    54,    92,    69,    12,    99,    54,
      92,    88,    88,    92,    51,    63,    92,     0,    61,    66,
      76,    14,    52,    71,    29,    30,    31,    32,    33,    46,
      47,    54,    58,    27,    28,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    49,    92,    92,    55,
      91,    92,    55,    91,     1,     6,     7,    10,    11,    12,
      13,    14,    75,    81,    82,    83,    66,    54,    54,    92,
      54,     7,    91,    65,    55,    98,     1,    50,    92,    94,
      92,    92,    92,    92,    90,    92,    92,    92,    92,    92,
      92,    92,    92,    92,    92,    92,    92,    92,    92,    57,
      57,    53,    55,    92,    55,    97,    84,    53,    82,    29,
      56,    16,    92,    14,    71,    88,    55,     6,    67,    84,
      55,    51,    91,     9,    86,    87,    92,    54,    78,    82,
      85,    92,    94,    92,    54,    55,    98,    52,    19,    66,
      97,    53,    55,    78,    64,    51,   100,     1,    58,    14,
      79,    80,    62,    57,    92,    66,    83,    92,    92,    26,
      29,     6,    62,    62,    54,    56,    86,    98,    53,    55,
      29,    55,    19,    52,    55,    66,    75,    97,    55,    91,
      92,    83,    80,    92,    94,    52,    92,    92,    73,    29,
      55,    57,    29,    55,    55,    66,    75,    92,    74,    66,
      66
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
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
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
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
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


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*-------------------------.
| yyparse or yypush_parse.  |
`-------------------------*/

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
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

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
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
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

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
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

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
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

/* Line 1455 of yacc.c  */
#line 70 "grammar.yy"
    { }
    break;

  case 3:

/* Line 1455 of yacc.c  */
#line 71 "grammar.yy"
    { }
    break;

  case 4:

/* Line 1455 of yacc.c  */
#line 72 "grammar.yy"
    { }
    break;

  case 5:

/* Line 1455 of yacc.c  */
#line 76 "grammar.yy"
    { if (((yyvsp[(1) - (1)].node) != NULL) && (!cmdparser.addStatement((yyvsp[(1) - (1)].node)))) YYABORT; }
    break;

  case 6:

/* Line 1455 of yacc.c  */
#line 77 "grammar.yy"
    { }
    break;

  case 7:

/* Line 1455 of yacc.c  */
#line 83 "grammar.yy"
    { (yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (5)].node),(yyvsp[(3) - (5)].node)); }
    break;

  case 8:

/* Line 1455 of yacc.c  */
#line 84 "grammar.yy"
    { (yyval.node) = cmdparser.addFunction(Command::NoFunction); }
    break;

  case 9:

/* Line 1455 of yacc.c  */
#line 88 "grammar.yy"
    { (yyval.node) = cmdparser.pushScope(); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 10:

/* Line 1455 of yacc.c  */
#line 92 "grammar.yy"
    { if (!cmdparser.popScope()) YYABORT; }
    break;

  case 11:

/* Line 1455 of yacc.c  */
#line 96 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 12:

/* Line 1455 of yacc.c  */
#line 97 "grammar.yy"
    { (yyval.node) = NULL; }
    break;

  case 13:

/* Line 1455 of yacc.c  */
#line 98 "grammar.yy"
    { (yyval.node) = cmdparser.joinCommands((yyvsp[(1) - (2)].node), (yyvsp[(2) - (2)].node)); }
    break;

  case 14:

/* Line 1455 of yacc.c  */
#line 99 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (2)].node); }
    break;

  case 15:

/* Line 1455 of yacc.c  */
#line 103 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (2)].node); }
    break;

  case 16:

/* Line 1455 of yacc.c  */
#line 104 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 105 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 18:

/* Line 1455 of yacc.c  */
#line 111 "grammar.yy"
    { if (!cmdparser.setFilterOption(&tokenName, (yyvsp[(4) - (4)].node))) YYABORT; }
    break;

  case 19:

/* Line 1455 of yacc.c  */
#line 112 "grammar.yy"
    { if (!cmdparser.setFilterOption(&tokenName, (yyvsp[(6) - (6)].node))) YYABORT; }
    break;

  case 20:

/* Line 1455 of yacc.c  */
#line 116 "grammar.yy"
    { if (!cmdparser.addStatement((yyvsp[(6) - (6)].node))) YYABORT; cmdparser.popTree(); }
    break;

  case 21:

/* Line 1455 of yacc.c  */
#line 120 "grammar.yy"
    { cmdparser.pushTree(TRUE); }
    break;

  case 22:

/* Line 1455 of yacc.c  */
#line 126 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 23:

/* Line 1455 of yacc.c  */
#line 127 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 24:

/* Line 1455 of yacc.c  */
#line 128 "grammar.yy"
    { (yyval.node) = cmdparser.addFunction(Command::Help, cmdparser.addConstant((yyvsp[(2) - (2)].functionId))); }
    break;

  case 25:

/* Line 1455 of yacc.c  */
#line 129 "grammar.yy"
    { (yyval.node) = cmdparser.addFunction(Command::Return,(yyvsp[(2) - (2)].node)); }
    break;

  case 26:

/* Line 1455 of yacc.c  */
#line 130 "grammar.yy"
    { (yyval.node) = cmdparser.addFunction(Command::Return); }
    break;

  case 27:

/* Line 1455 of yacc.c  */
#line 131 "grammar.yy"
    { msg.print("Error: Expected ';' before current expression.\n"); YYABORT; }
    break;

  case 28:

/* Line 1455 of yacc.c  */
#line 136 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 29:

/* Line 1455 of yacc.c  */
#line 137 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 30:

/* Line 1455 of yacc.c  */
#line 141 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::If,(yyvsp[(3) - (7)].node),(yyvsp[(5) - (7)].node),(yyvsp[(7) - (7)].node)); }
    break;

  case 31:

/* Line 1455 of yacc.c  */
#line 143 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::If,(yyvsp[(3) - (5)].node),(yyvsp[(5) - (5)].node)); }
    break;

  case 32:

/* Line 1455 of yacc.c  */
#line 145 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (10)].node), cmdparser.addFunction(Command::For, (yyvsp[(4) - (10)].node),(yyvsp[(6) - (10)].node),(yyvsp[(8) - (10)].node),(yyvsp[(10) - (10)].node))); cmdparser.popScope(); }
    break;

  case 33:

/* Line 1455 of yacc.c  */
#line 147 "grammar.yy"
    {
		if ((yyvsp[(4) - (7)].node)->returnType() <= VTypes::VectorData) { msg.print("Error: For/In loop variable must be of pointer type.\n"); YYABORT; }
		if ((yyvsp[(4) - (7)].node)->returnType() != (yyvsp[(6) - (7)].node)->returnType()) { msg.print("Error: For/In loop variable is not being assigned the correct type.\n"); YYABORT; }
		}
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 151 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (9)].node), cmdparser.addFunction(Command::ForIn,(yyvsp[(4) - (9)].node),(yyvsp[(6) - (9)].node),(yyvsp[(9) - (9)].node))); cmdparser.popScope(); }
    break;

  case 35:

/* Line 1455 of yacc.c  */
#line 153 "grammar.yy"
    { 
		if (declaredType <= VTypes::VectorData) { msg.print("Error: For/In loop variable must be of pointer type.\n"); YYABORT; }
		tempNode = cmdparser.addVariable(declaredType, &tokenName);
		if (declaredType != (yyvsp[(8) - (9)].node)->returnType()) { msg.print("Error: For/In loop variable is not being assigned the correct type.\n"); YYABORT; }
		}
    break;

  case 36:

/* Line 1455 of yacc.c  */
#line 158 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (11)].node), cmdparser.addFunction(Command::ForIn,tempNode,(yyvsp[(8) - (11)].node),(yyvsp[(11) - (11)].node))); cmdparser.popScope(); }
    break;

  case 37:

/* Line 1455 of yacc.c  */
#line 160 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (6)].node), cmdparser.addFunction(Command::While, (yyvsp[(4) - (6)].node),(yyvsp[(6) - (6)].node))); cmdparser.popScope(); }
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 162 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (8)].node), cmdparser.addFunction(Command::DoWhile, (yyvsp[(3) - (8)].node),(yyvsp[(6) - (8)].node))); cmdparser.popScope(); }
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 169 "grammar.yy"
    { (yyval.node) = cmdparser.addConstant((yyvsp[(1) - (1)].intconst)); }
    break;

  case 40:

/* Line 1455 of yacc.c  */
#line 170 "grammar.yy"
    { (yyval.node) = cmdparser.addConstant((yyvsp[(1) - (1)].doubleconst)); }
    break;

  case 41:

/* Line 1455 of yacc.c  */
#line 171 "grammar.yy"
    { (yyval.node) = cmdparser.addConstant((yyvsp[(1) - (1)].name)->get()); }
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 172 "grammar.yy"
    { (yyval.node) = cmdparser.addElementConstant((yyvsp[(1) - (1)].intconst)); }
    break;

  case 43:

/* Line 1455 of yacc.c  */
#line 178 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : funcdeclaration : user-defined statement\n");
		if (!cmdparser.addStatement((yyvsp[(6) - (6)].node))) YYABORT; cmdparser.popTree(); declaredType = VTypes::NoData; }
    break;

  case 44:

/* Line 1455 of yacc.c  */
#line 181 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : funcdeclaration : user-defined function\n");
		if (!cmdparser.addStatement((yyvsp[(6) - (6)].node))) YYABORT; cmdparser.popTree(); declaredType = VTypes::NoData; }
    break;

  case 45:

/* Line 1455 of yacc.c  */
#line 184 "grammar.yy"
    {
		msg.print("Error: '%s' is not a recognised built-in or user-defined function.\n", (yyvsp[(1) - (1)].name)->get()); YYABORT; }
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 189 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : vardeclaration : standard variable declaration list\n");
		(yyval.node) = cmdparser.addDeclarations((yyvsp[(3) - (3)].node)); declaredType = VTypes::NoData; }
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 192 "grammar.yy"
    {
		msg.print("Illegal use of reserved word '%s'.\n", VTypes::dataType(declaredType)); YYABORT; }
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 197 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : arglist : bracketed argument list\n");
		(yyval.node) = (yyvsp[(2) - (3)].node); }
    break;

  case 49:

/* Line 1455 of yacc.c  */
#line 203 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : args : empty\n");
		(yyval.node) = NULL; }
    break;

  case 50:

/* Line 1455 of yacc.c  */
#line 206 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : args : adding item\n");
		(yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 51:

/* Line 1455 of yacc.c  */
#line 209 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : args : joining items\n");
		if ((yyvsp[(3) - (3)].node) == NULL) YYABORT; (yyval.node) = Tree::joinArguments((yyvsp[(3) - (3)].node),(yyvsp[(1) - (3)].node)); }
    break;

  case 52:

/* Line 1455 of yacc.c  */
#line 215 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : argdeclaration : function argument '%s' with expr assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addVariableAsArgument(declaredType, &tokenName, (yyvsp[(5) - (5)].node)); }
    break;

  case 53:

/* Line 1455 of yacc.c  */
#line 218 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : argdeclaration : function argument '%s'\n", tokenName.get());
		(yyval.node) = cmdparser.addVariableAsArgument(declaredType, &tokenName); }
    break;

  case 54:

/* Line 1455 of yacc.c  */
#line 224 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyvsp[(1) - (1)].node) == NULL) YYABORT; }
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 225 "grammar.yy"
    { if ((yyvsp[(3) - (3)].node) == NULL) YYABORT; (yyval.node) = Tree::joinArguments((yyvsp[(3) - (3)].node),(yyvsp[(1) - (3)].node)); }
    break;

  case 56:

/* Line 1455 of yacc.c  */
#line 226 "grammar.yy"
    { msg.print("Error: Missing comma between declarations?\n"); YYABORT; }
    break;

  case 57:

/* Line 1455 of yacc.c  */
#line 230 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : vardec : array var '%s'\n", tokenName.get());
		(yyval.node) = cmdparser.addArrayVariable(declaredType, &tokenName, (yyvsp[(3) - (4)].node)); }
    break;

  case 58:

/* Line 1455 of yacc.c  */
#line 233 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : vardec : var '%s' with array assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addVariable(declaredType, &tokenName, (yyvsp[(3) - (3)].node)); }
    break;

  case 59:

/* Line 1455 of yacc.c  */
#line 236 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : vardec : var '%s' with widget assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addVariable(declaredType, &tokenName, (yyvsp[(3) - (3)].node)); }
    break;

  case 60:

/* Line 1455 of yacc.c  */
#line 239 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : vardec : array var '%s' with expr assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addArrayVariable(declaredType, &tokenName,(yyvsp[(3) - (6)].node),(yyvsp[(6) - (6)].node)); }
    break;

  case 61:

/* Line 1455 of yacc.c  */
#line 242 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : vardec : array var '%s' with array assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addArrayVariable(declaredType, &tokenName,(yyvsp[(3) - (6)].node),(yyvsp[(6) - (6)].node)); }
    break;

  case 62:

/* Line 1455 of yacc.c  */
#line 245 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : vardec : var '%s' with expr assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addVariable(declaredType, &tokenName, (yyvsp[(3) - (3)].node)); }
    break;

  case 63:

/* Line 1455 of yacc.c  */
#line 248 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : vardec : var '%s'\n", tokenName.get());
		(yyval.node) = cmdparser.addVariable(declaredType, &tokenName); }
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 254 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : newname : existing var '%s'\n", tokenName.get());
		tokenName = yylval.variable->name(); (yyval.name) = &tokenName; }
    break;

  case 65:

/* Line 1455 of yacc.c  */
#line 257 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : newname : existing built-in function '%s'\n", tokenName.get());
		tokenName = Command::data[yylval.functionId].keyword; (yyval.name) = &tokenName; }
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 260 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : newname : existing local var '%s'\n", tokenName.get());
		msg.print("Error: Existing variable in local scope cannot be redeclared.\n"); YYABORT; }
    break;

  case 67:

/* Line 1455 of yacc.c  */
#line 263 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : newname : constant '%s'\n", tokenName.get());
		msg.print("Error: Constant value found in declaration.\n"); YYABORT; }
    break;

  case 68:

/* Line 1455 of yacc.c  */
#line 266 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : newname : existing user function '%s'\n", tokenName.get());
		msg.print("Error: Existing user-defined function name cannot be redeclared.\n"); YYABORT; }
    break;

  case 69:

/* Line 1455 of yacc.c  */
#line 269 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : newname : variable type-name '%s'\n", VTypes::dataType( yylval.vtype));
		msg.print("Error: Type-name used in variable declaration.\n"); YYABORT; }
    break;

  case 70:

/* Line 1455 of yacc.c  */
#line 272 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : newname : new token '%s'\n", tokenName.get());
		if (declaredType == VTypes::NoData) { msg.print("Token '%s' is undeclared.\n", tokenName.get()); YYABORT; } (yyval.name) = (yyvsp[(1) - (2)].name); }
    break;

  case 71:

/* Line 1455 of yacc.c  */
#line 278 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : pushfunc : function/statement '%s'\n", yylval.name->get());
		cmdparser.pushFunction(yylval.name->get(), declaredType); }
    break;

  case 72:

/* Line 1455 of yacc.c  */
#line 287 "grammar.yy"
    { (yyval.node) = cmdparser.addWidget((yyvsp[(3) - (4)].node)); }
    break;

  case 73:

/* Line 1455 of yacc.c  */
#line 293 "grammar.yy"
    { if (!cmdparser.expandPath(stepNameStack.last(), (yyvsp[(4) - (5)].node))) YYABORT; stepNameStack.removeLast(); }
    break;

  case 74:

/* Line 1455 of yacc.c  */
#line 294 "grammar.yy"
    { if (!cmdparser.expandPath(stepNameStack.last(), NULL, (yyvsp[(4) - (5)].node))) YYABORT; stepNameStack.removeLast(); }
    break;

  case 75:

/* Line 1455 of yacc.c  */
#line 295 "grammar.yy"
    { if (!cmdparser.expandPath(stepNameStack.last(), NULL, NULL)) YYABORT; stepNameStack.removeLast(); }
    break;

  case 76:

/* Line 1455 of yacc.c  */
#line 296 "grammar.yy"
    { if (!cmdparser.expandPath((yyvsp[(1) - (2)].name))) YYABORT; stepNameStack.removeLast(); }
    break;

  case 77:

/* Line 1455 of yacc.c  */
#line 300 "grammar.yy"
    { }
    break;

  case 78:

/* Line 1455 of yacc.c  */
#line 301 "grammar.yy"
    { }
    break;

  case 79:

/* Line 1455 of yacc.c  */
#line 302 "grammar.yy"
    { msg.print("Error formulating path.\n"); YYABORT; }
    break;

  case 80:

/* Line 1455 of yacc.c  */
#line 306 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 81:

/* Line 1455 of yacc.c  */
#line 310 "grammar.yy"
    { (yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (4)].variable),(yyvsp[(3) - (4)].node)); }
    break;

  case 82:

/* Line 1455 of yacc.c  */
#line 311 "grammar.yy"
    { (yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (1)].variable)); }
    break;

  case 83:

/* Line 1455 of yacc.c  */
#line 312 "grammar.yy"
    { (yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (4)].variable),(yyvsp[(3) - (4)].node)); }
    break;

  case 84:

/* Line 1455 of yacc.c  */
#line 313 "grammar.yy"
    { (yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (1)].variable)); }
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 314 "grammar.yy"
    { cmdparser.createPath((yyvsp[(1) - (2)].node)); }
    break;

  case 86:

/* Line 1455 of yacc.c  */
#line 315 "grammar.yy"
    { (yyval.node) = cmdparser.finalisePath(); }
    break;

  case 87:

/* Line 1455 of yacc.c  */
#line 316 "grammar.yy"
    { msg.print("Can't use a variable as a function. Did you mean '[' instead?\n"); (yyval.node) = NULL; }
    break;

  case 88:

/* Line 1455 of yacc.c  */
#line 322 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 89:

/* Line 1455 of yacc.c  */
#line 323 "grammar.yy"
    { (yyval.node) = Tree::joinArguments((yyvsp[(3) - (3)].node),(yyvsp[(1) - (3)].node)); }
    break;

  case 90:

/* Line 1455 of yacc.c  */
#line 324 "grammar.yy"
    { msg.print("Error: Missing comma between items.\n"); YYABORT; }
    break;

  case 91:

/* Line 1455 of yacc.c  */
#line 328 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 92:

/* Line 1455 of yacc.c  */
#line 332 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 93:

/* Line 1455 of yacc.c  */
#line 333 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 94:

/* Line 1455 of yacc.c  */
#line 334 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 95:

/* Line 1455 of yacc.c  */
#line 335 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignment,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); }
    break;

  case 96:

/* Line 1455 of yacc.c  */
#line 336 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignment,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); }
    break;

  case 97:

/* Line 1455 of yacc.c  */
#line 337 "grammar.yy"
    { msg.print("Mangled expression used in assignment.\n"); YYABORT; }
    break;

  case 98:

/* Line 1455 of yacc.c  */
#line 338 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignmentPlus,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); }
    break;

  case 99:

/* Line 1455 of yacc.c  */
#line 339 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignmentSubtract,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); }
    break;

  case 100:

/* Line 1455 of yacc.c  */
#line 340 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignmentMultiply,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); }
    break;

  case 101:

/* Line 1455 of yacc.c  */
#line 341 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignmentDivide,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); }
    break;

  case 102:

/* Line 1455 of yacc.c  */
#line 342 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorNegate, (yyvsp[(2) - (2)].node)); }
    break;

  case 103:

/* Line 1455 of yacc.c  */
#line 343 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPostfixIncrease, (yyvsp[(1) - (2)].node));  }
    break;

  case 104:

/* Line 1455 of yacc.c  */
#line 344 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPostfixDecrease, (yyvsp[(1) - (2)].node)); }
    break;

  case 105:

/* Line 1455 of yacc.c  */
#line 345 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPrefixIncrease, (yyvsp[(2) - (2)].node)); }
    break;

  case 106:

/* Line 1455 of yacc.c  */
#line 346 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPrefixDecrease, (yyvsp[(2) - (2)].node)); }
    break;

  case 107:

/* Line 1455 of yacc.c  */
#line 347 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 108:

/* Line 1455 of yacc.c  */
#line 348 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAdd, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 109:

/* Line 1455 of yacc.c  */
#line 349 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorSubtract, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 110:

/* Line 1455 of yacc.c  */
#line 350 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorMultiply, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 111:

/* Line 1455 of yacc.c  */
#line 351 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorDivide, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 112:

/* Line 1455 of yacc.c  */
#line 352 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPower, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 113:

/* Line 1455 of yacc.c  */
#line 353 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorModulus, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 114:

/* Line 1455 of yacc.c  */
#line 354 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 115:

/* Line 1455 of yacc.c  */
#line 355 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorNotEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 116:

/* Line 1455 of yacc.c  */
#line 356 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorGreaterThan, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 117:

/* Line 1455 of yacc.c  */
#line 357 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorGreaterThanEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 118:

/* Line 1455 of yacc.c  */
#line 358 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorLessThan, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 119:

/* Line 1455 of yacc.c  */
#line 359 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorLessThanEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 120:

/* Line 1455 of yacc.c  */
#line 360 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAnd, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 121:

/* Line 1455 of yacc.c  */
#line 361 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorOr, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); }
    break;

  case 122:

/* Line 1455 of yacc.c  */
#line 362 "grammar.yy"
    { (yyval.node) = (yyvsp[(2) - (3)].node); }
    break;

  case 123:

/* Line 1455 of yacc.c  */
#line 363 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorNot, (yyvsp[(2) - (2)].node)); }
    break;

  case 124:

/* Line 1455 of yacc.c  */
#line 364 "grammar.yy"
    { msg.print("Error: '%s' has not been declared as a function or a variable.\n", yylval.name->get()); YYABORT; }
    break;

  case 125:

/* Line 1455 of yacc.c  */
#line 370 "grammar.yy"
    { (yyval.node) = cmdparser.addArrayConstant((yyvsp[(2) - (3)].node)); }
    break;

  case 126:

/* Line 1455 of yacc.c  */
#line 376 "grammar.yy"
    { (yyval.node) = cmdparser.addFunction( (Command::Function) (yyvsp[(1) - (3)].functionId)); }
    break;

  case 127:

/* Line 1455 of yacc.c  */
#line 377 "grammar.yy"
    { (yyval.node) = cmdparser.addFunctionWithArglist( (Command::Function) (yyvsp[(1) - (4)].functionId),(yyvsp[(3) - (4)].node)); }
    break;

  case 128:

/* Line 1455 of yacc.c  */
#line 378 "grammar.yy"
    { (yyval.node) = cmdparser.addFunction( (Command::Function) (yyvsp[(1) - (1)].functionId)); }
    break;

  case 129:

/* Line 1455 of yacc.c  */
#line 382 "grammar.yy"
    { (yyval.node) = cmdparser.addUserFunction((yyvsp[(1) - (4)].functree),(yyvsp[(3) - (4)].node)); }
    break;

  case 130:

/* Line 1455 of yacc.c  */
#line 383 "grammar.yy"
    { (yyval.node) = cmdparser.addUserFunction((yyvsp[(1) - (3)].functree)); }
    break;

  case 131:

/* Line 1455 of yacc.c  */
#line 384 "grammar.yy"
    { (yyval.node) = cmdparser.addUserFunction((yyvsp[(1) - (1)].functree)); }
    break;

  case 132:

/* Line 1455 of yacc.c  */
#line 390 "grammar.yy"
    { tokenName = *yylval.name; }
    break;

  case 133:

/* Line 1455 of yacc.c  */
#line 394 "grammar.yy"
    { declaredType = yylval.vtype; }
    break;

  case 134:

/* Line 1455 of yacc.c  */
#line 398 "grammar.yy"
    { declaredType = VTypes::NoData; }
    break;

  case 135:

/* Line 1455 of yacc.c  */
#line 402 "grammar.yy"
    { stepNameStack.add()->set(yylval.name->get()); }
    break;



/* Line 1455 of yacc.c  */
#line 2849 "grammar.cc"
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
      /* If just tried and failed to reuse lookahead token after an
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

  /* Else will try to reuse lookahead token after shifting the error
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

#if !defined(yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
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



/* Line 1675 of yacc.c  */
#line 405 "grammar.yy"


void yyerror(char *s)
{
}

