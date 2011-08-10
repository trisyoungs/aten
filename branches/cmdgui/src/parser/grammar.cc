
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
     ATEN_ELSE = 283,
     OR = 284,
     AND = 285,
     DEQ = 286,
     TEQ = 287,
     MEQ = 288,
     PEQ = 289,
     NEQ = 290,
     EQ = 291,
     LEQ = 292,
     GEQ = 293,
     UMINUS = 294,
     MINUSMINUS = 295,
     PLUSPLUS = 296
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
#define ATEN_ELSE 283
#define OR 284
#define AND 285
#define DEQ 286
#define TEQ 287
#define MEQ 288
#define PEQ 289
#define NEQ 290
#define EQ 291
#define LEQ 292
#define GEQ 293
#define UMINUS 294
#define MINUSMINUS 295
#define PLUSPLUS 296




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 214 of yacc.c  */
#line 30 "grammar.yy"

	int functionId;			/* Function enum id */
	Dnchar *name;			/* character pointer for names */
	TreeNode *node;			/* node pointer */
	Variable *variable;		/* variable pointer */
	Tree *tree;			/* function (tree) pointer */
	VTypes::DataType vtype;		/* variable type for next declarations */
	int intconst;			/* integer constant value */
	double doubleconst;		/* double constant value */



/* Line 214 of yacc.c  */
#line 235 "grammar.cc"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 247 "grammar.cc"

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
#define YYFINAL  72
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1256

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  62
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  44
/* YYNRULES -- Number of rules.  */
#define YYNRULES  143
/* YYNRULES -- Number of states.  */
#define YYNSTATES  288

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   296

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    50,     2,     2,     2,    46,     2,     2,
      54,    55,    44,    42,    59,    43,    56,    45,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    61,    60,
      37,    31,    36,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    52,     2,    53,    51,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    57,     2,    58,     2,     2,     2,     2,
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
      25,    26,    27,    28,    29,    30,    32,    33,    34,    35,
      38,    39,    40,    41,    47,    48,    49
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     8,    10,    12,    14,    16,    18,
      20,    26,    32,    37,    40,    42,    46,    49,    54,    56,
      61,    63,    64,    69,    72,    76,    81,    84,    88,    93,
      96,   100,   104,   108,   112,   114,   116,   118,   122,   126,
     130,   134,   137,   140,   143,   146,   149,   151,   155,   159,
     163,   167,   171,   175,   179,   183,   187,   191,   195,   199,
     203,   207,   211,   214,   216,   218,   222,   225,   227,   229,
     231,   233,   235,   237,   240,   244,   251,   258,   262,   264,
     269,   271,   273,   277,   280,   284,   290,   292,   296,   299,
     303,   307,   315,   316,   326,   334,   335,   345,   348,   351,
     354,   357,   360,   362,   364,   366,   369,   373,   376,   379,
     382,   384,   387,   393,   396,   398,   400,   408,   414,   425,
     436,   437,   447,   448,   460,   467,   476,   477,   486,   492,
     495,   497,   500,   503,   508,   515,   522,   523,   524,   525,
     526,   527,   528,   529
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      63,     0,    -1,    64,    -1,    63,    64,    -1,    87,    -1,
      88,    -1,     3,    -1,     5,    -1,     7,    -1,     4,    -1,
       8,   103,    52,    74,    53,    -1,     8,   103,    54,    75,
      55,    -1,     8,   103,    54,    55,    -1,     8,   103,    -1,
      66,    -1,    67,    56,    66,    -1,    67,     1,    -1,     9,
      52,    74,    53,    -1,     9,    -1,    10,    52,    74,    53,
      -1,    10,    -1,    -1,    68,    56,    69,    67,    -1,    68,
      54,    -1,    11,    54,    55,    -1,    11,    54,    75,    55,
      -1,    11,     1,    -1,    12,    54,    55,    -1,    12,    54,
      75,    55,    -1,    12,     1,    -1,    57,    75,    58,    -1,
      68,    31,    74,    -1,    68,    31,    72,    -1,    68,    31,
       1,    -1,    65,    -1,    70,    -1,    71,    -1,    68,    35,
      74,    -1,    68,    34,    74,    -1,    68,    33,    74,    -1,
      68,    32,    74,    -1,    43,    74,    -1,    68,    49,    -1,
      68,    48,    -1,    49,    68,    -1,    48,    68,    -1,    68,
      -1,    74,    42,    74,    -1,    74,    43,    74,    -1,    74,
      44,    74,    -1,    74,    45,    74,    -1,    74,    51,    74,
      -1,    74,    46,    74,    -1,    74,    39,    74,    -1,    74,
      38,    74,    -1,    74,    36,    74,    -1,    74,    41,    74,
      -1,    74,    37,    74,    -1,    74,    40,    74,    -1,    74,
      30,    74,    -1,    74,    29,    74,    -1,    54,    74,    55,
      -1,    50,    74,    -1,     6,    -1,    74,    -1,    75,    59,
      74,    -1,    75,    74,    -1,     9,    -1,    11,    -1,    10,
      -1,    65,    -1,    12,    -1,    13,    -1,     6,    98,    -1,
      76,    31,    72,    -1,    76,    52,    74,    53,    31,    74,
      -1,    76,    52,    74,    53,    31,    72,    -1,    76,    31,
      74,    -1,    76,    -1,    76,    52,    74,    53,    -1,    77,
      -1,    78,    -1,    79,    59,    78,    -1,    79,    78,    -1,
      13,    99,    76,    -1,    13,    99,    76,    31,    74,    -1,
      80,    -1,    81,    59,    80,    -1,    81,    80,    -1,    13,
      99,    79,    -1,    13,    99,     1,    -1,    25,   100,     6,
     104,    54,    55,    88,    -1,    -1,    25,   100,     6,   104,
      54,    81,    55,    84,    88,    -1,    13,    99,     6,   104,
      54,    55,    88,    -1,    -1,    13,    99,     6,   104,    54,
      81,    55,    85,    88,    -1,    73,    60,    -1,    82,    60,
      -1,    74,    60,    -1,    70,    60,    -1,    71,    60,    -1,
      90,    -1,    83,    -1,    97,    -1,    24,    11,    -1,    22,
      74,    60,    -1,    22,    60,    -1,    26,    60,    -1,    27,
      60,    -1,    86,    -1,    87,    86,    -1,    57,   101,    87,
      58,   102,    -1,    57,    58,    -1,    86,    -1,    88,    -1,
      20,    54,    74,    55,    89,    28,    89,    -1,    20,    54,
      74,    55,    89,    -1,    16,   101,    54,    73,    60,    74,
      60,    74,    55,    89,    -1,    16,   101,    54,    82,    60,
      74,    60,    74,    55,    89,    -1,    -1,    16,   101,    54,
      68,    21,    74,    55,    91,    89,    -1,    -1,    16,   101,
      54,    13,    99,    76,    21,    74,    55,    92,    89,    -1,
      15,   101,    54,    74,    55,    89,    -1,    14,   101,    88,
      15,    54,    74,    55,    60,    -1,    -1,    17,    54,    74,
      55,    93,    57,    95,    58,    -1,    18,    54,    74,    55,
      61,    -1,    19,    61,    -1,    94,    -1,    95,    87,    -1,
      95,    94,    -1,     6,    98,    31,    65,    -1,    96,    59,
       6,    98,    31,    65,    -1,    23,   105,    54,    96,    55,
      88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    76,    76,    77,    82,    85,    95,    96,    97,    98,
     107,   111,   115,   119,   127,   128,   129,   134,   138,   142,
     146,   150,   150,   155,   166,   171,   176,   184,   189,   194,
     206,   217,   218,   219,   224,   225,   226,   227,   228,   229,
     230,   231,   232,   233,   234,   235,   236,   237,   238,   239,
     240,   241,   242,   243,   244,   245,   246,   247,   248,   249,
     250,   251,   252,   253,   258,   262,   265,   277,   282,   287,
     292,   297,   302,   307,   316,   320,   324,   328,   336,   340,
     344,   351,   354,   357,   365,   369,   377,   380,   383,   391,
     396,   408,   414,   414,   422,   428,   428,   444,   447,   450,
     453,   456,   459,   462,   465,   468,   471,   474,   477,   480,
     487,   490,   498,   501,   508,   511,   518,   521,   524,   527,
     530,   530,   537,   537,   553,   557,   561,   561,   575,   584,
     591,   594,   597,   608,   612,   620,   632,   636,   640,   644,
     648,   652,   656,   664
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
  "ATEN_BREAK", "ATEN_ELSE", "OR", "AND", "'='", "DEQ", "TEQ", "MEQ",
  "PEQ", "'>'", "'<'", "NEQ", "EQ", "LEQ", "GEQ", "'+'", "'-'", "'*'",
  "'/'", "'%'", "UMINUS", "MINUSMINUS", "PLUSPLUS", "'!'", "'^'", "'['",
  "']'", "'('", "')'", "'.'", "'{'", "'}'", "','", "';'", "':'", "$accept",
  "programlist", "program", "constant", "step", "steplist", "variable",
  "$@1", "function", "userfunction", "ARRAYCONST", "assignment",
  "expression", "expressionlist", "variablename", "assignedvariablename",
  "variablelistitem", "variablelist", "typedvariablelistitem",
  "typedvariablelist", "declaration", "functiondeclaration", "$@2", "$@3",
  "statement", "statementlist", "block", "blockment", "flowstatement",
  "$@4", "$@5", "$@6", "caselabel", "caselist", "filteroptions", "filter",
  "savetokenname", "savetype", "cleartype", "pushscope", "popscope",
  "pushstepname", "pushfunc", "pushfilter", 0
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
     285,    61,   286,   287,   288,   289,    62,    60,   290,   291,
     292,   293,    43,    45,    42,    47,    37,   294,   295,   296,
      33,    94,    91,    93,    40,    41,    46,   123,   125,    44,
      59,    58
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    62,    63,    63,    64,    64,    65,    65,    65,    65,
      66,    66,    66,    66,    67,    67,    67,    68,    68,    68,
      68,    69,    68,    68,    70,    70,    70,    71,    71,    71,
      72,    73,    73,    73,    74,    74,    74,    74,    74,    74,
      74,    74,    74,    74,    74,    74,    74,    74,    74,    74,
      74,    74,    74,    74,    74,    74,    74,    74,    74,    74,
      74,    74,    74,    74,    75,    75,    75,    76,    76,    76,
      76,    76,    76,    76,    77,    77,    77,    77,    78,    78,
      78,    79,    79,    79,    80,    80,    81,    81,    81,    82,
      82,    83,    84,    83,    83,    85,    83,    86,    86,    86,
      86,    86,    86,    86,    86,    86,    86,    86,    86,    86,
      87,    87,    88,    88,    89,    89,    90,    90,    90,    90,
      91,    90,    92,    90,    90,    90,    93,    90,    94,    94,
      95,    95,    95,    96,    96,    97,    98,    99,   100,   101,
     102,   103,   104,   105
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     1,     1,     1,     1,     1,     1,
       5,     5,     4,     2,     1,     3,     2,     4,     1,     4,
       1,     0,     4,     2,     3,     4,     2,     3,     4,     2,
       3,     3,     3,     3,     1,     1,     1,     3,     3,     3,
       3,     2,     2,     2,     2,     2,     1,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     2,     1,     1,     3,     2,     1,     1,     1,
       1,     1,     1,     2,     3,     6,     6,     3,     1,     4,
       1,     1,     3,     2,     3,     5,     1,     3,     2,     3,
       3,     7,     0,     9,     7,     0,     9,     2,     2,     2,
       2,     2,     1,     1,     1,     2,     3,     2,     2,     2,
       1,     2,     5,     2,     1,     1,     7,     5,    10,    10,
       0,     9,     0,    11,     6,     8,     0,     8,     5,     2,
       1,     2,     2,     4,     6,     6,     0,     0,     0,     0,
       0,     0,     0,     0
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     6,     9,     7,    63,     8,    18,    20,     0,     0,
     137,   139,   139,   139,     0,     0,     0,   143,     0,   138,
       0,     0,     0,     0,     0,     0,     0,   139,     0,     2,
      34,    46,    35,    36,     0,     0,     0,   103,   110,     4,
       5,   102,   104,     0,     0,    26,     0,    29,     0,     0,
       0,     0,     0,     0,     0,   107,    46,    35,    36,     0,
       0,   105,     0,   108,   109,    41,    45,    44,    62,     0,
     113,     0,     1,     3,     0,     0,     0,     0,     0,    43,
      42,    23,    21,   100,   101,    97,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      99,    98,   111,     0,     0,    24,    64,     0,    27,     0,
      90,   136,    67,    69,    68,    71,    72,    70,    78,    80,
      81,    89,     0,     0,     0,     0,     0,   106,     0,   142,
      61,     0,    33,     0,    32,    31,    40,    39,    38,    37,
       0,    60,    59,    55,    57,    54,    53,    58,    56,    47,
      48,    49,    50,    52,    51,    17,    19,    25,     0,    66,
      28,    73,     0,     0,     0,   136,     0,    83,     0,     0,
     137,     0,     0,     0,   126,     0,   136,     0,     0,   140,
       0,   141,    14,     0,    65,     0,    74,    77,     0,    82,
       0,     0,     0,     0,     0,     0,     0,   114,   115,   117,
       0,     0,     0,     0,   112,    30,    13,    16,     0,   137,
       0,    86,     0,    79,     0,   124,    78,     0,     0,     0,
       0,     0,     0,   135,   136,     0,     0,     0,     0,    15,
       0,    94,    95,     0,    88,     0,     0,     0,   120,     0,
       0,     0,     0,   130,     0,   116,   133,     0,    91,    92,
       0,    12,     0,    84,     0,    87,    76,    75,   125,     0,
       0,     0,     0,     0,   129,   127,   131,   132,     0,     0,
      10,    11,     0,    96,   122,   121,     0,     0,     0,   134,
      93,    85,     0,   118,   119,     0,   123,   128
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    28,    29,    30,   182,   183,    56,   140,    57,    58,
     134,    34,    35,   107,   118,   119,   120,   121,   211,   212,
      36,    37,   269,   254,   197,    39,   198,   199,    41,   260,
     282,   196,   243,   244,   177,    42,   161,    49,    62,    50,
     204,   206,   162,    60
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -184
static const yytype_int16 yypact[] =
{
     732,  -184,  -184,  -184,  -184,  -184,    30,    37,    12,    20,
    -184,  -184,  -184,  -184,   -43,    42,    45,  -184,    73,  -184,
      44,    49,   900,   119,   119,   900,   900,    33,   140,  -184,
    -184,   183,    51,    54,    56,   269,    66,  -184,  -184,   875,
    -184,  -184,  -184,   900,   900,  -184,   767,  -184,   787,   703,
      81,    48,    76,   900,   900,  -184,   368,  -184,  -184,   418,
      85,  -184,   136,  -184,  -184,   107,   -29,   -29,   107,   922,
    -184,   875,  -184,  -184,    58,   900,   900,   900,   900,  -184,
    -184,  -184,  -184,  -184,  -184,  -184,   900,   900,   900,   900,
     900,   900,   900,   900,   900,   900,   900,   900,   900,   900,
    -184,  -184,  -184,  1122,  1140,  -184,  1194,   356,  -184,   382,
    -184,   105,  -184,  -184,  -184,  -184,  -184,  -184,   -17,  -184,
    -184,   196,   146,   900,     7,   942,   962,  -184,   162,  -184,
    -184,   676,  -184,   900,  -184,  1194,   613,   613,   613,   613,
     164,   613,   613,   207,   207,   207,   207,   207,   207,   125,
     125,   107,   107,   107,   107,  -184,  -184,  -184,   900,  1194,
    -184,  -184,   120,   757,   900,  -184,  1243,  -184,   126,   982,
    -184,    69,   113,   122,  -184,   732,  -184,   -33,   130,  -184,
     551,  -184,  -184,   493,  1194,    31,  -184,  1194,  1158,  -184,
     900,   732,   927,   900,   900,   900,   128,  -184,  -184,   168,
     161,    81,   192,    32,  -184,  -184,   -16,  -184,   164,  -184,
      81,  -184,    21,   179,  1002,  -184,    89,  1022,   547,   809,
     118,   732,   114,  -184,  -184,    81,    24,   900,   822,  -184,
    1243,  -184,  -184,   198,  -184,   757,   152,   900,  -184,   900,
     900,   165,   159,  -184,   620,  -184,  -184,   191,  -184,  -184,
    1176,  -184,   563,   195,    81,  -184,  -184,  1194,  -184,  1042,
     732,  1062,  1082,   900,  -184,  -184,   875,  -184,   114,    81,
    -184,  -184,   900,  -184,  -184,  -184,   732,   732,  1102,  -184,
    -184,  1194,   732,  -184,  -184,   166,  -184,  -184
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -184,  -184,   201,   -44,    22,  -184,     0,  -184,     2,     4,
    -157,   109,   249,   -41,  -183,  -184,  -106,  -184,   -99,    35,
     110,  -184,  -184,  -184,     1,   -63,     3,  -179,  -184,  -184,
    -184,  -184,    -9,  -184,  -184,  -184,  -166,  -124,  -184,     6,
    -184,  -184,   111,  -184
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -143
static const yytype_int16 yytable[] =
{
      31,    38,    32,    40,    33,   117,   186,   109,   131,   216,
     200,    53,   215,    45,   163,   167,     6,     7,    51,    52,
     170,    47,   201,    66,    67,    81,   202,    82,    31,    38,
      32,    40,    33,    71,   209,   164,   227,   209,   228,    31,
     102,    32,   245,    33,   209,   209,   192,   253,     1,     2,
       3,     4,     5,   122,     6,     7,     8,     9,   247,   132,
     189,     1,     2,     3,     4,     5,    46,     6,     7,     8,
       9,    31,    38,    32,    48,    33,   232,   117,   256,   249,
     233,   275,    43,   233,    61,   230,   210,   225,    22,    44,
     193,    70,   180,    23,    24,    25,    54,   283,   284,    26,
      74,    22,   123,   286,    63,    55,    23,    24,    25,    64,
     237,    83,    26,   234,    84,   133,    85,     1,     2,     3,
     163,     5,   117,    81,   171,    82,   101,   234,     6,     7,
     124,    31,   102,    32,   255,    33,   241,   242,    27,   128,
      72,   164,   129,     1,     2,     3,     4,     5,   117,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    99,  -142,
      15,   168,    16,    17,    18,    19,    20,    21,   176,    96,
      97,    98,   181,   194,   185,    31,    99,    32,   246,    33,
     190,   266,   195,    22,   203,   220,   117,   252,    23,    24,
      25,    31,   222,    32,    26,    33,   221,    27,   224,     1,
       2,     3,   165,     5,   223,   112,   113,   114,   115,   116,
     235,   209,   258,   231,    74,    75,    76,    77,    78,   263,
     264,    31,   268,    32,   279,    33,   272,   287,   248,    73,
     229,    79,    80,   172,   173,   267,     0,    81,   226,    82,
     178,     0,     0,     0,    31,    38,    32,     0,    33,    94,
      95,    96,    97,    98,     0,   166,     0,   273,    99,     0,
      31,     0,    32,     0,    33,    59,    31,   102,    32,     0,
      33,    65,   280,     0,    68,    69,    31,    31,    32,    32,
      33,    33,    31,     0,    32,     0,    33,     0,     0,     0,
       0,     0,   103,   104,     0,   106,     0,   106,    86,    87,
       0,     0,   125,   126,     0,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,     0,     0,     0,     0,
      99,     0,     0,   135,   136,   137,   138,   139,     0,   100,
       0,     0,     0,     0,     0,   141,   142,   143,   144,   145,
     146,   147,   148,   149,   150,   151,   152,   153,   154,     0,
       0,     0,     0,     0,     0,     0,   159,     0,   159,     1,
       2,     3,     4,     5,     0,     6,     7,     8,     9,     0,
       0,     0,   169,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     1,     2,     3,     4,     5,
       0,     6,     7,     8,     9,     0,     0,     0,     0,    22,
      75,    76,    77,    78,    23,    24,    25,   184,     0,     0,
      26,   157,   187,   188,     0,   158,    79,    80,     0,     0,
       0,     0,    81,     0,    82,    22,     0,     0,     0,   159,
      23,    24,    25,     0,     0,     0,    26,   160,     0,   214,
       0,   158,   217,   218,   219,     0,     0,    86,    87,     0,
       0,     0,     0,     0,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,     0,     0,     0,     0,    99,
       0,     0,     0,     0,     0,     0,   250,   106,   127,     0,
       0,     0,     0,     0,   257,     0,   259,     0,   261,   262,
       0,     0,     0,     0,   207,     0,   -22,   -22,   -22,   -22,
     -22,   159,   -22,   -22,   -22,   -22,   -22,     0,     0,     0,
       0,     0,   278,     0,   -22,     0,     0,     0,     0,     0,
       0,   281,   -22,   -22,   -22,   -22,   -22,   -22,   -22,   -22,
     -22,   -22,   -22,   -22,   -22,   -22,   -22,   -22,   -22,   -22,
       0,   -22,   -22,   -22,   -22,     0,   -22,   -22,   -22,   208,
       0,   -22,   -22,   -22,     1,     2,     3,     4,     5,     0,
       6,     7,     8,     9,     0,     0,     1,     2,     3,     4,
       5,     0,     6,     7,     8,     9,    86,    87,     0,     0,
       0,     0,     0,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    22,     0,     0,     0,    99,    23,
      24,    25,     0,     0,     0,    26,    22,   239,     0,   205,
     158,    23,    24,    25,     0,     0,     0,    26,   271,     0,
       0,     0,   158,     1,     2,     3,     4,     5,     0,     6,
       7,     8,     9,    10,    11,    12,    13,    14,   241,   242,
      15,     0,    16,    17,    18,    19,    20,    21,     0,    88,
      89,    90,    91,    92,    93,    94,    95,    96,    97,    98,
       0,     0,     0,    22,    99,     0,     0,     0,    23,    24,
      25,     0,     0,     0,    26,     0,     0,     0,   265,     1,
       2,     3,     4,     5,     0,     6,     7,     8,     9,    10,
      11,    12,    13,    14,     0,     0,    15,     0,    16,    17,
      18,    19,    20,    21,   110,     0,     1,     2,     3,   111,
       5,     0,   112,   113,   114,   115,   116,     0,     0,    22,
       0,     0,     0,     0,    23,    24,    25,     0,     0,     0,
      26,     0,     0,     0,   179,     1,     2,     3,     4,     5,
       0,     6,     7,     8,     9,    10,    11,    12,    13,    14,
       0,     0,    15,     0,    16,    17,    18,    19,    20,    21,
       1,     2,     3,     4,     5,     0,     6,     7,     8,     9,
       1,     2,     3,     4,     5,    22,     6,     7,     8,     9,
      23,    24,    25,     0,     0,     0,    26,     0,     0,    27,
       1,     2,     3,     4,     5,     0,     6,     7,     8,     9,
      22,     0,     0,     0,     0,    23,    24,    25,     0,     0,
      22,    26,     0,     0,   133,    23,    24,    25,     0,     0,
       0,    26,   105,     0,     0,     1,     2,     3,     4,     5,
      22,     6,     7,     8,     9,    23,    24,    25,    86,    87,
       0,    26,   108,     0,     0,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,     0,     0,     0,     0,
      99,     0,     0,     0,     0,    22,     0,     0,     0,   240,
      23,    24,    25,     0,     0,     0,    26,   251,     1,     2,
       3,     4,     5,     0,     6,     7,     8,     9,    10,    11,
      12,    13,    14,     0,     0,    15,     0,    16,    17,    18,
      19,    20,    21,     1,     2,     3,     4,     5,     0,     6,
       7,     8,     9,     0,     0,     0,     0,     0,    22,     0,
       0,     0,     0,    23,    24,    25,     0,     0,   110,    26,
       1,     2,     3,   165,     5,     0,   112,   113,   114,   115,
     116,     0,     0,    22,     0,     0,     0,     0,    23,    24,
      25,    86,    87,     0,    26,     0,     0,     0,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,     0,
       0,    86,    87,    99,     0,     0,     0,   130,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,     0,
       0,    86,    87,    99,     0,     0,     0,   174,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,     0,
       0,    86,    87,    99,     0,     0,     0,   175,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,     0,
       0,    86,    87,    99,     0,     0,     0,   191,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,     0,
       0,    86,    87,    99,     0,     0,     0,   236,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,     0,
       0,    86,    87,    99,     0,     0,     0,   238,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,     0,
       0,    86,    87,    99,     0,     0,     0,   274,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,     0,
       0,    86,    87,    99,     0,     0,     0,   276,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,     0,
       0,    86,    87,    99,     0,     0,     0,   277,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,     0,
       0,    86,    87,    99,     0,     0,     0,   285,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    86,
      87,     0,     0,    99,     0,   155,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    86,    87,     0,
       0,    99,     0,   156,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    86,    87,     0,     0,    99,
       0,   213,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    86,    87,     0,     0,    99,     0,   270,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    97,
      98,     0,     0,     0,     0,    99,     1,     2,     3,   165,
       5,     0,   112,   113,   114,   115,   116
};

static const yytype_int16 yycheck[] =
{
       0,     0,     0,     0,     0,    49,   163,    48,    71,   192,
     176,    54,   191,     1,    31,   121,     9,    10,    12,    13,
      13,     1,    55,    23,    24,    54,    59,    56,    28,    28,
      28,    28,    28,    27,    13,    52,    52,    13,    54,    39,
      39,    39,   221,    39,    13,    13,   170,   230,     3,     4,
       5,     6,     7,    50,     9,    10,    11,    12,   224,     1,
     166,     3,     4,     5,     6,     7,    54,     9,    10,    11,
      12,    71,    71,    71,    54,    71,    55,   121,   235,    55,
      59,   260,    52,    59,    11,   209,    55,    55,    43,    52,
      21,    58,   133,    48,    49,    50,    54,   276,   277,    54,
      31,    43,    54,   282,    60,    60,    48,    49,    50,    60,
      21,    60,    54,   212,    60,    57,    60,     3,     4,     5,
      31,     7,   166,    54,   124,    56,    60,   226,     9,    10,
      54,   131,   131,   131,   233,   131,    18,    19,    57,    54,
       0,    52,     6,     3,     4,     5,     6,     7,   192,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    51,    54,
      20,    15,    22,    23,    24,    25,    26,    27,     6,    44,
      45,    46,     8,    60,    54,   175,    51,   175,   222,   175,
      54,   244,    60,    43,    54,    57,   230,   228,    48,    49,
      50,   191,    31,   191,    54,   191,    28,    57,     6,     3,
       4,     5,     6,     7,   201,     9,    10,    11,    12,    13,
      31,    13,    60,   210,    31,    32,    33,    34,    35,    54,
      61,   221,    31,   221,   268,   221,    31,    61,   225,    28,
     208,    48,    49,   124,   124,   244,    -1,    54,   203,    56,
     129,    -1,    -1,    -1,   244,   244,   244,    -1,   244,    42,
      43,    44,    45,    46,    -1,    59,    -1,   254,    51,    -1,
     260,    -1,   260,    -1,   260,    16,   266,   266,   266,    -1,
     266,    22,   269,    -1,    25,    26,   276,   277,   276,   277,
     276,   277,   282,    -1,   282,    -1,   282,    -1,    -1,    -1,
      -1,    -1,    43,    44,    -1,    46,    -1,    48,    29,    30,
      -1,    -1,    53,    54,    -1,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    -1,    74,    75,    76,    77,    78,    -1,    60,
      -1,    -1,    -1,    -1,    -1,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,   109,     3,
       4,     5,     6,     7,    -1,     9,    10,    11,    12,    -1,
      -1,    -1,   123,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   133,    -1,    -1,     3,     4,     5,     6,     7,
      -1,     9,    10,    11,    12,    -1,    -1,    -1,    -1,    43,
      32,    33,    34,    35,    48,    49,    50,   158,    -1,    -1,
      54,    55,   163,   164,    -1,    59,    48,    49,    -1,    -1,
      -1,    -1,    54,    -1,    56,    43,    -1,    -1,    -1,   180,
      48,    49,    50,    -1,    -1,    -1,    54,    55,    -1,   190,
      -1,    59,   193,   194,   195,    -1,    -1,    29,    30,    -1,
      -1,    -1,    -1,    -1,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    -1,    -1,    -1,    -1,    -1,   227,   228,    60,    -1,
      -1,    -1,    -1,    -1,   235,    -1,   237,    -1,   239,   240,
      -1,    -1,    -1,    -1,     1,    -1,     3,     4,     5,     6,
       7,   252,     9,    10,    11,    12,    13,    -1,    -1,    -1,
      -1,    -1,   263,    -1,    21,    -1,    -1,    -1,    -1,    -1,
      -1,   272,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    48,    49,    50,    51,    -1,    53,    54,    55,    56,
      -1,    58,    59,    60,     3,     4,     5,     6,     7,    -1,
       9,    10,    11,    12,    -1,    -1,     3,     4,     5,     6,
       7,    -1,     9,    10,    11,    12,    29,    30,    -1,    -1,
      -1,    -1,    -1,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    43,    -1,    -1,    -1,    51,    48,
      49,    50,    -1,    -1,    -1,    54,    43,    60,    -1,    58,
      59,    48,    49,    50,    -1,    -1,    -1,    54,    55,    -1,
      -1,    -1,    59,     3,     4,     5,     6,     7,    -1,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    -1,    22,    23,    24,    25,    26,    27,    -1,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    43,    51,    -1,    -1,    -1,    48,    49,
      50,    -1,    -1,    -1,    54,    -1,    -1,    -1,    58,     3,
       4,     5,     6,     7,    -1,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,     1,    -1,     3,     4,     5,     6,
       7,    -1,     9,    10,    11,    12,    13,    -1,    -1,    43,
      -1,    -1,    -1,    -1,    48,    49,    50,    -1,    -1,    -1,
      54,    -1,    -1,    -1,    58,     3,     4,     5,     6,     7,
      -1,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
       3,     4,     5,     6,     7,    -1,     9,    10,    11,    12,
       3,     4,     5,     6,     7,    43,     9,    10,    11,    12,
      48,    49,    50,    -1,    -1,    -1,    54,    -1,    -1,    57,
       3,     4,     5,     6,     7,    -1,     9,    10,    11,    12,
      43,    -1,    -1,    -1,    -1,    48,    49,    50,    -1,    -1,
      43,    54,    -1,    -1,    57,    48,    49,    50,    -1,    -1,
      -1,    54,    55,    -1,    -1,     3,     4,     5,     6,     7,
      43,     9,    10,    11,    12,    48,    49,    50,    29,    30,
      -1,    54,    55,    -1,    -1,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    -1,    -1,    -1,    43,    -1,    -1,    -1,    60,
      48,    49,    50,    -1,    -1,    -1,    54,    55,     3,     4,
       5,     6,     7,    -1,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,     3,     4,     5,     6,     7,    -1,     9,
      10,    11,    12,    -1,    -1,    -1,    -1,    -1,    43,    -1,
      -1,    -1,    -1,    48,    49,    50,    -1,    -1,     1,    54,
       3,     4,     5,     6,     7,    -1,     9,    10,    11,    12,
      13,    -1,    -1,    43,    -1,    -1,    -1,    -1,    48,    49,
      50,    29,    30,    -1,    54,    -1,    -1,    -1,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    29,    30,    51,    -1,    -1,    -1,    55,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    29,    30,    51,    -1,    -1,    -1,    55,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    29,    30,    51,    -1,    -1,    -1,    55,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    29,    30,    51,    -1,    -1,    -1,    55,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    29,    30,    51,    -1,    -1,    -1,    55,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    29,    30,    51,    -1,    -1,    -1,    55,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    29,    30,    51,    -1,    -1,    -1,    55,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    29,    30,    51,    -1,    -1,    -1,    55,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    29,    30,    51,    -1,    -1,    -1,    55,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    29,    30,    51,    -1,    -1,    -1,    55,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    29,
      30,    -1,    -1,    51,    -1,    53,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    29,    30,    -1,
      -1,    51,    -1,    53,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    29,    30,    -1,    -1,    51,
      -1,    53,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    29,    30,    -1,    -1,    51,    -1,    53,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,     3,     4,     5,     6,
       7,    -1,     9,    10,    11,    12,    13
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     5,     6,     7,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    20,    22,    23,    24,    25,
      26,    27,    43,    48,    49,    50,    54,    57,    63,    64,
      65,    68,    70,    71,    73,    74,    82,    83,    86,    87,
      88,    90,    97,    52,    52,     1,    54,     1,    54,    99,
     101,   101,   101,    54,    54,    60,    68,    70,    71,    74,
     105,    11,   100,    60,    60,    74,    68,    68,    74,    74,
      58,   101,     0,    64,    31,    32,    33,    34,    35,    48,
      49,    54,    56,    60,    60,    60,    29,    30,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    51,
      60,    60,    86,    74,    74,    55,    74,    75,    55,    75,
       1,     6,     9,    10,    11,    12,    13,    65,    76,    77,
      78,    79,    88,    54,    54,    74,    74,    60,    54,     6,
      55,    87,     1,    57,    72,    74,    74,    74,    74,    74,
      69,    74,    74,    74,    74,    74,    74,    74,    74,    74,
      74,    74,    74,    74,    74,    53,    53,    55,    59,    74,
      55,    98,   104,    31,    52,     6,    59,    78,    15,    74,
      13,    68,    73,    82,    55,    55,     6,    96,   104,    58,
      75,     8,    66,    67,    74,    54,    72,    74,    74,    78,
      54,    55,    99,    21,    60,    60,    93,    86,    88,    89,
      98,    55,    59,    54,   102,    58,   103,     1,    56,    13,
      55,    80,    81,    53,    74,    89,    76,    74,    74,    74,
      57,    28,    31,    88,     6,    55,    81,    52,    54,    66,
      99,    88,    55,    59,    80,    31,    55,    21,    55,    60,
      60,    18,    19,    94,    95,    89,    65,    98,    88,    55,
      74,    55,    75,    76,    85,    80,    72,    74,    60,    74,
      91,    74,    74,    54,    61,    58,    87,    94,    31,    84,
      53,    55,    31,    88,    55,    89,    55,    55,    74,    65,
      88,    74,    92,    89,    89,    55,    89,    61
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
#line 76 "grammar.yy"
    { }
    break;

  case 3:

/* Line 1455 of yacc.c  */
#line 77 "grammar.yy"
    { }
    break;

  case 4:

/* Line 1455 of yacc.c  */
#line 82 "grammar.yy"
    {
		if (((yyvsp[(1) - (1)].node) != NULL) && (!cmdparser.addStatement((yyvsp[(1) - (1)].node)))) YYABORT;
		}
    break;

  case 5:

/* Line 1455 of yacc.c  */
#line 85 "grammar.yy"
    {
		if (((yyvsp[(1) - (1)].node) != NULL) && (!cmdparser.addStatement((yyvsp[(1) - (1)].node)))) YYABORT;
		}
    break;

  case 6:

/* Line 1455 of yacc.c  */
#line 95 "grammar.yy"
    { (yyval.node) = cmdparser.addConstant((yyvsp[(1) - (1)].intconst)); }
    break;

  case 7:

/* Line 1455 of yacc.c  */
#line 96 "grammar.yy"
    { (yyval.node) = cmdparser.addConstant((yyvsp[(1) - (1)].doubleconst)); }
    break;

  case 8:

/* Line 1455 of yacc.c  */
#line 97 "grammar.yy"
    { (yyval.node) = cmdparser.addConstant((yyvsp[(1) - (1)].name)->get()); }
    break;

  case 9:

/* Line 1455 of yacc.c  */
#line 98 "grammar.yy"
    { (yyval.node) = cmdparser.addElementConstant((yyvsp[(1) - (1)].intconst)); }
    break;

  case 10:

/* Line 1455 of yacc.c  */
#line 107 "grammar.yy"
    {
		if (!cmdparser.expandPath(stepNameStack.last(), (yyvsp[(4) - (5)].node))) YYABORT;
		stepNameStack.removeLast();
		}
    break;

  case 11:

/* Line 1455 of yacc.c  */
#line 111 "grammar.yy"
    {
		if (!cmdparser.expandPath(stepNameStack.last(), NULL, (yyvsp[(4) - (5)].node))) YYABORT;
		stepNameStack.removeLast();
		}
    break;

  case 12:

/* Line 1455 of yacc.c  */
#line 115 "grammar.yy"
    {
		if (!cmdparser.expandPath(stepNameStack.last(), NULL, NULL)) YYABORT;
		stepNameStack.removeLast();
		}
    break;

  case 13:

/* Line 1455 of yacc.c  */
#line 119 "grammar.yy"
    {
		if (!cmdparser.expandPath((yyvsp[(1) - (2)].name))) YYABORT;
		stepNameStack.removeLast();
		}
    break;

  case 14:

/* Line 1455 of yacc.c  */
#line 127 "grammar.yy"
    { }
    break;

  case 15:

/* Line 1455 of yacc.c  */
#line 128 "grammar.yy"
    { }
    break;

  case 16:

/* Line 1455 of yacc.c  */
#line 129 "grammar.yy"
    { msg.print("Error formulating path.\n"); YYABORT; }
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 134 "grammar.yy"
    {
		(yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (4)].variable),(yyvsp[(3) - (4)].node));
		if ((yyval.node) == NULL) { msg.print("Error in variable expression (code 1)\n"); YYABORT; }
		}
    break;

  case 18:

/* Line 1455 of yacc.c  */
#line 138 "grammar.yy"
    {
		(yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (1)].variable));
		if ((yyval.node) == NULL) { msg.print("Error in variable expression (code 2)\n"); YYABORT; }
		}
    break;

  case 19:

/* Line 1455 of yacc.c  */
#line 142 "grammar.yy"
    {
		(yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (4)].variable),(yyvsp[(3) - (4)].node));
		if ((yyval.node) == NULL) { msg.print("Error in variable expression (code 3)\n"); YYABORT; }
		}
    break;

  case 20:

/* Line 1455 of yacc.c  */
#line 146 "grammar.yy"
    {
		(yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (1)].variable));
		if ((yyval.node) == NULL) { msg.print("Error in variable expression (code 4)\n"); YYABORT; }
		}
    break;

  case 21:

/* Line 1455 of yacc.c  */
#line 150 "grammar.yy"
    {
		cmdparser.createPath((yyvsp[(1) - (2)].node));
		}
    break;

  case 22:

/* Line 1455 of yacc.c  */
#line 152 "grammar.yy"
    {
		(yyval.node) = cmdparser.finalisePath();
		}
    break;

  case 23:

/* Line 1455 of yacc.c  */
#line 155 "grammar.yy"
    {
		msg.print("Can't use a variable as a function. Did you mean '[' instead?\n"); (yyval.node) = NULL;
		}
    break;

  case 24:

/* Line 1455 of yacc.c  */
#line 166 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction( (Command::Function) (yyvsp[(1) - (3)].functionId));
		if ((yyval.node) == NULL) YYABORT;
		msg.print(Messenger::Parse,"PARSER : function : function '%s'\n", commands.data[(Command::Function) (yyvsp[(1) - (3)].functionId)].keyword);
		}
    break;

  case 25:

/* Line 1455 of yacc.c  */
#line 171 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunctionWithArglist( (Command::Function) (yyvsp[(1) - (4)].functionId),(yyvsp[(3) - (4)].node));
		if ((yyval.node) == NULL) YYABORT;
		msg.print(Messenger::Parse,"PARSER : function : function '%s' with exprlist\n", commands.data[(Command::Function) (yyvsp[(1) - (4)].functionId)].keyword);
		}
    break;

  case 26:

/* Line 1455 of yacc.c  */
#line 176 "grammar.yy"
    {
		msg.print("Error: Missing brackets after function call?\n");
		YYABORT;
		}
    break;

  case 27:

/* Line 1455 of yacc.c  */
#line 184 "grammar.yy"
    {
		(yyval.node) = cmdparser.addUserFunction((yyvsp[(1) - (3)].tree));
		if ((yyval.node) == NULL) YYABORT;
		msg.print(Messenger::Parse,"PARSER : userfunction : function '%s'\n", (yyvsp[(1) - (3)].tree)->name());
		}
    break;

  case 28:

/* Line 1455 of yacc.c  */
#line 189 "grammar.yy"
    {
		(yyval.node) = cmdparser.addUserFunction((yyvsp[(1) - (4)].tree),(yyvsp[(3) - (4)].node));
		if ((yyval.node) == NULL) YYABORT;
		msg.print(Messenger::Parse,"PARSER : userfunction : function '%s' with expressionlist\n", (yyvsp[(1) - (4)].tree)->name());
		}
    break;

  case 29:

/* Line 1455 of yacc.c  */
#line 194 "grammar.yy"
    {
		msg.print("Error: Missing brackets after function call?\n");
		YYABORT;
		}
    break;

  case 30:

/* Line 1455 of yacc.c  */
#line 206 "grammar.yy"
    {
		(yyval.node) = cmdparser.addArrayConstant((yyvsp[(2) - (3)].node));
		if ((yyval.node) == NULL) YYABORT;
		}
    break;

  case 31:

/* Line 1455 of yacc.c  */
#line 217 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignment,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 32:

/* Line 1455 of yacc.c  */
#line 218 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignment,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 33:

/* Line 1455 of yacc.c  */
#line 219 "grammar.yy"
    { msg.print("Mangled expression used in assignment.\n"); YYABORT; }
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 224 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 35:

/* Line 1455 of yacc.c  */
#line 225 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 36:

/* Line 1455 of yacc.c  */
#line 226 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 37:

/* Line 1455 of yacc.c  */
#line 227 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignmentPlus,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 228 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignmentSubtract,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 229 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignmentMultiply,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 40:

/* Line 1455 of yacc.c  */
#line 230 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignmentDivide,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 41:

/* Line 1455 of yacc.c  */
#line 231 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorNegate, (yyvsp[(2) - (2)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 232 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPostfixIncrease, (yyvsp[(1) - (2)].node));  if ((yyval.node) == NULL) YYABORT; }
    break;

  case 43:

/* Line 1455 of yacc.c  */
#line 233 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPostfixDecrease, (yyvsp[(1) - (2)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 44:

/* Line 1455 of yacc.c  */
#line 234 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPrefixIncrease, (yyvsp[(2) - (2)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 45:

/* Line 1455 of yacc.c  */
#line 235 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPrefixDecrease, (yyvsp[(2) - (2)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 236 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 237 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAdd, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 238 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorSubtract, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 49:

/* Line 1455 of yacc.c  */
#line 239 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorMultiply, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 50:

/* Line 1455 of yacc.c  */
#line 240 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorDivide, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 51:

/* Line 1455 of yacc.c  */
#line 241 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPower, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 52:

/* Line 1455 of yacc.c  */
#line 242 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorModulus, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 53:

/* Line 1455 of yacc.c  */
#line 243 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 54:

/* Line 1455 of yacc.c  */
#line 244 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorNotEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 245 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorGreaterThan, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 56:

/* Line 1455 of yacc.c  */
#line 246 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorGreaterThanEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 57:

/* Line 1455 of yacc.c  */
#line 247 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorLessThan, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 58:

/* Line 1455 of yacc.c  */
#line 248 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorLessThanEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 59:

/* Line 1455 of yacc.c  */
#line 249 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAnd, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 60:

/* Line 1455 of yacc.c  */
#line 250 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorOr, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 61:

/* Line 1455 of yacc.c  */
#line 251 "grammar.yy"
    { (yyval.node) = (yyvsp[(2) - (3)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 62:

/* Line 1455 of yacc.c  */
#line 252 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorNot, (yyvsp[(2) - (2)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 63:

/* Line 1455 of yacc.c  */
#line 253 "grammar.yy"
    { msg.print("Error: '%s' has not been declared as a function or a variable.\n", yylval.name->get()); YYABORT; }
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 258 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		if ((yyval.node) == NULL) YYABORT;
		}
    break;

  case 65:

/* Line 1455 of yacc.c  */
#line 262 "grammar.yy"
    {
		(yyval.node) = Tree::joinArguments((yyvsp[(3) - (3)].node),(yyvsp[(1) - (3)].node));
		}
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 265 "grammar.yy"
    {
		msg.print("Error: Missing comma between items.\n");
		YYABORT;
		}
    break;

  case 67:

/* Line 1455 of yacc.c  */
#line 277 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : variablename : existing var '%s'\n", tokenName.get());
		tokenName = yylval.variable->name();
		(yyval.name) = &tokenName;
		}
    break;

  case 68:

/* Line 1455 of yacc.c  */
#line 282 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : variablename : existing built-in function '%s'\n", tokenName.get());
		tokenName = Command::data[yylval.functionId].keyword;
		(yyval.name) = &tokenName;
		}
    break;

  case 69:

/* Line 1455 of yacc.c  */
#line 287 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : variablename : existing local var '%s'\n", tokenName.get());
		msg.print("Error: Existing variable in local scope cannot be redeclared.\n");
		YYABORT;
		}
    break;

  case 70:

/* Line 1455 of yacc.c  */
#line 292 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : variablename : constant '%s'\n", tokenName.get());
		msg.print("Error: Constant value found in declaration.\n");
		YYABORT;
		}
    break;

  case 71:

/* Line 1455 of yacc.c  */
#line 297 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : variablename : existing user function '%s'\n", tokenName.get());
		msg.print("Error: Existing user-defined function name cannot be redeclared.\n");
		YYABORT;
		}
    break;

  case 72:

/* Line 1455 of yacc.c  */
#line 302 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : variablename : variable type-name '%s'\n", VTypes::dataType( yylval.vtype));
		msg.print("Error: Type-name used in variable declaration.\n");
		YYABORT;
		}
    break;

  case 73:

/* Line 1455 of yacc.c  */
#line 307 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : variablename : new token '%s'\n", tokenName.get());
		if (declaredType == VTypes::NoData) { msg.print("Token '%s' is undeclared.\n", tokenName.get()); YYABORT; }
		(yyval.name) = (yyvsp[(1) - (2)].name);
		}
    break;

  case 74:

/* Line 1455 of yacc.c  */
#line 316 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : assignedvariablename : var '%s' with array assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addVariable(declaredType, &tokenName, (yyvsp[(3) - (3)].node));
		}
    break;

  case 75:

/* Line 1455 of yacc.c  */
#line 320 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : assignedvariablename : array var '%s' with expr assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addArrayVariable(declaredType, &tokenName,(yyvsp[(3) - (6)].node),(yyvsp[(6) - (6)].node));
		}
    break;

  case 76:

/* Line 1455 of yacc.c  */
#line 324 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : assignedvariablename : array var '%s' with array assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addArrayVariable(declaredType, &tokenName,(yyvsp[(3) - (6)].node),(yyvsp[(6) - (6)].node));
		}
    break;

  case 77:

/* Line 1455 of yacc.c  */
#line 328 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : assignedvariablename : var '%s' with expr assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addVariable(declaredType, &tokenName, (yyvsp[(3) - (3)].node));
		}
    break;

  case 78:

/* Line 1455 of yacc.c  */
#line 336 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : assignedvariablename : var '%s'\n", tokenName.get());
		(yyval.node) = cmdparser.addVariable(declaredType, &tokenName);
		}
    break;

  case 79:

/* Line 1455 of yacc.c  */
#line 340 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : assignedvariablename : array var '%s'\n", tokenName.get());
		(yyval.node) = cmdparser.addArrayVariable(declaredType, &tokenName, (yyvsp[(3) - (4)].node));
		}
    break;

  case 80:

/* Line 1455 of yacc.c  */
#line 344 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 81:

/* Line 1455 of yacc.c  */
#line 351 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 82:

/* Line 1455 of yacc.c  */
#line 354 "grammar.yy"
    {
		(yyval.node) = Tree::joinArguments((yyvsp[(3) - (3)].node),(yyvsp[(1) - (3)].node));
		}
    break;

  case 83:

/* Line 1455 of yacc.c  */
#line 357 "grammar.yy"
    {
		msg.print("Error: Missing comma between declarations?\n");
		YYABORT;
		}
    break;

  case 84:

/* Line 1455 of yacc.c  */
#line 365 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : typedvariablelistitem : var '%s'\n", tokenName.get());
		(yyval.node) = cmdparser.addVariable(declaredType, &tokenName);
		}
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 369 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : typedvariablelistitem : var '%s' with expr assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addVariable(declaredType, &tokenName, (yyvsp[(5) - (5)].node));
		}
    break;

  case 86:

/* Line 1455 of yacc.c  */
#line 377 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 87:

/* Line 1455 of yacc.c  */
#line 380 "grammar.yy"
    {
		(yyval.node) = Tree::joinArguments((yyvsp[(3) - (3)].node),(yyvsp[(1) - (3)].node));
		}
    break;

  case 88:

/* Line 1455 of yacc.c  */
#line 383 "grammar.yy"
    {
		msg.print("Error: Missing comma between declarations?\n");
		YYABORT;
		}
    break;

  case 89:

/* Line 1455 of yacc.c  */
#line 391 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : declaration : standard variable declaration list\n");
		(yyval.node) = cmdparser.addDeclarations((yyvsp[(3) - (3)].node));
		declaredType = VTypes::NoData;
		}
    break;

  case 90:

/* Line 1455 of yacc.c  */
#line 396 "grammar.yy"
    {
		msg.print("Illegal use of reserved word '%s'.\n", VTypes::dataType(declaredType));
		YYABORT;
		}
    break;

  case 91:

/* Line 1455 of yacc.c  */
#line 408 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : functiondeclaration : user-defined subroutine (VOID return value, no arguments)\n");
		if (!cmdparser.addStatement((yyvsp[(7) - (7)].node))) YYABORT;
		cmdparser.popTree();
		declaredType = VTypes::NoData;
		}
    break;

  case 92:

/* Line 1455 of yacc.c  */
#line 414 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : functiondeclaration : user-defined subroutine (VOID return value, arguments)\n");
		if (!(yyvsp[(4) - (7)].tree)->addLocalFunctionArguments((yyvsp[(6) - (7)].node))) YYABORT;
		}
    break;

  case 93:

/* Line 1455 of yacc.c  */
#line 417 "grammar.yy"
    {
		if (!cmdparser.addStatement((yyvsp[(9) - (9)].node))) YYABORT;
		cmdparser.popTree();
		declaredType = VTypes::NoData;
		}
    break;

  case 94:

/* Line 1455 of yacc.c  */
#line 422 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : functiondeclaration : user-defined function (%s return value, no arguments)\n", VTypes::dataType((yyvsp[(4) - (7)].tree)->returnType()));
		if (!cmdparser.addStatement((yyvsp[(7) - (7)].node))) YYABORT;
		cmdparser.popTree();
		declaredType = VTypes::NoData;
		}
    break;

  case 95:

/* Line 1455 of yacc.c  */
#line 428 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : functiondeclaration : user-defined function (%s return value, arguments)\n", VTypes::dataType((yyvsp[(4) - (7)].tree)->returnType()));
		if (!(yyvsp[(4) - (7)].tree)->addLocalFunctionArguments((yyvsp[(6) - (7)].node))) YYABORT;
		}
    break;

  case 96:

/* Line 1455 of yacc.c  */
#line 431 "grammar.yy"
    {
		if (!cmdparser.addStatement((yyvsp[(9) - (9)].node))) YYABORT;
		cmdparser.popTree();
		declaredType = VTypes::NoData;
		}
    break;

  case 97:

/* Line 1455 of yacc.c  */
#line 444 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (2)].node);
		}
    break;

  case 98:

/* Line 1455 of yacc.c  */
#line 447 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (2)].node);
		}
    break;

  case 99:

/* Line 1455 of yacc.c  */
#line 450 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (2)].node);
		}
    break;

  case 100:

/* Line 1455 of yacc.c  */
#line 453 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (2)].node);
		}
    break;

  case 101:

/* Line 1455 of yacc.c  */
#line 456 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (2)].node);
		}
    break;

  case 102:

/* Line 1455 of yacc.c  */
#line 459 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 103:

/* Line 1455 of yacc.c  */
#line 462 "grammar.yy"
    {
		(yyval.node) = NULL;
		}
    break;

  case 104:

/* Line 1455 of yacc.c  */
#line 465 "grammar.yy"
    {
		(yyval.node) = NULL;
		}
    break;

  case 105:

/* Line 1455 of yacc.c  */
#line 468 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::Help, cmdparser.addConstant((yyvsp[(2) - (2)].functionId)));
		}
    break;

  case 106:

/* Line 1455 of yacc.c  */
#line 471 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::Return,(yyvsp[(2) - (3)].node));
		}
    break;

  case 107:

/* Line 1455 of yacc.c  */
#line 474 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::Return);
		}
    break;

  case 108:

/* Line 1455 of yacc.c  */
#line 477 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::Continue);
		}
    break;

  case 109:

/* Line 1455 of yacc.c  */
#line 480 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::Break);
		}
    break;

  case 110:

/* Line 1455 of yacc.c  */
#line 487 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 111:

/* Line 1455 of yacc.c  */
#line 490 "grammar.yy"
    {
		if ((yyvsp[(2) - (2)].node) == NULL) (yyval.node) = (yyvsp[(1) - (2)].node);
		else (yyval.node) = cmdparser.joinCommands((yyvsp[(1) - (2)].node), (yyvsp[(2) - (2)].node));
		}
    break;

  case 112:

/* Line 1455 of yacc.c  */
#line 498 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(3) - (5)].node);
		}
    break;

  case 113:

/* Line 1455 of yacc.c  */
#line 501 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::NoFunction);
		}
    break;

  case 114:

/* Line 1455 of yacc.c  */
#line 508 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 115:

/* Line 1455 of yacc.c  */
#line 511 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 116:

/* Line 1455 of yacc.c  */
#line 518 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::If,(yyvsp[(3) - (7)].node),(yyvsp[(5) - (7)].node),(yyvsp[(7) - (7)].node));
		}
    break;

  case 117:

/* Line 1455 of yacc.c  */
#line 521 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::If,(yyvsp[(3) - (5)].node),(yyvsp[(5) - (5)].node));
		}
    break;

  case 118:

/* Line 1455 of yacc.c  */
#line 524 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (10)].node), cmdparser.addFunction(Command::For, (yyvsp[(4) - (10)].node),(yyvsp[(6) - (10)].node),(yyvsp[(8) - (10)].node),(yyvsp[(10) - (10)].node))); cmdparser.popScope();
		}
    break;

  case 119:

/* Line 1455 of yacc.c  */
#line 527 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (10)].node), cmdparser.addFunction(Command::For, (yyvsp[(4) - (10)].node),(yyvsp[(6) - (10)].node),(yyvsp[(8) - (10)].node),(yyvsp[(10) - (10)].node))); cmdparser.popScope();
		}
    break;

  case 120:

/* Line 1455 of yacc.c  */
#line 530 "grammar.yy"
    {
		if ((yyvsp[(4) - (7)].node)->returnType() <= VTypes::VectorData) { msg.print("Error: For/In loop variable must be of pointer type.\n"); YYABORT; }
		if ((yyvsp[(4) - (7)].node)->returnType() != (yyvsp[(6) - (7)].node)->returnType()) { msg.print("Error: For/In loop variable is not being assigned the correct type.\n"); YYABORT; }
		}
    break;

  case 121:

/* Line 1455 of yacc.c  */
#line 533 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (9)].node), cmdparser.addFunction(Command::ForIn,(yyvsp[(4) - (9)].node),(yyvsp[(6) - (9)].node),(yyvsp[(9) - (9)].node)));
		cmdparser.popScope();
		}
    break;

  case 122:

/* Line 1455 of yacc.c  */
#line 537 "grammar.yy"
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

  case 123:

/* Line 1455 of yacc.c  */
#line 549 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (11)].node), cmdparser.addFunction(Command::ForIn,tempNode,(yyvsp[(8) - (11)].node),(yyvsp[(11) - (11)].node)));
		cmdparser.popScope();
		}
    break;

  case 124:

/* Line 1455 of yacc.c  */
#line 553 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (6)].node), cmdparser.addFunction(Command::While, (yyvsp[(4) - (6)].node),(yyvsp[(6) - (6)].node)));
		cmdparser.popScope();
		}
    break;

  case 125:

/* Line 1455 of yacc.c  */
#line 557 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (8)].node), cmdparser.addFunction(Command::DoWhile, (yyvsp[(3) - (8)].node),(yyvsp[(6) - (8)].node)));
		cmdparser.popScope();
		}
    break;

  case 126:

/* Line 1455 of yacc.c  */
#line 561 "grammar.yy"
    {
		if (((yyvsp[(3) - (4)].node)->returnType() != VTypes::IntegerData) && ((yyvsp[(3) - (4)].node)->returnType() != VTypes::StringData))
		{
			msg.print("Error: Switch value must be of integer or string type.\n");
			YYABORT;
		}
		}
    break;

  case 127:

/* Line 1455 of yacc.c  */
#line 567 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::Switch, (yyvsp[(3) - (8)].node));
		(yyval.node)->addJoinedArguments((yyvsp[(7) - (8)].node));
		}
    break;

  case 128:

/* Line 1455 of yacc.c  */
#line 575 "grammar.yy"
    {
		if (((yyvsp[(3) - (5)].node)->returnType() != VTypes::IntegerData) && ((yyvsp[(3) - (5)].node)->returnType() != VTypes::StringData))
		{
			msg.print("Error: Case value must be of integer or string type.\n");
			YYABORT;
		}
		(yyval.node) = cmdparser.addFunction(Command::Case, (yyvsp[(3) - (5)].node));
		if ((yyval.node) == NULL) { msg.print("Error: Invalid case expression.\n"); YYABORT; }
		}
    break;

  case 129:

/* Line 1455 of yacc.c  */
#line 584 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::Default);
		}
    break;

  case 130:

/* Line 1455 of yacc.c  */
#line 591 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 131:

/* Line 1455 of yacc.c  */
#line 594 "grammar.yy"
    {
		(yyval.node) = Tree::joinArguments((yyvsp[(2) - (2)].node),(yyvsp[(1) - (2)].node));
		}
    break;

  case 132:

/* Line 1455 of yacc.c  */
#line 597 "grammar.yy"
    {
		(yyval.node) = Tree::joinArguments((yyvsp[(2) - (2)].node),(yyvsp[(1) - (2)].node));
		}
    break;

  case 133:

/* Line 1455 of yacc.c  */
#line 608 "grammar.yy"
    {
		if (!cmdparser.setFilterOption(&tokenName, (yyvsp[(4) - (4)].node))) YYABORT;
		msg.print(Messenger::Parse,"PARSER : filteroptions : filter option '%s'\n", tokenName.get());
		}
    break;

  case 134:

/* Line 1455 of yacc.c  */
#line 612 "grammar.yy"
    {
		if (!cmdparser.setFilterOption(&tokenName, (yyvsp[(6) - (6)].node))) YYABORT;
		msg.print(Messenger::Parse,"PARSER : filteroptions : filter option '%s'\n", tokenName.get());
		}
    break;

  case 135:

/* Line 1455 of yacc.c  */
#line 620 "grammar.yy"
    {
		if (((yyvsp[(6) - (6)].node) != NULL) && (!cmdparser.addStatement((yyvsp[(6) - (6)].node)))) YYABORT;
		cmdparser.popTree();
		msg.print(Messenger::Parse,"PARSER : completed filter definition\n");
		}
    break;

  case 136:

/* Line 1455 of yacc.c  */
#line 632 "grammar.yy"
    { tokenName = *yylval.name; }
    break;

  case 137:

/* Line 1455 of yacc.c  */
#line 636 "grammar.yy"
    { declaredType = yylval.vtype; }
    break;

  case 138:

/* Line 1455 of yacc.c  */
#line 640 "grammar.yy"
    { declaredType = VTypes::NoData; }
    break;

  case 139:

/* Line 1455 of yacc.c  */
#line 644 "grammar.yy"
    { (yyval.node) = cmdparser.pushScope(); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 140:

/* Line 1455 of yacc.c  */
#line 648 "grammar.yy"
    { if (!cmdparser.popScope()) YYABORT; }
    break;

  case 141:

/* Line 1455 of yacc.c  */
#line 652 "grammar.yy"
    { stepNameStack.add()->set(yylval.name->get()); }
    break;

  case 142:

/* Line 1455 of yacc.c  */
#line 656 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : pushfunc : function/statement '%s'\n", yylval.name->get());
		(yyval.tree) = cmdparser.pushFunction(yylval.name->get(), declaredType);
		/*cmdparser.pushScope();*/
		}
    break;

  case 143:

/* Line 1455 of yacc.c  */
#line 664 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : pushfilter : new filter definition\n");
		cmdparser.pushFilter();
		}
    break;



/* Line 1455 of yacc.c  */
#line 3176 "grammar.cc"
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
#line 670 "grammar.yy"


void yyerror(char *s)
{
}

