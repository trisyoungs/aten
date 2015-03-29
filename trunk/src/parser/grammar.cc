/* A Bison parser, made by GNU Bison 2.7.  */

/* Bison implementation for Yacc-like parsers in C
   
      Copyright (C) 1984, 1989-1990, 2000-2012 Free Software Foundation, Inc.
   
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
#define YYBISON_VERSION "2.7"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1


/* Substitute the variable and function names.  */
#define yyparse         CommandParser_parse
#define yylex           CommandParser_lex
#define yyerror         CommandParser_error
#define yylval          CommandParser_lval
#define yychar          CommandParser_char
#define yydebug         CommandParser_debug
#define yynerrs         CommandParser_nerrs

/* Copy the first part of user declarations.  */
/* Line 371 of yacc.c  */
#line 4 "grammar.yy"


/* Includes */
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "command/commands.h"
#include "parser/parser.h"
#include "parser/tree.h"
#include <QtCore/QStringList>

/* Prototypes */
int CommandParser_lex(void);
void CommandParser_error(char *s);

ATEN_USING_NAMESPACE

/* Local Variables */
QString tokenName;
QStringList stepNameStack;
VTypes::DataType declaredType, funcType;
TreeNode* tempNode;
int globalDeclarations;
QString variableName;


/* Line 371 of yacc.c  */
#line 102 "grammar.cc"

# ifndef YY_NULL
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULL nullptr
#  else
#   define YY_NULL 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* In a future release of Bison, this section will be replaced
   by #include "y.tab.h".  */
#ifndef YY_COMMANDPARSER_GRAMMAR_HH_INCLUDED
# define YY_COMMANDPARSER_GRAMMAR_HH_INCLUDED
/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int CommandParser_debug;
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
     VARSAMESCOPE = 265,
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
     ATEN_IIF = 276,
     ATEN_IN = 277,
     ATEN_GLOBAL = 278,
     ATEN_RETURN = 279,
     FILTERBLOCK = 280,
     HELP = 281,
     ATEN_VOID = 282,
     ATEN_CONTINUE = 283,
     ATEN_BREAK = 284,
     ATEN_NEW = 285,
     ATEN_ELSE = 286,
     DEQ = 287,
     TEQ = 288,
     MEQ = 289,
     PEQ = 290,
     OR = 291,
     AND = 292,
     NEQ = 293,
     EQ = 294,
     GEQ = 295,
     LEQ = 296,
     UMINUS = 297,
     UPLUS = 298,
     MINUSMINUS = 299,
     PLUSPLUS = 300
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
#define VARSAMESCOPE 265
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
#define ATEN_IIF 276
#define ATEN_IN 277
#define ATEN_GLOBAL 278
#define ATEN_RETURN 279
#define FILTERBLOCK 280
#define HELP 281
#define ATEN_VOID 282
#define ATEN_CONTINUE 283
#define ATEN_BREAK 284
#define ATEN_NEW 285
#define ATEN_ELSE 286
#define DEQ 287
#define TEQ 288
#define MEQ 289
#define PEQ 290
#define OR 291
#define AND 292
#define NEQ 293
#define EQ 294
#define GEQ 295
#define LEQ 296
#define UMINUS 297
#define UPLUS 298
#define MINUSMINUS 299
#define PLUSPLUS 300



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{
/* Line 387 of yacc.c  */
#line 35 "grammar.yy"

	int functionId;			/* Function enum id */
	TreeNode* node;			/* node pointer */
	Variable* variable;		/* variable pointer */
	Tree* tree;			/* function (tree) pointer */
	VTypes::DataType vtype;		/* variable type for next declarations */
	int intConst;			/* integer constant value */
	double doubleConst;		/* double constant value */


/* Line 387 of yacc.c  */
#line 246 "grammar.cc"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE CommandParser_lval;

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int CommandParser_parse (void *YYPARSE_PARAM);
#else
int CommandParser_parse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int CommandParser_parse (void);
#else
int CommandParser_parse ();
#endif
#endif /* ! YYPARSE_PARAM */

#endif /* !YY_COMMANDPARSER_GRAMMAR_HH_INCLUDED  */

/* Copy the second part of user declarations.  */

/* Line 390 of yacc.c  */
#line 274 "grammar.cc"

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
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(N) (N)
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
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
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
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
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

# define YYCOPY_NEEDED 1

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

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (YYID (0))
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  76
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1369

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  67
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  46
/* YYNRULES -- Number of rules.  */
#define YYNRULES  148
/* YYNRULES -- Number of states.  */
#define YYNSTATES  300

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   300

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    52,     2,     2,     2,    51,     2,     2,
      60,    61,    49,    47,    65,    48,    62,    50,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    38,    66,
      43,    32,    44,    37,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    58,     2,    59,    55,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    63,     2,    64,     2,     2,     2,     2,
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
      36,    39,    40,    41,    42,    45,    46,    53,    54,    56,
      57
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
     203,   207,   211,   214,   220,   223,   225,   227,   231,   234,
     236,   238,   240,   242,   244,   246,   249,   253,   260,   267,
     271,   273,   278,   280,   282,   286,   289,   293,   299,   301,
     305,   308,   315,   319,   323,   331,   332,   342,   350,   351,
     361,   364,   367,   370,   373,   376,   378,   380,   382,   385,
     389,   392,   395,   398,   400,   403,   409,   412,   414,   416,
     424,   430,   441,   452,   453,   463,   464,   476,   483,   492,
     493,   502,   508,   511,   513,   516,   519,   524,   531,   538,
     539,   540,   541,   542,   543,   544,   545,   546,   547
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      68,     0,    -1,    69,    -1,    68,    69,    -1,    92,    -1,
      93,    -1,     3,    -1,     5,    -1,     7,    -1,     4,    -1,
       8,   108,    58,    79,    59,    -1,     8,   108,    60,    80,
      61,    -1,     8,   108,    60,    61,    -1,     8,   108,    -1,
      71,    -1,    72,    62,    71,    -1,    72,     1,    -1,     9,
      58,    79,    59,    -1,     9,    -1,    10,    58,    79,    59,
      -1,    10,    -1,    -1,    73,    62,    74,    72,    -1,    73,
      60,    -1,    11,    60,    61,    -1,    11,    60,    80,    61,
      -1,    11,     1,    -1,    12,    60,    61,    -1,    12,    60,
      80,    61,    -1,    12,     1,    -1,    63,    80,    64,    -1,
      73,    32,    79,    -1,    73,    32,    77,    -1,    73,    32,
       1,    -1,    70,    -1,    75,    -1,    76,    -1,    73,    36,
      79,    -1,    73,    35,    79,    -1,    73,    34,    79,    -1,
      73,    33,    79,    -1,    48,    79,    -1,    73,    57,    -1,
      73,    56,    -1,    57,    73,    -1,    56,    73,    -1,    73,
      -1,    79,    47,    79,    -1,    79,    48,    79,    -1,    79,
      49,    79,    -1,    79,    50,    79,    -1,    79,    55,    79,
      -1,    79,    51,    79,    -1,    79,    42,    79,    -1,    79,
      41,    79,    -1,    79,    44,    79,    -1,    79,    45,    79,
      -1,    79,    43,    79,    -1,    79,    46,    79,    -1,    79,
      40,    79,    -1,    79,    39,    79,    -1,    60,    79,    61,
      -1,    52,    79,    -1,    79,    37,    79,    38,    79,    -1,
      30,    13,    -1,     6,    -1,    79,    -1,    80,    65,    79,
      -1,    80,    79,    -1,     9,    -1,    11,    -1,    10,    -1,
      70,    -1,    12,    -1,    13,    -1,     6,   103,    -1,    81,
      32,    77,    -1,    81,    58,    79,    59,    32,    79,    -1,
      81,    58,    79,    59,    32,    77,    -1,    81,    32,    79,
      -1,    81,    -1,    81,    58,    79,    59,    -1,    82,    -1,
      83,    -1,    84,    65,    83,    -1,    84,    83,    -1,    13,
     104,    81,    -1,    13,   104,    81,    32,    79,    -1,    85,
      -1,    86,    65,    85,    -1,    86,    85,    -1,    23,   111,
      13,   104,    84,   112,    -1,    13,   104,    84,    -1,    13,
     104,     1,    -1,    27,   105,     6,   109,    60,    61,    93,
      -1,    -1,    27,   105,     6,   109,    60,    86,    61,    89,
      93,    -1,    13,   104,     6,   109,    60,    61,    93,    -1,
      -1,    13,   104,     6,   109,    60,    86,    61,    90,    93,
      -1,    78,    66,    -1,    87,    66,    -1,    79,    66,    -1,
      75,    66,    -1,    76,    66,    -1,    95,    -1,    88,    -1,
     102,    -1,    26,    11,    -1,    24,    79,    66,    -1,    24,
      66,    -1,    28,    66,    -1,    29,    66,    -1,    91,    -1,
      92,    91,    -1,    63,   106,    92,    64,   107,    -1,    63,
      64,    -1,    91,    -1,    93,    -1,    20,    60,    79,    61,
      94,    31,    94,    -1,    20,    60,    79,    61,    94,    -1,
      16,   106,    60,    78,    66,    79,    66,    79,    61,    94,
      -1,    16,   106,    60,    87,    66,    79,    66,    79,    61,
      94,    -1,    -1,    16,   106,    60,    73,    22,    79,    61,
      96,    94,    -1,    -1,    16,   106,    60,    13,   104,    81,
      22,    79,    61,    97,    94,    -1,    15,   106,    60,    79,
      61,    94,    -1,    14,   106,    93,    15,    60,    79,    61,
      66,    -1,    -1,    17,    60,    79,    61,    98,    63,   100,
      64,    -1,    18,    60,    79,    61,    38,    -1,    19,    38,
      -1,    99,    -1,   100,    92,    -1,   100,    99,    -1,     6,
     103,    32,    70,    -1,   101,    65,     6,   103,    32,    70,
      -1,    25,   110,    60,   101,    61,    93,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    84,    84,    85,    90,    93,   103,   104,   105,   106,
     115,   119,   123,   127,   135,   136,   137,   142,   146,   150,
     154,   158,   158,   163,   174,   179,   184,   192,   197,   202,
     214,   225,   226,   227,   232,   233,   234,   235,   236,   237,
     238,   239,   240,   241,   242,   243,   244,   245,   246,   247,
     248,   249,   250,   251,   252,   253,   254,   255,   256,   257,
     258,   259,   260,   261,   262,   263,   268,   272,   275,   287,
     294,   300,   306,   312,   318,   323,   332,   336,   340,   344,
     352,   356,   360,   367,   370,   373,   381,   385,   393,   396,
     399,   407,   412,   417,   429,   435,   435,   443,   449,   449,
     465,   468,   471,   474,   477,   480,   483,   486,   489,   492,
     495,   498,   501,   508,   511,   519,   522,   529,   532,   539,
     542,   545,   548,   551,   551,   558,   558,   574,   578,   582,
     582,   596,   605,   612,   615,   618,   629,   633,   641,   653,
     657,   661,   665,   669,   673,   677,   685,   692,   696
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "INTCONST", "ELEMENTCONST",
  "DOUBLECONST", "NEWTOKEN", "CHARCONST", "STEPTOKEN", "VAR",
  "VARSAMESCOPE", "FUNCCALL", "USERFUNCCALL", "VTYPE", "ATEN_DO",
  "ATEN_WHILE", "ATEN_FOR", "ATEN_SWITCH", "ATEN_CASE", "ATEN_DEFAULT",
  "ATEN_IF", "ATEN_IIF", "ATEN_IN", "ATEN_GLOBAL", "ATEN_RETURN",
  "FILTERBLOCK", "HELP", "ATEN_VOID", "ATEN_CONTINUE", "ATEN_BREAK",
  "ATEN_NEW", "ATEN_ELSE", "'='", "DEQ", "TEQ", "MEQ", "PEQ", "'?'", "':'",
  "OR", "AND", "NEQ", "EQ", "'<'", "'>'", "GEQ", "LEQ", "'+'", "'-'",
  "'*'", "'/'", "'%'", "'!'", "UMINUS", "UPLUS", "'^'", "MINUSMINUS",
  "PLUSPLUS", "'['", "']'", "'('", "')'", "'.'", "'{'", "'}'", "','",
  "';'", "$accept", "programlist", "program", "constant", "step",
  "steplist", "variable", "$@1", "function", "userfunction", "ARRAYCONST",
  "assignment", "expression", "expressionlist", "variablename",
  "assignedvariablename", "variablelistitem", "variablelist",
  "typedvariablelistitem", "typedvariablelist", "declaration",
  "functiondeclaration", "$@2", "$@3", "statement", "statementlist",
  "block", "blockment", "flowstatement", "$@4", "$@5", "$@6", "caselabel",
  "caselist", "filteroptions", "filter", "savetokenname", "savetype",
  "cleartype", "pushscope", "popscope", "pushstepname", "pushfunc",
  "pushfilter", "setglobal", "unsetglobal", YY_NULL
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
     285,   286,    61,   287,   288,   289,   290,    63,    58,   291,
     292,   293,   294,    60,    62,   295,   296,    43,    45,    42,
      47,    37,    33,   297,   298,    94,   299,   300,    91,    93,
      40,    41,    46,   123,   125,    44,    59
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    67,    68,    68,    69,    69,    70,    70,    70,    70,
      71,    71,    71,    71,    72,    72,    72,    73,    73,    73,
      73,    74,    73,    73,    75,    75,    75,    76,    76,    76,
      77,    78,    78,    78,    79,    79,    79,    79,    79,    79,
      79,    79,    79,    79,    79,    79,    79,    79,    79,    79,
      79,    79,    79,    79,    79,    79,    79,    79,    79,    79,
      79,    79,    79,    79,    79,    79,    80,    80,    80,    81,
      81,    81,    81,    81,    81,    81,    82,    82,    82,    82,
      83,    83,    83,    84,    84,    84,    85,    85,    86,    86,
      86,    87,    87,    87,    88,    89,    88,    88,    90,    88,
      91,    91,    91,    91,    91,    91,    91,    91,    91,    91,
      91,    91,    91,    92,    92,    93,    93,    94,    94,    95,
      95,    95,    95,    96,    95,    97,    95,    95,    95,    98,
      95,    99,    99,   100,   100,   100,   101,   101,   102,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112
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
       3,     3,     2,     5,     2,     1,     1,     3,     2,     1,
       1,     1,     1,     1,     1,     2,     3,     6,     6,     3,
       1,     4,     1,     1,     3,     2,     3,     5,     1,     3,
       2,     6,     3,     3,     7,     0,     9,     7,     0,     9,
       2,     2,     2,     2,     2,     1,     1,     1,     2,     3,
       2,     2,     2,     1,     2,     5,     2,     1,     1,     7,
       5,    10,    10,     0,     9,     0,    11,     6,     8,     0,
       8,     5,     2,     1,     2,     2,     4,     6,     6,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0
};

/* YYDEFACT[STATE-NAME] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     6,     9,     7,    65,     8,    18,    20,     0,     0,
     140,   142,   142,   142,     0,     0,   147,     0,   146,     0,
     141,     0,     0,     0,     0,     0,     0,     0,     0,   142,
       0,     2,    34,    46,    35,    36,     0,     0,     0,   106,
     113,     4,     5,   105,   107,     0,     0,    26,     0,    29,
       0,     0,     0,     0,     0,     0,     0,     0,   110,    46,
      35,    36,     0,     0,   108,     0,   111,   112,    64,    41,
      62,    45,    44,     0,   116,     0,     1,     3,     0,     0,
       0,     0,     0,    43,    42,    23,    21,   103,   104,   100,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   102,   101,   114,     0,     0,
      24,    66,     0,    27,     0,    93,   139,    69,    71,    70,
      73,    74,    72,    80,    82,    83,    92,     0,     0,     0,
       0,     0,   140,   109,     0,   145,    61,     0,    33,     0,
      32,    31,    40,    39,    38,    37,     0,     0,    60,    59,
      54,    53,    57,    55,    56,    58,    47,    48,    49,    50,
      52,    51,    17,    19,    25,     0,    68,    28,    75,     0,
       0,     0,   139,     0,    85,     0,     0,   140,     0,     0,
       0,   129,     0,     0,   139,     0,     0,   143,     0,   144,
      14,     0,     0,    67,     0,    76,    79,     0,    84,     0,
       0,     0,     0,     0,     0,     0,   117,   118,   120,   148,
       0,     0,     0,     0,   115,    30,    13,    16,     0,    63,
     140,     0,    88,     0,    81,     0,   127,    80,     0,     0,
       0,     0,     0,    91,     0,   138,   139,     0,     0,     0,
       0,    15,     0,    97,    98,     0,    90,     0,     0,     0,
     123,     0,     0,     0,     0,   133,     0,   119,   136,     0,
      94,    95,     0,    12,     0,    86,     0,    89,    78,    77,
     128,     0,     0,     0,     0,     0,   132,   130,   134,   135,
       0,     0,    10,    11,     0,    99,   125,   124,     0,     0,
       0,   137,    96,    87,     0,   121,   122,     0,   126,   131
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    30,    31,    32,   190,   191,    59,   146,    60,    61,
     140,    36,    37,   112,   123,   124,   125,   126,   222,   223,
      38,    39,   281,   266,   206,    41,   207,   208,    43,   272,
     294,   205,   255,   256,   185,    44,   168,    51,    65,    52,
     214,   216,   169,    63,    57,   233
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -189
static const yytype_int16 yypact[] =
{
     795,  -189,  -189,  -189,  -189,  -189,   -41,   -35,    10,    12,
    -189,  -189,  -189,  -189,   -32,   -24,  -189,    55,  -189,    40,
    -189,   -51,    -9,    58,   503,   503,    44,    44,   503,    14,
     398,  -189,  -189,   100,    23,    32,    33,   204,    36,  -189,
    -189,   943,  -189,  -189,  -189,   503,   503,  -189,   823,  -189,
     856,   492,    11,    31,    45,   503,   503,    80,  -189,   429,
    -189,  -189,   270,    48,  -189,   103,  -189,  -189,  -189,    71,
      71,   -23,   -23,   937,  -189,   943,  -189,  -189,   113,   503,
     503,   503,   503,  -189,  -189,  -189,  -189,  -189,  -189,  -189,
     503,   503,   503,   503,   503,   503,   503,   503,   503,   503,
     503,   503,   503,   503,   503,  -189,  -189,  -189,  1172,  1193,
    -189,  1275,   427,  -189,   598,  -189,    50,  -189,  -189,  -189,
    -189,  -189,  -189,   -12,  -189,  -189,   206,    98,   503,   165,
     965,   988,  -189,  -189,   121,  -189,  -189,   733,  -189,   503,
    -189,  1275,  1275,  1275,  1275,  1275,   120,  1258,   878,  1290,
    1303,  1303,   178,   178,   178,   178,   130,   130,    71,    71,
      71,  -189,  -189,  -189,  -189,   503,  1275,  -189,  -189,    82,
     337,   503,  -189,  1356,  -189,    89,  1011,  -189,   132,    65,
      93,  -189,   795,  1356,  -189,   -36,    95,  -189,   141,  -189,
    -189,   534,   503,  1275,    27,  -189,  1275,  1214,  -189,   503,
     795,   620,   503,   503,   503,   104,  -189,  -189,   137,   206,
     131,    11,   166,    35,  -189,  -189,   -11,  -189,   120,  1275,
    -189,    11,  -189,     8,   145,  1034,  -189,    -8,  1057,   666,
     728,    62,   795,  -189,   258,  -189,  -189,    11,    25,   503,
     884,  -189,  1356,  -189,  -189,   170,  -189,   337,   124,   503,
    -189,   503,   503,   136,   169,  -189,   671,  -189,  -189,   171,
    -189,  -189,  1235,  -189,   608,   176,    11,  -189,  -189,  1275,
    -189,  1080,   795,  1103,  1126,   503,  -189,  -189,   943,  -189,
     258,    11,  -189,  -189,   503,  -189,  -189,  -189,   795,   795,
    1149,  -189,  -189,  1275,   795,  -189,  -189,   182,  -189,  -189
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -189,  -189,   191,   -43,     5,  -189,     0,  -189,     2,     4,
    -160,   101,   278,   -45,  -179,  -189,  -117,    39,  -141,    18,
     106,  -189,  -189,  -189,     1,   -69,     3,  -188,  -189,  -189,
    -189,  -189,   -18,  -189,  -189,  -189,  -168,  -125,  -189,     6,
    -189,  -189,   107,  -189,  -189,  -189
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -146
static const yytype_int16 yytable[] =
{
      33,    40,    34,    42,    35,   114,   137,   183,   122,   174,
     195,    47,   226,    49,   249,    66,   210,    45,    53,    54,
     170,   220,   227,    46,   170,   211,    71,    72,    55,   212,
      33,    40,    34,    42,    35,    75,    56,    85,   220,    86,
     220,    33,   107,    34,   257,    35,   171,   239,   220,   240,
     171,    64,   201,     6,     7,   127,   198,    67,     1,     2,
       3,     4,     5,   265,     6,     7,     8,     9,   259,   244,
      48,    68,    50,   245,    29,    33,    40,    34,    74,    35,
     253,   254,   246,   122,   287,    23,   261,   268,   221,    87,
     245,   128,   174,   132,   188,   242,   237,   246,    88,    89,
     295,   296,   106,    24,   267,   129,   298,    25,   134,   135,
    -145,    26,    27,   175,   138,    28,     1,     2,     3,     4,
       5,    58,     6,     7,     8,     9,   104,   184,   189,   178,
     122,   203,    78,    79,    80,    81,    82,    33,   107,    34,
     122,    35,   194,    23,     1,     2,     3,     4,     5,   199,
       6,     7,     8,     9,   202,   213,    83,    84,   122,   204,
      85,    24,    86,   234,    78,    25,   122,   231,   232,    26,
      27,    23,   236,    28,     6,     7,   139,   247,   177,   101,
     102,   103,    33,   220,    34,   104,    35,   278,    16,    24,
     270,   258,    85,    25,    86,   264,   275,    26,    27,   122,
      33,    28,    34,   280,    35,   215,   165,   276,   284,     1,
       2,     3,   172,     5,   235,   117,   118,   119,   120,   121,
     299,    77,   209,   241,   243,    99,   100,   101,   102,   103,
     179,   238,    33,   104,    34,   180,    35,   291,   279,     0,
     260,    90,   186,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,   103,    33,    40,    34,   104,
      35,     1,     2,     3,     0,     5,     0,     0,     0,   285,
     105,   173,    33,     0,    34,     0,    35,     0,    33,   107,
      34,     0,    35,     0,   292,     0,     0,     0,    33,    33,
      34,    34,    35,    35,    33,    62,    34,     0,    35,     0,
       0,     0,    69,    70,     0,     0,    73,    90,     0,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,     0,   108,   109,   104,   111,     0,   111,     0,
       0,     0,     0,   130,   131,     0,   133,     0,     0,     0,
       1,     2,     3,     4,     5,     0,     6,     7,     8,     9,
       0,     0,     0,     0,     0,     0,   141,   142,   143,   144,
     145,     0,     0,     0,     0,     0,     0,    23,   147,   148,
     149,   150,   151,   152,   153,   154,   155,   156,   157,   158,
     159,   160,   161,     0,     0,    24,     0,     0,     0,    25,
     166,     0,   166,    26,    27,     0,     0,    28,    76,     0,
     139,     1,     2,     3,     4,     5,   176,     6,     7,     8,
       9,    10,    11,    12,    13,    14,     0,   111,    15,     0,
       0,    16,    17,    18,    19,    20,    21,    22,    23,     0,
       1,     2,     3,     4,     5,     0,     6,     7,     8,     9,
       0,     0,     0,   193,     0,     0,    24,     0,   196,   197,
      25,     0,     0,     0,    26,    27,     0,    23,    28,     0,
       0,    29,    79,    80,    81,    82,   166,     0,     0,     0,
     219,     0,     0,     0,     0,    24,     0,   225,     0,    25,
     228,   229,   230,    26,    27,    83,    84,    28,   164,    85,
       0,    86,   165,   115,     0,     1,     2,     3,   116,     5,
       0,   117,   118,   119,   120,   121,     1,     2,     3,     4,
       5,     0,     6,     7,     8,     9,     0,   262,   111,     0,
       0,     0,     0,     0,     0,   269,     0,   271,     0,   273,
     274,     0,     0,    23,     0,   217,     0,   -22,   -22,   -22,
     -22,   -22,   166,   -22,   -22,   -22,   -22,   -22,     0,     0,
       0,    24,     0,   290,     0,    25,   -22,     0,     0,    26,
      27,     0,   293,    28,   -22,     0,   -22,   -22,   -22,   -22,
     -22,   -22,   -22,   -22,   -22,   -22,   -22,   -22,   -22,   -22,
     -22,   -22,   -22,   -22,   -22,   -22,   -22,     0,     0,   -22,
     -22,   -22,     0,   -22,   -22,   -22,   218,     0,   -22,   -22,
     -22,     1,     2,     3,     4,     5,     0,     6,     7,     8,
       9,     1,     2,     3,     4,     5,     0,     6,     7,     8,
       9,   115,     0,     1,     2,     3,   172,     5,    23,   117,
     118,   119,   120,   121,     0,     0,     0,     0,    23,     0,
       0,     0,     0,     0,     0,     0,    24,     0,     0,     0,
      25,     0,     0,     0,    26,    27,    24,     0,    28,   167,
      25,     0,     0,   165,    26,    27,     0,     0,    28,   283,
       0,     0,     0,   165,     1,     2,     3,     4,     5,     0,
       6,     7,     8,     9,    10,    11,    12,    13,    14,   253,
     254,    15,     0,     0,    16,    17,    18,    19,    20,    21,
      22,    23,     0,    90,     0,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,     0,    24,
       0,   104,     0,    25,     0,     0,     0,    26,    27,     0,
       0,    28,   251,     0,     0,   277,     1,     2,     3,     4,
       5,     0,     6,     7,     8,     9,    10,    11,    12,    13,
      14,     0,     0,    15,     0,     0,    16,    17,    18,    19,
      20,    21,    22,    23,     0,    90,     0,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   103,
       0,    24,     0,   104,     0,    25,     0,     0,     0,    26,
      27,     0,     0,    28,   252,     0,     0,   187,     1,     2,
       3,     4,     5,     0,     6,     7,     8,     9,    10,    11,
      12,    13,    14,     0,     0,    15,     0,     0,    16,    17,
      18,    19,    20,    21,    22,    23,     1,     2,     3,     4,
       5,     0,     6,     7,     8,     9,     0,     0,     0,     0,
       0,     0,     0,    24,     0,     0,     0,    25,     0,     0,
       0,    26,    27,    23,     0,    28,     0,     0,    29,     1,
       2,     3,     4,     5,     0,     6,     7,     8,     9,     0,
       0,    24,     0,     0,     0,    25,     0,     0,     0,    26,
      27,     0,     0,    28,   110,     0,    23,     1,     2,     3,
       4,     5,     0,     6,     7,     8,     9,     0,     0,     0,
       0,     0,     0,     0,    24,     0,     0,     0,    25,     0,
       0,     0,    26,    27,    23,     0,    28,   113,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   103,
       0,     0,    24,   104,     0,     0,    25,     0,     0,     0,
      26,    27,     0,     0,    28,   263,     1,     2,     3,     4,
       5,     0,     6,     7,     8,     9,    10,    11,    12,    13,
      14,     0,     0,    15,     0,     0,    16,    17,    18,    19,
      20,    21,    22,    23,    90,     0,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,     0,
       0,    24,   104,     0,     0,    25,     0,     0,   136,    26,
      27,     0,    90,    28,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,   103,     0,     0,     0,
     104,     0,     0,     0,     0,    90,   181,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   103,
       0,     0,     0,   104,     0,     0,     0,     0,    90,   182,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,     0,     0,     0,   104,     0,     0,     0,
       0,    90,   200,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,   103,     0,     0,     0,   104,
       0,     0,     0,     0,    90,   248,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,     0,
       0,     0,   104,     0,     0,     0,     0,    90,   250,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,     0,     0,     0,   104,     0,     0,     0,     0,
      90,   286,    91,    92,    93,    94,    95,    96,    97,    98,
      99,   100,   101,   102,   103,     0,     0,     0,   104,     0,
       0,     0,     0,    90,   288,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,     0,     0,
       0,   104,     0,     0,     0,     0,    90,   289,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
     103,     0,     0,     0,   104,     0,     0,     0,     0,    90,
     297,    91,    92,    93,    94,    95,    96,    97,    98,    99,
     100,   101,   102,   103,     0,     0,     0,   104,     0,     0,
      90,   162,    91,    92,    93,    94,    95,    96,    97,    98,
      99,   100,   101,   102,   103,     0,     0,     0,   104,     0,
       0,    90,   163,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,   103,     0,     0,     0,   104,
       0,     0,    90,   224,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,   103,     0,     0,     0,
     104,     0,     0,     0,   282,    90,   192,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   103,
       0,     0,    90,   104,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,   103,     0,     0,     0,
     104,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,     0,     0,     0,   104,    95,    96,    97,    98,
      99,   100,   101,   102,   103,     0,     0,     0,   104,     1,
       2,     3,   172,     5,     0,   117,   118,   119,   120,   121
};

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-189)))

#define yytable_value_is_error(Yytable_value) \
  YYID (0)

static const yytype_int16 yycheck[] =
{
       0,     0,     0,     0,     0,    50,    75,   132,    51,   126,
     170,     1,   200,     1,    22,    66,   184,    58,    12,    13,
      32,    13,   201,    58,    32,    61,    26,    27,    60,    65,
      30,    30,    30,    30,    30,    29,    60,    60,    13,    62,
      13,    41,    41,    41,   232,    41,    58,    58,    13,    60,
      58,    11,   177,     9,    10,    52,   173,    66,     3,     4,
       5,     6,     7,   242,     9,    10,    11,    12,   236,    61,
      60,    13,    60,    65,    63,    75,    75,    75,    64,    75,
      18,    19,   223,   126,   272,    30,    61,   247,    61,    66,
      65,    60,   209,    13,   139,   220,    61,   238,    66,    66,
     288,   289,    66,    48,   245,    60,   294,    52,    60,     6,
      60,    56,    57,    15,     1,    60,     3,     4,     5,     6,
       7,    66,     9,    10,    11,    12,    55,     6,     8,   129,
     173,    66,    32,    33,    34,    35,    36,   137,   137,   137,
     183,   137,    60,    30,     3,     4,     5,     6,     7,    60,
       9,    10,    11,    12,    22,    60,    56,    57,   201,    66,
      60,    48,    62,    32,    32,    52,   209,    63,    31,    56,
      57,    30,     6,    60,     9,    10,    63,    32,    13,    49,
      50,    51,   182,    13,   182,    55,   182,   256,    23,    48,
      66,   234,    60,    52,    62,   240,    60,    56,    57,   242,
     200,    60,   200,    32,   200,    64,    65,    38,    32,     3,
       4,     5,     6,     7,   211,     9,    10,    11,    12,    13,
      38,    30,   183,   218,   221,    47,    48,    49,    50,    51,
     129,   213,   232,    55,   232,   129,   232,   280,   256,    -1,
     237,    37,   135,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,   256,   256,   256,    55,
     256,     3,     4,     5,    -1,     7,    -1,    -1,    -1,   266,
      66,    65,   272,    -1,   272,    -1,   272,    -1,   278,   278,
     278,    -1,   278,    -1,   281,    -1,    -1,    -1,   288,   289,
     288,   289,   288,   289,   294,    17,   294,    -1,   294,    -1,
      -1,    -1,    24,    25,    -1,    -1,    28,    37,    -1,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    -1,    45,    46,    55,    48,    -1,    50,    -1,
      -1,    -1,    -1,    55,    56,    -1,    66,    -1,    -1,    -1,
       3,     4,     5,     6,     7,    -1,     9,    10,    11,    12,
      -1,    -1,    -1,    -1,    -1,    -1,    78,    79,    80,    81,
      82,    -1,    -1,    -1,    -1,    -1,    -1,    30,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,   104,    -1,    -1,    48,    -1,    -1,    -1,    52,
     112,    -1,   114,    56,    57,    -1,    -1,    60,     0,    -1,
      63,     3,     4,     5,     6,     7,   128,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,   139,    20,    -1,
      -1,    23,    24,    25,    26,    27,    28,    29,    30,    -1,
       3,     4,     5,     6,     7,    -1,     9,    10,    11,    12,
      -1,    -1,    -1,   165,    -1,    -1,    48,    -1,   170,   171,
      52,    -1,    -1,    -1,    56,    57,    -1,    30,    60,    -1,
      -1,    63,    33,    34,    35,    36,   188,    -1,    -1,    -1,
     192,    -1,    -1,    -1,    -1,    48,    -1,   199,    -1,    52,
     202,   203,   204,    56,    57,    56,    57,    60,    61,    60,
      -1,    62,    65,     1,    -1,     3,     4,     5,     6,     7,
      -1,     9,    10,    11,    12,    13,     3,     4,     5,     6,
       7,    -1,     9,    10,    11,    12,    -1,   239,   240,    -1,
      -1,    -1,    -1,    -1,    -1,   247,    -1,   249,    -1,   251,
     252,    -1,    -1,    30,    -1,     1,    -1,     3,     4,     5,
       6,     7,   264,     9,    10,    11,    12,    13,    -1,    -1,
      -1,    48,    -1,   275,    -1,    52,    22,    -1,    -1,    56,
      57,    -1,   284,    60,    30,    -1,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    55,
      56,    57,    -1,    59,    60,    61,    62,    -1,    64,    65,
      66,     3,     4,     5,     6,     7,    -1,     9,    10,    11,
      12,     3,     4,     5,     6,     7,    -1,     9,    10,    11,
      12,     1,    -1,     3,     4,     5,     6,     7,    30,     9,
      10,    11,    12,    13,    -1,    -1,    -1,    -1,    30,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,    -1,
      52,    -1,    -1,    -1,    56,    57,    48,    -1,    60,    61,
      52,    -1,    -1,    65,    56,    57,    -1,    -1,    60,    61,
      -1,    -1,    -1,    65,     3,     4,     5,     6,     7,    -1,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    -1,    -1,    23,    24,    25,    26,    27,    28,
      29,    30,    -1,    37,    -1,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    -1,    48,
      -1,    55,    -1,    52,    -1,    -1,    -1,    56,    57,    -1,
      -1,    60,    66,    -1,    -1,    64,     3,     4,     5,     6,
       7,    -1,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    -1,    23,    24,    25,    26,
      27,    28,    29,    30,    -1,    37,    -1,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      -1,    48,    -1,    55,    -1,    52,    -1,    -1,    -1,    56,
      57,    -1,    -1,    60,    66,    -1,    -1,    64,     3,     4,
       5,     6,     7,    -1,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    -1,    23,    24,
      25,    26,    27,    28,    29,    30,     3,     4,     5,     6,
       7,    -1,     9,    10,    11,    12,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    -1,    -1,    -1,    52,    -1,    -1,
      -1,    56,    57,    30,    -1,    60,    -1,    -1,    63,     3,
       4,     5,     6,     7,    -1,     9,    10,    11,    12,    -1,
      -1,    48,    -1,    -1,    -1,    52,    -1,    -1,    -1,    56,
      57,    -1,    -1,    60,    61,    -1,    30,     3,     4,     5,
       6,     7,    -1,     9,    10,    11,    12,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    -1,    -1,    -1,    52,    -1,
      -1,    -1,    56,    57,    30,    -1,    60,    61,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      -1,    -1,    48,    55,    -1,    -1,    52,    -1,    -1,    -1,
      56,    57,    -1,    -1,    60,    61,     3,     4,     5,     6,
       7,    -1,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    -1,    23,    24,    25,    26,
      27,    28,    29,    30,    37,    -1,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    -1,
      -1,    48,    55,    -1,    -1,    52,    -1,    -1,    61,    56,
      57,    -1,    37,    60,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    -1,    -1,    -1,
      55,    -1,    -1,    -1,    -1,    37,    61,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      -1,    -1,    -1,    55,    -1,    -1,    -1,    -1,    37,    61,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    -1,    -1,    -1,    55,    -1,    -1,    -1,
      -1,    37,    61,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    -1,    -1,    -1,    55,
      -1,    -1,    -1,    -1,    37,    61,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    -1,
      -1,    -1,    55,    -1,    -1,    -1,    -1,    37,    61,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    -1,    -1,    -1,    55,    -1,    -1,    -1,    -1,
      37,    61,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    -1,    -1,    -1,    55,    -1,
      -1,    -1,    -1,    37,    61,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    -1,    -1,
      -1,    55,    -1,    -1,    -1,    -1,    37,    61,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    -1,    -1,    -1,    55,    -1,    -1,    -1,    -1,    37,
      61,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    -1,    -1,    -1,    55,    -1,    -1,
      37,    59,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    -1,    -1,    -1,    55,    -1,
      -1,    37,    59,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    -1,    -1,    -1,    55,
      -1,    -1,    37,    59,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    -1,    -1,    -1,
      55,    -1,    -1,    -1,    59,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      -1,    -1,    37,    55,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    -1,    -1,    -1,
      55,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    -1,    -1,    -1,    55,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    -1,    -1,    -1,    55,     3,
       4,     5,     6,     7,    -1,     9,    10,    11,    12,    13
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     5,     6,     7,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    20,    23,    24,    25,    26,
      27,    28,    29,    30,    48,    52,    56,    57,    60,    63,
      68,    69,    70,    73,    75,    76,    78,    79,    87,    88,
      91,    92,    93,    95,   102,    58,    58,     1,    60,     1,
      60,   104,   106,   106,   106,    60,    60,   111,    66,    73,
      75,    76,    79,   110,    11,   105,    66,    66,    13,    79,
      79,    73,    73,    79,    64,   106,     0,    69,    32,    33,
      34,    35,    36,    56,    57,    60,    62,    66,    66,    66,
      37,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    55,    66,    66,    91,    79,    79,
      61,    79,    80,    61,    80,     1,     6,     9,    10,    11,
      12,    13,    70,    81,    82,    83,    84,    93,    60,    60,
      79,    79,    13,    66,    60,     6,    61,    92,     1,    63,
      77,    79,    79,    79,    79,    79,    74,    79,    79,    79,
      79,    79,    79,    79,    79,    79,    79,    79,    79,    79,
      79,    79,    59,    59,    61,    65,    79,    61,   103,   109,
      32,    58,     6,    65,    83,    15,    79,    13,    73,    78,
      87,    61,    61,   104,     6,   101,   109,    64,    80,     8,
      71,    72,    38,    79,    60,    77,    79,    79,    83,    60,
      61,   104,    22,    66,    66,    98,    91,    93,    94,    84,
     103,    61,    65,    60,   107,    64,   108,     1,    62,    79,
      13,    61,    85,    86,    59,    79,    94,    81,    79,    79,
      79,    63,    31,   112,    32,    93,     6,    61,    86,    58,
      60,    71,   104,    93,    61,    65,    85,    32,    61,    22,
      61,    66,    66,    18,    19,    99,   100,    94,    70,   103,
      93,    61,    79,    61,    80,    81,    90,    85,    77,    79,
      66,    79,    96,    79,    79,    60,    38,    64,    92,    99,
      32,    89,    59,    61,    32,    93,    61,    94,    61,    61,
      79,    70,    93,    79,    97,    94,    94,    61,    94,    38
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
   Once GCC version 2 has supplanted version 1, this can go.  However,
   YYFAIL appears to be in use.  Nevertheless, it is formally deprecated
   in Bison 2.4.2's NEWS entry, where a plan to phase it out is
   discussed.  */

#define YYFAIL		goto yyerrlab
#if defined YYFAIL
  /* This is here to suppress warnings from the GCC cpp's
     -Wunused-macros.  Normally we don't worry about that warning, but
     some users do, and we want to make it easy for users to remove
     YYFAIL uses, which will produce warnings from Bison 2.5.  */
#endif

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))

/* Error token number */
#define YYTERROR	1
#define YYERRCODE	256


/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
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
  FILE *yyo = yyoutput;
  YYUSE (yyo);
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

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULL, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULL;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - Assume YYFAIL is not used.  It's too flawed to consider.  See
       <http://lists.gnu.org/archive/html/bison-patches/2009-12/msg00024.html>
       for details.  YYERROR is fine as it does not invoke this
       function.
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULL, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
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




/* The lookahead symbol.  */
int yychar;


#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval YY_INITIAL_VALUE(yyval_default);

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
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
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
  int yytoken = 0;
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

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
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
  if (yypact_value_is_default (yyn))
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
      if (yytable_value_is_error (yyn))
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
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

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
/* Line 1792 of yacc.c  */
#line 84 "grammar.yy"
    { }
    break;

  case 3:
/* Line 1792 of yacc.c  */
#line 85 "grammar.yy"
    { }
    break;

  case 4:
/* Line 1792 of yacc.c  */
#line 90 "grammar.yy"
    {
		if (((yyvsp[(1) - (1)].node) != NULL) && (!cmdparser.addStatement((yyvsp[(1) - (1)].node)))) YYABORT;
		}
    break;

  case 5:
/* Line 1792 of yacc.c  */
#line 93 "grammar.yy"
    {
		if (((yyvsp[(1) - (1)].node) != NULL) && (!cmdparser.addStatement((yyvsp[(1) - (1)].node)))) YYABORT;
		}
    break;

  case 6:
/* Line 1792 of yacc.c  */
#line 103 "grammar.yy"
    { (yyval.node) = cmdparser.addConstant((yyvsp[(1) - (1)].intConst)); }
    break;

  case 7:
/* Line 1792 of yacc.c  */
#line 104 "grammar.yy"
    { (yyval.node) = cmdparser.addConstant((yyvsp[(1) - (1)].doubleConst)); }
    break;

  case 8:
/* Line 1792 of yacc.c  */
#line 105 "grammar.yy"
    { (yyval.node) = cmdparser.addConstant(cmdparser.lexedName()); }
    break;

  case 9:
/* Line 1792 of yacc.c  */
#line 106 "grammar.yy"
    { (yyval.node) = cmdparser.addElementConstant((yyvsp[(1) - (1)].intConst)); }
    break;

  case 10:
/* Line 1792 of yacc.c  */
#line 115 "grammar.yy"
    {
		if (!cmdparser.expandPath(stepNameStack.last(), (yyvsp[(4) - (5)].node))) YYABORT;
		stepNameStack.removeLast();
		}
    break;

  case 11:
/* Line 1792 of yacc.c  */
#line 119 "grammar.yy"
    {
		if (!cmdparser.expandPath(stepNameStack.last(), NULL, (yyvsp[(4) - (5)].node))) YYABORT;
		stepNameStack.removeLast();
		}
    break;

  case 12:
/* Line 1792 of yacc.c  */
#line 123 "grammar.yy"
    {
		if (!cmdparser.expandPath(stepNameStack.last(), NULL, NULL)) YYABORT;
		stepNameStack.removeLast();
		}
    break;

  case 13:
/* Line 1792 of yacc.c  */
#line 127 "grammar.yy"
    {
		if (!cmdparser.expandPath(cmdparser.lexedName())) YYABORT;
		stepNameStack.removeLast();
		}
    break;

  case 14:
/* Line 1792 of yacc.c  */
#line 135 "grammar.yy"
    { }
    break;

  case 15:
/* Line 1792 of yacc.c  */
#line 136 "grammar.yy"
    { }
    break;

  case 16:
/* Line 1792 of yacc.c  */
#line 137 "grammar.yy"
    { Messenger::print("Error formulating path."); YYABORT; }
    break;

  case 17:
/* Line 1792 of yacc.c  */
#line 142 "grammar.yy"
    {
		(yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (4)].variable),(yyvsp[(3) - (4)].node));
		if ((yyval.node) == NULL) { Messenger::print("Error in variable expression (code 1)"); YYABORT; }
		}
    break;

  case 18:
/* Line 1792 of yacc.c  */
#line 146 "grammar.yy"
    {
		(yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (1)].variable));
		if ((yyval.node) == NULL) { Messenger::print("Error in variable expression (code 2)"); YYABORT; }
		}
    break;

  case 19:
/* Line 1792 of yacc.c  */
#line 150 "grammar.yy"
    {
		(yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (4)].variable),(yyvsp[(3) - (4)].node));
		if ((yyval.node) == NULL) { Messenger::print("Error in variable expression (code 3)"); YYABORT; }
		}
    break;

  case 20:
/* Line 1792 of yacc.c  */
#line 154 "grammar.yy"
    {
		(yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (1)].variable));
		if ((yyval.node) == NULL) { Messenger::print("Error in variable expression (code 4)"); YYABORT; }
		}
    break;

  case 21:
/* Line 1792 of yacc.c  */
#line 158 "grammar.yy"
    {
		cmdparser.createPath((yyvsp[(1) - (2)].node));
		}
    break;

  case 22:
/* Line 1792 of yacc.c  */
#line 160 "grammar.yy"
    {
		(yyval.node) = cmdparser.finalisePath();
		}
    break;

  case 23:
/* Line 1792 of yacc.c  */
#line 163 "grammar.yy"
    {
		Messenger::print("Can't use a variable as a function. Did you mean '[' instead?"); (yyval.node) = NULL;
		}
    break;

  case 24:
/* Line 1792 of yacc.c  */
#line 174 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction( (Commands::Function) (yyvsp[(1) - (3)].functionId));
		if ((yyval.node) == NULL) YYABORT;
		Messenger::print(Messenger::Parse, "PARSER : function : function '%s'", Commands::command((Commands::Function) (yyvsp[(1) - (3)].functionId)));
		}
    break;

  case 25:
/* Line 1792 of yacc.c  */
#line 179 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunctionWithArglist( (Commands::Function) (yyvsp[(1) - (4)].functionId),(yyvsp[(3) - (4)].node));
		if ((yyval.node) == NULL) YYABORT;
		Messenger::print(Messenger::Parse, "PARSER : function : function '%s' with exprlist", Commands::command((Commands::Function) (yyvsp[(1) - (4)].functionId)));
		}
    break;

  case 26:
/* Line 1792 of yacc.c  */
#line 184 "grammar.yy"
    {
		Messenger::print("Error: Missing brackets after function call?");
		YYABORT;
		}
    break;

  case 27:
/* Line 1792 of yacc.c  */
#line 192 "grammar.yy"
    {
		(yyval.node) = cmdparser.addUserFunction((yyvsp[(1) - (3)].tree));
		if ((yyval.node) == NULL) YYABORT;
		Messenger::print(Messenger::Parse,"PARSER : userfunction : function '%s'", qPrintable((yyvsp[(1) - (3)].tree)->name()));
		}
    break;

  case 28:
/* Line 1792 of yacc.c  */
#line 197 "grammar.yy"
    {
		(yyval.node) = cmdparser.addUserFunction((yyvsp[(1) - (4)].tree),(yyvsp[(3) - (4)].node));
		if ((yyval.node) == NULL) YYABORT;
		Messenger::print(Messenger::Parse,"PARSER : userfunction : function '%s' with expressionlist", qPrintable((yyvsp[(1) - (4)].tree)->name()));
		}
    break;

  case 29:
/* Line 1792 of yacc.c  */
#line 202 "grammar.yy"
    {
		Messenger::print("Error: Missing brackets after function call?");
		YYABORT;
		}
    break;

  case 30:
/* Line 1792 of yacc.c  */
#line 214 "grammar.yy"
    {
		(yyval.node) = cmdparser.addArrayConstant((yyvsp[(2) - (3)].node));
		if ((yyval.node) == NULL) YYABORT;
		}
    break;

  case 31:
/* Line 1792 of yacc.c  */
#line 225 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorAssignment,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 32:
/* Line 1792 of yacc.c  */
#line 226 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorAssignment,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 33:
/* Line 1792 of yacc.c  */
#line 227 "grammar.yy"
    { Messenger::print("Mangled expression used in assignment."); YYABORT; }
    break;

  case 34:
/* Line 1792 of yacc.c  */
#line 232 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 35:
/* Line 1792 of yacc.c  */
#line 233 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 36:
/* Line 1792 of yacc.c  */
#line 234 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 37:
/* Line 1792 of yacc.c  */
#line 235 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorAssignmentPlus,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 38:
/* Line 1792 of yacc.c  */
#line 236 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorAssignmentSubtract,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 39:
/* Line 1792 of yacc.c  */
#line 237 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorAssignmentMultiply,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 40:
/* Line 1792 of yacc.c  */
#line 238 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorAssignmentDivide,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 41:
/* Line 1792 of yacc.c  */
#line 239 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorNegate, (yyvsp[(2) - (2)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 42:
/* Line 1792 of yacc.c  */
#line 240 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorPostfixIncrease, (yyvsp[(1) - (2)].node));  if ((yyval.node) == NULL) YYABORT; }
    break;

  case 43:
/* Line 1792 of yacc.c  */
#line 241 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorPostfixDecrease, (yyvsp[(1) - (2)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 44:
/* Line 1792 of yacc.c  */
#line 242 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorPrefixIncrease, (yyvsp[(2) - (2)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 45:
/* Line 1792 of yacc.c  */
#line 243 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorPrefixDecrease, (yyvsp[(2) - (2)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 46:
/* Line 1792 of yacc.c  */
#line 244 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 47:
/* Line 1792 of yacc.c  */
#line 245 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorAdd, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 48:
/* Line 1792 of yacc.c  */
#line 246 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorSubtract, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 49:
/* Line 1792 of yacc.c  */
#line 247 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorMultiply, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 50:
/* Line 1792 of yacc.c  */
#line 248 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorDivide, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 51:
/* Line 1792 of yacc.c  */
#line 249 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorPower, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 52:
/* Line 1792 of yacc.c  */
#line 250 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorModulus, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 53:
/* Line 1792 of yacc.c  */
#line 251 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 54:
/* Line 1792 of yacc.c  */
#line 252 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorNotEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 55:
/* Line 1792 of yacc.c  */
#line 253 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorGreaterThan, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 56:
/* Line 1792 of yacc.c  */
#line 254 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorGreaterThanEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 57:
/* Line 1792 of yacc.c  */
#line 255 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorLessThan, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 58:
/* Line 1792 of yacc.c  */
#line 256 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorLessThanEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 59:
/* Line 1792 of yacc.c  */
#line 257 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorAnd, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 60:
/* Line 1792 of yacc.c  */
#line 258 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorOr, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 61:
/* Line 1792 of yacc.c  */
#line 259 "grammar.yy"
    { (yyval.node) = (yyvsp[(2) - (3)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 62:
/* Line 1792 of yacc.c  */
#line 260 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorNot, (yyvsp[(2) - (2)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 63:
/* Line 1792 of yacc.c  */
#line 261 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Commands::OperatorInlineIf, (yyvsp[(1) - (5)].node), (yyvsp[(3) - (5)].node), (yyvsp[(5) - (5)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 64:
/* Line 1792 of yacc.c  */
#line 262 "grammar.yy"
    { (yyval.node) = cmdparser.addNew(yylval.vtype); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 65:
/* Line 1792 of yacc.c  */
#line 263 "grammar.yy"
    { Messenger::print("Error: '%s' has not been declared as a function or a variable.", qPrintable(cmdparser.lexedName())); YYABORT; }
    break;

  case 66:
/* Line 1792 of yacc.c  */
#line 268 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		if ((yyval.node) == NULL) YYABORT;
		}
    break;

  case 67:
/* Line 1792 of yacc.c  */
#line 272 "grammar.yy"
    {
		(yyval.node) = Tree::joinArguments((yyvsp[(3) - (3)].node),(yyvsp[(1) - (3)].node));
		}
    break;

  case 68:
/* Line 1792 of yacc.c  */
#line 275 "grammar.yy"
    {
		Messenger::print("Error: Missing comma between items.");
		YYABORT;
		}
    break;

  case 69:
/* Line 1792 of yacc.c  */
#line 287 "grammar.yy"
    {
		tokenName = yylval.variable->name();
		Messenger::print(Messenger::Parse,"PARSER : variablename : existing var '%s'", qPrintable(tokenName));
		Messenger::print("Warning - declaration of variable '%s' in %s hides a previous declaration.", qPrintable(tokenName), qPrintable(cmdparser.sourceInfo()));
		variableName = tokenName;
/* 		$$ = &tokenName; */ // ATEN2 TODO TOCHECK
		}
    break;

  case 70:
/* Line 1792 of yacc.c  */
#line 294 "grammar.yy"
    {
		tokenName = Commands::command((Commands::Function) yylval.functionId);
		Messenger::print(Messenger::Parse,"PARSER : variablename : existing built-in function '%s'", qPrintable(tokenName));
		variableName = tokenName;
/* 		$$ = &tokenName; */ // ATEN2 TODO TOCHECK
		}
    break;

  case 71:
/* Line 1792 of yacc.c  */
#line 300 "grammar.yy"
    {
		tokenName = yylval.variable->name();
		Messenger::print(Messenger::Parse,"PARSER : variablename : existing var '%s' in same scope", qPrintable(tokenName));
		Messenger::print("Error: Declaration of variable '%s' in %s conflicts with a previous declaration.", qPrintable(tokenName), qPrintable(cmdparser.sourceInfo()));
		YYABORT;
		}
    break;

  case 72:
/* Line 1792 of yacc.c  */
#line 306 "grammar.yy"
    {
		tokenName = yylval.variable->name();
		Messenger::print(Messenger::Parse,"PARSER : variablename : constant '%s'", qPrintable(tokenName));
		Messenger::print("Error: Constant value found in declaration.");
		YYABORT;
		}
    break;

  case 73:
/* Line 1792 of yacc.c  */
#line 312 "grammar.yy"
    {
		tokenName = yylval.tree->name();
		Messenger::print(Messenger::Parse,"PARSER : variablename : existing user function '%s'", qPrintable(tokenName));
		Messenger::print("Error: Existing user-defined function '%s' in %s cannot be redeclared.", qPrintable(tokenName), qPrintable(cmdparser.sourceInfo()));
		YYABORT;
		}
    break;

  case 74:
/* Line 1792 of yacc.c  */
#line 318 "grammar.yy"
    {
		Messenger::print(Messenger::Parse,"PARSER : variablename : variable type-name '%s'", VTypes::dataType(yylval.vtype));
		Messenger::print("Error: Type-name used in variable declaration.");
		YYABORT;
		}
    break;

  case 75:
/* Line 1792 of yacc.c  */
#line 323 "grammar.yy"
    {
		Messenger::print(Messenger::Parse,"PARSER : variablename : new token, name is '%s'", qPrintable(tokenName));
		if (declaredType == VTypes::NoData) { Messenger::print("Token '%s' is undeclared.", qPrintable(tokenName)); YYABORT; }
		variableName = cmdparser.lexedName();
		}
    break;

  case 76:
/* Line 1792 of yacc.c  */
#line 332 "grammar.yy"
    {
		Messenger::print(Messenger::Parse,"PARSER : assignedvariablename : var '%s' with array assignment", qPrintable(tokenName));
		(yyval.node) = cmdparser.addVariable(declaredType, tokenName, (yyvsp[(3) - (3)].node), globalDeclarations);
		}
    break;

  case 77:
/* Line 1792 of yacc.c  */
#line 336 "grammar.yy"
    {
		Messenger::print(Messenger::Parse,"PARSER : assignedvariablename : array var '%s' with expr assignment", qPrintable(tokenName));
		(yyval.node) = cmdparser.addArrayVariable(declaredType, tokenName, (yyvsp[(3) - (6)].node), (yyvsp[(6) - (6)].node), globalDeclarations);
		}
    break;

  case 78:
/* Line 1792 of yacc.c  */
#line 340 "grammar.yy"
    {
		Messenger::print(Messenger::Parse,"PARSER : assignedvariablename : array var '%s' with array assignment", qPrintable(tokenName));
		(yyval.node) = cmdparser.addArrayVariable(declaredType, tokenName, (yyvsp[(3) - (6)].node), (yyvsp[(6) - (6)].node), globalDeclarations);
		}
    break;

  case 79:
/* Line 1792 of yacc.c  */
#line 344 "grammar.yy"
    {
		Messenger::print(Messenger::Parse,"PARSER : assignedvariablename : var '%s' with expr assignment", qPrintable(variableName));
		(yyval.node) = cmdparser.addVariable(declaredType, tokenName, (yyvsp[(3) - (3)].node), globalDeclarations);
		}
    break;

  case 80:
/* Line 1792 of yacc.c  */
#line 352 "grammar.yy"
    {
		Messenger::print(Messenger::Parse,"PARSER : assignedvariablename : var '%s'", qPrintable(tokenName));
		(yyval.node) = cmdparser.addVariable(declaredType, tokenName, NULL, globalDeclarations);
		}
    break;

  case 81:
/* Line 1792 of yacc.c  */
#line 356 "grammar.yy"
    {
		Messenger::print(Messenger::Parse,"PARSER : assignedvariablename : array var '%s'", qPrintable(tokenName));
		(yyval.node) = cmdparser.addArrayVariable(declaredType, tokenName, (yyvsp[(3) - (4)].node), NULL, globalDeclarations);
		}
    break;

  case 82:
/* Line 1792 of yacc.c  */
#line 360 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 83:
/* Line 1792 of yacc.c  */
#line 367 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 84:
/* Line 1792 of yacc.c  */
#line 370 "grammar.yy"
    {
		(yyval.node) = Tree::joinArguments((yyvsp[(3) - (3)].node),(yyvsp[(1) - (3)].node));
		}
    break;

  case 85:
/* Line 1792 of yacc.c  */
#line 373 "grammar.yy"
    {
		Messenger::print("Error: Missing comma between declarations?");
		YYABORT;
		}
    break;

  case 86:
/* Line 1792 of yacc.c  */
#line 381 "grammar.yy"
    {
		Messenger::print(Messenger::Parse,"PARSER : typedvariablelistitem : var '%s'", qPrintable(tokenName));
		(yyval.node) = cmdparser.addVariable(declaredType, tokenName);
		}
    break;

  case 87:
/* Line 1792 of yacc.c  */
#line 385 "grammar.yy"
    {
		Messenger::print(Messenger::Parse,"PARSER : typedvariablelistitem : var '%s' with expr assignment", qPrintable(tokenName));
		(yyval.node) = cmdparser.addVariable(declaredType, tokenName, (yyvsp[(5) - (5)].node));
		}
    break;

  case 88:
/* Line 1792 of yacc.c  */
#line 393 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 89:
/* Line 1792 of yacc.c  */
#line 396 "grammar.yy"
    {
		(yyval.node) = Tree::joinArguments((yyvsp[(3) - (3)].node),(yyvsp[(1) - (3)].node));
		}
    break;

  case 90:
/* Line 1792 of yacc.c  */
#line 399 "grammar.yy"
    {
		Messenger::print("Error: Missing comma between declarations?");
		YYABORT;
		}
    break;

  case 91:
/* Line 1792 of yacc.c  */
#line 407 "grammar.yy"
    {
		Messenger::print(Messenger::Parse,"PARSER : global declaration : standard variable declaration list");
		(yyval.node) = cmdparser.addDeclarations((yyvsp[(5) - (6)].node));
		declaredType = VTypes::NoData;
		}
    break;

  case 92:
/* Line 1792 of yacc.c  */
#line 412 "grammar.yy"
    {
		Messenger::print(Messenger::Parse,"PARSER : declaration : standard variable declaration list");
		(yyval.node) = cmdparser.addDeclarations((yyvsp[(3) - (3)].node));
		declaredType = VTypes::NoData;
		}
    break;

  case 93:
/* Line 1792 of yacc.c  */
#line 417 "grammar.yy"
    {
		Messenger::print("Illegal use of reserved word '%s'.", VTypes::dataType(declaredType));
		YYABORT;
		}
    break;

  case 94:
/* Line 1792 of yacc.c  */
#line 429 "grammar.yy"
    {
		Messenger::print(Messenger::Parse,"PARSER : functiondeclaration : user-defined subroutine (VOID return value, no arguments)");
		if (!cmdparser.addStatement((yyvsp[(7) - (7)].node))) YYABORT;
		cmdparser.popTree();
		declaredType = VTypes::NoData;
		}
    break;

  case 95:
/* Line 1792 of yacc.c  */
#line 435 "grammar.yy"
    {
		Messenger::print(Messenger::Parse,"PARSER : functiondeclaration : user-defined subroutine (VOID return value, arguments)");
		if (!(yyvsp[(4) - (7)].tree)->addLocalFunctionArguments((yyvsp[(6) - (7)].node))) YYABORT;
		}
    break;

  case 96:
/* Line 1792 of yacc.c  */
#line 438 "grammar.yy"
    {
		if (!cmdparser.addStatement((yyvsp[(9) - (9)].node))) YYABORT;
		cmdparser.popTree();
		declaredType = VTypes::NoData;
		}
    break;

  case 97:
/* Line 1792 of yacc.c  */
#line 443 "grammar.yy"
    {
		Messenger::print(Messenger::Parse,"PARSER : functiondeclaration : user-defined function (%s return value, no arguments)", VTypes::dataType((yyvsp[(4) - (7)].tree)->returnType()));
		if (!cmdparser.addStatement((yyvsp[(7) - (7)].node))) YYABORT;
		cmdparser.popTree();
		declaredType = VTypes::NoData;
		}
    break;

  case 98:
/* Line 1792 of yacc.c  */
#line 449 "grammar.yy"
    {
		Messenger::print(Messenger::Parse,"PARSER : functiondeclaration : user-defined function (%s return value, arguments)", VTypes::dataType((yyvsp[(4) - (7)].tree)->returnType()));
		if (!(yyvsp[(4) - (7)].tree)->addLocalFunctionArguments((yyvsp[(6) - (7)].node))) YYABORT;
		}
    break;

  case 99:
/* Line 1792 of yacc.c  */
#line 452 "grammar.yy"
    {
		if (!cmdparser.addStatement((yyvsp[(9) - (9)].node))) YYABORT;
		cmdparser.popTree();
		declaredType = VTypes::NoData;
		}
    break;

  case 100:
/* Line 1792 of yacc.c  */
#line 465 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (2)].node);
		}
    break;

  case 101:
/* Line 1792 of yacc.c  */
#line 468 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (2)].node);
		}
    break;

  case 102:
/* Line 1792 of yacc.c  */
#line 471 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (2)].node);
		}
    break;

  case 103:
/* Line 1792 of yacc.c  */
#line 474 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (2)].node);
		}
    break;

  case 104:
/* Line 1792 of yacc.c  */
#line 477 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (2)].node);
		}
    break;

  case 105:
/* Line 1792 of yacc.c  */
#line 480 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 106:
/* Line 1792 of yacc.c  */
#line 483 "grammar.yy"
    {
		(yyval.node) = NULL;
		}
    break;

  case 107:
/* Line 1792 of yacc.c  */
#line 486 "grammar.yy"
    {
		(yyval.node) = NULL;
		}
    break;

  case 108:
/* Line 1792 of yacc.c  */
#line 489 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Commands::Help, cmdparser.addConstant((yyvsp[(2) - (2)].functionId)));
		}
    break;

  case 109:
/* Line 1792 of yacc.c  */
#line 492 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Commands::Return,(yyvsp[(2) - (3)].node));
		}
    break;

  case 110:
/* Line 1792 of yacc.c  */
#line 495 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Commands::Return);
		}
    break;

  case 111:
/* Line 1792 of yacc.c  */
#line 498 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Commands::Continue);
		}
    break;

  case 112:
/* Line 1792 of yacc.c  */
#line 501 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Commands::Break);
		}
    break;

  case 113:
/* Line 1792 of yacc.c  */
#line 508 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 114:
/* Line 1792 of yacc.c  */
#line 511 "grammar.yy"
    {
		if ((yyvsp[(2) - (2)].node) == NULL) (yyval.node) = (yyvsp[(1) - (2)].node);
		else (yyval.node) = cmdparser.joinCommands((yyvsp[(1) - (2)].node), (yyvsp[(2) - (2)].node));
		}
    break;

  case 115:
/* Line 1792 of yacc.c  */
#line 519 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(3) - (5)].node);
		}
    break;

  case 116:
/* Line 1792 of yacc.c  */
#line 522 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Commands::NoFunction);
		}
    break;

  case 117:
/* Line 1792 of yacc.c  */
#line 529 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 118:
/* Line 1792 of yacc.c  */
#line 532 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 119:
/* Line 1792 of yacc.c  */
#line 539 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Commands::If,(yyvsp[(3) - (7)].node),(yyvsp[(5) - (7)].node),(yyvsp[(7) - (7)].node));
		}
    break;

  case 120:
/* Line 1792 of yacc.c  */
#line 542 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Commands::If,(yyvsp[(3) - (5)].node),(yyvsp[(5) - (5)].node));
		}
    break;

  case 121:
/* Line 1792 of yacc.c  */
#line 545 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (10)].node), cmdparser.addFunction(Commands::For, (yyvsp[(4) - (10)].node),(yyvsp[(6) - (10)].node),(yyvsp[(8) - (10)].node),(yyvsp[(10) - (10)].node))); cmdparser.popScope();
		}
    break;

  case 122:
/* Line 1792 of yacc.c  */
#line 548 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (10)].node), cmdparser.addFunction(Commands::For, (yyvsp[(4) - (10)].node),(yyvsp[(6) - (10)].node),(yyvsp[(8) - (10)].node),(yyvsp[(10) - (10)].node))); cmdparser.popScope();
		}
    break;

  case 123:
/* Line 1792 of yacc.c  */
#line 551 "grammar.yy"
    {
		if ((yyvsp[(4) - (7)].node)->returnType() <= VTypes::VectorData) { Messenger::print("Error: For/In loop variable must be of pointer type."); YYABORT; }
		if ((yyvsp[(4) - (7)].node)->returnType() != (yyvsp[(6) - (7)].node)->returnType()) { Messenger::print("Error: For/In loop variable is not being assigned the correct type."); YYABORT; }
		}
    break;

  case 124:
/* Line 1792 of yacc.c  */
#line 554 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (9)].node), cmdparser.addFunction(Commands::ForIn,(yyvsp[(4) - (9)].node),(yyvsp[(6) - (9)].node),(yyvsp[(9) - (9)].node)));
		cmdparser.popScope();
		}
    break;

  case 125:
/* Line 1792 of yacc.c  */
#line 558 "grammar.yy"
    { 
		if (declaredType <= VTypes::VectorData)
		{
			Messenger::print("Error: For/In loop variable must be of pointer type.");
			YYABORT;
		}
		tempNode = cmdparser.addVariable(declaredType, tokenName);
		if (declaredType != (yyvsp[(8) - (9)].node)->returnType())
		{
			Messenger::print("Error: For/In loop variable is not being assigned the correct type.");
			YYABORT;
		}
		}
    break;

  case 126:
/* Line 1792 of yacc.c  */
#line 570 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (11)].node), cmdparser.addFunction(Commands::ForIn,tempNode,(yyvsp[(8) - (11)].node),(yyvsp[(11) - (11)].node)));
		cmdparser.popScope();
		}
    break;

  case 127:
/* Line 1792 of yacc.c  */
#line 574 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (6)].node), cmdparser.addFunction(Commands::While, (yyvsp[(4) - (6)].node),(yyvsp[(6) - (6)].node)));
		cmdparser.popScope();
		}
    break;

  case 128:
/* Line 1792 of yacc.c  */
#line 578 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (8)].node), cmdparser.addFunction(Commands::DoWhile, (yyvsp[(3) - (8)].node),(yyvsp[(6) - (8)].node)));
		cmdparser.popScope();
		}
    break;

  case 129:
/* Line 1792 of yacc.c  */
#line 582 "grammar.yy"
    {
		if (((yyvsp[(3) - (4)].node)->returnType() != VTypes::IntegerData) && ((yyvsp[(3) - (4)].node)->returnType() != VTypes::StringData))
		{
			Messenger::print("Error: Switch value must be of integer or string type.");
			YYABORT;
		}
		}
    break;

  case 130:
/* Line 1792 of yacc.c  */
#line 588 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Commands::Switch, (yyvsp[(3) - (8)].node));
		(yyval.node)->addJoinedArguments((yyvsp[(7) - (8)].node));
		}
    break;

  case 131:
/* Line 1792 of yacc.c  */
#line 596 "grammar.yy"
    {
		if (((yyvsp[(3) - (5)].node)->returnType() != VTypes::IntegerData) && ((yyvsp[(3) - (5)].node)->returnType() != VTypes::StringData))
		{
			Messenger::print("Error: Case value must be of integer or string type.");
			YYABORT;
		}
		(yyval.node) = cmdparser.addFunction(Commands::Case, (yyvsp[(3) - (5)].node));
		if ((yyval.node) == NULL) { Messenger::print("Error: Invalid case expression."); YYABORT; }
		}
    break;

  case 132:
/* Line 1792 of yacc.c  */
#line 605 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Commands::Default);
		}
    break;

  case 133:
/* Line 1792 of yacc.c  */
#line 612 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 134:
/* Line 1792 of yacc.c  */
#line 615 "grammar.yy"
    {
		(yyval.node) = Tree::joinArguments((yyvsp[(2) - (2)].node),(yyvsp[(1) - (2)].node));
		}
    break;

  case 135:
/* Line 1792 of yacc.c  */
#line 618 "grammar.yy"
    {
		(yyval.node) = Tree::joinArguments((yyvsp[(2) - (2)].node),(yyvsp[(1) - (2)].node));
		}
    break;

  case 136:
/* Line 1792 of yacc.c  */
#line 629 "grammar.yy"
    {
		if (!cmdparser.setFilterOption(tokenName, (yyvsp[(4) - (4)].node))) YYABORT;
		Messenger::print(Messenger::Parse,"PARSER : filteroptions : filter option '%s'", qPrintable(tokenName));
		}
    break;

  case 137:
/* Line 1792 of yacc.c  */
#line 633 "grammar.yy"
    {
		if (!cmdparser.setFilterOption(tokenName, (yyvsp[(6) - (6)].node))) YYABORT;
		Messenger::print(Messenger::Parse,"PARSER : filteroptions : filter option '%s'", qPrintable(tokenName));
		}
    break;

  case 138:
/* Line 1792 of yacc.c  */
#line 641 "grammar.yy"
    {
		if (((yyvsp[(6) - (6)].node) != NULL) && (!cmdparser.addStatement((yyvsp[(6) - (6)].node)))) YYABORT;
		cmdparser.popTree();
		Messenger::print(Messenger::Parse,"PARSER : completed filter definition");
		}
    break;

  case 139:
/* Line 1792 of yacc.c  */
#line 653 "grammar.yy"
    { tokenName = cmdparser.lexedName(); }
    break;

  case 140:
/* Line 1792 of yacc.c  */
#line 657 "grammar.yy"
    { declaredType = yylval.vtype; }
    break;

  case 141:
/* Line 1792 of yacc.c  */
#line 661 "grammar.yy"
    { declaredType = VTypes::NoData; }
    break;

  case 142:
/* Line 1792 of yacc.c  */
#line 665 "grammar.yy"
    { (yyval.node) = cmdparser.pushScope(); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 143:
/* Line 1792 of yacc.c  */
#line 669 "grammar.yy"
    { if (!cmdparser.popScope()) YYABORT; }
    break;

  case 144:
/* Line 1792 of yacc.c  */
#line 673 "grammar.yy"
    { stepNameStack << cmdparser.lexedName(); }
    break;

  case 145:
/* Line 1792 of yacc.c  */
#line 677 "grammar.yy"
    {
		Messenger::print(Messenger::Parse,"PARSER : pushfunc : function/statement '%s'", qPrintable(cmdparser.lexedName()));
		(yyval.tree) = cmdparser.pushFunction(qPrintable(cmdparser.lexedName()), declaredType);
		/*cmdparser.pushScope();*/
		}
    break;

  case 146:
/* Line 1792 of yacc.c  */
#line 685 "grammar.yy"
    {
		Messenger::print(Messenger::Parse,"PARSER : pushfilter : new filter definition");
		cmdparser.pushFilter();
		}
    break;

  case 147:
/* Line 1792 of yacc.c  */
#line 692 "grammar.yy"
    { globalDeclarations = true; }
    break;

  case 148:
/* Line 1792 of yacc.c  */
#line 696 "grammar.yy"
    { globalDeclarations = false; }
    break;


/* Line 1792 of yacc.c  */
#line 3129 "grammar.cc"
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
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
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
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
      if (!yypact_value_is_default (yyn))
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

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


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

#if !defined yyoverflow || YYERROR_VERBOSE
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
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
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


/* Line 2055 of yacc.c  */
#line 699 "grammar.yy"


void yyerror(char *s)
{
}
