
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
int globalDeclarations;



/* Line 189 of yacc.c  */
#line 105 "grammar.cc"

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
     ATEN_IIF = 276,
     ATEN_IN = 277,
     ATEN_GLOBAL = 278,
     ATEN_RETURN = 279,
     FILTERBLOCK = 280,
     HELP = 281,
     ATEN_VOID = 282,
     ATEN_CONTINUE = 283,
     ATEN_BREAK = 284,
     ATEN_ELSE = 285,
     DEQ = 286,
     TEQ = 287,
     MEQ = 288,
     PEQ = 289,
     OR = 290,
     AND = 291,
     NEQ = 292,
     EQ = 293,
     GEQ = 294,
     LEQ = 295,
     UMINUS = 296,
     UPLUS = 297,
     MINUSMINUS = 298,
     PLUSPLUS = 299
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
#define ATEN_IIF 276
#define ATEN_IN 277
#define ATEN_GLOBAL 278
#define ATEN_RETURN 279
#define FILTERBLOCK 280
#define HELP 281
#define ATEN_VOID 282
#define ATEN_CONTINUE 283
#define ATEN_BREAK 284
#define ATEN_ELSE 285
#define DEQ 286
#define TEQ 287
#define MEQ 288
#define PEQ 289
#define OR 290
#define AND 291
#define NEQ 292
#define EQ 293
#define GEQ 294
#define LEQ 295
#define UMINUS 296
#define UPLUS 297
#define MINUSMINUS 298
#define PLUSPLUS 299




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 214 of yacc.c  */
#line 31 "grammar.yy"

	int functionId;			/* Function enum id */
	Dnchar *name;			/* character pointer for names */
	TreeNode *node;			/* node pointer */
	Variable *variable;		/* variable pointer */
	Tree *tree;			/* function (tree) pointer */
	VTypes::DataType vtype;		/* variable type for next declarations */
	int intconst;			/* integer constant value */
	double doubleconst;		/* double constant value */



/* Line 214 of yacc.c  */
#line 242 "grammar.cc"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 254 "grammar.cc"

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
#define YYFINAL  74
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1357

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  66
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  46
/* YYNRULES -- Number of rules.  */
#define YYNRULES  147
/* YYNRULES -- Number of states.  */
#define YYNSTATES  298

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   299

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    52,     2,     2,     2,    50,     2,     2,
      59,    60,    48,    46,    64,    47,    61,    49,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    37,    65,
      42,    31,    43,    36,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    57,     2,    58,    51,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    62,     2,    63,     2,     2,     2,     2,
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
      38,    39,    40,    41,    44,    45,    53,    54,    55,    56
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
     203,   207,   211,   214,   220,   222,   224,   228,   231,   233,
     235,   237,   239,   241,   243,   246,   250,   257,   264,   268,
     270,   275,   277,   279,   283,   286,   290,   296,   298,   302,
     305,   312,   316,   320,   328,   329,   339,   347,   348,   358,
     361,   364,   367,   370,   373,   375,   377,   379,   382,   386,
     389,   392,   395,   397,   400,   406,   409,   411,   413,   421,
     427,   438,   449,   450,   460,   461,   473,   480,   489,   490,
     499,   505,   508,   510,   513,   516,   521,   528,   535,   536,
     537,   538,   539,   540,   541,   542,   543,   544
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      67,     0,    -1,    68,    -1,    67,    68,    -1,    91,    -1,
      92,    -1,     3,    -1,     5,    -1,     7,    -1,     4,    -1,
       8,   107,    57,    78,    58,    -1,     8,   107,    59,    79,
      60,    -1,     8,   107,    59,    60,    -1,     8,   107,    -1,
      70,    -1,    71,    61,    70,    -1,    71,     1,    -1,     9,
      57,    78,    58,    -1,     9,    -1,    10,    57,    78,    58,
      -1,    10,    -1,    -1,    72,    61,    73,    71,    -1,    72,
      59,    -1,    11,    59,    60,    -1,    11,    59,    79,    60,
      -1,    11,     1,    -1,    12,    59,    60,    -1,    12,    59,
      79,    60,    -1,    12,     1,    -1,    62,    79,    63,    -1,
      72,    31,    78,    -1,    72,    31,    76,    -1,    72,    31,
       1,    -1,    69,    -1,    74,    -1,    75,    -1,    72,    35,
      78,    -1,    72,    34,    78,    -1,    72,    33,    78,    -1,
      72,    32,    78,    -1,    47,    78,    -1,    72,    56,    -1,
      72,    55,    -1,    56,    72,    -1,    55,    72,    -1,    72,
      -1,    78,    46,    78,    -1,    78,    47,    78,    -1,    78,
      48,    78,    -1,    78,    49,    78,    -1,    78,    51,    78,
      -1,    78,    50,    78,    -1,    78,    41,    78,    -1,    78,
      40,    78,    -1,    78,    43,    78,    -1,    78,    44,    78,
      -1,    78,    42,    78,    -1,    78,    45,    78,    -1,    78,
      39,    78,    -1,    78,    38,    78,    -1,    59,    78,    60,
      -1,    52,    78,    -1,    78,    36,    78,    37,    78,    -1,
       6,    -1,    78,    -1,    79,    64,    78,    -1,    79,    78,
      -1,     9,    -1,    11,    -1,    10,    -1,    69,    -1,    12,
      -1,    13,    -1,     6,   102,    -1,    80,    31,    76,    -1,
      80,    57,    78,    58,    31,    78,    -1,    80,    57,    78,
      58,    31,    76,    -1,    80,    31,    78,    -1,    80,    -1,
      80,    57,    78,    58,    -1,    81,    -1,    82,    -1,    83,
      64,    82,    -1,    83,    82,    -1,    13,   103,    80,    -1,
      13,   103,    80,    31,    78,    -1,    84,    -1,    85,    64,
      84,    -1,    85,    84,    -1,    23,   110,    13,   103,    83,
     111,    -1,    13,   103,    83,    -1,    13,   103,     1,    -1,
      27,   104,     6,   108,    59,    60,    92,    -1,    -1,    27,
     104,     6,   108,    59,    85,    60,    88,    92,    -1,    13,
     103,     6,   108,    59,    60,    92,    -1,    -1,    13,   103,
       6,   108,    59,    85,    60,    89,    92,    -1,    77,    65,
      -1,    86,    65,    -1,    78,    65,    -1,    74,    65,    -1,
      75,    65,    -1,    94,    -1,    87,    -1,   101,    -1,    26,
      11,    -1,    24,    78,    65,    -1,    24,    65,    -1,    28,
      65,    -1,    29,    65,    -1,    90,    -1,    91,    90,    -1,
      62,   105,    91,    63,   106,    -1,    62,    63,    -1,    90,
      -1,    92,    -1,    20,    59,    78,    60,    93,    30,    93,
      -1,    20,    59,    78,    60,    93,    -1,    16,   105,    59,
      77,    65,    78,    65,    78,    60,    93,    -1,    16,   105,
      59,    86,    65,    78,    65,    78,    60,    93,    -1,    -1,
      16,   105,    59,    72,    22,    78,    60,    95,    93,    -1,
      -1,    16,   105,    59,    13,   103,    80,    22,    78,    60,
      96,    93,    -1,    15,   105,    59,    78,    60,    93,    -1,
      14,   105,    92,    15,    59,    78,    60,    65,    -1,    -1,
      17,    59,    78,    60,    97,    62,    99,    63,    -1,    18,
      59,    78,    60,    37,    -1,    19,    37,    -1,    98,    -1,
      99,    91,    -1,    99,    98,    -1,     6,   102,    31,    69,
      -1,   100,    64,     6,   102,    31,    69,    -1,    25,   109,
      59,   100,    60,    92,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    90,    90,    91,    96,    99,   109,   110,   111,   112,
     121,   125,   129,   133,   141,   142,   143,   148,   152,   156,
     160,   164,   164,   169,   180,   185,   190,   198,   203,   208,
     220,   231,   232,   233,   238,   239,   240,   241,   242,   243,
     244,   245,   246,   247,   248,   249,   250,   251,   252,   253,
     254,   255,   256,   257,   258,   259,   260,   261,   262,   263,
     264,   265,   266,   267,   268,   273,   277,   280,   292,   297,
     302,   307,   312,   317,   322,   331,   335,   339,   343,   351,
     355,   359,   366,   369,   372,   380,   384,   392,   395,   398,
     406,   411,   416,   428,   434,   434,   442,   448,   448,   464,
     467,   470,   473,   476,   479,   482,   485,   488,   491,   494,
     497,   500,   507,   510,   518,   521,   528,   531,   538,   541,
     544,   547,   550,   550,   557,   557,   573,   577,   581,   581,
     595,   604,   611,   614,   617,   628,   632,   640,   652,   656,
     660,   664,   668,   672,   676,   684,   691,   695
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
  "ATEN_SWITCH", "ATEN_CASE", "ATEN_DEFAULT", "ATEN_IF", "ATEN_IIF",
  "ATEN_IN", "ATEN_GLOBAL", "ATEN_RETURN", "FILTERBLOCK", "HELP",
  "ATEN_VOID", "ATEN_CONTINUE", "ATEN_BREAK", "ATEN_ELSE", "'='", "DEQ",
  "TEQ", "MEQ", "PEQ", "'?'", "':'", "OR", "AND", "NEQ", "EQ", "'<'",
  "'>'", "GEQ", "LEQ", "'+'", "'-'", "'*'", "'/'", "'%'", "'^'", "'!'",
  "UMINUS", "UPLUS", "MINUSMINUS", "PLUSPLUS", "'['", "']'", "'('", "')'",
  "'.'", "'{'", "'}'", "','", "';'", "$accept", "programlist", "program",
  "constant", "step", "steplist", "variable", "$@1", "function",
  "userfunction", "ARRAYCONST", "assignment", "expression",
  "expressionlist", "variablename", "assignedvariablename",
  "variablelistitem", "variablelist", "typedvariablelistitem",
  "typedvariablelist", "declaration", "functiondeclaration", "$@2", "$@3",
  "statement", "statementlist", "block", "blockment", "flowstatement",
  "$@4", "$@5", "$@6", "caselabel", "caselist", "filteroptions", "filter",
  "savetokenname", "savetype", "cleartype", "pushscope", "popscope",
  "pushstepname", "pushfunc", "pushfilter", "setglobal", "unsetglobal", 0
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
     285,    61,   286,   287,   288,   289,    63,    58,   290,   291,
     292,   293,    60,    62,   294,   295,    43,    45,    42,    47,
      37,    94,    33,   296,   297,   298,   299,    91,    93,    40,
      41,    46,   123,   125,    44,    59
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    66,    67,    67,    68,    68,    69,    69,    69,    69,
      70,    70,    70,    70,    71,    71,    71,    72,    72,    72,
      72,    73,    72,    72,    74,    74,    74,    75,    75,    75,
      76,    77,    77,    77,    78,    78,    78,    78,    78,    78,
      78,    78,    78,    78,    78,    78,    78,    78,    78,    78,
      78,    78,    78,    78,    78,    78,    78,    78,    78,    78,
      78,    78,    78,    78,    78,    79,    79,    79,    80,    80,
      80,    80,    80,    80,    80,    81,    81,    81,    81,    82,
      82,    82,    83,    83,    83,    84,    84,    85,    85,    85,
      86,    86,    86,    87,    88,    87,    87,    89,    87,    90,
      90,    90,    90,    90,    90,    90,    90,    90,    90,    90,
      90,    90,    91,    91,    92,    92,    93,    93,    94,    94,
      94,    94,    95,    94,    96,    94,    94,    94,    97,    94,
      98,    98,    99,    99,    99,   100,   100,   101,   102,   103,
     104,   105,   106,   107,   108,   109,   110,   111
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
       3,     3,     2,     5,     1,     1,     3,     2,     1,     1,
       1,     1,     1,     1,     2,     3,     6,     6,     3,     1,
       4,     1,     1,     3,     2,     3,     5,     1,     3,     2,
       6,     3,     3,     7,     0,     9,     7,     0,     9,     2,
       2,     2,     2,     2,     1,     1,     1,     2,     3,     2,
       2,     2,     1,     2,     5,     2,     1,     1,     7,     5,
      10,    10,     0,     9,     0,    11,     6,     8,     0,     8,
       5,     2,     1,     2,     2,     4,     6,     6,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     6,     9,     7,    64,     8,    18,    20,     0,     0,
     139,   141,   141,   141,     0,     0,   146,     0,   145,     0,
     140,     0,     0,     0,     0,     0,     0,     0,   141,     0,
       2,    34,    46,    35,    36,     0,     0,     0,   105,   112,
       4,     5,   104,   106,     0,     0,    26,     0,    29,     0,
       0,     0,     0,     0,     0,     0,     0,   109,    46,    35,
      36,     0,     0,   107,     0,   110,   111,    41,    62,    45,
      44,     0,   115,     0,     1,     3,     0,     0,     0,     0,
       0,    43,    42,    23,    21,   102,   103,    99,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   101,   100,   113,     0,     0,    24,    65,
       0,    27,     0,    92,   138,    68,    70,    69,    72,    73,
      71,    79,    81,    82,    91,     0,     0,     0,     0,     0,
     139,   108,     0,   144,    61,     0,    33,     0,    32,    31,
      40,    39,    38,    37,     0,     0,    60,    59,    54,    53,
      57,    55,    56,    58,    47,    48,    49,    50,    52,    51,
      17,    19,    25,     0,    67,    28,    74,     0,     0,     0,
     138,     0,    84,     0,     0,   139,     0,     0,     0,   128,
       0,     0,   138,     0,     0,   142,     0,   143,    14,     0,
       0,    66,     0,    75,    78,     0,    83,     0,     0,     0,
       0,     0,     0,     0,   116,   117,   119,   147,     0,     0,
       0,     0,   114,    30,    13,    16,     0,    63,   139,     0,
      87,     0,    80,     0,   126,    79,     0,     0,     0,     0,
       0,    90,     0,   137,   138,     0,     0,     0,     0,    15,
       0,    96,    97,     0,    89,     0,     0,     0,   122,     0,
       0,     0,     0,   132,     0,   118,   135,     0,    93,    94,
       0,    12,     0,    85,     0,    88,    77,    76,   127,     0,
       0,     0,     0,     0,   131,   129,   133,   134,     0,     0,
      10,    11,     0,    98,   124,   123,     0,     0,     0,   136,
      95,    86,     0,   120,   121,     0,   125,   130
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    29,    30,    31,   188,   189,    58,   144,    59,    60,
     138,    35,    36,   110,   121,   122,   123,   124,   220,   221,
      37,    38,   279,   264,   204,    40,   205,   206,    42,   270,
     292,   203,   253,   254,   183,    43,   166,    50,    64,    51,
     212,   214,   167,    62,    56,   231
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -188
static const yytype_int16 yypact[] =
{
     797,  -188,  -188,  -188,  -188,  -188,   -29,   -23,    12,    23,
    -188,  -188,  -188,  -188,   -11,    19,  -188,    11,  -188,    48,
    -188,    -4,    32,   337,   337,    59,    59,   337,    35,   394,
    -188,  -188,   192,    37,    46,    51,   268,    57,  -188,  -188,
     940,  -188,  -188,  -188,   337,   337,  -188,   824,  -188,   857,
     625,    86,    88,    92,   337,   337,   139,  -188,   210,  -188,
    -188,   446,    94,  -188,   148,  -188,  -188,  -188,  -188,    85,
      85,   934,  -188,   940,  -188,  -188,   103,   337,   337,   337,
     337,  -188,  -188,  -188,  -188,  -188,  -188,  -188,   337,   337,
     337,   337,   337,   337,   337,   337,   337,   337,   337,   337,
     337,   337,   337,  -188,  -188,  -188,  1169,  1190,  -188,  1271,
     114,  -188,   593,  -188,    98,  -188,  -188,  -188,  -188,  -188,
    -188,    15,  -188,  -188,   204,   145,   337,    42,   962,   985,
    -188,  -188,   157,  -188,  -188,   736,  -188,   337,  -188,  1271,
    1271,  1271,  1271,  1271,   159,  1255,   385,  1296,  1306,  1306,
      83,    83,    83,    83,   142,   142,  -188,  -188,  -188,  -188,
    -188,  -188,  -188,   337,  1271,  -188,  -188,   109,   498,   337,
    -188,   915,  -188,   112,  1008,  -188,   118,   107,   110,  -188,
     797,   915,  -188,   -15,   117,  -188,   603,  -188,  -188,   530,
     337,  1271,    26,  -188,  1271,  1211,  -188,   337,   797,  1322,
     337,   337,   337,   119,  -188,  -188,   153,   204,   154,    86,
     180,    34,  -188,  -188,    44,  -188,   159,  1271,  -188,    86,
    -188,    24,   164,  1031,  -188,     5,  1054,   670,   731,    72,
     797,  -188,   138,  -188,  -188,    86,    25,   337,   882,  -188,
     915,  -188,  -188,   175,  -188,   498,   131,   337,  -188,   337,
     337,   140,   166,  -188,   675,  -188,  -188,   170,  -188,  -188,
    1232,  -188,   613,   173,    86,  -188,  -188,  1271,  -188,  1077,
     797,  1100,  1123,   337,  -188,  -188,   940,  -188,   138,    86,
    -188,  -188,   337,  -188,  -188,  -188,   797,   797,  1146,  -188,
    -188,  1271,   797,  -188,  -188,   168,  -188,  -188
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -188,  -188,   177,   -43,    13,  -188,     0,  -188,     2,     4,
    -158,    91,   276,   -44,  -180,  -188,  -115,    38,  -186,     9,
     101,  -188,  -188,  -188,     1,   -67,     3,  -187,  -188,  -188,
    -188,  -188,   -33,  -188,  -188,  -188,  -170,  -122,  -188,    67,
    -188,  -188,   100,  -188,  -188,  -188
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -145
static const yytype_int16 yytable[] =
{
      32,    39,    33,    41,    34,   112,   135,   120,   181,   172,
     193,   224,   208,    46,     1,     2,     3,     4,     5,   225,
       6,     7,     8,     9,    48,    69,    70,   247,    44,    32,
      39,    33,    41,    34,    45,   244,   168,   218,   218,   218,
      32,   105,    33,   255,    34,   209,   168,   218,    54,   210,
     244,     6,     7,   199,   125,   175,   196,   265,    23,    63,
     263,    65,   169,    24,   257,    16,    25,    26,     6,     7,
      27,    47,   169,    32,    39,    33,    57,    34,    55,    52,
      53,   120,    49,   285,   242,   259,   219,   266,   243,   243,
     251,   252,   172,   186,   235,    73,   240,    66,    72,   293,
     294,   237,    85,   238,   136,   296,     1,     2,     3,     4,
       5,    86,     6,     7,     8,     9,    87,     1,     2,     3,
       4,     5,   104,     6,     7,     8,     9,   176,   120,    97,
      98,    99,   100,   101,   102,    32,   105,    33,   120,    34,
     200,     1,     2,     3,    83,     5,    84,   126,    28,    76,
      23,   127,   130,   132,   133,    24,   120,  -144,    25,    26,
     173,    23,    27,   182,   120,   137,    24,   187,   192,    25,
      26,   197,   201,    27,   162,   202,   211,    83,   163,    84,
      32,   229,    33,   230,    34,   232,   234,   276,   218,   256,
      99,   100,   101,   102,   262,   245,   268,   120,    32,   273,
      33,   278,    34,   274,   282,   297,    75,     1,     2,     3,
     170,     5,   233,   115,   116,   117,   118,   119,   177,   207,
     236,   277,   241,    76,    77,    78,    79,    80,   178,   239,
      32,     0,    33,   184,    34,   289,     0,     0,   258,     0,
       0,     0,    77,    78,    79,    80,     0,    81,    82,     0,
       0,    83,     0,    84,    32,    39,    33,     0,    34,     0,
       0,     0,     0,     0,     0,    81,    82,   283,   171,    83,
      32,    84,    33,     0,    34,     0,    32,   105,    33,     0,
      34,     0,   290,     0,     0,     0,    32,    32,    33,    33,
      34,    34,    32,    61,    33,     0,    34,     0,     0,    67,
      68,     0,     0,    71,    88,     0,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
     106,   107,     0,   109,     0,   109,     0,     0,     0,     0,
     128,   129,     0,   103,     0,     0,     0,     0,     0,     0,
       1,     2,     3,     4,     5,     0,     6,     7,     8,     9,
       0,     0,   139,   140,   141,   142,   143,     0,     0,     0,
       0,     0,     0,     0,   145,   146,   147,   148,   149,   150,
     151,   152,   153,   154,   155,   156,   157,   158,   159,     0,
       0,     0,     0,     0,    23,     0,   164,     0,   164,    24,
       0,     0,    25,    26,    74,     0,    27,     1,     2,     3,
       4,     5,   174,     6,     7,     8,     9,    10,    11,    12,
      13,    14,     0,   109,    15,     0,     0,    16,    17,    18,
      19,    20,    21,    22,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,     0,     0,   191,
       0,    23,     0,     0,   194,   195,    24,     0,     0,    25,
      26,     0,     0,    27,     0,     0,    28,     0,     0,     0,
       0,     0,   164,     0,     0,     0,   217,     0,     0,     0,
       0,     0,     0,   223,     0,     0,   226,   227,   228,     0,
       0,     0,    88,     0,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,     0,     0,
       0,     1,     2,     3,     4,     5,     0,     6,     7,     8,
       9,   131,     0,   260,   109,     0,     0,     0,     0,     0,
       0,   267,     0,   269,     0,   271,   272,     0,     0,     0,
       0,   215,     0,   -22,   -22,   -22,   -22,   -22,   164,   -22,
     -22,   -22,   -22,   -22,     0,    23,     0,     0,     0,   288,
      24,     0,   -22,    25,    26,     0,     0,    27,   291,     0,
     137,   -22,   -22,   -22,   -22,   -22,   -22,   -22,   -22,   -22,
     -22,   -22,   -22,   -22,   -22,   -22,   -22,   -22,   -22,   -22,
     -22,   -22,   -22,     0,     0,   -22,   -22,     0,   -22,   -22,
     -22,   216,     0,   -22,   -22,   -22,     1,     2,     3,     4,
       5,     0,     6,     7,     8,     9,     1,     2,     3,     4,
       5,     0,     6,     7,     8,     9,     1,     2,     3,     4,
       5,     0,     6,     7,     8,     9,   113,     0,     1,     2,
       3,   114,     5,     0,   115,   116,   117,   118,   119,     0,
      23,     0,     0,     0,     0,    24,     0,     0,    25,    26,
      23,     0,    27,   165,     0,    24,     0,   163,    25,    26,
      23,     0,    27,     0,     0,    24,   213,   163,    25,    26,
       0,     0,    27,   281,     0,     0,     0,   163,     1,     2,
       3,     4,     5,     0,     6,     7,     8,     9,    10,    11,
      12,    13,    14,   251,   252,    15,     0,     0,    16,    17,
      18,    19,    20,    21,    22,     0,    88,     0,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,    23,     0,     0,     0,     0,    24,     0,     0,
      25,    26,     0,     0,    27,   249,     0,     0,   275,     1,
       2,     3,     4,     5,     0,     6,     7,     8,     9,    10,
      11,    12,    13,    14,     0,     0,    15,     0,     0,    16,
      17,    18,    19,    20,    21,    22,     0,    88,     0,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
     100,   101,   102,    23,     0,     0,     0,     0,    24,     0,
       0,    25,    26,     0,     0,    27,   250,     0,     0,   185,
       1,     2,     3,     4,     5,     0,     6,     7,     8,     9,
      10,    11,    12,    13,    14,     0,     0,    15,     0,     0,
      16,    17,    18,    19,    20,    21,    22,     1,     2,     3,
       4,     5,     0,     6,     7,     8,     9,     0,     0,     0,
       0,     0,     0,     0,    23,     0,     0,     0,     0,    24,
       0,     0,    25,    26,     0,     0,    27,     0,     0,    28,
       1,     2,     3,     4,     5,     0,     6,     7,     8,     9,
       0,    23,     0,     0,     0,     0,    24,     0,     0,    25,
      26,     0,     0,    27,   108,     1,     2,     3,     4,     5,
       0,     6,     7,     8,     9,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    23,     0,     0,     0,     0,    24,
       0,     0,    25,    26,     0,     0,    27,   111,     1,     2,
       3,   170,     5,     0,   115,   116,   117,   118,   119,    23,
       0,     0,     0,     0,    24,     0,     0,    25,    26,     0,
       0,    27,   261,     1,     2,     3,     4,     5,     0,     6,
       7,     8,     9,    10,    11,    12,    13,    14,     0,     0,
      15,     0,     0,    16,    17,    18,    19,    20,    21,    22,
      88,     0,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,     0,    23,     0,     0,
       0,     0,    24,     0,   134,    25,    26,     0,    88,    27,
      89,    90,    91,    92,    93,    94,    95,    96,    97,    98,
      99,   100,   101,   102,     0,     0,     0,     0,     0,     0,
       0,    88,   179,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,     0,     0,     0,
       0,     0,     0,     0,    88,   180,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
       0,     0,     0,     0,     0,     0,     0,    88,   198,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
     100,   101,   102,     0,     0,     0,     0,     0,     0,     0,
      88,   246,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,     0,     0,     0,     0,
       0,     0,     0,    88,   248,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,     0,
       0,     0,     0,     0,     0,     0,    88,   284,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,     0,     0,     0,     0,     0,     0,     0,    88,
     286,    89,    90,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,     0,     0,     0,     0,     0,
       0,     0,    88,   287,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,     0,     0,
       0,     0,     0,     0,     0,    88,   295,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,     0,     0,     0,     0,     0,    88,   160,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,     0,     0,     0,     0,     0,    88,   161,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
     100,   101,   102,     0,     0,     0,     0,     0,    88,   222,
      89,    90,    91,    92,    93,    94,    95,    96,    97,    98,
      99,   100,   101,   102,     0,     0,     0,     0,     0,     0,
     280,    88,   190,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,    88,     0,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
     100,   101,   102,   113,     0,     1,     2,     3,   170,     5,
       0,   115,   116,   117,   118,   119,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102
};

static const yytype_int16 yycheck[] =
{
       0,     0,     0,     0,     0,    49,    73,    50,   130,   124,
     168,   198,   182,     1,     3,     4,     5,     6,     7,   199,
       9,    10,    11,    12,     1,    25,    26,    22,    57,    29,
      29,    29,    29,    29,    57,   221,    31,    13,    13,    13,
      40,    40,    40,   230,    40,    60,    31,    13,    59,    64,
     236,     9,    10,   175,    51,    13,   171,   243,    47,    11,
     240,    65,    57,    52,   234,    23,    55,    56,     9,    10,
      59,    59,    57,    73,    73,    73,    65,    73,    59,    12,
      13,   124,    59,   270,    60,    60,    60,   245,    64,    64,
      18,    19,   207,   137,    60,    28,   218,    65,    63,   286,
     287,    57,    65,    59,     1,   292,     3,     4,     5,     6,
       7,    65,     9,    10,    11,    12,    65,     3,     4,     5,
       6,     7,    65,     9,    10,    11,    12,   127,   171,    46,
      47,    48,    49,    50,    51,   135,   135,   135,   181,   135,
      22,     3,     4,     5,    59,     7,    61,    59,    62,    31,
      47,    59,    13,    59,     6,    52,   199,    59,    55,    56,
      15,    47,    59,     6,   207,    62,    52,     8,    59,    55,
      56,    59,    65,    59,    60,    65,    59,    59,    64,    61,
     180,    62,   180,    30,   180,    31,     6,   254,    13,   232,
      48,    49,    50,    51,   238,    31,    65,   240,   198,    59,
     198,    31,   198,    37,    31,    37,    29,     3,     4,     5,
       6,     7,   209,     9,    10,    11,    12,    13,   127,   181,
     211,   254,   219,    31,    32,    33,    34,    35,   127,   216,
     230,    -1,   230,   133,   230,   278,    -1,    -1,   235,    -1,
      -1,    -1,    32,    33,    34,    35,    -1,    55,    56,    -1,
      -1,    59,    -1,    61,   254,   254,   254,    -1,   254,    -1,
      -1,    -1,    -1,    -1,    -1,    55,    56,   264,    64,    59,
     270,    61,   270,    -1,   270,    -1,   276,   276,   276,    -1,
     276,    -1,   279,    -1,    -1,    -1,   286,   287,   286,   287,
     286,   287,   292,    17,   292,    -1,   292,    -1,    -1,    23,
      24,    -1,    -1,    27,    36,    -1,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      44,    45,    -1,    47,    -1,    49,    -1,    -1,    -1,    -1,
      54,    55,    -1,    65,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,    -1,     9,    10,    11,    12,
      -1,    -1,    76,    77,    78,    79,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,    -1,
      -1,    -1,    -1,    -1,    47,    -1,   110,    -1,   112,    52,
      -1,    -1,    55,    56,     0,    -1,    59,     3,     4,     5,
       6,     7,   126,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,   137,    20,    -1,    -1,    23,    24,    25,
      26,    27,    28,    29,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    -1,    -1,   163,
      -1,    47,    -1,    -1,   168,   169,    52,    -1,    -1,    55,
      56,    -1,    -1,    59,    -1,    -1,    62,    -1,    -1,    -1,
      -1,    -1,   186,    -1,    -1,    -1,   190,    -1,    -1,    -1,
      -1,    -1,    -1,   197,    -1,    -1,   200,   201,   202,    -1,
      -1,    -1,    36,    -1,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    -1,    -1,
      -1,     3,     4,     5,     6,     7,    -1,     9,    10,    11,
      12,    65,    -1,   237,   238,    -1,    -1,    -1,    -1,    -1,
      -1,   245,    -1,   247,    -1,   249,   250,    -1,    -1,    -1,
      -1,     1,    -1,     3,     4,     5,     6,     7,   262,     9,
      10,    11,    12,    13,    -1,    47,    -1,    -1,    -1,   273,
      52,    -1,    22,    55,    56,    -1,    -1,    59,   282,    -1,
      62,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    55,    56,    -1,    58,    59,
      60,    61,    -1,    63,    64,    65,     3,     4,     5,     6,
       7,    -1,     9,    10,    11,    12,     3,     4,     5,     6,
       7,    -1,     9,    10,    11,    12,     3,     4,     5,     6,
       7,    -1,     9,    10,    11,    12,     1,    -1,     3,     4,
       5,     6,     7,    -1,     9,    10,    11,    12,    13,    -1,
      47,    -1,    -1,    -1,    -1,    52,    -1,    -1,    55,    56,
      47,    -1,    59,    60,    -1,    52,    -1,    64,    55,    56,
      47,    -1,    59,    -1,    -1,    52,    63,    64,    55,    56,
      -1,    -1,    59,    60,    -1,    -1,    -1,    64,     3,     4,
       5,     6,     7,    -1,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    -1,    -1,    23,    24,
      25,    26,    27,    28,    29,    -1,    36,    -1,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    47,    -1,    -1,    -1,    -1,    52,    -1,    -1,
      55,    56,    -1,    -1,    59,    65,    -1,    -1,    63,     3,
       4,     5,     6,     7,    -1,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    -1,    23,
      24,    25,    26,    27,    28,    29,    -1,    36,    -1,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    47,    -1,    -1,    -1,    -1,    52,    -1,
      -1,    55,    56,    -1,    -1,    59,    65,    -1,    -1,    63,
       3,     4,     5,     6,     7,    -1,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    -1,
      23,    24,    25,    26,    27,    28,    29,     3,     4,     5,
       6,     7,    -1,     9,    10,    11,    12,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    47,    -1,    -1,    -1,    -1,    52,
      -1,    -1,    55,    56,    -1,    -1,    59,    -1,    -1,    62,
       3,     4,     5,     6,     7,    -1,     9,    10,    11,    12,
      -1,    47,    -1,    -1,    -1,    -1,    52,    -1,    -1,    55,
      56,    -1,    -1,    59,    60,     3,     4,     5,     6,     7,
      -1,     9,    10,    11,    12,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    47,    -1,    -1,    -1,    -1,    52,
      -1,    -1,    55,    56,    -1,    -1,    59,    60,     3,     4,
       5,     6,     7,    -1,     9,    10,    11,    12,    13,    47,
      -1,    -1,    -1,    -1,    52,    -1,    -1,    55,    56,    -1,
      -1,    59,    60,     3,     4,     5,     6,     7,    -1,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    -1,
      20,    -1,    -1,    23,    24,    25,    26,    27,    28,    29,
      36,    -1,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    -1,    47,    -1,    -1,
      -1,    -1,    52,    -1,    60,    55,    56,    -1,    36,    59,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    36,    60,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    36,    60,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    36,    60,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      36,    60,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    36,    60,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    36,    60,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    36,
      60,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    36,    60,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    36,    60,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    -1,    -1,    -1,    -1,    -1,    36,    58,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    -1,    -1,    -1,    -1,    -1,    36,    58,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    -1,    -1,    -1,    -1,    -1,    36,    58,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    -1,    -1,    -1,    -1,    -1,    -1,
      58,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    36,    -1,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     1,    -1,     3,     4,     5,     6,     7,
      -1,     9,    10,    11,    12,    13,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     5,     6,     7,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    20,    23,    24,    25,    26,
      27,    28,    29,    47,    52,    55,    56,    59,    62,    67,
      68,    69,    72,    74,    75,    77,    78,    86,    87,    90,
      91,    92,    94,   101,    57,    57,     1,    59,     1,    59,
     103,   105,   105,   105,    59,    59,   110,    65,    72,    74,
      75,    78,   109,    11,   104,    65,    65,    78,    78,    72,
      72,    78,    63,   105,     0,    68,    31,    32,    33,    34,
      35,    55,    56,    59,    61,    65,    65,    65,    36,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    65,    65,    90,    78,    78,    60,    78,
      79,    60,    79,     1,     6,     9,    10,    11,    12,    13,
      69,    80,    81,    82,    83,    92,    59,    59,    78,    78,
      13,    65,    59,     6,    60,    91,     1,    62,    76,    78,
      78,    78,    78,    78,    73,    78,    78,    78,    78,    78,
      78,    78,    78,    78,    78,    78,    78,    78,    78,    78,
      58,    58,    60,    64,    78,    60,   102,   108,    31,    57,
       6,    64,    82,    15,    78,    13,    72,    77,    86,    60,
      60,   103,     6,   100,   108,    63,    79,     8,    70,    71,
      37,    78,    59,    76,    78,    78,    82,    59,    60,   103,
      22,    65,    65,    97,    90,    92,    93,    83,   102,    60,
      64,    59,   106,    63,   107,     1,    61,    78,    13,    60,
      84,    85,    58,    78,    93,    80,    78,    78,    78,    62,
      30,   111,    31,    92,     6,    60,    85,    57,    59,    70,
     103,    92,    60,    64,    84,    31,    60,    22,    60,    65,
      65,    18,    19,    98,    99,    93,    69,   102,    92,    60,
      78,    60,    79,    80,    89,    84,    76,    78,    65,    78,
      95,    78,    78,    59,    37,    63,    91,    98,    31,    88,
      58,    60,    31,    92,    60,    93,    60,    60,    78,    69,
      92,    78,    96,    93,    93,    60,    93,    37
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
#line 90 "grammar.yy"
    { }
    break;

  case 3:

/* Line 1455 of yacc.c  */
#line 91 "grammar.yy"
    { }
    break;

  case 4:

/* Line 1455 of yacc.c  */
#line 96 "grammar.yy"
    {
		if (((yyvsp[(1) - (1)].node) != NULL) && (!cmdparser.addStatement((yyvsp[(1) - (1)].node)))) YYABORT;
		}
    break;

  case 5:

/* Line 1455 of yacc.c  */
#line 99 "grammar.yy"
    {
		if (((yyvsp[(1) - (1)].node) != NULL) && (!cmdparser.addStatement((yyvsp[(1) - (1)].node)))) YYABORT;
		}
    break;

  case 6:

/* Line 1455 of yacc.c  */
#line 109 "grammar.yy"
    { (yyval.node) = cmdparser.addConstant((yyvsp[(1) - (1)].intconst)); }
    break;

  case 7:

/* Line 1455 of yacc.c  */
#line 110 "grammar.yy"
    { (yyval.node) = cmdparser.addConstant((yyvsp[(1) - (1)].doubleconst)); }
    break;

  case 8:

/* Line 1455 of yacc.c  */
#line 111 "grammar.yy"
    { (yyval.node) = cmdparser.addConstant((yyvsp[(1) - (1)].name)->get()); }
    break;

  case 9:

/* Line 1455 of yacc.c  */
#line 112 "grammar.yy"
    { (yyval.node) = cmdparser.addElementConstant((yyvsp[(1) - (1)].intconst)); }
    break;

  case 10:

/* Line 1455 of yacc.c  */
#line 121 "grammar.yy"
    {
		if (!cmdparser.expandPath(stepNameStack.last(), (yyvsp[(4) - (5)].node))) YYABORT;
		stepNameStack.removeLast();
		}
    break;

  case 11:

/* Line 1455 of yacc.c  */
#line 125 "grammar.yy"
    {
		if (!cmdparser.expandPath(stepNameStack.last(), NULL, (yyvsp[(4) - (5)].node))) YYABORT;
		stepNameStack.removeLast();
		}
    break;

  case 12:

/* Line 1455 of yacc.c  */
#line 129 "grammar.yy"
    {
		if (!cmdparser.expandPath(stepNameStack.last(), NULL, NULL)) YYABORT;
		stepNameStack.removeLast();
		}
    break;

  case 13:

/* Line 1455 of yacc.c  */
#line 133 "grammar.yy"
    {
		if (!cmdparser.expandPath((yyvsp[(1) - (2)].name))) YYABORT;
		stepNameStack.removeLast();
		}
    break;

  case 14:

/* Line 1455 of yacc.c  */
#line 141 "grammar.yy"
    { }
    break;

  case 15:

/* Line 1455 of yacc.c  */
#line 142 "grammar.yy"
    { }
    break;

  case 16:

/* Line 1455 of yacc.c  */
#line 143 "grammar.yy"
    { msg.print("Error formulating path.\n"); YYABORT; }
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 148 "grammar.yy"
    {
		(yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (4)].variable),(yyvsp[(3) - (4)].node));
		if ((yyval.node) == NULL) { msg.print("Error in variable expression (code 1)\n"); YYABORT; }
		}
    break;

  case 18:

/* Line 1455 of yacc.c  */
#line 152 "grammar.yy"
    {
		(yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (1)].variable));
		if ((yyval.node) == NULL) { msg.print("Error in variable expression (code 2)\n"); YYABORT; }
		}
    break;

  case 19:

/* Line 1455 of yacc.c  */
#line 156 "grammar.yy"
    {
		(yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (4)].variable),(yyvsp[(3) - (4)].node));
		if ((yyval.node) == NULL) { msg.print("Error in variable expression (code 3)\n"); YYABORT; }
		}
    break;

  case 20:

/* Line 1455 of yacc.c  */
#line 160 "grammar.yy"
    {
		(yyval.node) = cmdparser.wrapVariable((yyvsp[(1) - (1)].variable));
		if ((yyval.node) == NULL) { msg.print("Error in variable expression (code 4)\n"); YYABORT; }
		}
    break;

  case 21:

/* Line 1455 of yacc.c  */
#line 164 "grammar.yy"
    {
		cmdparser.createPath((yyvsp[(1) - (2)].node));
		}
    break;

  case 22:

/* Line 1455 of yacc.c  */
#line 166 "grammar.yy"
    {
		(yyval.node) = cmdparser.finalisePath();
		}
    break;

  case 23:

/* Line 1455 of yacc.c  */
#line 169 "grammar.yy"
    {
		msg.print("Can't use a variable as a function. Did you mean '[' instead?\n"); (yyval.node) = NULL;
		}
    break;

  case 24:

/* Line 1455 of yacc.c  */
#line 180 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction( (Command::Function) (yyvsp[(1) - (3)].functionId));
		if ((yyval.node) == NULL) YYABORT;
		msg.print(Messenger::Parse,"PARSER : function : function '%s'\n", commands.data[(Command::Function) (yyvsp[(1) - (3)].functionId)].keyword);
		}
    break;

  case 25:

/* Line 1455 of yacc.c  */
#line 185 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunctionWithArglist( (Command::Function) (yyvsp[(1) - (4)].functionId),(yyvsp[(3) - (4)].node));
		if ((yyval.node) == NULL) YYABORT;
		msg.print(Messenger::Parse,"PARSER : function : function '%s' with exprlist\n", commands.data[(Command::Function) (yyvsp[(1) - (4)].functionId)].keyword);
		}
    break;

  case 26:

/* Line 1455 of yacc.c  */
#line 190 "grammar.yy"
    {
		msg.print("Error: Missing brackets after function call?\n");
		YYABORT;
		}
    break;

  case 27:

/* Line 1455 of yacc.c  */
#line 198 "grammar.yy"
    {
		(yyval.node) = cmdparser.addUserFunction((yyvsp[(1) - (3)].tree));
		if ((yyval.node) == NULL) YYABORT;
		msg.print(Messenger::Parse,"PARSER : userfunction : function '%s'\n", (yyvsp[(1) - (3)].tree)->name());
		}
    break;

  case 28:

/* Line 1455 of yacc.c  */
#line 203 "grammar.yy"
    {
		(yyval.node) = cmdparser.addUserFunction((yyvsp[(1) - (4)].tree),(yyvsp[(3) - (4)].node));
		if ((yyval.node) == NULL) YYABORT;
		msg.print(Messenger::Parse,"PARSER : userfunction : function '%s' with expressionlist\n", (yyvsp[(1) - (4)].tree)->name());
		}
    break;

  case 29:

/* Line 1455 of yacc.c  */
#line 208 "grammar.yy"
    {
		msg.print("Error: Missing brackets after function call?\n");
		YYABORT;
		}
    break;

  case 30:

/* Line 1455 of yacc.c  */
#line 220 "grammar.yy"
    {
		(yyval.node) = cmdparser.addArrayConstant((yyvsp[(2) - (3)].node));
		if ((yyval.node) == NULL) YYABORT;
		}
    break;

  case 31:

/* Line 1455 of yacc.c  */
#line 231 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignment,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 32:

/* Line 1455 of yacc.c  */
#line 232 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignment,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 33:

/* Line 1455 of yacc.c  */
#line 233 "grammar.yy"
    { msg.print("Mangled expression used in assignment.\n"); YYABORT; }
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 238 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 35:

/* Line 1455 of yacc.c  */
#line 239 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 36:

/* Line 1455 of yacc.c  */
#line 240 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 37:

/* Line 1455 of yacc.c  */
#line 241 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignmentPlus,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 242 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignmentSubtract,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 243 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignmentMultiply,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 40:

/* Line 1455 of yacc.c  */
#line 244 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAssignmentDivide,(yyvsp[(1) - (3)].node),(yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 41:

/* Line 1455 of yacc.c  */
#line 245 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorNegate, (yyvsp[(2) - (2)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 246 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPostfixIncrease, (yyvsp[(1) - (2)].node));  if ((yyval.node) == NULL) YYABORT; }
    break;

  case 43:

/* Line 1455 of yacc.c  */
#line 247 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPostfixDecrease, (yyvsp[(1) - (2)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 44:

/* Line 1455 of yacc.c  */
#line 248 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPrefixIncrease, (yyvsp[(2) - (2)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 45:

/* Line 1455 of yacc.c  */
#line 249 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPrefixDecrease, (yyvsp[(2) - (2)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 250 "grammar.yy"
    { (yyval.node) = (yyvsp[(1) - (1)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 251 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAdd, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 252 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorSubtract, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 49:

/* Line 1455 of yacc.c  */
#line 253 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorMultiply, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 50:

/* Line 1455 of yacc.c  */
#line 254 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorDivide, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 51:

/* Line 1455 of yacc.c  */
#line 255 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorPower, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 52:

/* Line 1455 of yacc.c  */
#line 256 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorModulus, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 53:

/* Line 1455 of yacc.c  */
#line 257 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 54:

/* Line 1455 of yacc.c  */
#line 258 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorNotEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 259 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorGreaterThan, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 56:

/* Line 1455 of yacc.c  */
#line 260 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorGreaterThanEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 57:

/* Line 1455 of yacc.c  */
#line 261 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorLessThan, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 58:

/* Line 1455 of yacc.c  */
#line 262 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorLessThanEqualTo, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 59:

/* Line 1455 of yacc.c  */
#line 263 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorAnd, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 60:

/* Line 1455 of yacc.c  */
#line 264 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorOr, (yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 61:

/* Line 1455 of yacc.c  */
#line 265 "grammar.yy"
    { (yyval.node) = (yyvsp[(2) - (3)].node); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 62:

/* Line 1455 of yacc.c  */
#line 266 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorNot, (yyvsp[(2) - (2)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 63:

/* Line 1455 of yacc.c  */
#line 267 "grammar.yy"
    { (yyval.node) = cmdparser.addOperator(Command::OperatorInlineIf, (yyvsp[(1) - (5)].node), (yyvsp[(3) - (5)].node), (yyvsp[(5) - (5)].node)); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 268 "grammar.yy"
    { msg.print("Error: '%s' has not been declared as a function or a variable.\n", yylval.name->get()); YYABORT; }
    break;

  case 65:

/* Line 1455 of yacc.c  */
#line 273 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		if ((yyval.node) == NULL) YYABORT;
		}
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 277 "grammar.yy"
    {
		(yyval.node) = Tree::joinArguments((yyvsp[(3) - (3)].node),(yyvsp[(1) - (3)].node));
		}
    break;

  case 67:

/* Line 1455 of yacc.c  */
#line 280 "grammar.yy"
    {
		msg.print("Error: Missing comma between items.\n");
		YYABORT;
		}
    break;

  case 68:

/* Line 1455 of yacc.c  */
#line 292 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : variablename : existing var '%s'\n", tokenName.get());
		tokenName = yylval.variable->name();
		(yyval.name) = &tokenName;
		}
    break;

  case 69:

/* Line 1455 of yacc.c  */
#line 297 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : variablename : existing built-in function '%s'\n", tokenName.get());
		tokenName = Command::data[yylval.functionId].keyword;
		(yyval.name) = &tokenName;
		}
    break;

  case 70:

/* Line 1455 of yacc.c  */
#line 302 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : variablename : existing local var '%s'\n", tokenName.get());
		msg.print("Error: Existing variable in local scope cannot be redeclared.\n");
		YYABORT;
		}
    break;

  case 71:

/* Line 1455 of yacc.c  */
#line 307 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : variablename : constant '%s'\n", tokenName.get());
		msg.print("Error: Constant value found in declaration.\n");
		YYABORT;
		}
    break;

  case 72:

/* Line 1455 of yacc.c  */
#line 312 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : variablename : existing user function '%s'\n", tokenName.get());
		msg.print("Error: Existing user-defined function name cannot be redeclared.\n");
		YYABORT;
		}
    break;

  case 73:

/* Line 1455 of yacc.c  */
#line 317 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : variablename : variable type-name '%s'\n", VTypes::dataType( yylval.vtype));
		msg.print("Error: Type-name used in variable declaration.\n");
		YYABORT;
		}
    break;

  case 74:

/* Line 1455 of yacc.c  */
#line 322 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : variablename : new token '%s'\n", tokenName.get());
		if (declaredType == VTypes::NoData) { msg.print("Token '%s' is undeclared.\n", tokenName.get()); YYABORT; }
		(yyval.name) = (yyvsp[(1) - (2)].name);
		}
    break;

  case 75:

/* Line 1455 of yacc.c  */
#line 331 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : assignedvariablename : var '%s' with array assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addVariable(declaredType, &tokenName, (yyvsp[(3) - (3)].node), globalDeclarations);
		}
    break;

  case 76:

/* Line 1455 of yacc.c  */
#line 335 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : assignedvariablename : array var '%s' with expr assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addArrayVariable(declaredType, &tokenName, (yyvsp[(3) - (6)].node), (yyvsp[(6) - (6)].node), globalDeclarations);
		}
    break;

  case 77:

/* Line 1455 of yacc.c  */
#line 339 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : assignedvariablename : array var '%s' with array assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addArrayVariable(declaredType, &tokenName, (yyvsp[(3) - (6)].node), (yyvsp[(6) - (6)].node), globalDeclarations);
		}
    break;

  case 78:

/* Line 1455 of yacc.c  */
#line 343 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : assignedvariablename : var '%s' with expr assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addVariable(declaredType, &tokenName, (yyvsp[(3) - (3)].node), globalDeclarations);
		}
    break;

  case 79:

/* Line 1455 of yacc.c  */
#line 351 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : assignedvariablename : var '%s'\n", tokenName.get());
		(yyval.node) = cmdparser.addVariable(declaredType, &tokenName, NULL, globalDeclarations);
		}
    break;

  case 80:

/* Line 1455 of yacc.c  */
#line 355 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : assignedvariablename : array var '%s'\n", tokenName.get());
		(yyval.node) = cmdparser.addArrayVariable(declaredType, &tokenName, (yyvsp[(3) - (4)].node), NULL, globalDeclarations);
		}
    break;

  case 81:

/* Line 1455 of yacc.c  */
#line 359 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 82:

/* Line 1455 of yacc.c  */
#line 366 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 83:

/* Line 1455 of yacc.c  */
#line 369 "grammar.yy"
    {
		(yyval.node) = Tree::joinArguments((yyvsp[(3) - (3)].node),(yyvsp[(1) - (3)].node));
		}
    break;

  case 84:

/* Line 1455 of yacc.c  */
#line 372 "grammar.yy"
    {
		msg.print("Error: Missing comma between declarations?\n");
		YYABORT;
		}
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 380 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : typedvariablelistitem : var '%s'\n", tokenName.get());
		(yyval.node) = cmdparser.addVariable(declaredType, &tokenName);
		}
    break;

  case 86:

/* Line 1455 of yacc.c  */
#line 384 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : typedvariablelistitem : var '%s' with expr assignment\n", tokenName.get());
		(yyval.node) = cmdparser.addVariable(declaredType, &tokenName, (yyvsp[(5) - (5)].node));
		}
    break;

  case 87:

/* Line 1455 of yacc.c  */
#line 392 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 88:

/* Line 1455 of yacc.c  */
#line 395 "grammar.yy"
    {
		(yyval.node) = Tree::joinArguments((yyvsp[(3) - (3)].node),(yyvsp[(1) - (3)].node));
		}
    break;

  case 89:

/* Line 1455 of yacc.c  */
#line 398 "grammar.yy"
    {
		msg.print("Error: Missing comma between declarations?\n");
		YYABORT;
		}
    break;

  case 90:

/* Line 1455 of yacc.c  */
#line 406 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : global declaration : standard variable declaration list\n");
		(yyval.node) = cmdparser.addDeclarations((yyvsp[(5) - (6)].node));
		declaredType = VTypes::NoData;
		}
    break;

  case 91:

/* Line 1455 of yacc.c  */
#line 411 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : declaration : standard variable declaration list\n");
		(yyval.node) = cmdparser.addDeclarations((yyvsp[(3) - (3)].node));
		declaredType = VTypes::NoData;
		}
    break;

  case 92:

/* Line 1455 of yacc.c  */
#line 416 "grammar.yy"
    {
		msg.print("Illegal use of reserved word '%s'.\n", VTypes::dataType(declaredType));
		YYABORT;
		}
    break;

  case 93:

/* Line 1455 of yacc.c  */
#line 428 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : functiondeclaration : user-defined subroutine (VOID return value, no arguments)\n");
		if (!cmdparser.addStatement((yyvsp[(7) - (7)].node))) YYABORT;
		cmdparser.popTree();
		declaredType = VTypes::NoData;
		}
    break;

  case 94:

/* Line 1455 of yacc.c  */
#line 434 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : functiondeclaration : user-defined subroutine (VOID return value, arguments)\n");
		if (!(yyvsp[(4) - (7)].tree)->addLocalFunctionArguments((yyvsp[(6) - (7)].node))) YYABORT;
		}
    break;

  case 95:

/* Line 1455 of yacc.c  */
#line 437 "grammar.yy"
    {
		if (!cmdparser.addStatement((yyvsp[(9) - (9)].node))) YYABORT;
		cmdparser.popTree();
		declaredType = VTypes::NoData;
		}
    break;

  case 96:

/* Line 1455 of yacc.c  */
#line 442 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : functiondeclaration : user-defined function (%s return value, no arguments)\n", VTypes::dataType((yyvsp[(4) - (7)].tree)->returnType()));
		if (!cmdparser.addStatement((yyvsp[(7) - (7)].node))) YYABORT;
		cmdparser.popTree();
		declaredType = VTypes::NoData;
		}
    break;

  case 97:

/* Line 1455 of yacc.c  */
#line 448 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : functiondeclaration : user-defined function (%s return value, arguments)\n", VTypes::dataType((yyvsp[(4) - (7)].tree)->returnType()));
		if (!(yyvsp[(4) - (7)].tree)->addLocalFunctionArguments((yyvsp[(6) - (7)].node))) YYABORT;
		}
    break;

  case 98:

/* Line 1455 of yacc.c  */
#line 451 "grammar.yy"
    {
		if (!cmdparser.addStatement((yyvsp[(9) - (9)].node))) YYABORT;
		cmdparser.popTree();
		declaredType = VTypes::NoData;
		}
    break;

  case 99:

/* Line 1455 of yacc.c  */
#line 464 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (2)].node);
		}
    break;

  case 100:

/* Line 1455 of yacc.c  */
#line 467 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (2)].node);
		}
    break;

  case 101:

/* Line 1455 of yacc.c  */
#line 470 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (2)].node);
		}
    break;

  case 102:

/* Line 1455 of yacc.c  */
#line 473 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (2)].node);
		}
    break;

  case 103:

/* Line 1455 of yacc.c  */
#line 476 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (2)].node);
		}
    break;

  case 104:

/* Line 1455 of yacc.c  */
#line 479 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 105:

/* Line 1455 of yacc.c  */
#line 482 "grammar.yy"
    {
		(yyval.node) = NULL;
		}
    break;

  case 106:

/* Line 1455 of yacc.c  */
#line 485 "grammar.yy"
    {
		(yyval.node) = NULL;
		}
    break;

  case 107:

/* Line 1455 of yacc.c  */
#line 488 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::Help, cmdparser.addConstant((yyvsp[(2) - (2)].functionId)));
		}
    break;

  case 108:

/* Line 1455 of yacc.c  */
#line 491 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::Return,(yyvsp[(2) - (3)].node));
		}
    break;

  case 109:

/* Line 1455 of yacc.c  */
#line 494 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::Return);
		}
    break;

  case 110:

/* Line 1455 of yacc.c  */
#line 497 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::Continue);
		}
    break;

  case 111:

/* Line 1455 of yacc.c  */
#line 500 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::Break);
		}
    break;

  case 112:

/* Line 1455 of yacc.c  */
#line 507 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 113:

/* Line 1455 of yacc.c  */
#line 510 "grammar.yy"
    {
		if ((yyvsp[(2) - (2)].node) == NULL) (yyval.node) = (yyvsp[(1) - (2)].node);
		else (yyval.node) = cmdparser.joinCommands((yyvsp[(1) - (2)].node), (yyvsp[(2) - (2)].node));
		}
    break;

  case 114:

/* Line 1455 of yacc.c  */
#line 518 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(3) - (5)].node);
		}
    break;

  case 115:

/* Line 1455 of yacc.c  */
#line 521 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::NoFunction);
		}
    break;

  case 116:

/* Line 1455 of yacc.c  */
#line 528 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 117:

/* Line 1455 of yacc.c  */
#line 531 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 118:

/* Line 1455 of yacc.c  */
#line 538 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::If,(yyvsp[(3) - (7)].node),(yyvsp[(5) - (7)].node),(yyvsp[(7) - (7)].node));
		}
    break;

  case 119:

/* Line 1455 of yacc.c  */
#line 541 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::If,(yyvsp[(3) - (5)].node),(yyvsp[(5) - (5)].node));
		}
    break;

  case 120:

/* Line 1455 of yacc.c  */
#line 544 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (10)].node), cmdparser.addFunction(Command::For, (yyvsp[(4) - (10)].node),(yyvsp[(6) - (10)].node),(yyvsp[(8) - (10)].node),(yyvsp[(10) - (10)].node))); cmdparser.popScope();
		}
    break;

  case 121:

/* Line 1455 of yacc.c  */
#line 547 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (10)].node), cmdparser.addFunction(Command::For, (yyvsp[(4) - (10)].node),(yyvsp[(6) - (10)].node),(yyvsp[(8) - (10)].node),(yyvsp[(10) - (10)].node))); cmdparser.popScope();
		}
    break;

  case 122:

/* Line 1455 of yacc.c  */
#line 550 "grammar.yy"
    {
		if ((yyvsp[(4) - (7)].node)->returnType() <= VTypes::VectorData) { msg.print("Error: For/In loop variable must be of pointer type.\n"); YYABORT; }
		if ((yyvsp[(4) - (7)].node)->returnType() != (yyvsp[(6) - (7)].node)->returnType()) { msg.print("Error: For/In loop variable is not being assigned the correct type.\n"); YYABORT; }
		}
    break;

  case 123:

/* Line 1455 of yacc.c  */
#line 553 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (9)].node), cmdparser.addFunction(Command::ForIn,(yyvsp[(4) - (9)].node),(yyvsp[(6) - (9)].node),(yyvsp[(9) - (9)].node)));
		cmdparser.popScope();
		}
    break;

  case 124:

/* Line 1455 of yacc.c  */
#line 557 "grammar.yy"
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

  case 125:

/* Line 1455 of yacc.c  */
#line 569 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (11)].node), cmdparser.addFunction(Command::ForIn,tempNode,(yyvsp[(8) - (11)].node),(yyvsp[(11) - (11)].node)));
		cmdparser.popScope();
		}
    break;

  case 126:

/* Line 1455 of yacc.c  */
#line 573 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (6)].node), cmdparser.addFunction(Command::While, (yyvsp[(4) - (6)].node),(yyvsp[(6) - (6)].node)));
		cmdparser.popScope();
		}
    break;

  case 127:

/* Line 1455 of yacc.c  */
#line 577 "grammar.yy"
    {
		(yyval.node) = cmdparser.joinCommands((yyvsp[(2) - (8)].node), cmdparser.addFunction(Command::DoWhile, (yyvsp[(3) - (8)].node),(yyvsp[(6) - (8)].node)));
		cmdparser.popScope();
		}
    break;

  case 128:

/* Line 1455 of yacc.c  */
#line 581 "grammar.yy"
    {
		if (((yyvsp[(3) - (4)].node)->returnType() != VTypes::IntegerData) && ((yyvsp[(3) - (4)].node)->returnType() != VTypes::StringData))
		{
			msg.print("Error: Switch value must be of integer or string type.\n");
			YYABORT;
		}
		}
    break;

  case 129:

/* Line 1455 of yacc.c  */
#line 587 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::Switch, (yyvsp[(3) - (8)].node));
		(yyval.node)->addJoinedArguments((yyvsp[(7) - (8)].node));
		}
    break;

  case 130:

/* Line 1455 of yacc.c  */
#line 595 "grammar.yy"
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

  case 131:

/* Line 1455 of yacc.c  */
#line 604 "grammar.yy"
    {
		(yyval.node) = cmdparser.addFunction(Command::Default);
		}
    break;

  case 132:

/* Line 1455 of yacc.c  */
#line 611 "grammar.yy"
    {
		(yyval.node) = (yyvsp[(1) - (1)].node);
		}
    break;

  case 133:

/* Line 1455 of yacc.c  */
#line 614 "grammar.yy"
    {
		(yyval.node) = Tree::joinArguments((yyvsp[(2) - (2)].node),(yyvsp[(1) - (2)].node));
		}
    break;

  case 134:

/* Line 1455 of yacc.c  */
#line 617 "grammar.yy"
    {
		(yyval.node) = Tree::joinArguments((yyvsp[(2) - (2)].node),(yyvsp[(1) - (2)].node));
		}
    break;

  case 135:

/* Line 1455 of yacc.c  */
#line 628 "grammar.yy"
    {
		if (!cmdparser.setFilterOption(&tokenName, (yyvsp[(4) - (4)].node))) YYABORT;
		msg.print(Messenger::Parse,"PARSER : filteroptions : filter option '%s'\n", tokenName.get());
		}
    break;

  case 136:

/* Line 1455 of yacc.c  */
#line 632 "grammar.yy"
    {
		if (!cmdparser.setFilterOption(&tokenName, (yyvsp[(6) - (6)].node))) YYABORT;
		msg.print(Messenger::Parse,"PARSER : filteroptions : filter option '%s'\n", tokenName.get());
		}
    break;

  case 137:

/* Line 1455 of yacc.c  */
#line 640 "grammar.yy"
    {
		if (((yyvsp[(6) - (6)].node) != NULL) && (!cmdparser.addStatement((yyvsp[(6) - (6)].node)))) YYABORT;
		cmdparser.popTree();
		msg.print(Messenger::Parse,"PARSER : completed filter definition\n");
		}
    break;

  case 138:

/* Line 1455 of yacc.c  */
#line 652 "grammar.yy"
    { tokenName = *yylval.name; }
    break;

  case 139:

/* Line 1455 of yacc.c  */
#line 656 "grammar.yy"
    { declaredType = yylval.vtype; }
    break;

  case 140:

/* Line 1455 of yacc.c  */
#line 660 "grammar.yy"
    { declaredType = VTypes::NoData; }
    break;

  case 141:

/* Line 1455 of yacc.c  */
#line 664 "grammar.yy"
    { (yyval.node) = cmdparser.pushScope(); if ((yyval.node) == NULL) YYABORT; }
    break;

  case 142:

/* Line 1455 of yacc.c  */
#line 668 "grammar.yy"
    { if (!cmdparser.popScope()) YYABORT; }
    break;

  case 143:

/* Line 1455 of yacc.c  */
#line 672 "grammar.yy"
    { stepNameStack.add()->set(yylval.name->get()); }
    break;

  case 144:

/* Line 1455 of yacc.c  */
#line 676 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : pushfunc : function/statement '%s'\n", yylval.name->get());
		(yyval.tree) = cmdparser.pushFunction(yylval.name->get(), declaredType);
		/*cmdparser.pushScope();*/
		}
    break;

  case 145:

/* Line 1455 of yacc.c  */
#line 684 "grammar.yy"
    {
		msg.print(Messenger::Parse,"PARSER : pushfilter : new filter definition\n");
		cmdparser.pushFilter();
		}
    break;

  case 146:

/* Line 1455 of yacc.c  */
#line 691 "grammar.yy"
    { globalDeclarations = TRUE; }
    break;

  case 147:

/* Line 1455 of yacc.c  */
#line 695 "grammar.yy"
    { globalDeclarations = FALSE; }
    break;



/* Line 1455 of yacc.c  */
#line 3241 "grammar.cc"
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
#line 698 "grammar.yy"


void yyerror(char *s)
{
}

