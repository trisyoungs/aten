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
#define yyparse         NetaParser_parse
#define yylex           NetaParser_lex
#define yyerror         NetaParser_error
#define yylval          NetaParser_lval
#define yychar          NetaParser_char
#define yydebug         NetaParser_debug
#define yynerrs         NetaParser_nerrs

/* Copy the first part of user declarations.  */
/* Line 371 of yacc.c  */
#line 4 "neta_grammar.yy"


/* Includes */
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "classes/neta.h"
#include "classes/neta_parser.h"

/* Prototypes */
int NetaParser_lex(void);
void NetaParser_error(char *s);

/* Local Variables */
Neta::NetaValue savedval;


/* Line 371 of yacc.c  */
#line 93 "neta_grammar.cc"

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
#ifndef YY_NETAPARSER_NETA_GRAMMAR_HH_INCLUDED
# define YY_NETAPARSER_NETA_GRAMMAR_HH_INCLUDED
/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int NetaParser_debug;
#endif

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
     DOUBLECONST = 268,
     NETAKEY = 269,
     NETAVAL = 270,
     NETARING = 271,
     NETACHAIN = 272,
     NETAGEOMETRY = 273,
     NETAPATH = 274,
     NETAGEOMETRYTYPE = 275,
     TOKEN = 276
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
#define DOUBLECONST 268
#define NETAKEY 269
#define NETAVAL 270
#define NETARING 271
#define NETACHAIN 272
#define NETAGEOMETRY 273
#define NETAPATH 274
#define NETAGEOMETRYTYPE 275
#define TOKEN 276



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{
/* Line 387 of yacc.c  */
#line 27 "neta_grammar.yy"

	NetaNode *netanode;			/* Generic node pointer */
	NetaBoundNode *boundnode;		/* Bound node pointer */
	NetaRingNode *ringnode;			/* Ring node pointer */
	NetaChainNode *chainnode;		/* Chain node pointer */
	NetaMeasurementNode *measurenode;	/* Measurement node pointer */
	int intconst;				/* Integer number */
	double doubleconst;			/* Floating point number */
	Neta::NetaKeyword netakey;		/* NETA keyword ID */
	Atom::AtomGeometry atomgeom;		/* NETA geometry ID */
	Neta::NetaValue netaval;		/* NETA value ID */
	Refitem<ForcefieldAtom,int> *typelist;	/* Pointer to head of created element/type list */


/* Line 387 of yacc.c  */
#line 193 "neta_grammar.cc"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE NetaParser_lval;

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int NetaParser_parse (void *YYPARSE_PARAM);
#else
int NetaParser_parse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int NetaParser_parse (void);
#else
int NetaParser_parse ();
#endif
#endif /* ! YYPARSE_PARAM */

#endif /* !YY_NETAPARSER_NETA_GRAMMAR_HH_INCLUDED  */

/* Copy the second part of user declarations.  */

/* Line 390 of yacc.c  */
#line 221 "neta_grammar.cc"

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
#define YYFINAL  41
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   137

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  36
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  18
/* YYNRULES -- Number of rules.  */
#define YYNRULES  54
/* YYNRULES -- Number of states.  */
#define YYNSTATES  107

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   276

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    29,     2,     2,    30,     2,    35,     2,
      27,    28,     2,     2,    25,    32,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       8,     6,     7,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    33,     2,    34,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,    26,     2,    31,     2,     2,     2,
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
       5,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint8 yyprhs[] =
{
       0,     0,     3,     4,     6,     8,    12,    16,    20,    23,
      26,    29,    32,    34,    36,    38,    40,    43,    45,    47,
      49,    51,    57,    62,    67,    72,    77,    82,    87,    89,
      91,    97,   103,   113,   123,   125,   128,   132,   136,   140,
     144,   151,   158,   165,   167,   171,   173,   177,   179,   182,
     183,   184,   185,   186,   187
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      37,     0,    -1,    -1,    38,    -1,    39,    -1,    38,    25,
      39,    -1,    38,    26,    38,    -1,    27,    38,    28,    -1,
      29,    40,    -1,    29,    41,    -1,    29,    42,    -1,    29,
      44,    -1,    40,    -1,    41,    -1,    42,    -1,    44,    -1,
      30,    24,    -1,    24,    -1,    13,    -1,    17,    -1,    23,
      -1,    18,    48,     6,     6,    13,    -1,    18,    48,     6,
      13,    -1,    18,    48,     7,    13,    -1,    18,    48,     8,
      13,    -1,    18,    48,    12,    13,    -1,    18,    48,    11,
      13,    -1,    18,    48,     9,    13,    -1,    19,    -1,    20,
      -1,    19,    27,    49,    38,    28,    -1,    20,    27,    50,
      43,    28,    -1,    21,    27,    51,    16,    25,    16,    25,
      43,    28,    -1,    22,    27,    52,    16,    25,    16,    25,
      43,    28,    -1,    44,    -1,    29,    44,    -1,    43,    25,
      43,    -1,    31,    45,    53,    -1,    32,    45,    53,    -1,
       6,    45,    53,    -1,    31,    45,    27,    53,    38,    28,
      -1,    32,    45,    27,    53,    38,    28,    -1,     6,    45,
      27,    53,    38,    28,    -1,    47,    -1,    33,    46,    34,
      -1,    47,    -1,    46,    25,    47,    -1,    14,    -1,    35,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    63,    63,    64,    68,    69,    70,    71,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    87,    91,    96,
      97,   102,   103,   104,   105,   106,   107,   108,   113,   114,
     115,   116,   117,   118,   122,   123,   124,   128,   129,   130,
     131,   132,   133,   138,   139,   143,   144,   148,   149,   154,
     159,   163,   167,   171,   175
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "IF", "OR", "AND", "'='", "'>'", "'<'",
  "NEQ", "EQ", "LEQ", "GEQ", "INTCONST", "ELEMENT", "TYPEREF",
  "DOUBLECONST", "NETAKEY", "NETAVAL", "NETARING", "NETACHAIN",
  "NETAGEOMETRY", "NETAPATH", "NETAGEOMETRYTYPE", "TOKEN", "','", "'|'",
  "'('", "')'", "'!'", "'$'", "'~'", "'-'", "'['", "']'", "'&'", "$accept",
  "neta", "nodelist", "node", "keyword", "value", "expander", "chain",
  "bound", "elemtypelist", "elemtypes", "elemtype", "saveval", "pushctxtr",
  "pushctxtc", "pushctxtg", "pushctxtp", "pushctxtb", YY_NULL
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,    61,    62,    60,   261,
     262,   263,   264,   265,   266,   267,   268,   269,   270,   271,
     272,   273,   274,   275,   276,    44,   124,    40,    41,    33,
      36,   126,    45,    91,    93,    38
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    36,    37,    37,    38,    38,    38,    38,    39,    39,
      39,    39,    39,    39,    39,    39,    39,    39,    39,    40,
      40,    41,    41,    41,    41,    41,    41,    41,    42,    42,
      42,    42,    42,    42,    43,    43,    43,    44,    44,    44,
      44,    44,    44,    45,    45,    46,    46,    47,    47,    48,
      49,    50,    51,    52,    53
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     1,     1,     3,     3,     3,     2,     2,
       2,     2,     1,     1,     1,     1,     2,     1,     1,     1,
       1,     5,     4,     4,     4,     4,     4,     4,     1,     1,
       5,     5,     9,     9,     1,     2,     3,     3,     3,     3,
       6,     6,     6,     1,     3,     1,     3,     1,     2,     0,
       0,     0,     0,     0,     0
};

/* YYDEFACT[STATE-NAME] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,     0,    18,    19,    49,    28,    29,     0,     0,    20,
      17,     0,     0,     0,     0,     0,     0,     3,     4,    12,
      13,    14,    15,    47,     0,     0,    54,    43,     0,    50,
      51,    52,    53,     0,     8,     9,    10,    11,    16,    54,
      54,     1,     0,     0,     0,    45,    48,    54,    39,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     7,
      54,    37,    54,    38,     5,     6,     0,    44,     0,     0,
      22,    23,    24,    27,    26,    25,     0,     0,     0,    34,
       0,     0,     0,     0,    46,     0,    21,    30,    35,     0,
      31,     0,     0,     0,     0,    42,    36,     0,     0,    40,
      41,     0,     0,     0,     0,    32,    33
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,    16,    17,    18,    19,    20,    21,    78,    22,    26,
      44,    27,    28,    55,    56,    57,    58,    48
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -39
static const yytype_int8 yypact[] =
{
      -4,    -3,   -39,   -39,   -39,    -6,    26,    35,    43,   -39,
     -39,    -4,    61,    51,    -3,    -3,    76,    31,   -39,   -39,
     -39,   -39,   -39,   -39,     8,    78,    72,   -39,    89,   -39,
     -39,   -39,   -39,   -18,   -39,   -39,   -39,   -39,   -39,    79,
      81,   -39,    18,    -4,   -22,   -39,   -39,   -39,   -39,    55,
      91,    99,   101,   104,   108,    -4,    23,   107,   109,   -39,
     -39,   -39,   -39,   -39,   -39,    31,     8,   -39,    -4,   111,
     -39,   -39,   -39,   -39,   -39,   -39,    77,    28,    41,   -39,
     102,   103,    -4,    -4,   -39,    85,   -39,   -39,   -39,    23,
     -39,   110,   113,    90,    94,   -39,   105,   106,   112,   -39,
     -39,    23,    23,    46,    60,   -39,   -39
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -39,   -39,   -10,    92,   120,   121,   123,   -38,   -12,    -9,
     -39,   -20,   -39,   -39,   -39,   -39,   -39,    47
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint8 yytable[] =
{
      37,    33,     1,    66,    45,    39,    40,    42,    43,     2,
      59,    23,    67,     3,     4,     5,     6,     7,     8,     9,
      10,    29,    23,    11,     1,    12,    13,    14,    15,     1,
      24,     2,    25,    65,     1,     3,     4,     5,     6,     7,
       8,     9,    10,    25,    79,    76,    84,    12,    13,    14,
      15,    96,    77,    30,    14,    15,    42,    43,    85,    14,
      15,    69,    31,   103,   104,    88,    89,     1,    70,    90,
      32,    89,    93,    94,   105,    38,    41,    79,     3,     4,
       5,     6,     7,     8,     9,    89,    61,    63,   106,    79,
      79,    46,    14,    15,    68,    49,    50,    51,    52,    47,
      53,    54,    42,    43,    71,    87,    60,    82,    62,    83,
      42,    43,    72,    95,    73,    42,    43,    74,    99,    42,
      43,    75,   100,    80,    86,    81,    97,    91,    92,    98,
      89,   101,    34,    35,    64,    36,     0,   102
};

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-39)))

#define yytable_value_is_error(Yytable_value) \
  YYID (0)

static const yytype_int8 yycheck[] =
{
      12,    11,     6,    25,    24,    14,    15,    25,    26,    13,
      28,    14,    34,    17,    18,    19,    20,    21,    22,    23,
      24,    27,    14,    27,     6,    29,    30,    31,    32,     6,
      33,    13,    35,    43,     6,    17,    18,    19,    20,    21,
      22,    23,    24,    35,    56,    55,    66,    29,    30,    31,
      32,    89,    29,    27,    31,    32,    25,    26,    68,    31,
      32,     6,    27,   101,   102,    77,    25,     6,    13,    28,
      27,    25,    82,    83,    28,    24,     0,    89,    17,    18,
      19,    20,    21,    22,    23,    25,    39,    40,    28,   101,
     102,    13,    31,    32,    47,     6,     7,     8,     9,    27,
      11,    12,    25,    26,    13,    28,    27,    60,    27,    62,
      25,    26,    13,    28,    13,    25,    26,    13,    28,    25,
      26,    13,    28,    16,    13,    16,    16,    25,    25,    16,
      25,    25,    12,    12,    42,    12,    -1,    25
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     6,    13,    17,    18,    19,    20,    21,    22,    23,
      24,    27,    29,    30,    31,    32,    37,    38,    39,    40,
      41,    42,    44,    14,    33,    35,    45,    47,    48,    27,
      27,    27,    27,    38,    40,    41,    42,    44,    24,    45,
      45,     0,    25,    26,    46,    47,    13,    27,    53,     6,
       7,     8,     9,    11,    12,    49,    50,    51,    52,    28,
      27,    53,    27,    53,    39,    38,    25,    34,    53,     6,
      13,    13,    13,    13,    13,    13,    38,    29,    43,    44,
      16,    16,    53,    53,    47,    38,    13,    28,    44,    25,
      28,    25,    25,    38,    38,    28,    43,    16,    16,    28,
      28,    25,    25,    43,    43,    28,    28
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
#line 63 "neta_grammar.yy"
    { netaparser.setDescription(NULL); YYACCEPT; }
    break;

  case 3:
/* Line 1792 of yacc.c  */
#line 64 "neta_grammar.yy"
    { netaparser.setDescription((yyvsp[(1) - (1)].netanode)); YYACCEPT; }
    break;

  case 4:
/* Line 1792 of yacc.c  */
#line 68 "neta_grammar.yy"
    { (yyval.netanode) = (yyvsp[(1) - (1)].netanode); }
    break;

  case 5:
/* Line 1792 of yacc.c  */
#line 69 "neta_grammar.yy"
    { (yyval.netanode) = netaparser.join(Neta::NetaAndLogic, (yyvsp[(1) - (3)].netanode), (yyvsp[(3) - (3)].netanode)); }
    break;

  case 6:
/* Line 1792 of yacc.c  */
#line 70 "neta_grammar.yy"
    { (yyval.netanode) = netaparser.join(Neta::NetaOrLogic, (yyvsp[(1) - (3)].netanode), (yyvsp[(3) - (3)].netanode)); }
    break;

  case 7:
/* Line 1792 of yacc.c  */
#line 71 "neta_grammar.yy"
    { (yyval.netanode) = (yyvsp[(2) - (3)].netanode); }
    break;

  case 8:
/* Line 1792 of yacc.c  */
#line 75 "neta_grammar.yy"
    { (yyvsp[(2) - (2)].netanode)->setReverseLogic(); (yyval.netanode) = (yyvsp[(2) - (2)].netanode); }
    break;

  case 9:
/* Line 1792 of yacc.c  */
#line 76 "neta_grammar.yy"
    { (yyvsp[(2) - (2)].netanode)->setReverseLogic(); (yyval.netanode) = (yyvsp[(2) - (2)].netanode); }
    break;

  case 10:
/* Line 1792 of yacc.c  */
#line 77 "neta_grammar.yy"
    { (yyvsp[(2) - (2)].netanode)->setReverseLogic(); (yyval.netanode) = (yyvsp[(2) - (2)].netanode); }
    break;

  case 11:
/* Line 1792 of yacc.c  */
#line 78 "neta_grammar.yy"
    { (yyvsp[(2) - (2)].netanode)->setReverseLogic(); (yyval.netanode) = (yyvsp[(2) - (2)].netanode); }
    break;

  case 12:
/* Line 1792 of yacc.c  */
#line 79 "neta_grammar.yy"
    { (yyval.netanode) = (yyvsp[(1) - (1)].netanode); }
    break;

  case 13:
/* Line 1792 of yacc.c  */
#line 80 "neta_grammar.yy"
    { (yyval.netanode) = (yyvsp[(1) - (1)].netanode); }
    break;

  case 14:
/* Line 1792 of yacc.c  */
#line 81 "neta_grammar.yy"
    { (yyval.netanode) = (yyvsp[(1) - (1)].netanode); }
    break;

  case 15:
/* Line 1792 of yacc.c  */
#line 82 "neta_grammar.yy"
    { (yyval.netanode) = (yyvsp[(1) - (1)].netanode); }
    break;

  case 16:
/* Line 1792 of yacc.c  */
#line 83 "neta_grammar.yy"
    {
		(yyval.netanode) = netaparser.findDefine(netaparser.lastUnknownToken());
		if ((yyval.netanode) == NULL) { msg.print("Error: NETA description references a non-existent 'define' name (%s)\n", netaparser.lastUnknownToken()); YYABORT; }
		}
    break;

  case 17:
/* Line 1792 of yacc.c  */
#line 87 "neta_grammar.yy"
    {
		msg.print("Error: NETA description contains an unrecognised keyword (%s)\n", netaparser.lastUnknownToken());
		YYABORT;
		}
    break;

  case 18:
/* Line 1792 of yacc.c  */
#line 91 "neta_grammar.yy"
    { msg.print("Error: Stray integer constant found in NETA description.\n"); YYABORT; }
    break;

  case 19:
/* Line 1792 of yacc.c  */
#line 96 "neta_grammar.yy"
    { (yyval.netanode) = netaparser.createKeywordNode((yyvsp[(1) - (1)].netakey)); }
    break;

  case 20:
/* Line 1792 of yacc.c  */
#line 97 "neta_grammar.yy"
    { (yyval.netanode) = netaparser.createGeometryNode((yyvsp[(1) - (1)].atomgeom)); }
    break;

  case 21:
/* Line 1792 of yacc.c  */
#line 102 "neta_grammar.yy"
    { (yyval.netanode) = netaparser.createValueNode(savedval, Neta::EqualTo, (yyvsp[(5) - (5)].intconst)); }
    break;

  case 22:
/* Line 1792 of yacc.c  */
#line 103 "neta_grammar.yy"
    { (yyval.netanode) = netaparser.createValueNode(savedval, Neta::EqualTo, (yyvsp[(4) - (4)].intconst)); }
    break;

  case 23:
/* Line 1792 of yacc.c  */
#line 104 "neta_grammar.yy"
    { (yyval.netanode) = netaparser.createValueNode(savedval, Neta::GreaterThan, (yyvsp[(4) - (4)].intconst)); }
    break;

  case 24:
/* Line 1792 of yacc.c  */
#line 105 "neta_grammar.yy"
    { (yyval.netanode) = netaparser.createValueNode(savedval, Neta::LessThan, (yyvsp[(4) - (4)].intconst)); }
    break;

  case 25:
/* Line 1792 of yacc.c  */
#line 106 "neta_grammar.yy"
    { (yyval.netanode) = netaparser.createValueNode(savedval, Neta::GreaterThanEqualTo, (yyvsp[(4) - (4)].intconst)); }
    break;

  case 26:
/* Line 1792 of yacc.c  */
#line 107 "neta_grammar.yy"
    { (yyval.netanode) = netaparser.createValueNode(savedval, Neta::LessThanEqualTo, (yyvsp[(4) - (4)].intconst)); }
    break;

  case 27:
/* Line 1792 of yacc.c  */
#line 108 "neta_grammar.yy"
    { (yyval.netanode) = netaparser.createValueNode(savedval, Neta::NotEqualTo, (yyvsp[(4) - (4)].intconst)); }
    break;

  case 28:
/* Line 1792 of yacc.c  */
#line 113 "neta_grammar.yy"
    { (yyval.netanode) = netaparser.createRingNode(); netaparser.popContext(); }
    break;

  case 29:
/* Line 1792 of yacc.c  */
#line 114 "neta_grammar.yy"
    { (yyval.netanode) = netaparser.createChainNode(); netaparser.popContext(); }
    break;

  case 30:
/* Line 1792 of yacc.c  */
#line 115 "neta_grammar.yy"
    { (yyvsp[(3) - (5)].ringnode)->setInnerNeta((yyvsp[(4) - (5)].netanode)); (yyval.netanode) = (yyvsp[(3) - (5)].ringnode); netaparser.popContext(); }
    break;

  case 31:
/* Line 1792 of yacc.c  */
#line 116 "neta_grammar.yy"
    { (yyvsp[(3) - (5)].chainnode)->setInnerNeta(NULL,(yyvsp[(4) - (5)].netanode)); (yyval.netanode) = (yyvsp[(3) - (5)].chainnode); netaparser.popContext(); }
    break;

  case 32:
/* Line 1792 of yacc.c  */
#line 117 "neta_grammar.yy"
    { (yyvsp[(3) - (9)].measurenode)->setInnerNeta(NULL,(yyvsp[(8) - (9)].netanode)); (yyvsp[(3) - (9)].measurenode)->setRequiredValue((yyvsp[(4) - (9)].doubleconst),(yyvsp[(6) - (9)].doubleconst)); (yyval.netanode) = (yyvsp[(3) - (9)].measurenode); netaparser.popContext(); }
    break;

  case 33:
/* Line 1792 of yacc.c  */
#line 118 "neta_grammar.yy"
    { (yyvsp[(3) - (9)].measurenode)->setInnerNeta(NULL,(yyvsp[(8) - (9)].netanode)); (yyvsp[(3) - (9)].measurenode)->setRequiredValue((yyvsp[(4) - (9)].doubleconst),(yyvsp[(6) - (9)].doubleconst)); (yyval.netanode) = (yyvsp[(3) - (9)].measurenode); netaparser.popContext(); }
    break;

  case 34:
/* Line 1792 of yacc.c  */
#line 122 "neta_grammar.yy"
    { (yyval.netanode) = (yyvsp[(1) - (1)].netanode); }
    break;

  case 35:
/* Line 1792 of yacc.c  */
#line 123 "neta_grammar.yy"
    { (yyval.netanode) = (yyvsp[(2) - (2)].netanode); (yyvsp[(2) - (2)].netanode)->setReverseLogic(); }
    break;

  case 36:
/* Line 1792 of yacc.c  */
#line 124 "neta_grammar.yy"
    { (yyval.netanode) = netaparser.link((yyvsp[(1) - (3)].netanode),(yyvsp[(3) - (3)].netanode)); }
    break;

  case 37:
/* Line 1792 of yacc.c  */
#line 128 "neta_grammar.yy"
    { (yyvsp[(3) - (3)].boundnode)->set((yyvsp[(2) - (3)].typelist), NULL, Bond::Any); (yyval.netanode) = (yyvsp[(3) - (3)].boundnode); netaparser.popContext(); }
    break;

  case 38:
/* Line 1792 of yacc.c  */
#line 129 "neta_grammar.yy"
    { (yyvsp[(3) - (3)].boundnode)->set((yyvsp[(2) - (3)].typelist), NULL, Bond::Single); (yyval.netanode) = (yyvsp[(3) - (3)].boundnode); netaparser.popContext(); }
    break;

  case 39:
/* Line 1792 of yacc.c  */
#line 130 "neta_grammar.yy"
    { (yyvsp[(3) - (3)].boundnode)->set((yyvsp[(2) - (3)].typelist), NULL, Bond::Double); (yyval.netanode) = (yyvsp[(3) - (3)].boundnode); netaparser.popContext(); }
    break;

  case 40:
/* Line 1792 of yacc.c  */
#line 131 "neta_grammar.yy"
    { (yyvsp[(4) - (6)].boundnode)->set((yyvsp[(2) - (6)].typelist), (yyvsp[(5) - (6)].netanode), Bond::Any); (yyval.netanode) = (yyvsp[(4) - (6)].boundnode); netaparser.popContext(); }
    break;

  case 41:
/* Line 1792 of yacc.c  */
#line 132 "neta_grammar.yy"
    { (yyvsp[(4) - (6)].boundnode)->set((yyvsp[(2) - (6)].typelist), (yyvsp[(5) - (6)].netanode), Bond::Single); (yyval.netanode) = (yyvsp[(4) - (6)].boundnode); netaparser.popContext(); }
    break;

  case 42:
/* Line 1792 of yacc.c  */
#line 133 "neta_grammar.yy"
    { (yyvsp[(4) - (6)].boundnode)->set((yyvsp[(2) - (6)].typelist), (yyvsp[(5) - (6)].netanode), Bond::Double); (yyval.netanode) = (yyvsp[(4) - (6)].boundnode); netaparser.popContext(); }
    break;

  case 43:
/* Line 1792 of yacc.c  */
#line 138 "neta_grammar.yy"
    { (yyval.typelist) = (yyvsp[(1) - (1)].typelist); }
    break;

  case 44:
/* Line 1792 of yacc.c  */
#line 139 "neta_grammar.yy"
    { (yyval.typelist) = (yyvsp[(2) - (3)].typelist); }
    break;

  case 45:
/* Line 1792 of yacc.c  */
#line 143 "neta_grammar.yy"
    { (yyval.typelist) = (yyvsp[(1) - (1)].typelist); }
    break;

  case 46:
/* Line 1792 of yacc.c  */
#line 144 "neta_grammar.yy"
    { (yyval.typelist) = netaparser.joinElementTypes((yyvsp[(1) - (3)].typelist),(yyvsp[(3) - (3)].typelist)); }
    break;

  case 47:
/* Line 1792 of yacc.c  */
#line 148 "neta_grammar.yy"
    { (yyval.typelist) = netaparser.createElementType((yyvsp[(1) - (1)].intconst)); if ((yyval.typelist) == NULL) YYABORT; }
    break;

  case 48:
/* Line 1792 of yacc.c  */
#line 149 "neta_grammar.yy"
    { (yyval.typelist) = netaparser.createElementType(-(yyvsp[(2) - (2)].intconst)); if ((yyval.typelist) == NULL) YYABORT; }
    break;

  case 49:
/* Line 1792 of yacc.c  */
#line 154 "neta_grammar.yy"
    { savedval = yylval.netaval; }
    break;

  case 50:
/* Line 1792 of yacc.c  */
#line 159 "neta_grammar.yy"
    { (yyval.ringnode) = netaparser.createRingNode(); }
    break;

  case 51:
/* Line 1792 of yacc.c  */
#line 163 "neta_grammar.yy"
    { (yyval.chainnode) = netaparser.createChainNode(); }
    break;

  case 52:
/* Line 1792 of yacc.c  */
#line 167 "neta_grammar.yy"
    { (yyval.measurenode) = netaparser.createMeasurementNode(false); }
    break;

  case 53:
/* Line 1792 of yacc.c  */
#line 171 "neta_grammar.yy"
    { (yyval.measurenode) = netaparser.createMeasurementNode(true); }
    break;

  case 54:
/* Line 1792 of yacc.c  */
#line 175 "neta_grammar.yy"
    { (yyval.boundnode) = netaparser.createBoundNode(); }
    break;


/* Line 1792 of yacc.c  */
#line 1827 "neta_grammar.cc"
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
#line 178 "neta_grammar.yy"


void yyerror(char *s)
{
}
