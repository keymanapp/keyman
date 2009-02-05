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



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     TOK_ANY = 258,
     TOK_ANSI = 259,
     TOK_AUTHOR = 260,
     TOK_BEEP = 261,
     TOK_BITMAP = 262,
     TOK_BRKT = 263,
     TOK_CALL = 264,
     TOK_CAPSFREE = 265,
     TOK_CAPSOFF = 266,
     TOK_CAPSON = 267,
     TOK_COMMA = 268,
     TOK_CONTEXT = 269,
     TOK_COPYRIGHT = 270,
     TOK_CHAR = 271,
     TOK_DEADKEY = 272,
     TOK_DOLLAR = 273,
     TOK_ERROR = 274,
     TOK_ETHNOLOGUE = 275,
     TOK_GROUP = 276,
     TOK_GT = 277,
     TOK_HOTKEY = 278,
     TOK_INDEX = 279,
     TOK_LANGUAGE = 280,
     TOK_LAYOUT = 281,
     TOK_MATCH = 282,
     TOK_MESSAGE = 283,
     TOK_MNEMONIC = 284,
     TOK_NOMATCH = 285,
     TOK_NAME = 286,
     TOK_NOTANY = 287,
     TOK_NUL = 288,
     TOK_NUMBER = 289,
     TOK_NL = 290,
     TOK_OUTS = 291,
     TOK_RAWKEY = 292,
     TOK_PLUS = 293,
     TOK_QM = 294,
     TOK_RTN = 295,
     TOK_SB = 296,
     TOK_SHIFT = 297,
     TOK_STORE = 298,
     TOK_STOREINSTORE = 299,
     TOK_SWITCH = 300,
     TOK_UNICODE = 301,
     TOK_USE = 302,
     TOK_USINGKEYS = 303,
     TOK_UTF = 304,
     TOK_VERSION = 305,
     TOK_XKEYSYM = 306
   };
#endif
/* Tokens.  */
#define TOK_ANY 258
#define TOK_ANSI 259
#define TOK_AUTHOR 260
#define TOK_BEEP 261
#define TOK_BITMAP 262
#define TOK_BRKT 263
#define TOK_CALL 264
#define TOK_CAPSFREE 265
#define TOK_CAPSOFF 266
#define TOK_CAPSON 267
#define TOK_COMMA 268
#define TOK_CONTEXT 269
#define TOK_COPYRIGHT 270
#define TOK_CHAR 271
#define TOK_DEADKEY 272
#define TOK_DOLLAR 273
#define TOK_ERROR 274
#define TOK_ETHNOLOGUE 275
#define TOK_GROUP 276
#define TOK_GT 277
#define TOK_HOTKEY 278
#define TOK_INDEX 279
#define TOK_LANGUAGE 280
#define TOK_LAYOUT 281
#define TOK_MATCH 282
#define TOK_MESSAGE 283
#define TOK_MNEMONIC 284
#define TOK_NOMATCH 285
#define TOK_NAME 286
#define TOK_NOTANY 287
#define TOK_NUL 288
#define TOK_NUMBER 289
#define TOK_NL 290
#define TOK_OUTS 291
#define TOK_RAWKEY 292
#define TOK_PLUS 293
#define TOK_QM 294
#define TOK_RTN 295
#define TOK_SB 296
#define TOK_SHIFT 297
#define TOK_STORE 298
#define TOK_STOREINSTORE 299
#define TOK_SWITCH 300
#define TOK_UNICODE 301
#define TOK_USE 302
#define TOK_USINGKEYS 303
#define TOK_UTF 304
#define TOK_VERSION 305
#define TOK_XKEYSYM 306




/* Copy the first part of user declarations.  */
#line 1 "yacc.y"

/* 
	YACC grammar for reading Keyman-style keyboard definition files 
	and outputting UTF-8 byte code for the Linux interpreter
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "compiler.h"

extern char *fname;			/* Current file name */
extern int yylex();			/* Reference needed by yacc.c */

int  lineno   = 1;			/* Current line number in file */
int	 errcount = 0;			/* Count of errors */

int	 n;						/* Temporary index value */
GROUP *gp = NULL;			/* Temporary group pointer */

#define YYDEBUG 1			/* Allow compiler debugging (if yydebug true) */


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
#line 25 "yacc.y"
{
	int simple;
	ITEM number;
	char *string;
	ITEM *items;
	RULE *rule;
	GROUP *group;
	}
/* Line 187 of yacc.c.  */
#line 230 "yacc.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 243 "yacc.c"

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
#define YYLAST   274

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  52
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  16
/* YYNRULES -- Number of rules.  */
#define YYNRULES  79
/* YYNRULES -- Number of states.  */
#define YYNSTATES  160

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   306

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
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
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     6,     8,    10,    13,    17,    21,    27,
      31,    35,    39,    43,    47,    51,    55,    59,    63,    67,
      70,    73,    76,    81,    87,    93,   101,   105,   109,   113,
     117,   119,   121,   124,   126,   129,   133,   138,   140,   143,
     148,   153,   155,   157,   159,   162,   165,   167,   171,   173,
     176,   179,   182,   185,   187,   194,   199,   201,   203,   205,
     210,   213,   215,   217,   220,   223,   226,   228,   232,   235,
     239,   241,   244,   247,   249,   252,   254,   257,   259,   261
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      53,     0,    -1,    54,    56,    -1,    56,    -1,    55,    -1,
      55,    54,    -1,    31,    67,    35,    -1,    31,    63,    35,
      -1,    23,    41,    65,    41,    35,    -1,    23,    63,    35,
      -1,    50,    67,    35,    -1,    50,    63,    35,    -1,     7,
      67,    35,    -1,     7,    63,    35,    -1,    15,    63,    35,
      -1,    28,    63,    35,    -1,    25,    61,    35,    -1,    26,
      63,    35,    -1,    26,    67,    35,    -1,    11,    35,    -1,
      12,    35,    -1,    10,    35,    -1,    43,    64,    61,    35,
      -1,     4,    22,    47,    64,    35,    -1,    46,    22,    47,
      64,    35,    -1,     4,    22,    47,    64,    47,    64,    35,
      -1,     5,    63,    35,    -1,    29,    63,    35,    -1,    20,
      63,    35,    -1,    19,    67,    35,    -1,    35,    -1,    57,
      -1,    57,    56,    -1,    58,    -1,    58,    59,    -1,    21,
      64,    35,    -1,    21,    64,    48,    35,    -1,    60,    -1,
      60,    59,    -1,    61,    22,    61,    35,    -1,    43,    64,
      61,    35,    -1,    35,    -1,    62,    -1,    63,    -1,    62,
      61,    -1,    63,    61,    -1,    34,    -1,    41,    65,    41,
      -1,    38,    -1,     3,    64,    -1,    32,    64,    -1,    36,
      64,    -1,    17,    64,    -1,    33,    -1,    24,     8,    67,
      13,    67,     8,    -1,    24,     8,    67,     8,    -1,    40,
      -1,     6,    -1,    14,    -1,    14,     8,    67,     8,    -1,
      47,    64,    -1,    27,    -1,    30,    -1,     9,    64,    -1,
      45,    64,    -1,    18,    67,    -1,    19,    -1,    39,    67,
      39,    -1,    39,    39,    -1,     8,    67,     8,    -1,    63,
      -1,    66,    63,    -1,    66,    37,    -1,    37,    -1,    66,
      51,    -1,    51,    -1,    66,    42,    -1,    42,    -1,    16,
      -1,    16,    67,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   102,   102,   108,   117,   118,   122,   126,   130,   134,
     138,   142,   146,   150,   154,   158,   162,   166,   170,   174,
     178,   182,   186,   190,   194,   198,   203,   207,   211,   215,
     216,   220,   224,   231,   238,   249,   255,   263,   267,   274,
     278,   282,   289,   293,   297,   301,   308,   312,   316,   320,
     332,   344,   356,   360,   364,   376,   381,   385,   389,   393,
     397,   401,   405,   409,   414,   419,   433,   440,   445,   452,
     459,   464,   469,   473,   477,   481,   488,   492,   499,   503
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "TOK_ANY", "TOK_ANSI", "TOK_AUTHOR",
  "TOK_BEEP", "TOK_BITMAP", "TOK_BRKT", "TOK_CALL", "TOK_CAPSFREE",
  "TOK_CAPSOFF", "TOK_CAPSON", "TOK_COMMA", "TOK_CONTEXT", "TOK_COPYRIGHT",
  "TOK_CHAR", "TOK_DEADKEY", "TOK_DOLLAR", "TOK_ERROR", "TOK_ETHNOLOGUE",
  "TOK_GROUP", "TOK_GT", "TOK_HOTKEY", "TOK_INDEX", "TOK_LANGUAGE",
  "TOK_LAYOUT", "TOK_MATCH", "TOK_MESSAGE", "TOK_MNEMONIC", "TOK_NOMATCH",
  "TOK_NAME", "TOK_NOTANY", "TOK_NUL", "TOK_NUMBER", "TOK_NL", "TOK_OUTS",
  "TOK_RAWKEY", "TOK_PLUS", "TOK_QM", "TOK_RTN", "TOK_SB", "TOK_SHIFT",
  "TOK_STORE", "TOK_STOREINSTORE", "TOK_SWITCH", "TOK_UNICODE", "TOK_USE",
  "TOK_USINGKEYS", "TOK_UTF", "TOK_VERSION", "TOK_XKEYSYM", "$accept",
  "T_FILE", "T_HEADER", "T_HEADLINE", "T_GROUPS", "T_GROUP", "T_GHEADER",
  "T_RULES", "T_RULELINE", "T_ITEMS", "T_ITEM", "T_STRING", "T_PARAMETER",
  "T_KEYDEF", "T_KEYMODS", "T_BYTES", 0
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
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    52,    53,    53,    54,    54,    55,    55,    55,    55,
      55,    55,    55,    55,    55,    55,    55,    55,    55,    55,
      55,    55,    55,    55,    55,    55,    55,    55,    55,    55,
      55,    56,    56,    57,    57,    58,    58,    59,    59,    60,
      60,    60,    61,    61,    61,    61,    62,    62,    62,    62,
      62,    62,    62,    62,    62,    62,    62,    62,    62,    62,
      62,    62,    62,    62,    62,    62,    62,    63,    63,    64,
      65,    65,    65,    65,    65,    65,    66,    66,    67,    67
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     2,     1,     1,     2,     3,     3,     5,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     2,
       2,     2,     4,     5,     5,     7,     3,     3,     3,     3,
       1,     1,     2,     1,     2,     3,     4,     1,     2,     4,
       4,     1,     1,     1,     2,     2,     1,     3,     1,     2,
       2,     2,     2,     1,     6,     4,     1,     1,     1,     4,
       2,     1,     1,     2,     2,     2,     1,     3,     2,     3,
       1,     2,     2,     1,     2,     1,     2,     1,     1,     2
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    30,     0,     0,
       0,     0,     0,     4,     3,    31,    33,     0,     0,     0,
      78,     0,     0,    21,    19,    20,     0,     0,     0,     0,
       0,     0,     0,     0,    57,     0,    58,     0,     0,    66,
       0,    61,    62,     0,    53,    46,     0,    48,    56,     0,
       0,     0,     0,    42,    43,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     1,     2,     5,    32,    41,
       0,    34,    37,     0,     0,    68,     0,    26,    79,    13,
      12,    14,    29,    28,     0,    35,     0,    73,    77,    75,
      70,     0,     0,     9,    49,    63,     0,    52,    65,     0,
      50,    51,     0,    64,    60,    16,    44,    45,    17,    18,
      15,    27,     7,     6,     0,     0,    11,    10,     0,    38,
       0,     0,    67,    69,    36,     0,    72,    76,    74,    71,
       0,     0,    47,    22,     0,     0,     0,    23,     0,     8,
      59,    55,     0,    24,    40,    39,     0,     0,    25,    54
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,    21,    22,    23,    24,    25,    26,    81,    82,    83,
      63,    64,    40,   101,   102,    32
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -33
static const yytype_int16 yypact[] =
{
     117,   -16,   -22,   -12,   -28,   -21,     2,   -22,    25,   -22,
      10,     6,   227,   -12,   -22,   -22,   -12,   -33,    10,    20,
     -12,    46,    27,   154,   -33,    27,   184,     3,    -7,    14,
      25,    16,    17,   -33,   -33,   -33,    24,    30,    37,    25,
     -25,   -26,    38,    10,   -33,    10,    47,    10,    25,   -33,
      48,   -33,   -33,    10,   -33,   -33,    10,   -33,   -33,   -26,
      10,    10,    39,   227,   227,    41,    42,    43,    44,    45,
      49,   227,    34,    50,    51,   -33,   -33,   -33,   -33,   -33,
      10,   -33,   184,    60,    10,   -33,    52,   -33,   -33,   -33,
     -33,   -33,   -33,   -33,    75,   -33,    53,   -33,   -33,   -33,
     -33,    19,   -18,   -33,   -33,   -33,    25,   -33,   -33,    25,
     -33,   -33,    54,   -33,   -33,   -33,   -33,   -33,   -33,   -33,
     -33,   -33,   -33,   -33,    57,    10,   -33,   -33,   227,   -33,
     227,   -32,   -33,   -33,   -33,    58,   -33,   -33,   -33,   -33,
      79,    21,   -33,   -33,    59,    62,    64,   -33,    10,   -33,
     -33,   -33,    25,   -33,   -33,   -33,    65,    81,   -33,   -33
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -33,   -33,    67,   -33,    13,   -33,   -33,    22,   -33,   -10,
     -33,    55,   -17,    56,   -33,    -8
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint8 yytable[] =
{
      37,    71,    62,   147,    30,    66,    27,    33,    70,    30,
      95,    97,    74,    28,    34,   148,    98,    28,    39,   136,
      86,    28,    88,    96,   137,    99,   104,    28,   105,   151,
     107,    94,    85,   138,   152,    76,   110,    35,    78,   111,
     108,    30,    72,   113,   114,    28,    75,    41,    10,    87,
      84,    89,    90,   116,   117,   106,   109,    29,    31,    91,
     135,   124,    36,   128,    38,    92,    42,   131,    65,    67,
      68,    69,    93,   103,   115,    73,   118,   119,   120,   121,
     122,   125,   130,   133,   123,   126,   127,   150,   134,   159,
      77,   132,   143,   149,   153,   142,   100,   154,   140,   155,
     158,   141,     0,     0,   129,     0,     0,     0,   144,     0,
       0,     0,     0,     0,   100,   112,     0,     0,   145,     0,
     146,     1,     2,     0,     3,     0,     0,     4,     5,     6,
       0,   156,     7,     0,     0,     0,     8,     9,    10,     0,
      11,     0,    12,    13,   157,    14,    15,     0,    16,     0,
       0,     0,    17,     0,     0,     0,     0,   139,     1,     2,
      18,     3,     0,    19,     4,     5,     6,    20,     0,     7,
       0,     0,     0,     8,     9,     0,     0,    11,     0,    12,
      13,     0,    14,    15,     0,    16,     0,    43,     0,    17,
      44,     0,     0,    45,     0,     0,     0,    18,    46,     0,
      19,    47,    48,    49,    20,     0,     0,     0,    50,     0,
       0,    51,     0,     0,    52,     0,    53,    54,    55,    79,
      56,     0,    57,    28,    58,    59,     0,    80,     0,    60,
      43,    61,     0,    44,     0,     0,    45,     0,     0,     0,
       0,    46,     0,     0,    47,    48,    49,     0,     0,     0,
       0,    50,     0,     0,    51,     0,     0,    52,     0,    53,
      54,    55,     0,    56,     0,    57,    28,    58,    59,     0,
       0,     0,    60,     0,    61
};

static const yytype_int16 yycheck[] =
{
       8,    18,    12,    35,    16,    13,    22,    35,    16,    16,
      35,    37,    20,    39,    35,    47,    42,    39,     8,    37,
      28,    39,    30,    48,    42,    51,    43,    39,    45,     8,
      47,    39,    39,    51,    13,    22,    53,    35,    25,    56,
      48,    16,    22,    60,    61,    39,     0,    41,    21,    35,
      47,    35,    35,    63,    64,     8,     8,     2,     3,    35,
      41,    71,     7,    80,     9,    35,    11,    84,    13,    14,
      15,    16,    35,    35,    35,    20,    35,    35,    35,    35,
      35,    47,    22,     8,    35,    35,    35,     8,    35,     8,
      23,    39,    35,    35,    35,    41,    41,    35,   106,    35,
      35,   109,    -1,    -1,    82,    -1,    -1,    -1,   125,    -1,
      -1,    -1,    -1,    -1,    59,    59,    -1,    -1,   128,    -1,
     130,     4,     5,    -1,     7,    -1,    -1,    10,    11,    12,
      -1,   148,    15,    -1,    -1,    -1,    19,    20,    21,    -1,
      23,    -1,    25,    26,   152,    28,    29,    -1,    31,    -1,
      -1,    -1,    35,    -1,    -1,    -1,    -1,   102,     4,     5,
      43,     7,    -1,    46,    10,    11,    12,    50,    -1,    15,
      -1,    -1,    -1,    19,    20,    -1,    -1,    23,    -1,    25,
      26,    -1,    28,    29,    -1,    31,    -1,     3,    -1,    35,
       6,    -1,    -1,     9,    -1,    -1,    -1,    43,    14,    -1,
      46,    17,    18,    19,    50,    -1,    -1,    -1,    24,    -1,
      -1,    27,    -1,    -1,    30,    -1,    32,    33,    34,    35,
      36,    -1,    38,    39,    40,    41,    -1,    43,    -1,    45,
       3,    47,    -1,     6,    -1,    -1,     9,    -1,    -1,    -1,
      -1,    14,    -1,    -1,    17,    18,    19,    -1,    -1,    -1,
      -1,    24,    -1,    -1,    27,    -1,    -1,    30,    -1,    32,
      33,    34,    -1,    36,    -1,    38,    39,    40,    41,    -1,
      -1,    -1,    45,    -1,    47
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     4,     5,     7,    10,    11,    12,    15,    19,    20,
      21,    23,    25,    26,    28,    29,    31,    35,    43,    46,
      50,    53,    54,    55,    56,    57,    58,    22,    39,    63,
      16,    63,    67,    35,    35,    35,    63,    67,    63,     8,
      64,    41,    63,     3,     6,     9,    14,    17,    18,    19,
      24,    27,    30,    32,    33,    34,    36,    38,    40,    41,
      45,    47,    61,    62,    63,    63,    67,    63,    63,    63,
      67,    64,    22,    63,    67,     0,    56,    54,    56,    35,
      43,    59,    60,    61,    47,    39,    67,    35,    67,    35,
      35,    35,    35,    35,    67,    35,    48,    37,    42,    51,
      63,    65,    66,    35,    64,    64,     8,    64,    67,     8,
      64,    64,    65,    64,    64,    35,    61,    61,    35,    35,
      35,    35,    35,    35,    61,    47,    35,    35,    64,    59,
      22,    64,    39,     8,    35,    41,    37,    42,    51,    63,
      67,    67,    41,    35,    64,    61,    61,    35,    47,    35,
       8,     8,    13,    35,    35,    35,    64,    67,    35,     8
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
#line 103 "yacc.y"
    {
		kbp->groups = (yyvsp[(2) - (2)].group);
		kbp->ngroups = count_groups((yyvsp[(2) - (2)].group));
		kbp->nstores = count_stores(kbp->stores);
	}
    break;

  case 3:
#line 109 "yacc.y"
    {
		kbp->groups = (yyvsp[(1) - (1)].group);
		kbp->ngroups = count_groups((yyvsp[(1) - (1)].group));
		kbp->nstores = count_stores(kbp->stores);
	}
    break;

  case 6:
#line 123 "yacc.y"
    {
		new_store_from_string("&name",(yyvsp[(2) - (3)].string),lineno);
	}
    break;

  case 7:
#line 127 "yacc.y"
    {
		new_store_from_string("&name",(yyvsp[(2) - (3)].string),lineno);
	}
    break;

  case 8:
#line 131 "yacc.y"
    {
		new_store("&hotkey",new_list((yyvsp[(3) - (5)].number)),lineno);
	}
    break;

  case 9:
#line 135 "yacc.y"
    {
		new_store_from_string("&hotkey",(yyvsp[(2) - (3)].string),lineno);
	}
    break;

  case 10:
#line 139 "yacc.y"
    {
		new_store_from_string("&version",(yyvsp[(2) - (3)].string),lineno);
	}
    break;

  case 11:
#line 143 "yacc.y"
    {
		new_store_from_string("&version",(yyvsp[(2) - (3)].string),lineno);
	}
    break;

  case 12:
#line 147 "yacc.y"
    { 
		new_store_from_string("&bitmap",(yyvsp[(2) - (3)].string),lineno);
	}
    break;

  case 13:
#line 151 "yacc.y"
    { 
		new_store_from_string("&bitmap",(yyvsp[(2) - (3)].string),lineno);
	}
    break;

  case 14:
#line 155 "yacc.y"
    { 
		new_store_from_string("&copyright",(yyvsp[(2) - (3)].string),lineno);	
	}
    break;

  case 15:
#line 159 "yacc.y"
    { 
		new_store_from_string("&message",(yyvsp[(2) - (3)].string),lineno);
	}
    break;

  case 16:
#line 163 "yacc.y"
    { 
		new_store_from_string("&language",(yyvsp[(2) - (3)].items),lineno);
	}
    break;

  case 17:
#line 167 "yacc.y"
    { 
		new_store_from_string("&layout",(yyvsp[(2) - (3)].string),lineno);
	}
    break;

  case 18:
#line 171 "yacc.y"
    { 
		new_store_from_string("&layout",(yyvsp[(2) - (3)].string),lineno);
	}
    break;

  case 19:
#line 175 "yacc.y"
    { 
		new_store_from_string("&capsalwaysoff","1",lineno);
	}
    break;

  case 20:
#line 179 "yacc.y"
    { 
		new_store_from_string("&capsononly","1",lineno);
	}
    break;

  case 21:
#line 183 "yacc.y"
    { 
		new_store_from_string("&shiftfreescaps","1",lineno);
	}
    break;

  case 22:
#line 187 "yacc.y"
    {
		new_store((yyvsp[(2) - (4)].string),(yyvsp[(3) - (4)].items),lineno);
	}
    break;

  case 23:
#line 191 "yacc.y"
    {
		set_start_group((yyvsp[(4) - (5)].string),KF_ANSI, lineno);
	}
    break;

  case 24:
#line 195 "yacc.y"
    {
		set_start_group((yyvsp[(4) - (5)].string),KF_UNICODE, lineno);
	}
    break;

  case 25:
#line 199 "yacc.y"
    {
		kmflcomp_error(lineno,"alternate starting groups not supported");
		fail(11,"obsolete syntax");
	}
    break;

  case 26:
#line 204 "yacc.y"
    { 
		new_store_from_string("&author",(yyvsp[(2) - (3)].string),lineno);
	}
    break;

  case 27:
#line 208 "yacc.y"
    { 
		new_store_from_string("&mnemoniclayout",(yyvsp[(2) - (3)].string),lineno);
	}
    break;

  case 28:
#line 212 "yacc.y"
    { 
		new_store_from_string("&ethnologuecode",(yyvsp[(2) - (3)].string),lineno);
	}
    break;

  case 31:
#line 221 "yacc.y"
    {
		(yyval.group) = kbp->groups;
	}
    break;

  case 32:
#line 225 "yacc.y"
    {
		(yyval.group) = kbp->groups;
	}
    break;

  case 33:
#line 232 "yacc.y"
    {
		(yyval.group) = (yyvsp[(1) - (1)].group);
		((yyval.group))->rules = NULL;
		((yyval.group))->nrules = 0;
		kmflcomp_warn(0,"group(%s) is empty!",((yyvsp[(1) - (1)].group))->name);
	}
    break;

  case 34:
#line 239 "yacc.y"
    {
		(yyval.group) = (yyvsp[(1) - (2)].group);
		((yyval.group))->rules = (yyvsp[(2) - (2)].rule);
		((yyval.group))->nrules = count_rules((yyvsp[(2) - (2)].rule));
		if(((yyval.group))->nrules == 0) 
			kmflcomp_warn(0,"group(%s) is empty!",((yyvsp[(1) - (2)].group))->name); 
	}
    break;

  case 35:
#line 250 "yacc.y"
    {
		(yyval.group) = gp = new_group((yyvsp[(2) - (3)].string), lineno);
		if(gp) gp->flags = 0;
	}
    break;

  case 36:
#line 256 "yacc.y"
    {
		(yyval.group) = gp = new_group((yyvsp[(2) - (4)].string), lineno);
		if(gp) gp->flags = GF_USEKEYS;
	}
    break;

  case 37:
#line 264 "yacc.y"
    {
		(yyval.rule) = (yyvsp[(1) - (1)].rule);
	}
    break;

  case 38:
#line 268 "yacc.y"
    {
		(yyval.rule) = add_rule((yyvsp[(1) - (2)].rule), (yyvsp[(2) - (2)].rule));
	}
    break;

  case 39:
#line 275 "yacc.y"
    {
		(yyval.rule) = new_rule(gp, (yyvsp[(1) - (4)].items), (yyvsp[(3) - (4)].items), lineno);
	}
    break;

  case 40:
#line 279 "yacc.y"
    {
		new_store((yyvsp[(2) - (4)].string),(yyvsp[(3) - (4)].items),lineno); (yyval.rule) = NULL;
	}
    break;

  case 41:
#line 283 "yacc.y"
    {
		(yyval.rule) = NULL;
	}
    break;

  case 42:
#line 290 "yacc.y"
    {
		(yyval.items) = new_list((yyvsp[(1) - (1)].number));
	}
    break;

  case 43:
#line 294 "yacc.y"
    {
		(yyval.items) = items_from_string((yyvsp[(1) - (1)].string),lineno);
	}
    break;

  case 44:
#line 298 "yacc.y"
    {
		(yyval.items) = add_item_to_list((yyvsp[(2) - (2)].items),(yyvsp[(1) - (2)].number));
	}
    break;

  case 45:
#line 302 "yacc.y"
    {
		(yyval.items) = add_lists((yyvsp[(2) - (2)].items),items_from_string((yyvsp[(1) - (2)].string),lineno));
	}
    break;

  case 46:
#line 309 "yacc.y"
    {
		(yyval.number) = MAKE_ITEM(ITEM_CHAR,(yyvsp[(1) - (1)].number));
	}
    break;

  case 47:
#line 313 "yacc.y"
    {
		(yyval.number) = MAKE_ITEM(ITEM_KEYSYM,(yyvsp[(2) - (3)].number));
	}
    break;

  case 48:
#line 317 "yacc.y"
    {
		(yyval.number) = MAKE_ITEM(ITEM_PLUS,0);	/* include in item list - remove later when testing validity */
	}
    break;

  case 49:
#line 321 "yacc.y"
    {
		if((n=store_number((yyvsp[(2) - (2)].string),lineno)) != UNDEFINED)
		{
			(yyval.number) = MAKE_ITEM(ITEM_ANY,n);
		}
		else
		{
			kmflcomp_warn(lineno,"store (%s) is undefined!",(yyvsp[(2) - (2)].string));
			(yyval.number) = 0;
		}
	}
    break;

  case 50:
#line 333 "yacc.y"
    {
		if((n=store_number((yyvsp[(2) - (2)].string),lineno)) != UNDEFINED)
		{
			(yyval.number) = MAKE_ITEM(ITEM_NOTANY,n);
		}
		else
		{
			kmflcomp_warn(lineno,"store (%s) is undefined!",(yyvsp[(2) - (2)].string));
			(yyval.number) = 0;
		}
	}
    break;

  case 51:
#line 345 "yacc.y"
    {
		if((n=store_number((yyvsp[(2) - (2)].string),lineno)) != UNDEFINED)
		{
			(yyval.number) = MAKE_ITEM(ITEM_OUTS,n);
		}
		else
		{
			kmflcomp_warn(lineno,"store (%s) is undefined!",(yyvsp[(2) - (2)].string));
			(yyval.number) = 0;
		}
	}
    break;

  case 52:
#line 357 "yacc.y"
    {
		(yyval.number) = MAKE_ITEM(ITEM_DEADKEY,deadkey_number((yyvsp[(2) - (2)].string), lineno));
	}
    break;

  case 53:
#line 361 "yacc.y"
    {
		(yyval.number) = MAKE_ITEM(ITEM_NUL,0);
	}
    break;

  case 54:
#line 365 "yacc.y"
    {
		if((n=store_number((yyvsp[(3) - (6)].string),lineno)) != UNDEFINED)
		{
			(yyval.number) = MAKE_PARAMETER_ITEM(ITEM_INDEX,atoi((yyvsp[(5) - (6)].string)),n);
		}
		else
		{
			kmflcomp_warn(lineno,"store (%s) is undefined!",(yyvsp[(3) - (6)].string));
			(yyval.number) = 0;
		}
	}
    break;

  case 55:
#line 377 "yacc.y"
    {
		kmflcomp_warn(lineno,"index(%s) must have TWO parameters!",(yyvsp[(3) - (4)].string));
		(yyval.number) = 0;
	}
    break;

  case 56:
#line 382 "yacc.y"
    {
		(yyval.number) = MAKE_ITEM(ITEM_RETURN,0);
	}
    break;

  case 57:
#line 386 "yacc.y"
    {
		(yyval.number) = MAKE_ITEM(ITEM_BEEP,0);
	}
    break;

  case 58:
#line 390 "yacc.y"
    {
		(yyval.number) = MAKE_ITEM(ITEM_CONTEXT,0);
	}
    break;

  case 59:
#line 394 "yacc.y"
    {
		(yyval.number) = MAKE_ITEM(ITEM_CONTEXT,atoi((yyvsp[(3) - (4)].string)));
	}
    break;

  case 60:
#line 398 "yacc.y"
    {
		(yyval.number) = MAKE_ITEM(ITEM_USE,group_number((yyvsp[(2) - (2)].string), lineno));
	}
    break;

  case 61:
#line 402 "yacc.y"
    {
		(yyval.number) = MAKE_ITEM(ITEM_MATCH,0);
	}
    break;

  case 62:
#line 406 "yacc.y"
    {
		(yyval.number) = MAKE_ITEM(ITEM_NOMATCH,0);
	}
    break;

  case 63:
#line 410 "yacc.y"
    {
		kmflcomp_error(lineno,"call keyword not implemented");
		fail(12,"unsupported keyword");
	}
    break;

  case 64:
#line 415 "yacc.y"
    {
		kmflcomp_error(lineno,"switch keyword not implemented");
		fail(11,"obsolete syntax");
	}
    break;

  case 65:
#line 420 "yacc.y"
    {
		STORE *sp;		/* check for named constants */
		sp = find_store((yyvsp[(2) - (2)].string));
		if(sp)
		{
			(yyval.number) = *sp->items;
		}
		else
		{
			(yyval.number) = 0;
			kmflcomp_error(lineno,"undefined constant");
		}
	}
    break;

  case 66:
#line 434 "yacc.y"
    {
		kmflcomp_error(lineno,"illegal or unrecognized item in rule or store");
	}
    break;

  case 67:
#line 441 "yacc.y"
    { 	
		(yyval.string) = (yyvsp[(2) - (3)].string);
	}
    break;

  case 68:
#line 446 "yacc.y"
    {
		(yyval.string) = new_string(0); /* allow for empty strings */
	}
    break;

  case 69:
#line 453 "yacc.y"
    {
		(yyval.string) = (yyvsp[(2) - (3)].string);
	}
    break;

  case 70:
#line 460 "yacc.y"
    {	
		(yyval.number) = make_keysym(lineno, 0,string_to_keysym((yyvsp[(1) - (1)].string),lineno));
	}
    break;

  case 71:
#line 465 "yacc.y"
    {	
		(yyval.number) = make_keysym(lineno, 0,string_to_keysym((yyvsp[(2) - (2)].string),lineno));
	}
    break;

  case 72:
#line 470 "yacc.y"
    {
		(yyval.number) = make_keysym(lineno, (yyvsp[(1) - (2)].number),(yyvsp[(2) - (2)].number));
	}
    break;

  case 73:
#line 474 "yacc.y"
    {
		(yyval.number) = make_keysym(lineno, 0,(yyvsp[(1) - (1)].number));
	}
    break;

  case 74:
#line 478 "yacc.y"
    {
		(yyval.number) = make_xkeysym(lineno, (yyvsp[(1) - (2)].number), (yyvsp[(2) - (2)].number));
	}
    break;

  case 75:
#line 482 "yacc.y"
    {
		(yyval.number) = make_xkeysym(lineno, 0, (yyvsp[(1) - (1)].number));
	}
    break;

  case 76:
#line 489 "yacc.y"
    {
		(yyval.number) = (yyvsp[(1) - (2)].number) | (yyvsp[(2) - (2)].number);
	}
    break;

  case 77:
#line 493 "yacc.y"
    {
		(yyval.number) = (yyvsp[(1) - (1)].number);
	}
    break;

  case 78:
#line 500 "yacc.y"
    {	
		(yyval.string) = new_string((yyvsp[(1) - (1)].number));
	}
    break;

  case 79:
#line 504 "yacc.y"
    {
		(yyval.string) = add_char((yyvsp[(2) - (2)].string),(yyvsp[(1) - (2)].number));
	}
    break;


/* Line 1267 of yacc.c.  */
#line 2175 "yacc.c"
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


#line 510 "yacc.y"


void yyerror(char *str)
{
    fflush (stdout); (void) fflush (stderr);
    fprintf (stderr, "Error: %s (line %d)\n", str, lineno);
    fflush (stderr); errcount++;
}

#ifdef _WIN32
int yywrap(void) 
{
	return 1;
}
#endif

