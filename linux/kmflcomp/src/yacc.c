/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

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
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 1 "../../kmflcomp/src/yacc.y" /* yacc.c:339  */

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

#line 89 "yacc.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
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
#ifndef YY_YY_YACC_H_INCLUDED
# define YY_YY_YACC_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
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
    TOK_TARGETS = 301,
    TOK_UNICODE = 302,
    TOK_USE = 303,
    TOK_USINGKEYS = 304,
    TOK_UTF = 305,
    TOK_VERSION = 306,
    TOK_XKEYSYM = 307
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
#define TOK_TARGETS 301
#define TOK_UNICODE 302
#define TOK_USE 303
#define TOK_USINGKEYS 304
#define TOK_UTF 305
#define TOK_VERSION 306
#define TOK_XKEYSYM 307

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 25 "../../kmflcomp/src/yacc.y" /* yacc.c:355  */

	int simple;
	ITEM number;
	char *string;
	ITEM *items;
	RULE *rule;
	GROUP *group;
	

#line 243 "yacc.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_YACC_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 260 "yacc.c" /* yacc.c:358  */

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
#else
typedef signed char yytype_int8;
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
# elif ! defined YYSIZE_T
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

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
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
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
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
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
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
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
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
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

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
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  77
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   244

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  53
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  16
/* YYNRULES -- Number of rules.  */
#define YYNRULES  80
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  163

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   307

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
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
      45,    46,    47,    48,    49,    50,    51,    52
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   103,   103,   109,   118,   119,   123,   127,   131,   135,
     139,   143,   147,   151,   155,   159,   163,   167,   171,   175,
     179,   183,   187,   191,   195,   199,   204,   208,   212,   216,
     220,   221,   225,   229,   236,   243,   254,   260,   268,   272,
     279,   283,   287,   294,   298,   302,   306,   313,   317,   321,
     325,   337,   349,   361,   365,   369,   381,   386,   390,   394,
     398,   402,   406,   410,   414,   419,   424,   438,   445,   450,
     457,   464,   469,   474,   478,   482,   486,   493,   497,   504,
     508
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
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
  "TOK_STORE", "TOK_STOREINSTORE", "TOK_SWITCH", "TOK_TARGETS",
  "TOK_UNICODE", "TOK_USE", "TOK_USINGKEYS", "TOK_UTF", "TOK_VERSION",
  "TOK_XKEYSYM", "$accept", "T_FILE", "T_HEADER", "T_HEADLINE", "T_GROUPS",
  "T_GROUP", "T_GHEADER", "T_RULES", "T_RULELINE", "T_ITEMS", "T_ITEM",
  "T_STRING", "T_PARAMETER", "T_KEYDEF", "T_KEYMODS", "T_BYTES", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307
};
# endif

#define YYPACT_NINF -34

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-34)))

#define YYTABLE_NINF -1

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     120,    -2,    -5,   -13,     0,     3,     4,    -5,    26,    -5,
      35,   -23,    74,   -13,    -5,    -5,   -13,   -34,    35,    -5,
      24,   -13,    47,    27,   165,   -34,    27,   196,     1,    -9,
      15,    26,    16,    17,   -34,   -34,   -34,    18,    21,    22,
      26,   -21,   -33,    23,    35,   -34,    35,    53,    35,    26,
     -34,    55,   -34,   -34,    35,   -34,   -34,    35,   -34,   -34,
     -33,    35,    35,    32,    74,    74,    39,    40,    44,    46,
      49,    50,    74,    51,    34,    52,    54,   -34,   -34,   -34,
     -34,   -34,    35,   -34,   196,    68,    35,   -34,    56,   -34,
     -34,   -34,   -34,   -34,   -34,   -34,    86,   -34,    61,   -34,
     -34,   -34,   -34,    64,   -27,   -34,   -34,   -34,    26,   -34,
     -34,    26,   -34,   -34,    75,   -34,   -34,   -34,   -34,   -34,
     -34,   -34,   -34,   -34,   -34,   -34,    62,   -34,    35,   -34,
     -34,    74,   -34,    74,   -24,   -34,   -34,   -34,    67,   -34,
     -34,   -34,   -34,   101,     9,   -34,   -34,    83,    85,    91,
     -34,    35,   -34,   -34,   -34,    26,   -34,   -34,   -34,    93,
     121,   -34,   -34
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    31,     0,     0,
       0,     0,     0,     0,     4,     3,    32,    34,     0,     0,
       0,    79,     0,     0,    21,    19,    20,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,    59,     0,     0,
      67,     0,    62,    63,     0,    54,    47,     0,    49,    57,
       0,     0,     0,     0,    43,    44,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     1,     2,     5,
      33,    42,     0,    35,    38,     0,     0,    69,     0,    26,
      80,    13,    12,    14,    30,    29,     0,    36,     0,    74,
      78,    76,    71,     0,     0,     9,    50,    64,     0,    53,
      66,     0,    51,    52,     0,    65,    61,    16,    45,    46,
      17,    18,    15,    28,     7,     6,     0,    27,     0,    11,
      10,     0,    39,     0,     0,    68,    70,    37,     0,    73,
      77,    75,    72,     0,     0,    48,    22,     0,     0,     0,
      23,     0,     8,    60,    56,     0,    24,    41,    40,     0,
       0,    25,    55
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -34,   -34,   109,   -34,    10,   -34,   -34,    58,   -34,   -10,
     -34,    57,   -17,    76,   -34,    -8
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,    22,    23,    24,    25,    26,    27,    83,    84,    85,
      64,    65,    41,   103,   104,    33
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
      38,    72,    63,    31,    99,    67,    29,    31,    71,   100,
     139,   150,    29,    76,    97,   140,    29,   154,    42,   101,
      28,    88,   155,    90,   151,   141,    29,   106,    98,   107,
      87,   109,    96,    78,    29,    34,    80,   112,    35,    36,
     113,   110,    31,    40,   115,   116,    74,    77,    10,    86,
      89,    91,    92,    93,   118,   119,    94,    95,   105,    30,
      32,   108,   126,   111,    37,   131,    39,   117,    43,   134,
      66,    68,    69,    70,   120,   121,    73,    44,    75,   122,
      45,   123,   128,    46,   124,   125,   127,   129,    47,   130,
     133,    48,    49,    50,   136,   135,   137,   146,    51,   102,
     143,    52,   152,   144,    53,   138,    54,    55,    56,   153,
      57,   147,    58,    29,    59,    60,   145,   102,   156,    61,
     157,   148,    62,   149,     1,     2,   158,     3,   161,   162,
       4,     5,     6,    79,   159,     7,   114,     0,     0,     8,
       9,    10,   132,    11,     0,    12,    13,   160,    14,    15,
       0,    16,     0,     0,     0,    17,     0,     0,     0,     0,
       0,   142,     0,    18,     0,     0,    19,    20,     0,     1,
       2,    21,     3,     0,     0,     4,     5,     6,     0,     0,
       7,     0,     0,     0,     8,     9,     0,     0,    11,     0,
      12,    13,     0,    14,    15,     0,    16,     0,     0,    44,
      17,     0,    45,     0,     0,    46,     0,     0,    18,     0,
      47,    19,    20,    48,    49,    50,    21,     0,     0,     0,
      51,     0,     0,    52,     0,     0,    53,     0,    54,    55,
      56,    81,    57,     0,    58,    29,    59,    60,     0,    82,
       0,    61,     0,     0,    62
};

static const yytype_int16 yycheck[] =
{
       8,    18,    12,    16,    37,    13,    39,    16,    16,    42,
      37,    35,    39,    21,    35,    42,    39,     8,    41,    52,
      22,    29,    13,    31,    48,    52,    39,    44,    49,    46,
      39,    48,    40,    23,    39,    35,    26,    54,    35,    35,
      57,    49,    16,     8,    61,    62,    22,     0,    21,    48,
      35,    35,    35,    35,    64,    65,    35,    35,    35,     2,
       3,     8,    72,     8,     7,    82,     9,    35,    11,    86,
      13,    14,    15,    16,    35,    35,    19,     3,    21,    35,
       6,    35,    48,     9,    35,    35,    35,    35,    14,    35,
      22,    17,    18,    19,     8,    39,    35,    35,    24,    42,
     108,    27,    35,   111,    30,    41,    32,    33,    34,     8,
      36,   128,    38,    39,    40,    41,    41,    60,    35,    45,
      35,   131,    48,   133,     4,     5,    35,     7,    35,     8,
      10,    11,    12,    24,   151,    15,    60,    -1,    -1,    19,
      20,    21,    84,    23,    -1,    25,    26,   155,    28,    29,
      -1,    31,    -1,    -1,    -1,    35,    -1,    -1,    -1,    -1,
      -1,   104,    -1,    43,    -1,    -1,    46,    47,    -1,     4,
       5,    51,     7,    -1,    -1,    10,    11,    12,    -1,    -1,
      15,    -1,    -1,    -1,    19,    20,    -1,    -1,    23,    -1,
      25,    26,    -1,    28,    29,    -1,    31,    -1,    -1,     3,
      35,    -1,     6,    -1,    -1,     9,    -1,    -1,    43,    -1,
      14,    46,    47,    17,    18,    19,    51,    -1,    -1,    -1,
      24,    -1,    -1,    27,    -1,    -1,    30,    -1,    32,    33,
      34,    35,    36,    -1,    38,    39,    40,    41,    -1,    43,
      -1,    45,    -1,    -1,    48
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     4,     5,     7,    10,    11,    12,    15,    19,    20,
      21,    23,    25,    26,    28,    29,    31,    35,    43,    46,
      47,    51,    54,    55,    56,    57,    58,    59,    22,    39,
      64,    16,    64,    68,    35,    35,    35,    64,    68,    64,
       8,    65,    41,    64,     3,     6,     9,    14,    17,    18,
      19,    24,    27,    30,    32,    33,    34,    36,    38,    40,
      41,    45,    48,    62,    63,    64,    64,    68,    64,    64,
      64,    68,    65,    64,    22,    64,    68,     0,    57,    55,
      57,    35,    43,    60,    61,    62,    48,    39,    68,    35,
      68,    35,    35,    35,    35,    35,    68,    35,    49,    37,
      42,    52,    64,    66,    67,    35,    65,    65,     8,    65,
      68,     8,    65,    65,    66,    65,    65,    35,    62,    62,
      35,    35,    35,    35,    35,    35,    62,    35,    48,    35,
      35,    65,    60,    22,    65,    39,     8,    35,    41,    37,
      42,    52,    64,    68,    68,    41,    35,    65,    62,    62,
      35,    48,    35,     8,     8,    13,    35,    35,    35,    65,
      68,    35,     8
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    53,    54,    54,    55,    55,    56,    56,    56,    56,
      56,    56,    56,    56,    56,    56,    56,    56,    56,    56,
      56,    56,    56,    56,    56,    56,    56,    56,    56,    56,
      56,    56,    57,    57,    58,    58,    59,    59,    60,    60,
      61,    61,    61,    62,    62,    62,    62,    63,    63,    63,
      63,    63,    63,    63,    63,    63,    63,    63,    63,    63,
      63,    63,    63,    63,    63,    63,    63,    63,    64,    64,
      65,    66,    66,    66,    66,    66,    66,    67,    67,    68,
      68
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     2,     1,     1,     2,     3,     3,     5,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     2,
       2,     2,     4,     5,     5,     7,     3,     3,     3,     3,
       3,     1,     1,     2,     1,     2,     3,     4,     1,     2,
       4,     4,     1,     1,     1,     2,     2,     1,     3,     1,
       2,     2,     2,     2,     1,     6,     4,     1,     1,     1,
       4,     2,     1,     1,     2,     2,     2,     1,     3,     2,
       3,     1,     2,     2,     1,     2,     1,     2,     1,     1,
       2
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


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
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

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
#ifndef YYINITDEPTH
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
static YYSIZE_T
yystrlen (const char *yystr)
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
static char *
yystpcpy (char *yydest, const char *yysrc)
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
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
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
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
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

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

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
      yychar = yylex ();
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
     '$$ = $1'.

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
#line 104 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		kbp->groups = (yyvsp[0].group);
		kbp->ngroups = count_groups((yyvsp[0].group));
		kbp->nstores = count_stores(kbp->stores);
	}
#line 1472 "yacc.c" /* yacc.c:1646  */
    break;

  case 3:
#line 110 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		kbp->groups = (yyvsp[0].group);
		kbp->ngroups = count_groups((yyvsp[0].group));
		kbp->nstores = count_stores(kbp->stores);
	}
#line 1482 "yacc.c" /* yacc.c:1646  */
    break;

  case 6:
#line 124 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		new_store_from_string("&name",(yyvsp[-1].string),lineno);
	}
#line 1490 "yacc.c" /* yacc.c:1646  */
    break;

  case 7:
#line 128 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		new_store_from_string("&name",(yyvsp[-1].string),lineno);
	}
#line 1498 "yacc.c" /* yacc.c:1646  */
    break;

  case 8:
#line 132 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		new_store("&hotkey",new_list((yyvsp[-2].number)),lineno);
	}
#line 1506 "yacc.c" /* yacc.c:1646  */
    break;

  case 9:
#line 136 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		new_store_from_string("&hotkey",(yyvsp[-1].string),lineno);
	}
#line 1514 "yacc.c" /* yacc.c:1646  */
    break;

  case 10:
#line 140 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		new_store_from_string("&version",(yyvsp[-1].string),lineno);
	}
#line 1522 "yacc.c" /* yacc.c:1646  */
    break;

  case 11:
#line 144 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		new_store_from_string("&version",(yyvsp[-1].string),lineno);
	}
#line 1530 "yacc.c" /* yacc.c:1646  */
    break;

  case 12:
#line 148 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&bitmap",(yyvsp[-1].string),lineno);
	}
#line 1538 "yacc.c" /* yacc.c:1646  */
    break;

  case 13:
#line 152 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&bitmap",(yyvsp[-1].string),lineno);
	}
#line 1546 "yacc.c" /* yacc.c:1646  */
    break;

  case 14:
#line 156 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&copyright",(yyvsp[-1].string),lineno);	
	}
#line 1554 "yacc.c" /* yacc.c:1646  */
    break;

  case 15:
#line 160 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&message",(yyvsp[-1].string),lineno);
	}
#line 1562 "yacc.c" /* yacc.c:1646  */
    break;

  case 16:
#line 164 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&language",(yyvsp[-1].items),lineno);
	}
#line 1570 "yacc.c" /* yacc.c:1646  */
    break;

  case 17:
#line 168 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&layout",(yyvsp[-1].string),lineno);
	}
#line 1578 "yacc.c" /* yacc.c:1646  */
    break;

  case 18:
#line 172 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&layout",(yyvsp[-1].string),lineno);
	}
#line 1586 "yacc.c" /* yacc.c:1646  */
    break;

  case 19:
#line 176 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&capsalwaysoff","1",lineno);
	}
#line 1594 "yacc.c" /* yacc.c:1646  */
    break;

  case 20:
#line 180 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&capsononly","1",lineno);
	}
#line 1602 "yacc.c" /* yacc.c:1646  */
    break;

  case 21:
#line 184 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&shiftfreescaps","1",lineno);
	}
#line 1610 "yacc.c" /* yacc.c:1646  */
    break;

  case 22:
#line 188 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		new_store((yyvsp[-2].string),(yyvsp[-1].items),lineno);
	}
#line 1618 "yacc.c" /* yacc.c:1646  */
    break;

  case 23:
#line 192 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		set_start_group((yyvsp[-1].string),KF_ANSI, lineno);
	}
#line 1626 "yacc.c" /* yacc.c:1646  */
    break;

  case 24:
#line 196 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		set_start_group((yyvsp[-1].string),KF_UNICODE, lineno);
	}
#line 1634 "yacc.c" /* yacc.c:1646  */
    break;

  case 25:
#line 200 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		kmflcomp_error(lineno,"alternate starting groups not supported");
		fail(11,"obsolete syntax");
	}
#line 1643 "yacc.c" /* yacc.c:1646  */
    break;

  case 26:
#line 205 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&author",(yyvsp[-1].string),lineno);
	}
#line 1651 "yacc.c" /* yacc.c:1646  */
    break;

  case 27:
#line 209 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&targets",(yyvsp[-1].string),lineno);	
	}
#line 1659 "yacc.c" /* yacc.c:1646  */
    break;

  case 28:
#line 213 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&mnemoniclayout",(yyvsp[-1].string),lineno);
	}
#line 1667 "yacc.c" /* yacc.c:1646  */
    break;

  case 29:
#line 217 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&ethnologuecode",(yyvsp[-1].string),lineno);
	}
#line 1675 "yacc.c" /* yacc.c:1646  */
    break;

  case 32:
#line 226 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.group) = kbp->groups;
	}
#line 1683 "yacc.c" /* yacc.c:1646  */
    break;

  case 33:
#line 230 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.group) = kbp->groups;
	}
#line 1691 "yacc.c" /* yacc.c:1646  */
    break;

  case 34:
#line 237 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.group) = (yyvsp[0].group);
		((yyval.group))->rules = NULL;
		((yyval.group))->nrules = 0;
		kmflcomp_warn(0,"group(%s) is empty!",((yyvsp[0].group))->name);
	}
#line 1702 "yacc.c" /* yacc.c:1646  */
    break;

  case 35:
#line 244 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.group) = (yyvsp[-1].group);
		((yyval.group))->rules = (yyvsp[0].rule);
		((yyval.group))->nrules = count_rules((yyvsp[0].rule));
		if(((yyval.group))->nrules == 0) 
			kmflcomp_warn(0,"group(%s) is empty!",((yyvsp[-1].group))->name); 
	}
#line 1714 "yacc.c" /* yacc.c:1646  */
    break;

  case 36:
#line 255 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.group) = gp = new_group((yyvsp[-1].string), lineno);
		if(gp) gp->flags = 0;
	}
#line 1723 "yacc.c" /* yacc.c:1646  */
    break;

  case 37:
#line 261 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.group) = gp = new_group((yyvsp[-2].string), lineno);
		if(gp) gp->flags = GF_USEKEYS;
	}
#line 1732 "yacc.c" /* yacc.c:1646  */
    break;

  case 38:
#line 269 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.rule) = (yyvsp[0].rule);
	}
#line 1740 "yacc.c" /* yacc.c:1646  */
    break;

  case 39:
#line 273 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.rule) = add_rule((yyvsp[-1].rule), (yyvsp[0].rule));
	}
#line 1748 "yacc.c" /* yacc.c:1646  */
    break;

  case 40:
#line 280 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.rule) = new_rule(gp, (yyvsp[-3].items), (yyvsp[-1].items), lineno);
	}
#line 1756 "yacc.c" /* yacc.c:1646  */
    break;

  case 41:
#line 284 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		new_store((yyvsp[-2].string),(yyvsp[-1].items),lineno); (yyval.rule) = NULL;
	}
#line 1764 "yacc.c" /* yacc.c:1646  */
    break;

  case 42:
#line 288 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.rule) = NULL;
	}
#line 1772 "yacc.c" /* yacc.c:1646  */
    break;

  case 43:
#line 295 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.items) = new_list((yyvsp[0].number));
	}
#line 1780 "yacc.c" /* yacc.c:1646  */
    break;

  case 44:
#line 299 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.items) = items_from_string((yyvsp[0].string),lineno);
	}
#line 1788 "yacc.c" /* yacc.c:1646  */
    break;

  case 45:
#line 303 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.items) = add_item_to_list((yyvsp[0].items),(yyvsp[-1].number));
	}
#line 1796 "yacc.c" /* yacc.c:1646  */
    break;

  case 46:
#line 307 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.items) = add_lists((yyvsp[0].items),items_from_string((yyvsp[-1].string),lineno));
	}
#line 1804 "yacc.c" /* yacc.c:1646  */
    break;

  case 47:
#line 314 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = MAKE_ITEM(ITEM_CHAR,(yyvsp[0].number));
	}
#line 1812 "yacc.c" /* yacc.c:1646  */
    break;

  case 48:
#line 318 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = MAKE_ITEM(ITEM_KEYSYM,(yyvsp[-1].number));
	}
#line 1820 "yacc.c" /* yacc.c:1646  */
    break;

  case 49:
#line 322 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = MAKE_ITEM(ITEM_PLUS,0);	/* include in item list - remove later when testing validity */
	}
#line 1828 "yacc.c" /* yacc.c:1646  */
    break;

  case 50:
#line 326 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		if((n=store_number((yyvsp[0].string),lineno)) != UNDEFINED)
		{
			(yyval.number) = MAKE_ITEM(ITEM_ANY,n);
		}
		else
		{
			kmflcomp_warn(lineno,"store (%s) is undefined!",(yyvsp[0].string));
			(yyval.number) = 0;
		}
	}
#line 1844 "yacc.c" /* yacc.c:1646  */
    break;

  case 51:
#line 338 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		if((n=store_number((yyvsp[0].string),lineno)) != UNDEFINED)
		{
			(yyval.number) = MAKE_ITEM(ITEM_NOTANY,n);
		}
		else
		{
			kmflcomp_warn(lineno,"store (%s) is undefined!",(yyvsp[0].string));
			(yyval.number) = 0;
		}
	}
#line 1860 "yacc.c" /* yacc.c:1646  */
    break;

  case 52:
#line 350 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		if((n=store_number((yyvsp[0].string),lineno)) != UNDEFINED)
		{
			(yyval.number) = MAKE_ITEM(ITEM_OUTS,n);
		}
		else
		{
			kmflcomp_warn(lineno,"store (%s) is undefined!",(yyvsp[0].string));
			(yyval.number) = 0;
		}
	}
#line 1876 "yacc.c" /* yacc.c:1646  */
    break;

  case 53:
#line 362 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = MAKE_ITEM(ITEM_DEADKEY,deadkey_number((yyvsp[0].string), lineno));
	}
#line 1884 "yacc.c" /* yacc.c:1646  */
    break;

  case 54:
#line 366 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = MAKE_ITEM(ITEM_NUL,0);
	}
#line 1892 "yacc.c" /* yacc.c:1646  */
    break;

  case 55:
#line 370 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		if((n=store_number((yyvsp[-3].string),lineno)) != UNDEFINED)
		{
			(yyval.number) = MAKE_PARAMETER_ITEM(ITEM_INDEX,atoi((yyvsp[-1].string)),n);
		}
		else
		{
			kmflcomp_warn(lineno,"store (%s) is undefined!",(yyvsp[-3].string));
			(yyval.number) = 0;
		}
	}
#line 1908 "yacc.c" /* yacc.c:1646  */
    break;

  case 56:
#line 382 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		kmflcomp_warn(lineno,"index(%s) must have TWO parameters!",(yyvsp[-1].string));
		(yyval.number) = 0;
	}
#line 1917 "yacc.c" /* yacc.c:1646  */
    break;

  case 57:
#line 387 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = MAKE_ITEM(ITEM_RETURN,0);
	}
#line 1925 "yacc.c" /* yacc.c:1646  */
    break;

  case 58:
#line 391 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = MAKE_ITEM(ITEM_BEEP,0);
	}
#line 1933 "yacc.c" /* yacc.c:1646  */
    break;

  case 59:
#line 395 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = MAKE_ITEM(ITEM_CONTEXT,0);
	}
#line 1941 "yacc.c" /* yacc.c:1646  */
    break;

  case 60:
#line 399 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = MAKE_ITEM(ITEM_CONTEXT,atoi((yyvsp[-1].string)));
	}
#line 1949 "yacc.c" /* yacc.c:1646  */
    break;

  case 61:
#line 403 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = MAKE_ITEM(ITEM_USE,group_number((yyvsp[0].string), lineno));
	}
#line 1957 "yacc.c" /* yacc.c:1646  */
    break;

  case 62:
#line 407 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = MAKE_ITEM(ITEM_MATCH,0);
	}
#line 1965 "yacc.c" /* yacc.c:1646  */
    break;

  case 63:
#line 411 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = MAKE_ITEM(ITEM_NOMATCH,0);
	}
#line 1973 "yacc.c" /* yacc.c:1646  */
    break;

  case 64:
#line 415 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		kmflcomp_error(lineno,"call keyword not implemented");
		fail(12,"unsupported keyword");
	}
#line 1982 "yacc.c" /* yacc.c:1646  */
    break;

  case 65:
#line 420 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		kmflcomp_error(lineno,"switch keyword not implemented");
		fail(11,"obsolete syntax");
	}
#line 1991 "yacc.c" /* yacc.c:1646  */
    break;

  case 66:
#line 425 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		STORE *sp;		/* check for named constants */
		sp = find_store((yyvsp[0].string));
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
#line 2009 "yacc.c" /* yacc.c:1646  */
    break;

  case 67:
#line 439 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		kmflcomp_error(lineno,"illegal or unrecognized item in rule or store");
	}
#line 2017 "yacc.c" /* yacc.c:1646  */
    break;

  case 68:
#line 446 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 	
		(yyval.string) = (yyvsp[-1].string);
	}
#line 2025 "yacc.c" /* yacc.c:1646  */
    break;

  case 69:
#line 451 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.string) = new_string(0); /* allow for empty strings */
	}
#line 2033 "yacc.c" /* yacc.c:1646  */
    break;

  case 70:
#line 458 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.string) = (yyvsp[-1].string);
	}
#line 2041 "yacc.c" /* yacc.c:1646  */
    break;

  case 71:
#line 465 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {	
		(yyval.number) = make_keysym(lineno, 0,string_to_keysym((yyvsp[0].string),lineno));
	}
#line 2049 "yacc.c" /* yacc.c:1646  */
    break;

  case 72:
#line 470 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {	
		(yyval.number) = make_keysym(lineno, 0,string_to_keysym((yyvsp[0].string),lineno));
	}
#line 2057 "yacc.c" /* yacc.c:1646  */
    break;

  case 73:
#line 475 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = make_keysym(lineno, (yyvsp[-1].number),(yyvsp[0].number));
	}
#line 2065 "yacc.c" /* yacc.c:1646  */
    break;

  case 74:
#line 479 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = make_keysym(lineno, 0,(yyvsp[0].number));
	}
#line 2073 "yacc.c" /* yacc.c:1646  */
    break;

  case 75:
#line 483 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = make_xkeysym(lineno, (yyvsp[-1].number), (yyvsp[0].number));
	}
#line 2081 "yacc.c" /* yacc.c:1646  */
    break;

  case 76:
#line 487 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = make_xkeysym(lineno, 0, (yyvsp[0].number));
	}
#line 2089 "yacc.c" /* yacc.c:1646  */
    break;

  case 77:
#line 494 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = (yyvsp[-1].number) | (yyvsp[0].number);
	}
#line 2097 "yacc.c" /* yacc.c:1646  */
    break;

  case 78:
#line 498 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = (yyvsp[0].number);
	}
#line 2105 "yacc.c" /* yacc.c:1646  */
    break;

  case 79:
#line 505 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {	
		(yyval.string) = new_string((yyvsp[0].number));
	}
#line 2113 "yacc.c" /* yacc.c:1646  */
    break;

  case 80:
#line 509 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.string) = add_char((yyvsp[0].string),(yyvsp[-1].number));
	}
#line 2121 "yacc.c" /* yacc.c:1646  */
    break;


#line 2125 "yacc.c" /* yacc.c:1646  */
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

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
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

  /* Do not reclaim the symbols of the rule whose action triggered
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
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

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
  /* Do not reclaim the symbols of the rule whose action triggered
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
  return yyresult;
}
#line 515 "../../kmflcomp/src/yacc.y" /* yacc.c:1906  */


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
