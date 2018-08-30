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
    TOK_KEYBOARDVERSION = 282,
    TOK_MATCH = 283,
    TOK_MESSAGE = 284,
    TOK_MNEMONIC = 285,
    TOK_NOMATCH = 286,
    TOK_NAME = 287,
    TOK_NOTANY = 288,
    TOK_NUL = 289,
    TOK_NUMBER = 290,
    TOK_NL = 291,
    TOK_OUTS = 292,
    TOK_RAWKEY = 293,
    TOK_PLUS = 294,
    TOK_QM = 295,
    TOK_RTN = 296,
    TOK_SB = 297,
    TOK_SHIFT = 298,
    TOK_STORE = 299,
    TOK_STOREINSTORE = 300,
    TOK_SWITCH = 301,
    TOK_TARGETS = 302,
    TOK_UNICODE = 303,
    TOK_USE = 304,
    TOK_USINGKEYS = 305,
    TOK_UTF = 306,
    TOK_VERSION = 307,
    TOK_VISUALKEYBOARD = 308,
    TOK_XKEYSYM = 309
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
#define TOK_KEYBOARDVERSION 282
#define TOK_MATCH 283
#define TOK_MESSAGE 284
#define TOK_MNEMONIC 285
#define TOK_NOMATCH 286
#define TOK_NAME 287
#define TOK_NOTANY 288
#define TOK_NUL 289
#define TOK_NUMBER 290
#define TOK_NL 291
#define TOK_OUTS 292
#define TOK_RAWKEY 293
#define TOK_PLUS 294
#define TOK_QM 295
#define TOK_RTN 296
#define TOK_SB 297
#define TOK_SHIFT 298
#define TOK_STORE 299
#define TOK_STOREINSTORE 300
#define TOK_SWITCH 301
#define TOK_TARGETS 302
#define TOK_UNICODE 303
#define TOK_USE 304
#define TOK_USINGKEYS 305
#define TOK_UTF 306
#define TOK_VERSION 307
#define TOK_VISUALKEYBOARD 308
#define TOK_XKEYSYM 309

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
	

#line 247 "yacc.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_YACC_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 264 "yacc.c" /* yacc.c:358  */

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
#define YYFINAL  61
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   279

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  55
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  16
/* YYNRULES -- Number of rules.  */
#define YYNRULES  83
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  171

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   309

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
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   105,   105,   111,   120,   121,   125,   129,   133,   137,
     141,   145,   149,   153,   157,   161,   165,   169,   173,   177,
     181,   185,   189,   193,   197,   201,   205,   209,   214,   218,
     222,   226,   230,   234,   235,   239,   243,   250,   257,   268,
     274,   282,   286,   293,   297,   301,   308,   312,   316,   320,
     327,   331,   335,   339,   351,   363,   375,   379,   383,   395,
     400,   404,   408,   412,   416,   420,   424,   428,   433,   438,
     452,   459,   464,   471,   478,   483,   488,   492,   496,   500,
     507,   511,   518,   522
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
  "TOK_LAYOUT", "TOK_KEYBOARDVERSION", "TOK_MATCH", "TOK_MESSAGE",
  "TOK_MNEMONIC", "TOK_NOMATCH", "TOK_NAME", "TOK_NOTANY", "TOK_NUL",
  "TOK_NUMBER", "TOK_NL", "TOK_OUTS", "TOK_RAWKEY", "TOK_PLUS", "TOK_QM",
  "TOK_RTN", "TOK_SB", "TOK_SHIFT", "TOK_STORE", "TOK_STOREINSTORE",
  "TOK_SWITCH", "TOK_TARGETS", "TOK_UNICODE", "TOK_USE", "TOK_USINGKEYS",
  "TOK_UTF", "TOK_VERSION", "TOK_VISUALKEYBOARD", "TOK_XKEYSYM", "$accept",
  "T_FILE", "T_HEADER", "T_HEADLINE", "T_GROUPS", "T_GROUP", "T_GHEADER",
  "T_RULES", "T_RULELINE", "T_ITEMS", "T_ITEM", "T_STRING", "T_PARAMETER",
  "T_KEYDEF", "T_KEYMODS", "T_BYTES", YY_NULLPTR
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
     305,   306,   307,   308,   309
};
# endif

#define YYPACT_NINF -49

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-49)))

#define YYTABLE_NINF -1

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     106,   -19,   -36,    -4,   -25,   -17,   -16,   -36,    17,   -36,
      35,     4,   -36,    -4,    -4,   -36,   -36,    -4,   -49,    35,
     -36,    30,    -4,   -36,    54,    34,   156,   -49,    34,   188,
       7,    -1,    33,    17,    36,    39,   -49,   -49,   -49,    40,
      41,    42,    17,   -34,    10,    43,    44,    45,    46,    47,
      48,    49,    50,    52,    53,   230,    56,    19,    57,    58,
      59,   -49,   -49,   -49,   -49,    35,   -49,    35,    62,    35,
      17,   -49,    63,   -49,   -49,    35,   -49,   -49,   -49,    35,
     -49,   -49,    10,    35,    35,    35,   -49,   188,    68,   230,
     230,    35,   -49,    60,   -49,   -49,   -49,   -49,   -49,   -49,
     -49,    88,   -49,    65,   -49,   -49,   -49,   -49,    61,    20,
     -49,   -49,   -49,   -49,   -49,   -49,   -49,   -49,   -49,   -49,
      66,   -49,    35,   -49,   -49,   -49,   -49,   -49,    17,   -49,
     -49,    17,   -49,   -49,    64,   230,   -49,   -49,   -49,   230,
     -49,   -49,   -28,   -49,   -49,   -49,    69,   -49,   -49,   -49,
     -49,   -49,    71,    90,    32,   -49,    72,    73,   -49,    35,
     -49,   -49,   -49,   -49,    17,   -49,   -49,    76,    91,   -49,
     -49
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    34,     0,
       0,     0,     0,     0,     0,     0,     4,     3,    35,    37,
       0,     0,     0,    82,     0,     0,    23,    21,    22,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     1,     2,     5,    36,     0,    61,     0,    62,     0,
       0,    70,     0,    65,    66,     0,    57,    50,    45,     0,
      52,    60,     0,     0,     0,     0,    38,    41,     0,    46,
      47,     0,    72,     0,    28,    83,    15,    14,    16,    33,
      32,     0,    39,     0,    77,    81,    79,    74,     0,     0,
       9,    18,    19,    20,    13,    12,    17,    31,     7,     6,
       0,    29,     0,    11,    10,    30,    53,    67,     0,    56,
      69,     0,    54,    55,     0,     0,    68,    64,    42,     0,
      48,    49,     0,    71,    73,    40,     0,    76,    80,    78,
      75,    24,     0,     0,     0,    51,     0,     0,    25,     0,
       8,    26,    63,    59,     0,    44,    43,     0,     0,    27,
      58
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -49,   -49,    89,   -49,   -15,   -49,   -49,    27,   -49,   -48,
     -49,    15,   -18,    37,   -49,    -8
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,    24,    25,    26,    27,    28,    29,    86,    87,    88,
      89,    90,    43,   108,   109,    35
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
      40,    55,   102,    30,    31,    48,    50,   120,   158,    54,
      62,    36,    33,    64,    59,    33,   103,    32,    34,    37,
      38,   159,    39,    93,    41,    95,    45,    46,    47,    49,
      51,    52,    53,    33,   101,    56,    31,    58,    60,    92,
     163,   140,   141,    42,    31,   164,    44,   126,   104,   127,
      31,   129,    57,   105,    61,    10,    91,   132,   147,   107,
      31,   133,   130,   148,   106,   135,   136,   137,   122,    94,
     128,   131,    96,   142,   149,    97,    98,    99,   100,   110,
     111,   112,   113,   114,   115,   116,   117,   156,   118,   119,
     139,   157,   121,   123,   124,   125,   144,   107,   162,   170,
     143,   145,   151,   146,   152,   160,   155,   161,   165,   166,
       1,     2,   169,     3,   138,    63,     4,     5,     6,   134,
     153,     7,     0,   154,   150,     8,     9,    10,     0,    11,
       0,    12,    13,    14,     0,    15,    16,     0,    17,     0,
       0,   167,    18,     0,     0,     0,     0,     0,     0,     0,
      19,     0,     0,    20,    21,     0,   168,     0,    22,    23,
       1,     2,     0,     3,     0,     0,     4,     5,     6,     0,
       0,     7,     0,     0,     0,     8,     9,     0,     0,    11,
       0,    12,    13,    14,     0,    15,    16,     0,    17,     0,
       0,    65,    18,     0,    66,     0,     0,    67,     0,     0,
      19,     0,    68,    20,    21,    69,    70,    71,    22,    23,
       0,     0,    72,     0,     0,     0,    73,     0,     0,    74,
       0,    75,    76,    77,    78,    79,     0,    80,    31,    81,
      82,     0,    83,    65,    84,     0,    66,    85,     0,    67,
       0,     0,     0,     0,    68,     0,     0,    69,    70,    71,
       0,     0,     0,     0,    72,     0,     0,     0,    73,     0,
       0,    74,     0,    75,    76,    77,     0,    79,     0,    80,
      31,    81,    82,     0,     0,     0,    84,     0,     0,    85
};

static const yytype_int16 yycheck[] =
{
       8,    19,    36,    22,    40,    13,    14,    55,    36,    17,
      25,    36,    16,    28,    22,    16,    50,     2,     3,    36,
      36,    49,     7,    31,     9,    33,    11,    12,    13,    14,
      15,    16,    17,    16,    42,    20,    40,    22,    23,    40,
       8,    89,    90,     8,    40,    13,    42,    65,    38,    67,
      40,    69,    22,    43,     0,    21,    49,    75,    38,    44,
      40,    79,    70,    43,    54,    83,    84,    85,    49,    36,
       8,     8,    36,    91,    54,    36,    36,    36,    36,    36,
      36,    36,    36,    36,    36,    36,    36,   135,    36,    36,
      22,   139,    36,    36,    36,    36,     8,    82,     8,     8,
      40,    36,    36,    42,   122,    36,    42,    36,    36,    36,
       4,     5,    36,     7,    87,    26,    10,    11,    12,    82,
     128,    15,    -1,   131,   109,    19,    20,    21,    -1,    23,
      -1,    25,    26,    27,    -1,    29,    30,    -1,    32,    -1,
      -1,   159,    36,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      44,    -1,    -1,    47,    48,    -1,   164,    -1,    52,    53,
       4,     5,    -1,     7,    -1,    -1,    10,    11,    12,    -1,
      -1,    15,    -1,    -1,    -1,    19,    20,    -1,    -1,    23,
      -1,    25,    26,    27,    -1,    29,    30,    -1,    32,    -1,
      -1,     3,    36,    -1,     6,    -1,    -1,     9,    -1,    -1,
      44,    -1,    14,    47,    48,    17,    18,    19,    52,    53,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    -1,    31,
      -1,    33,    34,    35,    36,    37,    -1,    39,    40,    41,
      42,    -1,    44,     3,    46,    -1,     6,    49,    -1,     9,
      -1,    -1,    -1,    -1,    14,    -1,    -1,    17,    18,    19,
      -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      -1,    31,    -1,    33,    34,    35,    -1,    37,    -1,    39,
      40,    41,    42,    -1,    -1,    -1,    46,    -1,    -1,    49
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     4,     5,     7,    10,    11,    12,    15,    19,    20,
      21,    23,    25,    26,    27,    29,    30,    32,    36,    44,
      47,    48,    52,    53,    56,    57,    58,    59,    60,    61,
      22,    40,    66,    16,    66,    70,    36,    36,    36,    66,
      70,    66,     8,    67,    42,    66,    66,    66,    70,    66,
      70,    66,    66,    66,    70,    67,    66,    22,    66,    70,
      66,     0,    59,    57,    59,     3,     6,     9,    14,    17,
      18,    19,    24,    28,    31,    33,    34,    35,    36,    37,
      39,    41,    42,    44,    46,    49,    62,    63,    64,    65,
      66,    49,    40,    70,    36,    70,    36,    36,    36,    36,
      36,    70,    36,    50,    38,    43,    54,    66,    68,    69,
      36,    36,    36,    36,    36,    36,    36,    36,    36,    36,
      64,    36,    49,    36,    36,    36,    67,    67,     8,    67,
      70,     8,    67,    67,    68,    67,    67,    67,    62,    22,
      64,    64,    67,    40,     8,    36,    42,    38,    43,    54,
      66,    36,    67,    70,    70,    42,    64,    64,    36,    49,
      36,    36,     8,     8,    13,    36,    36,    67,    70,    36,
       8
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    55,    56,    56,    57,    57,    58,    58,    58,    58,
      58,    58,    58,    58,    58,    58,    58,    58,    58,    58,
      58,    58,    58,    58,    58,    58,    58,    58,    58,    58,
      58,    58,    58,    58,    58,    59,    59,    60,    60,    61,
      61,    62,    62,    63,    63,    63,    64,    64,    64,    64,
      65,    65,    65,    65,    65,    65,    65,    65,    65,    65,
      65,    65,    65,    65,    65,    65,    65,    65,    65,    65,
      65,    66,    66,    67,    68,    68,    68,    68,    68,    68,
      69,    69,    70,    70
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     2,     1,     1,     2,     3,     3,     5,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     2,     2,     2,     4,     5,     5,     7,     3,     3,
       3,     3,     3,     3,     1,     1,     2,     1,     2,     3,
       4,     1,     2,     4,     4,     1,     1,     1,     2,     2,
       1,     3,     1,     2,     2,     2,     2,     1,     6,     4,
       1,     1,     1,     4,     2,     1,     1,     2,     2,     2,
       1,     3,     2,     3,     1,     2,     2,     1,     2,     1,
       2,     1,     1,     2
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
#line 106 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		kbp->groups = (yyvsp[0].group);
		kbp->ngroups = count_groups((yyvsp[0].group));
		kbp->nstores = count_stores(kbp->stores);
	}
#line 1486 "yacc.c" /* yacc.c:1646  */
    break;

  case 3:
#line 112 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		kbp->groups = (yyvsp[0].group);
		kbp->ngroups = count_groups((yyvsp[0].group));
		kbp->nstores = count_stores(kbp->stores);
	}
#line 1496 "yacc.c" /* yacc.c:1646  */
    break;

  case 6:
#line 126 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		new_store_from_string("&name",(yyvsp[-1].string),lineno);
	}
#line 1504 "yacc.c" /* yacc.c:1646  */
    break;

  case 7:
#line 130 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		new_store_from_string("&name",(yyvsp[-1].string),lineno);
	}
#line 1512 "yacc.c" /* yacc.c:1646  */
    break;

  case 8:
#line 134 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		new_store("&hotkey",new_list((yyvsp[-2].number)),lineno);
	}
#line 1520 "yacc.c" /* yacc.c:1646  */
    break;

  case 9:
#line 138 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		new_store_from_string("&hotkey",(yyvsp[-1].string),lineno);
	}
#line 1528 "yacc.c" /* yacc.c:1646  */
    break;

  case 10:
#line 142 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		new_store_from_string("&version",(yyvsp[-1].string),lineno);
	}
#line 1536 "yacc.c" /* yacc.c:1646  */
    break;

  case 11:
#line 146 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		new_store_from_string("&version",(yyvsp[-1].string),lineno);
	}
#line 1544 "yacc.c" /* yacc.c:1646  */
    break;

  case 12:
#line 150 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		new_store_from_string("&keyboardversion",(yyvsp[-1].string),lineno);
	}
#line 1552 "yacc.c" /* yacc.c:1646  */
    break;

  case 13:
#line 154 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		new_store_from_string("&keyboardversion",(yyvsp[-1].string),lineno);
	}
#line 1560 "yacc.c" /* yacc.c:1646  */
    break;

  case 14:
#line 158 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&bitmap",(yyvsp[-1].string),lineno);
	}
#line 1568 "yacc.c" /* yacc.c:1646  */
    break;

  case 15:
#line 162 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&bitmap",(yyvsp[-1].string),lineno);
	}
#line 1576 "yacc.c" /* yacc.c:1646  */
    break;

  case 16:
#line 166 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&copyright",(yyvsp[-1].string),lineno);	
	}
#line 1584 "yacc.c" /* yacc.c:1646  */
    break;

  case 17:
#line 170 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&message",(yyvsp[-1].string),lineno);
	}
#line 1592 "yacc.c" /* yacc.c:1646  */
    break;

  case 18:
#line 174 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&language",(yyvsp[-1].string),lineno);
	}
#line 1600 "yacc.c" /* yacc.c:1646  */
    break;

  case 19:
#line 178 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&layout",(yyvsp[-1].string),lineno);
	}
#line 1608 "yacc.c" /* yacc.c:1646  */
    break;

  case 20:
#line 182 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&layout",(yyvsp[-1].string),lineno);
	}
#line 1616 "yacc.c" /* yacc.c:1646  */
    break;

  case 21:
#line 186 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&capsalwaysoff","1",lineno);
	}
#line 1624 "yacc.c" /* yacc.c:1646  */
    break;

  case 22:
#line 190 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&capsononly","1",lineno);
	}
#line 1632 "yacc.c" /* yacc.c:1646  */
    break;

  case 23:
#line 194 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&shiftfreescaps","1",lineno);
	}
#line 1640 "yacc.c" /* yacc.c:1646  */
    break;

  case 24:
#line 198 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		new_store((yyvsp[-2].string),(yyvsp[-1].items),lineno);
	}
#line 1648 "yacc.c" /* yacc.c:1646  */
    break;

  case 25:
#line 202 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		set_start_group((yyvsp[-1].string),KF_ANSI, lineno);
	}
#line 1656 "yacc.c" /* yacc.c:1646  */
    break;

  case 26:
#line 206 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		set_start_group((yyvsp[-1].string),KF_UNICODE, lineno);
	}
#line 1664 "yacc.c" /* yacc.c:1646  */
    break;

  case 27:
#line 210 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		kmflcomp_error(lineno,"alternate starting groups not supported");
		fail(11,"obsolete syntax");
	}
#line 1673 "yacc.c" /* yacc.c:1646  */
    break;

  case 28:
#line 215 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&author",(yyvsp[-1].string),lineno);
	}
#line 1681 "yacc.c" /* yacc.c:1646  */
    break;

  case 29:
#line 219 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&targets",(yyvsp[-1].string),lineno);	
	}
#line 1689 "yacc.c" /* yacc.c:1646  */
    break;

  case 30:
#line 223 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		new_store_from_string("&visualkeyboard",(yyvsp[-1].string),lineno);
	}
#line 1697 "yacc.c" /* yacc.c:1646  */
    break;

  case 31:
#line 227 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&mnemoniclayout",(yyvsp[-1].string),lineno);
	}
#line 1705 "yacc.c" /* yacc.c:1646  */
    break;

  case 32:
#line 231 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 
		new_store_from_string("&ethnologuecode",(yyvsp[-1].string),lineno);
	}
#line 1713 "yacc.c" /* yacc.c:1646  */
    break;

  case 35:
#line 240 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.group) = kbp->groups;
	}
#line 1721 "yacc.c" /* yacc.c:1646  */
    break;

  case 36:
#line 244 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.group) = kbp->groups;
	}
#line 1729 "yacc.c" /* yacc.c:1646  */
    break;

  case 37:
#line 251 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.group) = (yyvsp[0].group);
		((yyval.group))->rules = NULL;
		((yyval.group))->nrules = 0;
		kmflcomp_warn(0,"group(%s) is empty!",((yyvsp[0].group))->name);
	}
#line 1740 "yacc.c" /* yacc.c:1646  */
    break;

  case 38:
#line 258 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.group) = (yyvsp[-1].group);
		((yyval.group))->rules = (yyvsp[0].rule);
		((yyval.group))->nrules = count_rules((yyvsp[0].rule));
		if(((yyval.group))->nrules == 0) 
			kmflcomp_warn(0,"group(%s) is empty!",((yyvsp[-1].group))->name); 
	}
#line 1752 "yacc.c" /* yacc.c:1646  */
    break;

  case 39:
#line 269 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.group) = gp = new_group((yyvsp[-1].string), lineno);
		if(gp) gp->flags = 0;
	}
#line 1761 "yacc.c" /* yacc.c:1646  */
    break;

  case 40:
#line 275 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.group) = gp = new_group((yyvsp[-2].string), lineno);
		if(gp) gp->flags = GF_USEKEYS;
	}
#line 1770 "yacc.c" /* yacc.c:1646  */
    break;

  case 41:
#line 283 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.rule) = (yyvsp[0].rule);
	}
#line 1778 "yacc.c" /* yacc.c:1646  */
    break;

  case 42:
#line 287 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.rule) = add_rule((yyvsp[-1].rule), (yyvsp[0].rule));
	}
#line 1786 "yacc.c" /* yacc.c:1646  */
    break;

  case 43:
#line 294 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.rule) = new_rule(gp, (yyvsp[-3].items), (yyvsp[-1].items), lineno);
	}
#line 1794 "yacc.c" /* yacc.c:1646  */
    break;

  case 44:
#line 298 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		new_store((yyvsp[-2].string),(yyvsp[-1].items),lineno); (yyval.rule) = NULL;
	}
#line 1802 "yacc.c" /* yacc.c:1646  */
    break;

  case 45:
#line 302 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.rule) = NULL;
	}
#line 1810 "yacc.c" /* yacc.c:1646  */
    break;

  case 46:
#line 309 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.items) = new_list((yyvsp[0].number));
	}
#line 1818 "yacc.c" /* yacc.c:1646  */
    break;

  case 47:
#line 313 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.items) = items_from_string((yyvsp[0].string),lineno);
	}
#line 1826 "yacc.c" /* yacc.c:1646  */
    break;

  case 48:
#line 317 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.items) = add_item_to_list((yyvsp[0].items),(yyvsp[-1].number));
	}
#line 1834 "yacc.c" /* yacc.c:1646  */
    break;

  case 49:
#line 321 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.items) = add_lists((yyvsp[0].items),items_from_string((yyvsp[-1].string),lineno));
	}
#line 1842 "yacc.c" /* yacc.c:1646  */
    break;

  case 50:
#line 328 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = MAKE_ITEM(ITEM_CHAR,(yyvsp[0].number));
	}
#line 1850 "yacc.c" /* yacc.c:1646  */
    break;

  case 51:
#line 332 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = MAKE_ITEM(ITEM_KEYSYM,(yyvsp[-1].number));
	}
#line 1858 "yacc.c" /* yacc.c:1646  */
    break;

  case 52:
#line 336 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = MAKE_ITEM(ITEM_PLUS,0);	/* include in item list - remove later when testing validity */
	}
#line 1866 "yacc.c" /* yacc.c:1646  */
    break;

  case 53:
#line 340 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
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
#line 1882 "yacc.c" /* yacc.c:1646  */
    break;

  case 54:
#line 352 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
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
#line 1898 "yacc.c" /* yacc.c:1646  */
    break;

  case 55:
#line 364 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
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
#line 1914 "yacc.c" /* yacc.c:1646  */
    break;

  case 56:
#line 376 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = MAKE_ITEM(ITEM_DEADKEY,deadkey_number((yyvsp[0].string), lineno));
	}
#line 1922 "yacc.c" /* yacc.c:1646  */
    break;

  case 57:
#line 380 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = MAKE_ITEM(ITEM_NUL,0);
	}
#line 1930 "yacc.c" /* yacc.c:1646  */
    break;

  case 58:
#line 384 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
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
#line 1946 "yacc.c" /* yacc.c:1646  */
    break;

  case 59:
#line 396 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		kmflcomp_warn(lineno,"index(%s) must have TWO parameters!",(yyvsp[-1].string));
		(yyval.number) = 0;
	}
#line 1955 "yacc.c" /* yacc.c:1646  */
    break;

  case 60:
#line 401 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = MAKE_ITEM(ITEM_RETURN,0);
	}
#line 1963 "yacc.c" /* yacc.c:1646  */
    break;

  case 61:
#line 405 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = MAKE_ITEM(ITEM_BEEP,0);
	}
#line 1971 "yacc.c" /* yacc.c:1646  */
    break;

  case 62:
#line 409 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = MAKE_ITEM(ITEM_CONTEXT,0);
	}
#line 1979 "yacc.c" /* yacc.c:1646  */
    break;

  case 63:
#line 413 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = MAKE_ITEM(ITEM_CONTEXT,atoi((yyvsp[-1].string)));
	}
#line 1987 "yacc.c" /* yacc.c:1646  */
    break;

  case 64:
#line 417 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = MAKE_ITEM(ITEM_USE,group_number((yyvsp[0].string), lineno));
	}
#line 1995 "yacc.c" /* yacc.c:1646  */
    break;

  case 65:
#line 421 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = MAKE_ITEM(ITEM_MATCH,0);
	}
#line 2003 "yacc.c" /* yacc.c:1646  */
    break;

  case 66:
#line 425 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = MAKE_ITEM(ITEM_NOMATCH,0);
	}
#line 2011 "yacc.c" /* yacc.c:1646  */
    break;

  case 67:
#line 429 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		kmflcomp_error(lineno,"call keyword not implemented");
		fail(12,"unsupported keyword");
	}
#line 2020 "yacc.c" /* yacc.c:1646  */
    break;

  case 68:
#line 434 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		kmflcomp_error(lineno,"switch keyword not implemented");
		fail(11,"obsolete syntax");
	}
#line 2029 "yacc.c" /* yacc.c:1646  */
    break;

  case 69:
#line 439 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
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
#line 2047 "yacc.c" /* yacc.c:1646  */
    break;

  case 70:
#line 453 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		kmflcomp_error(lineno,"illegal or unrecognized item in rule or store");
	}
#line 2055 "yacc.c" /* yacc.c:1646  */
    break;

  case 71:
#line 460 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    { 	
		(yyval.string) = (yyvsp[-1].string);
	}
#line 2063 "yacc.c" /* yacc.c:1646  */
    break;

  case 72:
#line 465 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.string) = new_string(0); /* allow for empty strings */
	}
#line 2071 "yacc.c" /* yacc.c:1646  */
    break;

  case 73:
#line 472 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.string) = (yyvsp[-1].string);
	}
#line 2079 "yacc.c" /* yacc.c:1646  */
    break;

  case 74:
#line 479 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {	
		(yyval.number) = make_keysym(lineno, 0,string_to_keysym((yyvsp[0].string),lineno));
	}
#line 2087 "yacc.c" /* yacc.c:1646  */
    break;

  case 75:
#line 484 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {	
		(yyval.number) = make_keysym(lineno, 0,string_to_keysym((yyvsp[0].string),lineno));
	}
#line 2095 "yacc.c" /* yacc.c:1646  */
    break;

  case 76:
#line 489 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = make_keysym(lineno, (yyvsp[-1].number),(yyvsp[0].number));
	}
#line 2103 "yacc.c" /* yacc.c:1646  */
    break;

  case 77:
#line 493 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = make_keysym(lineno, 0,(yyvsp[0].number));
	}
#line 2111 "yacc.c" /* yacc.c:1646  */
    break;

  case 78:
#line 497 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = make_xkeysym(lineno, (yyvsp[-1].number), (yyvsp[0].number));
	}
#line 2119 "yacc.c" /* yacc.c:1646  */
    break;

  case 79:
#line 501 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = make_xkeysym(lineno, 0, (yyvsp[0].number));
	}
#line 2127 "yacc.c" /* yacc.c:1646  */
    break;

  case 80:
#line 508 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = (yyvsp[-1].number) | (yyvsp[0].number);
	}
#line 2135 "yacc.c" /* yacc.c:1646  */
    break;

  case 81:
#line 512 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.number) = (yyvsp[0].number);
	}
#line 2143 "yacc.c" /* yacc.c:1646  */
    break;

  case 82:
#line 519 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {	
		(yyval.string) = new_string((yyvsp[0].number));
	}
#line 2151 "yacc.c" /* yacc.c:1646  */
    break;

  case 83:
#line 523 "../../kmflcomp/src/yacc.y" /* yacc.c:1646  */
    {
		(yyval.string) = add_char((yyvsp[0].string),(yyvsp[-1].number));
	}
#line 2159 "yacc.c" /* yacc.c:1646  */
    break;


#line 2163 "yacc.c" /* yacc.c:1646  */
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
#line 529 "../../kmflcomp/src/yacc.y" /* yacc.c:1906  */


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
