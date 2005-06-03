/* A Bison parser, made by GNU Bison 1.875.  */

/* Skeleton parser for Yacc-like parsing with Bison,
   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002 Free Software Foundation, Inc.

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
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* Written by Richard Stallman by simplifying the original so called
   ``semantic'' parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

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
     TOK_NUL = 287,
     TOK_NUMBER = 288,
     TOK_NL = 289,
     TOK_OUTS = 290,
     TOK_RAWKEY = 291,
     TOK_PLUS = 292,
     TOK_QM = 293,
     TOK_RTN = 294,
     TOK_SB = 295,
     TOK_SHIFT = 296,
     TOK_STORE = 297,
     TOK_STOREINSTORE = 298,
     TOK_SWITCH = 299,
     TOK_UNICODE = 300,
     TOK_USE = 301,
     TOK_USINGKEYS = 302,
     TOK_UTF = 303,
     TOK_VERSION = 304
   };
#endif
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
#define TOK_NUL 287
#define TOK_NUMBER 288
#define TOK_NL 289
#define TOK_OUTS 290
#define TOK_RAWKEY 291
#define TOK_PLUS 292
#define TOK_QM 293
#define TOK_RTN 294
#define TOK_SB 295
#define TOK_SHIFT 296
#define TOK_STORE 297
#define TOK_STOREINSTORE 298
#define TOK_SWITCH 299
#define TOK_UNICODE 300
#define TOK_USE 301
#define TOK_USINGKEYS 302
#define TOK_UTF 303
#define TOK_VERSION 304




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

#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)
#line 25 "yacc.y"
typedef union YYSTYPE {
	int simple;
	ITEM number;
	char *string;
	ITEM *items;
	RULE *rule;
	GROUP *group;
	} YYSTYPE;
/* Line 191 of yacc.c.  */
#line 205 "yacc.c"
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 214 of yacc.c.  */
#line 217 "yacc.c"

#if ! defined (yyoverflow) || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# if YYSTACK_USE_ALLOCA
#  define YYSTACK_ALLOC alloca
# else
#  ifndef YYSTACK_USE_ALLOCA
#   if defined (alloca) || defined (_ALLOCA_H)
#    define YYSTACK_ALLOC alloca
#   else
#    ifdef __GNUC__
#     define YYSTACK_ALLOC __builtin_alloca
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning. */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
# else
#  if defined (__STDC__) || defined (__cplusplus)
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   define YYSIZE_T size_t
#  endif
#  define YYSTACK_ALLOC malloc
#  define YYSTACK_FREE free
# endif
#endif /* ! defined (yyoverflow) || YYERROR_VERBOSE */


#if (! defined (yyoverflow) \
     && (! defined (__cplusplus) \
	 || (YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  short yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE))				\
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  register YYSIZE_T yyi;		\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (0)
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
    while (0)

#endif

#if defined (__STDC__) || defined (__cplusplus)
   typedef signed char yysigned_char;
#else
   typedef short yysigned_char;
#endif

/* YYFINAL -- State number of the termination state. */
#define YYFINAL  74
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   263

/* YYNTOKENS -- Number of terminals. */
#define YYNTOKENS  50
/* YYNNTS -- Number of nonterminals. */
#define YYNNTS  16
/* YYNRULES -- Number of rules. */
#define YYNRULES  76
/* YYNRULES -- Number of states. */
#define YYNSTATES  156

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   304

#define YYTRANSLATE(YYX) 						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const unsigned char yytranslate[] =
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
      45,    46,    47,    48,    49
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const unsigned char yyprhs[] =
{
       0,     0,     3,     6,     8,    10,    13,    17,    21,    27,
      31,    35,    39,    43,    47,    51,    55,    59,    63,    67,
      70,    73,    76,    81,    87,    93,   101,   105,   109,   113,
     117,   119,   121,   124,   126,   129,   133,   138,   140,   143,
     148,   153,   155,   157,   159,   162,   165,   167,   171,   173,
     176,   179,   182,   184,   191,   196,   198,   200,   202,   207,
     210,   212,   214,   217,   220,   223,   225,   229,   232,   236,
     238,   241,   244,   246,   249,   251,   253
};

/* YYRHS -- A `-1'-separated list of the rules' RHS. */
static const yysigned_char yyrhs[] =
{
      51,     0,    -1,    52,    54,    -1,    54,    -1,    53,    -1,
      53,    52,    -1,    31,    65,    34,    -1,    31,    61,    34,
      -1,    23,    40,    63,    40,    34,    -1,    23,    61,    34,
      -1,    49,    65,    34,    -1,    49,    61,    34,    -1,     7,
      65,    34,    -1,     7,    61,    34,    -1,    15,    61,    34,
      -1,    28,    61,    34,    -1,    25,    59,    34,    -1,    26,
      61,    34,    -1,    26,    65,    34,    -1,    11,    34,    -1,
      12,    34,    -1,    10,    34,    -1,    42,    62,    59,    34,
      -1,     4,    22,    46,    62,    34,    -1,    45,    22,    46,
      62,    34,    -1,     4,    22,    46,    62,    46,    62,    34,
      -1,     5,    61,    34,    -1,    29,    61,    34,    -1,    20,
      61,    34,    -1,    19,    65,    34,    -1,    34,    -1,    55,
      -1,    55,    54,    -1,    56,    -1,    56,    57,    -1,    21,
      62,    34,    -1,    21,    62,    47,    34,    -1,    58,    -1,
      58,    57,    -1,    59,    22,    59,    34,    -1,    42,    62,
      59,    34,    -1,    34,    -1,    60,    -1,    61,    -1,    60,
      59,    -1,    61,    59,    -1,    33,    -1,    40,    63,    40,
      -1,    37,    -1,     3,    62,    -1,    35,    62,    -1,    17,
      62,    -1,    32,    -1,    24,     8,    65,    13,    65,     8,
      -1,    24,     8,    65,     8,    -1,    39,    -1,     6,    -1,
      14,    -1,    14,     8,    65,     8,    -1,    46,    62,    -1,
      27,    -1,    30,    -1,     9,    62,    -1,    44,    62,    -1,
      18,    65,    -1,    19,    -1,    38,    65,    38,    -1,    38,
      38,    -1,     8,    65,     8,    -1,    61,    -1,    64,    61,
      -1,    64,    36,    -1,    36,    -1,    64,    41,    -1,    41,
      -1,    16,    -1,    16,    65,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const unsigned short yyrline[] =
{
       0,   100,   100,   106,   115,   116,   120,   124,   128,   132,
     136,   140,   144,   148,   152,   156,   160,   164,   168,   172,
     176,   180,   184,   188,   192,   196,   201,   205,   209,   213,
     214,   218,   222,   229,   236,   247,   253,   261,   265,   272,
     276,   280,   287,   291,   295,   299,   306,   310,   314,   318,
     330,   342,   346,   350,   362,   367,   371,   375,   379,   383,
     387,   391,   395,   400,   405,   419,   426,   431,   438,   445,
     450,   455,   459,   466,   470,   477,   481
};
#endif

#if YYDEBUG || YYERROR_VERBOSE
/* YYTNME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals. */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "TOK_ANY", "TOK_ANSI", "TOK_AUTHOR", 
  "TOK_BEEP", "TOK_BITMAP", "TOK_BRKT", "TOK_CALL", "TOK_CAPSFREE", 
  "TOK_CAPSOFF", "TOK_CAPSON", "TOK_COMMA", "TOK_CONTEXT", 
  "TOK_COPYRIGHT", "TOK_CHAR", "TOK_DEADKEY", "TOK_DOLLAR", "TOK_ERROR", 
  "TOK_ETHNOLOGUE", "TOK_GROUP", "TOK_GT", "TOK_HOTKEY", "TOK_INDEX", 
  "TOK_LANGUAGE", "TOK_LAYOUT", "TOK_MATCH", "TOK_MESSAGE", 
  "TOK_MNEMONIC", "TOK_NOMATCH", "TOK_NAME", "TOK_NUL", "TOK_NUMBER", 
  "TOK_NL", "TOK_OUTS", "TOK_RAWKEY", "TOK_PLUS", "TOK_QM", "TOK_RTN", 
  "TOK_SB", "TOK_SHIFT", "TOK_STORE", "TOK_STOREINSTORE", "TOK_SWITCH", 
  "TOK_UNICODE", "TOK_USE", "TOK_USINGKEYS", "TOK_UTF", "TOK_VERSION", 
  "$accept", "T_FILE", "T_HEADER", "T_HEADLINE", "T_GROUPS", "T_GROUP", 
  "T_GHEADER", "T_RULES", "T_RULELINE", "T_ITEMS", "T_ITEM", "T_STRING", 
  "T_PARAMETER", "T_KEYDEF", "T_KEYMODS", "T_BYTES", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const unsigned short yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const unsigned char yyr1[] =
{
       0,    50,    51,    51,    52,    52,    53,    53,    53,    53,
      53,    53,    53,    53,    53,    53,    53,    53,    53,    53,
      53,    53,    53,    53,    53,    53,    53,    53,    53,    53,
      53,    54,    54,    55,    55,    56,    56,    57,    57,    58,
      58,    58,    59,    59,    59,    59,    60,    60,    60,    60,
      60,    60,    60,    60,    60,    60,    60,    60,    60,    60,
      60,    60,    60,    60,    60,    60,    61,    61,    62,    63,
      63,    63,    63,    64,    64,    65,    65
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const unsigned char yyr2[] =
{
       0,     2,     2,     1,     1,     2,     3,     3,     5,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     2,
       2,     2,     4,     5,     5,     7,     3,     3,     3,     3,
       1,     1,     2,     1,     2,     3,     4,     1,     2,     4,
       4,     1,     1,     1,     2,     2,     1,     3,     1,     2,
       2,     2,     1,     6,     4,     1,     1,     1,     4,     2,
       1,     1,     2,     2,     2,     1,     3,     2,     3,     1,
       2,     2,     1,     2,     1,     1,     2
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const unsigned char yydefact[] =
{
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    30,     0,     0,
       0,     0,     0,     4,     3,    31,    33,     0,     0,     0,
      75,     0,     0,    21,    19,    20,     0,     0,     0,     0,
       0,     0,     0,     0,    56,     0,    57,     0,     0,    65,
       0,    60,    61,    52,    46,     0,    48,    55,     0,     0,
       0,     0,    42,    43,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     1,     2,     5,    32,    41,     0,
      34,    37,     0,     0,    67,     0,    26,    76,    13,    12,
      14,    29,    28,     0,    35,     0,    72,    74,    69,     0,
       0,     9,    49,    62,     0,    51,    64,     0,    50,     0,
      63,    59,    16,    44,    45,    17,    18,    15,    27,     7,
       6,     0,     0,    11,    10,     0,    38,     0,     0,    66,
      68,    36,     0,    71,    73,    70,     0,     0,    47,    22,
       0,     0,     0,    23,     0,     8,    58,    54,     0,    24,
      40,    39,     0,     0,    25,    53
};

/* YYDEFGOTO[NTERM-NUM]. */
static const yysigned_char yydefgoto[] =
{
      -1,    21,    22,    23,    24,    25,    26,    80,    81,    82,
      62,    63,    40,    99,   100,    32
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -31
static const short yypact[] =
{
     118,    -1,   -25,   -13,   -11,    -7,     5,   -25,     8,   -25,
      28,    -5,   217,   -13,   -25,   -25,   -13,   -31,    28,    19,
     -13,    44,    24,   146,   -31,    24,   175,     0,    -9,    13,
       8,    14,    15,   -31,   -31,   -31,    16,    17,    20,     8,
     -30,   -27,    21,    28,   -31,    28,    50,    28,     8,   -31,
      51,   -31,   -31,   -31,   -31,    28,   -31,   -31,   -27,    28,
      28,    30,   217,   217,    37,    38,    39,    41,    42,    43,
     217,    32,    45,    46,   -31,   -31,   -31,   -31,   -31,    28,
     -31,   175,    59,    28,   -31,    47,   -31,   -31,   -31,   -31,
     -31,   -31,   -31,    74,   -31,    49,   -31,   -31,   -31,    48,
      -4,   -31,   -31,   -31,     8,   -31,   -31,     8,   -31,    52,
     -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,
     -31,    53,    28,   -31,   -31,   217,   -31,   217,   -28,   -31,
     -31,   -31,    55,   -31,   -31,   -31,    76,     2,   -31,   -31,
      56,    57,    60,   -31,    28,   -31,   -31,   -31,     8,   -31,
     -31,   -31,    63,    78,   -31,   -31
};

/* YYPGOTO[NTERM-NUM].  */
static const yysigned_char yypgoto[] =
{
     -31,   -31,    70,   -31,    -6,   -31,   -31,    22,   -31,   -10,
     -31,    54,   -17,    40,   -31,    -8
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const unsigned char yytable[] =
{
      37,    70,    61,    30,    94,    65,   143,    30,    69,    96,
     147,    28,    73,    28,    97,   148,    75,    95,   144,    77,
      85,    27,    87,    33,    30,    28,   102,    34,   103,    84,
     105,    93,   133,    28,    28,    41,    39,   134,   108,    35,
     106,    71,   110,   111,    74,    10,    83,    86,    88,    89,
      90,    91,   113,   114,    92,   101,    29,    31,   104,   107,
     121,    36,   125,    38,   112,    42,   128,    64,    66,    67,
      68,   115,   116,   117,    72,   118,   119,   120,   122,   123,
     124,   127,   130,   131,   146,   129,   155,   139,   132,   145,
     149,   150,   138,    76,   151,    98,   136,   154,   109,   137,
       0,     0,     0,   126,     0,   140,     0,     0,     0,     0,
       0,     0,    98,     0,     0,   141,     0,   142,     0,     0,
       0,     0,     1,     2,     0,     3,     0,   152,     4,     5,
       6,     0,     0,     7,     0,     0,     0,     8,     9,    10,
     153,    11,     0,    12,    13,     0,    14,    15,     0,    16,
       1,     2,    17,     3,   135,     0,     4,     5,     6,     0,
      18,     7,     0,    19,     0,     8,     9,    20,     0,    11,
       0,    12,    13,     0,    14,    15,     0,    16,    43,     0,
      17,    44,     0,     0,    45,     0,     0,     0,    18,    46,
       0,    19,    47,    48,    49,    20,     0,     0,     0,    50,
       0,     0,    51,     0,     0,    52,     0,    53,    54,    78,
      55,     0,    56,    28,    57,    58,     0,    79,     0,    59,
      43,    60,     0,    44,     0,     0,    45,     0,     0,     0,
       0,    46,     0,     0,    47,    48,    49,     0,     0,     0,
       0,    50,     0,     0,    51,     0,     0,    52,     0,    53,
      54,     0,    55,     0,    56,    28,    57,    58,     0,     0,
       0,    59,     0,    60
};

static const short yycheck[] =
{
       8,    18,    12,    16,    34,    13,    34,    16,    16,    36,
       8,    38,    20,    38,    41,    13,    22,    47,    46,    25,
      28,    22,    30,    34,    16,    38,    43,    34,    45,    38,
      47,    39,    36,    38,    38,    40,     8,    41,    55,    34,
      48,    22,    59,    60,     0,    21,    46,    34,    34,    34,
      34,    34,    62,    63,    34,    34,     2,     3,     8,     8,
      70,     7,    79,     9,    34,    11,    83,    13,    14,    15,
      16,    34,    34,    34,    20,    34,    34,    34,    46,    34,
      34,    22,     8,    34,     8,    38,     8,    34,    40,    34,
      34,    34,    40,    23,    34,    41,   104,    34,    58,   107,
      -1,    -1,    -1,    81,    -1,   122,    -1,    -1,    -1,    -1,
      -1,    -1,    58,    -1,    -1,   125,    -1,   127,    -1,    -1,
      -1,    -1,     4,     5,    -1,     7,    -1,   144,    10,    11,
      12,    -1,    -1,    15,    -1,    -1,    -1,    19,    20,    21,
     148,    23,    -1,    25,    26,    -1,    28,    29,    -1,    31,
       4,     5,    34,     7,   100,    -1,    10,    11,    12,    -1,
      42,    15,    -1,    45,    -1,    19,    20,    49,    -1,    23,
      -1,    25,    26,    -1,    28,    29,    -1,    31,     3,    -1,
      34,     6,    -1,    -1,     9,    -1,    -1,    -1,    42,    14,
      -1,    45,    17,    18,    19,    49,    -1,    -1,    -1,    24,
      -1,    -1,    27,    -1,    -1,    30,    -1,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,    -1,    42,    -1,    44,
       3,    46,    -1,     6,    -1,    -1,     9,    -1,    -1,    -1,
      -1,    14,    -1,    -1,    17,    18,    19,    -1,    -1,    -1,
      -1,    24,    -1,    -1,    27,    -1,    -1,    30,    -1,    32,
      33,    -1,    35,    -1,    37,    38,    39,    40,    -1,    -1,
      -1,    44,    -1,    46
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const unsigned char yystos[] =
{
       0,     4,     5,     7,    10,    11,    12,    15,    19,    20,
      21,    23,    25,    26,    28,    29,    31,    34,    42,    45,
      49,    51,    52,    53,    54,    55,    56,    22,    38,    61,
      16,    61,    65,    34,    34,    34,    61,    65,    61,     8,
      62,    40,    61,     3,     6,     9,    14,    17,    18,    19,
      24,    27,    30,    32,    33,    35,    37,    39,    40,    44,
      46,    59,    60,    61,    61,    65,    61,    61,    61,    65,
      62,    22,    61,    65,     0,    54,    52,    54,    34,    42,
      57,    58,    59,    46,    38,    65,    34,    65,    34,    34,
      34,    34,    34,    65,    34,    47,    36,    41,    61,    63,
      64,    34,    62,    62,     8,    62,    65,     8,    62,    63,
      62,    62,    34,    59,    59,    34,    34,    34,    34,    34,
      34,    59,    46,    34,    34,    62,    57,    22,    62,    38,
       8,    34,    40,    36,    41,    61,    65,    65,    40,    34,
      62,    59,    59,    34,    46,    34,     8,     8,    13,    34,
      34,    34,    62,    65,    34,     8
};

#if ! defined (YYSIZE_T) && defined (__SIZE_TYPE__)
# define YYSIZE_T __SIZE_TYPE__
#endif
#if ! defined (YYSIZE_T) && defined (size_t)
# define YYSIZE_T size_t
#endif
#if ! defined (YYSIZE_T)
# if defined (__STDC__) || defined (__cplusplus)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# endif
#endif
#if ! defined (YYSIZE_T)
# define YYSIZE_T unsigned int
#endif

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrlab1

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
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { 								\
      yyerror ("syntax error: cannot back up");\
      YYERROR;							\
    }								\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

/* YYLLOC_DEFAULT -- Compute the default location (before the actions
   are run).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)         \
  Current.first_line   = Rhs[1].first_line;      \
  Current.first_column = Rhs[1].first_column;    \
  Current.last_line    = Rhs[N].last_line;       \
  Current.last_column  = Rhs[N].last_column;
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
} while (0)

# define YYDSYMPRINT(Args)			\
do {						\
  if (yydebug)					\
    yysymprint Args;				\
} while (0)

# define YYDSYMPRINTF(Title, Token, Value, Location)		\
do {								\
  if (yydebug)							\
    {								\
      YYFPRINTF (stderr, "%s ", Title);				\
      yysymprint (stderr, 					\
                  Token, Value);	\
      YYFPRINTF (stderr, "\n");					\
    }								\
} while (0)

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (cinluded).                                                   |
`------------------------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_stack_print (short *bottom, short *top)
#else
static void
yy_stack_print (bottom, top)
    short *bottom;
    short *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (/* Nothing. */; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_reduce_print (int yyrule)
#else
static void
yy_reduce_print (yyrule)
    int yyrule;
#endif
{
  int yyi;
  unsigned int yylineno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %u), ",
             yyrule - 1, yylineno);
  /* Print the symbols being reduced, and their result.  */
  for (yyi = yyprhs[yyrule]; 0 <= yyrhs[yyi]; yyi++)
    YYFPRINTF (stderr, "%s ", yytname [yyrhs[yyi]]);
  YYFPRINTF (stderr, "-> %s\n", yytname [yyr1[yyrule]]);
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (Rule);		\
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YYDSYMPRINT(Args)
# define YYDSYMPRINTF(Title, Token, Value, Location)
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
   SIZE_MAX < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#if YYMAXDEPTH == 0
# undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined (__GLIBC__) && defined (_STRING_H)
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
#   if defined (__STDC__) || defined (__cplusplus)
yystrlen (const char *yystr)
#   else
yystrlen (yystr)
     const char *yystr;
#   endif
{
  register const char *yys = yystr;

  while (*yys++ != '\0')
    continue;

  return yys - yystr - 1;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined (__GLIBC__) && defined (_STRING_H) && defined (_GNU_SOURCE)
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
#   if defined (__STDC__) || defined (__cplusplus)
yystpcpy (char *yydest, const char *yysrc)
#   else
yystpcpy (yydest, yysrc)
     char *yydest;
     const char *yysrc;
#   endif
{
  register char *yyd = yydest;
  register const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

#endif /* !YYERROR_VERBOSE */



#if YYDEBUG
/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yysymprint (FILE *yyoutput, int yytype, YYSTYPE *yyvaluep)
#else
static void
yysymprint (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  if (yytype < YYNTOKENS)
    {
      YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
# ifdef YYPRINT
      YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
    }
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  switch (yytype)
    {
      default:
        break;
    }
  YYFPRINTF (yyoutput, ")");
}

#endif /* ! YYDEBUG */
/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yydestruct (int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yytype, yyvaluep)
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  switch (yytype)
    {

      default:
        break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM);
# else
int yyparse ();
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
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



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM)
# else
int yyparse (YYPARSE_PARAM)
  void *YYPARSE_PARAM;
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  register int yystate;
  register int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  short	yyssa[YYINITDEPTH];
  short *yyss = yyssa;
  register short *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  register YYSTYPE *yyvsp;



#define YYPOPSTACK   (yyvsp--, yyssp--)

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* When reducing, the number of symbols on the RHS of the reduced
     rule.  */
  int yylen;

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
     have just been pushed. so pushing a state here evens the stacks.
     */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack. Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	short *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyoverflowlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyoverflowlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	short *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyoverflowlab;
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

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

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
      YYDSYMPRINTF ("Next token is", yytoken, &yylval, &yylloc);
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

  /* Shift the lookahead token.  */
  YYDPRINTF ((stderr, "Shifting token %s, ", yytname[yytoken]));

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;


  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  yystate = yyn;
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
#line 101 "yacc.y"
    {
		kbp->groups = yyvsp[0].group;
		kbp->ngroups = count_groups(yyvsp[0].group);
		kbp->nstores = count_stores(kbp->stores);
	}
    break;

  case 3:
#line 107 "yacc.y"
    {
		kbp->groups = yyvsp[0].group;
		kbp->ngroups = count_groups(yyvsp[0].group);
		kbp->nstores = count_stores(kbp->stores);
	}
    break;

  case 6:
#line 121 "yacc.y"
    {
		new_store_from_string("&name",yyvsp[-1].string,lineno);
	}
    break;

  case 7:
#line 125 "yacc.y"
    {
		new_store_from_string("&name",yyvsp[-1].string,lineno);
	}
    break;

  case 8:
#line 129 "yacc.y"
    {
		new_store("&hotkey",new_list(yyvsp[-2].number),lineno);
	}
    break;

  case 9:
#line 133 "yacc.y"
    {
		new_store_from_string("&hotkey",yyvsp[-1].string,lineno);
	}
    break;

  case 10:
#line 137 "yacc.y"
    {
		new_store_from_string("&version",yyvsp[-1].string,lineno);
	}
    break;

  case 11:
#line 141 "yacc.y"
    {
		new_store_from_string("&version",yyvsp[-1].string,lineno);
	}
    break;

  case 12:
#line 145 "yacc.y"
    { 
		new_store_from_string("&bitmap",yyvsp[-1].string,lineno);
	}
    break;

  case 13:
#line 149 "yacc.y"
    { 
		new_store_from_string("&bitmap",yyvsp[-1].string,lineno);
	}
    break;

  case 14:
#line 153 "yacc.y"
    { 
		new_store_from_string("&copyright",yyvsp[-1].string,lineno);	
	}
    break;

  case 15:
#line 157 "yacc.y"
    { 
		new_store_from_string("&message",yyvsp[-1].string,lineno);
	}
    break;

  case 16:
#line 161 "yacc.y"
    { 
		new_store_from_string("&language",yyvsp[-1].items,lineno);
	}
    break;

  case 17:
#line 165 "yacc.y"
    { 
		new_store_from_string("&layout",yyvsp[-1].string,lineno);
	}
    break;

  case 18:
#line 169 "yacc.y"
    { 
		new_store_from_string("&layout",yyvsp[-1].string,lineno);
	}
    break;

  case 19:
#line 173 "yacc.y"
    { 
		new_store_from_string("&capsalwaysoff","1",lineno);
	}
    break;

  case 20:
#line 177 "yacc.y"
    { 
		new_store_from_string("&capsononly","1",lineno);
	}
    break;

  case 21:
#line 181 "yacc.y"
    { 
		new_store_from_string("&shiftfreescaps","1",lineno);
	}
    break;

  case 22:
#line 185 "yacc.y"
    {
		new_store(yyvsp[-2].string,yyvsp[-1].items,lineno);
	}
    break;

  case 23:
#line 189 "yacc.y"
    {
		set_start_group(yyvsp[-1].string,KF_ANSI, lineno);
	}
    break;

  case 24:
#line 193 "yacc.y"
    {
		set_start_group(yyvsp[-1].string,KF_UNICODE, lineno);
	}
    break;

  case 25:
#line 197 "yacc.y"
    {
		kmflcomp_error(lineno,"alternate starting groups not supported");
		fail(11,"obsolete syntax");
	}
    break;

  case 26:
#line 202 "yacc.y"
    { 
		new_store_from_string("&author",yyvsp[-1].string,lineno);
	}
    break;

  case 27:
#line 206 "yacc.y"
    { 
		new_store_from_string("&mnemoniclayout",yyvsp[-1].string,lineno);
	}
    break;

  case 28:
#line 210 "yacc.y"
    { 
		new_store_from_string("&ethnologuecode",yyvsp[-1].string,lineno);
	}
    break;

  case 31:
#line 219 "yacc.y"
    {
		yyval.group = kbp->groups;
	}
    break;

  case 32:
#line 223 "yacc.y"
    {
		yyval.group = kbp->groups;
	}
    break;

  case 33:
#line 230 "yacc.y"
    {
		yyval.group = yyvsp[0].group;
		(yyval.group)->rules = NULL;
		(yyval.group)->nrules = 0;
		kmflcomp_warn(0,"group(%s) is empty!",(yyvsp[0].group)->name);
	}
    break;

  case 34:
#line 237 "yacc.y"
    {
		yyval.group = yyvsp[-1].group;
		(yyval.group)->rules = yyvsp[0].rule;
		(yyval.group)->nrules = count_rules(yyvsp[0].rule);
		if((yyval.group)->nrules == 0) 
			kmflcomp_warn(0,"group(%s) is empty!",(yyvsp[-1].group)->name); 
	}
    break;

  case 35:
#line 248 "yacc.y"
    {
		yyval.group = gp = new_group(yyvsp[-1].string, lineno);
		if(gp) gp->flags = 0;
	}
    break;

  case 36:
#line 254 "yacc.y"
    {
		yyval.group = gp = new_group(yyvsp[-2].string, lineno);
		if(gp) gp->flags = GF_USEKEYS;
	}
    break;

  case 37:
#line 262 "yacc.y"
    {
		yyval.rule = yyvsp[0].rule;
	}
    break;

  case 38:
#line 266 "yacc.y"
    {
		yyval.rule = add_rule(yyvsp[-1].rule, yyvsp[0].rule);
	}
    break;

  case 39:
#line 273 "yacc.y"
    {
		yyval.rule = new_rule(gp, yyvsp[-3].items, yyvsp[-1].items, lineno);
	}
    break;

  case 40:
#line 277 "yacc.y"
    {
		new_store(yyvsp[-2].string,yyvsp[-1].items,lineno); yyval.rule = NULL;
	}
    break;

  case 41:
#line 281 "yacc.y"
    {
		yyval.rule = NULL;
	}
    break;

  case 42:
#line 288 "yacc.y"
    {
		yyval.items = new_list(yyvsp[0].number);
	}
    break;

  case 43:
#line 292 "yacc.y"
    {
		yyval.items = items_from_string(yyvsp[0].string,lineno);
	}
    break;

  case 44:
#line 296 "yacc.y"
    {
		yyval.items = add_item_to_list(yyvsp[0].items,yyvsp[-1].number);
	}
    break;

  case 45:
#line 300 "yacc.y"
    {
		yyval.items = add_lists(yyvsp[0].items,items_from_string(yyvsp[-1].string,lineno));
	}
    break;

  case 46:
#line 307 "yacc.y"
    {
		yyval.number = MAKE_ITEM(ITEM_CHAR,yyvsp[0].number);
	}
    break;

  case 47:
#line 311 "yacc.y"
    {
		yyval.number = MAKE_ITEM(ITEM_KEYSYM,yyvsp[-1].number);
	}
    break;

  case 48:
#line 315 "yacc.y"
    {
		yyval.number = MAKE_ITEM(ITEM_PLUS,0);	/* include in item list - remove later when testing validity */
	}
    break;

  case 49:
#line 319 "yacc.y"
    {
		if((n=store_number(yyvsp[0].string,lineno)) != UNDEFINED)
		{
			yyval.number = MAKE_ITEM(ITEM_ANY,n);
		}
		else
		{
			kmflcomp_warn(lineno,"store (%s) is undefined!",yyvsp[0].string);
			yyval.number = 0;
		}
	}
    break;

  case 50:
#line 331 "yacc.y"
    {
		if((n=store_number(yyvsp[0].string,lineno)) != UNDEFINED)
		{
			yyval.number = MAKE_ITEM(ITEM_OUTS,n);
		}
		else
		{
			kmflcomp_warn(lineno,"store (%s) is undefined!",yyvsp[0].string);
			yyval.number = 0;
		}
	}
    break;

  case 51:
#line 343 "yacc.y"
    {
		yyval.number = MAKE_ITEM(ITEM_DEADKEY,deadkey_number(yyvsp[0].string, lineno));
	}
    break;

  case 52:
#line 347 "yacc.y"
    {
		yyval.number = MAKE_ITEM(ITEM_NUL,0);
	}
    break;

  case 53:
#line 351 "yacc.y"
    {
		if((n=store_number(yyvsp[-3].string,lineno)) != UNDEFINED)
		{
			yyval.number = MAKE_PARAMETER_ITEM(ITEM_INDEX,atoi(yyvsp[-1].string),n);
		}
		else
		{
			kmflcomp_warn(lineno,"store (%s) is undefined!",yyvsp[-3].string);
			yyval.number = 0;
		}
	}
    break;

  case 54:
#line 363 "yacc.y"
    {
		kmflcomp_warn(lineno,"index(%s) must have TWO parameters!",yyvsp[-1].string);
		yyval.number = 0;
	}
    break;

  case 55:
#line 368 "yacc.y"
    {
		yyval.number = MAKE_ITEM(ITEM_RETURN,0);
	}
    break;

  case 56:
#line 372 "yacc.y"
    {
		yyval.number = MAKE_ITEM(ITEM_BEEP,0);
	}
    break;

  case 57:
#line 376 "yacc.y"
    {
		yyval.number = MAKE_ITEM(ITEM_CONTEXT,0);
	}
    break;

  case 58:
#line 380 "yacc.y"
    {
		yyval.number = MAKE_ITEM(ITEM_CONTEXT,atoi(yyvsp[-1].string));
	}
    break;

  case 59:
#line 384 "yacc.y"
    {
		yyval.number = MAKE_ITEM(ITEM_USE,group_number(yyvsp[0].string, lineno));
	}
    break;

  case 60:
#line 388 "yacc.y"
    {
		yyval.number = MAKE_ITEM(ITEM_MATCH,0);
	}
    break;

  case 61:
#line 392 "yacc.y"
    {
		yyval.number = MAKE_ITEM(ITEM_NOMATCH,0);
	}
    break;

  case 62:
#line 396 "yacc.y"
    {
		kmflcomp_error(lineno,"call keyword not implemented");
		fail(12,"unsupported keyword");
	}
    break;

  case 63:
#line 401 "yacc.y"
    {
		kmflcomp_error(lineno,"switch keyword not implemented");
		fail(11,"obsolete syntax");
	}
    break;

  case 64:
#line 406 "yacc.y"
    {
		STORE *sp;		/* check for named constants */
		sp = find_store(yyvsp[0].string);
		if(sp)
		{
			yyval.number = *sp->items;
		}
		else
		{
			yyval.number = 0;
			kmflcomp_error(lineno,"undefined constant");
		}
	}
    break;

  case 65:
#line 420 "yacc.y"
    {
		kmflcomp_error(lineno,"illegal or unrecognized item in rule or store");
	}
    break;

  case 66:
#line 427 "yacc.y"
    { 	
		yyval.string = yyvsp[-1].string;
	}
    break;

  case 67:
#line 432 "yacc.y"
    {
		yyval.string = new_string(0); /* allow for empty strings */
	}
    break;

  case 68:
#line 439 "yacc.y"
    {
		yyval.string = yyvsp[-1].string;
	}
    break;

  case 69:
#line 446 "yacc.y"
    {	
		yyval.number = make_keysym(0,string_to_keysym(yyvsp[0].string,lineno));
	}
    break;

  case 70:
#line 451 "yacc.y"
    {	
		yyval.number = make_keysym(0,string_to_keysym(yyvsp[0].string,lineno));
	}
    break;

  case 71:
#line 456 "yacc.y"
    {
		yyval.number = make_keysym(yyvsp[-1].number,yyvsp[0].number);
	}
    break;

  case 72:
#line 460 "yacc.y"
    {
		yyval.number = make_keysym(0,yyvsp[0].number);
	}
    break;

  case 73:
#line 467 "yacc.y"
    {
		yyval.number = yyvsp[-1].number | yyvsp[0].number;
	}
    break;

  case 74:
#line 471 "yacc.y"
    {
		yyval.number = yyvsp[0].number;
	}
    break;

  case 75:
#line 478 "yacc.y"
    {	
		yyval.string = new_string(yyvsp[0].number);
	}
    break;

  case 76:
#line 482 "yacc.y"
    {
		yyval.string = add_char(yyvsp[0].string,yyvsp[-1].number);
	}
    break;


    }

/* Line 991 of yacc.c.  */
#line 1797 "yacc.c"

  yyvsp -= yylen;
  yyssp -= yylen;


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
#if YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (YYPACT_NINF < yyn && yyn < YYLAST)
	{
	  YYSIZE_T yysize = 0;
	  int yytype = YYTRANSLATE (yychar);
	  char *yymsg;
	  int yyx, yycount;

	  yycount = 0;
	  /* Start YYX at -YYN if negative to avoid negative indexes in
	     YYCHECK.  */
	  for (yyx = yyn < 0 ? -yyn : 0;
	       yyx < (int) (sizeof (yytname) / sizeof (char *)); yyx++)
	    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	      yysize += yystrlen (yytname[yyx]) + 15, yycount++;
	  yysize += yystrlen ("syntax error, unexpected ") + 1;
	  yysize += yystrlen (yytname[yytype]);
	  yymsg = (char *) YYSTACK_ALLOC (yysize);
	  if (yymsg != 0)
	    {
	      char *yyp = yystpcpy (yymsg, "syntax error, unexpected ");
	      yyp = yystpcpy (yyp, yytname[yytype]);

	      if (yycount < 5)
		{
		  yycount = 0;
		  for (yyx = yyn < 0 ? -yyn : 0;
		       yyx < (int) (sizeof (yytname) / sizeof (char *));
		       yyx++)
		    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
		      {
			const char *yyq = ! yycount ? ", expecting " : " or ";
			yyp = yystpcpy (yyp, yyq);
			yyp = yystpcpy (yyp, yytname[yyx]);
			yycount++;
		      }
		}
	      yyerror (yymsg);
	      YYSTACK_FREE (yymsg);
	    }
	  else
	    yyerror ("syntax error; also virtual memory exhausted");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror ("syntax error");
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      /* Return failure if at end of input.  */
      if (yychar == YYEOF)
        {
	  /* Pop the error token.  */
          YYPOPSTACK;
	  /* Pop the rest of the stack.  */
	  while (yyss < yyssp)
	    {
	      YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
	      yydestruct (yystos[*yyssp], yyvsp);
	      YYPOPSTACK;
	    }
	  YYABORT;
        }

      YYDSYMPRINTF ("Error: discarding", yytoken, &yylval, &yylloc);
      yydestruct (yytoken, &yylval);
      yychar = YYEMPTY;

    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab2;


/*----------------------------------------------------.
| yyerrlab1 -- error raised explicitly by an action.  |
`----------------------------------------------------*/
yyerrlab1:

  /* Suppress GCC warning that yyerrlab1 is unused when no action
     invokes YYERROR.  */
#if defined (__GNUC_MINOR__) && 2093 <= (__GNUC__ * 1000 + __GNUC_MINOR__) \
    && !defined __cplusplus
  __attribute__ ((__unused__))
#endif


  goto yyerrlab2;


/*---------------------------------------------------------------.
| yyerrlab2 -- pop states until the error token can be shifted.  |
`---------------------------------------------------------------*/
yyerrlab2:
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

      YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
      yydestruct (yystos[yystate], yyvsp);
      yyvsp--;
      yystate = *--yyssp;

      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  YYDPRINTF ((stderr, "Shifting error token, "));

  *++yyvsp = yylval;


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
/*----------------------------------------------.
| yyoverflowlab -- parser overflow comes here.  |
`----------------------------------------------*/
yyoverflowlab:
  yyerror ("parser stack overflow");
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  return yyresult;
}


#line 488 "yacc.y"


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

