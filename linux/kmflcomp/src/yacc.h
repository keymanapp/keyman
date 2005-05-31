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
/* Line 1249 of yacc.c.  */
#line 143 "yacc.h"
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;



