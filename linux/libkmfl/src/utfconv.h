/* utfconv.h
 * Copyright (C) 2006 SIL International
 *
 * This file is part of the KMFL library.
 *
 * The KMFL library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * The KMFL library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with the KMFL library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 *
 */

#if !defined(UTFCONV_H)
#define UTFCONV_H

#ifdef __uint32_t_defined
typedef uint32_t        UTF32;
typedef uint16_t        UTF16;
typedef uint8_t UTF8;
#else
typedef unsigned long   UTF32;
typedef unsigned short  UTF16;
typedef unsigned char   UTF8;
#endif

size_t IConvertUTF8toUTF16 (
		const UTF8** sourceStart, const UTF8* sourceEnd, 
		UTF16** targetStart, UTF16* targetEnd);

size_t IConvertUTF16toUTF8 (
		const UTF16** sourceStart, const UTF16* sourceEnd, 
		UTF8** targetStart, UTF8* targetEnd);
		
size_t IConvertUTF8toUTF32 (
		const UTF8** sourceStart, const UTF8* sourceEnd, 
		UTF32** targetStart, UTF32* targetEnd);

size_t IConvertUTF32toUTF8 (
		const UTF32** sourceStart, const UTF32* sourceEnd, 
		UTF8** targetStart, UTF8* targetEnd);
		
size_t IConvertUTF16toUTF32 (
		const UTF16** sourceStart, const UTF16* sourceEnd, 
		UTF32** targetStart, UTF32* targetEnd);

size_t IConvertUTF32toUTF16 (
		const UTF32** sourceStart, const UTF32* sourceEnd, 
		UTF16** targetStart, UTF16* targetEnd);
#endif
