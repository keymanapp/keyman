/* utfconv.c
 * Copyright (C) 2006 SIL International
 *
 */

#include <iconv.h>
#include "kmflutfconv.h"


#define UTF8MODE "UTF-8"
#define UTF16MODE "UTF-16LE"
#define UTF32MODE "UTF-32LE"

size_t UTFConvert (
		char * sourceCode, char * targetCode,
		const void ** sourceStart, const void * sourceEnd, 
		void ** targetStart, void * targetEnd) 
{		
	size_t result = 0;
	char * source = *(char**)sourceStart;
	char * target = *(char**)targetStart;
	size_t inbytesleft = sourceEnd - *sourceStart;
	size_t outbytesleft = targetEnd - *targetStart;
	
	iconv_t ic;

	
	ic=iconv_open(targetCode, sourceCode);
	
	while (inbytesleft > 0 && result != (size_t) -1) {
		result=iconv(ic, &source, &inbytesleft, &target, &outbytesleft);
	}
	*sourceStart = source;
	*targetStart = target;
	
	iconv_close(ic);
	
	return result;
}

size_t IConvertUTF8toUTF16 (
		const UTF8** sourceStart, const UTF8* sourceEnd, 
		UTF16** targetStart, UTF16* targetEnd)
{
	return UTFConvert(UTF8MODE, UTF16MODE, (const void**)sourceStart, (const void *)sourceEnd, (void**)targetStart, (void *)targetEnd);
}

size_t IConvertUTF16toUTF8 (
		const UTF16** sourceStart, const UTF16* sourceEnd, 
		UTF8** targetStart, UTF8* targetEnd)
{
	return UTFConvert(UTF16MODE, UTF8MODE, (const void**)sourceStart, (const void *)sourceEnd, (void**)targetStart, (void *)targetEnd);
}
		
		
size_t IConvertUTF8toUTF32 (
		const UTF8** sourceStart, const UTF8* sourceEnd, 
		UTF32** targetStart, UTF32* targetEnd)
{
	return UTFConvert(UTF8MODE, UTF32MODE, (const void**)sourceStart, (const void *)sourceEnd, (void**)targetStart, (void *)targetEnd);
}

size_t IConvertUTF32toUTF8 (
		const UTF32** sourceStart, const UTF32* sourceEnd, 
		UTF8** targetStart, UTF8* targetEnd)
{
	return UTFConvert(UTF32MODE, UTF8MODE, (const void**)sourceStart, (const void *)sourceEnd, (void**)targetStart, (void *)targetEnd);
}
		
size_t IConvertUTF16toUTF32 (
		const UTF16** sourceStart, const UTF16* sourceEnd, 
		UTF32** targetStart, UTF32* targetEnd)
{
	return UTFConvert(UTF16MODE, UTF32MODE, (const void**)sourceStart, (const void *)sourceEnd, (void**)targetStart, (void *)targetEnd);
}

size_t IConvertUTF32toUTF16 (
		const UTF32** sourceStart, const UTF32* sourceEnd, 
		UTF16** targetStart, UTF16* targetEnd)
{
	return UTFConvert(UTF32MODE, UTF16MODE, (const void**)sourceStart, (const void *)sourceEnd, (void**)targetStart, (void *)targetEnd);
}
