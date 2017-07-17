
#ifndef _UNICODE_H
#define _UNICODE_H

#define Uni_IsSurrogate1(ch) ((ch) >= 0xD800 && (ch) <= 0xDBFF)
#define Uni_IsSurrogate2(ch) ((ch) >= 0xDC00 && (ch) <= 0xDFFF)
#define Uni_IsSMP(ch) ((ch) >= 0x10000)

#define Uni_SurrogateToUTF32(ch, cl) (((ch) - 0xD800) * 0x400 + ((cl) - 0xDC00) + 0x10000)

#define Uni_UTF32ToSurrogate1(ch)	(((ch) - 0x10000) / 0x400 + 0xD800)
#define Uni_UTF32ToSurrogate2(ch)	(((ch) - 0x10000) % 0x400 + 0xDC00)


int wcssuppos(PWSTR p1, PWSTR p);		// return p-p1, surr pairs as one chr
int GetSuppChar(WCHAR *p);				// return *p, + *(++p) if surr pair)
int wcspos(PWSTR s, int pos);			// return pos, inc for s[xx] is surr.
PWSTR wcschrsupp(PWSTR buf, int x);
WCHAR ByteToWChar(char b);
unsigned char WCharToByte(WCHAR w);

#endif
