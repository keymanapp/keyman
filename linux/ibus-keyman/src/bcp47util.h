#ifndef __BCP47UTIL_H__
#define __BCP47UTIL_H__

int bcp47_minimize(const char* tag, char* minimizedTag, int capacity);
int bcp47_get_language_code(const char* tag, char* lang_code, int capacity);

#endif // __BCP47UTIL_H__
