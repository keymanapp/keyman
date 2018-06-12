/* memman.h
 * Copyright (C) 2005  SIL International
 *
 */

void * mem_calloc(size_t n, size_t sz);
void * mem_alloc(size_t size);
void * mem_realloc(void * ptr, size_t size);
char * mem_strdup(char * str);
void mem_free(void * ptr);
void mem_free_all(void);
