/* memman.c
 * Copyright (C) 2005  SIL International
 *
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "memman.h"

// Memory Management
typedef struct memnod
{
    struct memnod   * mh_next;
    struct memnod * mh_prev;
} MEMHDR;

#define CLIENT_TO_HDR(a) ((MEMHDR *) (((char *) (a)) - sizeof(MEMHDR)))
#define HDR_TO_CLIENT(a) ((void *) (((char *) (a)) + sizeof(MEMHDR)))

static MEMHDR * memlist= NULL;

static void mem_list_add(MEMHDR * p)
{
    p->mh_next= memlist;
    p->mh_prev= NULL;

    if (memlist != NULL)
    {
        memlist->mh_prev= p;
    }
    memlist= p;

}

static void mem_list_delete(MEMHDR * p)
{
    if (p->mh_next != NULL)
        p->mh_next->mh_prev= p->mh_prev;

    if (p->mh_prev != NULL)
        p->mh_prev->mh_next= p->mh_next;
    else
        memlist= p->mh_next;
}

static int PointerInList(MEMHDR * ptr)
{
    MEMHDR * p;

    p= memlist;

    while (p != NULL)
    {
        if (p == ptr)
            return 1;

        p= p->mh_next;
    }

    return 0;
}

void * mem_calloc(size_t n, size_t sz)
{
    void * p;
    p = mem_alloc(n*sz);
    if (p)
        memset(p, 0, n*sz);
    return p;
}

void * mem_alloc(size_t size)
{
    MEMHDR * p;

    p= malloc(sizeof(MEMHDR) + size);

    if (p == NULL)
        return(NULL);

    mem_list_add(p);
    return HDR_TO_CLIENT(p);
}

void * mem_realloc(void * ptr, size_t size)
{
    MEMHDR *p;

    if (ptr == NULL)
        p= NULL;
    else
    {
        p= CLIENT_TO_HDR(ptr);

        mem_list_delete(p);
    }
    p= (MEMHDR *) realloc(p, sizeof(MEMHDR) + size);

    if (p == NULL)
        return(NULL);

    mem_list_add(p);

    return HDR_TO_CLIENT(p);
}

char * mem_strdup(char * str)
{
    char * s;

    s= mem_alloc(strlen(str) + 1);

    if (s != NULL)
        strcpy(s, str);

    return(s);
}

void mem_free(void * ptr)
{
    MEMHDR *p;

    p= CLIENT_TO_HDR(ptr);
    if (!PointerInList(p))
    {
        fprintf(stderr, "Error: freeing unallocated memory\n");
        return;
    }

    mem_list_delete(p);
    free(p);
}

void mem_free_all(void)
{
    MEMHDR *p;

    while (memlist != NULL)
    {
        p = memlist;
        mem_list_delete(memlist);
        free(p);
    }
}
