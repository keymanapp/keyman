/* memman.h
 * Copyright (C) 2005 SIL International
 *
 * This file is part of KMFL Compiler library.
 *
 * KMFL Compiler library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * KMFL Compiler library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with KMFL Compiler library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 *
 */

void * mem_calloc(size_t n, size_t sz);
void * mem_alloc(size_t size);
void * mem_realloc(void * ptr, size_t size);
char * mem_strdup(char * str);
void mem_free(void * ptr);
void mem_free_all(void);
