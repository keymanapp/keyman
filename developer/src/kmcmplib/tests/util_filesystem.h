#pragma once

#include <stdio.h>
#include <km_u16.h>

// Opens files on windows and non-windows platforms. Datatypes for Filename and mode must be the same.
// returns FILE* if file could be opened; FILE needs to be closed in calling function
FILE* Open_File(const KMX_CHAR* Filename, const KMX_CHAR* mode);
FILE* Open_File(const KMX_WCHAR* Filename, const KMX_WCHAR* mode);
KMX_BOOL kmcmp_FileExists(const KMX_CHAR *filename);
KMX_BOOL kmcmp_FileExists(const KMX_WCHAR *filename);

bool IsRelativePath(KMX_CHAR const * p);
bool IsRelativePath(KMX_WCHAR const * p);
