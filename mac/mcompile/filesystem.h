#pragma once

#include <stdio.h>
#include "u16.h"

// Open files on windows and non-windows platforms. Datatypes for Filename and mode must be the same.
// return FILE* if file could be opened; FILE must to be closed in calling function
FILE* Open_File(const KMX_CHAR* Filename, const KMX_CHAR* mode);
FILE* Open_File(const KMX_WCHART* Filename, const KMX_WCHART* mode);
FILE* Open_File(const KMX_WCHAR* Filename, const KMX_WCHAR* mode);
KMX_BOOL kmcmp_FileExists(const KMX_CHAR *filename);
KMX_BOOL kmcmp_FileExists(const KMX_WCHAR *filename);

//################################################################################################################################################
//################################# Code beyond these lines needs to be included in mcompile #####################################################
//################################################################################################################################################
