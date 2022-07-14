//#pragma once

//#include "../../../Windows/src/engine/keyman32/keyman64.h"       // _S2 #include "keyman64.h"

//#include <ctype.h>
//#include <stdio.h>

//#include <string.h>
//#include <windows.h>       // Can be removed here :-)



#pragma once
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0600
#endif

#ifndef STRICT
#define STRICT
#endif

#include <windows.h>
#include <assert.h>

#include "kmcompx.h"
#include "kmx_file.h"
#include "kmx_u16.h"

#include "../../../common/windows/cpp/include/unicode.h"
#include "../../../common/windows/cpp/include/xstring.h"

#include "../../../common/windows/cpp/include/registry.h"
#include "../../../common/windows/cpp/include/crc32.h"

/*

#include "../../../core/src/kmx/kmx_processevent.h"       // _S2 included before all to prevent use of legacy_kmx_file

#include <ctype.h>
#include <stdio.h>

#include <string.h>
*/