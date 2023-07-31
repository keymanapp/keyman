#pragma once

#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0600
#endif

#ifndef STRICT
#define STRICT
#endif

#include <windows.h>
#include <assert.h>

#include "../../../common/windows/cpp/include/legacy_kmx_file.h"
#include "../../../common/windows/cpp/include/xstring.h"

#include "../../../common/windows/cpp/include/registry.h"
#include "../../../common/windows/cpp/include/unicode.h"
#include "../../../common/windows/cpp/include/crc32.h"

#include <ctype.h>
#include <stdio.h>

#include <string.h>
