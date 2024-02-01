/**
 * ICU modules used by Keyman Core
 */
#pragma once

#if !defined(HAVE_ICU4C)
#error icu4c is required for this code
#endif

#define U_FALLTHROUGH
#include "unicode/utypes.h"
#include "unicode/unistr.h"
#include "unicode/normalizer2.h"
