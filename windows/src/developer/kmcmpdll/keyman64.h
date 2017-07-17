/*
  Name:             keyman64
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      22 Mar 2010

  Modified Date:    25 Oct 2016
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          22 Mar 2010 - mcdurdin - Compiler fixup - x64 support
                    25 Oct 2016 - mcdurdin - I5135 - Remove product and licensing references from Developer projects
*/
#ifndef _KEYMAN64_H
#define _KEYMAN64_H

#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0600
#endif

#ifndef STRICT
#define STRICT
#endif

#include <windows.h>
#include <assert.h>

#include "compiler.h"
#include "xstring.h"

#include "registry.h"
#include "unicode.h"
#include "rc4.h"
   // I5135
#include "crc32.h"

#endif
