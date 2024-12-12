#ifndef __TESTFIXTURE_H__
#define __TESTFIXTURE_H__

#include <glib-object.h>
#include <glib.h>
#include <ibus.h>
#include "ibusimcontext.h"

#define KEYMAN_F24_KEYCODE_OUTPUT_SENTINEL 194  // 0xC2

typedef struct {
  IBusBus* bus;
  GtkIMContext* context;
  IBusIMContext* ibuscontext;
} IBusKeymanTestsFixture;

#endif // __TESTFIXTURE_H__
