#ifndef __TESTFIXTURE_H__
#define __TESTFIXTURE_H__

#include <glib-object.h>
#include <glib.h>
#include <ibus.h>
#include <km_linux_common.h>
#include "ibusimcontext.h"

typedef struct {
  IBusBus* bus;
  GtkIMContext* context;
  IBusIMContext* ibuscontext;
} IBusKeymanTestsFixture;

#endif // __TESTFIXTURE_H__
