#ifndef __DBUSTESTHELPER_H__
#define __DBUSTESTHELPER_H__

#include "config.h"
#if DBUS_IMPLEMENTATION == SYSTEMD
#include <systemd/sd-bus.h>
#else
#include <basu/sd-bus.h>
#endif
#include "testfixture.h"

#ifdef __cplusplus
extern "C" {
#endif

void dbus_testhelper_init(IBusKeymanTestsFixture* fixture);
void dbus_testhelper_close();

#ifdef __cplusplus
}
#endif

#endif // __DBUSTESTHELPER_H__
