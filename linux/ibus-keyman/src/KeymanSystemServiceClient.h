#ifndef __KEYMANSYSTEMSERVICECLIENT_H__
#define __KEYMANSYSTEMSERVICECLIENT_H__

#include <gio/gio.h>

#ifdef __cplusplus
extern "C" {
#endif

void set_capslock_indicator(guint32 capsLockState);
gint32 get_capslock_indicator();
void order_output(gboolean isKeyUp);

#ifdef __cplusplus
}
#endif

#endif // __KEYMANSYSTEMSERVICECLIENT_H__
