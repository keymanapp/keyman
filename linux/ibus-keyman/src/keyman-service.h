/* vim:set et sts=4: */

/*
 * Keyman Input Method for IBUS (The Input Bus)
 *
 * Copyright (C) 2018-2023 SIL Global
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 *
 */

 /*
    This file is based on work from Mousetweaks
    Copyright © 2007-2010 Gerd Kohlberger <gerdko gmail com>
*/

#ifndef __KM_SERVICE_H__
#define __KM_SERVICE_H__

#include <glib-object.h>
#include <ibus.h>

G_BEGIN_DECLS

#define KEYMAN_DBUS_NAME       "com.Keyman"
#define KEYMAN_DBUS_IFACE      "com.Keyman"
#define KEYMAN_DBUS_PATH       "/com/Keyman/IBus"

#define KM_TYPE_SERVICE  (km_service_get_type ())
#define KM_SERVICE(o)    (G_TYPE_CHECK_INSTANCE_CAST ((o), KM_TYPE_SERVICE, KeymanService))
#define KM_IS_SERVICE(o) (G_TYPE_CHECK_INSTANCE_TYPE ((o), KM_TYPE_SERVICE))

typedef GObjectClass             KeymanServiceClass;
typedef struct _KeymanService        KeymanService;
typedef struct _KeymanServicePrivate KeymanServicePrivate;

struct _KeymanService
{
    GObject           parent;
    KeymanServicePrivate *priv;
};

GType             km_service_get_type          (void) G_GNUC_CONST;

KeymanService *       km_service_get_default       (IBusEngine *keyman);

// const gchar *  km_service_get_ldmlfile(KeymanService *service);
void              km_service_set_ldmlfile(KeymanService *service,
                                          const gchar   *xml);

// const gchar *  km_service_get_name    (KeymanService *service);
void              km_service_set_name    (KeymanService *service,
                                          const gchar   *name);

void              km_service_send_text   (KeymanService *service,
                                          const gchar   *text);

G_END_DECLS

#endif /* __KM_SERVICE_H__ */
