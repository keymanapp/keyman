/* vim:set et sts=4: */

/*
 * Keyman Input Method for IBUS (The Input Bus)
 *
 * Copyright (C) 2018-2023 SIL International
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

#include <gio/gio.h>

#include "engine.h"
#include "keyman-service.h"

struct _KeymanServicePrivate
{
    guint          owner_id;
    GDBusNodeInfo *ispec;
    IBusEngine    *engine;

    gchar *          name;
    gchar *          ldmlfile;
};

enum {
  PROP_0,
  PROP_NAME,
  PROP_LDMLFILE
};

static const gchar introspection_xml[] =
    "<node>"
    "  <interface name='com.Keyman'>"
    "    <property type='s' name='LDMLFile' access='read'/>"
    "    <property type='s' name='Name' access='read'/>"
    "    <method name='SendText'>"
    "        <arg type='s' name='text' direction='in' />"
    "    </method>"
    "  </interface>"
    "</node>";

static void km_service_bus_acquired (GDBusConnection *connection,
                                     const gchar     *name,
                                     gpointer         data);

G_DEFINE_TYPE_WITH_PRIVATE(KeymanService, km_service, G_TYPE_OBJECT)

static void
km_service_init (KeymanService *service)
{
    KeymanServicePrivate *priv;
    GError *error = NULL;

    g_message("WDG: km_service_init");

    service->priv = priv = (KeymanServicePrivate*)km_service_get_instance_private(service);

    priv->name = g_strdup ("None");
    priv->ldmlfile = g_strdup ("");
    priv->owner_id = g_bus_own_name (G_BUS_TYPE_SESSION,
                                     KEYMAN_DBUS_NAME,
                                     G_BUS_NAME_OWNER_FLAGS_NONE,
                                     km_service_bus_acquired,
                                     NULL, NULL,
                                     service, NULL);
    priv->ispec = g_dbus_node_info_new_for_xml (introspection_xml, &error);
    if (error)
    {
        g_warning ("%s\n", error->message);
        g_error_free (error);
    }
}

static void
km_service_set_property (GObject      *object,
                         guint         prop_id,
                         const GValue *value,
                         GParamSpec   *pspec)
{
    KeymanService *service = KM_SERVICE (object);

    switch (prop_id)
    {
        case PROP_NAME:
            if (service->priv->name)
            {
                g_free(service->priv->name);
            }
            service->priv->name = g_value_dup_string (value);
            break;
        case PROP_LDMLFILE:
            if (service->priv->ldmlfile)
            {
                g_free(service->priv->ldmlfile);
            }
            service->priv->ldmlfile = g_value_dup_string (value);
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
    }
}

static void
km_service_get_property (GObject    *object,
                         guint       prop_id,
                         GValue     *value,
                         GParamSpec *pspec)
{
    KeymanService *service = KM_SERVICE (object);

    switch (prop_id)
    {
        case PROP_NAME:
            g_value_set_string (value, service->priv->name);
            break;
        case PROP_LDMLFILE:
            g_value_set_string (value, service->priv->ldmlfile);
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
    }
}

static void
km_service_dispose (GObject *object)
{
    g_message("WDG: km_service_dispose");
    KeymanServicePrivate *priv = KM_SERVICE (object)->priv;

    if (priv->owner_id)
    {
        g_bus_unown_name (priv->owner_id);
        priv->owner_id = 0;
    }

    if (priv->ispec)
    {
        g_dbus_node_info_unref (priv->ispec);
        priv->ispec = NULL;
    }

    if (priv->name)
    {
        g_free(priv->name);
    }

    if (priv->ldmlfile)
    {
        g_free(priv->ldmlfile);
    }

    G_OBJECT_CLASS (km_service_parent_class)->dispose (object);
}

static void
km_service_class_init (KeymanServiceClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);

    object_class->get_property = km_service_get_property;
    object_class->set_property = km_service_set_property;
    object_class->dispose = km_service_dispose;

    g_object_class_install_property (object_class,
                                     PROP_NAME,
                                     g_param_spec_string ("name",
                                                          "Name",
                                                          "Name of the currently active Keyman keyboard",
                                                          "None",
                                                          G_PARAM_READWRITE |
                                                          G_PARAM_STATIC_STRINGS));
    g_object_class_install_property (object_class,
                                     PROP_LDMLFILE,
                                     g_param_spec_string ("ldmlfile",
                                                          "LDML file",
                                                          "On screen keyboard file for the currently active Keyman keyboard",
                                                          "",
                                                          G_PARAM_READWRITE |
                                                          G_PARAM_STATIC_STRINGS));
}

static void
handle_method_call(
    GDBusConnection *connection,
    const gchar *sender,
    const gchar *object_path,
    const gchar *interface_name,
    const gchar *method_name,
    GVariant *parameters,
    GDBusMethodInvocation *invocation,
    gpointer user_data)
{
    KeymanService *service = KM_SERVICE(user_data);

    gsize sz;
    if (g_strcmp0(method_name, "SendText") == 0) {
        GVariant *param = g_variant_get_child_value(parameters, 0);
        gchar *text     = g_variant_dup_string(param, &sz);

        km_service_send_text(service, text);
        g_dbus_method_invocation_return_value(invocation, NULL);

        g_free(text);
        g_variant_unref(param);
    }
}

static GVariant *
handle_get_property (GDBusConnection *connection,
                     const gchar     *sender,
                     const gchar     *path,
                     const gchar     *interface_name,
                     const gchar     *property,
                     GError         **error,
                     KeymanService       *service)
{
    GVariant *ret = NULL;

    if (g_strcmp0 (property, "Name") == 0)
    {
        ret = g_variant_new_string (service->priv->name);
    }
    else if (g_strcmp0 (property, "LDMLFile") == 0)
    {
        ret = g_variant_new_string (service->priv->ldmlfile);
    }
    return ret;
}

static gboolean
handle_set_property (GDBusConnection *connection,
                     const gchar     *sender,
                     const gchar     *path,
                     const gchar     *interface,
                     const gchar     *property,
                     GVariant        *value,
                     GError         **error,
                     KeymanService       *service)
{
    gsize sz;
    if (g_strcmp0 (property, "Name") == 0)
    {
        km_service_set_name (service, g_variant_dup_string (value, &sz));
    }
    else if (g_strcmp0 (property, "LDMLFile") == 0)
    {
        km_service_set_ldmlfile (service, g_variant_dup_string (value, &sz));
    }
    return TRUE;
}

static const GDBusInterfaceVTable interface_vtable =
{
    (GDBusInterfaceMethodCallFunc)handle_method_call,
    (GDBusInterfaceGetPropertyFunc)handle_get_property,
    (GDBusInterfaceSetPropertyFunc)handle_set_property
};

static void
emit_property_changed (GObject         *object,
                       GParamSpec      *pspec,
                       GDBusConnection *connection)
{
    KeymanService *service = KM_SERVICE (object);
    GError *error = NULL;
    GVariantBuilder builder, inv_builder;
    GVariant *prop_v;

    g_variant_builder_init (&builder, G_VARIANT_TYPE_ARRAY);
    g_variant_builder_init (&inv_builder, G_VARIANT_TYPE ("as"));

    if (g_strcmp0 (pspec->name, "name") == 0)
    {
        g_variant_builder_add (&builder, "{sv}", "Name",
                               g_variant_new_string (service->priv->name));
    }
    else if (g_strcmp0 (pspec->name, "ldmlfile") == 0)
    {
        g_variant_builder_add (&builder, "{sv}", "LDMLFile",
                               g_variant_new_string (service->priv->ldmlfile));
    }

    prop_v = g_variant_new ("(sa{sv}as)",
                            KEYMAN_DBUS_IFACE,
                            &builder, &inv_builder);

    if (!g_dbus_connection_emit_signal (connection, NULL,
                                        KEYMAN_DBUS_PATH,
                                        "org.freedesktop.DBus.Properties",
                                        "PropertiesChanged",
                                        prop_v, &error))
    {
        g_warning ("%s\n", error->message);
        g_error_free (error);
    }
}

static void
km_service_bus_acquired (GDBusConnection *connection,
                         const gchar     *name,
                         gpointer         data)
{
    KeymanService *service = data;
    if (service->priv->ispec) {
        GError *error = NULL;

        g_dbus_connection_register_object (connection,
                                           KEYMAN_DBUS_PATH,
                                           service->priv->ispec->interfaces[0],
                                           &interface_vtable,
                                           service, NULL, &error);
        if (error)
        {
            g_warning ("%s", error->message);
            g_error_free (error);
        }

        g_signal_connect (service, "notify",
                          G_CALLBACK (emit_property_changed), connection);
    }
}

KeymanService *
km_service_get_default(IBusEngine *engine)
{
    static KeymanService *service = NULL;

    // if (service)
    // {
    //     g_free(service);
    //     service = g_object_new (KM_TYPE_SERVICE, NULL);
    //     g_object_add_weak_pointer (G_OBJECT (service), (gpointer *) &service);
    // }
    if (!service)
    {
        service = g_object_new(KM_TYPE_SERVICE, NULL);
        g_object_add_weak_pointer (G_OBJECT (service), (gpointer *) &service);
    }

    KeymanServicePrivate *priv = KM_SERVICE(service)->priv;
    priv->engine = engine;

    return service;
}

void
km_service_set_name (KeymanService       *service,
                          const gchar *name)
{
    g_return_if_fail (KM_IS_SERVICE (service));

    if (g_strcmp0(name,service->priv->name) != 0)
    {
        g_free(service->priv->name);
        service->priv->name = g_strdup(name);
        g_object_notify (G_OBJECT (service), "name");
    }
}

// const gchar *
// km_service_get_name (KeymanService *service)
// {
//     g_return_val_if_fail (KM_IS_SERVICE (service), -1);

//     return service->priv->name;
// }

void
km_service_set_ldmlfile (KeymanService       *service,
                           const gchar *xml)
{
    g_return_if_fail (KM_IS_SERVICE (service));

    if (g_strcmp0(xml,service->priv->ldmlfile) != 0)
    {
        g_free(service->priv->ldmlfile);
        service->priv->ldmlfile = g_strdup(xml);
        g_object_notify (G_OBJECT (service), "ldmlfile");
    }
}

// const gchar *
// km_service_get_ldmlfile (KeymanService *service)
// {
//     g_return_val_if_fail (KM_IS_SERVICE (service), -1);

//     return service->priv->ldmlfile;
// }

void
km_service_send_text(KeymanService *service, const gchar *text) {
    g_return_if_fail(KM_IS_SERVICE(service));

    // engine will be NULL unless a keyman keyboard is active
    if (!service->priv->engine)
        return;

    ibus_keyman_set_text(service->priv->engine, text);
}
