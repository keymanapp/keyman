/* vim:set et sts=4: */

/*
 * Keyman Input Method for IBUS (The Input Bus)
 *
 * Copyright (C) 2018 SIL Global
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
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

#include <ibus.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include "engine.h"
#include "keyman-service.h"
#include "keymanutil.h"
#include "keymanutil_internal.h"

static IBusBus *bus         = NULL;
static IBusFactory *factory = NULL;

/* options */
static gboolean xml  = FALSE;
static gboolean ibus = FALSE;
gboolean testing     = FALSE;

static const GOptionEntry entries[] = {
    {"xml",    'x', 0, G_OPTION_ARG_NONE, &xml, "generate xml for engines", NULL},
    {"ibus",   'i', 0, G_OPTION_ARG_NONE, &ibus, "component is executed by ibus", NULL},
    {"testing", 0,  0, G_OPTION_ARG_NONE, &testing, "component is executed by integration testing", NULL},
    {NULL},
};

// Add an environment variable to see debug messages: export G_MESSAGES_DEBUG=all

static void
ibus_disconnected_cb(IBusBus *unused_bus, gpointer unused_data) {
  g_debug("bus disconnected");
  KeymanService *service = km_service_get_default(NULL);
  g_clear_object(&service);

  if (factory) g_object_unref(factory);
  if (bus) g_object_unref(bus);

  ibus_quit();
}

static void
add_single_keyboard(gpointer data, gpointer user_data) {
  IBusEngineDesc *engine = IBUS_ENGINE_DESC(data);
#if IBUS_CHECK_VERSION(1, 3, 99)
  const gchar *engine_name = ibus_engine_desc_get_name(engine);
#else
  const gchar *engine_name = engine->name;
#endif /* !IBUS_CHECK_VERSION(1,3,99) */
  if (engine_name) {
    ibus_factory_add_engine(factory, engine_name, IBUS_TYPE_KEYMAN_ENGINE);
  } else {
    g_error("%s: Trying to add NULL engine", __FUNCTION__);
  }
}

static void
add_keyboards(IBusBus *bus, gpointer user_data) {
  g_autolist(IBusEngineDesc) engines;
  g_autoptr(IBusComponent) component;

  g_message("Adding keyboards to ibus");

  component = ibus_keyman_get_component();

  GDBusConnection *connection = ibus_bus_get_connection(bus);
  factory = ibus_factory_new(g_object_ref(connection));

  g_signal_connect(bus, "disconnected", G_CALLBACK(ibus_disconnected_cb), NULL);

  engines = ibus_component_get_engines(component);
  g_list_foreach(engines, add_single_keyboard, NULL);

  if (ibus) {
    ibus_bus_request_name(bus, "org.freedesktop.IBus.Keyman", 0);
  } else {
    ibus_bus_register_component(bus, component);
  }

  km_service_get_default(NULL);  // initialise dbus service
}

static void
start_component(void) {
  g_message("Starting ibus-engine-keyman");

  ibus_init();

  bus = ibus_bus_new();

  if (ibus_bus_is_connected(bus)) {
    add_keyboards(bus, NULL);
  } else {
    g_message("Waiting for ibus-daemon to start up...");
    g_signal_connect(bus, "connected", G_CALLBACK(add_keyboards), NULL);
  }

  ibus_main();
}

static void
print_engines_xml(void) {
  g_autoptr(IBusComponent) component;
  g_autoptr(GString) output;

  ibus_init();

  component = ibus_keyman_get_component();
  output    = g_string_new("");

  ibus_component_output_engines(component, output, 0);

  fprintf(stdout, "%s", output->str);
}

int
main(gint argc, gchar **argv) {
  GError *error = NULL;
  GOptionContext *context;

  setlocale(LC_ALL, "");

  context = g_option_context_new("- ibus Keyman engine component");

  g_option_context_add_main_entries(context, entries, "ibus-keyman");

  if (!g_option_context_parse(context, &argc, &argv, &error)) {
    g_assert(error != NULL);
    g_print("Option parsing failed: %s\n", error->message);
    g_error_free(error);
    g_option_context_free(context);
    exit(-1);
  }

  if (xml) {
    print_engines_xml();
    g_option_context_free(context);
    exit(0);
  }

  start_component();

  g_option_context_free(context);
  g_message("Exiting ibus-engine-keyman");
  return 0;
}
