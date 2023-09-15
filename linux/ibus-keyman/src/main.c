/* vim:set et sts=4: */

/*
 * Keyman Input Method for IBUS (The Input Bus)
 *
 * Copyright (C) 2018 SIL International
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

  g_object_unref(factory);
  g_object_unref(bus);

  ibus_quit();
}

static void
start_component(void) {
  GList *engines, *p;
  IBusComponent *component;

  ibus_init();

  bus = ibus_bus_new();
  g_signal_connect(bus, "disconnected", G_CALLBACK(ibus_disconnected_cb), NULL);

  component = ibus_keyman_get_component();

  factory = ibus_factory_new(ibus_bus_get_connection(bus));

  engines = ibus_component_get_engines(component);
  for (p = engines; p != NULL; p = p->next) {
    IBusEngineDesc *engine = (IBusEngineDesc *)p->data;
#if IBUS_CHECK_VERSION(1, 3, 99)
    const gchar *engine_name = ibus_engine_desc_get_name(engine);
#else
    const gchar *engine_name = engine->name;
#endif /* !IBUS_CHECK_VERSION(1,3,99) */
    ibus_factory_add_engine(factory, engine_name, IBUS_TYPE_KEYMAN_ENGINE);
  }

  if (ibus) {
    ibus_bus_request_name(bus, "org.freedesktop.IBus.Keyman", 0);
  } else {
    ibus_bus_register_component(bus, component);
  }

  g_object_unref(component);
  km_service_get_default(NULL);  // initialise dbus service

  ibus_main();
}

static void
print_engines_xml(void) {
  IBusComponent *component;
  GString *output;

  ibus_init();

  component = ibus_keyman_get_component();
  output    = g_string_new("");

  ibus_component_output_engines(component, output, 0);

  fprintf(stdout, "%s", output->str);

  g_string_free(output, TRUE);
  g_object_unref(component);
}

int
main(gint argc, gchar **argv) {
  GError *error = NULL;
  GOptionContext *context;

  setlocale(LC_ALL, "");

  context = g_option_context_new("- ibus Keyman engine component");

  g_option_context_add_main_entries(context, entries, "ibus-keyman");

  if (!g_option_context_parse(context, &argc, &argv, &error)) {
    g_print("Option parsing failed: %s\n", error->message);
    exit(-1);
  }

  if (xml) {
    print_engines_xml();
    exit(0);
  }

  start_component();
  return 0;
}
