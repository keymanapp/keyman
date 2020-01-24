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

#ifndef __ENGINE_H__
#define __ENGINE_H__

#include <ibus.h>

#define KEYMAN_DCONF_NAME "com.keyman.options"
#define KEYMAN_CHILD_DCONF_NAME "com.keyman.options.child"
#define KEYMAN_DCONF_PATH "/desktop/ibus/keyman/options/"
#define KEYMAN_DCONF_OPTIONS_KEY "options"

// Number of default Keyboard processor environment options for: "platform", "baseLayout", and "baseLayoutAlt"
#define KEYMAN_ENVIRONMENT_OPTIONS 3

#define IBUS_TYPE_KEYMAN_ENGINE	\
	(ibus_keyman_engine_get_type ())

GType   ibus_keyman_engine_get_type    (void);

#endif
