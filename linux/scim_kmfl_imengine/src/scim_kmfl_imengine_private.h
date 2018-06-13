/** @file scim_kmfl_imengine_private.h
 *  private used headers are included in this header.
 */

/*
 * KMFL Input Method for SCIM (Smart Common Input Method)
 *
 * Copyright (C) 2005 SIL International
 * based on source from SCIM Copyright (c) 2004 James Su <suzhe@tsinghua.org.cn>
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

#ifndef __SCIM_KMFL_IMENGINE_PRIVATE_H
#define __SCIM_KMFL_IMENGINE_PRIVATE_H

// Include kmfl configuration header
#ifdef HAVE_CONFIG_H
  #include <config.h>
#endif

#if defined(HAVE_LIBINTL_H) && defined(ENABLE_NLS)
  #include <libintl.h>
  #define _(String) dgettext(GETTEXT_PACKAGE,String)
  #define N_(String) (String)
#else
  #define _(String) (String)
  #define N_(String) (String)
  #define bindtextdomain(Package,Directory)
  #define textdomain(domain)
  #define bind_textdomain_codeset(domain,codeset)
#endif

#endif //__SCIM_KMFL_IMENGINE_PRIVATE_H

/*
vi:ts=4:nowrap:ai:expandtab
*/
