/** @file scim_kmfl_imengine_private.h
 *  private used headers are included in this header.
 */
/*
 * KMFL Input Method for SCIM (Smart Common Input Method)
 *
 * Copyright (c) 2004 Doug Rintoul <doug_rintoul@sil.org>
 *
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA  02111-1307  USA
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
