/* -*- mode: C; c-basic-offset: 4; indent-tabs-mode: nil; -*- */
/* vim:set et ts=4: */
/* ibus - The Input Bus
 * Copyright (C) 2008-2010 Peng Huang <shawn.p.huang@gmail.com>
 * Copyright (C) 2008-2010 Red Hat, Inc.
 * Copyright (C) 2021-2022 SIL Global
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301
 * USA
 */
#ifndef __IBUSIMCONTEXT_H__
#define __IBUSIMCONTEXT_H__

#include <gtk/gtk.h>

/*
 * Type macros.
 */
#define IBUS_TYPE_IM_CONTEXT             \
    (ibus_im_context_get_type ())
#define IBUS_IM_CONTEXT(obj)             \
    (G_TYPE_CHECK_INSTANCE_CAST ((obj), IBUS_TYPE_IM_CONTEXT, IBusIMContext))
#define IBUS_IM_CONTEXT_CLASS(klass)     \
    (G_TYPE_CHECK_CLASS_CAST ((klass), IBUS_TYPE_IM_CONTEXT, IBusIMContextClass))
#define IBUS_IS_IM_CONTEXT(obj)          \
    (G_TYPE_CHECK_INSTANCE_TYPE ((obj), IBUS_TYPE_IM_CONTEXT))
#define IBUS_IS_IM_CONTEXT_CLASS(klass)  \
    (G_TYPE_CHECK_CLASS_TYPE ((klass), IBUS_TYPE_IM_CONTEXT))
#define IBUS_IM_CONTEXT_GET_CLASS(obj)   \
    (G_TYPE_CHECK_GET_CLASS ((obj), IBUS_TYPE_IM_CONTEXT, IBusIMContextClass))

G_BEGIN_DECLS
typedef struct _IBusIMContext IBusIMContext;
typedef struct _IBusIMContextClass IBusIMContextClass;
typedef struct _IBusIMContextPrivate IBusIMContextPrivate;


GType    ibus_im_context_get_type (void);
IBusIMContext
        *ibus_im_context_new      (void);
void     ibus_im_context_register_type
                                  (GTypeModule    *type_module);
void     ibus_im_context_shutdown
                                  (void);
const gchar
        *ibus_im_context_get_ic   (IBusIMContext  *context);
void     ibus_im_context_set_ic   (IBusIMContext  *context,
                                   const gchar    *ic);
void     ibus_im_context_enable   (IBusIMContext  *context);
void     ibus_im_context_disable  (IBusIMContext  *context);
void     ibus_im_context_commit_string
                                  (IBusIMContext  *context,
                                   const gchar    *string);

void ibus_im_test_set_thread_loop(IBusIMContext *context, GMainLoop *loop);

void ibus_im_test_set_text(IBusIMContext *context, const gchar *text);
const gchar *ibus_im_test_get_text(IBusIMContext *context);
void ibus_im_test_clear_text(IBusIMContext *context);
void ibus_im_test_set_surrounding_text_supported(gboolean supported);
G_END_DECLS
#endif // __IBUSIMCONTEXT_H__

