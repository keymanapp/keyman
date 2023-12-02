// In ths program we use a 3D-Vector  Vector[language][Keys][Shiftstates]
#pragma once
#ifndef DEADKEY_H
#define DEADKEY_H

#include <X11/XKBlib.h>
#include <X11/Xlib.h>
#include <gdk/gdk.h>
#include <map>

int createDK_ComposeTable(v_dw_2D & dk_ComposeTable);

KMX_DWORD find_ComposedCharacter(v_dw_2D * dk_ComposeTable, KMX_DWORD first, KMX_DWORD second , KMX_DWORD third = 0 );

# endif /*DEADKEY_H*/