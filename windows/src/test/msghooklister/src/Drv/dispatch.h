#ifndef DISPATCH_H
#define DISPATCH_H

#pragma once

#include "stdafx.h"

extern "C"
{
	DRIVER_DISPATCH MsgListerDispatchCreate;
	DRIVER_DISPATCH MsgListerDispatchClose;
	DRIVER_DISPATCH MsgListerDispatchControl;
}

#ifdef ALLOC_PRAGMA
#pragma alloc_text(PAGE, MsgListerDispatchCreate, MsgListerDispatchClose)
#pragma alloc_text(PAGE, MsgListerDispatchControl)
#endif

#endif
