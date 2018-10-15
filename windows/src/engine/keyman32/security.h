#pragma once

#include <aclapi.h>

BOOL SetObjectToLowIntegrity(HANDLE hObject, SE_OBJECT_TYPE type = SE_KERNEL_OBJECT);
BOOL GrantPermissionToAllApplicationPackages(HANDLE handle, DWORD dwAccessPermissions, SE_OBJECT_TYPE type = SE_KERNEL_OBJECT);
