#include "TrayIcon.h"

Object Init(Env env, Object exports) {
	CTrayIconContainer::Init(env, exports);
	return exports;
}

NODE_API_MODULE(addon, Init)