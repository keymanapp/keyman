#include <node.h>
#include <Windows.h>

void HideConsole(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    ShowWindow(GetConsoleWindow(), SW_HIDE);
}

void ShowConsole(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    ShowWindow(GetConsoleWindow(), SW_SHOW);
}

void Initialize(v8::Local<v8::Object> exports) {
    NODE_SET_METHOD(exports, "hideConsole", HideConsole);
    NODE_SET_METHOD(exports, "showConsole", ShowConsole);
}

NODE_MODULE(addon, Initialize);