#ifdef __EMSCRIPTEN__
#ifdef __EMSCRIPTEN__
#include <emscripten/emscripten.h>
#include <emscripten/bind.h>

#else
#define EMSCRIPTEN_KEEPALIVE
#endif

#ifdef __cplusplus
#define EXTERN extern "C" EMSCRIPTEN_KEEPALIVE
#else
#define EXTERN EMSCRIPTEN_KEEPALIVE
#endif

#include <keyman_core.h>

constexpr km_core_attr const engine_attrs = {
  256,
  KM_CORE_LIB_CURRENT,
  KM_CORE_LIB_AGE,
  KM_CORE_LIB_REVISION,
  KM_CORE_TECH_KMX,
  "SIL International"
};

EMSCRIPTEN_KEEPALIVE km_core_attr const & tmp_wasm_attributes() {
  return engine_attrs;
}

EMSCRIPTEN_BINDINGS(compiler_interface) {

  // emscripten::class_<WasmCallbackInterface>("WasmCallbackInterface")
  //   .function("message", &WasmCallbackInterface::message, emscripten::pure_virtual())
  //   .function("loadFile", &WasmCallbackInterface::loadFile, emscripten::pure_virtual())
  //   .allow_subclass<WasmCallbackInterfaceWrapper>("WasmCallbackInterfaceWrapper")
  //   ;

  // emscripten::class_<KMCMP_COMPILER_OPTIONS>("CompilerOptions")
  //   .constructor<>()
  //   .property("saveDebug", &KMCMP_COMPILER_OPTIONS::saveDebug)
  //   .property("compilerWarningsAsErrors", &KMCMP_COMPILER_OPTIONS::compilerWarningsAsErrors)
  //   .property("warnDeprecatedCode", &KMCMP_COMPILER_OPTIONS::warnDeprecatedCode)
  //   .property("shouldAddCompilerVersion", &KMCMP_COMPILER_OPTIONS::shouldAddCompilerVersion)
  //   .property("target", &KMCMP_COMPILER_OPTIONS::target)
  //   ;

  // emscripten::class_<WASM_COMPILER_RESULT>("CompilerResult")
  //   .constructor<>()
  //   .property("result", &WASM_COMPILER_RESULT::result)
  //   .property("kmx", &WASM_COMPILER_RESULT::kmx)
  //   .property("kmxSize", &WASM_COMPILER_RESULT::kmxSize)
  //   .property("extra", &WASM_COMPILER_RESULT::extra)
  //   ;

  // emscripten::class_<KMCMP_COMPILER_RESULT_MESSAGE>("CompilerResultMessage")
  //   .constructor<>()
  //   .property("errorCode", &KMCMP_COMPILER_RESULT_MESSAGE::errorCode)
  //   .property("lineNumber", &KMCMP_COMPILER_RESULT_MESSAGE::lineNumber)
  //   .property("columnNumber", &KMCMP_COMPILER_RESULT_MESSAGE::columnNumber)
  //   .property("filename", &KMCMP_COMPILER_RESULT_MESSAGE::filename)
  //   .property("parameters", &KMCMP_COMPILER_RESULT_MESSAGE::parameters)
  //   ;

  // emscripten::class_<KMCMP_COMPILER_RESULT_EXTRA>("CompilerResultExtra")
  //   .constructor<>()
  //   .property("targets", &KMCMP_COMPILER_RESULT_EXTRA::targets)
  //   .property("kmnFilename", &KMCMP_COMPILER_RESULT_EXTRA::kmnFilename)
  //   .property("kvksFilename", &KMCMP_COMPILER_RESULT_EXTRA::kvksFilename)
  //   .property("displayMapFilename", &KMCMP_COMPILER_RESULT_EXTRA::displayMapFilename)
  //   .property("stores", &KMCMP_COMPILER_RESULT_EXTRA::stores)
  //   .property("groups", &KMCMP_COMPILER_RESULT_EXTRA::groups)
  //   ;

  // emscripten::value_object<KMCMP_COMPILER_RESULT_EXTRA_STORE>("CompilerResultExtraStore")
  //   .field("storeType", &KMCMP_COMPILER_RESULT_EXTRA_STORE::storeType)
  //   .field("name", &KMCMP_COMPILER_RESULT_EXTRA_STORE::name)
  //   .field("line", &KMCMP_COMPILER_RESULT_EXTRA_STORE::line)
  //   ;

  emscripten::value_object<km_core_attr>("km_core_attr")
    .field("max_context", &km_core_attr::max_context)
    .field("current", &km_core_attr::current)
    .field("revision", &km_core_attr::revision)
    .field("age", &km_core_attr::age)
    .field("technology", &km_core_attr::technology)
    //.field("vendor", &km_core_attr::vendor, emscripten::allow_raw_pointers())
    ;

  emscripten::function("tmp_wasm_attributes", &tmp_wasm_attributes);
}
#endif
