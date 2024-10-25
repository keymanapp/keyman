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

EMSCRIPTEN_BINDINGS(core_interface) {

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
