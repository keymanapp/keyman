#ifdef __EMSCRIPTEN__
#include <emscripten/emscripten.h>
#include <emscripten/bind.h>
#include <vector>

#ifdef __cplusplus
#define EXTERN extern "C" EMSCRIPTEN_KEEPALIVE
#else
#define EXTERN EMSCRIPTEN_KEEPALIVE
#endif

#include "processor.hpp"
#include <keyman_core.h>

namespace em = emscripten;

constexpr km_core_attr const engine_attrs = {
  256,
  KM_CORE_LIB_CURRENT,
  KM_CORE_LIB_AGE,
  KM_CORE_LIB_REVISION,
  KM_CORE_TECH_KMX,
  "SIL International"
};

EMSCRIPTEN_KEEPALIVE km_core_attr const& tmp_wasm_attributes() {
  return engine_attrs;
}

template <typename T> class CoreReturn {
public:
  CoreReturn(int status = 0, const T* obj = nullptr) : status(status), object(obj) {
  }
  void setStatus(int status) {
    this->status = status;
  }
  int getStatus() const {
    return status;
  }
  void setObject(const T* obj) {
    object = obj;
  }
  const T* getObject() const {
    return object;
  }

private:
  int status;
  const T* object;
};

km_core_status
keyboard_load_from_blob_internal(const km_core_path_name kb_name, const std::vector<uint8_t>& buf, km_core_keyboard** keyboard);

EMSCRIPTEN_KEEPALIVE const CoreReturn<km_core_keyboard>*
km_core_keyboard_load_from_blob_wasm(
  std::string kb_name,
  const emscripten::val& blob_val
) {
  std::vector<uint8_t> blob;
  km_core_keyboard* keyboard_ptr = nullptr;

  const auto length = blob_val["length"].as<unsigned>();
  blob.resize(length);

  emscripten::val memoryView{emscripten::typed_memory_view(length, blob.data())};
  memoryView.call<void>("set", blob_val);
  km_core_status retVal = ::keyboard_load_from_blob_internal(kb_name.c_str(), blob, &keyboard_ptr);
  return new CoreReturn<km_core_keyboard>(retVal, keyboard_ptr);
}

EMSCRIPTEN_BINDINGS(core_interface) {

  em::value_object<km_core_attr>("km_core_attr")
    .field("max_context", &km_core_attr::max_context)
    .field("current", &km_core_attr::current)
    .field("revision", &km_core_attr::revision)
    .field("age", &km_core_attr::age)
    .field("technology", &km_core_attr::technology)
    //.field("vendor", &km_core_attr::vendor, em::allow_raw_pointers())
    ;

  em::function("tmp_wasm_attributes", &tmp_wasm_attributes);


  // Unfortunately embind has an open issue with enums and typescript where it
  // only generates a type for the enum, but not the values in a usable way.
  // Therefore it's not much use to expose the enum here.
  // See https://github.com/emscripten-core/emscripten/issues/18585

  // em::enum_<km_core_status_codes>("km_core_status_codes")
  //   .value("OK", KM_CORE_STATUS_OK)
  //   .value("NO_MEM", KM_CORE_STATUS_NO_MEM)
  //   .value("IO_ERROR", KM_CORE_STATUS_IO_ERROR)
  //   .value("INVALID_ARGUMENT", KM_CORE_STATUS_INVALID_ARGUMENT)
  //   .value("KEY_ERROR", KM_CORE_STATUS_KEY_ERROR)
  //   .value("INSUFFICENT_BUFFER", KM_CORE_STATUS_INSUFFICENT_BUFFER)
  //   .value("INVALID_UTF", KM_CORE_STATUS_INVALID_UTF)
  //   .value("INVALID_KEYBOARD", KM_CORE_STATUS_INVALID_KEYBOARD)
  //   .value("NOT_IMPLEMENTED", KM_CORE_STATUS_NOT_IMPLEMENTED)
  //   .value("OS_ERROR", KM_CORE_STATUS_OS_ERROR);

  em::class_<km_core_keyboard>("km_core_keyboard");
  em::class_<CoreReturn<km_core_keyboard>>("CoreKeyboardReturn")
      .property("status", &CoreReturn<km_core_keyboard>::getStatus)
      .property("object", &CoreReturn<km_core_keyboard>::getObject, em::allow_raw_pointers());

  em::function("keyboard_load_from_blob", &km_core_keyboard_load_from_blob_wasm, em::allow_raw_pointers());
}
#endif
