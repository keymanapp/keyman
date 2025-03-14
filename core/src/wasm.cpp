#ifdef __EMSCRIPTEN__
#include <emscripten/emscripten.h>
#include <emscripten/bind.h>
#include <vector>

#ifdef __cplusplus
#define EXTERN extern "C" EMSCRIPTEN_KEEPALIVE
#else
#define EXTERN EMSCRIPTEN_KEEPALIVE
#endif

#include <keyman_core.h>
#include "processor.hpp"
#include "state.hpp"

namespace em = emscripten;

// This little bit of magic gives us implicit bindings for any `std::vector`,
// so long as we bind `emscripten::value_object<T>`, and comes from:
// https://github.com/emscripten-core/emscripten/issues/11070#issuecomment-717675128
namespace emscripten {
namespace internal {

template <typename T, typename Allocator>
struct BindingType<std::vector<T, Allocator>> {
    using ValBinding = BindingType<val>;
    using WireType = ValBinding::WireType;

#if __EMSCRIPTEN_major__ == 3 && __EMSCRIPTEN_minor__ == 1 && __EMSCRIPTEN_tiny__ >= 60
    // emscripten-core/emscripten#21692
    static WireType toWireType(const std::vector<T, Allocator> &vec, rvp::default_tag) {
        return ValBinding::toWireType(val::array(vec), rvp::default_tag{});
    }
#else
    static WireType toWireType(const std::vector<T, Allocator> &vec) {
        return ValBinding::toWireType(val::array(vec));
    }
#endif

    static std::vector<T, Allocator> fromWireType(WireType value) {
        return vecFromJSArray<T>(ValBinding::fromWireType(value));
    }
};

template <typename T>
struct TypeID<T,
              typename std::enable_if_t<std::is_same<
                  typename Canonicalized<T>::type,
                  std::vector<typename Canonicalized<T>::type::value_type,
                              typename Canonicalized<T>::type::allocator_type>>::value>> {
    static constexpr TYPEID get() { return TypeID<val>::get(); }
};

}  // namespace internal
}  // namespace emscripten

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

struct km_core_option_item_wasm {
  std::u16string   key;
  std::u16string   value;
  uint8_t          scope;
};


class km_core_keyboard_attrs_wasm {
public:
  std::u16string version_string;
  std::u16string id;
  std::vector<km_core_option_item_wasm> default_options;
};

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
  std::vector<uint8_t> blob = emscripten::convertJSArrayToNumberVector<uint8_t>(blob_val);
  km_core_keyboard* keyboard_ptr = nullptr;

  km_core_status retVal = ::keyboard_load_from_blob_internal(kb_name.c_str(), blob, &keyboard_ptr);
  return new CoreReturn<km_core_keyboard>(retVal, keyboard_ptr);
}

EMSCRIPTEN_KEEPALIVE const CoreReturn<km_core_keyboard_attrs_wasm>*
km_core_keyboard_get_attrs_wasm(const km_core_keyboard* keyboard) {
  const km_core_keyboard_attrs* attrs = nullptr;
  km_core_status retVal = km_core_keyboard_get_attrs(keyboard, &attrs);
  km_core_keyboard_attrs_wasm* attrs_wasm = nullptr;
  if (attrs) {
    attrs_wasm = new km_core_keyboard_attrs_wasm();
    attrs_wasm->version_string = std::u16string(attrs->version_string);
    attrs_wasm->id = std::u16string(attrs->id);
    for (const km_core_option_item* item = attrs->default_options; item->key; ++item) {
      km_core_option_item_wasm item_wasm;
      item_wasm.key = std::u16string(item->key);
      item_wasm.value = std::u16string(item->value);
      item_wasm.scope = item->scope;
      attrs_wasm->default_options.push_back(item_wasm);
    }
  }
  return new CoreReturn<km_core_keyboard_attrs_wasm>(retVal, attrs_wasm);
}

EMSCRIPTEN_KEEPALIVE const CoreReturn<km_core_state>*
km_core_state_create_wasm(const km_core_keyboard* keyboard,
                          const std::vector<km_core_option_item_wasm>& env) {
  km_core_option_item* env_c = new km_core_option_item[env.size() + 1];
  for (size_t i = 0; i < env.size(); ++i) {
    env_c[i].key = env[i].key.c_str();
    env_c[i].value = env[i].value.c_str();
    env_c[i].scope = env[i].scope;
  }
  env_c[env.size()] = {nullptr, nullptr, 0};

  km_core_state* state = nullptr;
  km_core_status retVal = km_core_state_create(keyboard, env_c, &state);
  delete[] env_c;
  return new CoreReturn<km_core_state>(retVal, state);
}

EMSCRIPTEN_KEEPALIVE const CoreReturn<km_core_state>*
km_core_state_clone_wasm(const km_core_state* state) {
  km_core_state* new_state = nullptr;
  km_core_status retVal = km_core_state_clone(state, &new_state);
  return new CoreReturn<km_core_state>(retVal, new_state);
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
  em::value_object<km_core_option_item_wasm>("km_core_option_item")
    .field("key", &km_core_option_item_wasm::key)
    .field("value", &km_core_option_item_wasm::value)
    .field("scope", &km_core_option_item_wasm::scope);

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
  em::class_<CoreReturn<km_core_keyboard_attrs_wasm>>("CoreKeyboardAttrsReturn")
      .property("status", &CoreReturn<km_core_keyboard_attrs_wasm>::getStatus)
      .property("object", &CoreReturn<km_core_keyboard_attrs_wasm>::getObject, em::allow_raw_pointers());
  em::class_<km_core_keyboard_attrs_wasm>("km_core_keyboard_attrs")
    .property("version_string", &km_core_keyboard_attrs_wasm::version_string)
    .property("id", &km_core_keyboard_attrs_wasm::id)
    .property("default_options", &km_core_keyboard_attrs_wasm::default_options);

  em::class_<km_core_state>("km_core_state");
  em::class_<CoreReturn<km_core_state>>("CoreStateReturn")
      .property("status", &CoreReturn<km_core_state>::getStatus)
      .property("object", &CoreReturn<km_core_state>::getObject, em::allow_raw_pointers());

  em::function("keyboard_load_from_blob", &km_core_keyboard_load_from_blob_wasm, em::allow_raw_pointers());
  em::function("keyboard_dispose", &km_core_keyboard_dispose, em::allow_raw_pointers());
  em::function("keyboard_get_attrs", &km_core_keyboard_get_attrs_wasm, em::allow_raw_pointers());
  em::function("state_create", &km_core_state_create_wasm, em::allow_raw_pointers());
  em::function("state_clone", &km_core_state_clone_wasm, em::allow_raw_pointers());
  em::function("state_dispose", &km_core_state_dispose, em::allow_raw_pointers());
  em::function("process_event", &km_core_process_event, em::allow_raw_pointers());
}
#endif
