/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
#ifdef __EMSCRIPTEN__
#include <emscripten/emscripten.h>
#include <emscripten/bind.h>
#include <vector>
#include <sstream>

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

// Enable for std::vector<T>
template <typename T>
struct TypeID<
    T,
    typename std::enable_if_t<std::is_same<
        typename Canonicalized<T>::type,
        std::vector<typename Canonicalized<T>::type::value_type, typename Canonicalized<T>::type::allocator_type>>::value>> {
  static constexpr TYPEID get() {
    return TypeID<val>::get();
  }
};

template <typename T, typename Allocator> struct BindingType<std::vector<T, Allocator>> {
  using ValBinding = BindingType<val>;
  using WireType   = ValBinding::WireType;

#if (__EMSCRIPTEN_major__ == 3 && __EMSCRIPTEN_minor__ == 1 && __EMSCRIPTEN_tiny__ >= 60) || (__EMSCRIPTEN_major__ > 3)
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

km_core_attr const& tmp_wasm_attributes() {
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

class km_core_actions_wasm {
private:
  std::string getCodePoints(const std::u32string& str) const {
    std::stringstream result;
    for (char32_t codePoint : str) {
      result << "U+" << std::hex << codePoint << " ";
    }
    return result.str();
  }

public:
  unsigned int code_points_to_delete;
  std::u32string output;
  std::vector<km_core_option_item_wasm> persist_options;
  bool do_alert;
  bool emit_keystroke;
  int new_caps_lock_state;
  std::u32string deleted_context;

#ifndef NDEBUG
  std::string toString() const {
    std::string result = "Actions(delete=" + std::to_string(this->code_points_to_delete) +
                         ", output=\"" + std::string(this->output.begin(), this->output.end()) + "\" (" + getCodePoints(this->output) + ")" +
                         ", do_alert=" + (this->do_alert ? "true" : "false") +
                         ", emit_keystroke=" + (this->emit_keystroke ? "true" : "false") +
                         ", new_caps_lock_state=" + std::to_string(this->new_caps_lock_state) +
                         ", deleted_context=\"" + std::string(this->deleted_context.begin(), this->deleted_context.end()) + "\"" +
                         ", persist_options=[";
    for (size_t i = 0; i < this->persist_options.size(); ++i) {
      result += "{" + std::string(this->persist_options[i].key.begin(), this->persist_options[i].key.end()) +
                ": " + std::string(this->persist_options[i].value.begin(), this->persist_options[i].value.end()) +
                ", scope=" + std::to_string(this->persist_options[i].scope) + "}";
      if (i + 1 < this->persist_options.size()) {
        result += ", ";
      }
    }
    result += "])";
    return result;
  }
#endif
};

class km_core_context_item_wasm: public km_core_context_item {
  // Binding a union is not directly supported by emscripten, so we
  // provide getters and setters.
  // See https://github.com/emscripten-core/emscripten/issues/5381

public:
  km_core_context_item_wasm() {}
  km_core_context_item_wasm(km_core_context_item const& item) : km_core_context_item(item) {}

  unsigned int getType() const {
    return this->type;
  }

  unsigned int getCharacter() const {
    return this->character;
  }
  void setCharacter(unsigned int character) {
    this->character = character;
    this->type = KM_CORE_CT_CHAR;
  }

  unsigned int getMarker() const {
    return this->marker;
  }
  void setMarker(unsigned int marker) {
    this->marker = marker;
    this->type = KM_CORE_CT_MARKER;
  }

  std::string toString() const {
    switch (this->type) {
      case KM_CORE_CT_CHAR:
        return "CHAR(" + std::to_string(this->character) + ")";
      case KM_CORE_CT_MARKER:
        return "MARKER(" + std::to_string(this->marker) + ")";
      case KM_CORE_CT_END:
        return "END";
      default:
        return "UNKNOWN";
    }
  }
};

struct km_core_context_items {
private:
  std::vector<km_core_context_item_wasm> items = std::vector<km_core_context_item_wasm>();
public:
  void push_back(const km_core_context_item_wasm& item) {
    items.push_back(km_core_context_item_wasm(item));
  }
  void resize(const size_t size) {
    items.resize(size);
  }
  size_t size() const {
    return items.size();
  }
  const km_core_context_item_wasm& get(size_t index) const {
    if (index >= items.size()) {
      throw std::out_of_range("index out of range");
    }
    return items[index];
  }
  void set(size_t index, const km_core_context_item_wasm& item) {
    if (index >= items.size()) {
      throw std::out_of_range("index out of range");
    }
    items[index] = item;
  }

  const km_core_context_item_wasm* data() const {
    return items.data();
  }

  std::string toString() const {
    std::string result = "(" + std::to_string(items.size()) + ") [";
    for (size_t i = 0; i < items.size(); ++i) {
      result += items[i].toString();
      if (i + 1 < items.size()) {
        result += ", ";
      }
    }
    result += "]";
    return result;
  }
};

template <typename T>
class CoreReturn {
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

km_core_context_item_wasm* km_core_context_item_create_end() {
  return new km_core_context_item_wasm({KM_CORE_CT_END, {0,}, {0,}});
}

km_core_status
keyboard_load_from_blob_internal(const km_core_path_name kb_name, const std::vector<uint8_t>& buf, km_core_keyboard** keyboard);

const CoreReturn<km_core_keyboard>*
km_core_keyboard_load_from_blob_wasm(
  std::string kb_name,
  const emscripten::val& blob_val
) {
  std::vector<uint8_t> blob = emscripten::convertJSArrayToNumberVector<uint8_t>(blob_val);
  km_core_keyboard* keyboard_ptr = nullptr;

  km_core_status retVal = ::keyboard_load_from_blob_internal(kb_name.c_str(), blob, &keyboard_ptr);
  return new CoreReturn<km_core_keyboard>(retVal, keyboard_ptr);
}

const CoreReturn<km_core_keyboard_attrs_wasm>*
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

const CoreReturn<km_core_state>*
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

const CoreReturn<km_core_state>*
km_core_state_clone_wasm(const km_core_state* state) {
  km_core_state* new_state = nullptr;
  km_core_status retVal = km_core_state_clone(state, &new_state);
  return new CoreReturn<km_core_state>(retVal, new_state);
}

int
km_core_state_context_set_if_needed_wasm(
  km_core_state const* state,
  std::u16string application_context
) {
  return km_core_state_context_set_if_needed(const_cast<km_core_state*>(state), application_context.c_str());
}

std::u16string
km_core_state_context_debug_wasm(const km_core_state* state, int context_type) {
  std::u16string context;
  km_core_cu* context_c = km_core_state_context_debug(state, static_cast<km_core_debug_context_type>(context_type));
  if (context_c) {
    context = std::u16string(context_c);
    km_core_cu_dispose(context_c);
  }
  return context;
}

km_core_actions_wasm*
km_core_state_get_actions_wasm(km_core_state const *state) {
  km_core_actions const* actions = km_core_state_get_actions(state);
  km_core_actions_wasm* actions_wasm = new km_core_actions_wasm();
  actions_wasm->code_points_to_delete = actions->code_points_to_delete;
  actions_wasm->output = std::u32string(actions->output);
  actions_wasm->do_alert = actions->do_alert;
  actions_wasm->emit_keystroke = actions->emit_keystroke;
  actions_wasm->new_caps_lock_state = actions->new_caps_lock_state;
  actions_wasm->deleted_context = std::u32string(actions->deleted_context);
  for (const km_core_option_item* item = actions->persist_options; item->key; ++item) {
    km_core_option_item_wasm item_wasm;
    item_wasm.key = std::u16string(item->key);
    item_wasm.value = std::u16string(item->value);
    item_wasm.scope = item->scope;
    actions_wasm->persist_options.push_back(item_wasm);
  }
  return actions_wasm;
}

int
km_core_state_options_update_wasm(
  km_core_state const *state,
  const std::vector<km_core_option_item_wasm>& new_options
) {
  km_core_option_item* options_c = new km_core_option_item[new_options.size() + 1];
  for (size_t i = 0; i < new_options.size(); ++i) {
    options_c[i].key = new_options[i].key.c_str();
    options_c[i].value = new_options[i].value.c_str();
    options_c[i].scope = new_options[i].scope;
  }
  options_c[new_options.size()] = {nullptr, nullptr, 0};
  return km_core_state_options_update(const_cast<km_core_state*>(state), options_c);
}

const CoreReturn<km_core_context_items>*
km_core_context_get_wasm(
  km_core_context const* context
) {
  km_core_context_item* items = nullptr;
  km_core_status status = km_core_context_get(context, &items);
  km_core_context_items* items_wasm = new km_core_context_items();
  if (status == KM_CORE_STATUS_OK) {
    for (const km_core_context_item* item = items; item && item->type != KM_CORE_CT_END; item++) {
      items_wasm->push_back(km_core_context_item_wasm(*item));
    }
    items_wasm->push_back(km_core_context_item_wasm({KM_CORE_CT_END, {0,}, {0,}}));
    km_core_context_items_dispose(items);
  }
  return new CoreReturn<km_core_context_items>(status, items_wasm);
}

km_core_status
km_core_context_set_wasm(
  km_core_context const* context,
  km_core_context_items const *context_items
) {
  if (!context_items) {
    return KM_CORE_STATUS_INVALID_ARGUMENT;
  }
  auto items = std::vector<km_core_context_item>(context_items->size());
  for (size_t i = 0; i < context_items->size(); ++i) {
    items[i] = context_items->get(i);
  }
  return km_core_context_set((km_core_context*)context, items.data());
}

km_core_status
km_core_process_event_wasm(
  km_core_state const *state_,
  km_core_virtual_key vk,
  uint16_t modifier_state,
  bool is_key_down,
  uint16_t event_flags
) {
  return km_core_process_event(state_, vk, modifier_state, is_key_down ? 1 : 0, event_flags);
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
  em::class_<CoreReturn<km_core_context_items>>("CoreContextReturn")
      .property("status", &CoreReturn<km_core_context_items>::getStatus)
      .property("object", &CoreReturn<km_core_context_items>::getObject, em::allow_raw_pointers());
  em::class_<km_core_keyboard_attrs_wasm>("km_core_keyboard_attrs")
    .property("version_string", &km_core_keyboard_attrs_wasm::version_string)
    .property("id", &km_core_keyboard_attrs_wasm::id)
    .property("default_options", &km_core_keyboard_attrs_wasm::default_options);
  em::class_<km_core_actions_wasm>("km_core_actions")
#ifndef NDEBUG
    .function("toString", &km_core_actions_wasm::toString)
#endif
    .property("code_points_to_delete", &km_core_actions_wasm::code_points_to_delete)
    .property("output", &km_core_actions_wasm::output)
    .property("persist_options", &km_core_actions_wasm::persist_options)
    .property("do_alert", &km_core_actions_wasm::do_alert)
    .property("emit_keystroke", &km_core_actions_wasm::emit_keystroke)
    .property("new_caps_lock_state", &km_core_actions_wasm::new_caps_lock_state)
    .property("deleted_context", &km_core_actions_wasm::deleted_context);

  em::class_<km_core_state>("km_core_state");
  em::class_<CoreReturn<km_core_state>>("CoreStateReturn")
    .property("status", &CoreReturn<km_core_state>::getStatus)
    .property("object", &CoreReturn<km_core_state>::getObject, em::allow_raw_pointers());

  em::class_<km_core_context>("km_core_context");
  // Since we use it in CoreReturn it has to be bound as class.
  em::class_<km_core_context_items>("km_core_context_items")
    .constructor()
    .function("push_back(item)", &km_core_context_items::push_back)
    .function("resize(size)", &km_core_context_items::resize)
    .function("size", &km_core_context_items::size)
    .function("get(index)", &km_core_context_items::get)
    .function("set(index, item)", &km_core_context_items::set)
    .function("data", &km_core_context_items::data, em::allow_raw_pointers())
    .function("toString", &km_core_context_items::toString);

  em::class_<km_core_context_item_wasm>("km_core_context_item")
    .constructor<>()
    .property("type", &km_core_context_item_wasm::getType)
    .property("character", &km_core_context_item_wasm::getCharacter, &km_core_context_item_wasm::setCharacter)
    .property("marker", &km_core_context_item_wasm::getMarker, &km_core_context_item_wasm::setMarker)
    .function("toString", &km_core_context_item_wasm::toString);

  em::function("create_end_context", &km_core_context_item_create_end, em::allow_raw_pointers());
  em::function("keyboard_load_from_blob(kb_name, blob)", &km_core_keyboard_load_from_blob_wasm, em::allow_raw_pointers());
  em::function("keyboard_dispose(keyboard)", &km_core_keyboard_dispose, em::allow_raw_pointers());
  em::function("keyboard_get_attrs(keyboard)", &km_core_keyboard_get_attrs_wasm, em::allow_raw_pointers());
  em::function("state_create(keyboard, env)", &km_core_state_create_wasm, em::allow_raw_pointers());
  em::function("state_clone(state)", &km_core_state_clone_wasm, em::allow_raw_pointers());
  em::function("state_dispose(state)", &km_core_state_dispose, em::allow_raw_pointers());
  em::function("process_event(state, vk, modifier_state, is_key_down, event_flags)", &km_core_process_event_wasm, em::allow_raw_pointers());

  em::function("state_context(state)", &km_core_state_context, em::allow_raw_pointers());
  em::function("state_context_set_if_needed(state, application_context)", &km_core_state_context_set_if_needed_wasm, em::allow_raw_pointers());
  em::function("state_context_clear(state)", &km_core_state_context_clear, em::allow_raw_pointers());
  em::function("state_context_debug(state, context_type)", &km_core_state_context_debug_wasm, em::allow_raw_pointers());

  em::function("state_get_actions(state)", &km_core_state_get_actions_wasm, em::allow_raw_pointers());

  em::function("state_options_update(state, new_options)", &km_core_state_options_update_wasm, em::allow_raw_pointers());

  em::function("context_get(context)", &km_core_context_get_wasm, em::allow_raw_pointers());
  em::function("context_set(context, context_items)", &km_core_context_set_wasm, em::allow_raw_pointers());
}

#endif
