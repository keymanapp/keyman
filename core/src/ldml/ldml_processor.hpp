/*
  Copyright:    © SIL International.
  Description:  Internal keyboard class and adaptor class for the LDML Keyboard Processor.
  Create Date:  6 Aug 2022
  Authors:      Marc Durdin
*/

#pragma once

#include <map>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>
#include <memory>
#include "keyman_core.h"
#include "processor.hpp"
#include "option.hpp"
#include "ldml_vkeys.hpp"
#include "ldml_transforms.hpp"

namespace km {
namespace core {

#define KM_CORE_LMDL_PROCESSOR_VERSION u"1.0"

class ldml_event_state;

/** our actual processor */
class ldml_processor : public abstract_processor {
  public:
    ldml_processor(
      std::u16string const& kb_name,
      const std::vector<uint8_t> & data
    );

    km_core_status
    process_event(
      km_core_state *state,
      km_core_virtual_key vk,
      uint16_t modifier_state,
      uint8_t is_key_down,
      uint16_t event_flags
    ) override;

    virtual km_core_attr const & attributes() const override;
    km_core_status               validate() const override;

    char16_t const *
    lookup_option(
      km_core_option_scope _kmn_unused(scope),
      std::u16string const & _kmn_unused(key)
    ) const override {
      return nullptr;
    }

    option
    update_option(
      km_core_option_scope _kmn_unused(scope),
      std::u16string const & _kmn_unused(key),
      std::u16string const & _kmn_unused(value)
    ) override {
      return option();
    }

    km_core_status process_queued_actions(km_core_state *state) override;

    bool queue_action(
      km_core_state * state,
      km_core_action_item const* action_item
    ) override;

    km_core_context_item * get_intermediate_context() override;

    km_core_keyboard_key  * get_key_list() const override;

    km_core_keyboard_imx  * get_imx_list() const override;

    inline bool
    supports_normalization() const override {
      return !normalization_disabled;
    }

    static bool is_handled(const std::vector<uint8_t> & buf);

  private:
    /** process a key-up */
    void process_key_up(ldml_event_state& ldml_state) const;

    /** process a key-down (if it wasn't handled exceptionally) */
    void process_key_down(ldml_event_state &ldml_state) const;

    /** process a typed key */
    void process_key_string(ldml_event_state &ldml_state, const std::u16string &key_str) const;

    /** process a backspace */
    void process_backspace(ldml_event_state &ldml_state) const;

    /**
     * common function for outputting a string with transforms/normalization applied.
     * @param str string to output (such as from a key), or empty
     * @param with_transforms transforms to use or nullptr
     * @returns length of matched input context
     */
    size_t process_output(ldml_event_state &ldml_state, const std::u32string &str, ldml::transforms *with_transforms) const;

    /**
     * Apply transforms.
     * @param new_ctxt input/output context to apply
     * @param with_transforms transforms to use
     * @returns length of matched input context, or 0 if no match
     */
    size_t apply_transforms(std::u32string &new_ctxt, ldml::transforms *with_transforms) const;

  private:
    bool _valid;
    std::unique_ptr<ldml::transforms> transforms, bksp_transforms;
    ldml::vkeys keys;
    bool normalization_disabled;
};


/** class holding state as we process an event. mirrors process_event args. */
class ldml_event_state {
public:
  ldml_event_state(
      km_core_state *state,
      km_core_virtual_key vk,
      uint16_t modifier_state,
      uint8_t is_key_down,
      uint16_t event_flags);
  ~ldml_event_state();
  /** done with this, copy it into core state */
  void commit();
  /** clear this object out */
  void clear();
  /** clear underlying context */
  void context_clear();

  // getters
  inline km_core_virtual_key get_vk() const;
  inline uint16_t get_modifier_state() const;

  // actions

  /** emit text to context and actions */
  void emit_text(const std::u16string &str);
  /** emit text to context and actions */
  void emit_text(const std::u32string &str);
  /** emit char to context and actions */
  void emit_text(km_core_usv ch);
  /** emit a marker */
  void emit_marker(KMX_DWORD marker);
  /** emit a pass-through */
  void emit_passthrough_keystroke();
  /** emit a backspace (for a user-initiated deletion) */
  void emit_backspace();
  /** emit the difference between two strings */
  void emit_difference(const std::u32string &old_ctxt, const std::u32string &new_ctxt);

  /**
   * Delete text from the state, by:
   *  1. calling actions().push_backspace() to push the appropriate backspaces
   *  2. popping the same items from the context items
   *  3. mutating 'str' by removing the same number of items.
   * This function handles marker strings correctly.
   * @param str string with text to remove, from the end
   * @param length number of chars from the end of str to drop
   */
   void remove_text(std::u32string &str, size_t length);

   /**
    * add the string+marker portion of the context to the beginning of str.
    * Stop when a non-string and non-marker is hit.
    * Convert markers into the UC_SENTINEL format.
    * @return the number of context items consumed
    */
   size_t context_to_string(std::u32string &str, bool include_markers = true);

 private:
   km_core_virtual_key vk;
   uint16_t modifier_state;
   uint8_t is_key_down;
   uint16_t event_flags;
   km_core_state *state;

   // our in-flight action struct.
   km_core_actions actions;
   // text to add
   std::u32string  text;
};


// implementation
km_core_virtual_key
ldml_event_state::get_vk() const {
  return vk;
}

uint16_t
ldml_event_state::get_modifier_state() const {
  return modifier_state;
}

} // namespace core
} // namespace km
