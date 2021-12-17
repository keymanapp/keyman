/*
  Copyright:    © 2018 SIL International.
  Description:  Internal keyboard class and adaptor class for the API.
  Create Date:  2 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      2 Oct 2018 - TSE - Refactored out of km_kbp_keyboard_api.cpp
*/

#pragma once

#include <string>
#include <unordered_map>

#include <keyman/keyboardprocessor.h>

#include "keyboard.hpp"

namespace km {
namespace kbp
{
  class abstract_processor
  {
    std::unordered_map<std::u16string, std::u16string>  _persisted;
  protected:
    keyboard_attributes                                 _attributes;

  public:
    abstract_processor() {}
    abstract_processor(keyboard_attributes && kb) : _attributes(std::move(kb)) {}
    virtual ~abstract_processor() { };

    keyboard_attributes const & keyboard() const noexcept {
      return _attributes;
    }

    auto & persisted_store() const noexcept { return _persisted; }
    auto & persisted_store() noexcept { return _persisted; }

    virtual km_kbp_status
    process_event(
      km_kbp_state *,
      km_kbp_virtual_key,
      uint16_t modifier_state,
      uint8_t is_key_down
    ) = 0;

    virtual km_kbp_attr const & attributes() const = 0;
    virtual km_kbp_status       validate() const = 0;

    virtual char16_t const *
    lookup_option(
      km_kbp_option_scope,
      std::u16string const & key
    ) const = 0;

    virtual option
    update_option(
      km_kbp_option_scope,
      std::u16string const & key,
      std::u16string const & value
    ) = 0;

    /**
     * Add the action items to this keyboard processor queue
     *
     * @param  action_item
     * @param  bool  return true if action item list is successfully processed
     */
    virtual bool
    queue_action(km_kbp_action_item const* action_item
    ) = 0;

  /**
   * Returns the list of keys that belong to the keyboard rules. The matching dispose
   * call needs to be called to free the memory.
   *
   * @return km_kbp_keyboard_key*
   */
    virtual km_kbp_keyboard_key *
    get_key_list() const = 0;


   /** Get the imx list of external libraries and functions
      * this keyboard calls.
    *
    * @return km_kbp_keyboard_imx*
    */
    virtual km_kbp_keyboard_imx *
    get_imx_list() const = 0;

    friend json & operator << (json &j, abstract_processor const &opts);
  };

  json & operator << (json &j, abstract_processor const &opts);

} // namespace kbp
} // namespace km

struct km_kbp_keyboard : public km::kbp::abstract_processor {};
