/*
  Copyright:    © 2018 SIL International.
  Description:  Internal keyboard class and adaptor class for the API.
  Create Date:  2 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      2 Oct 2018 - TSE - Refactored out of km_kbp_keyboard_api.cpp
*/

#pragma once

#include <string>
#include <vector>
#include <keyman/keyboardprocessor.h>

#include "processor.hpp"
#include "option.hpp"

namespace km {
namespace kbp
{
  class mock_processor : public abstract_processor
  {
    std::vector<option> _options;
    option const * _find_option(km_kbp_option_scope scope,
                                std::u16string const & key) const;
  public:
    mock_processor(km::kbp::path const &);
//    ~mock_processor() override;

    km_kbp_status process_event(km_kbp_state *state,
                                km_kbp_virtual_key vk,
                                uint16_t modifier_state) override;

    virtual km_kbp_attr const & attributes() const override;
    km_kbp_status               validate() const override;


    void    update_option(km_kbp_state *state,
                          km_kbp_option_scope scope,
                          std::u16string const & key,
                          std::u16string const & value) override;

    void init_state(std::vector<option> &) override;
  };

  class null_processor : public mock_processor {
  public:
    null_processor(): mock_processor(path())
    {
      _attributes = keyboard_attributes(u"null", u"0.0", path(), {});
    }

    km_kbp_status validate() const override;
  };
} // namespace kbp
} // namespace km
