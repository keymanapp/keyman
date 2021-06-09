/*
  Copyright:    © 2021 SIL International.
  Description:  Internal keyboard class and adaptor class for the Rust API implementations.
  Authors:      Marc Durdin; starting from mock_processor.hpp
*/

#pragma once

#include <string>
#include <unordered_map>
#include <keyman/keyboardprocessor.h>

#include "processor.hpp"
#include "option.hpp"

namespace km {
namespace kbp
{
  class rust_mock_processor : public abstract_processor
  {
    std::unordered_map<std::u16string, std::u16string> _options;

  public:
    rust_mock_processor(km::kbp::path const &);
//    ~mock_processor() override;

    km_kbp_status process_event(km_kbp_state *state,
                                km_kbp_virtual_key vk,
                                uint16_t modifier_state) override;

    virtual km_kbp_attr const & attributes() const override;
    km_kbp_status               validate() const override;



    char16_t const * lookup_option(km_kbp_option_scope,
                  std::u16string const & key) const override;
    option  update_option(km_kbp_option_scope,
                  std::u16string const & key,
                  std::u16string const & value) override;
  };

  class rust_null_processor : public rust_mock_processor {
  public:
    rust_null_processor(): rust_mock_processor(path())
    {
      _attributes = keyboard_attributes(u"null", u"0.0", path(), {});
    }

    km_kbp_status validate() const override;
  };
} // namespace kbp
} // namespace km
