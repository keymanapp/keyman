#pragma once

#include <km_types.h>

#include <cstddef>

#include <kmx_file.h>

#ifdef KM_CORE_LIBRARY
// TODO: move this to a common namespace keyman::common::kmx_file or similar in the future
namespace km {
namespace core {
namespace kmx {
#endif


class KMX_FileValidator : public COMP_KEYBOARD {
public:
  /**
   * @brief Return TRUE if keyboard is OK, otherwise FALSE
   * Non const because the checksum gets cleared
   *
   * @param sz total size of keyboard structure
   * @return KMX_BOOL
   */
  KMX_BOOL VerifyKeyboard(std::size_t sz) const;
};


#ifdef KM_CORE_LIBRARY
} // namespace kmx
} // namespace core
} // namespace km
#endif
