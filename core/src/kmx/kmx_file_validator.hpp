#pragma once

#include <km_types.h>

#include <cstddef>

#include <kmx_file.h>

#ifdef KMN_CORE
// TODO: move this to a common namespace keyman::common::kmx_file or similar in the future
namespace km {
namespace kbp {
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


#ifdef KMN_CORE
} // namespace kmx
} // namespace kbp
} // namespace km
#endif
