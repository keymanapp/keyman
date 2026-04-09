#ifndef __LOAD_KMX_FILE_HPP__
#define __LOAD_KMX_FILE_HPP__

#include "path.hpp"

namespace km {
namespace tests {

std::vector<uint8_t> load_kmx_file(km::core::path const& kb_path);

}
}

#endif  // __LOAD_KMX_FILE_HPP__
