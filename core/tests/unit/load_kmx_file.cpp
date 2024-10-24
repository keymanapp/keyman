#include <fstream>
#include <iostream>
#include <list>
#include <string>

#include "kmx_file.h"
#include "path.hpp"
#include "utfcodec.hpp"

namespace km::tests {

std::vector<uint8_t> load_kmx_file(km::core::path const& kb_path) {
  std::vector<uint8_t> data;
  std::ifstream file(static_cast<std::string>(kb_path), std::ios::binary | std::ios::ate);
  if (file.fail()) {
    return std::vector<uint8_t>();
  }
  const std::streamsize size = file.tellg();
  if (size >= KMX_MAX_ALLOWED_FILE_SIZE) {
    return std::vector<uint8_t>();
  }

  file.seekg(0, std::ios::beg);

  data.resize((size_t)size);
  if (!file.read((char*)data.data(), size)) {
    return std::vector<uint8_t>();
  }

  file.close();
  return data;
}

}
