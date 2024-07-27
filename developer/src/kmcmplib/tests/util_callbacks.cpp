#include <stdio.h>
#include <string>
#include <kmcmplibapi.h>
#include "util_filesystem.h"
#include "../src/compfile.h"
#include <kmn_compiler_errors.h>

std::vector<int> error_vec;

void msgproc(const KMCMP_COMPILER_RESULT_MESSAGE &message, void* context) {
  error_vec.push_back(message.errorCode);
  const char*t = "unknown";
  switch(message.errorCode & 0xF000) {
    case SevHint:    t="   hint"; break;
    case SevWarn:    t="warning"; break;
    case SevError:   t="  error"; break;
    case SevFatal:   t="  fatal"; break;
  }
  printf("line %d  %s %4.4x:  %s\n", message.lineNumber, t, (unsigned int)message.errorCode, message.message.c_str());
}

const std::vector<uint8_t> loadfileProc(const std::string& filename, const std::string& baseFilename) {
  std::string resolvedFilename = filename;
  if(baseFilename.length() && IsRelativePath(filename.c_str())) {
    char* p;
    const char *base = baseFilename.c_str();
    if ((p = strrchr_slash((char*)base)) != nullptr) {
      resolvedFilename = std::string(baseFilename, 0, (int)(p - base + 1));
      resolvedFilename.append(filename);
    }
  }

  std::vector<uint8_t> buf;

  FILE* fp = Open_File(resolvedFilename.c_str(), "rb");
  if(!fp) {
    return buf;
  }

  if(fseek(fp, 0, SEEK_END) != 0) {
    fclose(fp);
    return buf;
  }
  size_t size = ftell(fp);

  buf.resize(size);
  fseek(fp, 0, SEEK_SET);
  if(fread(buf.data(), 1, size, fp) != size) {
    buf.resize(0);
  }
  fclose(fp);
  return buf;
}
