#include <stdio.h>
#include <string>
#include <algorithm>
#include <kmcmplibapi.h>
#include "util_filesystem.h"
#include "../src/compfile.h"
#include <kmn_compiler_errors.h>

std::vector<int> error_vec;

void msgproc(const KMCMP_COMPILER_RESULT_MESSAGE &message, void* context) {
  error_vec.push_back(message.errorCode);
  const char*t = "unknown";
  switch(message.errorCode & MESSAGE_SEVERITY_MASK) {
    case CompilerErrorSeverity::Hint:    t=" SevHint"; break;
    case CompilerErrorSeverity::Warn:    t=" SevWarn"; break;
    case CompilerErrorSeverity::Error:   t="SevError"; break;
    case CompilerErrorSeverity::Fatal:   t="SevFatal"; break;
    case CompilerErrorSeverity::Info:    t=" SevInfo"; break;
  }
  printf("[CompilerMessage] %s(%d): %s | 0x%3.3x (",
    message.filename.c_str(),
    message.lineNumber,
    t,
    (unsigned int)message.errorCode & MESSAGE_BASEERROR_MASK
  );
  for(auto it = message.parameters.begin(); it != message.parameters.end(); it++) {
    printf("%s'%s'", it == message.parameters.begin() ? "" : ", ", it->c_str());
  }
  printf(")\n");
}

void normalizeSlashes(std::string& s) {
#ifdef _WIN32
  std::replace(s.begin(), s.end(), '/', '\\');
#else
  std::replace(s.begin(), s.end(), '\\', '/');
#endif
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

  normalizeSlashes(resolvedFilename);

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
