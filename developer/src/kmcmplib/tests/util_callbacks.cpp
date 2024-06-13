#include <stdio.h>
#include <string>
#include <kmcmplibapi.h>
#include "util_filesystem.h"
#include "../src/compfile.h"
#include <kmn_compiler_errors.h>

std::vector<int> error_vec;

int msgproc(int line, uint32_t dwMsgCode, const char* szText, void* context) {
  error_vec.push_back(dwMsgCode);
  const char*t = "unknown";
  switch(dwMsgCode & 0xF000) {
    case CERR_HINT:    t="   hint"; break;
    case CERR_WARNING: t="warning"; break;
    case CERR_ERROR:   t="  error"; break;
    case CERR_FATAL:   t="  fatal"; break;
  }
  printf("line %d  %s %4.4x:  %s\n", line, t, (unsigned int)dwMsgCode, szText);
	return 1;
}

bool loadfileProc(const char* filename, const char* baseFilename, void* data, int* size, void* context) {
  std::string resolvedFilename = filename;
  if(baseFilename && *baseFilename && IsRelativePath(filename)) {
    char* p;
    if ((p = strrchr_slash((char*)baseFilename)) != nullptr) {
      std::string basePath = std::string(baseFilename, (int)(p - baseFilename + 1));
      resolvedFilename = basePath;
      resolvedFilename.append(filename);
    }
  }

  FILE* fp = Open_File(resolvedFilename.c_str(), "rb");
  if(!fp) {
    return false;
  }

  if(!data) {
    // return size
    if(fseek(fp, 0, SEEK_END) != 0) {
      fclose(fp);
      return false;
    }
    *size = ftell(fp);
    if(*size == -1L) {
      fclose(fp);
      return false;
    }
  } else {
    // return data
    if((int)fread(data, 1, *size, fp) != *size) {
      fclose(fp);
      return false;
    }
  }
  fclose(fp);
  return true;
}
