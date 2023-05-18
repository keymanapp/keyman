
#include <json-schema.hpp>
#include <fstream>
#include "../kmcmplib/include/kmcmplibapi.h"

#include <windows.h>

typedef bool (*kmcmp_ValidateJsonMessageProc)(int64_t offset, const char* szText, void* context);

extern bool flag_use_new_kmcomp;

using nlohmann::json;
using nlohmann::json_uri;
using nlohmann::json_schema_draft4::json_validator;

typedef BOOL (CALLBACK *ValidateJsonMessageProc)(INT64 offset, const char* szText);

static void loader(const json_uri &uri, json &schema)
{
  std::fstream lf("." + uri.path());
  if (!lf.good())
    throw std::invalid_argument("could not open " + uri.url() + " tried with " + uri.path());

  try {
    lf >> schema;
  }
  catch (std::exception &e) {
    throw e;
  }
}

bool kmcmpMessageProc(int64_t offset, const char* szText, void* context) {
  return ((ValidateJsonMessageProc)context)(offset, szText);
}

extern "C" BOOL __declspec(dllexport) ValidateJsonFile(PWSTR pwszSchemaFile, PWSTR pwszJsonFile, ValidateJsonMessageProc MessageProc) {

  if ( flag_use_new_kmcomp )
  {
    std::fstream f(pwszSchemaFile);
    std::fstream fd(pwszJsonFile);
    return kmcmp_ValidateJsonFile(f, fd, kmcmpMessageProc, (void*)MessageProc);
  }

  std::fstream f(pwszSchemaFile);
  if (!f.good()) {
    MessageProc(-1, "Schema file could not be loaded.");
    return FALSE;
  }

  // 1) Read the schema for the document you want to validate
  json schema;
  try {
    f >> schema;
  }
  catch (std::exception &e) {
    MessageProc(f.tellp(), e.what());
    return FALSE;
  }

  // 2) create the validator and
  json_validator validator(loader, [](const std::string &, const std::string &) {});

  try {
    // insert this schema as the root to the validator
    // this resolves remote-schemas, sub-schemas and references via the given loader-function
    validator.set_root_schema(schema);
  }
  catch (std::exception &e) {
    MessageProc(-2, e.what());
    return FALSE;
  }

  // 3) do the actual validation of the document
  json document;

  std::fstream fd(pwszJsonFile);
  if (!fd.good()) {
    MessageProc(-3, "Json file could not be loaded.");
    return FALSE;
  }

  try {
    fd >> document;
    validator.validate(document);
  }
  catch (std::exception &e) {
    MessageProc(fd.tellp(), e.what());
    return FALSE;
  }

  return TRUE;
}