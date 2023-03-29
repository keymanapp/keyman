
#include <json-schema.hpp>
#include <fstream>

#include <kmcmplibapi.h>
#include "kmcompx.h"

using nlohmann::json;
using nlohmann::json_uri;
using nlohmann::json_schema_draft4::json_validator;

static void loader(const json_uri &uri, json &schema)
{
  std::fstream lf("." + uri.path());
  if (!lf.good())
    throw std::invalid_argument("could not open " + uri.url() + " tried with " + uri.path());

  lf >> schema;
}

EXTERN bool kmcmp_ValidateJsonFile(std::fstream& f, std::fstream& fd, kmcmp_ValidateJsonMessageProc MessageProc, void* context) {
  if (!f.good()) {
    MessageProc(-1, "Schema file could not be loaded.", context);
    return FALSE;
  }

  // 1) Read the schema for the document you want to validate
  json schema;
  try {
    f >> schema;
  }
  catch (std::exception &e) {
    MessageProc(f.tellp(), e.what(), context);
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
    MessageProc(-2, e.what(), context);
    return FALSE;
  }

  // 3) do the actual validation of the document
  json document;

  if (!fd.good()) {
    MessageProc(-3, "Json file could not be loaded.", context);
    return FALSE;
  }

  try {
    fd >> document;
    validator.validate(document);
  }
  catch (std::exception &e) {
    MessageProc(fd.tellp(), e.what(), context);
    return FALSE;
  }

  return TRUE;
}