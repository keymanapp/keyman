/*
  Copyright:    © 2011,2018 SIL International.
  Description:  JSON prettyprinter test harness.
  Create Date:  15 Dec 2011
  Authors:      Tim Eves (TSE)
  History:      28 Sep 2018 - TSE - Imported from graphite2 project.
                            - TSE - Refactored to use C++ std::ostream.
                25 Oct 2018 - TSE - Relicensed under the MIT license for
                                    inclusion in the Keyman project.
*/

// JSON debug logging very basic test harness
// Author: Tim Eves

#include <iostream>
#include <fstream>
#include <json.hpp>


int main(int argc, char * argv[])
{
  std::ofstream log;
  if (argc > 1)
    log.open(argv[1]);

  json	jo(argc == 1 ? std::cout : log);


  jo << json::array
      << "a string"
      << 0.54
      << 123
      << false
      << json::null
      << json::close;

  jo << json::object << "empty object" << json::object << json::close;

  jo << "primitive types" << json::object
      << "string"   << "a string"
      << "number"   << 0.54
      << "integer"  << 123
      << "boolean"  << false
      << "null"     << json::null
      << json::close;

  jo << "complex object" << json::object
      << "firstName"  << "John"
      << "lastName"   << "Smith"
      << "age"        << 25
      << "address"    << json::flat << json::object
        << "streetAddress"  << "21 2nd Street"
        << "city"           << "New York"
        << "state"          << "NY"
        << "postalCode"     << "10021"
        << json::close
      << "phoneNmuber" << json::array
        << json::flat << json::object
          << "type"   << "home"
          << "number" << "212 555-1234"
          << json::close
        << json::object
          << "type"   << "fax"
          << "number" << "646 555-4567";

  return 0;
}
