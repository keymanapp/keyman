/*
  Copyright:    © SIL International.
  Description:  Minimal, hermetic Keyman core test.
  Create Date:  6 Jan 2024
  Authors:      Steven R. Loomis

  This is a different kind of a test. It's a very minimal test of the core API.
  It does not have any file i/o other than console output and return code.

  This test is expected to fail with an assertion failure. It's to exercise the API.
*/

#include <iostream>
#include <assert.h>

#include "keyman_core.h"
#include "../load_kmx_file.hpp"

int main(int argc, const char *argv[]) {

    // "load" our "keyboard"
    km_core_keyboard * test_kb = nullptr;

    km_core_status status;
    km_core_path_name nowhere = {0}; // this is a narrow or wide char string
    auto blob = km::tests::load_kmx_file(nowhere);
    status = km_core_keyboard_load_from_blob(nowhere, blob.data(), blob.size(), &test_kb);

    std::cerr << "null km_core_keyboard_load = " << status << std::endl;
    assert(status == KM_CORE_STATUS_INVALID_ARGUMENT);
    assert(test_kb == nullptr);
    km_core_keyboard_dispose(test_kb);
    km_core_state_dispose(nullptr);
    km_core_cu_dispose(nullptr);

    status = km_core_process_event(nullptr, 0, 0, 0, 0);
    /* NOTREACHED - assertion fails above. */
    assert(status == KM_CORE_STATUS_INVALID_ARGUMENT);

    return 0;
}
