#include <stdio.h>
#include "kmx/kmx_plus.h"
#include "../test_assert.h"

using namespace km::kbp::kmx;

int main(int argc, const char *argv[]) {
    // COMP_KMXPLUS_KEYS_ENTRY test
    COMP_KMXPLUS_KEYS_ENTRY e[2] = {
        {
            KM_KBP_VKEY__AB,
            0,
            0x00000127, // to = U+0127 = 295
            0x00000000  // flags: !EXTEND
        },
        {
            KM_KBP_VKEY__A1,
            0,
            0x0001F640, // to
            0x00000000  // flags: !EXTEND
        }
    };
    std::u16string s0 = e[0].get_string();
    assert_equal(s0.length(), 1);
    assert_equal(s0.at(0), 0x0127);
    assert(s0 == std::u16string(u"ħ"));

    std::u16string s1 = e[1].get_string();
    assert_equal(s1.length(), 2);
    assert_equal(s1.at(0), 0xD83D);
    assert_equal(s1.at(1), 0xDE40);
    assert(s1 == std::u16string(u"🙀"));

    return 0;
}
