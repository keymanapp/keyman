#include <stdio.h>
#include "kmx/kmx_plus.h"
#include "../test_assert.h"

using namespace km::kbp::kmx;

int main(int argc, const char *argv[]) {
    COMP_KMXPLUS_KEY2_KEY e[2] = {
        {
            0x00000127, // to = U+0127 = 295
            0x00000000  // flags: !EXTEND
        },
        {
            0x0001F640, // to
            0x00000000  // flags: !EXTEND
        }
    };
    COMP_KMXPLUS_ELEM_ELEMENT elems[2] = {
        {
            0x00000127, // to = U+0127 = 295
            0x00000000  // flags: !LDML_ELEM_FLAGS_UNICODE_SET
        },
        {
            0x0001F640, // to
            0x00000000  // flags: !LDML_ELEM_FLAGS_UNICODE_SET
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

    // now, elems. Parallel.
    std::u16string es0 = elems[0].get_string();
    assert_equal(es0.length(), 1);
    assert_equal(es0.at(0), 0x0127);
    assert(es0 == std::u16string(u"ħ"));

    std::u16string es1 = elems[1].get_string();
    assert_equal(es1.length(), 2);
    assert_equal(es1.at(0), 0xD83D);
    assert_equal(es1.at(1), 0xDE40);
    assert(es1 == std::u16string(u"🙀"));

    return 0;
}
