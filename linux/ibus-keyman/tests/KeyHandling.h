#ifndef __KEYHANDLING_H__
#define __KEYHANDLING_H__

#include "kmx_test_source.hpp"
#include "testfixture.h"

typedef struct {
  IBusKeymanTestsFixture* fixture;
  km::tests::key_event key_event;
} KeyPressData;

#ifdef __cplusplus
extern "C" {
#endif

void press_key(IBusKeymanTestsFixture* fixture, km::tests::key_event key_event);

#ifdef __cplusplus
}
#endif

#endif // __KEYHANDLING_H__
