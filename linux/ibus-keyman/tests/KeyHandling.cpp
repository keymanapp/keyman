#include <kmx/kmx_processevent.h>
#include <linux/input-event-codes.h>
#include <list>
#include <stack>
#include "keycodes.h"
#include "KeymanSystemServiceClient.h"
#include "testfixture.h"
#include "KeyHandling.h"

#define KEYMAN_LCTRL 29  // 0x1D
#define KEYMAN_LALT 56   // 0x38
#define KEYMAN_RCTRL 97  // 0x61
#define KEYMAN_RALT 100  // 0x64

extern GdkWindow* window;

typedef struct {
  guint modifiers_keydown;
  guint modifiers_keyup;
  guint keyval;
  guint16 keycode;
  guint is_modifier;
} gdk_key_event;

static gdk_key_event get_key_event_for_modifier(guint modifier, guint& prev_modifiers, guint keyval, guint keycode) {
  gdk_key_event event;
  event.modifiers_keydown = prev_modifiers;
  event.modifiers_keyup   = prev_modifiers | modifier | IBUS_RELEASE_MASK;
  prev_modifiers |= modifier;
  event.keyval      = keyval;
  event.keycode     = keycode;
  event.is_modifier = 1;
  return event;
}

static unsigned short vk_to_keycode(unsigned short vk) {
  for (int i = 0; i < (int)sizeof(keycode_to_vk); i++) {
    if (keycode_to_vk[i] == vk)
      return i;
  }
  return -1;
}

static bool is_modifier(guint16 keycode) {
  switch (keycode) {
  case KEY_LEFTSHIFT:
  case KEY_RIGHTSHIFT:
  case KEY_LEFTALT:
  case KEY_RIGHTALT:
  case KEY_LEFTCTRL:
  case KEY_RIGHTCTRL:
  case KEY_CAPSLOCK:
    return true;
  }
  return false;
}

static std::list<gdk_key_event> get_involved_keys(km::tests::key_event test_event) {
  guint modifiers = 0;
  if (get_capslock_indicator()) {
    modifiers = IBUS_LOCK_MASK;
  }

  std::list<gdk_key_event> result;
  if (test_event.vk == KEYMAN_F24_KEYCODE_OUTPUT_SENTINEL) {
    gdk_key_event event;
    event.modifiers_keydown = modifiers;
    event.modifiers_keyup   = modifiers | IBUS_RELEASE_MASK;
    event.keyval            = 0;
    event.keycode           = KEYMAN_F24_KEYCODE_OUTPUT_SENTINEL;
    event.is_modifier       = is_modifier(event.keycode);
    result.push_back(event);

    return result;
  }

  // process all modifiers
  if (test_event.modifier_state & KM_CORE_MODIFIER_SHIFT) {
    // we don't distinguish between R and L Shift
    result.push_back(get_key_event_for_modifier(IBUS_SHIFT_MASK, modifiers, GDK_KEY_Shift_L, KEY_LEFTSHIFT));
  }
  if (test_event.modifier_state & KM_CORE_MODIFIER_ALT || test_event.modifier_state & KM_CORE_MODIFIER_LALT) {
    result.push_back(get_key_event_for_modifier(IBUS_MOD1_MASK, modifiers, GDK_KEY_Alt_L, KEY_LEFTALT));
  }
  if (test_event.modifier_state & KM_CORE_MODIFIER_RALT) {
    if (test_event.vk == KEYMAN_RALT)
      result.push_back(get_key_event_for_modifier(IBUS_MOD1_MASK, modifiers, GDK_KEY_Alt_R, KEY_RIGHTALT));
    else
      result.push_back(get_key_event_for_modifier(IBUS_MOD5_MASK, modifiers, GDK_KEY_Alt_R, KEY_RIGHTALT));
  }
  if (test_event.modifier_state & KM_CORE_MODIFIER_CTRL || test_event.modifier_state & KM_CORE_MODIFIER_LCTRL) {
    result.push_back(get_key_event_for_modifier(IBUS_CONTROL_MASK, modifiers, GDK_KEY_Control_L, KEY_LEFTCTRL));
  }
  if (test_event.modifier_state & KM_CORE_MODIFIER_RCTRL) {
    result.push_back(get_key_event_for_modifier(IBUS_CONTROL_MASK, modifiers, GDK_KEY_Control_R, KEY_RIGHTCTRL));
  }

  // process key
  guint keyval = (test_event.modifier_state & KM_CORE_MODIFIER_SHIFT) ? gdk_keyval_to_upper(test_event.vk)
                                                                      : gdk_keyval_to_lower(test_event.vk);

  // REVIEW: the keyval we use here are not correct for < 0x20 and >= 0x7f
  // See /usr/include/X11/keysymdef.h and /usr/include/ibus-1.0/ibuskeysyms.h
  // Currently this is not a problem because we don't use keyval, but if that ever changes we
  // should be aware that our test behaves different than the real client.

  gdk_key_event event;
  event.modifiers_keydown = modifiers;
  event.modifiers_keyup   = modifiers | IBUS_RELEASE_MASK;
  event.keyval            = keyval;
  event.keycode           = vk_to_keycode(test_event.vk);
  event.is_modifier       = is_modifier(event.keycode);
  result.push_back(event);

  return result;
}

void press_key(IBusKeymanTestsFixture* fixture, km::tests::key_event key_event) {
  auto involved_keys = get_involved_keys(key_event);

  // press and hold the individual modifier keys, finally the actual key
  std::stack<GdkEventKey> keys_to_release;
  for (auto key = involved_keys.begin(); key != involved_keys.end(); key++) {
    bool old_caps_lock  = get_capslock_indicator();
    guint lock_modifier = 0;

    // KEY_CAPSLOCK behaves a bit strange:
    // If capslock is off:
    // - on keypress we turn on capslock, but the modifier is still not set.
    // - The LOCK_MASK modifier gets set on release.
    // When capslock is already on:
    // - the LOCK_MASK modifier is set on both keypress and release
    // - capslock gets turned off on release
    if (key->keycode == KEY_CAPSLOCK && !old_caps_lock) {
      set_capslock_indicator((guint32) true);
      lock_modifier = IBUS_LOCK_MASK;
    }
    GdkEventKey keyEvent = {
        .type             = GDK_KEY_PRESS,
        .window           = window,
        .send_event       = 0,
        .time             = 0,
        .state            = key->modifiers_keydown,
        .keyval           = key->keyval,
        .length           = 0,
        .string           = NULL,
        .hardware_keycode = key->keycode,
        .group            = 0,
        .is_modifier      = key->is_modifier};
    gtk_im_context_filter_keypress(fixture->context, &keyEvent);

    keyEvent.type  = GDK_KEY_RELEASE;
    keyEvent.state = key->modifiers_keyup | lock_modifier;
    keys_to_release.push(keyEvent);

    if (key->keycode == KEY_CAPSLOCK && old_caps_lock) {
      set_capslock_indicator((guint32) false);
    }
  }

  // then release the keys in reverse order
  while (!keys_to_release.empty()) {
    auto keyEvent = keys_to_release.top();
    gtk_im_context_filter_keypress(fixture->context, &keyEvent);
    keys_to_release.pop();
  }
}
