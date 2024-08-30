/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyman Keyboard Processor API - On-Screen Keyboard Layout Interfaces
 */

#pragma once

#include <string>
#include <map>
#include <vector>

#include "keyman_core_api.h"

#if defined(__cplusplus)
extern "C" {
#endif

/**
 * Possible directions of a flick
 */
enum keyboard_layout_flick_direction {
  /** flick up (north) */
  n = 0,
  /** flick down (south) */
  s = 1,
  /** flick right (east) */
  e = 2,
  /** flick left (west) */
  w = 3,
  /** flick up-right (north-east) */
  ne = 4,
  /** flick up-left (north-west) */
  nw = 5,
  /** flick down-right (south-east) */
  se = 6,
  /** flick down-left (south-west) */
  sw = 7
};

/**
 * key type like regular key, framekeys, deadkeys, blank, etc.
 */
enum keyboard_layout_key_type {
  /** regular key */
  normal = 0,
  /** A 'frame' key, such as Shift or Enter */
  special = 1,
  /** A 'frame' key, such as Shift or Enter, which is is active, such as
   * the shift key on a shift layer */
  specialActive = 2,
  /** **KeymanWeb runtime private use:** a variant of `special` with the
   *  keyboard font rather than 'KeymanwebOsk' font */
  customSpecial = 3,
  /** **KeymanWeb runtime private use:** a  variant of `specialActive` with the
   *  keyboard font rather than 'KeymanwebOsk' font. */
  customSpecialActive = 4,
  /** A deadkey */
  deadkey = 8,
  /** A key which is rendered as a blank keycap, should block any interaction */
  blank = 9,
  /** Renders the key only as a gap or spacer, should block any interaction */
  spacer = 10
};

/**
 * A key on a touch layout/on-screen keyboard
 */
struct keyboard_layout_key {
  /** key id */
  std::u16string id;  // ??? perhaps necessary for special keys, Enter, etc? or can we get that from virtualKey?
  /** the virtual key code */
  int virtualKey; // ??? do we need this? both id and virtualKey? Or just one of them?
  /** text to display on key cap */
  std::u16string display;
  /** hint e.g. for longpress */
  std::u16string hint;
  /** the type of key */
  keyboard_layout_key_type type;

  /**
   * the modifier combination (not layer) that should be used in key events,
   * for this key, overriding the layer that the key is a part of.
   */
  int modifiersOverride;
  /** the next layer to switch to after this key is pressed */
  std::u16string nextLayerId;

  // touch layouts only

  /** padding - space to the left of key (in what units?) */
  int gap;
  /** width of the key (in what units?) */
  int width;

  /** longpress keys, also known as subkeys */
  std::vector<keyboard_layout_key> longpresses;
  /** multitaps */
  std::vector<keyboard_layout_key> multiTaps;
  /** flicks */
  std::map<keyboard_layout_flick_direction, keyboard_layout_key> flicks;
};

/**
 * a row of keys on a touch layout/on-screen keyboard
 */
struct keyboard_layout_row {
  /** row id */
  int id; // ??? do we need this? Web has it (`TouchLayoutRow`)
  /** keys in this row */
  std::vector<keyboard_layout_key> keys;
};

/**
 * a layer with rows of keys on a touch layout/on-screen keyboard
 */
struct keyboard_layout_layer {
  /** layer id */
  std::u16string id;
  /** layer modifiers */
  // ??? we added this during our discussion, but Web doesn't have it.
  // Should be an enum if it's needed.
  int modifiers;  //? 0 = default, n = shift, etc. -1 = unspecified?
  /** rows in this layer */
  std::vector<keyboard_layout_row> rows;
};

/**
 * layout specification for a specific platform like desktop, phone or tablet
 */
struct keyboard_layout_platform {
  /** platform form factor, e.g. 'iso', 'touch', 'ansi', ... (see ldml spec) */
  std::u16string form;
  /** width of screen for touch layout */
  int minWidthMm;       // we don't have mobile vs tablet, instead use this
  /** layers for this platform */
  std::vector<keyboard_layout_layer> layers;

  // ??? Do we need these:
  // Web additionally has:
  // - font (should be in CSS; we have it in `keyboard_layout`)
  // - fontsize (should be in CSS; we have it in `keyboard_layout`)
  // - displayUnderlying
  // - defaultHint ("none"|"dot"|"longpress"|"multitap"|"flick"|"flick-n"|"flick-ne"|
  //               "flick-e"|"flick-se"|"flick-s"|"flick-sw"|"flick-w"|"flick-nw")
};

/**
 * On screen keyboard description consisting of specific layouts for different
 * form factors.
 */
struct keyboard_layout {
  /** layouts for different form factors */
  std::vector<keyboard_layout_platform> platforms;
  /** font face name to use for key caps*/
  std::string fontFacename;
  /** font size to use for key caps */
  int fontSizeEm;  // ??? em? px? something else?
};


/**
 * Get the on-screen keyboard layout for the specified keyboard.
 *
 * @param keyboard         [in]  The keyboard to get the layout for.
 * @param layout           [out] The on-screen keyboard layout.
 * @return km_core_status  `KM_CORE_STATUS_OK`: On success.
 *                         `KM_CORE_STATUS_INVALID_ARGUMENT`: If `keyboard` is not a valid keyboard or `layout` is null.
 */
km_core_status
keyboard_get_layout(
  km_core_keyboard const* keyboard,
  keyboard_layout**       layout
);


/**
 * Dispose the on-screen keyboard layout.
 */
void
keyboard_layout_dispose(keyboard_layout* layout);

#if defined(__cplusplus)
}
#endif
