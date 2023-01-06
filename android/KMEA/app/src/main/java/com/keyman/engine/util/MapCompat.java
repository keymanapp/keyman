package com.keyman.engine.util;

import android.os.Build;

import java.util.HashMap;
import java.lang.NullPointerException;

/**
 * HashMap.getOrDefault is only available for Android API 24+, so this method is an alternative
 * for older devices.
 * Reference: https://stackoverflow.com/questions/41211960/alternative-to-getordefault-for-devices-below-api-24-android
 */
public final class MapCompat {
  private static final String TAG = "MapCompat";

  /**
   *
   * @param map HashMap<String, String>
   * @param key String
   * @param defaultValue String
   * @return String
   * @throws NullPointerException
   */
  public static <K, V> V getOrDefault(HashMap<K, V> map, K key, V defaultValue) throws NullPointerException {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N) {
      return map.getOrDefault(key, defaultValue);
    }

    V v;
    return (((v = map.get(key)) != null) || map.containsKey(key)) ? v : defaultValue;
  }

}