/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.keyman.engine;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.keyman.engine.KMManager.KeyboardType;

public final class KeyboardEventHandler {

  public enum EventType {
    KEYBOARD_LOADED,
    KEYBOARD_CHANGED,
    KEYBOARD_SHOWN,
    KEYBOARD_DISMISSED,
    KEYBOARD_DOWNLOAD_STARTED,
    KEYBOARD_DOWNLOAD_FINISHED,
    PACKAGE_INSTALLED,
    LEXICAL_MODEL_INSTALLED;
  }

  public static void notifyListeners(ArrayList<OnKeyboardEventListener> listeners, KeyboardType keyboardType, EventType event, String newValue) {
    if (listeners != null) {
      @SuppressWarnings("unchecked")
      // make a copy of the list to avoid concurrent modification while iterating
        ArrayList<OnKeyboardEventListener> _listeners = (ArrayList<OnKeyboardEventListener>) listeners.clone();
      if (event == EventType.KEYBOARD_LOADED) {
        for (OnKeyboardEventListener listener : _listeners)
          listener.onKeyboardLoaded(keyboardType);
      } else if (event == EventType.KEYBOARD_CHANGED) {
        for (OnKeyboardEventListener listener : _listeners)
          listener.onKeyboardChanged(newValue);
      } else if (event == EventType.KEYBOARD_SHOWN) {
        for (OnKeyboardEventListener listener : _listeners)
          listener.onKeyboardShown();
      } else if (event == EventType.KEYBOARD_DISMISSED) {
        for (OnKeyboardEventListener listener : _listeners)
          listener.onKeyboardDismissed();
      }
    }
  }

  public static void notifyListeners(ArrayList<OnKeyboardDownloadEventListener> listeners,
                                     EventType event, HashMap<String, String> keyboardInfo, int result) {
    if (listeners != null) {
      @SuppressWarnings("unchecked")
      // make a copy of the list to avoid concurrent modification while iterating
      ArrayList<OnKeyboardDownloadEventListener> _listeners =
        (ArrayList<OnKeyboardDownloadEventListener>) listeners.clone();
      if (event == EventType.KEYBOARD_DOWNLOAD_STARTED) {
        for (OnKeyboardDownloadEventListener listener : _listeners)
          listener.onKeyboardDownloadStarted(keyboardInfo);
      } else if (event == EventType.KEYBOARD_DOWNLOAD_FINISHED) {
        for (OnKeyboardDownloadEventListener listener : _listeners)
          listener.onKeyboardDownloadFinished(keyboardInfo, result);
      }
    }
  }

  public static void notifyListeners(ArrayList<OnKeyboardDownloadEventListener> listeners,
                                     EventType event, List<Map<String, String>> info, int result) {
    if (listeners != null) {
      @SuppressWarnings("unchecked")
      // make a copy of the list to avoid concurrent modification while iterating
      ArrayList<OnKeyboardDownloadEventListener> _listeners =
        (ArrayList<OnKeyboardDownloadEventListener>) listeners.clone();
      if (event == EventType.PACKAGE_INSTALLED) {
        for (OnKeyboardDownloadEventListener listener : _listeners) {
          listener.onPackageInstalled(info);
        }
      } else if (event == EventType.LEXICAL_MODEL_INSTALLED) {
        for (OnKeyboardDownloadEventListener listener : _listeners) {
          listener.onLexicalModelInstalled(info);
        }

      }
    }
  }


  public interface OnKeyboardEventListener {
    void onKeyboardLoaded(KeyboardType keyboardType);

    void onKeyboardChanged(String newKeyboard); // newKeyboard string format: languageID_keyboardID e.g. eng_us

    void onKeyboardShown();

    void onKeyboardDismissed();
  }

  public interface OnKeyboardDownloadEventListener {
    void onKeyboardDownloadStarted(HashMap<String, String> keyboardInfo);

    void onKeyboardDownloadFinished(HashMap<String, String> keyboardInfo, int result); // result > 0 if successful, < 0 if failed

    void onPackageInstalled(List<Map<String, String>> keyboardsInstalled);

    void onLexicalModelInstalled(List<Map<String, String>> lexicalModelsInstalled);
  }
}
