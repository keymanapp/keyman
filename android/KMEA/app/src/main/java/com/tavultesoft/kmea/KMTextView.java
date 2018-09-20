/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import java.util.ArrayList;

import com.tavultesoft.kmea.KMManager.KeyboardType;
import com.tavultesoft.kmea.KeyboardEventHandler.EventType;
import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardEventListener;

import android.app.Activity;
import android.content.ContextWrapper;
import android.view.ContextThemeWrapper;
import android.support.v7.app.AppCompatActivity;
import android.content.Context;
import android.graphics.Color;
import android.graphics.Typeface;
import android.util.AttributeSet;
import android.util.Log;
import android.view.KeyEvent;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewGroup.OnHierarchyChangeListener;
import android.view.Window;
import android.view.WindowManager;
import android.view.inputmethod.InputMethodManager;
import android.support.v7.widget.AppCompatEditText;
import android.widget.FrameLayout;
import android.widget.RelativeLayout;

public final class KMTextView extends AppCompatEditText {
  private Context context;
  protected KMHardwareKeyboardInterpreter hardwareKeyboardInterpreter;

  private static boolean keyboardVisible = false;
  private static RelativeLayout keyboardLayout = null;
  private static OnHierarchyChangeListener hcListener = null;
  protected static View activeView = null;
  protected static ArrayList<OnKeyboardEventListener> kbEventListeners = null;

  private boolean _blockEventProcessing = false;

  public KMTextView(Context context) {
    super(context);
    init(context);
  }

  public KMTextView(Context context, AttributeSet attrs) {
    super(context, attrs);
    init(context);
  }

  public KMTextView(Context context, AttributeSet attrs, int defStyle) {
    super(context, attrs, defStyle);
    init(context);
  }

  private void init(final Context context) {
    this.context = context;
    this.hardwareKeyboardInterpreter = new KMHardwareKeyboardInterpreter(context, KeyboardType.KEYBOARD_TYPE_INAPP);

    AppCompatActivity activity = (AppCompatActivity)context;

    Window mainWindow = activity.getWindow();
    mainWindow.setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_ALWAYS_HIDDEN);
    FrameLayout mainLayout = (FrameLayout) mainWindow.getDecorView().findViewById(android.R.id.content);

    if (hcListener == null) {
      hcListener = new OnHierarchyChangeListener() {

        @Override
        public void onChildViewAdded(View parent, View child) {
          if (keyboardLayout != null && !child.equals(keyboardLayout)) {
            ViewGroup p = (ViewGroup) keyboardLayout.getParent();
            if (p != null && parent.equals(p)) {
              p.bringChildToFront(keyboardLayout);
              p.requestLayout();
              p.invalidate();
            }
          }
        }

        @Override
        public void onChildViewRemoved(View parent, View child) {

        }
      };
    }
    mainLayout.setOnHierarchyChangeListener(hcListener);

    if (keyboardLayout == null) {
      keyboardLayout = new RelativeLayout(context.getApplicationContext());
      keyboardLayout.setLayoutParams(new FrameLayout.LayoutParams(FrameLayout.LayoutParams.MATCH_PARENT, FrameLayout.LayoutParams.MATCH_PARENT));
      keyboardLayout.setBackgroundColor(Color.TRANSPARENT);
      keyboardLayout.setVisibility(View.GONE);
      keyboardLayout.setEnabled(false);
    }

    if (KMManager.InAppKeyboard != null && KMManager.InAppKeyboard.getParent() == null) {
      keyboardLayout.addView(KMManager.InAppKeyboard);
    }

    setOnFocusChangeListener(new OnFocusChangeListener() {
      @Override
      public void onFocusChange(View v, boolean hasFocus) {
        if (hasFocus) {
          activeView = v;
          if (KMManager.InAppKeyboardLoaded) {
            KMTextView textView = (KMTextView) activeView;
            int selStart = textView.getSelectionStart();
            int selEnd = textView.getSelectionEnd();
            KMManager.updateText(KeyboardType.KEYBOARD_TYPE_INAPP, textView.getText().toString());
            KMManager.updateSelectionRange(KeyboardType.KEYBOARD_TYPE_INAPP, selStart, selEnd);
            KMManager.resetContext(KeyboardType.KEYBOARD_TYPE_INAPP);
          }
          showKeyboard();
        } else {
          activeView = null;
          dismissKeyboard();
        }
      }
    });

    setOnClickListener(new OnClickListener() {
      @Override
      public void onClick(View v) {
        if (!isKeyboardVisible())
          showKeyboard();
      }
    });

    /*
    // Disable standard keyboard hard way
    // NOTE There is also an easy way: 'edittext.setInputType(InputType.TYPE_NULL)' (but you will not have a cursor, and no 'edittext.setCursorVisible(true)' doesn't work )
    setOnTouchListener(new OnTouchListener() {
      @Override
      public boolean onTouch(View v, MotionEvent event) {
                KMTextView textView = (KMTextView) v;
                int inType = textView.getInputType();       // Backup the input type
                textView.setInputType(InputType.TYPE_NULL); // Disable standard keyboard
                textView.onTouchEvent(event);               // Call native handler
                textView.setInputType(inType);              // Restore input type
        return true;
      }
    });*/
  }

  @Override
  protected void onTextChanged(CharSequence text, int start, int lengthBefore, int lengthAfter) {
    if (activeView != null && activeView.equals(this)) {
      KMManager.updateText(KMManager.KeyboardType.KEYBOARD_TYPE_INAPP, text.toString());
    }
  }

  @Override
  protected void onSelectionChanged(int selStart, int selEnd) {
    if (activeView != null && activeView.equals(this)) {
      KMManager.updateSelectionRange(KMManager.KeyboardType.KEYBOARD_TYPE_INAPP, selStart, selEnd);
    }
  }

  @Override
  public void onWindowFocusChanged(boolean hasWindowFocus) {
    AppCompatActivity activity = (AppCompatActivity)context;

    Window mainWindow = activity.getWindow();
    if (hasWindowFocus) {
      KMManager.KMInAppKeyboardWebViewClient.context = context;
      activeView = mainWindow.getCurrentFocus();

      Typeface font = KMManager.getFontTypeface(context, KMKeyboard.textFontFilename());
      if (font != null) {
        KMTextView.this.setTypeface(font);
      } else {
        KMTextView.this.setTypeface(Typeface.SANS_SERIF);
      }

      if (activeView != null && activeView.equals(this)) {
        if (KMManager.InAppKeyboardLoaded) {
          KMTextView textView = (KMTextView) activeView;
          int selStart = textView.getSelectionStart();
          int selEnd = textView.getSelectionEnd();
          KMManager.updateText(KeyboardType.KEYBOARD_TYPE_INAPP, textView.getText().toString());
          KMManager.updateSelectionRange(KeyboardType.KEYBOARD_TYPE_INAPP, selStart, selEnd);
        }

        if (keyboardVisible) {
          showKeyboard();
        }
      }
    }
  }

  protected void keyDownUp(int keyEventCode) {
    // Note - this flag and event blocking mechanism aren't threadsafe.
    _blockEventProcessing = true;
    try {
      dispatchKeyEvent(new KeyEvent(KeyEvent.ACTION_DOWN, keyEventCode));
      dispatchKeyEvent(new KeyEvent(KeyEvent.ACTION_UP, keyEventCode));
    } catch (Exception e) {
      Log.e("KMEA Error:", e.toString());
    } finally {
      _blockEventProcessing = false;
    }
  }

  @Override
  public boolean onKeyDown(int keyCode, KeyEvent event) {
    if (_blockEventProcessing) {
      return super.onKeyDown(keyCode, event);
    }

    if (!hardwareKeyboardInterpreter.onKeyDown(keyCode, event) && !_blockEventProcessing) {
      return super.onKeyDown(keyCode, event);
    } else {
      return true;
    }
  }

  @Override
  public boolean onKeyUp(int keyCode, KeyEvent event) {
    if (_blockEventProcessing) {
      return super.onKeyUp(keyCode, event);
    }

    if (!hardwareKeyboardInterpreter.onKeyUp(keyCode, event)) {
      return super.onKeyUp(keyCode, event);
    } else {
      return true;
    }
  }

  @Override
  public boolean onKeyMultiple(int keyCode, int count, KeyEvent event) {
    if (_blockEventProcessing) {
      return super.onKeyMultiple(keyCode, count, event);
    }

    if (!hardwareKeyboardInterpreter.onKeyMultiple(keyCode, count, event)) {
      return super.onKeyMultiple(keyCode, count, event);
    } else {
      return true;
    }
  }

  @Override
  public boolean onKeyLongPress(int keyCode, KeyEvent event) {
    if (_blockEventProcessing) {
      return super.onKeyLongPress(keyCode, event);
    }

    if (!hardwareKeyboardInterpreter.onKeyLongPress(keyCode, event)) {
      return super.onKeyLongPress(keyCode, event);
    } else {
      return true;
    }
  }

  private void showKeyboard() {
    AppCompatActivity activity = (AppCompatActivity)context;

    Window mainWindow = activity.getWindow();
    FrameLayout mainLayout = (FrameLayout) mainWindow.getDecorView().findViewById(android.R.id.content);

    mainWindow.setFlags(WindowManager.LayoutParams.FLAG_ALT_FOCUSABLE_IM, WindowManager.LayoutParams.FLAG_ALT_FOCUSABLE_IM);
    ((InputMethodManager) activity.getSystemService(AppCompatActivity.INPUT_METHOD_SERVICE)).hideSoftInputFromWindow(mainLayout.getWindowToken(), 0);

    ViewGroup parent = (ViewGroup) keyboardLayout.getParent();
    if (parent != null) {
      parent.removeView(keyboardLayout);
    }

    mainLayout.addView(keyboardLayout);

    //keyboardLayout.setAnimation(AnimationUtils.loadAnimation(context, R.anim.slide_in));
    //keyboardLayout.setAnimation(null);
    keyboardLayout.setVisibility(View.VISIBLE);
    keyboardLayout.setEnabled(true);
    keyboardVisible = true;

    mainLayout.bringChildToFront(keyboardLayout);
    mainLayout.requestLayout();
    mainLayout.invalidate();

    KeyboardEventHandler.notifyListeners(kbEventListeners, KeyboardType.KEYBOARD_TYPE_INAPP, EventType.KEYBOARD_SHOWN, null);
  }

  public void dismissKeyboard() {
    AppCompatActivity activity;
    if (context instanceof ContextThemeWrapper) {
      activity = (AppCompatActivity)(((ContextThemeWrapper)context).getBaseContext());
    } else {
      activity = (AppCompatActivity)context;
    }

    Window mainWindow = activity.getWindow();
    FrameLayout mainLayout = (FrameLayout) mainWindow.getDecorView().findViewById(android.R.id.content);
    //mainWindow.clearFlags(WindowManager.LayoutParams.FLAG_ALT_FOCUSABLE_IM);
    ((InputMethodManager) activity.getSystemService(AppCompatActivity.INPUT_METHOD_SERVICE)).hideSoftInputFromWindow(mainLayout.getWindowToken(), 0);
    //keyboardLayout.setAnimation(AnimationUtils.loadAnimation(context, R.anim.slide_out));
    //keyboardLayout.setAnimation(null);
    keyboardLayout.setVisibility(View.GONE);
    keyboardLayout.setEnabled(false);
    keyboardVisible = false;

    KeyboardEventHandler.notifyListeners(kbEventListeners, KeyboardType.KEYBOARD_TYPE_INAPP, EventType.KEYBOARD_DISMISSED, null);
  }

  protected static void addOnKeyboardEventListener(OnKeyboardEventListener listener) {
    if (kbEventListeners == null)
      kbEventListeners = new ArrayList<OnKeyboardEventListener>();

    if (listener != null && !kbEventListeners.contains(listener)) {
      kbEventListeners.add(listener);
    }
  }

  protected static void removeOnKeyboardEventListener(OnKeyboardEventListener listener) {
    if (kbEventListeners != null) {
      kbEventListeners.remove(listener);
    }
  }

  public boolean isKeyboardVisible() {
    return keyboardVisible;
  }
}