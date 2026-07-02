/**
 * Copyright (C) SIL International. All rights reserved.
 */
package com.tavultesoft.kmapro;

import android.content.Context;
import android.content.res.Configuration;
import android.graphics.drawable.ColorDrawable;
import android.os.Bundle;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.appcompat.app.ActionBar;
import androidx.appcompat.widget.Toolbar;
import androidx.core.content.ContextCompat;

import com.keyman.engine.BaseActivity;
import com.keyman.engine.KMManager;

/**
 * Settings menu for adjusting the keyboard height.
 * The value for the current device orientation is saved via KMManager.
 */
public class AdjustKeyboardHeightActivity extends BaseActivity {
  public static final String adjustKeyboardHeightKey = "AdjustKeyboardHeight";

  private Button resetButton;
  private ImageView sampleKeyboard;
  private TextView percentageDisplay;
  private int currentHeight;
  private ViewGroup.LayoutParams layoutParams;

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);

    setContentView(R.layout.activity_adjust_keyboard_height);

    setupEdgeToEdge(R.id.adjust_keyboard_layout);
    setupStatusBarColors(R.color.keyman_blue, android.R.color.white);

    Toolbar toolbar = findViewById(R.id.titlebar);
    setSupportActionBar(toolbar);
    ActionBar actionBar = getSupportActionBar();
    if (actionBar != null) {
      actionBar.setTitle(null);
      actionBar.setDisplayUseLogoEnabled(false);
      actionBar.setDisplayHomeAsUpEnabled(true);
      actionBar.setDisplayShowHomeEnabled(true);
      actionBar.setDisplayShowTitleEnabled(false);
      actionBar.setDisplayShowCustomEnabled(true);
      actionBar.setBackgroundDrawable(new ColorDrawable(ContextCompat.getColor(this, R.color.keyman_blue)));
    }

    TextView adjustKeyboardHeightActivityTitle = findViewById(R.id.bar_title);
    adjustKeyboardHeightActivityTitle.setTextColor(ContextCompat.getColor(this, R.color.ms_white));
    adjustKeyboardHeightActivityTitle.setText(getString(R.string.adjust_keyboard_height));

    sampleKeyboard = findViewById(R.id.sample_keyboard);
    percentageDisplay = findViewById(R.id.keyboard_height_percentage);
    View resizeHandle = findViewById(R.id.resize_handle);

    layoutParams = sampleKeyboard.getLayoutParams();
    currentHeight = KMManager.getKeyboardHeight(this);
    refreshSampleKeyboard();

    resetButton = findViewById(R.id.reset_to_defaults);
    resetButton.setOnClickListener(v -> {
      KMManager.applyKeyboardHeight(this, KMManager.KeyboardHeight_Reset);
      currentHeight = KMManager.getKeyboardHeight(this);
      refreshSampleKeyboard();
    });

    View.OnTouchListener touchListener = (view, event) -> {
      switch (event.getAction()) {
        case MotionEvent.ACTION_DOWN:
          view.setPressed(true);
          break;
        case MotionEvent.ACTION_MOVE:
          int[] location = new int[2];
          sampleKeyboard.getLocationOnScreen(location);
          int viewBottom = location[1] + sampleKeyboard.getHeight();
          int touchY = (int) event.getRawY();
          currentHeight = calculateKeyboardHeightFromTouch(touchY, viewBottom);
          refreshSampleKeyboard();
          break;
        case MotionEvent.ACTION_UP:
        case MotionEvent.ACTION_CANCEL:
          view.setPressed(false);
          KMManager.applyKeyboardHeight(this, currentHeight);
          break;
      }
      return true;
    };

    resizeHandle.setOnTouchListener(touchListener);
    sampleKeyboard.setOnTouchListener(touchListener);
  }

  /**
   * Refresh the layout for the sample keyboard and update percentage display.
   */
  private void refreshSampleKeyboard() {
    if (layoutParams != null && sampleKeyboard != null) {
      layoutParams.height = currentHeight;
      sampleKeyboard.requestLayout();

      if (percentageDisplay != null) {
        percentageDisplay.setText(createLiveKeyboardHeightString());
      }
    }
  }

  /**
   * Calculate percentage from height and default height.
   * @param height Current height
   * @param defaultHeight Default height
   * @return Percentage rounded to nearest integer
   */
  private static int calculatePercentage(int height, int defaultHeight) {
    if (defaultHeight == 0 || height == 0) {
      return 100;
    }
    return (int) Math.round((double) height / defaultHeight * 100);
  }

  /**
   * Create a string showing the keyboard height as percentages for both orientations.
   * @param context Context
   * @return String in format "100% Portrait | 100% Landscape"
   */
  public static String createKeyboardHeightString(Context context) {
    int portraitHeight = KMManager.getKeyboardHeight(context, Configuration.ORIENTATION_PORTRAIT);
    int landscapeHeight = KMManager.getKeyboardHeight(context, Configuration.ORIENTATION_LANDSCAPE);

    int portraitDefault = KMManager.getDefaultKeyboardHeight(Configuration.ORIENTATION_PORTRAIT);
    int landscapeDefault = KMManager.getDefaultKeyboardHeight(Configuration.ORIENTATION_LANDSCAPE);

    int portraitPercent = calculatePercentage(portraitHeight, portraitDefault);
    int landscapePercent = calculatePercentage(landscapeHeight, landscapeDefault);

    return context.getString(R.string.keyboard_height_format, portraitPercent, landscapePercent);
  }

  /**
   * Calculate keyboard height from touch Y coordinate and clamp 
   * within min-max bounds.
   * @param touchY Touch Y coordinate in screen coordinates
   * @param viewBottom Bottom Y coordinate of keyboard view
   * @return Keyboard height in pixels, clamped to min/max bounds
   */
  private int calculateKeyboardHeightFromTouch(int touchY, int viewBottom) {
    int height = viewBottom - touchY;
    int minKeyboardHeight = KMManager.getKeyboardHeightMin(this);
    int maxKeyboardHeight = KMManager.getKeyboardHeightMax(this);
    return Math.max(minKeyboardHeight, Math.min(maxKeyboardHeight, height));
  }

  /**
   * Create a string showing keyboard height percentages for both orientations,
   * using the current (unsaved) height for the active orientation.
   * @return String in format "100% Portrait | 100% Landscape"
   */
  private String createLiveKeyboardHeightString() {
    int currentOrientation = KMManager.getOrientation(this);

    int portraitDefault = KMManager.getDefaultKeyboardHeight(Configuration.ORIENTATION_PORTRAIT);
    int landscapeDefault = KMManager.getDefaultKeyboardHeight(Configuration.ORIENTATION_LANDSCAPE);

    int portraitHeight;
    int landscapeHeight;
    if (currentOrientation == Configuration.ORIENTATION_PORTRAIT) {
      portraitHeight = currentHeight;
      landscapeHeight = KMManager.getKeyboardHeight(this, Configuration.ORIENTATION_LANDSCAPE);
    } else {
      landscapeHeight = currentHeight;
      portraitHeight = KMManager.getKeyboardHeight(this, Configuration.ORIENTATION_PORTRAIT);
    }

    int portraitPercent = calculatePercentage(portraitHeight, portraitDefault);
    int landscapePercent = calculatePercentage(landscapeHeight, landscapeDefault);

    return getString(R.string.keyboard_height_format, portraitPercent, landscapePercent);
  }

  @Override
  protected void onPause() {
    super.onPause();
    // Ensure height is saved even if user switches tasks or navigates away abnormally
    // This prevents the touch zone from being out of sync with the keyboard visual
    if (currentHeight > 0) {
      KMManager.applyKeyboardHeight(this, currentHeight);
    }
  }

  @Override
  public void onConfigurationChanged(Configuration newConfig) {
    super.onConfigurationChanged(newConfig);

    // IMPORTANT: Apply current height BEFORE switching orientations
    // This ensures the old orientation's height is saved before loading the new one
    if (currentHeight > 0) {
      KMManager.applyKeyboardHeight(this, currentHeight);
    }

    layoutParams = sampleKeyboard.getLayoutParams();

    // When the user rotates the device, restore currentHeight for the new orientation
    currentHeight = KMManager.getKeyboardHeight(this);
    refreshSampleKeyboard();
  }

  @Override
  public boolean onSupportNavigateUp() {
    onBackPressed();
    return true;
  }
}
