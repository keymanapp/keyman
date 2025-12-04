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
 * Settings menu for adjusting the keyboard height. The value for the current device orientation
 * is saved in shared preferences.
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
    refreshSampleKeyboard(this);

    resetButton = findViewById(R.id.reset_to_defaults);
    resetButton.setOnClickListener(new View.OnClickListener() {
      public void onClick(View v) {
        KMManager.applyKeyboardHeight(AdjustKeyboardHeightActivity.this, KMManager.KeyboardHeight_Reset);
        currentHeight = KMManager.getKeyboardHeight(AdjustKeyboardHeightActivity.this);
        refreshSampleKeyboard(AdjustKeyboardHeightActivity.this);
      }
    });

    View.OnTouchListener touchListener = new View.OnTouchListener() {
      @Override
      public boolean onTouch(View view, MotionEvent event) {
        switch (event.getAction()) {
          case MotionEvent.ACTION_DOWN:
            view.setPressed(true);
            break;
          case MotionEvent.ACTION_MOVE:
            int[] location = new int[2];
            sampleKeyboard.getLocationOnScreen(location);
            int viewBottom = location[1] + sampleKeyboard.getHeight();
            int touchY = (int) event.getRawY();
            currentHeight = calculateKeyboardHeightFromTouch(AdjustKeyboardHeightActivity.this, touchY, viewBottom);
            refreshSampleKeyboard(AdjustKeyboardHeightActivity.this);
            break;
          case MotionEvent.ACTION_UP:
          case MotionEvent.ACTION_CANCEL:
            view.setPressed(false);
            KMManager.applyKeyboardHeight(AdjustKeyboardHeightActivity.this, currentHeight);
            break;
        }
        return true;
      }
    };

    resizeHandle.setOnTouchListener(touchListener);
    sampleKeyboard.setOnTouchListener(touchListener);
  }

  /**
   * Refresh the layout for the sample keyboard and update percentage display.
   * @param context Context
   */
  private void refreshSampleKeyboard(Context context) {
    if (layoutParams != null && sampleKeyboard != null) {
      layoutParams.height = currentHeight;
      sampleKeyboard.requestLayout();

      // Update percentage display based on current (unsaved) height
      if (percentageDisplay != null) {
        String percentageText = createLiveKeyboardHeightString(context);
        percentageDisplay.setText(percentageText);
      }
    }
  }

  /**
   * Get keyboard height as percentage of default for current orientation.
   * @param context Context
   * @return Percentage (e.g., 120 for 120%)
   */
  private static int getKeyboardHeightPercentage(Context context) {
    int orientation = KMManager.getOrientation(context);
    return getKeyboardHeightPercentage(context, orientation);
  }

  /**
   * Calculate percentage from height and default height.
   * @param height Current height
   * @param defaultHeight Default height
   * @return Percentage (minimum 100)
   */
  private static int calculatePercentage(int height, int defaultHeight) {
    if (defaultHeight == 0 || height == 0) {
      return 100;
    }
    int percent = (int) Math.ceil((double) height / defaultHeight * 100);
    return Math.max(percent, 100);
  }

  /**
   * Get keyboard height as percentage of default for specified orientation.
   * @param context Context
   * @param orientation Configuration.ORIENTATION_PORTRAIT or Configuration.ORIENTATION_LANDSCAPE
   * @return Percentage (e.g., 120 for 120%)
   */
  private static int getKeyboardHeightPercentage(Context context, int orientation) {
    int currentHeight = KMManager.getKeyboardHeight(context, orientation);
    int defaultHeight = KMManager.getDefaultKeyboardHeight(orientation);
    return calculatePercentage(currentHeight, defaultHeight);
  }

  /**
   * Create a string showing the keyboard height as percentages for both orientations.
   * @param context Context
   * @return String in format "100% Portrait | 100% Landscape"
   */
  public static String createKeyboardHeightString(Context context) {
    android.content.SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);

    int landscapeDefault = KMManager.getDefaultKeyboardHeight(Configuration.ORIENTATION_LANDSCAPE);
    int portraitDefault = KMManager.getDefaultKeyboardHeight(Configuration.ORIENTATION_PORTRAIT);

    int landscapeHeight = prefs.getInt(KMManager.KMKey_KeyboardHeightLandscape, landscapeDefault);
    int portraitHeight = prefs.getInt(KMManager.KMKey_KeyboardHeightPortrait, portraitDefault);

    int landscapePercent = calculatePercentage(landscapeHeight, landscapeDefault);
    int portraitPercent = calculatePercentage(portraitHeight, portraitDefault);

    return context.getString(R.string.keyboard_height_format, portraitPercent, landscapePercent);
  }

  /**
   * Calculate keyboard height from touch Y coordinate.
   * @param context Context
   * @param touchY Touch Y coordinate in screen coordinates
   * @param viewBottom Bottom Y coordinate of keyboard view
   * @return Keyboard height in pixels, clamped to min/max bounds
   */
  private static int calculateKeyboardHeightFromTouch(Context context, int touchY, int viewBottom) {
    int height = viewBottom - touchY;
    int minKeyboardHeight = KMManager.getKeyboardHeightMin(context);
    int maxKeyboardHeight = KMManager.getKeyboardHeightMax(context);
    return Math.max(minKeyboardHeight, Math.min(maxKeyboardHeight, height));
  }

  /**
   * Create a string showing keyboard height percentages for both orientations,
   * using the current (unsaved) height for the active orientation.
   * @param context Context
   * @return String in format "100% Portrait | 100% Landscape"
   */
  private String createLiveKeyboardHeightString(Context context) {
    int currentOrientation = KMManager.getOrientation(context);
    int liveDefaultHeight = KMManager.getDefaultKeyboardHeight(currentOrientation);
    int livePercent = calculatePercentage(currentHeight, liveDefaultHeight);

    int portraitPercent;
    int landscapePercent;
    if (currentOrientation == Configuration.ORIENTATION_PORTRAIT) {
      portraitPercent = livePercent;
      landscapePercent = getKeyboardHeightPercentage(context, Configuration.ORIENTATION_LANDSCAPE);
    } else {
      landscapePercent = livePercent;
      portraitPercent = getKeyboardHeightPercentage(context, Configuration.ORIENTATION_PORTRAIT);
    }

    return context.getString(R.string.keyboard_height_format, portraitPercent, landscapePercent);
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
    refreshSampleKeyboard(this);
  }

  @Override
  public boolean onSupportNavigateUp() {
    onBackPressed();
    return true;
  }
}
