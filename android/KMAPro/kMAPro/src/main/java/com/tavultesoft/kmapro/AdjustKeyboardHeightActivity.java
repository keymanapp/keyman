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
  private static final String TAG = "AdjustKbdHeight";
  public static final String adjustKeyboardHeightKey = "AdjustKeyboardHeight";

  private static Button resetButton = null;
  private static ImageView sampleKeyboard = null;
  private static TextView percentageDisplay = null;

  // Keeps track of the adjusted keyboard height for saving
  private static int currentHeight = 0;
  private static ViewGroup.LayoutParams layoutParams;

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);

    final Context context = this;

    setContentView(R.layout.activity_adjust_keyboard_height);

    setupEdgeToEdge(R.id.adjust_keyboard_layout);
    setupStatusBarColors(R.color.keyman_blue, android.R.color.white);

    Toolbar toolbar = (Toolbar) findViewById(R.id.titlebar);
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

    TextView adjustKeyboardHeightActivityTitle = (TextView) findViewById(R.id.bar_title);

    String titleStr = getString(R.string.adjust_keyboard_height);
    adjustKeyboardHeightActivityTitle.setTextColor(ContextCompat.getColor(this, R.color.ms_white));
    adjustKeyboardHeightActivityTitle.setText(titleStr);

    sampleKeyboard = (ImageView) findViewById(R.id.sample_keyboard);
    percentageDisplay = (TextView) findViewById(R.id.keyboard_height_percentage);
    View resizeHandle = findViewById(R.id.resize_handle);

    layoutParams = sampleKeyboard.getLayoutParams();
    currentHeight = KMManager.getKeyboardHeight(context);
    refreshSampleKeyboard(context);

    resetButton = (Button) findViewById(R.id.reset_to_defaults);
    resetButton.setOnClickListener(new View.OnClickListener() {
      public void onClick(View v) {
        // Clear the keyboard height preferences to reset
        KMManager.applyKeyboardHeight(context, KMManager.KeyboardHeight_Reset);

        // Restore default height
        currentHeight = KMManager.getKeyboardHeight(context);
        refreshSampleKeyboard(context);
      }
    });

    // Set up touch listener on both the resize handle and keyboard container
    View.OnTouchListener touchListener = new View.OnTouchListener() {
      @Override
      public boolean onTouch(View view, MotionEvent event) {
        switch (event.getAction()) {
          case MotionEvent.ACTION_DOWN:
            view.setPressed(true);
            break;
          case MotionEvent.ACTION_MOVE:
            // Get the touch position relative to the screen bottom
            int[] location = new int[2];
            sampleKeyboard.getLocationOnScreen(location);
            int viewBottom = location[1] + sampleKeyboard.getHeight();
            int touchY = (int) event.getRawY();

            // Calculate height with bounds checking
            currentHeight = KMManager.calculateKeyboardHeightFromTouch(context, touchY, viewBottom);

            refreshSampleKeyboard(context);
            break;
          case MotionEvent.ACTION_UP:
          case MotionEvent.ACTION_CANCEL:
            view.setPressed(false);
            // Save the currentHeight when the user releases
            KMManager.applyKeyboardHeight(context, currentHeight);
            break;
        }
        return true;
      }
    };

    // Apply touch listener to both the resize handle and keyboard container
    // This allows dragging from anywhere in the keyboard preview area
    resizeHandle.setOnTouchListener(touchListener);
    sampleKeyboard.setOnTouchListener(touchListener);
  }

  /**
   * Refresh the layout for the sample keyboard and update percentage display
   * @param context
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
   * Create a string showing keyboard height percentages for both orientations.
   * Uses the current (unsaved) height for the active orientation and saved height for the other.
   * @param context Context
   * @return String in format "100% Portrait | 100% Landscape"
   */
  private String createLiveKeyboardHeightString(Context context) {
    int currentOrientation = KMManager.getOrientation(context);
    return KMManager.createKeyboardHeightString(
        context,
        currentHeight,
        currentOrientation,
        getString(R.string.portrait),
        getString(R.string.landscape)
    );
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
