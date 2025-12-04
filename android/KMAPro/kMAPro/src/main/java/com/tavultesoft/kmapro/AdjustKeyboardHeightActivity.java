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
            currentHeight = calculateKeyboardHeightFromTouch(context, touchY, viewBottom);

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
   * Get keyboard height as percentage of default for current orientation
   * @param context Context
   * @return Percentage (e.g., 120 for 120%)
   */
  private static int getKeyboardHeightPercentage(Context context) {
    int orientation = KMManager.getOrientation(context);
    return getKeyboardHeightPercentage(context, orientation);
  }

  /**
   * Get keyboard height as percentage of default for specified orientation.
   *
   * <p>The percentage is calculated using Math.ceil() to round up to the nearest integer.
   * This ensures consistency and prevents percentages from being displayed as 0%.
   *
   * <p>Example: If current height is 288px and default is 200px:
   * <pre>
   * percentage = ceil(288 * 100.0 / 200) = ceil(144.0) = 144%
   * </pre>
   *
   * @param context Context
   * @param orientation Configuration.ORIENTATION_PORTRAIT or Configuration.ORIENTATION_LANDSCAPE
   * @return Percentage (e.g., 120 for 120%), minimum value is 100
   */
  private static int getKeyboardHeightPercentage(Context context, int orientation) {
    int currentHeight = KMManager.getKeyboardHeight(context, orientation);
    int defaultHeight = KMManager.getDefaultKeyboardHeight(orientation);
    if (defaultHeight == 0) return 100;
    // Use Math.ceil to match createKeyboardHeightString() calculation
    int percent = (int) Math.ceil((currentHeight * 100.0) / defaultHeight);
    if (percent == 0) {
      percent = 100;
    }
    return percent;
  }

  /**
   * Create a string showing the keyboard height as percentages for both orientations.
   * This method reads directly from SharedPreferences to ensure accurate percentages,
   * especially after app reinstall or when the static context variables may be stale.
   *
   * @param context Context
   * @param portraitLabel Label for portrait (e.g., "Portrait")
   * @param landscapeLabel Label for landscape (e.g., "Landscape")
   * @return String in format "100% Portrait | 100% Landscape"
   */
  public static String createKeyboardHeightString(Context context, String portraitLabel, String landscapeLabel) {
    Integer landscapePercent = 100;
    Integer portraitPercent = 100;

    // Read the current keyboard heights directly from SharedPreferences
    // This ensures we get the actual stored values, not the potentially stale static variables
    android.content.SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);

    // Get default heights (these should be set during KMManager initialization)
    int landscapeDefault = KMManager.KeyboardHeight_Context_Landscape_Default;
    int portraitDefault = KMManager.KeyboardHeight_Context_Portrait_Default;

    // Read current heights from SharedPreferences, using defaults as fallback
    int landscapeHeight = prefs.getInt(KMManager.KMKey_KeyboardHeightLandscape, landscapeDefault);
    int portraitHeight = prefs.getInt(KMManager.KMKey_KeyboardHeightPortrait, portraitDefault);

    // Calculate landscape percentage
    if (landscapeDefault != 0 && landscapeHeight > 0) {
      double landscapeBase = landscapeDefault * 1.0;
      double landscapeFactor = ((double) landscapeHeight / landscapeBase);
      landscapePercent = (int) Math.ceil(landscapeFactor * 100);
      if (landscapePercent == 0) {
        landscapePercent = 100;
      }
    }

    // Calculate portrait percentage
    if (portraitDefault != 0 && portraitHeight > 0) {
      double portraitBase = portraitDefault * 1.0;
      double portraitFactor = ((double) portraitHeight / portraitBase);
      portraitPercent = (int) Math.ceil(portraitFactor * 100);
      if (portraitPercent == 0) {
        portraitPercent = 100;
      }
    }

    String percentages = portraitPercent.toString() + "% " + portraitLabel + " | " +
                        landscapePercent.toString() + "% " + landscapeLabel;
    return percentages;
  }

  /**
   * Create a string showing keyboard height percentages for both orientations.
   * Uses a live (unsaved) height for the specified orientation and reads saved height for the other.
   * This is useful for displaying real-time percentages during keyboard height adjustment.
   *
   * <p>The percentage is calculated using Math.ceil() to ensure consistency.
   *
   * <p>Example usage during drag operation:
   * <pre>{@code
   * // User is dragging to adjust portrait height
   * int currentOrientation = KMManager.getOrientation(context);
   * String display = createKeyboardHeightString(
   *     context,
   *     currentHeight,  // Live height being adjusted
   *     currentOrientation,
   *     "Portrait",
   *     "Landscape"
   * );
   * // Result: "145% Portrait | 100% Landscape"
   * }</pre>
   *
   * @param context Context
   * @param liveHeight The current keyboard height in pixels (not yet saved to SharedPreferences)
   * @param liveOrientation The orientation for which liveHeight applies
   *                        (Configuration.ORIENTATION_PORTRAIT or Configuration.ORIENTATION_LANDSCAPE)
   * @param portraitLabel Label for portrait orientation (e.g., "Portrait")
   * @param landscapeLabel Label for landscape orientation (e.g., "Landscape")
   * @return String in format "100% Portrait | 100% Landscape"
   */
  private static String createKeyboardHeightString(Context context, int liveHeight, int liveOrientation,
                                                   String portraitLabel, String landscapeLabel) {
    int portraitPercent = 100;
    int landscapePercent = 100;

    // Calculate percentage for the live orientation
    int liveDefaultHeight = KMManager.getDefaultKeyboardHeight(liveOrientation);
    if (liveDefaultHeight > 0 && liveHeight > 0) {
      int livePercent = (int) Math.ceil((liveHeight * 100.0) / liveDefaultHeight);
      if (livePercent == 0) {
        livePercent = 100;
      }

      if (liveOrientation == Configuration.ORIENTATION_PORTRAIT) {
        portraitPercent = livePercent;
        landscapePercent = getKeyboardHeightPercentage(context, Configuration.ORIENTATION_LANDSCAPE);
      } else {
        landscapePercent = livePercent;
        portraitPercent = getKeyboardHeightPercentage(context, Configuration.ORIENTATION_PORTRAIT);
      }
    } else {
      // Fallback: read both from saved preferences
      portraitPercent = getKeyboardHeightPercentage(context, Configuration.ORIENTATION_PORTRAIT);
      landscapePercent = getKeyboardHeightPercentage(context, Configuration.ORIENTATION_LANDSCAPE);
    }

    return portraitPercent + "% " + portraitLabel + " | " +
           landscapePercent + "% " + landscapeLabel;
  }

  /**
   * Calculate keyboard height from a touch Y coordinate, applying min/max bounds.
   * This is used during interactive keyboard height adjustment (e.g., dragging a resize handle).
   *
   * <p>The height is calculated as the distance from the touch point to the bottom of the screen,
   * then clamped to the valid range defined by KMManager.getKeyboardHeightMin and
   * KMManager.getKeyboardHeightMax.
   *
   * <p>Example usage in touch listener:
   * <pre>{@code
   * case MotionEvent.ACTION_MOVE:
   *     int[] location = new int[2];
   *     keyboardView.getLocationOnScreen(location);
   *     int viewBottom = location[1] + keyboardView.getHeight();
   *     int touchY = (int) event.getRawY();
   *
   *     int newHeight = calculateKeyboardHeightFromTouch(
   *         context, touchY, viewBottom);
   *     // newHeight is guaranteed to be within min/max bounds
   *     break;
   * }</pre>
   *
   * @param context Context
   * @param touchY The Y coordinate of the touch event in screen coordinates (from event.getRawY())
   * @param viewBottom The bottom Y coordinate of the keyboard view in screen coordinates
   *                   (typically: view.getLocationOnScreen()[1] + view.getHeight())
   * @return The calculated keyboard height in pixels, clamped to valid range
   */
  private static int calculateKeyboardHeightFromTouch(Context context, int touchY, int viewBottom) {
    // Calculate height: distance from touch point to bottom of screen
    int height = viewBottom - touchY;

    // Apply lower and upper bounds
    int minKeyboardHeight = KMManager.getKeyboardHeightMin(context);
    int maxKeyboardHeight = KMManager.getKeyboardHeightMax(context);
    height = Math.max(minKeyboardHeight, height);
    height = Math.min(maxKeyboardHeight, height);

    return height;
  }

  /**
   * Create a string showing keyboard height percentages for both orientations.
   * Uses the current (unsaved) height for the active orientation and saved height for the other.
   * @param context Context
   * @return String in format "100% Portrait | 100% Landscape"
   */
  private String createLiveKeyboardHeightString(Context context) {
    int currentOrientation = KMManager.getOrientation(context);
    return createKeyboardHeightString(
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
