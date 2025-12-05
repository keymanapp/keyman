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

    sampleKeyboard.setOnTouchListener(new View.OnTouchListener() {
      @Override
      public boolean onTouch(View view, MotionEvent event) {
        int y = (int)event.getY();

        switch(event.getAction()) {
          case MotionEvent.ACTION_MOVE:
            // Update currentHeight as the user drags (moves)
            // Increasing the keyboard height is a negative y
            currentHeight -= y;

            // Apply lower and upper bounds on currentHeight
            int minKeyboardHeight = KMManager.getKeyboardHeightMin(context);
            int maxKeyboardHeight = KMManager.getKeyboardHeightMax(context);
            currentHeight = Math.max(minKeyboardHeight, currentHeight);
            currentHeight = Math.min(maxKeyboardHeight, currentHeight);

            refreshSampleKeyboard(context);
            break;
          case MotionEvent.ACTION_UP:
            // Save the currentHeight when the user releases
            KMManager.applyKeyboardHeight(context, currentHeight);
            break;
        }
        return true;
      }
    });
  }

  /**
   * Refresh the layout for the sample keyboard
   * @param context
   */
  private void refreshSampleKeyboard(Context context) {
    layoutParams.height = currentHeight;
    sampleKeyboard.setLayoutParams(layoutParams);
  }

  @Override
  public void onConfigurationChanged(Configuration newConfig) {
    super.onConfigurationChanged(newConfig);

    layoutParams = sampleKeyboard.getLayoutParams();

    // When the user rotates the device, restore currentHeight
    currentHeight = KMManager.getKeyboardHeight(this);
    refreshSampleKeyboard(this);
  }

  @Override
  public boolean onSupportNavigateUp() {
    onBackPressed();
    return true;
  }
}
