/**
 * Copyright (C) SIL International. All rights reserved.
 */
package com.tavultesoft.kmapro;

import android.content.Context;
import android.content.SharedPreferences;
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

import com.tavultesoft.kmea.BaseActivity;
import com.tavultesoft.kmea.KMManager;

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
  private static SharedPreferences.Editor editor = null;
  private static int currentHeight = 0;
  private static ViewGroup.LayoutParams layoutParams;

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);

    final Context context = this;

    setContentView(R.layout.activity_adjust_keyboard_height);

    Toolbar toolbar = (Toolbar) findViewById(R.id.titlebar);
    setSupportActionBar(toolbar);
    ActionBar actionBar = getSupportActionBar();
    if (actionBar != null) {
      actionBar.setTitle(null);
      actionBar.setDisplayUseLogoEnabled(false);
      actionBar.setDisplayShowHomeEnabled(false);
      actionBar.setDisplayShowTitleEnabled(false);
      actionBar.setDisplayShowCustomEnabled(true);
      actionBar.setBackgroundDrawable(new ColorDrawable(ContextCompat.getColor(this, R.color.keyman_blue)));
    }

    TextView adjustKeyboardHeightActivityTitle = (TextView) findViewById(R.id.bar_title);
    adjustKeyboardHeightActivityTitle.setWidth((int) getResources().getDimension(R.dimen.package_label_width));

    String titleStr = getString(R.string.adjust_keyboard_height);
    adjustKeyboardHeightActivityTitle.setTextColor(ContextCompat.getColor(this, R.color.ms_white));
    adjustKeyboardHeightActivityTitle.setText(titleStr);

    SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    editor = prefs.edit();

    sampleKeyboard = (ImageView) findViewById(R.id.sample_keyboard);
    layoutParams = sampleKeyboard.getLayoutParams();
    refreshSampleKeyboard(context, true);

    resetButton = (Button) findViewById(R.id.reset_to_defaults);
    resetButton.setOnClickListener(new View.OnClickListener() {
      public void onClick(View v) {
        // Clear the keyboard height preferences to reset
        editor.remove(KMManager.KMKey_KeyboardHeightPortrait);
        editor.remove(KMManager.KMKey_KeyboardHeightLandscape);
        editor.commit();

        // Restore default height
        refreshSampleKeyboard(context, true);
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

            refreshSampleKeyboard(context, false);
            break;
          case MotionEvent.ACTION_UP:
            // Save the currentHeight when the user releases
            int orientation = context.getResources().getConfiguration().orientation;
            String keyboardHeightKey = (orientation == Configuration.ORIENTATION_LANDSCAPE) ?
              KMManager.KMKey_KeyboardHeightLandscape : KMManager.KMKey_KeyboardHeightPortrait;
            editor.putInt(keyboardHeightKey, currentHeight);
            editor.commit();
            break;
        }
        return true;
      }
    });
  }

  /**
   * Refresh the layout for the sample keyboard
   * @param context
   * @param updateCurrentHeight - boolean if true, updates currentHeight
   */
  private void refreshSampleKeyboard(Context context, boolean updateCurrentHeight) {
    if (updateCurrentHeight) {
      currentHeight = KMManager.getKeyboardHeight(context);
    }

    layoutParams.height = currentHeight;
    sampleKeyboard.setLayoutParams(layoutParams);
  }

  @Override
  public void onConfigurationChanged(Configuration newConfig) {
    super.onConfigurationChanged(newConfig);

    layoutParams = sampleKeyboard.getLayoutParams();

    // When the user rotates the device, restore currentHeight
    refreshSampleKeyboard(this,true);
  }

  @Override
  public void onBackPressed() {
    // Apply the adjusted height on exit
    KMManager.setKeyboardHeight(this, currentHeight);

    super.onBackPressed();
  }

}