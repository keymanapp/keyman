/**
 * Copyright (C) SIL International. All rights reserved.
 */
package com.tavultesoft.kmapro;

import android.content.Context;
import android.content.SharedPreferences;
import android.graphics.drawable.ColorDrawable;
import android.os.Bundle;
import android.view.View;
import android.widget.SeekBar;
import android.widget.TextView;

import androidx.appcompat.app.ActionBar;
import androidx.appcompat.widget.Toolbar;
import androidx.core.content.ContextCompat;

import com.keyman.engine.BaseActivity;
import com.keyman.engine.KMManager;

/**
 * Settings menu for adjusting the longpress delay time. The value for the current longpress delay time
 * is saved in shared preferences as an integer (milliseconds).
 */
public class AdjustLongpressDelayActivity extends BaseActivity {
  private static final String TAG = "AdjustLongpressDelay";
  public static final String adjustLongpressDelayKey = "AdjustLongpressDelay";
  private static SharedPreferences.Editor editor = null;

  // Keeps track of the adjusted longpress delay time for saving.
  // Internally use milliseconds, but GUI displays seconds
  private static int currentDelayTime = KMManager.KMDefault_LongpressDelay;  // ms
  private static int minLongpressTime = 300;   // ms
  private static int maxLongpressTime = 1500;  // ms
  private static int delayTimeIncrement = 200; // ms

  /**
   * Convert currentDelayTime to progress
   * @return int
   */
  private int delayTimeToProgress() {
    return (currentDelayTime / delayTimeIncrement) - 1;
  }

  /**
   * Convert progress to currentDelayTime
   * @param progress
   * @return int (milliseconds)
   */
  private int progressToDelayTime(int progress) {
    return (progress + 1) * delayTimeIncrement + 100;
  }

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);

    final Context context = this;

    setContentView(R.layout.activity_adjust_longpress_delay);
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

    TextView adjustLongpressDelayActivityTitle = (TextView) findViewById(R.id.bar_title);

    String titleStr = getString(R.string.adjust_longpress_delay);
    adjustLongpressDelayActivityTitle.setTextColor(ContextCompat.getColor(this, R.color.ms_white));
    adjustLongpressDelayActivityTitle.setText(titleStr);

    SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    editor = prefs.edit();
    currentDelayTime = KMManager.getLongpressDelay(this);

    TextView adjustLongpressDelayText = (TextView) findViewById(R.id.delayTimeText);
    String longpressDelayText = String.format(getString(R.string.longpress_delay_time), (float)(currentDelayTime/1000.0));
    adjustLongpressDelayText.setTextAlignment(View.TEXT_ALIGNMENT_CENTER);
    adjustLongpressDelayText.setText(longpressDelayText);

    final SeekBar seekBar = (SeekBar) findViewById(R.id.seekBar);
    seekBar.setProgress(delayTimeToProgress());
    seekBar.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {

      @Override
      public void onStopTrackingTouch(SeekBar seekBar) {
        // Do nothing
      }

      @Override
      public void onStartTrackingTouch(SeekBar seekBar) {
        // Do nothing
      }

      @Override
      public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {
        currentDelayTime = progressToDelayTime(progress);
        String longpressDelayText = String.format(getString(R.string.longpress_delay_time), (float)(currentDelayTime/1000.0));
        adjustLongpressDelayText.setText(longpressDelayText);

        editor.putInt(KMManager.KMKey_LongpressDelay, currentDelayTime);
        editor.commit();
      }
    });

    findViewById(R.id.delayTimeDownButton).setOnClickListener(new View.OnClickListener() {
      @Override
      public void onClick(View v) {
        if (currentDelayTime > minLongpressTime) {
          currentDelayTime -= delayTimeIncrement;
          seekBar.setProgress(delayTimeToProgress());
        }
      }
    });

    findViewById(R.id.delayTimeUpButton).setOnClickListener(new View.OnClickListener() {
      @Override
      public void onClick(View v) {
        if (currentDelayTime < maxLongpressTime) {
          currentDelayTime += delayTimeIncrement;
          seekBar.setProgress(delayTimeToProgress());
        }
      }
    });
  }

  @Override
  public void onBackPressed() {
    // Apply the adjusted longpress delay on exit
    KMManager.setLongpressDelay(currentDelayTime);

    super.onBackPressed();
  }

  @Override
  public boolean onSupportNavigateUp() {
    onBackPressed();
    return true;
  }

}
