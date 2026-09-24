/*
 * Keyman is copyright (C) SIL Global. MIT License.
 * 
 * Created by MengchouChey on 2026-06-10
 *            Internship Program, 9th Feb - 27th Apr 2026
 * 
 * This file is the Application class for Keyman, which is used to set the default night mode based on user preferences when the application starts.
 * 
 */
package com.tavultesoft.kmapro;

import android.app.Application;
import android.content.SharedPreferences;

import androidx.appcompat.app.AppCompatDelegate;

public class KeymanApplication extends Application {
  @Override
  public void onCreate() {
    SharedPreferences prefs = getSharedPreferences("settings", MODE_PRIVATE);
    int mode = prefs.getInt("theme_mode", AppCompatDelegate.MODE_NIGHT_FOLLOW_SYSTEM);
    AppCompatDelegate.setDefaultNightMode(mode);
    super.onCreate();
  }
}
