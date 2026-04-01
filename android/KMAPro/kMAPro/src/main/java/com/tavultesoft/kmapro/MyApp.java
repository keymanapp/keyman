package com.tavultesoft.kmapro;

import android.app.Application;
import android.content.SharedPreferences;
import androidx.appcompat.app.AppCompatDelegate;

public class MyApp extends Application {
  @Override
  public void onCreate() {
    super.onCreate();
    SharedPreferences prefs = getSharedPreferences("settings", MODE_PRIVATE);
    int mode = prefs.getInt("theme_mode", AppCompatDelegate.MODE_NIGHT_FOLLOW_SYSTEM);
    AppCompatDelegate.setDefaultNightMode(mode);
  }
}