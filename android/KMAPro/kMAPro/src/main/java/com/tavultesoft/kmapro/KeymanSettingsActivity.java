package com.tavultesoft.kmapro;

import android.os.Bundle;

import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;

public class KeymanSettingsActivity extends AppCompatActivity {
  protected static final String installedLanguagesKey = "InstalledLanguages";
  protected static final String showBannerKey = "ShowBanner";

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);

    setContentView(R.layout.keyman_settings_layout);
    Toolbar toolbar = (Toolbar) findViewById(R.id.keyman_settings_toolbar);
    toolbar.setTitleTextColor(getResources().getColor(android.R.color.white));
    setSupportActionBar(toolbar);
    if (getSupportActionBar() != null) {
      getSupportActionBar().setDisplayHomeAsUpEnabled(true);
      getSupportActionBar().setDisplayShowHomeEnabled(true);
      getSupportActionBar().setDisplayShowTitleEnabled(true);
    }
  }
}