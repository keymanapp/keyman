package com.tavultesoft.kmapro;

import android.os.Bundle;

import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;

public class KeymanSettingsActivity extends BaseActivity {
  protected static final String installedLanguagesKey = "InstalledLanguages";
  protected static final String installKeyboardOrDictionaryKey = "InstallKeyboardOrDictionary";
  protected static final String displayLanguageKey = "DisplayLanguage";
  protected static final String showBannerKey = "ShowBanner";
  protected static final String sendCrashReport = "SendCrashReport";

  protected KeymanSettingsFragment innerFragment;

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);

    setContentView(R.layout.keyman_settings_layout);
    Toolbar toolbar = (Toolbar) findViewById(R.id.keyman_settings_toolbar);
    toolbar.setTitleTextColor(getResources().getColor(android.R.color.white));
    toolbar.setTitle(R.string.keyman_settings);
    setSupportActionBar(toolbar);
    if (getSupportActionBar() != null) {
      getSupportActionBar().setDisplayHomeAsUpEnabled(true);
      getSupportActionBar().setDisplayShowHomeEnabled(true);
      getSupportActionBar().setDisplayShowTitleEnabled(true);
    }

    innerFragment = (KeymanSettingsFragment) getSupportFragmentManager().findFragmentById(R.id.keyman_settings_fragment);
  }

  @Override
  public void onWindowFocusChanged(boolean hasFocus) {
    super.onWindowFocusChanged(hasFocus);

    if(hasFocus) {
      innerFragment.update();
    }
  }
}
