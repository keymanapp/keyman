package com.tavultesoft.kmapro;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;

import androidx.preference.Preference;
import androidx.preference.PreferenceFragmentCompat;
import androidx.preference.PreferenceScreen;
import androidx.preference.SwitchPreference;

import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.data.Dataset;

import java.util.ArrayList;
import java.util.HashMap;

public class KeymanSettingsFragment extends PreferenceFragmentCompat {
  private static final String TAG = "SettingsFragment";
  private static Context context;

  private Preference languagesPreference;

  @Override
  public void onCreatePreferences(Bundle savedInstanceState, String rootKey) {
    context = getActivity();
    PreferenceScreen screen = getPreferenceManager().createPreferenceScreen(context);

    // Set the filename so this Fragment uses the common preferences file as other Activities
    getPreferenceManager().setSharedPreferencesName(getString(R.string.kma_prefs_name));

    languagesPreference = new Preference(context);
    languagesPreference.setKey(KeymanSettingsActivity.installedLanguagesKey);
    languagesPreference.setTitle(getInstalledLanguagesText());
    languagesPreference.setWidgetLayoutResource(R.layout.preference_icon_layout);
    Intent languagesIntent = new Intent();
    languagesIntent.setClassName(context.getPackageName(), "com.tavultesoft.kmea.LanguagesSettingsActivity");
    languagesIntent.putExtra(KMManager.KMKey_DisplayKeyboardSwitcher, false);
    languagesPreference.setIntent(languagesIntent);

    /*
      Automatically does the following:
        SharedPreferences.Editor editor = prefs.edit();
          editor.putBoolean(KeymanSettingsActivity.showBannerKey, isChecked);
      as part of the default onClick() used by SwitchPreference.
     */
    SwitchPreference bannerPreference = new SwitchPreference(context);
    bannerPreference.setKey(KeymanSettingsActivity.showBannerKey);
    bannerPreference.setTitle(getString(R.string.show_banner));
    bannerPreference.setSummaryOn(getString(R.string.show_banner_on));
    bannerPreference.setSummaryOff(getString(R.string.show_banner_off));

    SwitchPreference getStartedPreference = new SwitchPreference(context);
    getStartedPreference.setKey(GetStartedActivity.showGetStartedKey);
    getStartedPreference.setTitle(getString(R.string.show_get_started));
    getStartedPreference.setDefaultValue(true);

    screen.addPreference(languagesPreference);
    screen.addPreference(bannerPreference);
    screen.addPreference(getStartedPreference);

    setPreferenceScreen(screen);
  }

  String getInstalledLanguagesText() {
    Dataset dataset = KMManager.getInstalledDataset(context);
    int langCount = 0;

    // Some dataset languages may only have lexical models installed.
    // This may happen if one model covers multiple variants of one language.  (Ex:  en vs en-us)
    for(Dataset.LanguageDataset language: dataset.asList()) {
      if(language.keyboards.size() > 0) {
        langCount++;
      }
    }

    if(langCount > 1) {
      // Has a %d slot to insert the count.
      return String.format(getString(R.string.installed_languages), langCount);
    } else {
      return getString(R.string.installed_languages_empty);
    }
  }

  @Override
  public void onResume() {
    super.onResume();

    // Update the language / keyboard count when we return to this menu from deeper levels.
    languagesPreference.setTitle(getInstalledLanguagesText());
  }
}
