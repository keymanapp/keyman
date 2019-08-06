package com.tavultesoft.kmapro;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.provider.Settings;
import android.view.inputmethod.InputMethodManager;

import androidx.preference.CheckBoxPreference;
import androidx.preference.Preference;
import androidx.preference.PreferenceFragmentCompat;
import androidx.preference.PreferenceScreen;
import androidx.preference.SwitchPreference;

import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.data.Dataset;

public class KeymanSettingsFragment extends PreferenceFragmentCompat {
  private static final String TAG = "SettingsFragment";
  private static Context context;

  private Preference languagesPreference;
  private CheckBoxPreference setSystemKeyboardPreference;
  private CheckBoxPreference setDefaultKeyboardPreference;

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

    setSystemKeyboardPreference = new CheckBoxPreference(context);
    setSystemKeyboardPreference.setTitle(R.string.enable_system_keyboard);
    setSystemKeyboardPreference.setDefaultValue(GetStartedActivity.isEnabledAsSystemKB(context));
    setSystemKeyboardPreference.setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
      @Override
      public boolean onPreferenceClick(Preference preference) {
        startActivity(new Intent(Settings.ACTION_INPUT_METHOD_SETTINGS));
        return false;
      }
    });
    setSystemKeyboardPreference.setOnPreferenceChangeListener(new Preference.OnPreferenceChangeListener() {
      @Override
      public boolean onPreferenceChange(Preference preference, Object newValue) {
        // Blocks the checkmark from changing from user interaction.
        return false;
      }
    });

    setDefaultKeyboardPreference = new CheckBoxPreference(context);
    setDefaultKeyboardPreference.setTitle(R.string.set_keyman_as_default);
    setDefaultKeyboardPreference.setDefaultValue(GetStartedActivity.isDefaultKB(context));
    setDefaultKeyboardPreference.setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
      @Override
      public boolean onPreferenceClick(Preference preference) {
        InputMethodManager imManager = (InputMethodManager) context.getSystemService(Context.INPUT_METHOD_SERVICE);
        imManager.showInputMethodPicker();
        return false;
      }
    });
    setDefaultKeyboardPreference.setOnPreferenceChangeListener(new Preference.OnPreferenceChangeListener() {
      @Override
      public boolean onPreferenceChange(Preference preference, Object newValue) {
        // Blocks the checkmark from changing from user interaction.
        return false;
      }
    });

    screen.addPreference(languagesPreference);
    screen.addPreference(setSystemKeyboardPreference);
    screen.addPreference(setDefaultKeyboardPreference);

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

    update();
  }

  public void update() {
    setSystemKeyboardPreference.setChecked(GetStartedActivity.isEnabledAsSystemKB(context));
    // This function isn't called after the default-keyboard selection and cannot fix the option.
    setDefaultKeyboardPreference.setChecked(GetStartedActivity.isDefaultKB(context));

    // Update the language / keyboard count when we return to this menu from deeper levels.
    languagesPreference.setTitle(getInstalledLanguagesText());
  }
}
