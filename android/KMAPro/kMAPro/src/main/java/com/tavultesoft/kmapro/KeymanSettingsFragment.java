package com.tavultesoft.kmapro;

import android.content.Context;
import android.content.Intent;
import android.net.Uri;
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

  private Preference languagesPreference, addKeyboardFromDevice;
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

    // Launch System file browser for user to navigate to local .kmp files
    // Using generic "Add keyboard" title even though this can also install lexical model .kmp's.
    addKeyboardFromDevice = new Preference(context);
    addKeyboardFromDevice.setTitle(getString(R.string.title_add_keyboard_from_device));
    addKeyboardFromDevice.setWidgetLayoutResource(R.layout.preference_file_browser_layout);
    addKeyboardFromDevice.setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
      @Override
      public boolean onPreferenceClick(Preference preference) {
        Intent intent = new Intent(Intent.ACTION_OPEN_DOCUMENT);
        intent.addCategory(Intent.CATEGORY_OPENABLE);
        intent.putExtra("android.content.extra.SHOW_ADVANCED", false);
        // Unfortunately, we can't filter for a "kmp" mime type
        intent.setType("*/*");
        startActivityForResult(intent, MainActivity.READ_REQUEST_CODE);
        return false;
      }
    });

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

    // Blocks the default checkmark interaction; we want to control the checkmark's state separately
    // from within update() based on if the user has taken the appropriate actions with the OS.
    final Preference.OnPreferenceChangeListener checkBlocker = new Preference.OnPreferenceChangeListener() {
      @Override
      public boolean onPreferenceChange(Preference preference, Object newValue) {
        return false;
      }
    };

    setSystemKeyboardPreference = new CheckBoxPreference(context);
    setSystemKeyboardPreference.setTitle(R.string.enable_system_keyboard);
    setSystemKeyboardPreference.setDefaultValue(SystemIMESettings.isEnabledAsSystemKB(context));
    setSystemKeyboardPreference.setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
      @Override
      public boolean onPreferenceClick(Preference preference) {
        startActivity(new Intent(Settings.ACTION_INPUT_METHOD_SETTINGS));
        return false;
      }
    });
    setSystemKeyboardPreference.setOnPreferenceChangeListener(checkBlocker);

    setDefaultKeyboardPreference = new CheckBoxPreference(context);
    setDefaultKeyboardPreference.setTitle(R.string.set_keyman_as_default);
    setDefaultKeyboardPreference.setDefaultValue(SystemIMESettings.isDefaultKB(context));
    setDefaultKeyboardPreference.setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
      @Override
      public boolean onPreferenceClick(Preference preference) {
        InputMethodManager imManager = (InputMethodManager) context.getSystemService(Context.INPUT_METHOD_SERVICE);
        imManager.showInputMethodPicker();
        return false;
      }
    });
    setDefaultKeyboardPreference.setOnPreferenceChangeListener(checkBlocker);

    screen.addPreference(languagesPreference);
    screen.addPreference(addKeyboardFromDevice);

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

    // The onResume() function isn't called after the default-keyboard selection and cannot fix that
    // option, though it is sufficient for handling the system-keyboard enablement option.
    //
    // As a result, we rely on KeymanSettingsActivity.onWindowFocusChanged to call
    // .update() on our behalf.
  }


  public void onActivityResult(int requestCode, int resultCode, Intent returnIntent) {
    // Handle kmp file selected from file browser
    if ((requestCode == MainActivity.READ_REQUEST_CODE) && (returnIntent != null)) {
      String kmpFilename = returnIntent.getDataString();
      Uri data = Uri.parse(kmpFilename);
      MainActivity.useLocalKMP(this.getContext(), data);
    }
  }

  public void update() {
    setSystemKeyboardPreference.setChecked(SystemIMESettings.isEnabledAsSystemKB(context));
    setDefaultKeyboardPreference.setChecked(SystemIMESettings.isDefaultKB(context));

    // Update the language / keyboard count when we return to this menu from deeper levels.
    languagesPreference.setTitle(getInstalledLanguagesText());
  }
}
