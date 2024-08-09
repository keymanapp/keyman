/**
 * Copyright (C) 2020-2021 SIL International. All rights reserved.
 */

package com.tavultesoft.kmapro;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.provider.Settings;
import android.view.inputmethod.InputMethodManager;

import androidx.preference.CheckBoxPreference;
import androidx.preference.ListPreference;
import androidx.preference.Preference;
import androidx.preference.PreferenceFragmentCompat;
import androidx.preference.PreferenceScreen;
import androidx.preference.SwitchPreference;

import com.keyman.engine.DisplayLanguages;
import com.keyman.engine.KMManager;
import com.keyman.engine.data.Dataset;

import java.util.HashMap;

public class KeymanSettingsFragment extends PreferenceFragmentCompat {
  private static final String TAG = "SettingsFragment";
  private static Context context;

  private Preference languagesPreference, installKeyboardOrDictionary, displayLanguagePreference,
    adjustKeyboardHeight, adjustLongpressDelay;
  private ListPreference spacebarTextPreference;
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
    Intent languagesIntent = new Intent(context, LanguagesSettingsActivity.class);
    languagesIntent.putExtra(KMManager.KMKey_DisplayKeyboardSwitcher, false);
    languagesPreference.setIntent(languagesIntent);

    installKeyboardOrDictionary = new Preference(context);
    installKeyboardOrDictionary.setKey(KeymanSettingsActivity.installKeyboardOrDictionaryKey);
    installKeyboardOrDictionary.setTitle(getString(R.string.install_keyboard_or_dictionary));
    installKeyboardOrDictionary.setWidgetLayoutResource(R.layout.preference_add_icon_layout);
    Intent installIntent = new Intent(context, KeymanSettingsInstallActivity.class);
    installKeyboardOrDictionary.setIntent(installIntent);

    displayLanguagePreference = new Preference(context);
    displayLanguagePreference.setKey(DisplayLanguages.displayLanguageKey);
    displayLanguagePreference.setTitle(getString(R.string.change_display_language));
    displayLanguagePreference.setWidgetLayoutResource(R.layout.preference_translate_icon_layout);
    Intent displayLanguageIntent = new Intent(context, KeymanSettingsLocalizeActivity.class);
    displayLanguagePreference.setIntent(displayLanguageIntent);

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
    setSystemKeyboardPreference.setSingleLineTitle(false);
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
    setDefaultKeyboardPreference.setSingleLineTitle(false);
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

    adjustKeyboardHeight = new Preference(context);
    adjustKeyboardHeight.setKey(AdjustKeyboardHeightActivity.adjustKeyboardHeightKey);
    adjustKeyboardHeight.setTitle(getString(R.string.adjust_keyboard_height));
    adjustKeyboardHeight.setWidgetLayoutResource(R.layout.preference_height_icon_layout);
    Intent adjustKeyboardHeightIntent = new Intent(context, AdjustKeyboardHeightActivity.class);
    adjustKeyboardHeight.setIntent(adjustKeyboardHeightIntent);

    adjustLongpressDelay = new Preference(context);
    adjustLongpressDelay.setKey(AdjustLongpressDelayActivity.adjustLongpressDelayKey);
    adjustLongpressDelay.setTitle(getString(R.string.adjust_longpress_delay));
    adjustLongpressDelay.setWidgetLayoutResource(R.layout.preference_duration_icon_layout);
    Intent adjustLongpressDelayIntent = new Intent(context, AdjustLongpressDelayActivity.class);
    adjustLongpressDelay.setIntent(adjustLongpressDelayIntent);

    /* Spacebar Caption Preference */

    spacebarTextPreference = new ListPreference(context);
    spacebarTextPreference.setKey(KeymanSettingsActivity.spacebarTextKey);

    spacebarTextPreference.setTitle(getString(R.string.spacebar_caption)); // getString(R.string.change_display_language));

    CharSequence[] entries = {
      getString(R.string.spacebar_caption_language),
      getString(R.string.spacebar_caption_keyboard),
      getString(R.string.spacebar_caption_language_keyboard),
      getString(R.string.spacebar_caption_blank)
    };

    HashMap<KMManager.SpacebarText,String> captions = new HashMap<>();
    captions.put(KMManager.SpacebarText.LANGUAGE, getString(R.string.spacebar_caption_hint_language));
    captions.put(KMManager.SpacebarText.KEYBOARD, getString(R.string.spacebar_caption_hint_keyboard));
    captions.put(KMManager.SpacebarText.LANGUAGE_KEYBOARD, getString(R.string.spacebar_caption_hint_language_keyboard));
    captions.put(KMManager.SpacebarText.BLANK, getString(R.string.spacebar_caption_hint_blank));

    CharSequence[] entryValues = {
      KMManager.SpacebarText.LANGUAGE.toString(),
      KMManager.SpacebarText.KEYBOARD.toString(),
      KMManager.SpacebarText.LANGUAGE_KEYBOARD.toString(),
      KMManager.SpacebarText.BLANK.toString()
    };

    spacebarTextPreference.setEntries(entries);
    spacebarTextPreference.setEntryValues(entryValues);
    spacebarTextPreference.setValue(KMManager.getSpacebarText().toString());
    spacebarTextPreference.setSummary(captions.get(KMManager.getSpacebarText()));
    spacebarTextPreference.setOnPreferenceChangeListener(new Preference.OnPreferenceChangeListener() {
      @Override
      public boolean onPreferenceChange(Preference preference, Object newValue) {
        if(newValue == null)
          return false;
        KMManager.SpacebarText mode = KMManager.SpacebarText.fromString((String) newValue);
        KMManager.setSpacebarText(mode);
        spacebarTextPreference.setSummary(captions.get(mode));
        return true;
      }
    });

    SwitchPreference hapticFeedbackPreference = new SwitchPreference(context);
    hapticFeedbackPreference.setKey(KeymanSettingsActivity.hapticFeedbackKey);
    hapticFeedbackPreference.setTitle(getString(R.string.haptic_feedback));
    hapticFeedbackPreference.setDefaultValue(false);
    hapticFeedbackPreference.setOnPreferenceChangeListener(new Preference.OnPreferenceChangeListener() {
      @Override
      public boolean onPreferenceChange(Preference preference, Object newValue) {
        if (newValue == null) {
          return false;
        }
        KMManager.setHapticFeedback((boolean)newValue);
        return true;
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
    getStartedPreference.setTitle(String.format(getString(R.string.show_get_started), getString(R.string.get_started)));
    getStartedPreference.setDefaultValue(true);

    SwitchPreference sendCrashReportPreference = new SwitchPreference(context);
    sendCrashReportPreference.setKey(KeymanSettingsActivity.sendCrashReport);
    sendCrashReportPreference.setTitle(getString(R.string.show_send_crash_report));
    sendCrashReportPreference.setSummaryOn(getString(R.string.show_send_crash_report_on));
    sendCrashReportPreference.setSummaryOff(getString(R.string.show_send_crash_report_off));
    sendCrashReportPreference.setDefaultValue(true);

    screen.addPreference(languagesPreference);
    screen.addPreference(installKeyboardOrDictionary);
    screen.addPreference(displayLanguagePreference);
    screen.addPreference(setSystemKeyboardPreference);
    screen.addPreference(setDefaultKeyboardPreference);

    screen.addPreference(adjustKeyboardHeight);
    screen.addPreference(adjustLongpressDelay);
    screen.addPreference(spacebarTextPreference);

    screen.addPreference(hapticFeedbackPreference);
    screen.addPreference(bannerPreference);
    screen.addPreference(getStartedPreference);
    screen.addPreference(sendCrashReportPreference);

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

    return getContext().getResources().getQuantityString(R.plurals.installed_languages, langCount, langCount);
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

  public void update() {
    setSystemKeyboardPreference.setChecked(SystemIMESettings.isEnabledAsSystemKB(context));
    setDefaultKeyboardPreference.setChecked(SystemIMESettings.isDefaultKB(context));

    // Update the language / keyboard count when we return to this menu from deeper levels.
    languagesPreference.setTitle(getInstalledLanguagesText());
  }
}
