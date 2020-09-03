/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */
package com.tavultesoft.kmapro;

import android.content.Context;
import android.graphics.Typeface;
import android.os.Bundle;
import android.os.Handler;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ListView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;
import androidx.fragment.app.Fragment;

import com.stepstone.stepper.BlockingStep;
import com.stepstone.stepper.StepperLayout;
import com.stepstone.stepper.VerificationError;
import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.data.Keyboard;
import com.tavultesoft.kmea.data.KeyboardController;
import com.tavultesoft.kmea.packages.PackageProcessor;
import com.tavultesoft.kmea.util.BCP47;
import com.tavultesoft.kmea.util.KMLog;

import org.json.JSONObject;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Displays a list of available language names for the user to add for a given installed packageID/keyboardID.
 */
public final class SelectLanguageFragment extends Fragment implements BlockingStep {
  private static final String TAG = "SelectLanguageFragment";
  private static ArrayList<HashMap<String, String>> list = null;
  private static KMListAdapter adapter = null;
  private static Typeface titleFont = null;
  private static final String titleKey = "title";
  private static final String subtitleKey = "subtitle";
  private static final String iconKey = "icon";
  private static final String isEnabledKey = "isEnabled";
  private static Context context;
  private Boolean isInstallingPackage = true;
  private static final boolean excludeInstalledLanguages = false;
  private static String packageID = null;
  private static String languageID = null;
  private ArrayList<String> languageList = null;
  private ArrayList<Keyboard> addKeyboardsList = null;
  private String title_no_install = null;
  private TextView textView;
  private File packagePath;
  private OnLanguagesSelectedListener callback;

  public void setOnLanguagesSelectedListener(OnLanguagesSelectedListener callback) {
    this.callback = callback;
  }

  // This interface to be implemented by calling Activity
  public interface OnLanguagesSelectedListener  {
    public void onLanguagesSelected(String pkgTarget, String packageID, ArrayList<String> languageList);
    public void onLanguagesSelected(ArrayList<Keyboard> addKeyboardsList);
  }

  @Override
  public View onCreateView(LayoutInflater inflater, ViewGroup container,
      Bundle savedInstanceState) {

    View v = inflater.inflate(R.layout.fragment_select_language, container, false);

    super.onCreate(savedInstanceState);
    context = getActivity();


    final ListView listView = v.findViewById(R.id.listView);
    listView.setChoiceMode(ListView.CHOICE_MODE_MULTIPLE);
    listView.setItemsCanFocus(false);
    listView.setFastScrollEnabled(true);

    Bundle bundle = getArguments();
    if (bundle == null) {
      KMLog.LogError(TAG, "No bundle parameters");
      return v;
    }

    isInstallingPackage = bundle.getBoolean("tempPath");
    File resourceRoot =  new File(getActivity().getDir("data", Context.MODE_PRIVATE).toString() + File.separator);
    PackageProcessor kmpProcessor =  new PackageProcessor(resourceRoot);
    // Get the list of available Keyboards from the keyboard package kmp.json (could be temp path or installed path)
    packagePath = (File)bundle.getSerializable("packagePath");
    packageID = bundle.getString("packageID");
    languageID = bundle.getString("languageID");
    // Initialize the list of selected languages
    languageList = new ArrayList<String>();

    JSONObject pkgInfo = kmpProcessor.loadPackageInfo(packagePath);
    Keyboard keyboard = bundle.containsKey("keyboard") ? (Keyboard)bundle.getSerializable("keyboard") :
      kmpProcessor.getKeyboard(pkgInfo, packageID, 0);
    if (keyboard == null) {
      KMLog.LogError(TAG, "Package " + packageID + " has 0 keyboards");
      return v;
    }
    final String keyboardID = keyboard.getKeyboardID();
    final String keyboardName = keyboard.getKeyboardName();
    String title_install = String.format(getString(R.string.title_select_languages_for_package), keyboardName);
    title_no_install = getString(R.string.all_languages_installed);

    final Toolbar toolbar = v.findViewById(R.id.list_toolbar);
    ((AppCompatActivity)getActivity()).setSupportActionBar(toolbar);
    ActionBar actionBar = ((AppCompatActivity)getActivity()).getSupportActionBar();
    if (actionBar != null) {
      actionBar.setTitle(null);
      actionBar.setDisplayUseLogoEnabled(false);
      actionBar.setDisplayShowHomeEnabled(false);
      actionBar.setDisplayShowTitleEnabled(false);
      actionBar.setDisplayShowCustomEnabled(true);

      // When installing packages, use the Keyman theme for the accent
      if (isInstallingPackage) {
        actionBar.setBackgroundDrawable(MainActivity.getActionBarDrawable(getContext()));

        // Hide the optional accent that's used in Settings menus
        View accent = v.findViewById(R.id.bar_accent);
        accent.setVisibility(View.GONE);
      }
    }

    textView = v.findViewById(R.id.bar_title);
    textView.setText(title_no_install);
    if (titleFont != null) {
      textView.setTypeface(titleFont, Typeface.BOLD);
    }

    addKeyboardsList = new ArrayList<Keyboard>();

    List<Keyboard> availableKeyboardsList = kmpProcessor.getKeyboardList(
      pkgInfo, packageID, keyboardID, isInstallingPackage, excludeInstalledLanguages);
    int position = KeyboardController.INDEX_NOT_FOUND;
    if (languageID != null && !languageID.isEmpty()) {
      // Add default languageID to the list of selected languages
      languageList.add(languageID);

      //  Determine position of matching language ID (if it exists)
      for (int i=0; i<availableKeyboardsList.size(); i++) {
        Keyboard k = availableKeyboardsList.get(i);
        if (BCP47.languageEquals(k.getLanguageID(), languageID)) {
          position = i;
          break;
        }
      }
    }

    final String noIcon = "0";
    list = new ArrayList<HashMap<String, String>>();
    for (Keyboard k : availableKeyboardsList) {
      HashMap<String, String> hashMap = new HashMap<>();
      hashMap.put(titleKey, k.getLanguageName());
      // No need to display subtitleKey - k.getLanguageID()
      hashMap.put("packageID", packageID);
      String enable = "true";
      String icon = noIcon;
      if (!excludeInstalledLanguages && !isInstallingPackage && KeyboardController.getInstance().keyboardExists(
          k.getPackageID(), k.getKeyboardID(), k.getLanguageID())) {
        // If Activity is listing an installed keyboard package, mark installed keyboards with a check
        icon = String.valueOf(R.drawable.ic_check);
        enable = "false";
      } else {
        // Update title
        textView.setText(title_install);
      }
      hashMap.put(iconKey, icon);
      hashMap.put(isEnabledKey, enable);
      list.add(hashMap);
    }

    String[] from = new String[]{titleKey, subtitleKey, iconKey};
    int[] to = new int[]{R.id.text1, R.id.text2, com.tavultesoft.kmea.R.id.image1};

    adapter = new KMListAdapter(context, list, R.layout.list_row_layout2, from, to);
    listView.setAdapter(adapter);

    // If languageID exists, pre-select adapter with the language
    if (position != KeyboardController.INDEX_NOT_FOUND) {
      listView.setItemChecked(position, true);
    }

    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {

      @Override
      public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        Keyboard k = availableKeyboardsList.get(position);
        if (isInstallingPackage) {
          // Add/Remove the selected language from languageList
          if (languageList == null) {
            languageList = new ArrayList<String>();
          }
          String selectedLanguageID = k.getLanguageID();
          if (languageList.contains(selectedLanguageID)) {
            languageList.remove(selectedLanguageID);
          } else {
            languageList.add(selectedLanguageID);
          }
        } else {
          // Otherwise, add the language association
          if (addKeyboardsList == null) {
            addKeyboardsList = new ArrayList<Keyboard>();
          }
          if (addKeyboardsList.contains(k)) {
            addKeyboardsList.remove(k);
          } else {
            addKeyboardsList.add(k);
          }
        }

        // Disable install button if no languages selected or all languages already installed
        checkLanguages();
      }
    });

    return v;
  }

  /**
   * Validates a language has been selected in the "Select Language" step.
   * @return VerificationError
   */
  public VerificationError checkLanguages() {
    StepperLayout mStepperLayout = (StepperLayout) getActivity().findViewById(R.id.stepperLayout);

    // Only applies if stepper is in "Select Language" step
    if ((isInstallingPackage && mStepperLayout.getCurrentStepPosition() == 1) ||
        (!isInstallingPackage && mStepperLayout.getCurrentStepPosition() == 0)) {
      if (languageList.size() == 0 && addKeyboardsList.size() == 0) {
        mStepperLayout.setNextButtonVerificationFailed(true);
        return new VerificationError("No languages selected");
      } else if (textView.getText().equals(title_no_install)) {
        mStepperLayout.setNextButtonVerificationFailed(true);
        return new VerificationError("All languages already installed");
      } else {
        mStepperLayout.setNextButtonVerificationFailed(false);
      }
    } else {
      mStepperLayout.setNextButtonVerificationFailed(false);
    }

    return null;
  }

  @Override
  public void onNextClicked(final StepperLayout.OnNextClickedCallback callback) {
    // Send data to calling Activity
    if (!isInstallingPackage) {
      this.callback.onLanguagesSelected(addKeyboardsList);
    } else {
      this.callback.onLanguagesSelected(PackageProcessor.PP_TARGET_KEYBOARDS, packageID, languageList);
    }

    new Handler().postDelayed(new Runnable() {
      @Override
      public void run() {
        //you can do anythings you want
        callback.goToNextStep();
      }
    }, 1000L);// delay open another fragment,
  }
  @Override
  public void onCompleteClicked(StepperLayout.OnCompleteClickedCallback callback) {
    if (!isInstallingPackage) {
      this.callback.onLanguagesSelected(addKeyboardsList);
    }
    getActivity().finish();
  }
  @Override
  public void onBackClicked(StepperLayout.OnBackClickedCallback callback) {
    callback.goToPrevStep();
  }
  @Override
  public VerificationError verifyStep() {
    return checkLanguages();
  }

  @Override
  public void onSelected() {
    checkLanguages();
  }
  @Override
  public void onError(@NonNull VerificationError error) {
    // do nothing
  }

}
