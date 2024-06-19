/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */
package com.tavultesoft.kmapro;

import android.content.Context;
import android.os.Bundle;
import android.view.View;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;
import androidx.fragment.app.Fragment;

import com.stepstone.stepper.StepperLayout;
import com.stepstone.stepper.VerificationError;
import com.keyman.engine.KMManager;
import com.keyman.engine.data.Keyboard;
import com.keyman.engine.packages.PackageProcessor;
import com.keyman.engine.util.KMLog;

import org.json.JSONObject;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * Keyman Settings --> KeymanInstallActivity --> SelectPackageActivity --> SelectLanguageActivity
 * Also available during keyboard package installation that involves a language choice.
 * Displays a list of available language names for the user to add for a given installed packageID/keyboardID.
 */
public class SelectLanguageActivity extends AppCompatActivity implements
    StepperLayout.StepperListener, SelectLanguageFragment.OnLanguagesSelectedListener {
  private final static String TAG = "SelectLanguageActivity";
  private File packagePath;
  private String packageID;
  private boolean isInstallingPackage;
  private Keyboard keyboard;
  private StepperLayout mStepperLayout;
  private StepperAdapter mStepperAdapter;

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.activity_select_language);

    Bundle bundle = getIntent().getExtras();
    keyboard = (Keyboard)bundle.getSerializable("keyboard");
    packagePath = (File)bundle.getSerializable("packagePath");
    packageID = bundle.getString("packageID");
    isInstallingPackage = bundle.getBoolean("isInstallingPackage");
    String pkgTarget = PackageProcessor.PP_TARGET_KEYBOARDS;

    File resourceRoot =  new File(this.getDir("data", Context.MODE_PRIVATE).toString() + File.separator);
    PackageProcessor kmpProcessor =  new PackageProcessor(resourceRoot);
    JSONObject pkgInfo = kmpProcessor.loadPackageInfo(packagePath);

    String pkgName = kmpProcessor.getPackageName(pkgInfo);

    if (keyboard == null) {
      KMLog.LogError(TAG, "Package " + packageID + " has 0 keyboards");
      return;
    }

    mStepperLayout = (StepperLayout) findViewById(R.id.stepperLayout);
    mStepperAdapter = new StepperAdapter(getSupportFragmentManager(), this,
      isInstallingPackage, packagePath, pkgTarget, packageID, pkgName, keyboard);
    mStepperLayout.setAdapter(mStepperAdapter);
    mStepperLayout.setListener(this);

  }

  @Override
  public void onCompleted(View completedButton) {
    Toast.makeText(this, "onCompleted", Toast.LENGTH_SHORT).show();
  }

  @Override
  public void onError(VerificationError verificationError) {
    Toast.makeText(this, verificationError.getErrorMessage(), Toast.LENGTH_SHORT).show();
  }

  @Override
  public void onStepSelected(int newStepPosition) {


  }

  @Override
  public void onReturn() {
    finish();
  }

  @Override
  public void onAttachFragment(Fragment fragment) {
    if (fragment instanceof SelectLanguageFragment) {
      SelectLanguageFragment selectLanguageFragment = (SelectLanguageFragment) fragment;
      selectLanguageFragment.setOnLanguagesSelectedListener(this);
    }
  }

  @Override
  public void onLanguagesSelected(String pkgTarget, String packageID, ArrayList<String> languageList) {
  }

  @Override
  public void onLanguagesSelected(ArrayList<Keyboard> addKeyboardsList) {
    for (Keyboard k : addKeyboardsList) {
      KMManager.addKeyboard(this, k);
      String confirmation = String.format(getString(R.string.added_language_to_keyboard),
        k.getLanguageName(), k.getKeyboardName());
      Toast.makeText(this, confirmation, Toast.LENGTH_LONG).show();
    }
  }
}
