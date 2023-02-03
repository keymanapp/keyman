/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */
package com.tavultesoft.kmapro;

import android.content.Context;
import android.os.Bundle;

import androidx.annotation.IntRange;
import androidx.annotation.NonNull;
import androidx.fragment.app.FragmentManager;

import com.stepstone.stepper.Step;
import com.stepstone.stepper.adapter.AbstractFragmentStepAdapter;
import com.stepstone.stepper.viewmodel.StepViewModel;
import com.keyman.engine.data.Keyboard;
import com.keyman.engine.packages.PackageProcessor;
import com.keyman.engine.util.FileUtils;

import java.io.File;

/**
 * This adapter manages the Stepper that helps the user know how many steps they will encounter while
 * installing a package.
 */
public class StepperAdapter extends AbstractFragmentStepAdapter {
  private static final String CURRENT_STEP_POSITION_KEY = "messageResourceId";
  private boolean isInstallingPackage;
  private File tempPackagePath;
  private String pkgTarget;
  private String pkgName;

  private String packageID;
  private Keyboard keyboard;

  private String languageID;
  private int languageCount;

  /**
   * Constructor where languageID and languageCount are empty
   * @param fm
   * @param context
   * @param isInstallingPackage
   * @param tempPackagePath
   * @param pkgTarget
   * @param packageID
   * @param pkgName
   * @param keyboard
   */
  public StepperAdapter(FragmentManager fm, Context context,
                        boolean isInstallingPackage, File tempPackagePath,
                        String pkgTarget, String packageID, String pkgName,
                        Keyboard keyboard) {
    super(fm, context);
    this.isInstallingPackage = isInstallingPackage;
    this.tempPackagePath = tempPackagePath;
    this.pkgTarget = pkgTarget;
    this.packageID = packageID;
    this.pkgName = pkgName;
    this.keyboard = keyboard;
    this.languageID = null; // not filled
    this.languageCount = 0;
  }

  public StepperAdapter(FragmentManager fm, Context context,
                        boolean isInstallingPackage, File tempPackagePath,
                        String pkgTarget, String packageID, String pkgName,
                        Keyboard keyboard,
                        String languageID, int languageCount) {
    super(fm, context);
    this.isInstallingPackage = isInstallingPackage;
    this.tempPackagePath = tempPackagePath;
    this.pkgTarget = pkgTarget;
    this.packageID = packageID;
    this.pkgName = pkgName;
    this.keyboard = keyboard;
    this.languageID = languageID;
    this.languageCount = languageCount;
  }

  @Override
  public Step createStep(int position) {
    switch (position){
      case 0:
        if (isInstallingPackage) {
          final WebViewFragment step1 = CreateWebView(position, FileUtils.README_HTM);
          return step1;
        } else {
          final SelectLanguageFragment step1 = new SelectLanguageFragment();
          Bundle b1 = new Bundle();
          b1.putInt(CURRENT_STEP_POSITION_KEY, position);
          b1.putBoolean("tempPath", isInstallingPackage);
          b1.putSerializable("packagePath", tempPackagePath);
          b1.putString("packageID", packageID);
          b1.putString("languageID", languageID);
          if (keyboard != null) {
            b1.putSerializable("keyboard", keyboard);
          }
          step1.setArguments(b1);
          return step1;
        }
      case 1:
        final SelectLanguageFragment step2 = new SelectLanguageFragment();
        Bundle b2 = new Bundle();
        b2.putInt(CURRENT_STEP_POSITION_KEY, position);
        b2.putBoolean("tempPath", true);
        b2.putSerializable("packagePath", tempPackagePath);
        b2.putString("packageID", packageID);
        b2.putString("languageID", languageID);
        if (keyboard != null) {
          b2.putSerializable("keyboard", keyboard);
        }
        step2.setArguments(b2);
        return step2;
    }
    return null;
  }

  /**
   * Determine the number of states for the StepperAdapter
   * No longer displaying welcome.htm with the Stepper
   *
   * !isInstallingPackage:
   * 1 dot: keyboard package already installed, so just use SelectLanguageFragment
   *
   * Keyboard package:
   * 1 dot: package has one language.
   * 2 dots: package has multiple languages.
   *
   * Lexical Model package:
   * 1 dot
   * @return int
   */
  @Override
  public int getCount() {
    int count = 1;
    if ((pkgTarget != null) && pkgTarget.equals(PackageProcessor.PP_KEYBOARDS_KEY) && languageCount > 1) {
      // More than 1 language
      count = 2;
    }
    return count;
  }

  @NonNull
  @Override
  public StepViewModel getViewModel(@IntRange(from = 0) int position) {
    //Override this method to customize the steps
    switch (position){
      case 0:
        // readme.htm EndButton uses "NEXT" button
        String buttonLabel = context.getString(R.string.label_next);
        if (getCount() == 1) {
          // If only 1 step, use "INSTALL"
          buttonLabel = context.getString(R.string.label_install);
        }
        return new StepViewModel.Builder(context)
          .setEndButtonLabel(buttonLabel)
          .create();
      case 1:
        // Language picker EndButton uses "INSTALL" button
        buttonLabel = context.getString(R.string.label_install);
        boolean backButtonVisible = true;
        return new StepViewModel.Builder(context)
          .setEndButtonLabel(buttonLabel)
          .setBackButtonVisible(backButtonVisible)
          .create();
    }
    return null;
  }

  /**
   * Utility to create WebViewFragment
   * @param position int position in the adapter
   * @param fileName FileUtils.README_HTM
   * @return WebViewFragment
   */
  private WebViewFragment CreateWebView(int position, String fileName) {
    boolean isInstallButton = false;
    if (FileUtils.isReadmeFile(fileName)) {
      if (pkgTarget.equals(PackageProcessor.PP_TARGET_LEXICAL_MODELS)) {
        isInstallButton = true;
      } else if (pkgTarget.equals(PackageProcessor.PP_TARGET_KEYBOARDS)) {
        if (languageCount <= 1) {
          isInstallButton = true;
        }
      }
    }

    WebViewFragment step = new WebViewFragment();
    Bundle bundle = new Bundle();
    bundle.putInt(CURRENT_STEP_POSITION_KEY, position);
    bundle.putSerializable("tempPackagePath", tempPackagePath);
    bundle.putString("packageID", packageID);
    bundle.putString("pkgTarget", pkgTarget);
    bundle.putString("pkgName", pkgName);
    bundle.putString("fileName", fileName);
    bundle.putBoolean("isInstallButton", isInstallButton);
    step.setArguments(bundle);
    return step;
  }
}