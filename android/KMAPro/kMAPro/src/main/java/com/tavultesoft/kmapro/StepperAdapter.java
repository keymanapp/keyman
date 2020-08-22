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
import com.tavultesoft.kmea.data.Keyboard;
import com.tavultesoft.kmea.packages.PackageProcessor;
import com.tavultesoft.kmea.util.FileUtils;

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

  private int languageCount;
  private boolean hasWelcome;

  public StepperAdapter(FragmentManager fm, Context context,
                        boolean isInstallingPackage, File tempPackagePath,
                        String pkgTarget, String packageID, String pkgName,
                        boolean hasWelcome, int languageCount) {
    super(fm, context);
    this.isInstallingPackage = isInstallingPackage;
    this.tempPackagePath = tempPackagePath;
    this.pkgTarget = pkgTarget;
    this.packageID = packageID;
    this.pkgName = pkgName;
    this.hasWelcome = hasWelcome;
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
          step1.setArguments(b1);
          return step1;
        }
      case 1:
        if (getCount() > 2) {
          final SelectLanguageFragment step2 = new SelectLanguageFragment();
          Bundle b2 = new Bundle();
          b2.putInt(CURRENT_STEP_POSITION_KEY, position);
          b2.putBoolean("tempPath", true);
          b2.putSerializable("packagePath", tempPackagePath);
          b2.putString("packageID", packageID);
          step2.setArguments(b2);
          return step2;
        } else {
          final WebViewFragment step2 = CreateWebView(position, FileUtils.WELCOME_HTM);
          return step2;
        }
      case 2:
        final WebViewFragment step3 = CreateWebView(position, FileUtils.WELCOME_HTM);
        return step3;
    }
    return null;
  }

  /**
   * Determine the number of states for the StepperAdapter
   * !isInstallingPackage:
   * 1 dot: keyboard package already installed, so just use SelectLanguageFragment
   *
   * Keyboard package:
   * 1 dot: package has one language, no welcome.htm.
   * 2 dots: package has one language, has welcome.htm.
   * 2 dots: package has multiple languages but no welcome.htm.
   * 3 dots: package has multiple languages and welcome.htm.
   *
   * Lexical Model package:
   * 2 dots
   * @return int
   */
  @Override
  public int getCount() {
    int count = 3;
    if (pkgTarget != null) {
      if (pkgTarget.equals(PackageProcessor.PP_LEXICAL_MODELS_KEY)) {
        count = 2;
      } else if (pkgTarget.equals(PackageProcessor.PP_KEYBOARDS_KEY)) {
        if (!isInstallingPackage) {
          count = 1;
        }
        else if (languageCount <= 1) {
          // One or less language
          count = (!hasWelcome) ? 1 : 2;
        } else {
          // Multiple languages
          count = (!hasWelcome) ? 2 : 3;
        }
      }
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
        if (getCount() <= 2) {
          // If only 1-2 steps, use "INSTALL"
          buttonLabel = context.getString(R.string.label_install);
        }
        return new StepViewModel.Builder(context)
          .setEndButtonLabel(buttonLabel)
          .create();
      case 1:
        // Language picker EndButton uses "INSTALL" button
        buttonLabel = context.getString(R.string.label_install);
        boolean backButtonVisible = true;
        if (getCount() == 2) {
          // If only two steps, use "OK" and hide back button
          buttonLabel = context.getString(R.string.label_ok);
          backButtonVisible = false;
        }
        return new StepViewModel.Builder(context)
          .setEndButtonLabel(buttonLabel)
          .setBackButtonVisible(backButtonVisible)
          .create();
      case 2:
        // Adapter style defaults last step EndButton to "OK". Hide back button
        return new StepViewModel.Builder(context)
          .setBackButtonVisible(false)
          .create();
    }
    return null;
  }

  /**
   * Utility to create WebViewFragment
   * @param position int position in the adapter
   * @param fileName FileUtils.README_HTM or FileUtils.WELCOME_HTM
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