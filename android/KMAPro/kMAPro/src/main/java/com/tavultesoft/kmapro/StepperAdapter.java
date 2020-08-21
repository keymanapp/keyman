/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */
package com.tavultesoft.kmapro;

import android.content.Context;
import android.os.Bundle;

import androidx.annotation.IntRange;
import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.stepstone.stepper.Step;
import com.stepstone.stepper.adapter.AbstractFragmentStepAdapter;
import com.stepstone.stepper.viewmodel.StepViewModel;
import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.data.Keyboard;
import com.tavultesoft.kmea.util.FileUtils;

import java.io.File;

public class StepperAdapter extends AbstractFragmentStepAdapter {
  private static final String CURRENT_STEP_POSITION_KEY = "messageResourceId";
  private File tempPackagePath;
  private String pkgTarget;
  private String pkgName;

  private String packageID;
  private Keyboard keyboard;

  public StepperAdapter(FragmentManager fm, Context context, File tempPackagePath,
                        String pkgTarget, String pkgID, String pkgName) {
    super(fm, context);
    this.tempPackagePath = tempPackagePath;
    this.pkgTarget = pkgTarget;
    this.packageID = pkgID;
    this.pkgName = pkgName;
  }

  @Override
  public Step createStep(int position) {
    switch (position){
      case 0:
        final WebViewFragment step1 = new WebViewFragment();
        Bundle b1 = new Bundle();
        b1.putInt(CURRENT_STEP_POSITION_KEY, position);
        b1.putSerializable("tempPackagePath", tempPackagePath);
        b1.putString("pkgTarget", pkgTarget);
        b1.putString("pkgName", pkgName);
        b1.putString("fileName", FileUtils.README_HTM);
        step1.setArguments(b1);
        return step1;
      case 1:
        final SelectLanguageFragment step2 = new SelectLanguageFragment();
        Bundle b2 = new Bundle();
        b2.putInt(CURRENT_STEP_POSITION_KEY, position);
        b2.putBoolean("tempPath", true);
        b2.putSerializable("packagePath", tempPackagePath);
        b2.putString("packageID", packageID);
        step2.setArguments(b2);
        return step2;
      case 2:
        final WebViewFragment step3 = new WebViewFragment();
        Bundle b3 = new Bundle();
        b3.putInt(CURRENT_STEP_POSITION_KEY, position);
        b3.putSerializable("tempPackagePath", tempPackagePath);
        b3.putString("pkgTarget", pkgTarget);
        b3.putString("pkgName", pkgName);
        b3.putString("fileName", FileUtils.WELCOME_HTM);
        step3.setArguments(b3);
        return step3;
    }
    return null;
  }
  @Override
  public int getCount() {
    return 3;
  }

  @NonNull
  @Override
  public StepViewModel getViewModel(@IntRange(from = 0) int position) {
    //Override this method to set Step title for the Tabs, not necessary for other stepper types
    switch (position){
      case 0:
        return new StepViewModel.Builder(context)
          .setTitle("Tabs 1") //can be a CharSequence instead
          .create();
      case 1:
        return new StepViewModel.Builder(context)
          // Language picker uses "INSTALL" button
          .setEndButtonLabel(context.getString(R.string.label_install))
          .create();
      case 2:
        return new StepViewModel.Builder(context)
          // Hide back button on welcome.htm page
          .setBackButtonVisible(false)
          .setTitle("Tabs 3") //can be a CharSequence instead
          .create();
    }
    return null;
  }
}