/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmapro;

import com.tavultesoft.kmapro.R;
import com.tavultesoft.kmea.KMManager;

import android.app.ActionBar;
import android.os.Bundle;
import android.support.v4.app.FragmentActivity;

public class FBShareActivity extends FragmentActivity {
  private FBShareFragment fbShareFragment;

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);

    final ActionBar actionBar = getActionBar();
    actionBar.setLogo(R.drawable.keyman_logo);
    actionBar.setDisplayShowTitleEnabled(false);
    actionBar.setBackgroundDrawable(MainActivity.getActionBarDrawable(this));

    Bundle extras = getIntent().getExtras();
    if (extras != null) {
      FBShareFragment.messageText = extras.getString("messageText");
      FBShareFragment.messageTextSize = extras.getFloat("messageTextSize");
      FBShareFragment.messageTextTypeface = KMManager.getFontTypeface(getApplicationContext(), extras.getString("messageTextTypeface"));
    }

    if (savedInstanceState == null) {
      // Add the fragment on initial activity setup
      fbShareFragment = new FBShareFragment();
      getSupportFragmentManager().beginTransaction().add(android.R.id.content, fbShareFragment).commit();
    } else {
      // Or set the fragment from restored state info
      fbShareFragment = (FBShareFragment) getSupportFragmentManager().findFragmentById(android.R.id.content);
    }
  }
}