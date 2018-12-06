/**
 * Copyright (C) 2018 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import android.content.Context;
import android.content.ContextWrapper;
import android.support.v7.app.AppCompatActivity;

public class KMContextWrapper extends ContextWrapper {

  public KMContextWrapper(Context base) {
    super(base);
  }

  public AppCompatActivity getAppCompatActivity() {
    return (AppCompatActivity)getApplicationContext();
  }
}
