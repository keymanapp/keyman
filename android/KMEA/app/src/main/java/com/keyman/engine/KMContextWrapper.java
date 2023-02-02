/**
 * Copyright (C) 2018 SIL International. All rights reserved.
 */

package com.keyman.engine;

import android.content.Context;
import android.content.ContextWrapper;
import androidx.appcompat.app.AppCompatActivity;

public class KMContextWrapper extends ContextWrapper {

  public KMContextWrapper(Context base) {
    super(base);
  }

  public AppCompatActivity getAppCompatActivity() {
    return (AppCompatActivity)getApplicationContext();
  }
}
