/**
 * Copyright (C) 2021 SIL International. All rights reserved.
 */

package com.keyman.engine.util;

import android.content.ClipData;
import android.content.ClipDescription;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.widget.Toast;

import androidx.core.content.FileProvider;

import com.keyman.engine.BaseActivity;
import com.keyman.engine.KMManager;
import com.keyman.engine.R;
import com.keyman.engine.util.FileProviderUtils;
import com.keyman.engine.util.FileUtils;

import java.io.File;
import java.io.FileFilter;

public final class HelpFile {
  private static final String TAG = "HelpFile";

  /**
   * Utility to pass a pdf help file for Intent.ACTION_VIEW
   * @param context
   * @param helpFile Full path string of the pdf file to view
   * @param packageID String of the package ID
   * @return Intent
   */
  public static Intent toActionView(Context context, String helpFile, String packageID) {
    Intent i = new Intent(Intent.ACTION_VIEW);

    if (FileUtils.hasPDFExtension(helpFile) && ! KMManager.isTestMode()) {
      File customHelp = new File(new File(helpFile).getAbsolutePath());
      i.setFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION);
      // Starting with Android N, you can't pass file:// to intents, so we use FileProvider
      try {
        final String authority = FileProviderUtils.getAuthority(context);
        Uri contentUri = FileProvider.getUriForFile(
          context, authority, customHelp);
        i.setDataAndType(contentUri, "application/pdf");

        i.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        i.addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION);
        i.addFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);
      } catch (NullPointerException e) {
        // Localize Toast error
        BaseActivity.makeToast(context, R.string.fileprovider_undefined, Toast.LENGTH_LONG);

        // Don't localize message to Sentry
        String message = String.format(context.getString(R.string.fileprovider_undefined), customHelp.toString());
        KMLog.LogException(TAG, message, e);
      }
    } else {
      i.setData(Uri.parse(helpFile));
    }
    return i;
  }

}
