/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */
package com.tavultesoft.kmapro;

import android.app.ProgressDialog;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.ResultReceiver;
import android.widget.Toast;

import com.tavultesoft.kmea.util.FileUtils;

import java.io.File;

public class DownloadResultReceiver extends ResultReceiver {
  private Context context;
  private ProgressDialog progressDialog;

  public DownloadResultReceiver(Handler handler, Context context) {
    super(handler);

    this.context = context;
  }

  public void setProgressDialog(ProgressDialog progressDialog) {
    this.progressDialog = progressDialog;
  }

  @Override
  protected void onReceiveResult(int resultCode, Bundle resultData) {
    if (progressDialog != null && progressDialog.isShowing()) {
      progressDialog.dismiss();
    };
    progressDialog = null;
    switch(resultCode) {
      case FileUtils.DOWNLOAD_ERROR :
        Toast.makeText(context, "Download failed",
          Toast.LENGTH_SHORT).show();
        break;
      case FileUtils.DOWNLOAD_SUCCESS :
        String downloadedFilename = resultData.getString("filename");
        String languageID = resultData.getString("language");
        String kmpFilename = resultData.getString("destination") + File.separator + downloadedFilename;

        Bundle bundle = new Bundle();
        bundle.putString("kmpFile", kmpFilename);
        bundle.putString("language", languageID);
        Intent packageIntent = new Intent(context, PackageActivity.class);
        packageIntent.putExtras(bundle);
        context.startActivity(packageIntent);
        break;
    }
    super.onReceiveResult(resultCode, resultData);
  }
}