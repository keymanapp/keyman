package com.keyman.android;

import android.app.IntentService;
import android.content.Intent;
import android.os.Bundle;
import android.os.ResultReceiver;

import com.keyman.engine.KmpInstallMode;
import com.keyman.engine.util.FileUtils;
import com.keyman.engine.util.KMLog;

public class DownloadIntentService extends IntentService {
  private static final String TAG = "DownloadIntentSvc";
  private boolean cancelDownload = false; // If download cancelled, don't install

  public DownloadIntentService() {
    super(DownloadIntentService.class.getName());
  }

  @Override
  protected void onHandleIntent(Intent intent) {
    String url = intent.getStringExtra("url");
    String filename = intent.getStringExtra("filename");
    String destination = intent.getStringExtra("destination");
    String languageID = intent.getStringExtra("language");
    KmpInstallMode installMode = KmpInstallMode.fromString(intent.getStringExtra("installMode"));
    final ResultReceiver receiver = intent.getParcelableExtra("receiver");
    Bundle bundle = new Bundle();

    try {
      int result = FileUtils.download(getApplicationContext(), url, destination, filename);
      if (result == FileUtils.DOWNLOAD_SUCCESS) {
        if (!this.cancelDownload) {
          bundle.putString("filename", filename);
          bundle.putString("destination", destination);
          bundle.putString("language", languageID);
          bundle.putString("installMode", installMode.toString());
          receiver.send(FileUtils.DOWNLOAD_SUCCESS, bundle);
        } else {
          receiver.send(FileUtils.DOWNLOAD_CANCELLED, bundle);
        }
      } else {
        receiver.send(FileUtils.DOWNLOAD_ERROR, bundle);
      }
    } catch (Exception e) {
      receiver.send(FileUtils.DOWNLOAD_ERROR, Bundle.EMPTY);
      KMLog.LogException(TAG, "", e);
    }
  }

  @Override
  public void onDestroy() {
    super.onDestroy();
    this.cancelDownload = true;
  }
}
