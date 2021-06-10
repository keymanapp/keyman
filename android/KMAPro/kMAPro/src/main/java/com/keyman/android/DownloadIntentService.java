package com.keyman.android;

import android.app.IntentService;
import android.content.Intent;
import android.os.Bundle;
import android.os.ResultReceiver;

import com.tavultesoft.kmea.util.FileUtils;
import com.tavultesoft.kmea.util.KMLog;

public class DownloadIntentService extends IntentService {
  private static final String TAG = "DownloadIntentSvc";

  public DownloadIntentService() {
    super(DownloadIntentService.class.getName());
  }

  @Override
  protected void onHandleIntent(Intent intent) {
    String url = intent.getStringExtra("url");
    String filename = intent.getStringExtra("filename");
    String destination = intent.getStringExtra("destination");
    String languageID = intent.getStringExtra("language");
    KmpInstallMode installMode = (KmpInstallMode) intent.getSerializableExtra("installMode");
    if(installMode == null) installMode = KmpInstallMode.Full;
    final ResultReceiver receiver = intent.getParcelableExtra("receiver");
    Bundle bundle = new Bundle();

    try {
      int result = FileUtils.download(getApplicationContext(), url, destination, filename);
      if (result == FileUtils.DOWNLOAD_SUCCESS) {
        bundle.putString("destination", destination);
        bundle.putString("filename", filename);
        bundle.putString("language", languageID);
        bundle.putSerializable("installMode", installMode);
        receiver.send(FileUtils.DOWNLOAD_SUCCESS, bundle);
      } else {
        receiver.send(FileUtils.DOWNLOAD_ERROR, bundle);
      }
    } catch (Exception e) {
      receiver.send(FileUtils.DOWNLOAD_ERROR, Bundle.EMPTY);
      KMLog.LogException(TAG, "", e);
    }
  }
}
