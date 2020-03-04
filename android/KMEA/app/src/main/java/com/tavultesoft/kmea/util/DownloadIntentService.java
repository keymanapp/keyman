package com.tavultesoft.kmea.util;

import android.app.IntentService;
import android.content.Intent;
import android.os.Bundle;
import android.os.ResultReceiver;

public class DownloadIntentService extends IntentService {


  public DownloadIntentService() {
    super(DownloadIntentService.class.getName());
  }

  @Override
  protected void onHandleIntent(Intent intent) {
    String url = intent.getStringExtra("url");
    String filename = intent.getStringExtra("filename");
    String destination = intent.getStringExtra("destination");
    final ResultReceiver receiver = intent.getParcelableExtra("receiver");
    Bundle bundle = new Bundle();

    try {
      int result = FileUtils.download(getApplicationContext(), url, destination, filename);
      if (result == FileUtils.DOWNLOAD_SUCCESS) {
        bundle.putString("destination", destination);
        bundle.putString("filename", filename);
        receiver.send(FileUtils.DOWNLOAD_SUCCESS, bundle);
      }
    } catch (Exception e) {
      receiver.send(FileUtils.DOWNLOAD_ERROR, Bundle.EMPTY);
    }
  }
}
