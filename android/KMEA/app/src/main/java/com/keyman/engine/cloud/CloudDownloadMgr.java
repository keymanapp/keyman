package com.keyman.engine.cloud;

import android.app.DownloadManager;
import android.content.ActivityNotFoundException;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Build;
import android.provider.Settings;
import android.util.Log;

import com.keyman.engine.util.KMLog;

import java.io.File;
import java.util.HashMap;
import java.util.List;

/**
 * Central manager for cloud downloads.
 */
public class CloudDownloadMgr{
  private static final String TAG = "CloudDownloadMgr";
  private static CloudDownloadMgr instance;
  private static final String DOWNLOAD_MANAGER = "com.android.providers.downloads";

  /**
   *
   * @return get or create shared instance.
   */
  public static CloudDownloadMgr getInstance()
  {
    if(instance==null)
      createInstance();
    return instance;
  }

  /**
   * create singleton instance.
   */
  private synchronized static void createInstance()
  {
    if(instance!=null)
      return;
    KMLog.LogBreadcrumb("CloudDownloadMgr", "CloudDownloadMgr.createInstance() - first call", true);
    instance = new CloudDownloadMgr();
  }

  /**
   * Marks that the download receiver is already appended to the main context.
   */
  boolean isInitialized = false;
  int state;

  private HashMap<Long,String> internalDownloadIdToDownloadIdentifier = new HashMap<>();
  private HashMap<String, CloudApiTypes.CloudDownloadSet> downloadSetByDownloadIdentifier = new HashMap<>();

  /**
   * Append downloadreceiver to the main context.
   * @param aContext the context
   */
  public synchronized void initialize(Context aContext)
  {
    if(isInitialized)
      return;
    try {
      KMLog.LogBreadcrumb("CloudDownloadMgr", "attempting CloudDownloadMgr.initialize()", true);
      // Runtime-registered boradcasts receivers must specify export behavior to indicate whether
      // or not the receiver should be exported to all other apps on the device
      // https://developer.android.com/about/versions/14/behavior-changes-14#runtime-receivers-exported
      if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
        aContext.registerReceiver(completeListener, new IntentFilter(DownloadManager.ACTION_DOWNLOAD_COMPLETE),
          Context.RECEIVER_EXPORTED);
      } else {
        aContext.registerReceiver(completeListener, new IntentFilter(DownloadManager.ACTION_DOWNLOAD_COMPLETE));
      }
    } catch (IllegalArgumentException e) {
      String message = "initialize error: ";
      KMLog.LogException(TAG, message, e);
    }

    // Check if DownloadManager is enabled
    // https://github.com/keymanapp/keyman.com/issues/385
    state = aContext.getPackageManager().getApplicationEnabledSetting(DOWNLOAD_MANAGER);
    boolean override = true; // test code
    if (override) {
      // DownloadManager disabled
      KMLog.LogBreadcrumb(TAG, DOWNLOAD_MANAGER + " is disabled", true);

      try {
        // Attempt to open the DownloadManager Info page
        // We want the user to enable it
        Intent intent = new Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS);
        intent.setData(Uri.parse("package:" + DOWNLOAD_MANAGER));
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        aContext.startActivity(intent);
      } catch (ActivityNotFoundException e) {
        // Open the generic Apps page
        Intent intent = new Intent(android.provider.Settings.ACTION_MANAGE_APPLICATIONS_SETTINGS);
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        aContext.startActivity(intent);
      }
    }

    isInitialized = true;
    KMLog.LogBreadcrumb("CloudDownloadMgr", ".initialize() call complete", false);
  }

  /**
   * Remove downloadreceiver from the main context.
   * @param aContext the context
   */
  public synchronized void shutdown(Context aContext)
  {
    if(!isInitialized) {
      return;
    }

    KMLog.LogBreadcrumb("CloudDownloadMgr", "CloudDownloadMgr.shutdown()", true);

    try {
      aContext.unregisterReceiver(completeListener);
    } catch (IllegalArgumentException e) {
      String message = "shutdown error: ";
      KMLog.LogException(TAG, message, e);
    }
    isInitialized = false;
  }

  /**
   * callback for finished downloads.
   */
  BroadcastReceiver completeListener = new BroadcastReceiver() {

    @Override
    public void onReceive(Context context, Intent intent) {
      //Fetching the download id received with the broadcast
      long id = intent.getLongExtra(DownloadManager.EXTRA_DOWNLOAD_ID, -1);
      //Checking if the received broadcast is for our enqueued download by matching download id
      downloadCompleted(context,id);
    }
  };

  /**
   * Determine if DownloadManager is enabled at the device level.
   * Reference: https://stackoverflow.com/questions/30671548/unknown-url-content-downloads-my-downloads
   * @return boolean - true if the DownloadManager is enabled to access downloads
   */
  private boolean downloadManagerEnabled() {
    return (state == PackageManager.COMPONENT_ENABLED_STATE_DISABLED ||
      state == PackageManager.COMPONENT_ENABLED_STATE_DISABLED_USER ||
      state == PackageManager.COMPONENT_ENABLED_STATE_DISABLED_UNTIL_USED) ?
      false :
      true;
  }

  /**
   * find download set by the android internal download id.
   * @param anInternalDownloadId the internal download id
   * @return the result
   */
  private CloudApiTypes.CloudDownloadSet getDownloadSetForInternalDownloadId(long anInternalDownloadId)
  {
    String _downloadset_id = internalDownloadIdToDownloadIdentifier.get(anInternalDownloadId);
    if(_downloadset_id==null)
      return null;
    return downloadSetByDownloadIdentifier.get(_downloadset_id);
  }

  /**
   * clean up a download.
   * @param anIdenitifer the identifier
   */
  private void removeDownload(String anIdenitifer)
  {
    synchronized (downloadSetByDownloadIdentifier)
    {
      CloudApiTypes.CloudDownloadSet _downloadset = downloadSetByDownloadIdentifier.remove(anIdenitifer);
      if (_downloadset == null)
        return;
      List<CloudApiTypes.SingleCloudDownload> _children = _downloadset.getSingleDownloads();
      for (CloudApiTypes.SingleCloudDownload _d : _children)
        internalDownloadIdToDownloadIdentifier.remove(_d.getDownloadId());
    }
  }

  /**
   *  Executed when a single download is completed.
   *  If the whole download set is completed the download results will be processed.
   * @param aContext the context
   * @param anInternalDownloadId an internal id
   */
  private void downloadCompleted(Context aContext,long anInternalDownloadId)
  {
    synchronized (downloadSetByDownloadIdentifier) {

      CloudApiTypes.CloudDownloadSet _parentSet = getDownloadSetForInternalDownloadId(anInternalDownloadId);
      if(_parentSet==null) {
        // Download ID didn't match, so nothing to cleanup
        return;
      }
      _parentSet.setDone(anInternalDownloadId);
      if(!_parentSet.hasOpenDownloads())
      {
        processDownloadSet(aContext, _parentSet);
        removeDownload(_parentSet.getDownloadIdentifier());
      }
    }
  }

  /**
   * called after finishing the download.
   * @param aContext the context
   * @param aDownloadSet the download set
   * @param <ModelType> the target models type
   * @param <ResultType> the cloud requests result type
   */
  private <ModelType,ResultType> void processDownloadSet(Context aContext, CloudApiTypes.CloudDownloadSet<ModelType,ResultType> aDownloadSet)
  {
    aDownloadSet.setResultsReady();

    ICloudDownloadCallback<ModelType,ResultType> _callback = aDownloadSet.getCallback();

    ResultType jsonTuple = _callback
      .extractCloudResultFromDownloadSet(aContext, aDownloadSet);

    _callback.applyCloudDownloadToModel(aContext,aDownloadSet.getTargetModel(),jsonTuple);
  }

  /**
   * check if download is already running
   * @param anDownloadIdentifier an identifier to prevent duplicate downloading
   * @return the result
   */
  public boolean alreadyDownloadingData(String anDownloadIdentifier)
  {
    return downloadSetByDownloadIdentifier.containsKey(anDownloadIdentifier);
  }

  /**
   * execute download in background.
   * @param aContext the context
   * @param aDownloadIdentifier  an identifier to prevent duplicate downloading
   * @param aTargetModel the target model
   * @param aCallback the callback
   * @param params the cloud api params for download
   * @param <ModelType> the target models type
   * @param <ResultType> the cloud requests result type
   */
  public <ModelType,ResultType> void executeAsDownload(Context aContext, String aDownloadIdentifier,
                                ModelType aTargetModel,
                                ICloudDownloadCallback<ModelType,ResultType> aCallback,
                                CloudApiTypes.CloudApiParam... params)
  {
    if(!isInitialized) {
      Log.w(TAG, "DownloadManager not initialized. Initializing CloudDownloadMgr.");
      initialize(aContext);
    } else {
      KMLog.LogBreadcrumb("CloudDownloadMgr", "CloudDownloadMgr.executeAsDownload() called; already initialized", true);
    }

    synchronized (downloadSetByDownloadIdentifier) {

      if (alreadyDownloadingData(aDownloadIdentifier) || params == null) {
        return;
      }

      if (!downloadManagerEnabled()) {
        return;
      }

      DownloadManager downloadManager = (DownloadManager) aContext.getSystemService(Context.DOWNLOAD_SERVICE);
      if(downloadManager==null)
        throw new IllegalStateException("DownloadManager is not available");

      aCallback.initializeContext(aContext);

      CloudApiTypes.CloudDownloadSet<ModelType,ResultType> _downloadSet =
        new CloudApiTypes.CloudDownloadSet<ModelType,ResultType>(
        aDownloadIdentifier,aTargetModel);
      _downloadSet.setCallback(aCallback);

      downloadSetByDownloadIdentifier.put(aDownloadIdentifier,_downloadSet);

      for(int _i=0;_i<params.length;_i++)
      {
        CloudApiTypes.SingleCloudDownload _download = createRequest(params[_i]);
        _download.setDownloadId(downloadManager.enqueue(_download.getRequest()));// enqueue puts the download request in the queue.
        internalDownloadIdToDownloadIdentifier.put(_download.getDownloadId(),aDownloadIdentifier);
        _downloadSet.addDownload(_download);
      }
    }

  }

  /**
   * create request from api param.
   * @param aParam the api parameter
   * @return the single download
   */
  private CloudApiTypes.SingleCloudDownload createRequest(CloudApiTypes.CloudApiParam aParam)
  {
    /*
     * Create a DownloadManager.Request with all the information necessary to start the download
     * Not using setDestinationUri so we can avoid requesting WRITE_EXTERNAL_STORAGE permission
     * https://developer.android.com/reference/android/app/DownloadManager.Request#setDestinationUri(android.net.Uri)
     */
    DownloadManager.Request _request=new DownloadManager.Request(Uri.parse(aParam.url))
      .setTitle("Cloud Download " + aParam.target)
      .setDescription("Downloading " + aParam.target)
      .setNotificationVisibility(DownloadManager.Request.VISIBILITY_VISIBLE); // Visibility of the download Notification
      // api level 24
      //.setRequiresCharging(false)// Set if charging is required to begin the download
      //.setAllowedOverMetered(true)// Set if download is allowed on Mobile network
      //.setAllowedOverRoaming(true);// Set if download is allowed on roaming network

    return new CloudApiTypes.SingleCloudDownload(_request)
      .setCloudParams(aParam);
  }
}
