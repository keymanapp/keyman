package com.tavultesoft.kmea.cloud;

import android.app.DownloadManager;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.net.Uri;
import android.util.Log;

import com.tavultesoft.kmea.util.KMLog;

import java.io.File;
import java.util.HashMap;
import java.util.List;

/**
 * Central manager for cloud downloads.
 */
public class CloudDownloadMgr{
  private static final String TAG = "CloudDownloadMgr";
  private static CloudDownloadMgr instance;

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
    instance = new CloudDownloadMgr();
  }

  /**
   * Marks that the download receiver is already appended to the main context.
   */
  boolean isInitialized = false;

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
      aContext.registerReceiver(completeListener, new IntentFilter(DownloadManager.ACTION_DOWNLOAD_COMPLETE));
    } catch (IllegalStateException e) {
      String message = "initialize error: ";
      KMLog.LogException(TAG, message, e);
    }
    isInitialized = true;
  }

  /**
   * Remove downloadreceiver from the main context.
   * @param aContext the context
   */
  public synchronized void shutdown(Context aContext)
  {
    if(!isInitialized)
      return;
    try {
      aContext.unregisterReceiver(completeListener);
    } catch (IllegalStateException e) {
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
        KMLog.LogError(TAG, "Download with ID " + anInternalDownloadId + " is not available");
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
      Log.w(TAG, "Downloadmanager not initialized. Initializing CloudDownloadMgr.");
      initialize(aContext);
    }

    synchronized (downloadSetByDownloadIdentifier) {

      if (alreadyDownloadingData(aDownloadIdentifier) || params == null) {
        return;
      }

      DownloadManager downloadManager = (DownloadManager) aContext.getSystemService(Context.DOWNLOAD_SERVICE);
      if(downloadManager==null)
        throw new IllegalStateException("Downloadmanager is not available");

      aCallback.initializeContext(aContext);

      CloudApiTypes.CloudDownloadSet<ModelType,ResultType> _downloadSet =
        new CloudApiTypes.CloudDownloadSet<ModelType,ResultType>(
        aDownloadIdentifier,aTargetModel);
      _downloadSet.setCallback(aCallback);

      downloadSetByDownloadIdentifier.put(aDownloadIdentifier,_downloadSet);

      for(int _i=0;_i<params.length;_i++)
      {
        CloudApiTypes.SingleCloudDownload _download = createRequest(aContext,_i,params[_i]);
        _download.setDownloadId(downloadManager.enqueue(_download.getRequest()));// enqueue puts the download request in the queue.
        internalDownloadIdToDownloadIdentifier.put(_download.getDownloadId(),aDownloadIdentifier);
        _downloadSet.addDownload(_download);
      }
    }

  }

  /**
   * create request from api param.
   * @param aContext the context
   * @param aNo the number
   * @param aParam the api parameter
   * @return the single download
   */
  private CloudApiTypes.SingleCloudDownload createRequest(Context aContext, int aNo, CloudApiTypes.CloudApiParam aParam)
  {

    // From DownloadManager documentation:
    // https://developer.android.com/reference/android/app/DownloadManager.Request#setDestinationUri(android.net.Uri)
    // Must be a file to a path on external storage, and the calling app must have the WRITE_EXTERNAL_STORAGE permission
    //File _file=new File(aContext.getExternalFilesDir(null),"download_"+System.currentTimeMillis()+"_"+aNo);
       /*
       Create a DownloadManager.Request with all the information necessary to start the download
        */
    DownloadManager.Request _request=new DownloadManager.Request(Uri.parse(aParam.url))
      .setTitle("Cloud Download " + aParam.target)
      .setDescription("Downloading " + aParam.target)
      .setNotificationVisibility(DownloadManager.Request.VISIBILITY_VISIBLE);// Visibility of the download Notification
      //.setDestinationUri(Uri.fromFile(_file));// Uri of the destination file
      // api level 24
      //.setRequiresCharging(false)// Set if charging is required to begin the download
      //.setAllowedOverMetered(true)// Set if download is allowed on Mobile network
      //.setAllowedOverRoaming(true);// Set if download is allowed on roaming network

    return new CloudApiTypes.SingleCloudDownload(_request)
      .setCloudParams(aParam);
  }
}
