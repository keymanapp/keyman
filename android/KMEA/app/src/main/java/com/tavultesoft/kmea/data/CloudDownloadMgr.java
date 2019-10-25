package com.tavultesoft.kmea.data;

import android.app.DownloadManager;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.net.Uri;

import java.io.File;
import java.util.HashMap;
import java.util.List;

/**
 * Central manager for cloud downloads.
 */
public class CloudDownloadMgr{

  private static CloudDownloadMgr instance;

  public static CloudDownloadMgr getInstance()
  {
    if(instance==null)
      createInstance();
    return instance;
  }
  private synchronized static void createInstance()
  {
    if(instance!=null)
      return;
    instance = new CloudDownloadMgr();
  }

  boolean isInitialized = false;

  private HashMap<Long,String> internalDownloadIdToDownloadIdentifier = new HashMap<>();
  private HashMap<String, CloudApiTypes.CloudDownloadSet> downloadSetByDownloadIdentifier = new HashMap<>();

  private synchronized void initializeReceiver(Context aContext)
  {
    if(isInitialized)
      return;
    aContext.getApplicationContext().registerReceiver(completeListener,new IntentFilter(DownloadManager.ACTION_DOWNLOAD_COMPLETE));
    isInitialized = true;
  }

  BroadcastReceiver completeListener = new BroadcastReceiver() {

    @Override
    public void onReceive(Context context, Intent intent) {
      //Fetching the download id received with the broadcast
      long id = intent.getLongExtra(DownloadManager.EXTRA_DOWNLOAD_ID, -1);
      //Checking if the received broadcast is for our enqueued download by matching download id
      downloadCompleted(context,id);
    }
  };

  private CloudApiTypes.CloudDownloadSet getDownloadSetForInternalDownloadId(long anInternalDownloadId)
  {
    String _downloadset_id = internalDownloadIdToDownloadIdentifier.get(anInternalDownloadId);
    if(_downloadset_id==null)
      return null;
    return downloadSetByDownloadIdentifier.get(_downloadset_id);
  }

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

  private void downloadCompleted(Context aContext,long aDownloadId)
  {
    synchronized (downloadSetByDownloadIdentifier) {

      CloudApiTypes.CloudDownloadSet _parentSet = getDownloadSetForInternalDownloadId(aDownloadId);
      _parentSet.setDone(aDownloadId);
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
   */
  private void processDownloadSet(Context aContext, CloudApiTypes.CloudDownloadSet aDownloadSet)
  {
    aDownloadSet.setResultsAreProcessing(true);

    ICloudDownloadCallback _callback = aDownloadSet.getCallback();

    Object jsonTuple = _callback
      .extractCloudResultFromDownloadSet(aDownloadSet);

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
   */
  public <M,R> void executeAsDownload(Context aContext, String aDownloadIdentifier,
                                M aTargetModel,
                                ICloudDownloadCallback<M,R> aCallback, CloudApiTypes.CloudApiParam... params)
  {
    if(!isInitialized)
      initializeReceiver(aContext);

    synchronized (downloadSetByDownloadIdentifier) {

      if(alreadyDownloadingData(aDownloadIdentifier))
        return;

      DownloadManager downloadManager = (DownloadManager) aContext.getSystemService(Context.DOWNLOAD_SERVICE);

      CloudApiTypes.CloudDownloadSet _downloadSet = new CloudApiTypes.CloudDownloadSet(
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

    File _file=new File(aContext.getExternalFilesDir(null),"download_"+System.currentTimeMillis()+"_"+aNo);
       /*
       Create a DownloadManager.Request with all the information necessary to start the download
        */
    DownloadManager.Request _request=new DownloadManager.Request(Uri.parse(aParam.url))
      .setTitle("Cloud Download " + aParam.target)
      .setDescription("Downloading " + aParam.target)
      .setNotificationVisibility(DownloadManager.Request.VISIBILITY_VISIBLE)// Visibility of the download Notification
      .setDestinationUri(Uri.fromFile(_file));// Uri of the destination file
      // api level 24
      //.setRequiresCharging(false)// Set if charging is required to begin the download
      //.setAllowedOverMetered(true)// Set if download is allowed on Mobile network
      //.setAllowedOverRoaming(true);// Set if download is allowed on roaming network

    return new CloudApiTypes.SingleCloudDownload(_request,_file)
      .setJsonType(aParam.type)
      .setTarget(aParam.target);
  }
}
