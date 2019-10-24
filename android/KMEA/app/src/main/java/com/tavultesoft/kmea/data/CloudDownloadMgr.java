package com.tavultesoft.kmea.data;

import android.app.DownloadManager;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.net.Uri;
import android.net.UrlQuerySanitizer;
import android.util.JsonReader;
import android.util.Log;
import android.widget.Toast;

import com.tavultesoft.kmea.JSONParser;

import org.json.JSONArray;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.ObjectInputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

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

  private HashMap<Long,SingleCloudDownload> pendingCloudDownloads = new HashMap<>();

  private class SingleCloudDownload
  {
    DownloadManager.Request request;
    CloudApiTypes.CloudApiReturns api_return;
    boolean downloadfinished =false;
    long downloadId;
    CloudDownloadSet parentSet;
    File destiniationfile;
    CloudApiTypes.JSONType type;
    CloudApiTypes.ApiTarget target;

  }

  private class CloudDownloadSet
  {
    private LinkedList<SingleCloudDownload> downloads = new LinkedList<>();

    private CloudApiDownloadCallback callback;

    private boolean resultsAreProcessed = false;

    private long startingTime = System.currentTimeMillis();

    protected boolean hasOpenDownloads()
    {
      synchronized (downloads)
      {
          for (SingleCloudDownload _d : downloads) {
            if (!_d.downloadfinished)
              return true;
          }
          return false;
      }
    }

    private void addDownload(SingleCloudDownload aDownload)
    {
      synchronized (downloads)
      {
        if (resultsAreProcessed)
          throw new IllegalStateException("Could not add download to an allready processed download set");

        downloads.add(aDownload);
      }
    }

    private void setDone(long aDownload)
    {
      synchronized (downloads)
      {
        if (resultsAreProcessed)
          throw new IllegalStateException("Download is already processed");

        for (SingleCloudDownload _d : downloads) {
          if (_d.downloadId==aDownload)
          {
            _d.downloadfinished=true;
            return;
          }
        }
        return;
      }
    }

    protected CloudApiTypes.CloudDownloadReturns getDownloadReturns() {


      List<CloudApiTypes.CloudApiReturns> retrievedJSON = new ArrayList<>(downloads.size());



      for (SingleCloudDownload _d : downloads) {
        JSONParser jsonParser = new JSONParser();
        JSONArray dataArray = null;
        JSONObject dataObject = null;

        if(_d.destiniationfile!=null&& _d.destiniationfile.length()>0)
        {
          try {

            Object _o = jsonParser.getJSONObjectFromFile(_d.destiniationfile);
            if (_d.type == CloudApiTypes.JSONType.Array) {
               dataArray = (JSONArray) _o;
            } else {
              dataObject = (JSONObject)_o;
            }
          } catch (Exception e) {
            Log.d(CloudRepository.TAG, e.getMessage());
          }
          finally
          {
            _d.destiniationfile.delete();
          }
        } else {
          // Offline trouble!  That said, we can't get anything, so we simply shouldn't add anything.
        }

        if (_d.type == CloudApiTypes.JSONType.Array) {
          retrievedJSON.add(new CloudApiTypes.CloudApiReturns(_d.target, dataArray));  // Null if offline.
        } else {
          retrievedJSON.add(new CloudApiTypes.CloudApiReturns(_d.target, dataObject)); // Null if offline.
        }

      }

      return new CloudApiTypes.CloudDownloadReturns(retrievedJSON); // Will report empty arrays/objects if offline.
    }

  }

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


  public void downloadCompleted(Context aContext,long aDownloadId)
  {
    synchronized (pendingCloudDownloads) {
      SingleCloudDownload _result = pendingCloudDownloads.remove(aDownloadId);
      if (_result != null) {
        CloudDownloadSet _parentSet = _result.parentSet;
        _parentSet.setDone(aDownloadId);
        if(!_parentSet.hasOpenDownloads())
        {
          processDownloadSet(aContext, _parentSet);
        }

      }
    }
  }

  private void processDownloadSet(Context aContext, CloudDownloadSet aParentSet) {
    aParentSet.resultsAreProcessed = true;

    CloudApiTypes.CloudDownloadReturns jsonTuple = aParentSet.getDownloadReturns();

    CloudApiDownloadCallback _callback = aParentSet.callback;
    _callback.saveDataToCache(jsonTuple);

    _callback.ensureInitCloudReturn(aContext,jsonTuple);

    _callback.processCloudReturns(jsonTuple,true);
  }

  public boolean alreadyDownloadingData(Context aContext)
  {
    return !pendingCloudDownloads.isEmpty();
  }

  public void executeAsDownload(Context aContext, CloudApiDownloadCallback aCallback, CloudApiTypes.CloudApiParam... params)
  {
    if(!isInitialized)
      initializeReceiver(aContext);

    synchronized (pendingCloudDownloads) {

      if(alreadyDownloadingData(aContext))
        return;

      DownloadManager downloadManager = (DownloadManager) aContext.getSystemService(Context.DOWNLOAD_SERVICE);

      CloudDownloadSet _downloadSet = new CloudDownloadSet();
      _downloadSet.callback=aCallback;

      for(int _i=0;_i<params.length;_i++)
      {
        SingleCloudDownload _download = createRequest(aContext,_i,params[_i]);
        _download.downloadId = downloadManager.enqueue(_download.request);// enqueue puts the download request in the queue.
        _download.parentSet = _downloadSet;
        pendingCloudDownloads.put(_download.downloadId,_download);
        _downloadSet.addDownload(_download);
      }

    }

  }

  public SingleCloudDownload createRequest(Context aContext, int aNo,CloudApiTypes.CloudApiParam aParam)
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

    SingleCloudDownload _download= new SingleCloudDownload();
    _download.request = _request;
    _download.destiniationfile = _file;
    _download.type = aParam.type;
    _download.target = aParam.target;
    return _download;
  }
}
