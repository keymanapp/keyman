package com.tavultesoft.kmea.data;

import android.app.DownloadManager;

import androidx.annotation.NonNull;

import org.json.JSONArray;
import org.json.JSONObject;

import java.io.File;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

public class CloudApiTypes {
  protected static class CloudApiReturns {

    public final ApiTarget target;
    public final JSONArray jsonArray;
    public final JSONObject jsonObject;

    public CloudApiReturns(ApiTarget target, JSONArray jsonArray) {
      this.target = target;
      this.jsonArray = jsonArray;
      this.jsonObject = null;
    }

    public CloudApiReturns(ApiTarget target, JSONObject jsonObject) {
      this.target = target;
      this.jsonArray = null;
      this.jsonObject = jsonObject;
    }

  }

  protected enum ApiTarget {
    Keyboards,
    LexicalModels
  }

  protected enum JSONType {
    Array,
    Object
  }

  protected static class CloudApiParam {
    public final ApiTarget target;
    public final String url;
    public final JSONType type;

    CloudApiParam(ApiTarget target, String url, JSONType type) {
      this.target = target;
      this.url = url;
      this.type = type;
    }
  }

  public static class SingleCloudDownload
  {
    private DownloadManager.Request request;
    private boolean downloadFinished =false;
    private long downloadId;
    private File destinationFile;
    private CloudApiTypes.JSONType type;
    private CloudApiTypes.ApiTarget target;

    public SingleCloudDownload(DownloadManager.Request aRequest,File aDestinationFile)
    {
      request = aRequest;
      destinationFile = aDestinationFile;
    }
    public SingleCloudDownload setDownloadId(long downloadId) {
      this.downloadId = downloadId;
      return this;
    }

    public SingleCloudDownload setJsonType(JSONType type) {
      this.type = type;
      return this;
    }

    public SingleCloudDownload setTarget(ApiTarget target) {
      this.target = target;
      return this;
    }

    public DownloadManager.Request getRequest() {
      return request;
    }

    public long getDownloadId() {
      return downloadId;
    }

    public File getDestinationFile() {
      return destinationFile;
    }

    public JSONType getType() {
      return type;
    }

    public ApiTarget getTarget() {
      return target;
    }
  }

  /**
   * Typed Download sets for cloud download.
   * @param <ModelType> the model objects type
   * @param <ResultType> the result type of the download
   */
  public static class CloudDownloadSet<ModelType,ResultType> {
    private String downloadIdentifier;
    private ModelType targetModel;
    private LinkedList<SingleCloudDownload> downloads = new LinkedList<>();

    private ICloudDownloadCallback<ModelType,ResultType> callback;

    private boolean resultsReady = false;

    //TODO: maybe implement a max lifetime for downloads
    //private long startingTime = System.currentTimeMillis();

    public CloudDownloadSet(@NonNull String aDownloadIdentifier, ModelType theTargetObject)
    {
      targetModel = theTargetObject;
      downloadIdentifier = aDownloadIdentifier;
    }

    protected boolean hasOpenDownloads() {
      synchronized (downloads) {
        for (SingleCloudDownload _d : downloads) {
          if (!_d.downloadFinished)
            return true;
        }
        return false;
      }
    }

    void addDownload(SingleCloudDownload aDownload) {
      synchronized (downloads) {
        if (resultsReady)
          throw new IllegalStateException("Could not add download to an already processed download set");

        downloads.add(aDownload);
      }
    }

    public ICloudDownloadCallback getCallback() {
      return callback;
    }

    public void setCallback(ICloudDownloadCallback<ModelType,ResultType> callback) {
      this.callback = callback;
    }

    public boolean isResultsReady() {
      return resultsReady;
    }

    public void setResultsReady() {
      this.resultsReady = true;
    }

    public String getDownloadIdentifier() {
      return downloadIdentifier;
    }

    public ModelType getTargetModel() {
      return targetModel;
    }

    void setDone(long aDownload) {
      synchronized (downloads) {
        if (resultsReady)
          throw new IllegalStateException("Download is already ready");

        for (SingleCloudDownload _d : downloads) {
          if (_d.downloadId == aDownload) {
            _d.downloadFinished = true;
            return;
          }
        }
        return;
      }


    }

    public List<SingleCloudDownload> getSingleDownloads() {
      return Collections.unmodifiableList(downloads);
    }
  }
}
