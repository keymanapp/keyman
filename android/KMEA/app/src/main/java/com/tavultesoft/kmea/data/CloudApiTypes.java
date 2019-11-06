package com.tavultesoft.kmea.data;

import android.app.DownloadManager;

import androidx.annotation.NonNull;

import org.json.JSONArray;
import org.json.JSONObject;

import java.io.File;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

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

  public enum ApiTarget {
    /**
     * Catalog download: available keyboards including meta data.
     */
    Keyboards,
    /**
     * Catalog download: available lexical models including meta data.
     */
    LexicalModels,
    /**
     * Keyboard download: keyboard meta data for the selected keyboard.
     */
    Keyboard,
    /**
     * Keyboard download: lexical models meta data for the language of the selected keyboard.
     */
    KeyBoardLexicalModels,
    /**
     *  Keyboard download: download keyboard data package and fonts.
     */
    KeyboardData,
    /**
     *  Lexical download: download lexical model package
     *  Used for single lexical model download and
     *  automatic lexical model download during keyboard download
     */
    LexicalModelPackage,
  }

  public enum JSONType {
    Array,
    Object
  }

  public static class CloudApiParam {
    public final ApiTarget target;
    public final String url;
    public JSONType type;
    private Map<String,Object> additionalProperties = new HashMap<>();

    public CloudApiParam(ApiTarget target, String url) {
      this.target = target;
      this.url = url;
    }

    public CloudApiParam setType(JSONType type) {
      this.type = type;
      return this;
    }
    public CloudApiParam setAdditionalProperty(String aProperty, Object aValue)
    {
      additionalProperties.put(aProperty,aValue);
      return this;
    }

    public <T> T getAdditionalProperty(String aProperty,Class<T> aType)
    {
      return (T)additionalProperties.get(aProperty);
    }
  }

  public static class SingleCloudDownload
  {
    private DownloadManager.Request request;
    private boolean downloadFinished =false;
    private long downloadId;
    private File destinationFile;
    private CloudApiParam cloudParams;

    public SingleCloudDownload(DownloadManager.Request aRequest,File aDestinationFile)
    {
      request = aRequest;
      destinationFile = aDestinationFile;
    }
    public SingleCloudDownload setDownloadId(long downloadId) {
      this.downloadId = downloadId;
      return this;
    }

    public SingleCloudDownload setCloudParams(CloudApiParam params) {
      this.cloudParams = params;
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

    public CloudApiParam getCloudParams() {
      return cloudParams;
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
