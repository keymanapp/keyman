package com.tavultesoft.kmea.data;

import android.content.Context;

/**
 * Interface for {@link CloudDownloadMgr} as callback to do use case specific task.
 * @param <M> the model objects type
 * @param <R> the result type of the download
 */
public interface ICloudDownloadCallback<M,R> {


  /**
   * extract download result object from download set
   * @param aDownload the download
   * @return the result
   */
  R extractCloudResultFromDownloadSet(CloudApiTypes.CloudDownloadSet<M,R> aDownload);

  void applyCloudDownloadToModel(Context aContext, M aModel, R aCloudResult);
}
