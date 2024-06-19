package com.keyman.engine.cloud;

import android.content.Context;

import com.keyman.engine.cloud.CloudApiTypes;
import com.keyman.engine.cloud.CloudDownloadMgr;

/**
 * Interface for {@link CloudDownloadMgr} as callback to do use case specific task.
 * @param <ModelType> the model objects type
 * @param <ResultType> the result type of the download
 */
public interface ICloudDownloadCallback<ModelType,ResultType> {

  /**
   * Initialize callback using context.
   * @param context the context
   */
  void initializeContext(Context context);

  /**
   * extract download result object from download set
   * @param aContext the context
   * @param aDownload the download
   * @return the result
   */
  ResultType extractCloudResultFromDownloadSet(Context aContext, CloudApiTypes.CloudDownloadSet<ModelType,ResultType> aDownload);

  /**
   * Apply download results to target model.
   * @param aContext the context
   * @param aModel the model
   * @param aCloudResult the result
   */
  void applyCloudDownloadToModel(Context aContext, ModelType aModel, ResultType aCloudResult);
}
