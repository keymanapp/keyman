package com.tavultesoft.kmea.cloud.impl;

import android.content.Context;
import android.widget.Toast;

import com.tavultesoft.kmea.KMKeyboardDownloaderActivity;
import com.tavultesoft.kmea.KeyboardEventHandler;
import com.tavultesoft.kmea.R;
import com.tavultesoft.kmea.cloud.CloudApiTypes;
import com.tavultesoft.kmea.cloud.ICloudDownloadCallback;
import com.tavultesoft.kmea.packages.LexicalModelPackageProcessor;
import com.tavultesoft.kmea.packages.PackageProcessor;
import com.tavultesoft.kmea.util.FileUtils;
import com.tavultesoft.kmea.util.KMLog;

import org.json.JSONException;

import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * Install lexical model.
 */
public class CloudLexicalPackageDownloadCallback implements ICloudDownloadCallback<
  Void, CloudKeyboardDownloadReturns>
{

  private static final String TAG = "CloudLexModelPKGDldCb";

  private File resourceRoot;
  private File cacheDir;

  @Override
  public void initializeContext(Context context)
  {
    resourceRoot = new File(context.getDir("data", Context.MODE_PRIVATE).toString() + File.separator);
    cacheDir = context.getCacheDir();
  }

  @Override
  public CloudKeyboardDownloadReturns extractCloudResultFromDownloadSet(
    CloudApiTypes.CloudDownloadSet<Void, CloudKeyboardDownloadReturns> aDownload)
  {
    LexicalModelPackageProcessor kmpProcessor = new LexicalModelPackageProcessor(resourceRoot);
    List<Map<String, String>> installedLexicalModels = null;

    int _result = FileUtils.DOWNLOAD_SUCCESS;
    for(CloudApiTypes.SingleCloudDownload _d:aDownload.getSingleDownloads())
    {
      if (_d.getDestinationFile() != null && _d.getDestinationFile().length() > 0)
      {

        try {
          if (_d.getCloudParams().target == CloudApiTypes.ApiTarget.LexicalModelPackage) {
            installedLexicalModels = new LinkedList<>();
            // Extract the kmp. Validate it contains only lexical models, and then process the lexical model package
            File kmpFile = new File(cacheDir, FileUtils.getFilename(_d.getCloudParams().url));

            FileUtils.copy(_d.getDestinationFile(), kmpFile);

            String pkgTarget = kmpProcessor.getPackageTarget(kmpFile);
            if (pkgTarget.equals(PackageProcessor.PP_TARGET_LEXICAL_MODELS)) {
              File unzipPath = kmpProcessor.unzipKMP(kmpFile);
              installedLexicalModels.addAll(kmpProcessor.processKMP(kmpFile, unzipPath, PackageProcessor.PP_LEXICAL_MODELS_KEY));
            }
          }
        }
        catch (IOException | JSONException e) {
          KMLog.LogException(TAG, "", e);
        }
      }
      else
      {
          _result = FileUtils.DOWNLOAD_ERROR;
      }
    }
    return new CloudKeyboardDownloadReturns(_result,installedLexicalModels);
  }



  @Override
  public void applyCloudDownloadToModel(Context aContext, Void aModel, CloudKeyboardDownloadReturns aCloudResult)
  {
    Toast.makeText(aContext,
      aContext.getString(R.string.dictionary_download_finished),
      Toast.LENGTH_SHORT).show();

    if(aCloudResult.installedResource != null)
    {
      KeyboardEventHandler.notifyListeners(KMKeyboardDownloaderActivity.getKbDownloadEventListeners(),
        KeyboardEventHandler.EventType.LEXICAL_MODEL_INSTALLED,
        aCloudResult.installedResource, aCloudResult.kbdResult);
    }
  }

  /**
   * create a download id for the model.
   * @param aModelId the lexical model id
   * @return the result
   */
  public static String createDownloadId(String aModelId)
  {
    return "dictionary_" + aModelId;
  }
}
