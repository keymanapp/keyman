package com.keyman.engine.cloud.impl;

import android.content.Context;
import android.net.Uri;

import com.keyman.engine.BaseActivity;
import com.keyman.engine.KMKeyboardDownloaderActivity;
import com.keyman.engine.KeyboardEventHandler;
import com.keyman.engine.R;
import com.keyman.engine.cloud.CloudApiTypes;
import com.keyman.engine.cloud.ICloudDownloadCallback;
import com.keyman.engine.packages.LexicalModelPackageProcessor;
import com.keyman.engine.packages.PackageProcessor;
import com.keyman.engine.util.FileUtils;
import com.keyman.engine.util.KMLog;

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
    Context aContext, CloudApiTypes.CloudDownloadSet<Void, CloudKeyboardDownloadReturns> aDownload)
  {
    LexicalModelPackageProcessor kmpProcessor = new LexicalModelPackageProcessor(resourceRoot);
    List<Map<String, String>> installedLexicalModels = null;

    int _result = FileUtils.DOWNLOAD_SUCCESS;
    for(CloudApiTypes.SingleCloudDownload _d:aDownload.getSingleDownloads())
    {
      File destinationFile = _d.cacheAndOpenDestinationFile(aContext);
      if (destinationFile != null && destinationFile.length() > 0)
      {

        try {
          if (_d.getCloudParams().target == CloudApiTypes.ApiTarget.LexicalModelPackage) {
            installedLexicalModels = new LinkedList<>();

            // Parse url for the kmp filename
            Uri uri = Uri.parse(_d.getCloudParams().url);
            String kmpFilename = uri.getLastPathSegment();
            if (kmpFilename == null || kmpFilename.isEmpty()) {
              KMLog.LogError(TAG, "Cloud URL " + _d.getCloudParams().url + " has null model package");
            }
            if (!kmpFilename.endsWith(FileUtils.MODELPACKAGE)) {
              kmpFilename = kmpFilename + FileUtils.MODELPACKAGE;
            }

            // Extract the kmp. Validate it contains only lexical models, and then process the lexical model package
            File kmpFile = new File(cacheDir, kmpFilename);

            FileUtils.copy(destinationFile, kmpFile);

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
