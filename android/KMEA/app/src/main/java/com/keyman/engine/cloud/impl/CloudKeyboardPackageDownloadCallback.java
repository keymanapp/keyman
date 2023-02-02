package com.keyman.engine.cloud.impl;

import android.content.Context;
import android.net.Uri;
import android.widget.Toast;

import com.keyman.engine.BaseActivity;
import com.keyman.engine.KMKeyboardDownloaderActivity;
import com.keyman.engine.KMManager;
import com.keyman.engine.KeyboardEventHandler;
import com.keyman.engine.KmpInstallMode;
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
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * Install Keyboard package
 */
public class CloudKeyboardPackageDownloadCallback implements ICloudDownloadCallback<
  Void, CloudKeyboardDownloadReturns>
{

  private static final String TAG = "CloudKbdPKGDldCb";

  private File resourceRoot;
  private File cacheDir;
  private String languageID;

  public void setLanguageID(String languageID) {
    this.languageID = languageID;
  }

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
    PackageProcessor kbdKMPProcessor = new PackageProcessor(resourceRoot);
    List<Map<String, String>> installedKeyboards = null;

    int _result = FileUtils.DOWNLOAD_SUCCESS;
    for(CloudApiTypes.SingleCloudDownload _d:aDownload.getSingleDownloads())
    {

      File destinationFile = _d.cacheAndOpenDestinationFile(aContext);
      if (destinationFile != null && destinationFile.length() > 0)
      {

        try {
          if (_d.getCloudParams().target == CloudApiTypes.ApiTarget.KeyboardPackage) {
            installedKeyboards = new LinkedList<>();

            // Parse url for the kmp filename
            Uri uri = Uri.parse(_d.getCloudParams().url);
            String packageID = uri.getLastPathSegment();
            if (packageID == null || packageID.isEmpty()) {
              KMLog.LogError(TAG, "Cloud URL " + _d.getCloudParams().url + " has null packageID");
            }
            String kmpFilename = String.format("%s%s", packageID, FileUtils.KEYMANPACKAGE);

            // Extract the kmp.
            File kmpFile = new File(cacheDir, kmpFilename);

            FileUtils.copy(destinationFile, kmpFile);

            String pkgTarget = kbdKMPProcessor.getPackageTarget(kmpFile);
            if (pkgTarget.equals(PackageProcessor.PP_TARGET_KEYBOARDS)) {
              File unzipPath = kbdKMPProcessor.unzipKMP(kmpFile);
              ArrayList<String> languageList = new ArrayList<String>();
              if (languageID != null && !languageID.isEmpty()) {
                languageList.add(languageID);
              }
              installedKeyboards.addAll(kbdKMPProcessor.processKMP(kmpFile, unzipPath, PackageProcessor.PP_KEYBOARDS_KEY, languageList));

              // Set "silent install" so cloud updates don't display welcome.htm
              for(Map<String, String> kbdMap : installedKeyboards) {
                kbdMap.put(KMManager.KMKey_KMPInstall_Mode, KmpInstallMode.Silent.toString());
              }
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
    return new CloudKeyboardDownloadReturns(_result, installedKeyboards);
  }



  @Override
  public void applyCloudDownloadToModel(Context aContext, Void aModel, CloudKeyboardDownloadReturns aCloudResult)
  {
    BaseActivity.makeToast(aContext, R.string.keyboard_download_finished, Toast.LENGTH_SHORT);

    if(aCloudResult.installedResource != null)
    {
      KeyboardEventHandler.notifyListeners(KMKeyboardDownloaderActivity.getKbDownloadEventListeners(),
        KeyboardEventHandler.EventType.PACKAGE_INSTALLED,
        aCloudResult.installedResource, aCloudResult.kbdResult);
    }
  }

  /**
   * create a download id for the model.
   * @param languageID the language ID
   * @param keyboardID the keyboard ID
   * @return the result
   */
  public static String createDownloadId(String languageID, String keyboardID)
  {
    return String.format("keyboard_%s_%s", languageID, keyboardID);
  }
}
