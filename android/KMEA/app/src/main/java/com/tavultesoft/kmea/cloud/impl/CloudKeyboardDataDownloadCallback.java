package com.tavultesoft.kmea.cloud.impl;

import android.content.Context;
import android.util.Log;
import android.widget.Toast;

import com.tavultesoft.kmea.KMKeyboardDownloaderActivity;
import com.tavultesoft.kmea.KeyboardEventHandler;
import com.tavultesoft.kmea.R;
import com.tavultesoft.kmea.cloud.CloudApiTypes;
import com.tavultesoft.kmea.cloud.ICloudDownloadCallback;
import com.tavultesoft.kmea.util.FileUtils;
import com.tavultesoft.kmea.util.KMLog;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;

/**
 * Install keyboard data, when download is finished.
 * Could be keyboard packages or fonts.
 */
public class CloudKeyboardDataDownloadCallback implements ICloudDownloadCallback<
  Void, CloudKeyboardDownloadReturns>
{

  private static final String TAG = "CloudkbDataDldCb";

  private File dataDir;

  private HashMap<String,String> keyboardInfo;

  /**
   * Additional Cloud API parameter:
   * Parameter to force a special destination file name during installation.
   * Default name is the file name of the url.
   */
  public static final String PARAM_DESTINATION_FILE_NAME = "destination_file_name";

  /**
   * Additional Cloud API parameter:
   * Parameter to force a special destination file name during installation.
   * Default name is the file name of the url.
   */
  public static final String PARAM_PACKAGE = "package";

  /**
   * Keyboard meta data.
   * @param aKeyboardInfo the keyboard
   */
  public void setKeyboardInfo(HashMap<String, String> aKeyboardInfo) {
    this.keyboardInfo = aKeyboardInfo;
  }

  @Override
  public void initializeContext(Context context)
  {
    dataDir = context.getDir("data", Context.MODE_PRIVATE);
  }

  @Override
  public CloudKeyboardDownloadReturns extractCloudResultFromDownloadSet(
    CloudApiTypes.CloudDownloadSet<Void, CloudKeyboardDownloadReturns> aDownload)
  {
    int _result = FileUtils.DOWNLOAD_SUCCESS;
    for(CloudApiTypes.SingleCloudDownload _d:aDownload.getSingleDownloads())
    {
      if (_d.getDestinationFile() != null && _d.getDestinationFile().length() > 0)
      {

        try {

          String _pkg = _d.getCloudParams().getAdditionalProperty(PARAM_PACKAGE,String.class);
            String destination = dataDir.toString() +
               File.separator + _pkg + File.separator;

            String _filename = _d.getCloudParams().getAdditionalProperty(PARAM_DESTINATION_FILE_NAME,String.class);
            if (_filename==null) {

              _filename = FileUtils.getFilename(_d.getCloudParams().url);
            }

            File _data_file = new File(destination,_filename);
            FileUtils.copy(_d.getDestinationFile(), _data_file);

        }
        catch (IOException e) {
          KMLog.LogException(TAG, "", e);
        }
      }
      else
      {
        if (FileUtils.hasFontExtension(_d.getCloudParams().url))
          _result = 2;
        else
          _result = FileUtils.DOWNLOAD_ERROR;
      }
    }
    return new CloudKeyboardDownloadReturns(_result);
  }



  @Override
  public void applyCloudDownloadToModel(Context aContext, Void aModel, CloudKeyboardDownloadReturns aCloudResult)
  {
    Toast.makeText(aContext,
      aContext.getString(R.string.keyboard_download_finished),
      Toast.LENGTH_SHORT).show();

    if(keyboardInfo!=null)
    {
      KeyboardEventHandler.notifyListeners(KMKeyboardDownloaderActivity.getKbDownloadEventListeners(),
        KeyboardEventHandler.EventType.KEYBOARD_DOWNLOAD_FINISHED,
       keyboardInfo, aCloudResult.kbdResult);
    }
  }

  /**
   * create a download id for the keyboard data.
   * @param aKeyboardId the keyboard id
   * @return the result
   */
  public static String createDownloadId(String aKeyboardId)
  {
    return "keyboarddata_" + aKeyboardId;
  }
}
