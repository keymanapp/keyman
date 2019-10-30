package com.tavultesoft.kmea.data;

import android.content.Context;
import android.util.Log;
import android.widget.Toast;

import com.tavultesoft.kmea.KeyboardEventHandler;
import com.tavultesoft.kmea.R;
import com.tavultesoft.kmea.util.FileUtils;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;

import static com.tavultesoft.kmea.KMManager.KMDefault_UndefinedPackageID;

public class CloudKeyboardDataDownloadCallback implements ICloudDownloadCallback<
  Void, CloudKeyboardDownloadReturns>
{

  private static final String TAG = "CloudkbDataDldCb";

  private File dataDir;

  private ArrayList<KeyboardEventHandler.OnKeyboardDownloadEventListener> downloadEventListeners = new ArrayList<>();
  private HashMap<String,String> keyboardInfo;

  public static final String PARAM_DESTINATION_FILE_NAME = "destination_file_name";

  public void setDownloadEventListeners(ArrayList<KeyboardEventHandler.OnKeyboardDownloadEventListener> aDownloadEventListeners)
  {
    downloadEventListeners.clear();
    downloadEventListeners.addAll(aDownloadEventListeners);
  }

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

            String destination = dataDir.toString() +
               File.separator + KMDefault_UndefinedPackageID + File.separator;

            String _filename = _d.getCloudParams().getAdditionalProperty(PARAM_DESTINATION_FILE_NAME,String.class);
            if (_filename==null) {

              _filename = FileUtils.getFilename(_d.getCloudParams().url);
            }

            File _data_file = new File(destination,_filename);
            FileUtils.copy(_d.getDestinationFile(), _data_file);

        }
        catch (IOException _e)
        {
          Log.e(TAG,_e.getLocalizedMessage(),_e);
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
      KeyboardEventHandler.notifyListeners(downloadEventListeners, KeyboardEventHandler.EventType.KEYBOARD_DOWNLOAD_FINISHED,
       keyboardInfo, aCloudResult.kbdResult);
    }



  }

  public static String createDownloadId(String aKeyboardId)
  {
    return "keyboarddata_" + aKeyboardId;
  }
}
