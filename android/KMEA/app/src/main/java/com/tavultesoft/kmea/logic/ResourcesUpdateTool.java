package com.tavultesoft.kmea.logic;

import android.app.Application;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.ProgressDialog;
import android.app.TaskStackBuilder;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.widget.Toast;

import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationCompat.Builder;
import androidx.core.app.NotificationManagerCompat;

import com.tavultesoft.kmea.KMKeyboardDownloaderActivity;
import com.tavultesoft.kmea.KeyboardPickerActivity;
import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.KeyboardEventHandler;
import com.tavultesoft.kmea.R;
import com.tavultesoft.kmea.data.CloudRepository;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

public class ResourcesUpdateTool implements KeyboardEventHandler.OnKeyboardDownloadEventListener, CloudRepository.UpdateHandler{

  /**
   * Force resource update.
   * Only for testing, should be false for merging
   */
  public static final boolean FORCE_RESOURCE_UPDATE = false;

  /**
   * Send update notifications.
   */
  public static final boolean SEND_UPDATE_NOTIFICATIONS = true;

  private Context currentContext;

  private boolean updateCheckFailed = false;
  private boolean updateFailed = false;
  private Calendar lastUpdateCheck = null;
  private boolean checkingUpdates = false;

  private HashSet<String> openUpdates = new HashSet<>();
  private int failedUpdateCount = 0;

  private PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(this);


  private AtomicInteger notificationid = new AtomicInteger(1);

  private void checkResourceUpdatesInternal(boolean anInitialize) {

    Runnable onSuccess = new Runnable() {
      public void run() {
        if(! openUpdates.isEmpty()) {
          return;
        }

        Toast.makeText(currentContext, currentContext.getString(R.string.update_check_current), Toast.LENGTH_SHORT).show();
        lastUpdateCheck = Calendar.getInstance();
        SharedPreferences prefs = currentContext.getSharedPreferences(currentContext.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = prefs.edit();
        editor.putLong("lastUpdateCheck", lastUpdateCheck.getTime().getTime());
        editor.commit();
        checkingUpdates = false;
      }
    };

    Runnable onFailure = new Runnable() {
      public void run() {
        if(! openUpdates.isEmpty()) {
          return;
        }

        Toast.makeText(currentContext, currentContext.getString(R.string.update_check_unavailable), Toast.LENGTH_SHORT).show();
        lastUpdateCheck = Calendar.getInstance();
        updateCheckFailed = true;
        checkingUpdates = false;
      }
    };

    checkingUpdates = true;
    if(anInitialize)
      CloudRepository.shared.initializeDataSet(currentContext, this, onSuccess, onFailure);
    else
      CloudRepository.shared.updateDatasetIfNeeded(currentContext, this, onSuccess, onFailure);

  }

  public static void createNotificationChannel(Context aContext) {
    // Create the NotificationChannel, but only on API 26+ because
    // the NotificationChannel class is new and not in the support library
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O && SEND_UPDATE_NOTIFICATIONS) {
      CharSequence name = aContext.getString(R.string.keyboard_updates_channel);
      String description = aContext.getString(R.string.keyboard_updates_available);
      int importance = NotificationManager.IMPORTANCE_DEFAULT;
      NotificationChannel channel = new NotificationChannel(ResourcesUpdateTool.class.getName(), name, importance);
      channel.setDescription(description);
      // Register the channel with the system; you can't change the importance
      // or other notification behaviors after this
      NotificationManager notificationManager = aContext.getSystemService(NotificationManager.class);
      notificationManager.createNotificationChannel(channel);
    }
  }

  public void onUpdateDetection(final List<Bundle> updatableResources) {
    failedUpdateCount = 0;

    if(SEND_UPDATE_NOTIFICATIONS) {
      for (Bundle resourceBundle : updatableResources) {
        sendNotification( resourceBundle);
      }
    }
    else {
      AlertDialog.Builder dialogBuilder = new AlertDialog.Builder(currentContext);
      dialogBuilder.setTitle(currentContext.getString(R.string.keyboard_updates_available));
      dialogBuilder.setMessage(currentContext.getString(R.string.confirm_update));
      dialogBuilder.setPositiveButton(currentContext.getString(R.string.label_update), new DialogInterface.OnClickListener() {
        public void onClick(DialogInterface dialog, int which) {
          // Update keyboards
          if (KMManager.hasConnection(currentContext)) {
            // For each updatable keyboard, one at a time, do the update.
            // TODO:  May need a reework to better handle large amounts of updates -
            //        these calls will stack within the Android subsystem and may have issues accordingly.
            for (Bundle resourceBundle : updatableResources) {

              String langid = resourceBundle.getString(KMKeyboardDownloaderActivity.ARG_LANG_ID);

              boolean downloadOnlyLexicalModel = resourceBundle.containsKey(KMKeyboardDownloaderActivity.ARG_MODEL_URL) &&
                resourceBundle.getString(KMKeyboardDownloaderActivity.ARG_MODEL_URL) != null &&
                !resourceBundle.getString(KMKeyboardDownloaderActivity.ARG_MODEL_URL).isEmpty();

              if(downloadOnlyLexicalModel) {
                String modelid = resourceBundle.getString(KMKeyboardDownloaderActivity.ARG_MODEL_ID);
                addOpenUpdate(createLexicalModelId(langid,modelid));
              }
              else {
                String kbid = resourceBundle.getString(KMKeyboardDownloaderActivity.ARG_KB_ID);
                addOpenUpdate(createKeyboardId(langid,kbid));
              }

              Intent intent = new Intent(currentContext, KMKeyboardDownloaderActivity.class);
              intent.putExtras(resourceBundle);
              currentContext.startActivity(intent);
            }
          } else {
            Toast.makeText(currentContext, "No internet connection", Toast.LENGTH_SHORT).show();
            checkingUpdates = false;
          }
        }
      });

      dialogBuilder.setNegativeButton(currentContext.getString(R.string.label_later), new DialogInterface.OnClickListener() {
        public void onClick(DialogInterface dialog, int which) {
          lastUpdateCheck = Calendar.getInstance();
          checkingUpdates = false;
        }
      });

      AlertDialog dialog = dialogBuilder.create();
      if (isContextAvailable()) {
        dialog.setOnCancelListener(new DialogInterface.OnCancelListener() {
          @Override
          public void onCancel(DialogInterface dialog) {
            lastUpdateCheck = Calendar.getInstance();
            checkingUpdates = false;
          }
        });
        dialog.show();
      } else {
        checkingUpdates = false;
      }
    }
  }

  private void addOpenUpdate(String anId)
  {
    int _old= openUpdates.size();
    openUpdates.add(anId);
    propertyChangeSupport.firePropertyChange("updateCount",_old,openUpdates.size());
  }

  private void removeOpenUpdate(String anId)
  {
    int _old= openUpdates.size();
    openUpdates.remove(anId);
    propertyChangeSupport.firePropertyChange("updateCount",_old,openUpdates.size());
  }

  /**
   * get open update count.
   * @return the count
   */
  public int getOpenUpdateCount()
  {
    return openUpdates.size();
  }
  /**
   * send update notification.
   * @param theResourceBundle the bundle
   */
  private void sendNotification(Bundle theResourceBundle) {
    NotificationManagerCompat notificationManager = NotificationManagerCompat.from(currentContext);

    String langid = theResourceBundle.getString(KMKeyboardDownloaderActivity.ARG_LANG_ID);
    String langName = theResourceBundle.getString(KMKeyboardDownloaderActivity.ARG_LANG_NAME);
    boolean downloadOnlyLexicalModel = theResourceBundle.containsKey(KMKeyboardDownloaderActivity.ARG_MODEL_URL) &&
      theResourceBundle.getString(KMKeyboardDownloaderActivity.ARG_MODEL_URL) != null &&
      !theResourceBundle.getString(KMKeyboardDownloaderActivity.ARG_MODEL_URL).isEmpty();

    String message;

    if(downloadOnlyLexicalModel) {
      String modelid = theResourceBundle.getString(KMKeyboardDownloaderActivity.ARG_MODEL_ID);
      String modelName = theResourceBundle.getString(KMKeyboardDownloaderActivity.ARG_MODEL_NAME);
      message = currentContext.getString(R.string.dictionary_update_message, langName, modelName);
      addOpenUpdate(createLexicalModelId(langid,modelid));
    }
    else {
      String kbid = theResourceBundle.getString(KMKeyboardDownloaderActivity.ARG_KB_ID);
      String kbName = theResourceBundle.getString(KMKeyboardDownloaderActivity.ARG_KB_NAME);
      message =  currentContext.getString(R.string.keyboard_update_message, langName, kbName);
     addOpenUpdate(createKeyboardId(langid,kbid));

    }

    int  notification_id = this.notificationid.incrementAndGet();
    Intent intent = new Intent(currentContext, KMKeyboardDownloaderActivity.class);
    intent.putExtras(theResourceBundle);
    intent.putExtra(KMKeyboardDownloaderActivity.ARG_NOTIFICATION_ID,notification_id);

    // Create the TaskStackBuilder and add the intent, which inflates the back stack
    TaskStackBuilder stackBuilder = TaskStackBuilder.create(currentContext);
    stackBuilder.addNextIntentWithParentStack(intent);
    // Get the PendingIntent containing the entire back stack
    PendingIntent startUpdateIntent =
      stackBuilder.getPendingIntent(notification_id, PendingIntent.FLAG_UPDATE_CURRENT);

    Builder builder = new Builder(currentContext, getClass().getName())
      .setSmallIcon(R.drawable.ic_launcher)
      .setContentTitle(currentContext.getString(R.string.keyboard_updates_available))
      .setContentText(message)
      .setPriority(NotificationCompat.PRIORITY_DEFAULT)
      .setContentIntent(startUpdateIntent);

    notificationManager.notify(notification_id,builder.build());
  }

  private boolean isContextAvailable() {
    if(currentContext instanceof AppCompatActivity)
      return !((AppCompatActivity) currentContext).isFinishing();
    return true;
  }

  /**
   * check for updates.
   * @param aContext the context
   * @param anIsInitialize check for update on startup.
   */
  public void checkForResourceUpdates(final Context aContext, final boolean anIsInitialize) {

    currentContext = aContext;
    Handler handler = new Handler();
    handler.postDelayed(new Runnable() {
      @Override
      public void run() {

        if (shouldCheckUpdate(aContext)) {
          updateCheckFailed = false;
          updateFailed = false;
          if (!checkingUpdates) {
            checkResourceUpdatesInternal(anIsInitialize);
          }
        }
      }
    }, 1000);
  }

  public boolean shouldCheckUpdate(Context aContext) {
    boolean shouldCheckUpdate = false;
    if (lastUpdateCheck == null) {
      SharedPreferences prefs = aContext.getSharedPreferences(aContext.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
      Long lastUpdateCheckTime = prefs.getLong("lastUpdateCheck", 0);
      if (lastUpdateCheckTime > 0) {
        lastUpdateCheck = Calendar.getInstance();
        lastUpdateCheck.setTime(new Date(lastUpdateCheckTime));
      }
    }

    if (lastUpdateCheck != null) {
      Calendar lastChecked = Calendar.getInstance();
      lastChecked.setTime(lastUpdateCheck.getTime());
      if (updateCheckFailed || updateFailed) {
        lastChecked.add(Calendar.HOUR_OF_DAY, 1);
      } else {
        lastChecked.add(Calendar.HOUR_OF_DAY, 24);
      }

      Calendar now = Calendar.getInstance();
      if (now.compareTo(lastChecked) > 0) {
        shouldCheckUpdate = true;
      }
    } else {
      shouldCheckUpdate = true;
    }

    if(FORCE_RESOURCE_UPDATE)
      shouldCheckUpdate = true;

    return shouldCheckUpdate;
  }

  @Override
  public void onKeyboardDownloadStarted(HashMap<String, String> keyboardInfo) {
    // Do nothing
  }

  @Override
  public void onKeyboardDownloadFinished(HashMap<String, String> keyboardInfo, int result) {
    if (result > 0) {
      KeyboardPickerActivity.handleDownloadedKeyboard(currentContext, keyboardInfo);
    } else if (result < 0) {
      failedUpdateCount++;
    }

    if (! openUpdates.isEmpty()) {
      String _langid = keyboardInfo.get(KMManager.KMKey_LanguageID);
      String _kbid = keyboardInfo.get(KMManager.KMKey_KeyboardID);
      removeOpenUpdate(createKeyboardId(_langid,_kbid));
    }

    tryFinalizeUpdate();
  }

  @Override
  public void onPackageInstalled(List<Map<String, String>> keyboardsInstalled) {
    // Do nothing
  }

  @Override
  public void onLexicalModelInstalled(List<Map<String, String>> lexicalModelsInstalled) {
    if (! openUpdates.isEmpty()) {
      for(Map<String,String> _lm:lexicalModelsInstalled) {
        String _langid = _lm.get(KMManager.KMKey_LanguageID);
        String _modelid = _lm.get(KMManager.KMKey_LexicalModelID);
        removeOpenUpdate(createLexicalModelId(_langid, _modelid));
      }
    }

    tryFinalizeUpdate();
  }

  private String createKeyboardId(String theLanguageId, String theKbId) {
    return "kb_" + theLanguageId + "_" + theKbId;
  }

  private String createLexicalModelId(String theLanguageId, String theModelId) {
    return "model_" + theLanguageId + "_" + theModelId;
  }

  void tryFinalizeUpdate() {
    if (openUpdates.isEmpty()) {


      if (failedUpdateCount > 0) {
        Toast.makeText(currentContext, "One or more resources failed to update!", Toast.LENGTH_SHORT).show();
        lastUpdateCheck = Calendar.getInstance();
        updateFailed = true;
        checkingUpdates = false;
      } else {
        Toast.makeText(currentContext, "Resources successfully updated!", Toast.LENGTH_SHORT).show();
        lastUpdateCheck = Calendar.getInstance();
        SharedPreferences prefs = currentContext.getSharedPreferences(currentContext.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = prefs.edit();
        editor.putLong("lastUpdateCheck", lastUpdateCheck.getTime().getTime());
        editor.commit();
        checkingUpdates = false;
      }
    }
  }


  public void addPropertyChangeListener (PropertyChangeListener listener){
    this.propertyChangeSupport.addPropertyChangeListener(listener);
  }

  public void removePropertyChangeListener (PropertyChangeListener listener){
    this.propertyChangeSupport.removePropertyChangeListener(listener);
    }

}
