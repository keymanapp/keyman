package com.keyman.engine.logic;

import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;

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

import com.keyman.engine.BaseActivity;
import com.keyman.engine.KMKeyboardDownloaderActivity;
import com.keyman.engine.KeyboardPickerActivity;
import com.keyman.engine.KMManager;
import com.keyman.engine.KeyboardEventHandler;
import com.keyman.engine.R;
import com.keyman.engine.data.CloudRepository;
import com.keyman.engine.util.KMLog;
import com.keyman.engine.util.VersionUtils;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import org.json.JSONException;
import org.json.JSONObject;

public class ResourcesUpdateTool implements KeyboardEventHandler.OnKeyboardDownloadEventListener, CloudRepository.UpdateHandler{
  private static final String TAG = "ResourceUpdateTool";

  /**
   * Force resource update.
   * Only for testing, should be false for merging
   */
  public static final boolean FORCE_RESOURCE_UPDATE = false;

  /**
   * Send update notifications.
   */
  public static final boolean SEND_UPDATE_NOTIFICATIONS = true;
  /**
   * Switch of system notifications.
   * Use only update indicator to inform user about updates.
   */
  public static final boolean USE_UPDATE_INDICATOR_ONLY = false;

  /**
   * Preferences key to save last update check time.
   */
  public static final String PREF_KEY_LAST_UPDATE_CHECK = "lastUpdateCheck";

  /**
   * Preference key for ignored notifications
   */
  public static final String PREF_KEY_IGNORE_NOTIFICATIONS = "ignoredNotifications";

  /**
   * Months to ignore update notification
   */
  public static final int MONTHS_TO_IGNORE_NOTIFICATION = 3;

  private static final class OngoingUpdate
  {
    Integer notificationid;
    String updateid;
    Bundle bundle;
  }

  private Context currentContext;

  private boolean updateCheckFailed = false;
  private boolean updateFailed = false;
  private Calendar lastUpdateCheck = null;
  private boolean checkingUpdates = false;

  private LinkedHashMap<String, OngoingUpdate> openUpdates = new LinkedHashMap<>();
  private int failedUpdateCount = 0;

  private PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(this);


  private AtomicInteger notificationid = new AtomicInteger(1);

  private void checkResourceUpdatesInternal(boolean anInitialize) {

    Runnable onSuccess = new Runnable() {
      public void run() {
        if(! openUpdates.isEmpty()) {
          return;
        }

        lastUpdateCheck = Calendar.getInstance();
        SharedPreferences prefs = currentContext.getSharedPreferences(currentContext.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = prefs.edit();
        editor.putLong(PREF_KEY_LAST_UPDATE_CHECK, lastUpdateCheck.getTime().getTime());
        editor.commit();
        checkingUpdates = false;
      }
    };

    Runnable onFailure = new Runnable() {
      public void run() {
        if(! openUpdates.isEmpty()) {
          return;
        }

        BaseActivity.makeToast(currentContext, R.string.update_check_unavailable, Toast.LENGTH_SHORT);
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
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O && SEND_UPDATE_NOTIFICATIONS && aContext != null) {
      CharSequence name = aContext.getString(R.string.keyboard_updates_channel);
      String description = aContext.getString(R.string.keyboard_updates_available);
      int importance = NotificationManager.IMPORTANCE_DEFAULT;
      NotificationChannel channel = new NotificationChannel(ResourcesUpdateTool.class.getName(), name, importance);
      channel.setDescription(description);
      // Register the channel with the system; you can't change the importance
      // or other notification behaviors after this
      NotificationManager notificationManager = (NotificationManager) aContext.getSystemService(Context.NOTIFICATION_SERVICE);
      notificationManager.createNotificationChannel(channel);
    }
  }

  public static void destroyNotificationChannel(Context aContext) {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O && aContext != null) {
      NotificationManager notificationManager = (NotificationManager) aContext.getSystemService(Context.NOTIFICATION_SERVICE);
      notificationManager.deleteNotificationChannel(ResourcesUpdateTool.class.getName());
    }
  }

  public void onUpdateDetection(final List<Bundle> updatableResources) {
    failedUpdateCount = 0;

    if (USE_UPDATE_INDICATOR_ONLY)
    {
      for (Bundle resourceBundle : updatableResources) {
        addOpenUpdate( resourceBundle);
      }
    }
    else if(SEND_UPDATE_NOTIFICATIONS) {
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

             addOpenUpdate(resourceBundle);

              Intent intent = new Intent(currentContext, KMKeyboardDownloaderActivity.class);
              intent.putExtras(resourceBundle);
              currentContext.startActivity(intent);
            }
          } else {
            BaseActivity.makeToast(currentContext, R.string.cannot_connect, Toast.LENGTH_SHORT);
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

  /**
   * Add open update.
   * @param anUpdateId the update id
   * @param aNotificationId the system notification id
   * @param aBundle the bundle
   */
  private void addOpenUpdate(String anUpdateId,Integer aNotificationId, Bundle aBundle)
  {
    int _old= openUpdates.size();
    OngoingUpdate _update = new OngoingUpdate();
    _update.updateid = anUpdateId;
    _update.notificationid = aNotificationId;
    _update.bundle = aBundle;
    openUpdates.put(anUpdateId, _update);
    propertyChangeSupport.firePropertyChange("updateCount",_old,openUpdates.size());
  }

  /**
   * remove the open update package.
   * cancels the system notification.
   * @param anUpdateId the update id
   */
  private void removeOpenUpdate(String anUpdateId)
  {
    int _old= openUpdates.size();
    OngoingUpdate _update = openUpdates.remove(anUpdateId);

    if(_update==null) {
      return;
    }

    if(_update.notificationid!=null) {
      NotificationManagerCompat notificationManager = NotificationManagerCompat.from(currentContext);
      notificationManager.cancel(_update.notificationid);
    }
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
  private void addOpenUpdate(Bundle theResourceBundle) {

    String langid = theResourceBundle.getString(KMKeyboardDownloaderActivity.ARG_LANG_ID);
    boolean downloadOnlyLexicalModel = theResourceBundle.containsKey(KMKeyboardDownloaderActivity.ARG_MODEL_ID) &&
      theResourceBundle.getString(KMKeyboardDownloaderActivity.ARG_MODEL_ID) != null &&
      !theResourceBundle.getString(KMKeyboardDownloaderActivity.ARG_MODEL_ID).isEmpty();

    if (downloadOnlyLexicalModel) {
      String modelid = theResourceBundle.getString(KMKeyboardDownloaderActivity.ARG_MODEL_ID);
      addOpenUpdate(createLexicalModelId(langid, modelid), null, theResourceBundle);
    } else {
      String kbid = theResourceBundle.getString(KMKeyboardDownloaderActivity.ARG_KB_ID);

      addOpenUpdate(createKeyboardId(langid, kbid), null, theResourceBundle);
    }
  }

  /**
   * Check shared preference to see if an update notification should be ignored.
   * The window is MONTHS_TO_IGNORE_NOTIFICATION from the last time the notification was ignored.
   * @param id keyboard or lexical model ID
   * @return true if the notification should be ignored
   */
  private boolean shouldIgnoreNotification(String id) {
    // For local and PR test builds, return false to make it easier to get updates for testing
    if (VersionUtils.isLocalOrTestBuild()) {
      return false;
    }

    SharedPreferences prefs = currentContext.getSharedPreferences(currentContext.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    SharedPreferences.Editor editor = prefs.edit();
    String ignoredNotificationsStr = prefs.getString(PREF_KEY_IGNORE_NOTIFICATIONS, null);

    /*
     * Preference is a JSON Object (as a string)
     * { PREF_KEY_IGNORE_NOTIFICATIONS :
     *   { id1 : time1 ignored,
     *     id2 : time2 ignored
     *   }
     * }
     */
    JSONObject ignoredNotificationsObj;
    if (ignoredNotificationsStr != null) {
      try {
        ignoredNotificationsObj = new JSONObject(ignoredNotificationsStr);

        Long lastIgnoredTime = ignoredNotificationsObj.optLong(id, 0);
        if (lastIgnoredTime > 0) {
          Calendar now = Calendar.getInstance();
          Calendar ignoreUntilTime = Calendar.getInstance();
          ignoreUntilTime.setTime(new Date(lastIgnoredTime));
          ignoreUntilTime.add(Calendar.MONTH, MONTHS_TO_IGNORE_NOTIFICATION);
          if (now.compareTo(ignoreUntilTime) < 0) {
            return true;
          }
        }
      } catch (JSONException e) {
        KMLog.LogException(TAG, "JSON Exception parsing ignoreNotifications preference", e);
      }
    }

    return false;
  }

  /**
   * Update preference to ignore notifications for keyboard / lexical model ID
   * @param id : keyboard or lexical model ID to ignore for MONTHS_TO_IGNORE_NOTIFICATION months
   */
  private void setPrefKeyIgnoreNotifications(String id) {
    if (currentContext == null) {
      return;
    }
    SharedPreferences prefs = currentContext.getSharedPreferences(
      currentContext.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    SharedPreferences.Editor editor = prefs.edit();
    String ignoredNotificationsStr = prefs.getString(PREF_KEY_IGNORE_NOTIFICATIONS, null);

    /*
     * Preference is a JSON Object (stored as a string)
     * { PREF_KEY_IGNORE_NOTIFICATIONS :
     *   { id1 : time1 ignored,
     *     id2 : time2 ignored
     *   }
     * }
     */
    JSONObject ignoredNotificationsObj;
    try {
      if (ignoredNotificationsStr == null) {
        ignoredNotificationsObj = new JSONObject();
      } else {
        ignoredNotificationsObj = new JSONObject(ignoredNotificationsStr);
      }

      Calendar now = Calendar.getInstance();
      ignoredNotificationsObj.put(id, now.getTime().getTime());
      editor.putString(PREF_KEY_IGNORE_NOTIFICATIONS, ignoredNotificationsObj.toString());
      editor.commit();
    } catch (JSONException e) {
      KMLog.LogException(TAG, "JSON Exception updating ignoreNotifications preference", e);
    }
  }

  /**
   * send update notification.
   * @param theResourceBundle the bundle
   */
  private void sendNotification(Bundle theResourceBundle) {
    NotificationManagerCompat notificationManager = NotificationManagerCompat.from(currentContext);

    String langid = theResourceBundle.getString(KMKeyboardDownloaderActivity.ARG_LANG_ID);
    String langName = theResourceBundle.getString(KMKeyboardDownloaderActivity.ARG_LANG_NAME);
    boolean downloadOnlyLexicalModel = theResourceBundle.containsKey(KMKeyboardDownloaderActivity.ARG_MODEL_ID) &&
      theResourceBundle.getString(KMKeyboardDownloaderActivity.ARG_MODEL_ID) != null &&
      !theResourceBundle.getString(KMKeyboardDownloaderActivity.ARG_MODEL_ID).isEmpty();

    int  notification_id = this.notificationid.incrementAndGet();

    String message;

    if(downloadOnlyLexicalModel) {
      String modelid = theResourceBundle.getString(KMKeyboardDownloaderActivity.ARG_MODEL_ID);
      String modelName = theResourceBundle.getString(KMKeyboardDownloaderActivity.ARG_MODEL_NAME);
      message = currentContext.getString(R.string.dictionary_update_message, langName, modelName);
      if (!shouldIgnoreNotification(modelid)) {
        addOpenUpdate(createLexicalModelId(langid, modelid), notification_id, theResourceBundle);
      } else {
        // Update notification should be ignored
        notificationManager.cancel(notification_id);
        return;
      }
    }
    else {
      String kbid = theResourceBundle.getString(KMKeyboardDownloaderActivity.ARG_KB_ID);
      String kbName = theResourceBundle.getString(KMKeyboardDownloaderActivity.ARG_KB_NAME);
      message =  currentContext.getString(R.string.keyboard_update_message, langName, kbName);
      if (!shouldIgnoreNotification(kbid)) {
        addOpenUpdate(createKeyboardId(langid, kbid), notification_id, theResourceBundle);
      } else {
        // Update notification should be ignored
        notificationManager.cancel(notification_id);
        return;
      }

    }

    Intent intent = new Intent(currentContext, KMKeyboardDownloaderActivity.class);
    intent.putExtras(theResourceBundle);

    // Create the TaskStackBuilder and add the intent, which inflates the back stack
    TaskStackBuilder stackBuilder = TaskStackBuilder.create(currentContext);
    stackBuilder.addNextIntentWithParentStack(intent);
    // Get the PendingIntent containing the entire back stack
    PendingIntent startUpdateIntent =
      stackBuilder.getPendingIntent(notification_id, PendingIntent.FLAG_IMMUTABLE | PendingIntent.FLAG_UPDATE_CURRENT);

    Builder builder = new Builder(currentContext, getClass().getName())
      .setSmallIcon(R.drawable.ic_keyboard)
      .setContentTitle(currentContext.getString(R.string.keyboard_updates_available))
      .setContentText(message)
      .setPriority(NotificationCompat.PRIORITY_DEFAULT)
      .setAutoCancel(true)
      .setContentIntent(startUpdateIntent);

    try {
      notificationManager.notify(notification_id, builder.build());
    } catch (SecurityException e) {
      KMLog.LogException(TAG, "Security permission POST_NOTIFICATIONS denied", e);
    }
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
    // For local and PR test builds, invalidate cache to make keyboard updates easier
    if (VersionUtils.isLocalOrTestBuild() || FORCE_RESOURCE_UPDATE) {
      return true;
    }
    boolean shouldCheckUpdate = false;
    if (lastUpdateCheck == null) {
      SharedPreferences prefs = aContext.getSharedPreferences(aContext.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
      Long lastUpdateCheckTime = prefs.getLong(PREF_KEY_LAST_UPDATE_CHECK, 0);
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

    return shouldCheckUpdate;
  }

  @Override
  public void onKeyboardDownloadStarted(HashMap<String, String> keyboardInfo) {
    // Do nothing
  }

  @Override
  public void onKeyboardDownloadFinished(HashMap<String, String> keyboardInfo, int result) {
    // Do nothing
  }

  @Override
  public void onPackageInstalled(List<Map<String, String>> keyboardsInstalled) {
    if (! openUpdates.isEmpty() && keyboardsInstalled != null) {
      for (Map<String, String> k : keyboardsInstalled) {
        String _langid = k.get(KMManager.KMKey_LanguageID);
        String _kbid = k.get(KMManager.KMKey_KeyboardID);
        removeOpenUpdate(createKeyboardId(_langid, _kbid));
      }
    }

    tryFinalizeUpdate();
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
      // Trigger a host-page reset - we need to transition to the up-to-date versions.
      // TODO: make it smoother.  Documented as #11097.
      KMManager.clearKeyboardCache();

      Context appContext = currentContext.getApplicationContext();
      if (failedUpdateCount > 0) {
        BaseActivity.makeToast(appContext, R.string.update_failed, Toast.LENGTH_SHORT);
        lastUpdateCheck = Calendar.getInstance();
        updateFailed = true;
        checkingUpdates = false;
      } else {
        lastUpdateCheck = Calendar.getInstance();
        SharedPreferences prefs = appContext.getSharedPreferences(currentContext.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = prefs.edit();
        editor.putLong(PREF_KEY_LAST_UPDATE_CHECK, lastUpdateCheck.getTime().getTime());
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

  public void cancelKeyboardUpdate(String aLangId, String aKbId)
  {
    setPrefKeyIgnoreNotifications(aKbId);
    removeOpenUpdate(createKeyboardId(aLangId,aKbId));
    if(openUpdates.isEmpty())
      checkingUpdates = false;
  }

  public void cancelLexicalModelUpdate(String aLangId, String aModelId)
  {
    setPrefKeyIgnoreNotifications(aModelId);
    removeOpenUpdate(createLexicalModelId(aLangId,aModelId));
    if(openUpdates.isEmpty())
      checkingUpdates = false;
  }

  /**
   * Install available open Updates.
   */
  public void executeOpenUpdates()
  {
    for(OngoingUpdate _up:openUpdates.values())
    {
      Intent intent = new Intent(currentContext, KMKeyboardDownloaderActivity.class);
      intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
      intent.putExtras(_up.bundle);
      currentContext.startActivity(intent);
    }
  }
}
