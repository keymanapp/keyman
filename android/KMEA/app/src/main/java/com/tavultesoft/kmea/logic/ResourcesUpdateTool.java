package com.tavultesoft.kmea.logic;

import android.app.Application;
import android.app.ProgressDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.os.Handler;
import android.widget.Toast;

import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;

import com.tavultesoft.kmea.KMKeyboardDownloaderActivity;
import com.tavultesoft.kmea.KeyboardPickerActivity;
import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.KeyboardEventHandler;
import com.tavultesoft.kmea.R;
import com.tavultesoft.kmea.data.CloudRepository;

import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ResourcesUpdateTool implements KeyboardEventHandler.OnKeyboardDownloadEventListener, CloudRepository.UpdateHandler{

  private Context currentContext;

  private boolean updateCheckFailed = false;
  private boolean updateFailed = false;
  private Calendar lastUpdateCheck = null;
  private boolean checkingUpdates = false;

  private int updateCount = 0;
  private int failedUpdateCount = 0;


  private void checkResourceUpdatesInternal(boolean anInitialize) {

    Runnable onSuccess = new Runnable() {
      public void run() {
        if(updateCount > 0) {
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
        if(updateCount > 0) {
          return;
        }

        Toast.makeText(currentContext, currentContext.getString(R.string.update_check_unavailable), Toast.LENGTH_SHORT).show();
        lastUpdateCheck = Calendar.getInstance();
        updateCheckFailed = true;
        checkingUpdates = false;
      }
    };

    updateCount = 0;
    checkingUpdates = true;
    if(anInitialize)
      CloudRepository.shared.initializeDataSet(currentContext, this, onSuccess, onFailure);
    else
      CloudRepository.shared.updateDatasetIfNeeded(currentContext, this, onSuccess, onFailure);

  }

  public void onUpdateDetection(final List<Bundle> updatableResources) {
    failedUpdateCount = 0;
    updateCount = updatableResources.size();
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
          for (Bundle resourceBundle: updatableResources) {
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

  private boolean isContextAvailable() {
    if(currentContext instanceof AppCompatActivity)
      return !((AppCompatActivity) currentContext).isFinishing();
    return true;
  }

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

  private boolean shouldCheckUpdate(Context aContext) {
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
    // TODO:  Extremely temporary code for testing - remove before merging!
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

    if (updateCount > 0) {
      updateCount--;
    }

    tryFinalizeUpdate();
  }

  @Override
  public void onPackageInstalled(List<Map<String, String>> keyboardsInstalled) {
    // Do nothing
  }

  @Override
  public void onLexicalModelInstalled(List<Map<String, String>> lexicalModelsInstalled) {
    if (updateCount > 0) {
      updateCount -= lexicalModelsInstalled.size();
    }

    tryFinalizeUpdate();
  }

  void tryFinalizeUpdate() {
    if (updateCount == 0) {


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
}
