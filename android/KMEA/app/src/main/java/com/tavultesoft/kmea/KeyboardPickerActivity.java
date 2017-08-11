/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;

import org.json.JSONArray;
import org.json.JSONObject;

import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardDownloadEventListener;

import android.app.Activity;
import android.app.AlertDialog;
import android.app.ProgressDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.DialogInterface.OnCancelListener;
import android.content.Intent;
import android.content.SharedPreferences;
import android.graphics.Typeface;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.Handler;
import android.util.Log;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemLongClickListener;
import android.widget.BaseAdapter;
import android.widget.ImageButton;
import android.widget.ListView;
import android.widget.PopupMenu;
import android.widget.Toast;

public final class KeyboardPickerActivity extends Activity implements OnKeyboardDownloadEventListener {

  private static ListView listView = null;
  private static KMKeyboardPickerAdapter listAdapter = null;
  private static ArrayList<HashMap<String, String>> keyboardsList = null;
  private static HashMap<String, String> keyboardVersions = null;
  private static boolean checkingUpdates = false;
  private static int updateCount = 0;
  private static int failedUpdateCount = 0;
  private static ProgressDialog updateProgress;
  private static boolean didUpdate = false;
  private static boolean updateCheckFailed = false;
  private static boolean updateFailed = false;
  private static Calendar lastUpdateCheck = null;
  private static int selectedIndex = 0;

  protected static int selectedIndex() {
    return selectedIndex;
  }

  private boolean dismissOnSelect = true;
  protected static boolean canAddNewKeyboard = true;
  protected static boolean canRemoveKeyboard = true;
  protected static boolean shouldCheckKeyboardUpdates = true;
  protected static Typeface listFont = null;

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    final Context context = this;
    requestWindowFeature(Window.FEATURE_CUSTOM_TITLE);
    try {
      int titleContainerId = (Integer) Class.forName("com.android.internal.R$id").getField("title_container").get(null);
      ((ViewGroup) getWindow().findViewById(titleContainerId)).removeAllViews();
    } catch (Exception e) {
      Log.e("KeyboardPickerActivity", "Error: " + e);
    }

    getWindow().setFeatureInt(Window.FEATURE_CUSTOM_TITLE, R.layout.list_title_layout2);
    setContentView(R.layout.list_layout);
    listView = (ListView) findViewById(R.id.listView);

    File file = new File(context.getDir("userdata", Context.MODE_PRIVATE), KMManager.KMFilename_KeyboardsList);
    if (file.exists()) {
      keyboardsList = getKeyboardsList(context);
    } else {
      keyboardsList = new ArrayList<HashMap<String, String>>();
      HashMap<String, String> kbInfo = new HashMap<String, String>();
      kbInfo.put(KMManager.KMKey_KeyboardID, KMManager.KMDefault_KeyboardID);
      kbInfo.put(KMManager.KMKey_LanguageID, KMManager.KMDefault_LanguageID);
      kbInfo.put(KMManager.KMKey_KeyboardName, KMManager.KMDefault_KeyboardName);
      kbInfo.put(KMManager.KMKey_LanguageName, KMManager.KMDefault_LanguageName);
      kbInfo.put(KMManager.KMKey_KeyboardVersion, KMManager.getLatestKeyboardFileVersion(context, KMManager.KMDefault_KeyboardID));
      kbInfo.put(KMManager.KMKey_CustomKeyboard, "N");
      kbInfo.put(KMManager.KMKey_Font, KMManager.KMDefault_KeyboardFont);
      keyboardsList.add(kbInfo);
      saveKeyboardsList(context);
    }

    String[] from = new String[]{KMManager.KMKey_LanguageName, KMManager.KMKey_KeyboardName};
    int[] to = new int[]{R.id.text1, R.id.text2};
    listAdapter = new KMKeyboardPickerAdapter(context, keyboardsList, R.layout.list_row_layout3, from, to);
    listAdapter.listFont = listFont;
    listView.setAdapter(listAdapter);
    listView.setChoiceMode(ListView.CHOICE_MODE_SINGLE);
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
      @Override
      public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        switchKeyboard(position);
        if (dismissOnSelect)
          finish();
      }
    });

    listView.setOnItemLongClickListener(new OnItemLongClickListener() {
      @Override
      public boolean onItemLongClick(AdapterView<?> parent, View view, final int position, long id) {
        if (position > 0 && canRemoveKeyboard) {
          PopupMenu popup = new PopupMenu(context, view);
          popup.getMenuInflater().inflate(R.menu.popup, popup.getMenu());
          popup.setOnMenuItemClickListener(new PopupMenu.OnMenuItemClickListener() {
            public boolean onMenuItemClick(MenuItem item) {
              if (item.getItemId() == R.id.popup_delete) {
                deleteKeyboard(position);
                return true;
              } else {
                return false;
              }
            }
          });
          popup.show();
          return true;
        } else {
          return false;
        }
      }
    });

    final ImageButton leftButton = (ImageButton) findViewById(R.id.left_button);
    leftButton.setOnClickListener(new View.OnClickListener() {
      public void onClick(View v) {
        finish();
      }
    });

    final ImageButton addButton = (ImageButton) findViewById(R.id.right_button);
    addButton.setOnClickListener(new View.OnClickListener() {
      public void onClick(View v) {
        if (KMManager.hasConnection(context) || LanguageListActivity.getCacheFile(context).exists()) {
          dismissOnSelect = false;
          Intent i = new Intent(context, LanguageListActivity.class);
          i.addFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);
          context.startActivity(i);
        } else {
          AlertDialog.Builder dialogBuilder = new AlertDialog.Builder(context);
          dialogBuilder.setTitle(context.getResources().getString(R.string.title_add_keyboard));
          dialogBuilder.setIcon(context.getResources().getDrawable(android.R.drawable.ic_input_add));
          dialogBuilder.setMessage("\nCannot connect to Keyman server!\n");
          dialogBuilder.setPositiveButton("OK", null);
          AlertDialog dialog = dialogBuilder.create();
          dialog.show();
        }
      }
    });
    if (!canAddNewKeyboard)
      addButton.setVisibility(View.GONE);

    int curKbPos = getCurrentKeyboardIndex();
    setSelection(curKbPos);
  }

  @Override
  protected void onResume() {
    super.onResume();
    KMManager.addKeyboardDownloadEventListener(this);
    BaseAdapter adapter = (BaseAdapter) listAdapter;
    adapter.notifyDataSetChanged();
    int curKbPos = getCurrentKeyboardIndex();
    setSelection(curKbPos);
    if (!shouldCheckKeyboardUpdates)
      return;

    final Context context = this;
    Handler handler = new Handler();
    handler.postDelayed(new Runnable() {
      @Override
      public void run() {
        boolean shouldCheckUpdate = false;
        if (lastUpdateCheck == null) {
          SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
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

        if (shouldCheckUpdate) {
          updateCheckFailed = false;
          updateFailed = false;
          if (!checkingUpdates) {
            checkKeyboardUpdates(context);
          }
        }
      }
    }, 1000);
  }

  @Override
  protected void onPause() {
    super.onPause();
    KMManager.removeKeyboardDownloadEventListener(this);
    if (didUpdate) {
      if (KMManager.InAppKeyboard != null) {
        KMManager.InAppKeyboard.loadKeyboard();
      }
      if (KMManager.SystemKeyboard != null) {
        KMManager.SystemKeyboard.loadKeyboard();
      }
      didUpdate = false;
    }
  }

  private static int getCurrentKeyboardIndex() {
    int pos = 0;

    if (keyboardsList != null) {
      int length = keyboardsList.size();
      for (int i = 0; i < length; i++) {
        HashMap<String, String> kbInfo = keyboardsList.get(i);
        String langId = kbInfo.get(KMManager.KMKey_LanguageID);
        String kbId = kbInfo.get(KMManager.KMKey_KeyboardID);
        String kbKey = String.format("%s_%s", langId, kbId);
        if (kbKey.equals(KMKeyboard.currentKeyboard())) {
          pos = i;
          break;
        }
      }
    }

    return pos;
  }

  private static boolean saveKeyboardsList(Context context) {
    boolean result;
    try {
      File file = new File(context.getDir("userdata", Context.MODE_PRIVATE), KMManager.KMFilename_KeyboardsList);
      ObjectOutputStream outputStream = new ObjectOutputStream(new FileOutputStream(file));
      outputStream.writeObject(keyboardsList);
      outputStream.flush();
      outputStream.close();
      result = true;
    } catch (Exception e) {
      Log.e("Failed to save keyboards list", "Error: " + e);
      result = false;
    }

    return result;
  }

  protected static boolean updateKeyboardsList(Context context, ArrayList<HashMap<String, String>> list) {
    boolean result;
    keyboardsList = list;
    result = saveKeyboardsList(context);
    return result;
  }

  private void setSelection(int position) {
    listView.setItemChecked(position, true);
    listView.setSelection(position);
    selectedIndex = position;
  }

  private void switchKeyboard(int position) {
    setSelection(position);
    HashMap<String, String> kbInfo = keyboardsList.get(position);
    String kbId = kbInfo.get(KMManager.KMKey_KeyboardID);
    String langId = kbInfo.get(KMManager.KMKey_LanguageID);
    String kbName = kbInfo.get(KMManager.KMKey_KeyboardName);
    String langName = kbInfo.get(KMManager.KMKey_LanguageName);
    String kFont = kbInfo.get(KMManager.KMKey_Font);
    String kOskFont = kbInfo.get(KMManager.KMKey_OskFont);
    if (KMManager.InAppKeyboard != null) {
      KMManager.InAppKeyboard.setKeyboard(kbId, langId, kbName, langName, kFont, kOskFont);
    }
    if (KMManager.SystemKeyboard != null) {
      KMManager.SystemKeyboard.setKeyboard(kbId, langId, kbName, langName, kFont, kOskFont);
    }
  }

  protected static boolean addKeyboard(Context context, HashMap<String, String> keyboardInfo) {
    boolean result = false;

    if (keyboardsList == null) {
      keyboardsList = getKeyboardsList(context);
    }

    if (keyboardsList == null) {
      keyboardsList = new ArrayList<HashMap<String, String>>();
    }

    if (keyboardInfo != null) {
      String kbID = keyboardInfo.get(KMManager.KMKey_KeyboardID);
      String langID = keyboardInfo.get(KMManager.KMKey_LanguageID);
      if (kbID != null && langID != null) {
        String kbKey = String.format("%s_%s", langID, kbID);
        if (kbKey.length() >= 3) {
          int x = getKeyboardIndex(context, kbKey);
          if (x >= 0) {
            keyboardsList.set(x, keyboardInfo);
            result = saveKeyboardsList(context);
          } else {
            keyboardsList.add(keyboardInfo);
            result = saveKeyboardsList(context);
            if (!result) {
              keyboardsList.remove(keyboardsList.size() - 1);
            }
          }
        }
      }
    }

    return result;
  }

  protected static boolean removeKeyboard(Context context, int position) {
    boolean result = false;
    if (keyboardsList == null) {
      keyboardsList = getKeyboardsList(context);
    }

    if (keyboardsList != null && position >= 0 && position < keyboardsList.size()) {
      keyboardsList.remove(position);
      result = saveKeyboardsList(context);
    }

    return result;
  }

  protected void deleteKeyboard(int position) {
    int curKbPos = getCurrentKeyboardIndex();
    boolean result = removeKeyboard(this, position);
    ;

    if (result) {
      Toast.makeText(this, "Keyboard deleted", Toast.LENGTH_SHORT).show();
      BaseAdapter adapter = (BaseAdapter) listAdapter;
      adapter.notifyDataSetChanged();
      if (position == curKbPos) {
        switchKeyboard(0);
      } else {
        curKbPos = getCurrentKeyboardIndex();
        setSelection(curKbPos);
      }
    }
  }

  @SuppressWarnings("unchecked")
  protected static ArrayList<HashMap<String, String>> getKeyboardsList(Context context) {
    ArrayList<HashMap<String, String>> list = null;

    File file = new File(context.getDir("userdata", Context.MODE_PRIVATE), KMManager.KMFilename_KeyboardsList);
    if (file.exists()) {
      try {
        ObjectInputStream inputStream = new ObjectInputStream(new FileInputStream(file));
        list = (ArrayList<HashMap<String, String>>) inputStream.readObject();
        inputStream.close();
      } catch (Exception e) {
        Log.e("Failed to read keyboards list", "Error: " + e);
        list = null;
      }
    }

    return list;
  }

  protected static boolean containsKeyboard(Context context, String keyboardKey) {
    if (keyboardsList == null) {
      keyboardsList = getKeyboardsList(context);
    }

    String kbKeys = "";
    if (keyboardsList != null) {
      int length = keyboardsList.size();
      for (int i = 0; i < length; i++) {
        kbKeys += String.format("%s_%s", keyboardsList.get(i).get(KMManager.KMKey_LanguageID), keyboardsList.get(i).get(KMManager.KMKey_KeyboardID));
        if (i < length - 1) {
          kbKeys += ",";
        }
      }
    }

    return kbKeys.contains(keyboardKey);
  }

  protected static int getCurrentKeyboardIndex(Context context) {
    int index = -1;

    if (keyboardsList == null) {
      keyboardsList = getKeyboardsList(context);
    }

    if (keyboardsList != null) {
      int length = keyboardsList.size();
      for (int i = 0; i < length; i++) {
        HashMap<String, String> kbInfo = keyboardsList.get(i);
        String langId = kbInfo.get(KMManager.KMKey_LanguageID);
        String kbId = kbInfo.get(KMManager.KMKey_KeyboardID);
        String kbKey = String.format("%s_%s", langId, kbId);
        if (kbKey.equals(KMKeyboard.currentKeyboard())) {
          index = i;
          break;
        }
      }
    }

    return index;
  }

  protected static HashMap<String, String> getCurrentKeyboardInfo(Context context) {
    int index = getCurrentKeyboardIndex(context);
    if (index >= 0) {
      return keyboardsList.get(index);
    } else {
      return null;
    }
  }

  protected static int getKeyboardIndex(Context context, String keyboardKey) {
    int index = -1;

    if (keyboardsList == null) {
      keyboardsList = getKeyboardsList(context);
    }

    if (keyboardsList != null) {
      int length = keyboardsList.size();
      for (int i = 0; i < length; i++) {
        HashMap<String, String> kbInfo = keyboardsList.get(i);
        String langId = kbInfo.get(KMManager.KMKey_LanguageID);
        String kbId = kbInfo.get(KMManager.KMKey_KeyboardID);
        String kbKey = String.format("%s_%s", langId, kbId);
        if (kbKey.equals(keyboardKey)) {
          index = i;
          break;
        }
      }
    }

    return index;
  }

  protected static HashMap<String, String> getKeyboardInfo(Context context, int index) {
    if (index < 0) {
      return null;
    }

    if (keyboardsList == null) {
      keyboardsList = getKeyboardsList(context);
    }

    if (keyboardsList != null && index < keyboardsList.size()) {
      return keyboardsList.get(index);
    }

    return null;
  }

  protected static void showLanguageList(Context context) {
    if (KMManager.hasConnection(context) || LanguageListActivity.getCacheFile(context).exists()) {
      Intent i = new Intent(context, LanguageListActivity.class);
      i.addFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);
      context.startActivity(i);
    } else {
      AlertDialog.Builder dialogBuilder = new AlertDialog.Builder(context);
      dialogBuilder.setTitle(context.getResources().getString(R.string.title_add_keyboard));
      dialogBuilder.setIcon(context.getResources().getDrawable(android.R.drawable.ic_input_add));
      dialogBuilder.setMessage("\nCannot connect to Keyman server!\n");
      dialogBuilder.setPositiveButton("OK", null);
      AlertDialog dialog = dialogBuilder.create();
      dialog.show();
    }
  }

  private static void checkKeyboardUpdates(final Context context) {
    new AsyncTask<Void, Integer, Integer>() {
      private final boolean hasConnection = KMManager.hasConnection(context);
      private ProgressDialog progressDialog;
      JSONParser jsonParser = new JSONParser();

      @Override
      protected void onPreExecute() {
        super.onPreExecute();
        checkingUpdates = true;
        if (hasConnection) {
          progressDialog = new ProgressDialog(context);
          progressDialog.setMessage("Checking keyboard updates...");
          progressDialog.setCancelable(false);
          if (!((Activity) context).isFinishing()) {
            progressDialog.show();
          } else {
            cancel(true);
            progressDialog = null;
          }
        }
      }

      @Override
      protected Integer doInBackground(Void... voids) {
        int ret = 0;
        if (hasConnection && !isCancelled()) {
          try {
            String deviceType = context.getResources().getString(R.string.device_type);
            if (deviceType.equals("AndroidTablet")) {
              deviceType = "androidtablet";
            } else {
              deviceType = "androidphone";
            }

            keyboardVersions = new HashMap<String, String>();
            int len = keyboardsList.size();
            for (int i = 0; i < len; i++) {
              String languageID = keyboardsList.get(i).get(KMManager.KMKey_LanguageID);
              String keyboardID = keyboardsList.get(i).get(KMManager.KMKey_KeyboardID);
              String kbVersion = keyboardsList.get(i).get(KMManager.KMKey_KeyboardVersion);
              String url = String.format("%slanguages/%s/%s?device=%s", KMManager.kKeymanApiBaseURL, languageID, keyboardID, deviceType);
              JSONObject kbData = jsonParser.getJSONObjectFromUrl(url);
              JSONObject language = kbData.optJSONObject(KMManager.KMKey_Language);
              JSONArray keyboards = language.getJSONArray(KMManager.KMKey_LanguageKeyboards);
              JSONObject keyboard = keyboards.getJSONObject(0);
              String newKbVersion = keyboard.optString(KMManager.KMKey_KeyboardVersion, "1.0");
              String kbKey = String.format("%s_%s", languageID, keyboardID);
              if (keyboardVersions.get(kbKey) == null) {
                keyboardVersions.put(kbKey, newKbVersion);
              }

              if (Float.valueOf(newKbVersion) > Float.valueOf(kbVersion)) {
                ret++;
              }
            }
          } catch (Exception e) {
            keyboardVersions = null;
            ret = -1;
          }
        }

        return ret;
      }

      @Override
      protected void onProgressUpdate(Integer... progress) {
        // Do nothing
      }

      @Override
      protected void onPostExecute(Integer result) {
        if (progressDialog != null && progressDialog.isShowing()) {
          try {
            progressDialog.dismiss();
            progressDialog = null;
          } catch (Exception e) {
            progressDialog = null;
          }
        }

        if (result > 0) {
          failedUpdateCount = 0;
          updateCount = result;
          AlertDialog.Builder dialogBuilder = new AlertDialog.Builder(context);
          dialogBuilder.setTitle("Keyboard Updates Available");
          dialogBuilder.setMessage("Would you like to update keyboards now?");
          dialogBuilder.setPositiveButton("Update", new DialogInterface.OnClickListener() {
            public void onClick(DialogInterface dialog, int which) {
              // Update keyboards
              if (KMManager.hasConnection(context)) {
                int len = keyboardsList.size();
                for (int i = 0; i < len; i++) {
                  String languageID = keyboardsList.get(i).get(KMManager.KMKey_LanguageID);
                  String keyboardID = keyboardsList.get(i).get(KMManager.KMKey_KeyboardID);
                  String kbKey = String.format("%s_%s", languageID, keyboardID);
                  String kbVersion = keyboardsList.get(i).get(KMManager.KMKey_KeyboardVersion);
                  String newKbVersion = keyboardVersions.get(kbKey);
                  if (newKbVersion != null) {
                    keyboardVersions.put(kbKey, newKbVersion);
                    if (Float.valueOf(newKbVersion) > Float.valueOf(kbVersion)) {
                      if (updateProgress == null || !updateProgress.isShowing()) {
                        updateProgress = new ProgressDialog(context);
                        updateProgress.setMessage("Updating keyboards...");
                        updateProgress.setCancelable(false);
                        updateProgress.show();
                      }

                      KMManager.KMKeyboardDownloader.download(context, keyboardID, languageID, false);
                    }
                  }
                }
              } else {
                Toast.makeText(context, "No internet connection", Toast.LENGTH_SHORT).show();
                checkingUpdates = false;
              }
            }
          });

          dialogBuilder.setNegativeButton("Later", new DialogInterface.OnClickListener() {
            public void onClick(DialogInterface dialog, int which) {
              lastUpdateCheck = Calendar.getInstance();
              checkingUpdates = false;
            }
          });

          AlertDialog dialog = dialogBuilder.create();
          if (!((Activity) context).isFinishing()) {
            dialog.setOnCancelListener(new OnCancelListener() {
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
        } else if (result == 0) {
          Toast.makeText(context, "All keyboards are up to date!", Toast.LENGTH_SHORT).show();
          lastUpdateCheck = Calendar.getInstance();
          SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
          SharedPreferences.Editor editor = prefs.edit();
          editor.putLong("lastUpdateCheck", lastUpdateCheck.getTime().getTime());
          editor.commit();
          checkingUpdates = false;
        } else {
          Toast.makeText(context, "Failed to access Keyman server!", Toast.LENGTH_SHORT).show();
          lastUpdateCheck = Calendar.getInstance();
          updateCheckFailed = true;
          checkingUpdates = false;
        }
      }
    }.execute();
  }

  /*
  @Override
  public void onBackPressed() {
    Intent setIntent = new Intent(Intent.ACTION_MAIN);
    setIntent.addCategory(Intent.CATEGORY_HOME);
    setIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
    startActivity(setIntent);
  }*/

  @Override
  public void onKeyboardDownloadStarted(HashMap<String, String> keyboardInfo) {
    // Do nothing
  }

  @Override
  public void onKeyboardDownloadFinished(HashMap<String, String> keyboardInfo, int result) {
    if (result > 0) {
      didUpdate = true;
      String keyboardID = keyboardInfo.get(KMManager.KMKey_KeyboardID);
      String languageID = keyboardInfo.get(KMManager.KMKey_LanguageID);
      String kbKey = String.format("%s_%s", languageID, keyboardID);
      int index = getKeyboardIndex(this, kbKey);
      keyboardsList.set(index, keyboardInfo);
      saveKeyboardsList(this);
    } else if (result < 0) {
      failedUpdateCount++;
    }

    if (updateCount > 0) {
      updateCount--;
    }

    if (updateCount == 0 && updateProgress != null && updateProgress.isShowing()) {
      if (updateProgress != null && updateProgress.isShowing()) {
        try {
          updateProgress.dismiss();
          updateProgress = null;
        } catch (Exception e) {
          updateProgress = null;
        }
      }

      if (failedUpdateCount > 0) {
        Toast.makeText(this, "One or more keyboards failed to update!", Toast.LENGTH_SHORT).show();
        keyboardVersions = null;
        lastUpdateCheck = Calendar.getInstance();
        updateFailed = true;
        checkingUpdates = false;
      } else {
        Toast.makeText(this, "Keyboards successfully updated!", Toast.LENGTH_SHORT).show();
        lastUpdateCheck = Calendar.getInstance();
        SharedPreferences prefs = getSharedPreferences(getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = prefs.edit();
        editor.putLong("lastUpdateCheck", lastUpdateCheck.getTime().getTime());
        editor.commit();
        checkingUpdates = false;
      }
    }
  }
}