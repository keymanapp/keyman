package com.keyman.android.tests.keyboardHarness;

import android.app.Activity;
import android.content.res.Configuration;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;

import com.tavultesoft.kmea.KMKeyboardDownloaderActivity;
import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.KMTextView;
import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardEventListener;
import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardDownloadEventListener;
import com.tavultesoft.kmea.KMManager.KeyboardType;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class MainActivity extends Activity implements OnKeyboardEventListener, OnKeyboardDownloadEventListener {

  private KMTextView textView;

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);

    KMManager.setDebugMode(true);
    KMManager.initialize(this, KeyboardType.KEYBOARD_TYPE_INAPP);

    setContentView(R.layout.activity_main);
    textView = (KMTextView) findViewById(R.id.kmTextView);
    final String KeyboardFont = "{\"family\":\"LatinWeb\",\"source\":[\"DejaVuSans.ttf\"]}";

    // Add custom keyboards

    // Chirality test keyboard
    HashMap<String, String> chiralityKBInfo = new HashMap<String, String>();
    chiralityKBInfo.put(KMManager.KMKey_PackageID, "cloud");
    chiralityKBInfo.put(KMManager.KMKey_KeyboardID, "chirality");
    chiralityKBInfo.put(KMManager.KMKey_LanguageID, "en");
    chiralityKBInfo.put(KMManager.KMKey_KeyboardName, "Chirality Keyboard");
    chiralityKBInfo.put(KMManager.KMKey_LanguageName, "English");
    chiralityKBInfo.put(KMManager.KMKey_KeyboardVersion, "1.0");
    chiralityKBInfo.put(KMManager.KMKey_Font, KeyboardFont);
    KMManager.addKeyboard(this, chiralityKBInfo);

    // Longpress test keyboard
    HashMap<String, String> longpressKBbInfo = new HashMap<String, String>();
    longpressKBbInfo.put(KMManager.KMKey_PackageID, "cloud");
    longpressKBbInfo.put(KMManager.KMKey_KeyboardID, "longpress");
    longpressKBbInfo.put(KMManager.KMKey_LanguageID, "en");
    longpressKBbInfo.put(KMManager.KMKey_KeyboardName, "Longpress Keyboard");
    longpressKBbInfo.put(KMManager.KMKey_LanguageName, "English");
    longpressKBbInfo.put(KMManager.KMKey_KeyboardVersion, "1.0");
    longpressKBbInfo.put(KMManager.KMKey_Font, "code2001.ttf");
    KMManager.addKeyboard(this, longpressKBbInfo);

    // Platform test keyboard
    HashMap<String, String> platformtestKBbInfo = new HashMap<String, String>();
    platformtestKBbInfo.put(KMManager.KMKey_PackageID, "cloud");
    platformtestKBbInfo.put(KMManager.KMKey_KeyboardID, "platformtest");
    platformtestKBbInfo.put(KMManager.KMKey_LanguageID, "en");
    platformtestKBbInfo.put(KMManager.KMKey_KeyboardName, "platformtest Keyboard");
    platformtestKBbInfo.put(KMManager.KMKey_LanguageName, "English");
    platformtestKBbInfo.put(KMManager.KMKey_KeyboardVersion, "1.0");
    platformtestKBbInfo.put(KMManager.KMKey_Font, KeyboardFont);
    KMManager.addKeyboard(this, platformtestKBbInfo);
  }

  @Override
  public boolean onCreateOptionsMenu(Menu menu) {
    // Inflate the menu; this adds items to the action bar if it is present.
    getMenuInflater().inflate(R.menu.menu_main, menu);
    return true;
  }

  @Override
  public boolean onOptionsItemSelected(MenuItem item) {
    // Handle action bar item clicks here. The action bar will
    // automatically handle clicks on the Home/Up button, so long
    // as you specify a parent activity in AndroidManifest.xml.
    int id = item.getItemId();

    //noinspection SimplifiableIfStatement
    if (id == R.id.action_settings) {
      return true;
    }

    return super.onOptionsItemSelected(item);
  }

  @Override
  protected void onResume() {
    super.onResume();
    KMManager.onResume();
    KMManager.addKeyboardEventListener(this);
    KMKeyboardDownloaderActivity.addKeyboardDownloadEventListener(this);
  }

  @Override
  protected void onPause() {
    super.onPause();
    KMManager.onPause();
    KMManager.removeKeyboardEventListener(this);

    // Intentionally not removing KeyboardDownloadEventListener to
    // ensure onKeyboardDownloadFinished() gets called
  }

  @Override
  public void onConfigurationChanged(Configuration newConfig) {
    super.onConfigurationChanged(newConfig);
  }

  @Override
  public void onKeyboardLoaded(KeyboardType keyboardType) {
    // Handle Keyman keyboard loaded event here if needed
  }

  @Override
  public void onKeyboardChanged(String newKeyboard) {
    // Handle Keyman keyboard changed event here if needed
    textView.setTypeface(KMManager.getKeyboardTextFontTypeface(this));
  }

  @Override
  public void onKeyboardShown() {
    // Handle Keyman keyboard shown event here if needed
  }

  @Override
  public void onKeyboardDismissed() {
    // Handle Keyman keyboard dismissed event here if needed
  }

  @Override
  public void onKeyboardDownloadStarted(HashMap<String, String> keyboardInfo) {
    // Handle Keyman keyboard download started event here if needed
  }

  @Override
  public void onKeyboardDownloadFinished(HashMap<String, String> keyboardInfo, int result) {
    // Handle Keyman keyboard download finished event here if needed
  }

  @Override
  public void onPackageInstalled(List<Map<String, String>> keyboardInfo) {
  }
}
