package com.keyman.kmsample1;

import android.app.Activity;
import android.content.res.Configuration;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;

import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.KMTextView;
import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardEventListener;
import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardDownloadEventListener;
import com.tavultesoft.kmea.KMManager.KeyboardType;

import java.util.HashMap;

public class MainActivity extends Activity implements OnKeyboardEventListener, OnKeyboardDownloadEventListener {

  private KMTextView textView;

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);

    KMManager.setDebugMode(true);
    KMManager.initialize(this, KeyboardType.KEYBOARD_TYPE_INAPP);

    setContentView(R.layout.activity_main);
    textView = (KMTextView) findViewById(R.id.kmTextView);

    // Add a custom keyboard
    HashMap<String, String> kbInfo = new HashMap<String, String>();
    kbInfo.put(KMManager.KMKey_KeyboardID, "tamil99m");
    kbInfo.put(KMManager.KMKey_LanguageID, "tam");
    kbInfo.put(KMManager.KMKey_KeyboardName, "Tamil 99M");
    kbInfo.put(KMManager.KMKey_LanguageName, "Tamil");
    kbInfo.put(KMManager.KMKey_KeyboardVersion, "1.1");
    kbInfo.put(KMManager.KMKey_Font, "aava1.ttf");
    KMManager.addKeyboard(this, kbInfo);
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
    KMManager.addKeyboardDownloadEventListener(this);
  }

  @Override
  protected void onPause() {
    super.onPause();
    KMManager.onPause();
    KMManager.removeKeyboardEventListener(this);
    KMManager.removeKeyboardDownloadEventListener(this);
  }

  @Override
  public void onConfigurationChanged(Configuration newConfig) {
    super.onConfigurationChanged(newConfig);
  }

  @Override
  public void onKeyboardLoaded(KeyboardType keyboardType) {
    // Handle Keyman keyboard loaded event here if needed
    // We can set our custom keyboard here
    int kbIndex = KMManager.getKeyboardIndex(this, "tamil99m", "tam");
    KMManager.setKeyboard(this, kbIndex);
  }

  @Override
  public void onKeyboardChanged(String newKeyboard) {
    // Handle Keyman keyboard changed event here if needed
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
}
