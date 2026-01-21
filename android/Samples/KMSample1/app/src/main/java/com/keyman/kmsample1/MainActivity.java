package com.keyman.kmsample1;

import android.content.Context;
import android.content.res.Configuration;
import android.graphics.Point;
import android.os.Bundle;
import android.util.TypedValue;
import android.view.Menu;
import android.view.MenuItem;

import androidx.constraintlayout.widget.ConstraintLayout;

import com.keyman.engine.BaseActivity;
import com.keyman.engine.data.Keyboard;
import com.keyman.engine.KMKeyboardDownloaderActivity;
import com.keyman.engine.KMManager;
import com.keyman.engine.KMTextView;
import com.keyman.engine.KeyboardEventHandler.OnKeyboardEventListener;
import com.keyman.engine.KeyboardEventHandler.OnKeyboardDownloadEventListener;
import com.keyman.engine.KMManager.KeyboardType;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class MainActivity extends BaseActivity implements OnKeyboardEventListener, OnKeyboardDownloadEventListener {

  public static Context context;
  private ConstraintLayout constraintLayout;
  private KMTextView textView;
  private int lastOrientation = Configuration.ORIENTATION_UNDEFINED;

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    setTheme(R.style.AppTheme);
    super.onCreate(savedInstanceState);
    context = this;

    KMManager.setDebugMode(true);
    KMManager.initialize(this, KeyboardType.KEYBOARD_TYPE_INAPP);

    setContentView(R.layout.activity_main);
    constraintLayout = findViewById(R.id.constraintLayout);
    setupEdgeToEdge(R.id.constraintLayout);
    setupStatusBarColors(
      android.R.color.white,        // Color for top status bar
      android.R.color.darker_gray); // Color for bottom navigation bar

    textView = (KMTextView) findViewById(R.id.kmTextView);

    // Add a custom keyboard
    Keyboard kbInfo = new Keyboard(
      "basic_kbdtam99", // Package ID - filename of the .kmp file
      "basic_kbdtam99", // Keyboard ID
      "Tamil 99 Basic", // Keyboard Name
      "ta",             // Language ID
      "Tamil",          // Language Name
      "1.0",            // Keyboard Version
      null,             // URL to help documentation if available
      "",               // URL to latest .kmp file
      true,             // Boolean to show this is a new keyboard in the keyboard picker

      // Font information of the .ttf font to use in KMSample1 (for example "aava1.ttf").
      // basic_kbdtam99 doesn't include a font. Can set blank "" or KMManager.KMDefault_KeyboardFont
      KMManager.KMDefault_KeyboardFont,  // Font for KMSample1 text field
      KMManager.KMDefault_KeyboardFont); // Font for OSK
    KMManager.addKeyboard(this, kbInfo);

    // Add a dictionary
    HashMap<String, String>lexicalModelInfo = new HashMap<String, String>();
    lexicalModelInfo.put(KMManager.KMKey_PackageID, "example.ta.wordlist");
    lexicalModelInfo.put(KMManager.KMKey_LanguageID, "ta");
    lexicalModelInfo.put(KMManager.KMKey_LanguageName, "Tamil");
    lexicalModelInfo.put(KMManager.KMKey_LexicalModelID, "example.ta.wordlist");
    lexicalModelInfo.put(KMManager.KMKey_LexicalModelVersion, "1.0");
    KMManager.addLexicalModel(context, lexicalModelInfo);
    KMManager.registerAssociatedLexicalModel("ta");
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

    // onConfigurationChanged() only triggers when device is rotated while app is in foreground
    // This handles when device is rotated while app is in background
    // using KMManager.getOrientation() since getConfiguration().orientation is unreliable #10241
    int newOrientation = KMManager.getOrientation(context);
    if (newOrientation != lastOrientation) {
      lastOrientation = newOrientation;
      Configuration newConfig = this.getResources().getConfiguration();
      KMManager.onConfigurationChanged(newConfig);
    }
    resizeTextView(textView.isKeyboardVisible());

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
  	KMManager.onConfigurationChanged(newConfig);
	
	  resizeTextView(textView.isKeyboardVisible());
	  lastOrientation = newConfig.orientation;
  }

  @Override
  public void onKeyboardLoaded(KeyboardType keyboardType) {
    // Handle Keyman keyboard loaded event here if needed
  }

  @Override
  public void onKeyboardChanged(String newKeyboard) {
    // Handle Keyman keyboard changed event here if needed
  }

  @Override
  public void onKeyboardShown() {
    // Handle Keyman keyboard shown event here if needed
    resizeTextView(true);
  }

  @Override
  public void onKeyboardDismissed() {
    // Handle Keyman keyboard dismissed event here if needed
    resizeTextView(false);
  }

  public void resizeTextView(boolean isKeyboardVisible) {
    int bannerHeight = 0;
    int keyboardHeight = 0;
    if (isKeyboardVisible) {
      bannerHeight = KMManager.getBannerHeight(this);
      keyboardHeight = KMManager.getKeyboardHeight(this);
    }

    TypedValue outValue = new TypedValue();
    getTheme().resolveAttribute(android.R.attr.actionBarSize, outValue, true);
    int actionBarHeight = getResources().getDimensionPixelSize(outValue.resourceId);

    // *** TO DO: Try to check if status bar is visible, set statusBarHeight to 0 if it is not visible ***
    int statusBarHeight = 0;
    int resourceId = getResources().getIdentifier("status_bar_height", "dimen", "android");
    if (resourceId > 0) {
      statusBarHeight = getResources().getDimensionPixelSize(resourceId);
    }
    int navigationBarHeight = KMManager.getNavigationBarHeight(context, KeyboardType.KEYBOARD_TYPE_INAPP);

    Point size = KMManager.getWindowSize(context);
    int screenHeight = size.y;
    textView.setHeight(
      screenHeight - statusBarHeight - actionBarHeight - bannerHeight - keyboardHeight - navigationBarHeight);
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

  @Override
  public void onLexicalModelInstalled(List<Map<String, String>> lexicalModelsInstalled) {
  }

}
