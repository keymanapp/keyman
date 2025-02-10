/**
 * Keyman is copyright (C) SIL Global. MIT License
 */
package com.keyman.engine;

import android.content.Context;
import android.os.Bundle;
import android.view.View;
import android.view.inputmethod.InputMethodManager;
import android.widget.Button;
import android.widget.LinearLayout;

/**
 * Activity to display when WebView is either not installed or enabled.
 * Prompts user to switch system keyboard.
 */
public class WebViewErrorActivity extends BaseActivity {
  private static final String TAG = "WebViewErrorActivity";

  private static LinearLayout linearLayout = null;
  private static Button changeIMEButton = null;

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);

    final Context context = this;
    setContentView(R.layout.activity_webview_error);

    linearLayout = (LinearLayout) findViewById(R.id.webViewErrorLayout);
    changeIMEButton = (Button) findViewById(R.id.changeIMEButton);
    changeIMEButton.setOnClickListener(new View.OnClickListener() {
      public void onClick(View v) {
        InputMethodManager imManager = (InputMethodManager) context.getSystemService(Context.INPUT_METHOD_SERVICE);
        imManager.showInputMethodPicker();

        // Dismiss the View
        linearLayout.setVisibility(View.GONE);
        //finish(); Dismiss IME picker too fast
      }
    });
  }
}