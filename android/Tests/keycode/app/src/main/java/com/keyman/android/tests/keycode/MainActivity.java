package com.keyman.android.tests.keycode;

import androidx.appcompat.app.AppCompatActivity;
import android.util.Log;
import android.view.KeyEvent;

import android.os.Bundle;
import android.widget.TextView;

public class MainActivity extends AppCompatActivity {

  @Override
  public boolean dispatchKeyEvent(KeyEvent event) {
    String info = String.format("keyCode: %d\nscanCode: %d", event.getKeyCode(), event.getScanCode());
    Log.d("hw", info);

    TextView textView = (TextView) findViewById(R.id.main_text);
    textView.setText(info);
    return true;
  }

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.activity_main);
  }
}
