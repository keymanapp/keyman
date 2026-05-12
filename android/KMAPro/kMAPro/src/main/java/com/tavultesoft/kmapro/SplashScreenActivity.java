package com.tavultesoft.kmapro;

import android.content.SharedPreferences;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.os.Handler;
import android.content.Intent;
import android.widget.TextView;

import androidx.appcompat.app.AppCompatActivity;
import java.util.Calendar;

public class SplashScreenActivity extends AppCompatActivity {
  private final static String TAG = "Splash";
  // For now, disable this Activity so app is not delayed showing version/copyright text
  private static final int SPLASH_TIME_OUT = 0; // msec

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.activity_splash_screen);

    TextView version = (TextView)findViewById(R.id.splash_version);
    try {
      PackageInfo pInfo = getPackageManager().getPackageInfo(getPackageName(), 0);
      String ver = String.format("%s: %s", version.getText(), pInfo.versionName);
    } catch (PackageManager.NameNotFoundException e) {
      // Could not get version number
    }
    // TODO: Add version and copyright info when Android supports TextViews in the splash screen
    //version.setText(ver);

    TextView copyright = (TextView)findViewById(R.id.splash_copyright);
    int year = Calendar.getInstance().get(Calendar.YEAR);
    String date = String.format("%s%s", copyright.getText(), year);
    //copyright.setText(date);

    new Handler().postDelayed(new Runnable() {
      @Override
      public void run() {
        Intent intent = new Intent(getApplicationContext(),
          MainActivity.class);
        startActivity(intent);
        finish();
      }
    }, SPLASH_TIME_OUT);
  }
}
