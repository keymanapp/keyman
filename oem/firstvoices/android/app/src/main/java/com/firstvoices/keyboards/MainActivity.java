package com.firstvoices.keyboards;

import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.view.View;
import android.webkit.JavascriptInterface;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;

import io.sentry.android.core.SentryAndroid;

import com.tavultesoft.kmea.*;
import com.tavultesoft.kmea.data.Keyboard;
import com.tavultesoft.kmea.util.DownloadFileUtils;
import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardDownloadEventListener;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class MainActivity extends AppCompatActivity implements OnKeyboardDownloadEventListener {
    public static Context context;

    FVDownloadResultReceiver resultReceiver;

    @SuppressWarnings("SetJavascriptEnabled")
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        context = this;

        SentryAndroid.init(context, options -> {
            options.setRelease("release-"+com.firstvoices.keyboards.BuildConfig.VERSION_NAME);
            options.setEnvironment(com.firstvoices.keyboards.BuildConfig.VERSION_ENVIRONMENT);
        });

        resultReceiver = new FVDownloadResultReceiver(new Handler(), context);

        setContentView(R.layout.activity_main);

        new FVShared(this);

        FVShared.getInstance().upgradeTo12();
        FVShared.getInstance().upgradeTo14();
        FVShared.getInstance().preloadPackages();

        if (BuildConfig.DEBUG) {
            KMManager.setDebugMode(true);
        }
        KMManager.initialize(getApplicationContext(), KMManager.KeyboardType.KEYBOARD_TYPE_INAPP);

        /**
         * We need to set the default (fallback) keyboard to sil_euro_latin inside the fv_all package
         * rather than the normal default of sil_euro_latin inside the sil_euro_latin package.
         * Fallback keyboard needed in case the user never selects a FV keyboard to add
         * as a system keyboard.
        */
        String version = KMManager.getLatestKeyboardFileVersion(
            context, FVShared.FVDefault_PackageID, KMManager.KMDefault_KeyboardID);
        KMManager.setDefaultKeyboard(
            new Keyboard(
                FVShared.FVDefault_PackageID,
                KMManager.KMDefault_KeyboardID,
                KMManager.KMDefault_KeyboardName,
                KMManager.KMDefault_LanguageID,
                KMManager.KMDefault_LanguageName,
                version,
                null, // will use help.keyman.com link because context required to determine local welcome.htm path,
                "",
                false,
                KMManager.KMDefault_KeyboardFont,
                KMManager.KMDefault_KeyboardFont)
        );

      final String htmlPath = "file:///android_asset/setup/main.html";
        WebView webView = findViewById(R.id.webView);
        webView.addJavascriptInterface(new JSHandler(context), "jsInterface");
        webView.getSettings().setLayoutAlgorithm(WebSettings.LayoutAlgorithm.SINGLE_COLUMN);
        webView.getSettings().setJavaScriptEnabled(true);
        webView.getSettings().setSupportZoom(false);
        webView.getSettings().setUseWideViewPort(true);
        webView.getSettings().setLoadWithOverviewMode(true);
        webView.setLayerType(View.LAYER_TYPE_SOFTWARE, null);
        /*
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
            webView.setWebContentsDebuggingEnabled(true);
        }*/

        webView.setWebViewClient(new WebViewClient() {
            @Override
            public void onReceivedError(WebView view, int errorCode, String description, String failingUrl) {
                // Handle the error
            }

            @Override
            public boolean shouldOverrideUrlLoading(WebView view, String url) {
                if (url.equals(htmlPath)) {
                    view.loadUrl(url);
                }
                else {
                    Intent i = new Intent(Intent.ACTION_VIEW, Uri.parse(url));
                    startActivity(i);
                }

                return true;
            }

            @Override
            public void onPageFinished(WebView view, String url) {
                if (didCompleteSelectKeyboard())
                    view.loadUrl("javascript:setCheckBoxOn('checkbox1');");
                else
                    view.loadUrl("javascript:setCheckBoxOff('checkbox1');");

                if (didCompleteSetup())
                    view.loadUrl("javascript:setCheckBoxOn('checkbox2');");
                else
                    view.loadUrl("javascript:setCheckBoxOff('checkbox2');");
            }
        });

        webView.loadUrl(htmlPath);
    }

    public static void useLocalKMP(Context context, Uri data, boolean silentInstall) {
      DownloadFileUtils.Info info = DownloadFileUtils.cacheDownloadFile(context, data);
      boolean isKMP = info.isKMP();
      String filename = info.getFilename();
      File cacheKMPFile = info.getFile();

      if (filename == null || filename.isEmpty() || cacheKMPFile == null || !cacheKMPFile.exists()) {
        // failed to retrieve downloaded file
        String message = context.getString(R.string.failed_to_retrieve_file);
        Toast.makeText(context, message, Toast.LENGTH_LONG).show();
        return;
      } else if (!isKMP) {
        String noKeyboardsInstalledMessage = String.format(
          context.getString(R.string.not_valid_package_file), filename, context.getString(R.string.no_targets_to_install));
        Toast.makeText(context, noKeyboardsInstalledMessage, Toast.LENGTH_LONG).show();
        return;
      }

      if (cacheKMPFile != null) {
        Bundle bundle = new Bundle();
        bundle.putString("kmpFile", cacheKMPFile.getAbsolutePath());
        bundle.putBoolean("silentInstall", silentInstall);

        Intent packageIntent = new Intent(context, PackageActivity.class);
        packageIntent.putExtras(bundle);
        context.startActivity(packageIntent);
      }
    }

    @Override
    protected void onResume() {
        super.onResume();

        KMManager.onResume();
        KMKeyboardDownloaderActivity.addKeyboardDownloadEventListener(this);
        PackageActivity.addKeyboardDownloadEventListener(this);

        Intent intent = getIntent();
        Uri loadingIntentUri = intent.getData();

        if (loadingIntentUri != null) {
          String scheme = loadingIntentUri.getScheme().toLowerCase();
          switch (scheme) {
            // content:// Android DownloadManager
            case "content":
              // TODO: checkStoragePermission(loadingIntentUri);
              useLocalKMP(context, loadingIntentUri, true);
              break;
          }
        }

        WebView webView = findViewById(R.id.webView);
        if (webView != null) {
            if (didCompleteSelectKeyboard())
                webView.loadUrl("javascript:setCheckBoxOn('checkbox1');");
            else
                webView.loadUrl("javascript:setCheckBoxOff('checkbox1');");

            if (didCompleteSetup())
                webView.loadUrl("javascript:setCheckBoxOn('checkbox2');");
            else
                webView.loadUrl("javascript:setCheckBoxOff('checkbox2');");
        }
    }

    @Override
    protected void onPause() {
      super.onPause();
      KMManager.onPause();

      // Intentionally not removing KeyboardDownloadEventListener to
      // ensure onKeyboardDownloadFinished() gets called
    }

    /*
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
    */

    private static final class JSHandler {
        final private Context context;

        JSHandler(Context context) { this.context = context; }

        @SuppressWarnings("unused")
        @JavascriptInterface
        public void showSetup() {
            Intent setupIntent = new Intent(context, SetupActivity.class);
            context.startActivity(setupIntent);
        }

        @SuppressWarnings("unused")
        @JavascriptInterface
        public void showRegionList() {
            Intent i = new Intent(context, RegionListActivity.class);
            i.addFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);
            context.startActivity(i);
        }
    }

    private static boolean didCompleteSelectKeyboard() {
        return FVShared.getInstance().activeKeyboardCount() > 0;
    }

    private boolean didCompleteSetup() {
        if (!SetupActivity.isEnabledSystemWide(this))
            return false;

        return SetupActivity.isDefaultKB(this);
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
      // Do nothing
    }

    @Override
    public void onLexicalModelInstalled(List<Map<String, String>> lexicalModelsInstalled) {
      String langId = (KMManager.getCurrentKeyboardInfo(this) != null) ?
        KMManager.getCurrentKeyboardInfo(this).getLanguageID() :
        KMManager.KMDefault_LanguageID;
      boolean matchingModel = false;
      String lexicalModelName = "";

      for(int i=0; i<lexicalModelsInstalled.size(); i++) {
        HashMap<String, String>lexicalModelInfo = new HashMap<>(lexicalModelsInstalled.get(i));
        if(lexicalModelInfo.get(KMManager.KMKey_LanguageID).equals(langId)) {
          matchingModel = true;
          lexicalModelName = lexicalModelInfo.get(KMManager.KMKey_LexicalModelName);
        }
        KMManager.addLexicalModel(this, lexicalModelInfo);
      }

      // We're on the main thread, so if the active keyboard's language code matches,
      // let's register the associated lexical model.
      if(matchingModel) {
        KMManager.registerAssociatedLexicalModel(langId);

        // Update associated dictionary string if applicable
        FVKeyboardSettingsActivity.setActiveLexicalModelLabel(lexicalModelName);

      }

    }

}
