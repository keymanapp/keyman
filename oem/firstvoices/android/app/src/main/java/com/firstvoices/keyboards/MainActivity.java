package com.firstvoices.keyboards;

import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
//import android.support.v8.app.ActionBarActivity;
import android.view.View;
import android.webkit.JavascriptInterface;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import androidx.appcompat.app.AppCompatActivity;

import io.sentry.android.core.SentryAndroid;
import io.sentry.core.Sentry;

import com.tavultesoft.kmea.*;
import com.tavultesoft.kmea.data.Keyboard;

import java.util.ArrayList;
import java.util.HashMap;

public class MainActivity extends AppCompatActivity {
    @SuppressWarnings("SetJavascriptEnabled")
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        SentryAndroid.init(this, options -> {
          options.setRelease("release-"+com.firstvoices.keyboards.BuildConfig.VERSION_NAME);
          options.setEnvironment(com.firstvoices.keyboards.BuildConfig.VERSION_ENVIRONMENT);
        });

        setContentView(R.layout.activity_main);

        new FVShared(this);

        FVShared.getInstance().upgradeTo12();
        FVShared.getInstance().preloadPackages();

        if (BuildConfig.DEBUG) {
            KMManager.setDebugMode(true);
        }
        KMManager.initialize(getApplicationContext(), KMManager.KeyboardType.KEYBOARD_TYPE_INAPP);

        final Context context = this;

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

        ArrayList<HashMap<String, String>> modelsList = KMManager.getLexicalModelsList(context);
        if (modelsList == null || modelsList.size() == 0) {
          String lexicalModelVersion = KMManager.getLexicalModelPackageVersion(
            context, FVShared.FVDefault_DictionaryPackageID);

          // Add default dictionaries
          HashMap<String, String> lexicalModelInfo = new HashMap<String, String>();
          lexicalModelInfo.put(KMManager.KMKey_PackageID, FVShared.FVDefault_DictionaryPackageID);
          lexicalModelInfo.put(KMManager.KMKey_LanguageID, FVShared.FVDefault_DictionaryLanguageID);
          lexicalModelInfo.put(KMManager.KMKey_LexicalModelID, FVShared.FVDefault_DictionaryModelID);
          lexicalModelInfo.put(KMManager.KMKey_LexicalModelName, FVShared.FVDefault_DictionaryModelName);
          lexicalModelInfo.put(KMManager.KMKey_LexicalModelVersion, lexicalModelVersion);
          KMManager.addLexicalModel(context, lexicalModelInfo);
          KMManager.registerAssociatedLexicalModel(FVShared.FVDefault_DictionaryLanguageID);
        }

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
                    view.loadUrl("javascript:setCheckBox1On();");
                else
                    view.loadUrl("javascript:setCheckBox1Off();");

                if (didCompleteSetup())
                    view.loadUrl("javascript:setCheckBox2On();");
                else
                    view.loadUrl("javascript:setCheckBox2Off();");
            }
        });

        webView.loadUrl(htmlPath);
    }

    @Override
    protected void onResume() {
        super.onResume();

        WebView webView = findViewById(R.id.webView);
        if (webView != null) {
            if (didCompleteSelectKeyboard())
                webView.loadUrl("javascript:setCheckBox1On();");
            else
                webView.loadUrl("javascript:setCheckBox1Off();");

            if (didCompleteSetup())
                webView.loadUrl("javascript:setCheckBox2On();");
            else
                webView.loadUrl("javascript:setCheckBox2Off();");
        }
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

    private boolean didCompleteSelectKeyboard() {
        return FVShared.getInstance().activeKeyboardCount() > 0;
    }

    private boolean didCompleteSetup() {
        if (!SetupActivity.isEnabledSystemWide(this))
            return false;

        return SetupActivity.isDefaultKB(this);
    }
}
