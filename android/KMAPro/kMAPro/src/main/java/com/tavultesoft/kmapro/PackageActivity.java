package com.tavultesoft.kmapro;

import android.annotation.SuppressLint;
import androidx.appcompat.widget.Toolbar;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.app.AlertDialog;
import androidx.fragment.app.Fragment;

import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.graphics.Bitmap;
import android.os.Bundle;
import android.view.Gravity;
import android.view.View;
import android.view.View.OnClickListener;
import android.webkit.WebChromeClient;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.Button;
import android.widget.TextView;

import android.widget.Toast;

import com.stepstone.stepper.StepperLayout;
import com.stepstone.stepper.VerificationError;
import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.KeyboardEventHandler;
import com.tavultesoft.kmea.data.Keyboard;
import com.tavultesoft.kmea.packages.PackageProcessor;
import com.tavultesoft.kmea.packages.LexicalModelPackageProcessor;
import com.tavultesoft.kmea.util.FileUtils;
import com.tavultesoft.kmea.util.KMLog;

import org.json.JSONObject;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class PackageActivity extends AppCompatActivity implements
    StepperLayout.StepperListener, WebViewFragment.OnInstallClickedListener,
    SelectLanguageFragment.OnLanguagesSelectedListener {
  private final static String TAG = "PackageActivity";
  private AlertDialog alertDialog;
  private File kmpFile;
  private File tempPackagePath;
  private static ArrayList<KeyboardEventHandler.OnKeyboardDownloadEventListener> kbDownloadEventListeners = null;
  private PackageProcessor kmpProcessor;
  private String pkgName;
  private boolean hasWelcome;
  private StepperLayout mStepperLayout;
  private StepperAdapter mStepperAdapter;

  @SuppressLint({"SetJavaScriptEnabled", "InflateParams"})
  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.activity_package_installer);

    boolean silentInstall = false;
    String languageID = null;
    ArrayList<String> languageList = new ArrayList<String>();

    final Context context = this;
    Bundle bundle = getIntent().getExtras();
    if (bundle != null) {
      kmpFile = new File(bundle.getString("kmpFile"));
      silentInstall = bundle.getBoolean("silentInstall", false);
      languageID = bundle.getString("language", null);
      if (languageID != null && !languageID.isEmpty()) {
        languageList.add(languageID);
      }
    }

    File resourceRoot =  new File(context.getDir("data", Context.MODE_PRIVATE).toString() + File.separator);
    kmpProcessor =  new PackageProcessor(resourceRoot);
    final String pkgId = kmpProcessor.getPackageID(kmpFile);
    final String pkgTarget = kmpProcessor.getPackageTarget(kmpFile);

    try {
      if (pkgTarget.equals(PackageProcessor.PP_TARGET_LEXICAL_MODELS)) {
        kmpProcessor = new LexicalModelPackageProcessor(resourceRoot);
      } else if (!pkgTarget.equals(PackageProcessor.PP_TARGET_KEYBOARDS)) {
        showErrorToast(context, getString(R.string.no_targets_to_install));
        return;
      }
      tempPackagePath = kmpProcessor.unzipKMP(kmpFile);

    } catch (Exception e) {
      KMLog.LogException(TAG, "", e);
      showErrorToast(context, getString(R.string.failed_to_extract));
      return;
    }

    JSONObject pkgInfo = kmpProcessor.loadPackageInfo(tempPackagePath);
    if (pkgInfo == null) {
      showErrorToast(context, getString(R.string.invalid_metadata));
      return;
    }

    pkgName = kmpProcessor.getPackageName(pkgInfo);
    hasWelcome = kmpProcessor.hasWelcome(pkgInfo);
    final int keyboardCount = kmpProcessor.getKeyboardCount(pkgInfo);

    // Number of languages associated with the first keyboard in a keyboard package.
    // lexical-model packages will be 0
    final int languageCount = kmpProcessor.getLanguageCount(pkgInfo, PackageProcessor.PP_KEYBOARDS_KEY, 0);

    // Silent installation (skip displaying welcome.htm and user confirmation)
    if (silentInstall) {
      installPackage(context, pkgTarget, pkgId, languageList, true);
      return;
    }

    boolean isInstallingPackage = true;
    mStepperLayout = (StepperLayout) findViewById(R.id.stepperLayout);
    mStepperAdapter = new StepperAdapter(getSupportFragmentManager(), this,
      isInstallingPackage, tempPackagePath, pkgTarget, pkgId, pkgName, hasWelcome, languageID, languageCount);
    mStepperLayout.setAdapter(mStepperAdapter);
    mStepperLayout.setListener(this);
  }

  @Override
  protected void onActivityResult(int requestCode, int resultCode, Intent data) {
    super.onActivityResult(requestCode, resultCode, data);
    // Use the result of SelectLanguageActivity and install the package
    if (resultCode == 2 && data != null) {
      String pkgTarget = data.getStringExtra("pkgTarget");
      String pkgId = data.getStringExtra("packageID");
      ArrayList<String> languageList = (ArrayList)data.getSerializableExtra("languageList");
      installPackage(this, pkgTarget, pkgId, languageList, false);
    }
  }

  @Override
  public void onDestroy(){
    super.onDestroy();
    if ( alertDialog !=null && alertDialog.isShowing() ){
      alertDialog.dismiss();
    }
  }

  private void cleanup() {
    try {
      if (kmpFile != null && kmpFile.exists()) {
        kmpFile.delete();
      }
      if (tempPackagePath != null && tempPackagePath.exists()) {
        FileUtils.deleteDirectory(tempPackagePath);
      }
    } catch (Exception e) {
      KMLog.LogException(TAG, "cleanup() failed with error ", e);
    } finally {
      // Don't finish() because we still need to display welcome.htm
    }
  }

  @Override
  protected void onNewIntent(Intent intent) {
    super.onNewIntent(intent);
  }

  @Override
  public void onBackPressed() {
    finish();
    overridePendingTransition(0, android.R.anim.fade_out);
  }

  @Override
  public void onCompleted(View completedButton) {
    Toast.makeText(this, "onCompleted", Toast.LENGTH_SHORT).show();
  }

  @Override
  public void onError(VerificationError verificationError) {
    Toast.makeText(this, verificationError.getErrorMessage(), Toast.LENGTH_SHORT).show();
  }

  @Override
  public void onStepSelected(int newStepPosition) {


  }

  @Override
  public void onReturn() {
    finish();
  }

  @Override
  public void onAttachFragment(Fragment fragment) {
    if (fragment instanceof WebViewFragment) {
      WebViewFragment webViewFragment = (WebViewFragment) fragment;
      webViewFragment.setOnInstallClickedListener(this);
    } else if (fragment instanceof SelectLanguageFragment) {
      SelectLanguageFragment selectLanguageFragment = (SelectLanguageFragment) fragment;
      selectLanguageFragment.setOnLanguagesSelectedListener(this);
    }
  }

  @Override
  public void onInstallClicked(String pkgTarget, String packageID) {
    // Ignore language list and install the package
    installPackage(this, pkgTarget, packageID, null, false);
  }

  @Override
  public void onLanguagesSelected(String pkgTarget, String packageID, ArrayList<String>  languageList) {
    // Use the result of SelectLanguageActivity and install the package
    installPackage(this, pkgTarget, packageID, languageList, false);
  }

  @Override
  public void onLanguagesSelected(ArrayList<Keyboard> addKeyboardsList) {
  }

  private void showErrorToast(Context context, String message) {
    Toast.makeText(context, message, Toast.LENGTH_LONG).show();
    // Setting result to 1 so calling activity will finish too
    setResult(1);
    cleanup();
  }

  /**
   * Installs the keyboard or lexical model package, and then notifies the corresponding listeners
   * @param context Context   The activity context
   * @param pkgTarget String: PackageProcessor.PP_TARGET_KEYBOARDS or PP_TARGET_LEXICAL_MODELS
   * @param pkgId String      The Keyman package ID
   * @param preferredLanguages ArrayList<String>  The optional array of language ID's to use
   * @param anSilentInstall boolean If true, don't display readme.htm/welcome.htm content during installation
   */
  private void installPackage(Context context, String pkgTarget, String pkgId,
                              ArrayList<String> preferredLanguages, boolean anSilentInstall) {
    try {
      if (pkgTarget.equals(PackageProcessor.PP_TARGET_KEYBOARDS)) {
        // processKMP will remove currently installed package and install
        ArrayList<String> languageList;
        if (preferredLanguages != null && !preferredLanguages.isEmpty()) {
          languageList = preferredLanguages;
        } else {
          languageList = new ArrayList<String>();
        }

        //Dataset kmpDataset = new Dataset(context);
        //kmpDataset.keyboards.addAll(kbdsList);

        List<Map<String, String>> installedPackageKeyboards =
          kmpProcessor.processKMP(kmpFile, tempPackagePath, PackageProcessor.PP_KEYBOARDS_KEY, languageList);
        // Do the notifications!
        boolean success = installedPackageKeyboards.size() != 0;
        boolean _cleanup = true;
        if (success) {
          if(!anSilentInstall) {
            String keyboardName = installedPackageKeyboards.get(0).get(KMManager.KMKey_KeyboardName);
            Toast.makeText(context,
              String.format(context.getString(R.string.keyboard_install_toast), keyboardName),
              Toast.LENGTH_SHORT).show();
          }
          _cleanup = true;
          notifyPackageInstallListeners(KeyboardEventHandler.EventType.PACKAGE_INSTALLED,
            installedPackageKeyboards, 1);
          if (installedPackageKeyboards != null) {
            notifyPackageInstallListeners(KeyboardEventHandler.EventType.PACKAGE_INSTALLED,
              installedPackageKeyboards, 1);
          }
          if(_cleanup)
            cleanup();
        } else {
          showErrorDialog(context, pkgId, getString(R.string.no_new_touch_keyboards_to_install));
        }
      } else if (pkgTarget.equals(PackageProcessor.PP_TARGET_LEXICAL_MODELS)) {
        List<Map<String, String>> installedLexicalModels =
          kmpProcessor.processKMP(kmpFile, tempPackagePath, PackageProcessor.PP_LEXICAL_MODELS_KEY);
        // Do the notifications
        boolean success = installedLexicalModels.size() != 0;
        boolean _cleanup = true;
        if (success) {
          if(!anSilentInstall)
            Toast.makeText(context,
              context.getString(R.string.model_install_toast),
              Toast.LENGTH_SHORT).show();

            _cleanup = true;
          notifyLexicalModelInstallListeners(KeyboardEventHandler.EventType.LEXICAL_MODEL_INSTALLED,
            installedLexicalModels, 1);
          if (installedLexicalModels != null) {
            notifyLexicalModelInstallListeners(KeyboardEventHandler.EventType.LEXICAL_MODEL_INSTALLED,
              installedLexicalModels, 1);
          }
          if(_cleanup)
            cleanup();
        } else {
          showErrorDialog(context, pkgId, getString(R.string.no_new_predictive_text_to_install));
        }
      }
    } catch (Exception e) {
      KMLog.LogException(TAG, "", e);
      showErrorDialog(context, pkgId, getString(R.string.no_targets_to_install));
    }
  }

  private void showErrorDialog(Context context, String pkgId, String message) {
    AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(context);

    alertDialogBuilder.setTitle(String.format(getString(R.string.title_package_failed_to_install), pkgId));
    alertDialogBuilder
      .setMessage(message)
      .setCancelable(false)
      .setPositiveButton(getString(R.string.label_close),new DialogInterface.OnClickListener() {
        public void onClick(DialogInterface dialog,int id) {
          if (dialog != null) {
            dialog.dismiss();
          }
          cleanup();
        }
      });

    alertDialog = alertDialogBuilder.create();
    alertDialog.show();
  }

  void notifyPackageInstallListeners(KeyboardEventHandler.EventType eventType,
                                     List<Map<String, String>> keyboards, int result) {
    if (kbDownloadEventListeners != null) {
      KeyboardEventHandler.notifyListeners(kbDownloadEventListeners, eventType, keyboards, result);
    }
  }

  void notifyLexicalModelInstallListeners(KeyboardEventHandler.EventType eventType,
                                          List<Map<String, String>> models, int result) {
    if (kbDownloadEventListeners != null) {
      KeyboardEventHandler.notifyListeners(kbDownloadEventListeners, eventType, models, result);
    }
  }

  public static void addKeyboardDownloadEventListener(KeyboardEventHandler.OnKeyboardDownloadEventListener listener) {
    if (kbDownloadEventListeners == null) {
      kbDownloadEventListeners = new ArrayList<KeyboardEventHandler.OnKeyboardDownloadEventListener>();
    }

    if (listener != null && !kbDownloadEventListeners.contains(listener)) {
      kbDownloadEventListeners.add(listener);
    }
  }

  public static void removeKeyboardDownloadEventListener(KeyboardEventHandler.OnKeyboardDownloadEventListener listener) {
    if (kbDownloadEventListeners != null) {
      kbDownloadEventListeners.remove(listener);
    }
  }

}
