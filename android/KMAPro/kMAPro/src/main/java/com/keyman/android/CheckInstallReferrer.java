/**
 * Copyright (C) 2021 SIL International. All rights reserved.
 *
 * Uses Google Play Install Referrer API to check for default keyboard to install alongside the app
 * itself. Details are passed in the `referrer` query value in the Play Store url, e.g.
 *
 *   https://play.google.com/store/apps/details?id=com.tavultesoft.kmapro&
 *   referrer=source%3Dkeyman%26package%3Dgff_amharic%26bcp47%3Dam
 *
 * The referrer value in the example unencodes to:
 *   source=keyman&package=gff_amharic&bcp47=am
 *
 * Parameters:
 *   - `source` must be 'keyman'
 *   - `package` must be a package id from Keyman Cloud
 *   - `bcp47` is optional but if present must be a valid BCP 47 code
 */

package com.keyman.android;

import android.content.Context;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Build;
import android.os.RemoteException;
import android.util.Log;
import java.lang.IllegalArgumentException;

import com.android.installreferrer.api.InstallReferrerClient;
import com.android.installreferrer.api.InstallReferrerStateListener;
import com.android.installreferrer.api.ReferrerDetails;
import com.tavultesoft.kmapro.MainActivity;
import com.tavultesoft.kmapro.R;
import com.keyman.engine.KmpInstallMode;
import com.keyman.engine.util.KMLog;
import com.keyman.engine.util.KMString;
import com.keyman.engine.util.VersionUtils;

public class CheckInstallReferrer {
  private static final String TAG = "CheckInstallReferrer";
  private static final String hasGooglePlayInstallReferrerBeenCheckedKey =
    "HasGooglePlayInstallReferrerBeenChecked";
  private static final String GOOGLE_PLAY = "com.android.vending";

  /**
   * Contact Google Play Install Referrer API to find out if the Keyman site gave us a default
   * keyboard to install.
   * Limitation: Only contact API if Keyman was installed from Google Play store
   * @param mainActivity  TODO: refactor downloadKMP so we don't need backrefs like this
   * @param context
   */
  public static void checkGooglePlayInstallReferrer(MainActivity mainActivity, Context context) {
    InstallReferrerClient referrerClient;

    // We check the referrer value only once after installing the app,
    // even if it fails.
    SharedPreferences prefs = context.getSharedPreferences(
      context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    boolean hasGooglePlayInstallReferrerBeenChecked = prefs.getBoolean(
      hasGooglePlayInstallReferrerBeenCheckedKey, false);
    if(hasGooglePlayInstallReferrerBeenChecked) return;

    SharedPreferences.Editor editor = prefs.edit();
    editor.putBoolean(hasGooglePlayInstallReferrerBeenCheckedKey, true);
    editor.commit();

    // local environment or test builds are nearly always side loaded
    if (VersionUtils.isLocalBuild() || VersionUtils.isTestBuild()) {
      return;
    }

    // Determine if Keyman installed from Google Play store
    String installerInfo = null;
    try {
      PackageManager packageManager = context.getPackageManager();
      String packageName = context.getPackageName();
      if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
        installerInfo = packageManager.getInstallSourceInfo(packageName).getInstallingPackageName();
      } else {
        installerInfo = packageManager.getInstallerPackageName(packageName);
      }
    } catch (PackageManager.NameNotFoundException|IllegalArgumentException e) {
      KMLog.LogException(TAG, "Error determining packageManager", e);
      return;
    }

    if (installerInfo == null) {
      // Side loaded so just return
      return;
    }
    
    if (!installerInfo.equalsIgnoreCase(GOOGLE_PLAY)) {
      // Don't log install referrer #10762
      return;
    }

    Log.i(TAG, "Started checkGooglePlayInstallReferrer");

    // Connect to Google Play Install Referrer API
    referrerClient = InstallReferrerClient.newBuilder(context).build();
    referrerClient.startConnection(new InstallReferrerStateListener() {
      @Override
      public void onInstallReferrerSetupFinished(int responseCode) {
        switch (responseCode) {
          case InstallReferrerClient.InstallReferrerResponse.OK:
            // Connection established.
            Log.i(TAG, "onInstallReferrerSetupFinished:OK");
            processGooglePlayInstallReferrerData();
            break;
          case InstallReferrerClient.InstallReferrerResponse.FEATURE_NOT_SUPPORTED:
            // API not available on the current Play Store app; log to Sentry
            KMLog.LogError(TAG, "onInstallReferrerSetupFinished:FEATURE_NOT_SUPPORTED");
            break;
          case InstallReferrerClient.InstallReferrerResponse.SERVICE_UNAVAILABLE:
            // Connection couldn't be established; log to Sentry
            KMLog.LogError(TAG, "onInstallReferrerSetupFinished:SERVICE_UNAVAILABLE");
            break;
          case InstallReferrerClient.InstallReferrerResponse.PERMISSION_ERROR:
            KMLog.LogError(TAG, "onInstallReferrerSetupFinished.PERMISSION_ERROR");
            break;
          default:
            // There are some other error codes; log to Sentry
            KMLog.LogError(TAG, "onInstallReferrerSetupFinished: Unexpected code: " +
              responseCode);
            break;
        }
        // We have finished with the API so tidy up
        referrerClient.endConnection();
      }

      @Override
      public void onInstallReferrerServiceDisconnected() {
        // Try to restart the connection on the next request to
        // Google Play by calling the startConnection() method.

        // Note: the documentation on this is unclear, and examples I found typically have no
        // implementation here, so for now, we'll just log this to Sentry so that we can be aware
        // of it happening.
        KMLog.LogError(TAG, "onInstallReferrerServiceDisconnected");
      }

      private void processGooglePlayInstallReferrerData() {
        ReferrerDetails response;
        try {
          response = referrerClient.getInstallReferrer();
        } catch (RemoteException e) {
          KMLog.LogException(TAG, "Failed to get install referrer", e);
          return;
        }

        String referrerUrl = response.getInstallReferrer();
        if(referrerUrl == null) {
          KMLog.LogError(TAG, "getInstallReferrer() returned null");
          return;
        }

        Log.i(TAG, "Referrer URL: "+referrerUrl);
        installPackageFromInstallReferrer(mainActivity, referrerUrl);
      }
    });
  }

  private static void installPackageFromInstallReferrer(MainActivity mainActivity,
                                                        String urlReferrer) {
    // We're going to try to and parse the string; if it looks like a valid referrer then we'll
    // try and install the package. A valid referrer looks like:
    //   source=keyman&package=<package>[&bcp47=<code>]
    // Note that this will be encoded into a referrer url like this (i.e. = and & must be
    // double-encoded to fit into the referrer parameter):
    //   https://play.google.com/store/apps/details?id=com.tavultesoft.kmapro&
    //   referrer=source%3Dkeyman%26package%3D<package>%26bcp47%3D<code>
    // so we can use a standard URI parser to extract the parameters
    Uri referrerUri = Uri.parse("https://example?" + urlReferrer);
    String source = referrerUri.getQueryParameter("source");
    String packageId = referrerUri.getQueryParameter("package");
    String bcp47 = referrerUri.getQueryParameter("bcp47");

    KMLog.LogInfo(TAG, KMString.format("Install referrer details from Google Play: %s source=%s package=%s bcp47=%s",
      new Object[]{urlReferrer, source, packageId, bcp47}));

    // We use the 'source' parameter as a basic sanity check as anything could be passed in referrer
    if(source == null || !source.equals("keyman")) return;
    if(packageId == null) return;

    mainActivity.downloadKMP(packageId, bcp47, KmpInstallMode.WelcomeOnly);
  }
}
