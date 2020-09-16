package com.tavultesoft.kmea.util;

import android.content.ClipData;
import android.content.ClipDescription;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.widget.Toast;

import androidx.core.content.FileProvider;

import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.util.FileProviderUtils;
import com.tavultesoft.kmea.util.FileUtils;

import java.io.File;
import java.io.FileFilter;

public final class HelpFile {
  private static final String TAG = "HelpFile";
  private static final String[] ASSET_MIME_TYPES =  {
    ClipDescription.MIMETYPE_TEXT_HTML,
    "text/css",
    "image/gif",
    "image/jpeg",
    "image/png"};

  /**
   * Utility to pass a help file and all associated assets to an Intent for Intent.ACTION_VIEW
   * @param context
   * @param helpFile Full path string of the html file to view
   * @param packageID String of the package ID
   * @return Intent
   */
  public static Intent toActionView(Context context, String helpFile, String packageID) {
    Intent i = new Intent(Intent.ACTION_VIEW);

    if (FileUtils.isWelcomeFile(helpFile) && ! KMManager.isTestMode()) {
      File customHelp = new File(new File(helpFile).getAbsolutePath());
      i.setFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION);
      // Starting with Android N, you can't pass file:// to intents, so we use FileProvider
      try {
        final String authority = FileProviderUtils.getAuthority(context);
        Uri contentUri = FileProvider.getUriForFile(
          context, authority, customHelp);
        i.setDataAndType(contentUri, "text/html");

        // Grant read permission to all the files in the package so embedded assets can be viewed
        ClipData clipData = new ClipData(null, ASSET_MIME_TYPES, new ClipData.Item(contentUri));

        // Exclude html help files and JS files. Treat rest of the files as assets
        FileFilter _fileFilter = new FileFilter() {
          @Override
          public boolean accept(File pathname) {
            String name = pathname.getName();
            if (pathname.isFile() && (FileUtils.isReadmeFile(name) ||
              FileUtils.isWelcomeFile(name) || FileUtils.hasJavaScriptExtension(name))) {
              return false;
            }
            return true;
          }
        };

        String base  = helpFile.contains("packages") ? "packages" : "models";
        File packageDir = new File(
          context.getDir("data", Context.MODE_PRIVATE), base + File.separator + packageID + File.separator);
        File[] files = packageDir.listFiles(_fileFilter);
        for(File assetFile : files) {
          Uri assetUri = FileProvider.getUriForFile(
            context, authority, assetFile);
          clipData.addItem(new ClipData.Item(assetUri));
        }

        // Associate assets in clipData to the intent
        i.setClipData(clipData);
      } catch (NullPointerException e) {
        String message = "FileProvider undefined in app to load" + customHelp.toString();
        Toast.makeText(context, message, Toast.LENGTH_LONG).show();
        KMLog.LogException(TAG, message, e);
      }
    } else {
      i.setData(Uri.parse(helpFile));
    }
    return i;
  }

}
