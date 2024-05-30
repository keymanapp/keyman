/**
 * Copyright 2024 SIL International
 */
package com.tavultesoft.kmapro;

import static com.tavultesoft.kmapro.MainActivity.PERMISSION_REQUEST_STORAGE;

import android.Manifest;
import android.app.Activity;
import android.content.Context;
import android.content.pm.PackageManager;
import android.os.Build;
import android.os.Environment;
import android.widget.Toast;

import androidx.appcompat.app.AlertDialog;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;

public class CheckPermissions {

  public static boolean isPermissionOK(Activity activity) {
    boolean permissionsOK = true;

    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.M) {
      return permissionsOK;
    }

    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.R) {
      // < API 30
      permissionsOK = checkPermission(activity, Manifest.permission.WRITE_EXTERNAL_STORAGE);
    } else if (Build.VERSION.SDK_INT < Build.VERSION_CODES.TIRAMISU) {
      // API 30-32
      permissionsOK = Environment.isExternalStorageManager() ||
        checkPermission(activity, Manifest.permission.READ_EXTERNAL_STORAGE);
    } else {
      // API 33+
      //https://developer.android.com/about/versions/13/behavior-changes-13#granular-media-permissions
      // Manifest.permission.READ_MEDIA_AUDIO doesn't seem to be needed
      permissionsOK = permissionsOK && checkPermission(activity, Manifest.permission.READ_MEDIA_IMAGES);
      permissionsOK = permissionsOK && checkPermission(activity, Manifest.permission.READ_MEDIA_VIDEO);
    }

    return permissionsOK;
  }

  public static boolean checkPermission(Activity activity, String permission) {
    return ContextCompat.checkSelfPermission(activity.getApplicationContext(), permission) == PackageManager.PERMISSION_GRANTED;
  }

  public static void requestPermission (Activity activity, Context context) {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.UPSIDE_DOWN_CAKE) {
      // Need to request multiple permissions at once
      // TODO: request multiple permissions at once #10659
      showPermissionDenied(context);
    } else if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
      // TODO: find alternative to below. #10659
      // It works, but would need Play Store approval
      // For now, show permission denied
      showPermissionDenied(context);
      /*
      try {
        Intent intent = new Intent();
        intent.setAction(Settings.ACTION_MANAGE_APP_ALL_FILES_ACCESS_PERMISSION);
        Uri uri = Uri.fromParts("package", activity.getPackageName(), null);
        intent.setData(uri);
        activity.startActivity(intent);
      } catch (Exception e) {
        Intent intent = new Intent();
        intent.setAction(Settings.ACTION_MANAGE_ALL_FILES_ACCESS_PERMISSION);
        activity.startActivity(intent);
      }
       */
    } else if (ActivityCompat.shouldShowRequestPermissionRationale(activity, Manifest.permission.READ_EXTERNAL_STORAGE)) {
      // Provide additional rationale to the user if the permission was not granted
      String message = activity.getApplicationContext().getString(R.string.request_storage_permission);
      Toast.makeText(activity.getApplicationContext(), message ,
        Toast.LENGTH_LONG).show();
      ActivityCompat.requestPermissions(activity,
        new String[]{ Manifest.permission.READ_EXTERNAL_STORAGE },
        PERMISSION_REQUEST_STORAGE);
    } else {
      // Request the permission. The result will be received in onRequestPermissionsResult().
      ActivityCompat.requestPermissions(activity,
        new String[]{ Manifest.permission.READ_EXTERNAL_STORAGE },
        PERMISSION_REQUEST_STORAGE);
    }
  }

  public static void showPermissionDenied(Context context) {
    // Permission request denied
    AlertDialog.Builder dialogBuilder = new AlertDialog.Builder(context);
    dialogBuilder.setTitle(String.format(context.getString(R.string.title_add_keyboard)));
    dialogBuilder.setMessage(String.format(context.getString(R.string.storage_permission_denied2)));
    dialogBuilder.setPositiveButton(context.getString(R.string.label_ok), null);
    AlertDialog dialog = dialogBuilder.create();
    dialog.show();

  }
}
