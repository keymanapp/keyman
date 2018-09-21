package com.tavultesoft.kmea;

import android.support.v7.app.AppCompatActivity;
import android.support.v7.app.AlertDialog;
import android.app.Dialog;
import android.app.DialogFragment;
import android.content.DialogInterface;
import android.os.Bundle;
import android.widget.Toast;

/**
 * Confirmation dialog for downloading a Keyman keyboard
 */
public class ConfirmDialogFragment extends DialogFragment {
  public static String ARG_TITLE = "ConfirmDialogFragment.title";

  @Override
  public Dialog onCreateDialog(Bundle savedInstanceState) {
    Bundle args = getArguments();
    final String title = args.getString(ARG_TITLE);

    return new AlertDialog.Builder(getActivity())
      .setTitle(title)
      .setMessage("Would you like to download this keyboard?")
      .setPositiveButton("Download", new DialogInterface.OnClickListener() {
        @Override
        public void onClick(DialogInterface dialog, int which) {
          // Download keyboard
          if (KMManager.hasConnection(getActivity())) {
            KMKeyboardDownloaderActivity.download(getActivity(), true);
          } else {
            Toast.makeText(getActivity(), "No internet connection", Toast.LENGTH_SHORT).show();
          }
        }
      })
      .setNegativeButton("Cancel",  new DialogInterface.OnClickListener() {
       public void onClick(DialogInterface dialog, int which) {
         // Cancel
         dialog.dismiss();
         getActivity().finish();
       }
      })
      .create();
  }
}
