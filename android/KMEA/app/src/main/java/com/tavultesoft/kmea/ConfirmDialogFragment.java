package com.tavultesoft.kmea;

import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.app.AlertDialog;
import android.app.Dialog;
import android.app.DialogFragment;
import android.content.DialogInterface;
import android.os.Bundle;
import android.widget.Toast;

/**
 * Confirmation dialog for downloading or deleting a Keyman keyboard/model
 */
public class ConfirmDialogFragment extends DialogFragment {
  public static final String ARG_DIALOG_TYPE = "ConfirmDialogFragment.dialogType";
  public static final String ARG_TITLE = "ConfirmDialogFragment.title";
  public static final String ARG_MESSAGE = "ConfirmDialogFragment.message";
  public static final String ARG_KEYBOARD_KEY = "confirmDialogFragment.keyboardKey";
  private boolean dismissOnSelect = false;

  public enum DialogType {
    DIALOG_TYPE_DOWNLOAD_KEYBOARD,
    DIALOG_TYPE_DELETE_KEYBOARD
  }

  public static ConfirmDialogFragment newInstance(DialogType dialogType, String title, String message) {
    ConfirmDialogFragment frag = new ConfirmDialogFragment();
    Bundle args = new Bundle();
    args.putSerializable(ARG_DIALOG_TYPE, dialogType);
    args.putString(ARG_TITLE, title);
    args.putString(ARG_MESSAGE, message);
    frag.setArguments(args);
    return frag;
  }

  public static ConfirmDialogFragment newInstance(DialogType dialogType, String title, String message, String keyboardKey) {
    ConfirmDialogFragment frag = new ConfirmDialogFragment();
    Bundle args = new Bundle();
    args.putSerializable(ARG_DIALOG_TYPE, dialogType);
    args.putString(ARG_TITLE, title);
    args.putString(ARG_MESSAGE, message);
    args.putString(ARG_KEYBOARD_KEY, keyboardKey);
    frag.setArguments(args);
    return frag;
  }

  @Override
  public Dialog onCreateDialog(Bundle savedInstanceState) {
    Dialog dialog = super.onCreateDialog(savedInstanceState);

    final DialogType dialogType = (DialogType)getArguments().getSerializable(ARG_DIALOG_TYPE);
    final String title = getArguments().getString(ARG_TITLE);
    final String message = getArguments().getString(ARG_MESSAGE);
    final String keyboardKey = getArguments().getString(ARG_KEYBOARD_KEY);
    String positiveLabel = (keyboardKey == null) ? getString(R.string.label_download) : getString(R.string.label_delete);

    return new AlertDialog.Builder(getActivity())
      .setTitle(title)
      .setMessage(message)
      .setPositiveButton(positiveLabel, new DialogInterface.OnClickListener() {
        @Override
        public void onClick(DialogInterface dialog, int which) {
          switch (dialogType) {
            case DIALOG_TYPE_DOWNLOAD_KEYBOARD :
              // Confirmation to download keyboard
              if (KMManager.hasConnection(getActivity())) {
                KMKeyboardDownloaderActivity.download(getActivity(), true);
              } else {
                Toast.makeText(getActivity(), "No internet connection", Toast.LENGTH_SHORT).show();
              }
              break;
            case DIALOG_TYPE_DELETE_KEYBOARD :
              // Confirmation to delete item
              int keyboardIndex = KeyboardPickerActivity.getKeyboardIndex(getActivity(), keyboardKey);
              KeyboardPickerActivity.deleteKeyboard(getContext(), keyboardIndex);
              dismissOnSelect = true;
              break;
            default :
              break;
          }
          if (dialog != null) {
            dialog.dismiss();
          }

          if (dismissOnSelect && !getActivity().isFinishing()) {
            getActivity().finish();
          }
        }
      })
      .setNegativeButton(getString(R.string.label_cancel),  new DialogInterface.OnClickListener() {
       public void onClick(DialogInterface dialog, int which) {
         // Cancel
         if (dialog != null) {
           dialog.dismiss();
         }
         getActivity().finish();
       }
      })
      .create();
  }
}
