package com.keyman.engine;

import androidx.appcompat.app.AlertDialog;
import android.app.Dialog;
import android.app.DialogFragment;
import android.content.DialogInterface;
import android.os.Bundle;
import android.widget.Toast;

import com.keyman.engine.cloud.CloudApiTypes;
import com.keyman.engine.data.KeyboardController;

import java.util.ArrayList;


/**
 * Confirmation dialog for downloading or deleting a Keyman keyboard/model
 */
public class ConfirmDialogFragment extends DialogFragment {
  private static final String ARG_DIALOG_TYPE = "ConfirmDialogFragment.dialogType";
  private static final String ARG_TITLE = "ConfirmDialogFragment.title";
  private static final String ARG_MESSAGE = "ConfirmDialogFragment.message";
  private static final String ARG_ITEM_KEY = "confirmDialogFragment.itemKey";
  private static final String ARG_DOWNLOAD_QUERIES_KEY = "confirmDialogFragment.downloadQueries";
  private static final String ARG_MODEL_ID_KEY = "confirmDialogFragment.modelId";
  private static final String ARG_LANG_ID_KEY = "confirmDialogFragment.langId";
  private static final String ARG_KB_ID_KEY = "confirmDialogFragment.kbId";
  private boolean dismissOnSelect = false;

  public enum DialogType {
    DIALOG_TYPE_DOWNLOAD_KEYBOARD,
    DIALOG_TYPE_DELETE_KEYBOARD,
    DIALOG_TYPE_DOWNLOAD_MODEL,
    DIALOG_TYPE_DELETE_MODEL
  }

  public static ConfirmDialogFragment newInstanceForKeyboard(DialogType dialogType, String title, String message,
                                                  String aLangId, String aKbId,
                                                  ArrayList<CloudApiTypes.CloudApiParam> aQueries) {
    ConfirmDialogFragment frag = new ConfirmDialogFragment();
    Bundle args = new Bundle();
    args.putSerializable(ARG_DIALOG_TYPE, dialogType);
    args.putString(ARG_TITLE, title);
    args.putString(ARG_MESSAGE, message);
    args.putString(ARG_LANG_ID_KEY, aLangId);
    args.putString(ARG_KB_ID_KEY, aKbId);
    args.putSerializable(ARG_DOWNLOAD_QUERIES_KEY,aQueries);
    frag.setArguments(args);
    return frag;
  }

  public static ConfirmDialogFragment newInstanceForLexicalModel(DialogType dialogType, String title, String message,
                                                             String aLangId, String aModelId,
                                                             ArrayList<CloudApiTypes.CloudApiParam> aQueries) {
    ConfirmDialogFragment frag = new ConfirmDialogFragment();
    Bundle args = new Bundle();
    args.putSerializable(ARG_DIALOG_TYPE, dialogType);
    args.putString(ARG_TITLE, title);
    args.putString(ARG_MESSAGE, message);
    args.putString(ARG_LANG_ID_KEY, aLangId);
    args.putString(ARG_MODEL_ID_KEY, aModelId);
    args.putSerializable(ARG_DOWNLOAD_QUERIES_KEY,aQueries);
    frag.setArguments(args);
    return frag;
  }

  public static ConfirmDialogFragment newInstanceForItemKeyBasedAction(DialogType dialogType, String title, String message, String itemKey) {
    ConfirmDialogFragment frag = new ConfirmDialogFragment();
    Bundle args = new Bundle();
    args.putSerializable(ARG_DIALOG_TYPE, dialogType);
    args.putString(ARG_TITLE, title);
    args.putString(ARG_MESSAGE, message);
    args.putString(ARG_ITEM_KEY, itemKey);
    frag.setArguments(args);
    return frag;
  }

  @Override
  public Dialog onCreateDialog(Bundle savedInstanceState) {
     super.onCreateDialog(savedInstanceState);

    final DialogType dialogType = (DialogType)getArguments().getSerializable(ARG_DIALOG_TYPE);
    final String title = getArguments().getString(ARG_TITLE);
    final String message = getArguments().getString(ARG_MESSAGE);
    final String itemKey = getArguments().getString(ARG_ITEM_KEY);
    final String _langId = getArguments().getString(ARG_LANG_ID_KEY);
    final String _kbId = getArguments().getString(ARG_KB_ID_KEY);
    final String _modelId = getArguments().getString(ARG_MODEL_ID_KEY);
    final ArrayList<CloudApiTypes.CloudApiParam> _preparedCloudApiParams =
      (ArrayList)getArguments().getSerializable(ARG_DOWNLOAD_QUERIES_KEY);
    String positiveLabel = (dialogType == DialogType.DIALOG_TYPE_DOWNLOAD_KEYBOARD ||
      dialogType == DialogType.DIALOG_TYPE_DOWNLOAD_MODEL) ?
      getString(R.string.label_download) : getString(R.string.label_delete);

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
                KMKeyboardDownloaderActivity.downloadKeyboard(
                  getActivity(), _langId, _kbId,_preparedCloudApiParams);
              } else {
                Toast.makeText(getActivity(), getActivity().getString(R.string.no_internet_connection), Toast.LENGTH_SHORT).show();
              }
              break;
            case DIALOG_TYPE_DELETE_KEYBOARD :
              // Confirmation to delete keyboard
              int keyboardIndex = KeyboardController.getInstance().getKeyboardIndex(itemKey);
              KeyboardPickerActivity.deleteKeyboard(getActivity(), keyboardIndex);
              dismissOnSelect = true;
              break;
            case DIALOG_TYPE_DOWNLOAD_MODEL :
              // Confirmation to download lexical model
              if (KMManager.hasConnection(getActivity())) {
                KMKeyboardDownloaderActivity.downloadLexicalModel(getActivity(),
                  _modelId, _preparedCloudApiParams);
              } else {
                Toast.makeText(getActivity(), getActivity().getString(R.string.no_internet_connection), Toast.LENGTH_SHORT).show();
              }
              break;
            case DIALOG_TYPE_DELETE_MODEL :
              // Confirmation to delete model
              int modelIndex = KeyboardPickerActivity.getLexicalModelIndex(getActivity(), itemKey);
              KeyboardPickerActivity.deleteLexicalModel(getActivity(), modelIndex, false);
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
         switch (dialogType) {
           case DIALOG_TYPE_DOWNLOAD_KEYBOARD :
             KMManager.getUpdateTool().cancelKeyboardUpdate(_langId,_kbId);
             break;
           case DIALOG_TYPE_DOWNLOAD_MODEL :
             KMManager.getUpdateTool().cancelLexicalModelUpdate(_langId,_modelId);
             break;
           default :
             break;
         }
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
