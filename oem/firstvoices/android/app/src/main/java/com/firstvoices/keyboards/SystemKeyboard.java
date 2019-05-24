package com.firstvoices.keyboards;

import android.content.Context;
import android.content.res.Configuration;
import android.graphics.Point;
import android.graphics.Typeface;
import android.inputmethodservice.InputMethodService;
import android.util.Log;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.ExtractedText;
import android.view.inputmethod.ExtractedTextRequest;
import android.view.inputmethod.InputConnection;
import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.KMManager.KeyboardType;
import com.tavultesoft.kmea.KeyboardEventHandler;

public class SystemKeyboard extends InputMethodService implements KeyboardEventHandler.OnKeyboardEventListener {

    private static View inputView = null;

    /** Main initialization of the input method component. Be sure to call
     * to super class. */
    @Override
    public void onCreate() {
        super.onCreate();
        KMManager.setDebugMode(false); // *** TO DO: Disable/delete before publishing a new release ***
        KMManager.addKeyboardEventListener(this);
        KMManager.setCanAddNewKeyboard(false);
        KMManager.setCanRemoveKeyboard(false);
        KMManager.setShouldCheckKeyboardUpdates(false);
        KMManager.setKeyboardPickerFont(Typeface.createFromAsset(getAssets(), "fonts/NotoSansCanadianAboriginal.ttf"));
        KMManager.initialize(getApplicationContext(), KeyboardType.KEYBOARD_TYPE_SYSTEM);
    }

    @Override
    public void onDestroy() {
        inputView = null;
        KMManager.removeKeyboardEventListener(this);
        KMManager.onDestroy();
        super.onDestroy();
    }

    /** This is the point where you can do all of your UI initialization. It
     * is called after creation and any configuration change. */
    @Override
    public void onInitializeInterface() {
        super.onInitializeInterface();
    }

    /** Called by the framework when your view for creating input needs to
     * be generated. This will be called the first time your input method
     * is displayed, and every time it needs to be re-created such as due to
     * a configuration change. */
    @Override
    public View onCreateInputView() {
        // create the inputView only once
        if (inputView == null)
            inputView = KMManager.createInputView(this);

        // we must remove the inputView from its previous parent before returning it
        ViewGroup parent = (ViewGroup) inputView.getParent();
        if (parent != null)
            parent.removeView(inputView);

        return inputView;
    }

    /** Deal with the editor reporting movement of its cursor. */
    @Override
    public void onUpdateSelection(int oldSelStart, int oldSelEnd, int newSelStart, int newSelEnd, int candidatesStart, int candidatesEnd) {
        super.onUpdateSelection(oldSelStart, oldSelEnd, newSelStart, newSelEnd, candidatesStart, candidatesEnd);
        // Selection range has changed, we should pass it to Keyman Engine
        KMManager.updateSelectionRange(KeyboardType.KEYBOARD_TYPE_SYSTEM, newSelStart, newSelEnd);
    }

    /** This is the main point where we do our initialization of the input method
     * to begin operating on an application.  At this point we have been
     * bound to the client, and are now receiving all of the detailed information
     * about the target of our edits. */
    @Override
    public void onStartInput(EditorInfo attribute, boolean restarting) {
        // Disable IME fullscreen mode
        attribute.imeOptions |= EditorInfo.IME_FLAG_NO_EXTRACT_UI | EditorInfo.IME_FLAG_NO_FULLSCREEN;
        super.onStartInput(attribute, restarting);
        KMManager.onStartInput(attribute, restarting);

        // User switched to a new input field so we should extract the text from input field
        // and pass it to Keyman Engine together with selection range
        InputConnection ic = getCurrentInputConnection();
        if (ic != null) {
            ExtractedText icText = ic.getExtractedText(new ExtractedTextRequest(), 0);
            if (icText != null) {
                KMManager.updateText(KeyboardType.KEYBOARD_TYPE_SYSTEM, icText.text.toString());
                int selStart = icText.startOffset + icText.selectionStart;
                int selEnd = icText.startOffset + icText.selectionEnd;
                KMManager.updateSelectionRange(KeyboardType.KEYBOARD_TYPE_SYSTEM, selStart, selEnd);
            }
        }

        if (KMManager.getCurrentKeyboardIndex(this) < 0)
            KMManager.setKeyboard(this, 0);
    }

    @Override
    public void onStartInputView(EditorInfo attribute, boolean restarting) {
        super.onStartInputView(attribute, restarting);
    }

    @Override
    public void onUpdateExtractingVisibility(EditorInfo ei) {
        super.onUpdateExtractingVisibility(ei);
    }

    @Override
    public void onConfigurationChanged(Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        KMManager.onConfigurationChanged(newConfig);
    }

    @Override
    public void onComputeInsets(Insets outInsets) {
        super.onComputeInsets(outInsets);

        // We should extend the touchable region so that Keyman sub keys menu can receive touch events outside the keyboard frame
        WindowManager wm = (WindowManager)getSystemService(Context.WINDOW_SERVICE);
        Point size = new Point(0, 0);
        wm.getDefaultDisplay().getSize(size);

        int inputViewHeight = 0;
        if (inputView != null)
            inputViewHeight = inputView.getHeight();

        int kbHeight = KMManager.getKeyboardHeight(this);
        outInsets.contentTopInsets = inputViewHeight - kbHeight;
        outInsets.touchableInsets = Insets.TOUCHABLE_INSETS_REGION;
        outInsets.touchableRegion.set(0, outInsets.contentTopInsets, size.x, size.y);
    }

    @Override
    public void onKeyboardLoaded(KeyboardType keyboardType) {
        // Handle Keyman keyboard loaded event here if needed
    }

    @Override
    public void onKeyboardChanged(String newKeyboard) {
        // Handle Keyman keyboard changed event here if needed
    }

    @Override
    public void onKeyboardShown() {
        // Handle Keyman keyboard shown event here if needed
    }

    @Override
    public void onKeyboardDismissed() {
        // Handle Keyman keyboard dismissed event here if needed
    }
}
