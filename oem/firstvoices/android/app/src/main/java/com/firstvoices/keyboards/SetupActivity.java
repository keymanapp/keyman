package com.firstvoices.keyboards;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.graphics.Color;
import android.os.Bundle;
import android.provider.Settings;
import android.util.Log;
import android.view.View;
import android.view.Window;
import android.view.inputmethod.InputMethodInfo;
import android.view.inputmethod.InputMethodManager;
import android.widget.AdapterView;
import android.widget.ImageButton;
import android.widget.ListView;
import android.widget.SimpleAdapter;

import androidx.appcompat.app.AppCompatActivity;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class SetupActivity extends AppCompatActivity {

    private ListView listView = null;
    private SimpleAdapter listAdapter = null;
    private ArrayList<HashMap<String, String>> list = null;

    private final String iconKey = "icon";
    private final String textKey = "text";
    private final String isEnabledKey = "isEnabled";
    
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        supportRequestWindowFeature(Window.FEATURE_NO_TITLE);
        final Context context = this;
        setContentView(R.layout.setup_list_layout);

        listView = findViewById(R.id.listView);

        final ImageButton closeButton = findViewById(R.id.close_button);
        closeButton.setOnClickListener(new View.OnClickListener() {
            public void onClick(View v) {
                finish();
            }
        });

        list = new ArrayList<>();
        HashMap<String, String> hashMap;

        hashMap = new HashMap<>();
        hashMap.put(iconKey, "0");
        hashMap.put(textKey, "Enable 'FirstVoices'");
        hashMap.put(isEnabledKey, "true");
        list.add(hashMap);

        hashMap = new HashMap<>();
        hashMap.put(iconKey, "0");
        hashMap.put(textKey, "Choose 'FirstVoices' as current input method");
        hashMap.put(isEnabledKey, "false");
        list.add(hashMap);
        
        String[] from = new String[]{ iconKey, textKey };
        int[] to = new int[] { R.id.left_icon, R.id.text };
        listAdapter = new SimpleAdapter(context, list, R.layout.setup_row_layout, from, to);
        listView.setAdapter(listAdapter);
        listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                if (position == 0) {
                    startActivity(new Intent(Settings.ACTION_INPUT_METHOD_SETTINGS));
                }
                else if (position == 1) {
                    InputMethodManager imManager = (InputMethodManager) getSystemService(Context.INPUT_METHOD_SERVICE);
                    if(imManager != null) {
                        imManager.showInputMethodPicker();
                    } else {
                        Log.e("setupActivity.onCreate", "Failed to get system service INPUT_METHOD_SERVICE");
                    }
                }
            }
        });
    }
    
    @Override
    protected void onResume() {
        super.onResume();
    }
    
    @Override
    protected void onPause() {
        super.onPause();
    }
    
    @Override
    public void onWindowFocusChanged(boolean hasFocus) {
        super.onWindowFocusChanged(hasFocus);
        if (hasFocus) {
            String checkbox_off = String.valueOf(android.R.drawable.checkbox_off_background);
            String checkbox_on = String.valueOf(android.R.drawable.checkbox_on_background);

            if (isEnabledSystemWide(this)) {
                list.get(0).put(iconKey, checkbox_on);
                list.get(1).put(isEnabledKey, "true");
            }
            else {
                list.get(0).put(iconKey, checkbox_off);
                list.get(1).put(isEnabledKey, "false");
            }

            if (isDefaultKB(this))
                list.get(1).put(iconKey, checkbox_on);
            else
                list.get(1).put(iconKey, checkbox_off);

            String[] from = new String[]{ iconKey, textKey };
            int[] to = new int[] { R.id.left_icon, R.id.text };
            listAdapter = new SimpleAdapter(this, list, R.layout.setup_row_layout, from, to);
            listView.setAdapter(listAdapter);
        }
    }

    static boolean isEnabledSystemWide(Context context) {
        InputMethodManager imManager = (InputMethodManager) context.getSystemService(Context.INPUT_METHOD_SERVICE);
        if(imManager == null) {
            Log.e("isEnabledSystemWide", "Failed to get system service INPUT_METHOD_SERVICE");
            return false;
        }
        List<InputMethodInfo> imList = imManager.getEnabledInputMethodList();
        if(imList == null) {
            Log.e("isEnabledSystemWide", "Failed to get enabled input method list");
            return false;
        }

        final int size = imList.size();
        for(int i = 0; i < size; i++) {
            if (imList.get(i).getServiceName().equals("com.firstvoices.keyboards.SystemKeyboard")) {
                return true;
            }
        }

        return false;
    }

    static boolean isDefaultKB(Context context) {
        String inputMethod = Settings.Secure.getString(context.getContentResolver(), Settings.Secure.DEFAULT_INPUT_METHOD);
        return inputMethod.equals("com.firstvoices.keyboards/.SystemKeyboard");
    }
}
