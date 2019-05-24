package com.firstvoices.keyboards;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.graphics.Color;
import android.graphics.Typeface;
import android.os.Bundle;
import android.view.View;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.ImageButton;
import android.widget.ListView;
import android.widget.TextView;

import java.util.ArrayList;
import java.util.HashMap;

public final class KeyboardListActivity extends Activity {

	private static ListView listView = null;
    private static FVListAdapter listAdapter = null;
    private static ArrayList<HashMap<String, String>> keyboardSubList = null;

	@Override
    public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		final Context context = this;
		requestWindowFeature(Window.FEATURE_CUSTOM_TITLE);
		setContentView(R.layout.list_layout);
		getWindow().setFeatureInt(Window.FEATURE_CUSTOM_TITLE, R.layout.keyboard_title_layout);
        View titleDivider = getWindow().getDecorView().findViewById(getResources().getIdentifier("titleDivider", "id", "android"));
        titleDivider.setBackgroundColor(Color.rgb(170, 18, 37));

        TextView title = (TextView)findViewById(R.id.bar_title);
        title.setText(getIntent().getStringExtra("regionName"));

		listView = (ListView)findViewById(R.id.listView);

        int regionIndex = getIntent().getIntExtra("regionIndex", 0);
        int index = regionIndex*2 + 1;

        ArrayList<ArrayList<HashMap<String, String>>> keyboardList = FVShared.getKeyboardList(context);
        if (keyboardList != null) {
            keyboardSubList = keyboardList.get(index);
            if (keyboardSubList != null) {
                keyboardSubList = (ArrayList<HashMap<String, String>>)keyboardSubList.clone();
                int len = keyboardSubList.size();
                for (int i = 0; i < len; i++) {
                    HashMap<String, String> kbDict = keyboardSubList.get(i);
                    kbDict.put("isEnabled", "true");
                }
            }
        }

        String[] from = new String[]{ FVShared.FVKeyboardNameKey };
        int[] to = new int[] { R.id.text1 };
        listAdapter = new FVListAdapter(context, keyboardSubList, R.layout.keyboard_row_layout, from, to);
        listAdapter.listFont = Typeface.createFromAsset(getAssets(), "fonts/NotoSansCanadianAboriginal.ttf");
        listAdapter.regionIndex = regionIndex;
        listView.setAdapter(listAdapter);
        listView.setChoiceMode(ListView.CHOICE_MODE_SINGLE);
        listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                //
            }
        });

		final ImageButton backButton = (ImageButton)findViewById(R.id.left_button);
		backButton.setOnClickListener(new View.OnClickListener() {
			public void onClick(View v) {
                showRegionList();
                finish();
			}
		});
	}

    @Override
    public void onBackPressed() {
        showRegionList();
        super.onBackPressed();
    }

    private void showRegionList() {
        Intent i = new Intent(this, RegionListActivity.class);
        i.addFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);
        i.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        i.putExtra("listPosition", getIntent().getIntExtra("listPosition", 0));
        i.putExtra("offsetY", getIntent().getIntExtra("offsetY", 0));
        startActivity(i);
    }
}