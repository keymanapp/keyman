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
import java.util.ArrayList;
import java.util.HashMap;

public final class RegionListActivity extends Activity {

	private static ListView listView = null;
    private static FVListAdapter listAdapter = null;
    private static ArrayList<HashMap<String, String>> regionList = null;

	@Override
    public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		final Context context = this;
		requestWindowFeature(Window.FEATURE_CUSTOM_TITLE);
		setContentView(R.layout.list_layout);
		getWindow().setFeatureInt(Window.FEATURE_CUSTOM_TITLE, R.layout.region_title_layout);
        View titleDivider = getWindow().getDecorView().findViewById(getResources().getIdentifier("titleDivider", "id", "android"));
        titleDivider.setBackgroundColor(Color.rgb(170, 18, 37));
		listView = (ListView)findViewById(R.id.listView);

        ArrayList<ArrayList<HashMap<String, String>>> keyboardList = FVShared.getKeyboardList(context);
        regionList = new ArrayList<HashMap<String, String>>();
        if (keyboardList != null) {
            int len = keyboardList.size();
            for (int i = 0; i < len; i+=2) {
                HashMap<String, String> regionDict = (HashMap<String, String>)keyboardList.get(i).get(0).clone();
                regionDict.put("isEnabled", "true");
                regionList.add(regionDict);
            }
        }

        String[] from = new String[]{ FVShared.FVRegionNameKey };
        int[] to = new int[] { R.id.text1 };
        listAdapter = new FVListAdapter(context, regionList, R.layout.region_row_layout, from, to);
        listAdapter.listFont = Typeface.createFromAsset(getAssets(), "fonts/NotoSansCanadianAboriginal.ttf");
        listView.setAdapter(listAdapter);
        listView.setChoiceMode(ListView.CHOICE_MODE_SINGLE);
        listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                Intent i = new Intent(context, KeyboardListActivity.class);
                i.addFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);
                i.putExtra("regionName", regionList.get(position).get(FVShared.FVRegionNameKey));
                i.putExtra("regionIndex", position);
                int listPosition = listView.getFirstVisiblePosition();
                i.putExtra("listPosition", listPosition);
                View v = listView.getChildAt(0);
                int offsetY = (v == null) ? 0 : v.getTop();
                i.putExtra("offsetY", offsetY);
                startActivity(i);
            }
        });
        listView.setSelectionFromTop(getIntent().getIntExtra("listPosition", 0), getIntent().getIntExtra("offsetY", 0));

		final ImageButton backButton = (ImageButton)findViewById(R.id.left_button);
		backButton.setOnClickListener(new View.OnClickListener() {
			public void onClick(View v) {
				finish();
			}
		});
	}
}