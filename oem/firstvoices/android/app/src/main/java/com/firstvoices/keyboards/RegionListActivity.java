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

public final class RegionListActivity extends Activity {

	@Override
    public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		final Context context = this;
        final FVShared.FVRegionList regionList = FVShared.getInstance().getRegionList();

		// Setup layout

		requestWindowFeature(Window.FEATURE_CUSTOM_TITLE);
		setContentView(R.layout.list_layout);
		getWindow().setFeatureInt(Window.FEATURE_CUSTOM_TITLE, R.layout.region_title_layout);
        View titleDivider = getWindow().getDecorView().findViewById(getResources().getIdentifier("titleDivider", "id", "android"));
        titleDivider.setBackgroundColor(Color.rgb(170, 18, 37));

        FVRegionListAdapter listAdapter = new FVRegionListAdapter(context, regionList);
        listAdapter.listFont = Typeface.createFromAsset(getAssets(), "fonts/NotoSansCanadianAboriginal.ttf");
        final ListView listView = findViewById(R.id.listView);
        listView.setAdapter(listAdapter);

        listView.setChoiceMode(ListView.CHOICE_MODE_SINGLE);
        listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                Intent i = new Intent(context, KeyboardListActivity.class);
                i.addFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);
                i.putExtra("regionName", regionList.get(position).name);
                i.putExtra("regionIndex", position);

                // State to allow return to previous position
                int listPosition = listView.getFirstVisiblePosition();
                i.putExtra("listPosition", listPosition);
                View v = listView.getChildAt(0);
                int offsetY = (v == null) ? 0 : v.getTop();
                i.putExtra("offsetY", offsetY);
                startActivity(i);
            }
        });
        listView.setSelectionFromTop(getIntent().getIntExtra("listPosition", 0), getIntent().getIntExtra("offsetY", 0));

		final ImageButton backButton = findViewById(R.id.left_button);
		backButton.setOnClickListener(new View.OnClickListener() {
			public void onClick(View v) {
				finish();
			}
		});
	}
}