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

public final class KeyboardListActivity extends Activity {

	@Override
    public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		final Context context = this;

		requestWindowFeature(Window.FEATURE_CUSTOM_TITLE);
		setContentView(R.layout.list_layout);
		getWindow().setFeatureInt(Window.FEATURE_CUSTOM_TITLE, R.layout.keyboard_title_layout);
        View titleDivider = getWindow().getDecorView().findViewById(getResources().getIdentifier("titleDivider", "id", "android"));
        titleDivider.setBackgroundColor(Color.rgb(170, 18, 37));

        TextView title = findViewById(R.id.bar_title);
        title.setText(getIntent().getStringExtra("regionName"));

		ListView listView = findViewById(R.id.listView);

        int regionIndex = getIntent().getIntExtra("regionIndex", 0);

        FVShared.FVRegionList regionList = FVShared.getInstance().getRegionList();
        FVShared.FVRegion region = regionList.get(regionIndex);

        FVKeyboardListAdapter listAdapter = new FVKeyboardListAdapter(context, region);
        listAdapter.listFont = Typeface.createFromAsset(getAssets(), "fonts/NotoSansCanadianAboriginal.ttf");
        listView.setAdapter(listAdapter);
        listView.setChoiceMode(ListView.CHOICE_MODE_SINGLE);
        listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                //
            }
        });

		final ImageButton backButton = findViewById(R.id.left_button);
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
	    // Return to region list with scroll position and selection set as per history
        Intent i = new Intent(this, RegionListActivity.class);
        i.addFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);
        i.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        i.putExtra("listPosition", getIntent().getIntExtra("listPosition", 0));
        i.putExtra("offsetY", getIntent().getIntExtra("offsetY", 0));
        startActivity(i);
    }
}