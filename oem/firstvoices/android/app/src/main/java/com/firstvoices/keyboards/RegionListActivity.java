package com.firstvoices.keyboards;

import android.content.Context;
import android.content.Intent;
import android.graphics.Typeface;
import android.os.Bundle;
import android.view.View;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.ListView;
import android.widget.TextView;

import androidx.appcompat.widget.Toolbar;
import com.keyman.engine.BaseActivity;

public final class RegionListActivity extends BaseActivity {

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        supportRequestWindowFeature(Window.FEATURE_NO_TITLE);
        final Context context = this;

        // Setup layout
        setContentView(R.layout.activity_list_layout);
        setupEdgeToEdge(R.id.activity_list_layout);

        final Toolbar toolbar = findViewById(R.id.list_toolbar);
        final TextView textView = findViewById(R.id.bar_title);
        textView.setText(R.string.regions);

        setSupportActionBar(toolbar);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getSupportActionBar().setDisplayShowHomeEnabled(true);
        getSupportActionBar().setDisplayShowTitleEnabled(false);

        final FVShared.FVRegionList regionList = FVShared.getInstance().getRegionList();
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
    }

    @Override
    public boolean onSupportNavigateUp() {
        super.onBackPressed();
        return true;
    }
}