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

public final class KeyboardListActivity extends BaseActivity {

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        supportRequestWindowFeature(Window.FEATURE_NO_TITLE);
        final Context context = this;

        setContentView(R.layout.activity_list_layout);
        setupEdgeToEdge(R.id.activity_list_layout);

        final Toolbar toolbar = findViewById(R.id.list_toolbar);
        TextView title = findViewById(R.id.bar_title);
        title.setText(getIntent().getStringExtra("regionName"));

        setSupportActionBar(toolbar);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getSupportActionBar().setDisplayShowHomeEnabled(true);
        getSupportActionBar().setDisplayShowTitleEnabled(false);

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

    }

    @Override
    public boolean onSupportNavigateUp() {
        showRegionList();
        super.onBackPressed();
        return true;
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