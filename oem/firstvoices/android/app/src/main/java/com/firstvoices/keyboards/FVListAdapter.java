package com.firstvoices.keyboards;

import android.content.Context;
import android.content.Intent;
import android.graphics.Typeface;
import android.view.View;
import android.view.ViewGroup;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.ImageButton;
import android.widget.SimpleAdapter;
import android.widget.TextView;

import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.KeyboardInfoActivity;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

final class FVListAdapter extends SimpleAdapter {

	private Context context;
	private boolean[] enabled;
	protected Typeface listFont;
	protected int regionIndex = 0;

	public FVListAdapter(Context context, List<? extends Map<String, ?>> data, int resource, String[] from, int[] to) {
		super(context, data, resource, from, to);
		this.context = context;
		int length = data.size();
		enabled = new boolean[length];
		for(int i = 0; i < length; i++) {
			String value = data.get(i).get("isEnabled").toString();
            enabled[i] = value.equals("true");
		}
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View view = super.getView(position, convertView, parent);
		if (enabled[position])
			view.setAlpha(1.0f);
		else
			view.setAlpha(0.25f);

		TextView text1 = (TextView)view.findViewById(R.id.text1);
		TextView text2 = (TextView)view.findViewById(R.id.text2);
		if (listFont != null) {
			text1.setTypeface(listFont, Typeface.BOLD);
			if (text2 != null)
				text2.setTypeface(listFont, Typeface.NORMAL);
		}

		if (text2 != null) {
			int kbCount = FVShared.keyboardCount(context, position);
			int activeKbCount = FVShared.activeKeyboardCount(context, position);
			text2.setText(String.format("%d/%d", activeKbCount, kbCount));
		}

        ImageButton helpButton = (ImageButton)view.findViewById(R.id.buttonHelp);
        if (helpButton != null) {
            helpButton.setTag(String.valueOf(position));
            helpButton.setOnClickListener(new FVOnClickHelpListener());
        }

		CheckBox checkBox = (CheckBox)view.findViewById(R.id.checkBox1);
		if (checkBox != null) {
            checkBox.setTag(String.valueOf(position));
			checkBox.setChecked(FVShared.checkState(context, regionIndex, position));
			checkBox.setOnCheckedChangeListener(new FVOnCheckedChangeListener());
		}

		return view;
	}

	@Override
	public boolean areAllItemsEnabled() {
		return true;
	}

	@Override
	public boolean isEnabled(int position) {
		return enabled[position];
	}

    private class FVOnClickHelpListener implements ImageButton.OnClickListener {
        @Override
        public void onClick(View v) {
            FVShared.helpAction(context, regionIndex, Integer.parseInt((String)v.getTag()));
        }
    }

	private class FVOnCheckedChangeListener implements CheckBox.OnCheckedChangeListener {
		@Override
		public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
            FVShared.setCheckState(context, regionIndex, Integer.parseInt((String)buttonView.getTag()), isChecked);
		}
	}
}
