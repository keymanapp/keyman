package com.firstvoices.keyboards;

import android.content.Context;
import android.content.Intent;
import android.graphics.Typeface;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
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

final class FVRegionListAdapter extends ArrayAdapter<FVShared.FVRegion> {

	protected Typeface listFont;

	private static class ViewHolder {
		TextView text1, text2;
	}

	public FVRegionListAdapter(Context context, FVShared.FVRegionList regions) {
		super(context, 0, regions);
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		ViewHolder holder;
		FVShared.FVRegion region = getItem(position);

		if(convertView == null) {
			convertView = LayoutInflater.from(getContext()).inflate(R.layout.region_row_layout, parent, false);
			holder = new ViewHolder();
			holder.text1 = (TextView) convertView.findViewById(R.id.text1);
			holder.text2 = (TextView) convertView.findViewById(R.id.text2);

			if (listFont != null) {
				holder.text1.setTypeface(listFont, Typeface.BOLD);
				holder.text2.setTypeface(listFont, Typeface.NORMAL);
			}
			convertView.setTag(holder);
		} else {
			holder = (ViewHolder) convertView.getTag();
		}

		holder.text1.setText(region.name);

		int kbCount = region.keyboards.size();
		int activeKbCount = FVShared.activeKeyboardCount(getContext(), region);

		holder.text2.setText(String.format("%d/%d", activeKbCount, kbCount));

		return convertView;
	}

	@Override
	public boolean areAllItemsEnabled() {
		return true;
	}

	@Override
	public boolean isEnabled(int position) {
		return true;
	}
}
