package com.firstvoices.keyboards;

import android.content.Context;
import android.graphics.Typeface;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import java.util.Locale;

final class FVRegionListAdapter extends ArrayAdapter<FVShared.FVRegion> {

	Typeface listFont;

	private static class ViewHolder {
		TextView text1, text2;
	}

	FVRegionListAdapter(Context context, FVShared.FVRegionList regions) {
		super(context, 0, regions);
	}

	@NonNull
	@Override
	public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
		ViewHolder holder;
		FVShared.FVRegion region = getItem(position);

		if(convertView == null) {
			convertView = LayoutInflater.from(getContext()).inflate(R.layout.region_row_layout, parent, false);
			holder = new ViewHolder();
			holder.text1 = convertView.findViewById(R.id.text1);
			holder.text2 = convertView.findViewById(R.id.text2);

			if (listFont != null) {
				holder.text1.setTypeface(listFont, Typeface.BOLD);
				holder.text2.setTypeface(listFont, Typeface.NORMAL);
			}
			convertView.setTag(holder);
		} else {
			holder = (ViewHolder) convertView.getTag();
		}

		if(region != null) {

			holder.text1.setText(region.name);

			int kbCount = region.keyboards.size();
			int activeKbCount = FVShared.getInstance().activeKeyboardCount(region);

			holder.text2.setText(String.format(Locale.US, "%d/%d", activeKbCount, kbCount));
		}

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
