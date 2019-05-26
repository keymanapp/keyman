package com.firstvoices.keyboards;

import android.content.Context;
import android.graphics.Typeface;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.ImageButton;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

class FVKeyboardListAdapter extends ArrayAdapter<FVShared.FVKeyboard> {

    Typeface listFont;

    private static class ViewHolder {
        TextView text1;
        ImageButton helpButton;
        CheckBox checkBox;
    }

    FVKeyboardListAdapter(Context context, FVShared.FVRegion regionData) {
        super(context, 0, regionData.keyboards);
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        ViewHolder holder;

        FVShared.FVKeyboard keyboard = getItem(position);

        if(convertView == null) {
            convertView = LayoutInflater.from(getContext()).inflate(R.layout.keyboard_row_layout, parent, false);
            holder = new ViewHolder();
            holder.text1 = convertView.findViewById(R.id.text1);
            holder.checkBox = convertView.findViewById(R.id.checkBox1);
            holder.helpButton = convertView.findViewById(R.id.buttonHelp);
            holder.helpButton.setOnClickListener(new FVKeyboardListAdapter.FVOnClickHelpListener());
            holder.checkBox.setOnCheckedChangeListener(new FVKeyboardListAdapter.FVOnCheckedChangeListener());
            convertView.setTag(holder);

            if (listFont != null) {
                holder.text1.setTypeface(listFont, Typeface.BOLD);
            }
        } else {
            holder = (ViewHolder) convertView.getTag();
        }

        if(keyboard != null) {
            holder.text1.setText(keyboard.name);
            holder.helpButton.setTag(keyboard.id);
            holder.checkBox.setTag(keyboard.id);
            holder.checkBox.setChecked(FVShared.getInstance().checkState(keyboard.id));
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

    private class FVOnClickHelpListener implements ImageButton.OnClickListener {
        @Override
        public void onClick(View v) {
            FVShared.getInstance().helpAction((String)v.getTag());
        }
    }

    private class FVOnCheckedChangeListener implements CheckBox.OnCheckedChangeListener {
        @Override
        public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
            FVShared.getInstance().setCheckState((String)buttonView.getTag(), isChecked);
        }
    }
}
