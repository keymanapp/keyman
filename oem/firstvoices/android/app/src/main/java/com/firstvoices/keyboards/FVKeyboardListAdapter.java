package com.firstvoices.keyboards;

import android.content.Context;
import android.content.Intent;
import android.graphics.Typeface;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.ImageButton;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.keyman.engine.KMManager;
import com.keyman.engine.data.KeyboardController;

class FVKeyboardListAdapter extends ArrayAdapter<FVShared.FVKeyboard> {

    Typeface listFont;

    private static class ViewHolder {
        LinearLayout linearLayout;
        ImageView check;
        TextView text1;
        ImageButton helpButton;
        ImageButton nextButton;
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
            holder.linearLayout = convertView.findViewById(R.id.linearLayout);
            holder.linearLayout.setOnClickListener(new FVKeyboardListAdapter.FVOnClickNextListener());
            holder.check = convertView.findViewById(R.id.image1);
            holder.text1 = convertView.findViewById(R.id.text1);
            holder.helpButton = convertView.findViewById(R.id.buttonHelp);
            holder.helpButton.setOnClickListener(new FVKeyboardListAdapter.FVOnClickHelpListener());
            holder.nextButton = convertView.findViewById(R.id.imageNext);
            holder.nextButton.setOnClickListener(new FVKeyboardListAdapter.FVOnClickNextListener());
            convertView.setTag(holder);
        } else {
            holder = (ViewHolder) convertView.getTag();
        }

        if(keyboard != null) {
            // Check if keyboard is installed
            if (KeyboardController.getInstance().keyboardExists(FVShared.FVDefault_PackageID, keyboard.id, null)) {
                holder.check.setVisibility(View.VISIBLE);
            }
            holder.linearLayout.setTag(keyboard);
            holder.text1.setText(keyboard.name);
            holder.helpButton.setTag(keyboard.id);
            holder.nextButton.setTag(keyboard);
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
            FVShared.getInstance().helpAction(getContext(), (String)v.getTag());
        }
    }

    private class FVOnClickNextListener implements View.OnClickListener {
        @Override
        public void onClick(View v) {
            FVShared.FVKeyboard keyboard = (FVShared.FVKeyboard) v.getTag();
            Intent intent = new Intent(getContext(), FVKeyboardSettingsActivity.class);
            Bundle args = new Bundle();
            args.putString(KMManager.KMKey_KeyboardID, keyboard.id);
            args.putString(KMManager.KMKey_KeyboardName, keyboard.name);
            args.putString(KMManager.KMKey_LanguageID, keyboard.lgId);
            args.putString(KMManager.KMKey_LanguageName, keyboard.lgName);
            args.putString(KMManager.KMKey_Version, keyboard.version);
            intent.putExtras(args);
            getContext().startActivity(intent);
            notifyDataSetChanged();
        }
    }

}
