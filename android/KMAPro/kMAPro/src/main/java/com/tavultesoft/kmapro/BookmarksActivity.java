/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmapro;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;

import android.os.Bundle;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.BaseAdapter;
import android.widget.EditText;
import com.google.android.material.floatingactionbutton.FloatingActionButton;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.widget.PopupMenu;
import android.widget.SimpleAdapter;
import android.widget.TextView;
import android.widget.Toast;
import android.widget.AdapterView.OnItemLongClickListener;
import android.annotation.SuppressLint;
import androidx.appcompat.widget.Toolbar;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;

import com.keyman.engine.BaseActivity;
import com.keyman.engine.util.KMLog;

public class BookmarksActivity extends BaseActivity {
  private static final String TAG = "BookmarksActivity";
  private static ListView listView;
  private static ArrayList<HashMap<String, String>> list = null;
  private static String bookmarksFilename = "bookmarks.dat";
  private final String titleKey = "title";
  private final String urlKey = "url";
  private final String iconKey = "icon";
  private AlertDialog mDialog;

  @SuppressLint({"SetJavaScriptEnabled", "InflateParams"})
  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);

    final Context context = this;
    final String title = getIntent().getStringExtra(titleKey);
    final String url = getIntent().getStringExtra(urlKey);

    setContentView(R.layout.bookmarks_list_layout);

    final Toolbar toolbar = (Toolbar) findViewById(R.id.bookmarks_toolbar);
    setSupportActionBar(toolbar);
    getSupportActionBar().setLogo(null);
    getSupportActionBar().setDisplayShowHomeEnabled(false);
    getSupportActionBar().setDisplayShowTitleEnabled(false);
    getSupportActionBar().setDisplayShowCustomEnabled(true);
    getSupportActionBar().setBackgroundDrawable(MainActivity.getActionBarDrawable(this));

    listView = (ListView) findViewById(R.id.listView);
    list = getBookmarksList();
    String[] from = new String[]{titleKey, urlKey, iconKey};
    int[] to = new int[]{R.id.text1, R.id.text2, R.id.imageButton};
    final ListAdapter listAdapter = new SimpleAdapter(this, list, R.layout.bookmarks_row_layout, from, to);
    listView.setAdapter(listAdapter);
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
      @Override
      public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        TextView urlView = (TextView) view.findViewById(R.id.text2);
        Intent intent = new Intent();
        intent.putExtra("url", urlView.getText());
        setResult(RESULT_OK, intent);
        finish();
      }
    });

    listView.setOnItemLongClickListener(new OnItemLongClickListener() {
      @Override
      public boolean onItemLongClick(AdapterView<?> parent, View view, final int position, long id) {
        if (position >= 0) {
          PopupMenu popup = new PopupMenu(context, view);
          popup.getMenuInflater().inflate(R.menu.popup, popup.getMenu());
          popup.setOnMenuItemClickListener(new PopupMenu.OnMenuItemClickListener() {
            public boolean onMenuItemClick(MenuItem item) {
              if (item.getItemId() == R.id.popup_delete) {
                deleteBookmark(position);
                return true;
              } else {
                return false;
              }
            }
          });
          popup.show();
          return true;
        } else {
          return false;
        }
      }
    });

    TextView emptyText = (TextView) findViewById(android.R.id.empty);
    listView.setEmptyView(emptyText);

    FloatingActionButton  addButton = (FloatingActionButton) findViewById(R.id.addButton);
    addButton.setOnClickListener(new OnClickListener() {
      @Override
      public void onClick(View v) {
        AlertDialog.Builder dialogBuilder = new AlertDialog.Builder(context);
        dialogBuilder.setTitle(getString(R.string.add_bookmark));
        dialogBuilder.setView(getLayoutInflater().inflate(R.layout.add_bookmark_dialog_layout, null));
        dialogBuilder.setPositiveButton(getString(R.string.label_add), new DialogInterface.OnClickListener() {
          public void onClick(DialogInterface dialog, int which) {
            EditText titleField = (EditText) mDialog.findViewById(R.id.title);
            EditText urlField = (EditText) mDialog.findViewById(R.id.url);
            if (titleField.getText().toString().isEmpty()) {
              Toast.makeText(context, "Invalid title!", Toast.LENGTH_SHORT).show();
              return;
            } else if (urlField.getText().toString().isEmpty()) {
              Toast.makeText(context, "Invalid url!", Toast.LENGTH_SHORT).show();
              return;
            } else {
              String urlStr = urlField.getText().toString();
              try {
                new URL(urlStr);
              } catch (MalformedURLException e) {
                Toast.makeText(context, "Invalid url!", Toast.LENGTH_SHORT).show();
                return;
              }
            }

            HashMap<String, String> bookmark = new HashMap<String, String>();
            bookmark.put(titleKey, titleField.getText().toString());
            bookmark.put(urlKey, urlField.getText().toString());
            bookmark.put(iconKey, "");
            list.add(bookmark);
            if (saveBookmarksList()) {
              BaseAdapter adapter = (BaseAdapter) listAdapter;
              adapter.notifyDataSetChanged();
            } else {
              Toast.makeText(context, "Failed to save bookmark!", Toast.LENGTH_LONG).show();
            }

          }
        });

        dialogBuilder.setNegativeButton(getString(R.string.label_cancel), new DialogInterface.OnClickListener() {
          public void onClick(DialogInterface dialog, int which) {
            // Cancel
          }
        });

        mDialog = dialogBuilder.create();
        mDialog.show();
        EditText titleField = (EditText) mDialog.findViewById(R.id.title);
        titleField.setText(title);
        EditText urlField = (EditText) mDialog.findViewById(R.id.url);
        urlField.setText(url);
      }
    });
  }

  @Override
  public void onBackPressed() {
    super.onBackPressed();
    finish();
    overridePendingTransition(0, android.R.anim.fade_out);
  }

  @SuppressWarnings("unchecked")
  private ArrayList<HashMap<String, String>> getBookmarksList() {
    ArrayList<HashMap<String, String>> list = null;

    File file = new File(getDir("userdata", Context.MODE_PRIVATE), bookmarksFilename);
    if (file.exists()) {
      try {
        ObjectInputStream inputStream = new ObjectInputStream(new FileInputStream(file));
        list = (ArrayList<HashMap<String, String>>) inputStream.readObject();
        inputStream.close();
      } catch (Exception e) {
        KMLog.LogException(TAG, "Failed to read bookmarks list: ", e);
        list = null;
      }
    } else {
      list = new ArrayList<HashMap<String, String>>();
    }

    return list;
  }

  private boolean saveBookmarksList() {
    boolean result;
    try {
      File file = new File(getDir("userdata", Context.MODE_PRIVATE), bookmarksFilename);
      ObjectOutputStream outputStream = new ObjectOutputStream(new FileOutputStream(file));
      outputStream.writeObject(list);
      outputStream.flush();
      outputStream.close();
      result = true;
    } catch (Exception e) {
      KMLog.LogException(TAG, "Failed to save bookmarks list: ", e);
      result = false;
    }

    return result;
  }

  private void deleteBookmark(int position) {
    list.remove(position);
    if (saveBookmarksList()) {
      BaseAdapter adapter = (BaseAdapter) listView.getAdapter();
      adapter.notifyDataSetChanged();
      Toast.makeText(this, "Bookmark deleted", Toast.LENGTH_SHORT).show();
    }
  }
}