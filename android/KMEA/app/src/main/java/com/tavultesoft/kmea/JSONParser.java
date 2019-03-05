/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.InterruptedIOException;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.net.URL;

import org.json.JSONException;
import org.json.JSONObject;

import com.tavultesoft.kmea.util.Connection;

import android.util.Log;

public final class JSONParser {

  public JSONParser() {
  }

  private JSONObject getJSONObjectFromReader(BufferedReader reader) {
    String jsonStr = "";
    JSONObject jsonObj = null;
    String logTag = "JSONObjectFromReader";

    try {
      StringBuilder strBuilder = new StringBuilder();
      String line = null;
      while ((line = reader.readLine()) != null) {
        strBuilder.append(line + "\n");
      }
      jsonStr = strBuilder.toString();
      jsonObj = new JSONObject(jsonStr);
    } catch (UnsupportedEncodingException e) {
      Log.e(logTag, (e.getMessage() == null) ? "UnsupportedEncodingException" : e.getMessage());
      jsonObj = null;
      System.err.println(e);
    } catch (InterruptedIOException e) {
      // Disregard from cancelling action
      Log.e(logTag, (e.getMessage() == null)  ? "InterruptedIOException" : e.getMessage());
      jsonObj = null;
    } catch (IOException e) {
      Log.e(logTag, (e.getMessage() == null) ? "IOException" : e.getMessage());
      jsonObj = null;
      System.err.println(e);
    } catch (JSONException e) {
      Log.e(logTag, (e.getMessage() == null) ? "JSONException" : e.getMessage());
      jsonObj = null;
      System.err.println(e);
    } catch (Exception e) {
      Log.e(logTag, (e.getMessage() == null) ? "Exception" : e.getMessage());
      jsonObj = null;
      System.err.println(e);
    }

    return jsonObj;
  }

  public JSONObject getJSONObjectFromFile(File path) {
    BufferedReader reader = null;
    JSONObject jsonObj = null;

    try {
      reader = new BufferedReader(new FileReader(path));
      jsonObj = getJSONObjectFromReader(reader);
    } catch (FileNotFoundException e) {
      Log.e("JSONObjectFromFile", (e.getMessage() == null) ? "FileNotFoundException" : e.getMessage());
      jsonObj = null;
      System.err.println(e);
    } finally {
      if (reader != null) {
        try {
          reader.close();
        } catch (IOException e) {
          // Ignore.
        }
      }
    }

    return jsonObj;
  }

  // Doesn't work for directly-hosted files, hence the separate method above.
  public JSONObject getJSONObjectFromUrl(String urlStr) {
    BufferedReader reader = null;
    JSONObject jsonObj = null;
    InputStream inputStream = null;
    String logTag = "JSONObjectFromUrl";

    try {
      if (Connection.initialize(urlStr)) {
        inputStream = Connection.getInputStream();

        // get charset
        String charSet = null;
        String contentType = Connection.getContentType();
        String[] values = contentType.split(";");
        for (String value : values) {
          value = value.trim();
          if (value.toLowerCase().startsWith("charset=")) {
            charSet = value.substring("charset=".length());
          }
        }

        // if cannot get charset, use utf-8
        if (charSet == null)
          charSet = "utf-8";

        reader = new BufferedReader(new InputStreamReader(inputStream, charSet), 4096);
        jsonObj = getJSONObjectFromReader(reader);
      }
    } catch (UnsupportedEncodingException e) {
      Log.e(logTag, (e.getMessage() == null) ? "UnsupportedEncodingException" : e.getMessage());
      jsonObj = null;
      System.err.println(e);
    } catch (Exception e) {
      Log.e(logTag, (e.getMessage() == null) ? "Exception" : e.getMessage());
      jsonObj = null;
      System.err.println(e);
    } finally {
      Connection.disconnect();

      if(reader != null) {
        try {
          reader.close();
        } catch(IOException e) {
          // Ignore.
        }
      }
    }

    return jsonObj;
  }
}