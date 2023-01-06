/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.keyman.engine;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.InterruptedIOException;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import com.keyman.engine.util.Connection;
import com.keyman.engine.util.KMLog;

public final class JSONParser {
  private final String TAG = "JSONParser";

  public JSONParser() {
  }

  public <T extends Object> T getJSONObjectFromReader(BufferedReader reader, Class<T> type) {
    String jsonStr = "";
    T obj = null;
    String logTag = "JSONObjectFromReader";

    try {
      StringBuilder strBuilder = new StringBuilder();
      String line = null;
      while ((line = reader.readLine()) != null) {
        strBuilder.append(line + "\n");
      }
      jsonStr = strBuilder.toString();
      if (type == JSONObject.class) {
        String EMPTY_JSONARRAY_FORMATSTR = "^\\[\\](\\n)*$";
        Pattern pattern = Pattern.compile(EMPTY_JSONARRAY_FORMATSTR);
        Matcher matcher = pattern.matcher(jsonStr);
        if (matcher.matches()) {
          // Treat empty JSONArray as empty JSON Object. Issue #7564
          jsonStr = "{}";
        }
        obj = (T) new JSONObject(jsonStr);
      } else if (type == JSONArray.class) {
        obj = (T) new JSONArray(jsonStr);
      }
    } catch (UnsupportedEncodingException e) {
      KMLog.LogException(logTag, "", e);
      obj = null;
      System.err.println(e);
    } catch (InterruptedIOException e) {
      // Disregard from cancelling action
      KMLog.LogException(logTag, "", e);
      obj = null;
    } catch (IOException e) {
      KMLog.LogException(logTag, "", e);
      obj = null;
      System.err.println(e);
    } catch (JSONException e) {
      KMLog.LogException(logTag, "", e);
      obj = null;
      System.err.println(e);
    } catch (Exception e) {
      KMLog.LogException(logTag, "", e);
      obj = null;
      System.err.println(e);
    }

    return obj;
  }

  public JSONObject getJSONObjectFromReader(BufferedReader reader) {
    return getJSONObjectFromReader(reader, JSONObject.class);
  }

  public JSONObject getJSONObjectFromFile(File path) {
    return getJSONObjectFromFile(path,JSONObject.class);
  }

  public <T extends Object> T getJSONObjectFromFile(File path, Class<T> type) {
    BufferedReader reader = null;
    T jsonObj = null;

    try {
      reader = new BufferedReader(new FileReader(path));
      jsonObj = getJSONObjectFromReader(reader, type);
    } catch (FileNotFoundException e) {
      String errorString = (e.getMessage() == null) ? "FileNotFoundException" : e.getMessage();
      KMLog.LogException(TAG, "JSONObjectFromFile error ", e);
      jsonObj = null;
      System.err.println(e);
    } finally {
      if (reader != null) {
        try {
          reader.close();
        } catch (IOException e) {
          KMLog.LogException(TAG, "getJSONObjectFromFile error: ", e);
          // Ignore.
        }
      }
    }

    return jsonObj;
  }

  //

  /**
   * Download a JSONObject or JSONArray from a URL.
   * Doesn't work for directly-hosted files, hence the separate method above.
   * @param urlStr String URL of the endpoint
   * @param type Class<T> - JSONObject.class or JSONArray.class
   * @return JSONObject or JSONArray that matches type
   */
  public <T extends Object> T getJSONObjectFromUrl(String urlStr, Class<T> type) {
    BufferedReader reader = null;
    T obj = null;
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
        obj = (T) getJSONObjectFromReader(reader, type);
      }
    } catch (UnsupportedEncodingException e) {
      KMLog.LogException(logTag, "", e);
      obj = null;
      System.err.println(e);
    } catch (Exception e) {
      KMLog.LogException(logTag, "", e);
      obj = null;
      System.err.println(e);
    } finally {
      Connection.disconnect();

      if (reader != null) {
        try {
          reader.close();
        } catch (IOException e) {
          KMLog.LogException(logTag, "", e);
          // Ignore.
        }
      }
    }

    return obj;
  }

  public JSONObject getJSONObjectFromUrl(String urlStr) {
    return getJSONObjectFromUrl(urlStr, JSONObject.class);
  }

  public JSONObject getJSONObjectFromString(String str) {
    return getJSONObjectFromReader(new BufferedReader(new StringReader(str)));
  }

  public JSONObject getJSONObjectFromURIString(String str) {
    try {
      return getJSONObjectFromString(java.net.URLDecoder.decode(str, "UTF-8"));
    } catch (UnsupportedEncodingException e) {
      KMLog.LogException(TAG, "", e);
      return null;
    }
  }
}