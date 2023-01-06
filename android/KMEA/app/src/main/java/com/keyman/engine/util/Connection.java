package com.keyman.engine.util;

import java.io.InputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;

public final class Connection {
  private static final String TAG = "Connection";
  private static final int MAX_REDIRECTS = 5;
  private static final int TIMEOUT = 20 * 1000; // seconds

  private static HttpURLConnection urlConnection;

  // Latest URL connection
  private static URL url;

  // Boolean if a valid connection to the URL has been established and suitable to get the input stream
  private static boolean urlValid;

  /**
   * A method that returns the input stream of a URL connection
   * @return InputStream
   * @throws IOException
   */
  public static InputStream getInputStream() {
    try {
      if (urlValid && urlConnection != null) {
        return urlConnection.getInputStream();
      }
    } catch (IOException e) {
      KMLog.LogException(TAG, "getInputStream failed: ", e);
    }
    return urlConnection.getErrorStream();
  };

  public static String getContentType() {
    String contentType = "";
    if (urlConnection != null && urlValid) {
      contentType = urlConnection.getContentType();
    }
    return contentType;
  }

  public static String getFile() {
    String filename = "";
    if (url != null && urlValid) {
      filename = url.getFile();
    }
    return filename;
  };

  public static void disconnect() {
    if (urlConnection != null) {
      urlConnection.disconnect();
    }
    urlValid = false;
  };

  /**
   * Initialize the connection and follow up to 5 redirects.
   * @param originalUrl
   * String for original URL connection. Because of redirects, this may not be the final URL
   * that Connection establishes.
   * @return boolean if the connection was successful
   * @throws Exception
   */
  public static boolean initialize(String originalUrl) {
    boolean ret = false;
    String urlStr = originalUrl;
    urlValid = false;
    try {

      HttpURLConnection.setFollowRedirects(false);
      int attempt = 1;

      while (attempt <= MAX_REDIRECTS && !urlValid) {
        url = new URL(urlStr);
        urlConnection = (HttpURLConnection) url.openConnection();
        urlConnection.setRequestProperty("Cache-Control", "no-cache");
        urlConnection.setConnectTimeout(TIMEOUT);
        urlConnection.setReadTimeout(TIMEOUT);
        urlConnection.connect();
        int status = urlConnection.getResponseCode();

        if (status == HttpURLConnection.HTTP_OK) {
          urlValid = true;
          ret = true;
        } else {
          // Handle HTTP Status Codes 3xx
          if (HttpURLConnection.HTTP_MULT_CHOICE <= status &&
              status <= HttpURLConnection.HTTP_USE_PROXY &&
              status != HttpURLConnection.HTTP_NOT_MODIFIED) {
            urlStr = urlConnection.getHeaderField("Location");
          } else {
            // Abort for all other Status Codes
            break;
          }
          attempt++;
        }
      }
    } catch (Exception e) {
      KMLog.LogException(TAG, "Initialization failed:", e);
    }

    return ret;
  }

};
