/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmapro;

import java.util.ArrayList;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;

public class NetworkStateReceiver extends BroadcastReceiver {

  private static ArrayList<OnNetworkStateChangeListener> networkStateChangeListeners = null;

  @Override
  public void onReceive(Context context, Intent intent) {
    if (hasConnection(context)) {
      notifyListeners(networkStateChangeListeners, true);
    } else {
      notifyListeners(networkStateChangeListeners, false);
    }
  }

  public static boolean hasConnection(Context context) {
    ConnectivityManager cm = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);

    NetworkInfo wifiNetwork = cm.getNetworkInfo(ConnectivityManager.TYPE_WIFI);
    if (wifiNetwork != null && wifiNetwork.isConnected()) {
      return true;
    }

    NetworkInfo mobileNetwork = cm.getNetworkInfo(ConnectivityManager.TYPE_MOBILE);
    if (mobileNetwork != null && mobileNetwork.isConnected()) {
      return true;
    }

    NetworkInfo activeNetwork = cm.getActiveNetworkInfo();
    if (activeNetwork != null && activeNetwork.isConnected()) {
      return true;
    }

    return false;
  }

  public static void addNetworkStateChangeListener(OnNetworkStateChangeListener listener) {
    if (networkStateChangeListeners == null)
      networkStateChangeListeners = new ArrayList<OnNetworkStateChangeListener>();

    if (listener != null && !networkStateChangeListeners.contains(listener)) {
      networkStateChangeListeners.add(listener);
    }
  }

  public static void removeNetworkStateChangeListener(OnNetworkStateChangeListener listener) {
    if (networkStateChangeListeners != null) {
      networkStateChangeListeners.remove(listener);
    }
  }

  public static void notifyListeners(ArrayList<OnNetworkStateChangeListener> listeners, boolean hasConnection) {
    if (listeners != null) {
      @SuppressWarnings("unchecked")
      // make a copy of the list to avoid concurrent modification while iterating
        ArrayList<OnNetworkStateChangeListener> _listeners = (ArrayList<OnNetworkStateChangeListener>) listeners.clone();
      for (OnNetworkStateChangeListener listener : _listeners)
        listener.onNetworkStateChanged(hasConnection);
    }
  }

  public static interface OnNetworkStateChangeListener {
    void onNetworkStateChanged(boolean hasConnection);
  }
}