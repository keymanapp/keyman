/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmapro;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import com.facebook.FacebookRequestError;
import com.facebook.HttpMethod;
import com.facebook.Request;
import com.facebook.RequestAsyncTask;
import com.facebook.Response;
import com.facebook.Session;
import com.facebook.SessionState;
import com.facebook.UiLifecycleHelper;
import com.facebook.widget.LoginButton;
import com.tavultesoft.kmapro.R;

import android.support.v4.app.Fragment;
import android.text.method.ScrollingMovementMethod;
import android.content.Intent;
import android.graphics.Typeface;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.TextView;
import android.widget.Toast;

public class FBShareFragment extends Fragment {

  private UiLifecycleHelper uiHelper;
  private TextView textView;
  private Button shareButton;
  private static final List<String> PERMISSIONS = Arrays.asList("publish_actions");
  private static final String PENDING_PUBLISH_KEY = "pendingPublishReauthorization";
  private boolean pendingPublishReauthorization = false;
  private Session.StatusCallback callback = new Session.StatusCallback() {
    @Override
    public void call(Session session, SessionState state, Exception exception) {
      onSessionStateChange(session, state, exception);
    }
  };

  public static String messageText = null;
  public static Typeface messageTextTypeface = null;
  public static float messageTextSize = 0;

  @Override
  public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
    View view = inflater.inflate(R.layout.activity_fbshare, container, false);

    textView = (TextView) view.findViewById(R.id.textView);
    textView.setMovementMethod(new ScrollingMovementMethod());
    if (messageTextTypeface != null) {
      textView.setTypeface(messageTextTypeface);
    }
    if (messageTextSize > 0) {
      textView.setTextSize(messageTextSize);
    }
    if (messageText != null) {
      textView.setText(messageText);
    }

    LoginButton loginButton = (LoginButton) view.findViewById(R.id.loginButton);
    loginButton.setFragment(this);

    shareButton = (Button) view.findViewById(R.id.shareButton);
    shareButton.setOnClickListener(new View.OnClickListener() {
      @Override
      public void onClick(View v) {
        publishStory();
      }
    });

    if (savedInstanceState != null) {
      pendingPublishReauthorization = savedInstanceState.getBoolean(PENDING_PUBLISH_KEY, false);
    }

    Session session = Session.getActiveSession();
    if (session != null && session.isOpened()) {
      shareButton.setVisibility(View.VISIBLE);
    }

    return view;
  }

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    uiHelper = new UiLifecycleHelper(getActivity(), callback);
    uiHelper.onCreate(savedInstanceState);
  }

  @Override
  public void onResume() {
    super.onResume();
    uiHelper.onResume();
  }

  @Override
  public void onActivityResult(int requestCode, int resultCode, Intent data) {
    super.onActivityResult(requestCode, resultCode, data);
    uiHelper.onActivityResult(requestCode, resultCode, data);
  }

  @Override
  public void onPause() {
    super.onPause();
    uiHelper.onPause();
  }

  @Override
  public void onDestroy() {
    super.onDestroy();
    uiHelper.onDestroy();
  }

  @Override
  public void onSaveInstanceState(Bundle outState) {
    super.onSaveInstanceState(outState);
    outState.putBoolean(PENDING_PUBLISH_KEY, pendingPublishReauthorization);
    uiHelper.onSaveInstanceState(outState);
  }

  private void onSessionStateChange(Session session, SessionState state, Exception exception) {
    if (state.isOpened()) {
      shareButton.setVisibility(View.VISIBLE);
      if (pendingPublishReauthorization && state.equals(SessionState.OPENED_TOKEN_UPDATED)) {
        pendingPublishReauthorization = false;
        publishStory();
      }
    } else if (state.isClosed()) {
      shareButton.setVisibility(View.GONE);
    }
  }

  private void publishStory() {
    Session session = Session.getActiveSession();

    if (session != null) {
      // Check for publish permissions
      List<String> permissions = session.getPermissions();
      if (!isSubsetOf(PERMISSIONS, permissions)) {
        pendingPublishReauthorization = true;
        Session.NewPermissionsRequest newPermissionsRequest = new Session.NewPermissionsRequest(this, PERMISSIONS);
        session.requestNewPublishPermissions(newPermissionsRequest);
        return;
      }

      Bundle postParams = new Bundle();
      postParams.putString("message", textView.getText().toString());

      Request.Callback callback = new Request.Callback() {
        public void onCompleted(Response response) {
          try {
            FacebookRequestError error = response.getError();
            if (error != null) {
              Toast.makeText(getActivity().getApplicationContext(), error.getErrorMessage(), Toast.LENGTH_SHORT).show();
            } else {
              Toast.makeText(getActivity().getApplicationContext(), "Successfully posted", Toast.LENGTH_LONG).show();
            }
          } catch (Exception e) {
            Log.e("FBShareFragment", e.getMessage());
          }
        }
      };

      Request request = new Request(session, "me/feed", postParams, HttpMethod.POST, callback);

      RequestAsyncTask task = new RequestAsyncTask(request);
      task.execute();
    }
  }

  private boolean isSubsetOf(Collection<String> subset, Collection<String> superset) {
    for (String string : subset) {
      if (!superset.contains(string)) {
        return false;
      }
    }
    return true;
  }
}
