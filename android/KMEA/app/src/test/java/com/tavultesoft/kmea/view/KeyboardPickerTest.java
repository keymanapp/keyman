package com.tavultesoft.kmea.view;

import android.content.Intent;
import android.view.View;
import android.widget.ListView;

import androidx.test.core.app.ApplicationProvider;

import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.KeyboardInfoActivity;
import com.tavultesoft.kmea.KeyboardPickerActivity;
import com.tavultesoft.kmea.R;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.Robolectric;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.RuntimeEnvironment;
import org.robolectric.Shadows;
import org.robolectric.android.controller.ActivityController;

@RunWith(RobolectricTestRunner.class)
public class KeyboardPickerTest {
  @Test
  public void openKeyboardPickerAndSelectLanguage() {

    KMManager.initialize(
      ApplicationProvider.getApplicationContext(), KMManager.KeyboardType.KEYBOARD_TYPE_INAPP);

    ActivityController<KeyboardPickerActivity> _controller =
      Robolectric.buildActivity(KeyboardPickerActivity.class).setup();

    com.tavultesoft.kmea.KeyboardPickerActivity activity = _controller.get();
    ListView _view = activity.findViewById(R.id.listView);
    Assert.assertNotNull(_view);

    View _itemview = _view.getAdapter().getView(0, null, null);
    Assert.assertNotNull(_itemview);
    _itemview.findViewById(R.id.imageButton1).performClick();

    Intent actual = Shadows.shadowOf(activity).getNextStartedActivity();
    Intent expectedIntent = new Intent(activity, KeyboardInfoActivity.class);
    Assert.assertEquals(expectedIntent.getComponent(), actual.getComponent());
  }
}
