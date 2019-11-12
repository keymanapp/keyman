package com.tavultesoft.kmea.view;

import android.content.Context;
import android.content.Intent;
import android.view.View;
import android.widget.ListView;

import androidx.test.core.app.ApplicationProvider;

import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.KeyboardInfoActivity;
import com.tavultesoft.kmea.KeyboardPickerActivity;
import com.tavultesoft.kmea.R;
import com.tavultesoft.kmea.packages.PackageProcessor;

import org.json.JSONException;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.Robolectric;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.Shadows;
import org.robolectric.android.controller.ActivityController;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@RunWith(RobolectricTestRunner.class)
public class KeyboardPickerTest {

  private com.tavultesoft.kmea.KeyboardPickerActivity activity;

  @Test
  public void openKeyboardPickerAndOpenKeyboardInfo() {

    FunctionalTestHelper.initializeKeyman();

    //initializes the keyboard list
    activity = Robolectric.buildActivity(KeyboardPickerActivity.class).setup().get();

    FunctionalTestHelper.setInitialKeyboard();

    ListView _view = activity.findViewById(R.id.listView);
    Assert.assertNotNull(_view);

    View _itemview = _view.getAdapter().getView(0, null, null);
    Assert.assertNotNull(_itemview);
    _itemview.findViewById(R.id.imageButton1).performClick();

    Intent actual = Shadows.shadowOf(activity).getNextStartedActivity();
    Intent expectedIntent = new Intent(activity, KeyboardInfoActivity.class);
    Assert.assertEquals(expectedIntent.getComponent(), actual.getComponent());

    KMManager.onDestroy();

  }

  private static final File TEST_RESOURCE_ROOT = new File("test_resources");

  private static final String TEST_GFF_KMP_NAME = "gff_amh_7_test_json";
  private static final File TEST_GFF_KMP_FILE = new File(TEST_RESOURCE_ROOT, "v14" + File.separator + TEST_GFF_KMP_NAME + ".kmp");


  @Test
  public void openKeyboardPickerAndSwitchKeyboardInfo()
  throws IOException, JSONException
  {

    FunctionalTestHelper.initializeKeyman();

    //initializes the keyboard list
    ActivityController<KeyboardPickerActivity> controller = Robolectric.buildActivity(KeyboardPickerActivity.class);
    activity = controller.setup().get();

    FunctionalTestHelper.setInitialKeyboard();

    Map<String,String> _old = KMManager.getCurrentKeyboardInfo(ApplicationProvider.getApplicationContext());
    Assert.assertNotNull(_old);

    FunctionalTestHelper.installCustomKeyboard(TEST_GFF_KMP_FILE);

    ListView _view = activity.findViewById(R.id.listView);
    Assert.assertNotNull(_view);

    View _itemview = _view.getAdapter().getView(1, null, null);
    Assert.assertNotNull(_itemview);
    _view.performItemClick(_itemview,1,_view.getAdapter().getItemId(1));

    Assert.assertTrue(activity.isFinishing());

    Map<String,String> _current = KMManager.getCurrentKeyboardInfo(ApplicationProvider.getApplicationContext());
    Assert.assertNotNull(_current);

    Assert.assertNotEquals(_old.get(KMManager.KMKey_KeyboardID),_current.get(KMManager.KMKey_KeyboardID));

    KMManager.onDestroy();
  }


}
