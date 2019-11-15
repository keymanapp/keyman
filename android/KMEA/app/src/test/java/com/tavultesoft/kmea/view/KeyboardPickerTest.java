package com.tavultesoft.kmea.view;

import android.Manifest;
import android.app.Application;
import android.content.Context;
import android.content.Intent;
import android.view.View;
import android.widget.FilterQueryProvider;
import android.widget.ListView;

import androidx.core.content.FileProvider;
import androidx.test.core.app.ApplicationProvider;

import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.KeyboardInfoActivity;
import com.tavultesoft.kmea.KeyboardListActivity;
import com.tavultesoft.kmea.KeyboardPickerActivity;
import com.tavultesoft.kmea.ModelInfoActivity;
import com.tavultesoft.kmea.R;
import com.tavultesoft.kmea.packages.PackageProcessor;
import com.tavultesoft.kmea.util.FileProviderUtils;

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
import org.robolectric.shadows.ShadowContentProvider;
import org.robolectric.shadows.ShadowContentResolver;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@RunWith(RobolectricTestRunner.class)
public class KeyboardPickerTest {

  private static final File TEST_RESOURCE_ROOT = new File("test_resources");

  private static final String TEST_GFF_KMP_NAME = "gff_amh_7_test_json";
  private static final File TEST_GFF_KMP_FILE = new File(TEST_RESOURCE_ROOT, "v14" + File.separator + TEST_GFF_KMP_NAME + ".kmp");

  private com.tavultesoft.kmea.KeyboardPickerActivity activity;

  /**
   * Test show keyboard info.
   */
  @Test
  public void openKeyboardPickerAndOpenKeyboardInfo() {

    FunctionalTestHelper.initializeKeyman();

    ActivityController<KeyboardPickerActivity> _controller = null;
    try {
       _controller = Robolectric.buildActivity(KeyboardPickerActivity.class).setup();
      //initializes the keyboard picker (and keyboard list in background)
      activity = _controller.get();

      //Initial keyboard load (normally done by webview)
      // should be done directly in  FunctionalTestHelper.initializeKeyman();
      // but we need to initialize the keyboardpicker first, because auf initialization process
      FunctionalTestHelper.setInitialKeyboard();

      //find the list view
      ListView _view = activity.findViewById(R.id.listView);
      Assert.assertNotNull(_view);

      // click the info button to open keyboard info activity
      View _itemview = _view.getAdapter().getView(0, null, null);
      Assert.assertNotNull(_itemview);
      _itemview.findViewById(R.id.imageButton1).performClick();

      // check if expected intent was sent
      Intent actual = Shadows.shadowOf(activity).getNextStartedActivity();
      Intent expectedIntent = new Intent(activity, KeyboardInfoActivity.class);
      Assert.assertEquals(expectedIntent.getComponent(), actual.getComponent());
    }
    finally {
      KMManager.onDestroy();
      _controller.pause().stop().destroy();
    }
  }



  /**
   * Test keyboard switch using keyboard picker.
   * @throws IOException
   * @throws JSONException
   */
  @Test
  public void openKeyboardPickerAndSwitchKeyboardInfo()
  throws IOException, JSONException
  {

    FunctionalTestHelper.initializeKeyman();

    ActivityController<KeyboardPickerActivity> _controller = null;
    try {
      _controller = Robolectric.buildActivity(KeyboardPickerActivity.class).setup();
      //initializes the keyboard picker (and keyboard list in background)
      activity = _controller.get();

      //Initial keyboard load (normally done by webview)
      // should be done directly in  FunctionalTestHelper.initializeKeyman();
      // but we need to initialize the keyboardpicker first, because auf initialization process
      FunctionalTestHelper.setInitialKeyboard();

      // get current keyboard
      Map<String,String> _old = KMManager.getCurrentKeyboardInfo(ApplicationProvider.getApplicationContext());
      Assert.assertNotNull(_old);

      // install new custom keyboard programmatically
      FunctionalTestHelper.installCustomKeyboard(TEST_GFF_KMP_FILE);

      // get keyboard picker list
      ListView _view = activity.findViewById(R.id.listView);
      Assert.assertNotNull(_view);

      // click the new installed keyboard, to select it.
      View _itemview = _view.getAdapter().getView(1, null, null);
      Assert.assertNotNull(_itemview);
      _view.performItemClick(_itemview,1,_view.getAdapter().getItemId(1));

      // check keyboard picker is closing
      Assert.assertTrue(activity.isFinishing());

      // check if keyboardswitch is done
      Map<String,String> _current = KMManager.getCurrentKeyboardInfo(ApplicationProvider.getApplicationContext());
      Assert.assertNotNull(_current);

      Assert.assertNotEquals(_old.get(KMManager.KMKey_KeyboardID),_current.get(KMManager.KMKey_KeyboardID));

    }
    finally {
      KMManager.onDestroy();
      _controller.pause().stop().destroy();
    }
  }

  /**
   * Test show keyboard info.
   */
  @Test
  public void openKeyboardPickerAndOpenKeyboardHelplink()
    throws IOException, JSONException
  {

    FunctionalTestHelper.initializeKeyman();

    ActivityController<KeyboardPickerActivity> _controller = null;
    ActivityController<KeyboardInfoActivity> _controller2 = null;
    try {
      _controller = Robolectric.buildActivity(KeyboardPickerActivity.class).setup();
      //initializes the keyboard picker (and keyboard list in background)
      activity = _controller.get();

      //Initial keyboard load (normally done by webview)
      // should be done directly in  FunctionalTestHelper.initializeKeyman();
      // but we need to initialize the keyboardpicker first, because auf initialization process
      FunctionalTestHelper.setInitialKeyboard();

      // install new custom keyboard programmatically
      FunctionalTestHelper.installCustomKeyboard(TEST_GFF_KMP_FILE);



      //find the list view
      ListView _view = activity.findViewById(R.id.listView);
      Assert.assertNotNull(_view);

      // click the info button to open keyboard info activity
      View _itemview = _view.getAdapter().getView(1, null, null);
      Assert.assertNotNull(_itemview);
      _itemview.findViewById(R.id.imageButton1).performClick();

      // check if expected intent was sent
      Intent actual = Shadows.shadowOf(activity).getNextStartedActivity();
      Assert.assertNotNull(actual);

      _controller2 = Robolectric.buildActivity(KeyboardInfoActivity.class,actual).setup();
      KeyboardInfoActivity _info = _controller2.get();
      Assert.assertNotNull(_info);

      ListView _infolistview = _info.findViewById(R.id.listView);
      Assert.assertNotNull(_infolistview);

      View _helplink = _view.getAdapter().getView(1, null, null);
      Assert.assertNotNull(_helplink);
      _infolistview.performItemClick(_helplink,1,_infolistview.getAdapter().getItemId(1));

      Intent actual2 = Shadows.shadowOf(_info).getNextStartedActivity();

      Assert.assertNotNull(actual2);
      Assert.assertEquals(actual2.getAction(),Intent.ACTION_VIEW);
    }
    finally {
      KMManager.onDestroy();
      if(_controller!=null)
        _controller.pause().stop().destroy();
      if(_controller2!=null)
        _controller2.pause().stop().destroy();
    }
  }

}
