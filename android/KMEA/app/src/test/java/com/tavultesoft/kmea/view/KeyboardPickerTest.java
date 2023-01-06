package com.keyman.engine.view;

import android.content.Intent;
import android.util.Log;
import android.view.View;
import android.widget.ListView;

import androidx.test.core.app.ApplicationProvider;

import com.keyman.engine.KMManager;
import com.keyman.engine.KeyboardInfoActivity;
import com.keyman.engine.KeyboardPickerActivity;
import com.keyman.engine.R;
import com.keyman.engine.data.Keyboard;

import org.json.JSONException;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.Robolectric;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.Shadows;
import org.robolectric.android.controller.ActivityController;

import java.io.File;
import java.io.IOException;

@RunWith(RobolectricTestRunner.class)
public class KeyboardPickerTest {

  private static final File TEST_RESOURCE_ROOT = new File("test_resources");

  private static final File TEST_DEFAULT_KMP_FILE = new File(TEST_RESOURCE_ROOT, "v14" + File.separator + "sil_euro_latin.kmp");
  private static final String TEST_GFF_KMP_NAME = "gff_amh_7_test_json";
  private static final File TEST_GFF_KMP_FILE = new File(TEST_RESOURCE_ROOT, "v14" + File.separator + TEST_GFF_KMP_NAME + ".kmp");

  private ActivityController<KeyboardPickerActivity> keyboardPickerActivityActivityController;


  /**
   * initialize tests.
   */
  @Before
  public void startTest()
  {
    FunctionalTestHelper.initializeKeyman();
    //initializes the keyboard picker (and keyboard list in background)
    keyboardPickerActivityActivityController = Robolectric.buildActivity(KeyboardPickerActivity.class).create();
    keyboardPickerActivityActivityController.start();
    keyboardPickerActivityActivityController.resume();

// install new custom keyboard programmatically
    try {
      FunctionalTestHelper.installCustomKeyboard(TEST_DEFAULT_KMP_FILE);
    } catch (IOException | JSONException e) {
      Log.e("KeyboardPickerTest", "Exception installing default kmp");
    }

    //Initial keyboard load (normally done by webview)
    // should be done directly in  FunctionalTestHelper.initializeKeyman();
    // but we need to initialize the keyboardpicker first, because auf initialization process
    FunctionalTestHelper.setInitialKeyboard();

  }

  /**
   * clean up tests.
   */
  @After
  public void endTest()
  {
    KMManager.onDestroy();
    if(keyboardPickerActivityActivityController!=null)
      keyboardPickerActivityActivityController.pause().stop().destroy();
    keyboardPickerActivityActivityController=null;
  }
  /**
   * Test show keyboard info.
   */
  @Ignore("Investigate ResourcesNotFoundException")
  @Test
  public void openKeyboardPickerAndOpenKeyboardInfo()
  {
      KeyboardPickerActivity activity = keyboardPickerActivityActivityController.get();

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

  /**
   * Test keyboard switch using keyboard picker.
   * @throws IOException
   * @throws JSONException
   */
  @Ignore("Investigate ResourcesNotFoundException")
  @Test
  public void openKeyboardPickerAndSwitchKeyboardInfo()
  throws IOException, JSONException
  {
      KeyboardPickerActivity activity = keyboardPickerActivityActivityController.get();

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
      int index = KeyboardPickerActivity.selectedIndex();
      Keyboard _current = KMManager.getKeyboardInfo(ApplicationProvider.getApplicationContext(), index);
      Assert.assertNotNull(_current);
  }

  /**
   * Test show keyboard info and help.
   */
  @Ignore("Investigate ResourcesNotFoundException")
  @Test
  public void openKeyboardPickerAndOpenKeyboardHelplink()
    throws IOException, JSONException
  {

    FunctionalTestHelper.initializeKeyman();

    ActivityController<KeyboardInfoActivity> _controller2 = null;
    try {

      KeyboardPickerActivity activity = keyboardPickerActivityActivityController.get();

      // install new custom keyboard programmatically
      FunctionalTestHelper.installCustomKeyboard(TEST_GFF_KMP_FILE);

      //find the list view
      ListView _view = activity.findViewById(R.id.listView);
      Assert.assertNotNull(_view);

      // click the info button to open keyboard info activity
      int expectedKeyboardID = 0; // Index for expected keyboard
      View _itemview = _view.getAdapter().getView(expectedKeyboardID, null, null);
      Assert.assertNotNull(_itemview);
      _itemview.findViewById(R.id.imageButton1).performClick();

      // check if expected intent was sent
      Intent actual = Shadows.shadowOf(activity).getNextStartedActivity();
      Assert.assertNotNull(actual);

      //execute next activity -> opens Keyboardinfo
      _controller2 = Robolectric.buildActivity(KeyboardInfoActivity.class,actual).setup();
      KeyboardInfoActivity _info = _controller2.get();
      Assert.assertNotNull(_info);

      //get info list
      ListView _infolistview = _info.findViewById(R.id.listView);
      Assert.assertNotNull(_infolistview);

      //find helplink and click
      View _helplink = _view.getAdapter().getView(expectedKeyboardID, null, null);
      Assert.assertNotNull(_helplink);
      _infolistview.performItemClick(_helplink,1,_infolistview.getAdapter().getItemId(1));

      // check result intent to be a weblink to open
      Intent actual2 = Shadows.shadowOf(_info).getNextStartedActivity();
      Assert.assertNotNull(actual2);
      Assert.assertEquals("com.keyman.engine.KMHelpFileActivity", actual2.getComponent().getClassName());
    }
    finally {
      if(_controller2!=null)
        _controller2.pause().stop().destroy();
    }
  }

}
