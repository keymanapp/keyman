package com.keyman.engine.logic;

import android.content.Context;
import android.content.SharedPreferences;

import androidx.test.core.app.ApplicationProvider;

import com.keyman.engine.R;

import org.junit.Assert;
import org.junit.Assume;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;

import java.util.Calendar;
import java.util.GregorianCalendar;

@RunWith(RobolectricTestRunner.class)
public class ResourcesUpdateToolTest {

  @Ignore("Investigate ResourcesNotFoundException")
  @Test
  public void shouldUpdateFirstCallTest() {
    ResourcesUpdateTool _updateTool = new ResourcesUpdateTool();
    Assert.assertTrue(_updateTool.shouldCheckUpdate(ApplicationProvider.getApplicationContext()));
  }

  @Ignore("Investigate ResourcesNotFoundException")
  @Test
  public void shouldUpdateFalseCallTest() {
    Assume.assumeFalse(ResourcesUpdateTool.FORCE_RESOURCE_UPDATE);

    Calendar _cal = GregorianCalendar.getInstance();
    _cal.add(Calendar.HOUR_OF_DAY,-5);

    setLastUpdatePref(_cal);

    ResourcesUpdateTool _updateTool = new ResourcesUpdateTool();
    Assert.assertFalse(_updateTool.shouldCheckUpdate(ApplicationProvider.getApplicationContext()));
  }

  @Ignore("Investigate ResourcesNotFoundException")
  @Test
  public void shouldUpdateFalseCallTest2() {
    Assume.assumeFalse(ResourcesUpdateTool.FORCE_RESOURCE_UPDATE);

    Calendar _cal = GregorianCalendar.getInstance();

    setLastUpdatePref(_cal);

    ResourcesUpdateTool _updateTool = new ResourcesUpdateTool();
    Assert.assertFalse(_updateTool.shouldCheckUpdate(ApplicationProvider.getApplicationContext()));
  }

  @Ignore("Investigate ResourcesNotFoundException")
  @Test
  public void shouldUpdateTrueCallTest() {
    Calendar _cal = GregorianCalendar.getInstance();
    _cal.add(Calendar.DAY_OF_MONTH,-2);
    _cal.add(Calendar.MINUTE,-1);
    setLastUpdatePref(_cal);

    ResourcesUpdateTool _updateTool = new ResourcesUpdateTool();
    Assert.assertTrue(_updateTool.shouldCheckUpdate(ApplicationProvider.getApplicationContext()));
  }

  private void setLastUpdatePref(Calendar theLastUpdate) {
    SharedPreferences prefs = ApplicationProvider.getApplicationContext().getSharedPreferences(
      ApplicationProvider.getApplicationContext().getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);

    SharedPreferences.Editor editor = prefs.edit();
    editor.putLong(ResourcesUpdateTool.PREF_KEY_LAST_UPDATE_CHECK, theLastUpdate.getTimeInMillis());
    editor.commit();
  }
}
