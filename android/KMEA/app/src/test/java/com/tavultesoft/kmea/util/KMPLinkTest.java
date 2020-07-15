package com.tavultesoft.kmea.util;

import org.junit.Assert;
import org.junit.Test;

import com.tavultesoft.kmea.util.KMPLink;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;

public class KMPLinkTest {

  @Test
  public void test_isKeymanInstallLink() {
    Assert.assertFalse(KMPLink.isKeymanInstallLink(null));
    Assert.assertFalse(KMPLink.isKeymanInstallLink(""));

    // Valid Keyman keyboard install links
    Assert.assertTrue(KMPLink.isKeymanInstallLink("https://staging-keyman-com.azurewebsites.net/keyboards/install/malar_malayalam"));
    Assert.assertTrue(KMPLink.isKeymanInstallLink("https://staging-keyman-com.azurewebsites.net/keyboards/install/malar_malayalam?bcp47=ml"));
    Assert.assertTrue(KMPLink.isKeymanInstallLink("https://staging-keyman-com.azurewebsites.net/keyboards/install/fv_sencoten?bcp47=str-Latn"));
    Assert.assertTrue(KMPLink.isKeymanInstallLink("https://keyman.com/keyboards/install/malar_malayalam"));
    Assert.assertTrue(KMPLink.isKeymanInstallLink("https://keyman.com/keyboards/install/malar_malayalam?bcp47=ml"));

    // Keyboard link is wrong
    Assert.assertFalse(KMPLink.isKeymanInstallLink("https://staging-keyman-com.azurewebsites.net/keyboard/install/malar_malayalam"));
    Assert.assertFalse(KMPLink.isKeymanInstallLink("https://staging-keyman-com.azurewebsites.net/keyboard/install/malar_malayalam?bcp47=ml"));
    Assert.assertFalse(KMPLink.isKeymanInstallLink("https://keyman.com/keyboard/install/malar_malayalam"));
    Assert.assertFalse(KMPLink.isKeymanInstallLink("https://keyman.com/keyboard/install/malar_malayalam?bcp47=ml"));

    // link missing packageID
    Assert.assertFalse(KMPLink.isKeymanInstallLink("https://staging-keyman-com.azurewebsites.net/keyboards/install"));
    Assert.assertFalse(KMPLink.isKeymanInstallLink("https://staging-keyman-com.azurewebsites.net/keyboards/install/"));
    Assert.assertFalse(KMPLink.isKeymanInstallLink("https://keyman.com/keyboards/install"));
    Assert.assertFalse(KMPLink.isKeymanInstallLink("https://keyman.com/keyboards/install/"));
  }

  @Test
  public void test_isKeymanDownloadLink() {
    Assert.assertFalse(KMPLink.isKeymanDownloadLink(null));
    Assert.assertFalse(KMPLink.isKeymanDownloadLink(""));

    // Valid Keyman keyboard download links
    Assert.assertTrue(KMPLink.isKeymanDownloadLink("https://staging-keyman-com.azurewebsites.net/go/package/download/malar_malayalam?platform=android&tier=alpha"));
    Assert.assertTrue(KMPLink.isKeymanDownloadLink("https://staging-keyman-com.azurewebsites.net/go/package/download/malar_malayalam?platform=android&tier=alpha&bcp47=ml"));
    Assert.assertTrue(KMPLink.isKeymanDownloadLink("https://keyman.com/go/package/download/malar_malayalam?platform=android&tier=alpha"));
    Assert.assertTrue(KMPLink.isKeymanDownloadLink("https://keyman.com/go/package/download/malar_malayalam?platform=android&tier=alpha&bcp47=ml"));

    // Valid Keyman 13.0 keyboard download links
    Assert.assertTrue(KMPLink.isKeymanDownloadLink("https://keyman.com/keyboard/download?id=khmer_angkor&platform=android&mode=standalone&cid=335710037.1594761864"));

    // Invalid Keyman 13.0 keyboard download links
    Assert.assertFalse(KMPLink.isKeymanDownloadLink("https://keyman.com/keyboard/download?id="));
    Assert.assertFalse(KMPLink.isKeymanDownloadLink("https://keyman.com/keyboard/download?id=khmer_angkor"));

    // Staging host with Keyman 13.0 download link is invalid
    Assert.assertFalse(KMPLink.isKeymanDownloadLink("https://staging-keyman.com.azurewebsites.net/keyboard/download?id=khmer_angkor&platform=android&mode=standalone&cid=335710037.1594761864"));
  }
}