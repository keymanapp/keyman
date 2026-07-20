/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
package com.keyman.engine.data;

import android.content.Context;
import androidx.test.core.app.ApplicationProvider;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

@RunWith(RobolectricTestRunner.class)
public class CloudRepositoryTests {

  private CloudRepository repository;
  private Dataset dataset;
  private Context context;

  private boolean containsModel(Dataset.LexicalModels models, String modelID) {
    for (int i = 0; i < models.getCount(); i++) {
      if (models.getItem(i).getLexicalModelID().equals(modelID)) {
        return true;
      }
    }
    return false;
  }

  @Before
  public void setUp() {
    context = ApplicationProvider.getApplicationContext();
    repository = CloudRepository.shared;
    dataset = new Dataset(context);
  }

  @Test
  public void testMergeLexicalModels_PreservesUniqueExistingModels() throws Exception {
    // Setup
    dataset.lexicalModels.add(new LexicalModel("pkg1", "model1", "Model 1", "en", "English", "1.0", "", ""));
    List<LexicalModel> newModels = new ArrayList<>();
    newModels.add(new LexicalModel("pkg2", "model2", "Model 2", "fr", "French", "1.0", "", ""));

    // Execute
    Method mergeMethod = CloudRepository.class.getDeclaredMethod("mergeLexicalModels", Dataset.LexicalModels.class, List.class);
    mergeMethod.setAccessible(true);
    mergeMethod.invoke(repository, dataset.lexicalModels, newModels);

    // Verify
    Assert.assertEquals(2, dataset.lexicalModels.getCount());
    Assert.assertTrue(containsModel(dataset.lexicalModels, "model1"));
    Assert.assertTrue(containsModel(dataset.lexicalModels, "model2"));
  }

  @Test
  public void testMergeLexicalModels_UpdatesVersion() throws Exception {
    // Setup
    dataset.lexicalModels.add(new LexicalModel("pkg1", "model1", "Model 1", "en", "English", "1.0", "", ""));

    List<LexicalModel> newModels = new ArrayList<>();
    // New model (same ID, newer version v1.1)
    newModels.add(new LexicalModel("pkg1", "model1", "Model 1", "en", "English", "1.1", "", ""));

    // Execute
    Method mergeMethod = CloudRepository.class.getDeclaredMethod("mergeLexicalModels", Dataset.LexicalModels.class, List.class);
    mergeMethod.setAccessible(true);
    mergeMethod.invoke(repository, dataset.lexicalModels, newModels);

    // Verify
    Assert.assertEquals(1, dataset.lexicalModels.getCount());
    LexicalModel result = dataset.lexicalModels.getItem(0);
    Assert.assertEquals("1.1", result.getVersion());
  }

  @Test
  public void testMergeLexicalModels_KeepsOlderIfNewerIsLowerVersion() throws Exception {
    // Setup
    dataset.lexicalModels.add(new LexicalModel("pkg1", "model1", "Model 1", "en", "English", "1.2", "", ""));

    List<LexicalModel> newModels = new ArrayList<>();
    // New model (same ID, older version v1.1)
    newModels.add(new LexicalModel("pkg1", "model1", "Model 1", "en", "English", "1.1", "", ""));

    // Execute
    Method mergeMethod = CloudRepository.class.getDeclaredMethod("mergeLexicalModels", Dataset.LexicalModels.class, List.class);
    mergeMethod.setAccessible(true);
    mergeMethod.invoke(repository, dataset.lexicalModels, newModels);

    // Verify
    Assert.assertEquals(1, dataset.lexicalModels.getCount());
    LexicalModel result = dataset.lexicalModels.getItem(0);
    Assert.assertEquals("1.2", result.getVersion());
  }
}
