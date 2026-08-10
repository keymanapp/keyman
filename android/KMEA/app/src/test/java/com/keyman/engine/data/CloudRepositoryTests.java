/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
package com.keyman.engine.data;

import android.content.Context;
import androidx.test.core.app.ApplicationProvider;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;

import java.lang.reflect.Field;
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

  private void setMemCachedDataset(Dataset dataset) throws Exception {
    Field field = CloudRepository.class.getDeclaredField("memCachedDataset");
    field.setAccessible(true);
    field.set(repository, dataset);
  }

  @Before
  public void setUp() {
    context = ApplicationProvider.getApplicationContext();
    repository = CloudRepository.shared;
    dataset = new Dataset(context);
  }

  @After
  public void tearDown() throws Exception {
    // Reset singleton state
    setMemCachedDataset(null);
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

  @Test
  public void testGetAssociatedLexicalModel_ReturnsCorrectModel() throws Exception {
    // Setup
    LexicalModel model1 = new LexicalModel("pkg1", "model1", "Model 1", "en", "English", "1.0", "", "");
    LexicalModel model2 = new LexicalModel("pkg2", "model2", "Model 2", "fr", "French", "1.0", "", "");
    dataset.lexicalModels.add(model1);
    dataset.lexicalModels.add(model2);
    setMemCachedDataset(dataset);

    // Execute & Verify
    Assert.assertEquals(model1, repository.getAssociatedLexicalModel(context, "en"));
    Assert.assertEquals(model2, repository.getAssociatedLexicalModel(context, "fr"));
    Assert.assertEquals(model1, repository.getAssociatedLexicalModel(context, "EN")); // Case insensitivity
  }

  @Test
  public void testGetAssociatedLexicalModel_ReturnsNullWhenDatasetIsNull() throws Exception {
    // Setup
    setMemCachedDataset(null);

    // Execute & Verify
    Assert.assertNull(repository.getAssociatedLexicalModel(context, "en"));
  }

  @Test
  public void testGetAssociatedLexicalModel_ReturnsFirstMatch() throws Exception {
    // Setup
    LexicalModel model1 = new LexicalModel("pkg1", "model1", "Model 1", "en", "English", "1.0", "", "");
    LexicalModel model2 = new LexicalModel("pkg2", "model2", "Model 2", "en", "English", "1.0", "", "");
    dataset.lexicalModels.add(model1);
    dataset.lexicalModels.add(model2);
    setMemCachedDataset(dataset);

    // Execute & Verify
    Assert.assertEquals(model1, repository.getAssociatedLexicalModel(context, "en"));
  }
}
