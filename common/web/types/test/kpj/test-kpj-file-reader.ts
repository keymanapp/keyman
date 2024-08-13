import * as fs from 'fs';
import 'mocha';
import {assert} from 'chai';
import { makePathToFixture } from '../helpers/index.js';
import { KPJFileReader } from "../../src/kpj/kpj-file-reader.js";
import { KeymanDeveloperProjectFile10, KeymanDeveloperProjectType } from '../../src/kpj/keyman-developer-project.js';
import { TestCompilerCallbacks } from '../helpers/TestCompilerCallbacks.js';

const callbacks = new TestCompilerCallbacks();

describe('kpj-file-reader', function () {
  it('kpj-file-reader should read a valid file', function() {
    const kpjPath = 'khmer_angkor.kpj';
    const path = makePathToFixture('kpj', kpjPath);
    const input = fs.readFileSync(path);
    const reader = new KPJFileReader(callbacks);
    const kpj = reader.read(input);
    assert.doesNotThrow(() => {
      reader.validate(kpj);
    });
    assert.equal(kpj.KeymanDeveloperProject.Options.BuildPath, '$PROJECTPATH\\build');
    assert.equal(kpj.KeymanDeveloperProject.Options.CheckFilenameConventions, 'False');
    assert.equal(kpj.KeymanDeveloperProject.Options.CompilerWarningsAsErrors, 'True');
    assert.equal(kpj.KeymanDeveloperProject.Options.ProjectType, 'keyboard');
    assert.equal(kpj.KeymanDeveloperProject.Options.WarnDeprecatedCode, 'True');
    assert.isUndefined(kpj.KeymanDeveloperProject.Options.SkipMetadataFiles); // because this is a 1.0 version file
    assert.isUndefined(kpj.KeymanDeveloperProject.Options.Version);

    assert.lengthOf(kpj.KeymanDeveloperProject.Files.File, 21);

    // Test a representative set of files - kmn, kps, 2 child files

    let kf = kpj.KeymanDeveloperProject.Files.File[0];
    assert.equal(kf.ID, 'id_f347675c33d2e6b1c705c787fad4941a');
    assert.equal(kf.Filename, 'khmer_angkor.kmn');
    assert.equal(kf.Filepath, 'source\\khmer_angkor.kmn');
    assert.equal(kf.FileVersion, '1.3');
    assert.equal(kf.FileType, '.kmn');
    assert.equal(kf.Details.Name, 'Khmer Angkor');
    assert.equal(kf.Details.Copyright, '© 2015-2022 SIL International');
    assert.equal(kf.Details.Message, 'More than just a Khmer Unicode keyboard.');

    kf = kpj.KeymanDeveloperProject.Files.File[1];
    assert.equal(kf.ID, 'id_8d4eb765f80c9f2b0f769cf4e4aaa456');
    assert.equal(kf.Filename, 'khmer_angkor.kps');
    assert.equal(kf.Filepath, 'source\\khmer_angkor.kps');
    assert.isEmpty(kf.FileVersion);
    assert.equal(kf.FileType, '.kps');
    assert.equal(kf.Details.Name, 'Khmer Angkor');
    assert.equal(kf.Details.Copyright, '© 2015-2022 SIL International');

    kf = kpj.KeymanDeveloperProject.Files.File[2];
    assert.equal(kf.ID, 'id_8a1efc7c4ab7cfece8aedd847679ca27');
    assert.equal(kf.Filename, 'khmer_angkor.ico');
    assert.equal(kf.Filepath, 'source\\khmer_angkor.ico');
    assert.isEmpty(kf.FileVersion);
    assert.equal(kf.FileType, '.ico');
    assert.equal(kf.ParentFileID, 'id_f347675c33d2e6b1c705c787fad4941a');

    kf = kpj.KeymanDeveloperProject.Files.File[3];
    assert.equal(kf.ID, 'id_8dc195db32d1fd0514de0ad51fff5df0');
    assert.equal(kf.Filename, 'khmer_angkor.js');
    assert.equal(kf.Filepath, 'source\\..\\build\\khmer_angkor.js');
    assert.isEmpty(kf.FileVersion);
    assert.equal(kf.FileType, '.js');
    assert.equal(kf.ParentFileID, 'id_8d4eb765f80c9f2b0f769cf4e4aaa456');

    // Test transform of .kpj into a KeymanDeveloperProject

    const project = reader.transform(kpjPath, kpj);

    assert.equal(project.options.buildPath, '$PROJECTPATH/build');
    assert.isFalse(project.options.checkFilenameConventions);
    assert.isTrue(project.options.compilerWarningsAsErrors);
    assert.equal(project.options.projectType, KeymanDeveloperProjectType.Keyboard);
    assert.isTrue(project.options.warnDeprecatedCode);
    assert.isTrue(project.options.skipMetadataFiles);
    assert.equal(project.options.version, '1.0');

    assert.lengthOf(project.files, 2);

    let f: KeymanDeveloperProjectFile10 = <KeymanDeveloperProjectFile10>project.files[0];
    assert.equal(f.id, 'id_f347675c33d2e6b1c705c787fad4941a');
    assert.equal(f.filename, 'khmer_angkor.kmn');
    assert.equal(f.filePath, 'source/khmer_angkor.kmn');
    assert.equal(f.fileVersion, '1.3');
    assert.equal(f.fileType, '.kmn');
    assert.equal(f.details.name, 'Khmer Angkor');
    assert.equal(f.details.copyright, '© 2015-2022 SIL International');
    assert.equal(f.details.message, 'More than just a Khmer Unicode keyboard.');
    assert.isUndefined(f.details.version);
    assert.lengthOf(f.childFiles, 1);

    f = <KeymanDeveloperProjectFile10>project.files[1];
    assert.equal(f.id, 'id_8d4eb765f80c9f2b0f769cf4e4aaa456');
    assert.equal(f.filename, 'khmer_angkor.kps');
    assert.equal(f.filePath, 'source/khmer_angkor.kps');
    assert.equal(f.fileVersion, '');
    assert.equal(f.fileType, '.kps');
    assert.equal(f.details.name, 'Khmer Angkor');
    assert.isUndefined(f.details.message);
    assert.equal(f.details.copyright, '© 2015-2022 SIL International');
    assert.isUndefined(f.details.version);
    assert.lengthOf(f.childFiles, 18);

    f = <KeymanDeveloperProjectFile10>project.files[0];
    f = <KeymanDeveloperProjectFile10>f.childFiles[0];
    assert.equal(f.id, 'id_8a1efc7c4ab7cfece8aedd847679ca27');
    assert.equal(f.filename, 'khmer_angkor.ico');
    assert.equal(f.filePath, 'source/khmer_angkor.ico');
    assert.equal(f.fileVersion, '');
    assert.equal(f.fileType, '.ico');
    assert.isEmpty(f.details);
    assert.lengthOf(f.childFiles, 0);

    f = <KeymanDeveloperProjectFile10>project.files[1];
    f = <KeymanDeveloperProjectFile10>f.childFiles[0];
    assert.equal(f.id, 'id_8dc195db32d1fd0514de0ad51fff5df0');
    assert.equal(f.filename, 'khmer_angkor.js');
    assert.equal(f.filePath, 'source/../build/khmer_angkor.js');
    assert.equal(f.fileVersion, '');
    assert.equal(f.fileType, '.js');
    assert.isEmpty(f.details);
    assert.lengthOf(f.childFiles, 0);
  });

  it('should load a v1.0 keyboard project with missing <File>', function() {
    const path = makePathToFixture('kpj', 'project-missing-file', 'project_missing_file.kpj');
    const input = fs.readFileSync(path);
    const reader = new KPJFileReader(callbacks);
    const kpj = reader.read(input);
    reader.validate(kpj);
    if(callbacks.messages.length) {
      callbacks.printMessages();
    }
    assert.equal(callbacks.messages.length, 0);
    assert.lengthOf(kpj.KeymanDeveloperProject.Files.File, 0);
    const project = reader.transform(path, kpj);
    assert.equal(callbacks.messages.length, 0);
    assert.isNotNull(project);
  });

  it('should load a v1.0 keyboard project with missing <Files>', function() {
    const path = makePathToFixture('kpj', 'project-missing-file', 'project_missing_files.kpj');
    const input = fs.readFileSync(path);
    const reader = new KPJFileReader(callbacks);
    const kpj = reader.read(input);
    reader.validate(kpj);
    assert.equal(callbacks.messages.length, 0);
    assert.lengthOf(kpj.KeymanDeveloperProject.Files.File, 0);
    const project = reader.transform(path, kpj);
    assert.equal(callbacks.messages.length, 0);
    assert.isNotNull(project);
  });

});
