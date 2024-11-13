import 'mocha';
import * as fs from 'fs';
import { assert } from 'chai';
import { InfrastructureMessages } from '../src/messages/infrastructureMessages.js';
import { verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';
import { makePathToFixture } from './helpers/index.js';
import { NodeCompilerCallbacks } from '../src/util/NodeCompilerCallbacks.js';
import {  KeymanFileTypes } from '@keymanapp/common-types';
import { unitTestEndpoints } from '../src/commands/build.js';
import { KmnCompilerMessages } from '@keymanapp/kmc-kmn';
import { clearOptions } from '@keymanapp/developer-utils';
import { loadProject } from '../src/util/projectLoader.js';
import { CompilerErrorNamespace, CompilerEvent, defaultCompilerOptions, CompilerOptions} from '@keymanapp/developer-utils';
import { analyzeUnitTestEndpoints } from '../src/commands/analyze.js';
import { BuildKeyboardInfo } from '../src/commands/buildClasses/BuildKeyboardInfo.js';
import { BuildModelInfo } from '../src/commands/buildClasses/BuildModelInfo.js';

describe('InfrastructureMessages', function () {

  beforeEach(clearOptions);

  it('should have a valid InfrastructureMessages object', function() {
    return verifyCompilerMessagesObject(InfrastructureMessages, CompilerErrorNamespace.Infrastructure);
  });

  //
  // Message tests
  //

  // FATAL_UnexpectedException

  it('should generate FATAL_UnexpectedException if an exception is raised', async function() {
    const ncb = new NodeCompilerCallbacks({logLevel: 'silent'});
    const expectedMessages = [InfrastructureMessages.FATAL_UnexpectedException];

    process.env.SENTRY_CLIENT_TEST_BUILD_EXCEPTION = '1';
    await unitTestEndpoints.build(null, null, ncb, {});
    delete process.env.SENTRY_CLIENT_TEST_BUILD_EXCEPTION;

    assertMessagesEqual(ncb.messages, expectedMessages);
    assert.instanceOf<Error>(ncb.messages[0].exceptionVar, Error);
    });

  // ERROR_FileDoesNotExist

  it('should generate ERROR_FileDoesNotExist if a file does not exist', async function() {
    const ncb = new NodeCompilerCallbacks({logLevel: 'silent'});
    await unitTestEndpoints.build(makePathToFixture('invalid-keyboards', 'Error_File_Does_Not_Exist.kmn'), null, ncb, {});
    assert.isTrue(ncb.hasMessage(InfrastructureMessages.ERROR_FileDoesNotExist),
      'ERROR_FileDoesNotExist not generated, instead got: '+JSON.stringify(ncb.messages,null,2));
  });

  // ERROR_FileTypeNotRecognized

  it('should generate ERROR_FileTypeNotRecognized if a file is not a recognized type', async function() {
    const ncb = new NodeCompilerCallbacks({logLevel: 'silent'});
    await unitTestEndpoints.build(makePathToFixture('invalid-keyboards', 'error_file_type_not_recognized.xxx'), null, ncb, {});
    assert.isTrue(ncb.hasMessage(InfrastructureMessages.ERROR_FileTypeNotRecognized),
      'ERROR_FileTypeNotRecognized not generated, instead got: '+JSON.stringify(ncb.messages,null,2));
  });

  // ERROR_OutFileNotValidForProjects

  it('should generate ERROR_OutFileNotValidForProjects if an output file is specified for a project build', async function() {
    const ncb = new NodeCompilerCallbacks({logLevel: 'silent'});
    const projectPath = makePathToFixture('kpj-2.0/khmer_angkor', 'khmer_angkor.kpj');
    const outFilePath = makePathToFixture('kpj-2.0/khmer_angkor', 'khmer_angkor.kmx');
    const options: CompilerOptions = {...defaultCompilerOptions};
    await unitTestEndpoints.build(projectPath, outFilePath, ncb, options);
    assert.isTrue(ncb.hasMessage(InfrastructureMessages.ERROR_OutFileNotValidForProjects),
      'ERROR_OutFileNotValidForProjects not generated, instead got: '+JSON.stringify(ncb.messages,null,2));
  });

  // ERROR_InvalidProjectFile

  it('should generate ERROR_InvalidProjectFile if a project file is invalid', async function() {
    const ncb = new NodeCompilerCallbacks({logLevel: 'silent'});
    await unitTestEndpoints.build(makePathToFixture('invalid-projects', 'error_invalid_project_file.kpj'), null, ncb, {});
    assert.isTrue(ncb.hasMessage(InfrastructureMessages.ERROR_InvalidProjectFile),
      'ERROR_InvalidProjectFile not generated, instead got: '+JSON.stringify(ncb.messages,null,2));
  });

  // ERROR_NotAProjectFile

  it('should generate ERROR_NotAProjectFile if a project file is not the correct type', async function() {
    const ncb = new NodeCompilerCallbacks({logLevel: 'silent'});
    const projectPath = makePathToFixture('invalid-projects', 'error_not_a_project_file.xxx')
    await loadProject(projectPath, ncb);
    assert.isTrue(ncb.hasMessage(InfrastructureMessages.ERROR_NotAProjectFile),
    'ERROR_NotAProjectFile not generated, instead got: '+JSON.stringify(ncb.messages,null,2));
  });

  // ERROR_UnknownFileFormat

  it('should generate ERROR_UnknownFileFormat if an analyze osk-char-use mapping file is not the correct type', async function() {
    const ncb = new NodeCompilerCallbacks({logLevel: 'silent'});
    const options = {mappingFile: makePathToFixture('analyze', 'error_unknown_file_format.xxx')};
    await analyzeUnitTestEndpoints.analyzeOskCharUse(ncb, [], options);
    assert.isTrue(ncb.hasMessage(InfrastructureMessages.ERROR_UnknownFileFormat),
      'ERROR_UnknownFileFormat not generated, instead got: '+JSON.stringify(ncb.messages,null,2));
  });

  // ERROR_FileTypeNotFound (BuildKeyboardInfo)

  it('should generate ERROR_FileTypeNotFound if a project file does not contain a .kps file entry (BuildKeyboardInfo)', async function() {
    const buildKeyboardInfo = new BuildKeyboardInfo();
    const projectPath = makePathToFixture('invalid-projects', 'error_file_type_not_found__keyboard.kpj')
    const ncb = new NodeCompilerCallbacks({logLevel: 'silent'});
    await buildKeyboardInfo.build(projectPath, null, ncb, {});
    assert.isTrue(ncb.hasMessage(InfrastructureMessages.ERROR_FileTypeNotFound),
      'ERROR_FileTypeNotFound not generated, instead got: '+JSON.stringify(ncb.messages,null,2));
  });

  // ERROR_FileTypeNotFound (BuildModelInfo; .model.ts)

  it('should generate ERROR_FileTypeNotFound if a project file does not contain a .model.ts file entry (BuildModelInfo)', async function() {
    const buildModelInfo = new BuildModelInfo();
    const projectPath = makePathToFixture('invalid-projects', 'error_file_type_not_found__model_ts.kpj')
    const ncb = new NodeCompilerCallbacks({logLevel: 'silent'});
    await buildModelInfo.build(projectPath, null, ncb, {});
    assert.isTrue(ncb.hasMessage(InfrastructureMessages.ERROR_FileTypeNotFound),
      'ERROR_FileTypeNotFound not generated, instead got: '+JSON.stringify(ncb.messages,null,2));
    assert.isTrue(nodeCompilerMessage(ncb, InfrastructureMessages.ERROR_FileTypeNotFound).includes(KeymanFileTypes.Source.Model),
      KeymanFileTypes.Source.Model+' not found in the message');
  });

  // ERROR_FileTypeNotFound (BuildModelInfo; .kps)

  it('should generate ERROR_FileTypeNotFound if a project file does not contain a .kps file entry (BuildModelInfo)', async function() {
    const buildModelInfo = new BuildModelInfo();
    const projectPath = makePathToFixture('invalid-projects', 'error_file_type_not_found__model_kps.kpj')
    const ncb = new NodeCompilerCallbacks({logLevel: 'silent'});
    await buildModelInfo.build(projectPath, null, ncb, {});
    assert.isTrue(ncb.hasMessage(InfrastructureMessages.ERROR_FileTypeNotFound),
      'ERROR_FileTypeNotFound not generated, instead got: '+JSON.stringify(ncb.messages,null,2));
    assert.isTrue(nodeCompilerMessage(ncb, InfrastructureMessages.ERROR_FileTypeNotFound).includes(KeymanFileTypes.Source.Package),
      KeymanFileTypes.Source.Package+' not found in the message');
  });

  // ERROR_InvalidProjectFolder (no source folder)
  it('should generate ERROR_InvalidProjectFolder if there is no source folder when generating a default project file', async function() {
    const projectPath = makePathToFixture('no-source-folder', 'error_invalid_project_folder.kpj')
    const ncb = new NodeCompilerCallbacks({logLevel: 'silent'});
    await loadProject(projectPath, ncb);
    assert.isTrue(ncb.hasMessage(InfrastructureMessages.ERROR_InvalidProjectFolder),
      'ERROR_InvalidProjectFolder not generated, instead got: '+JSON.stringify(ncb.messages,null,2));
  });

  // ERROR_InvalidProjectFolder (invalid source folder)

  it('should generate ERROR_InvalidProjectFolder if there are no valid file types in the source folder when generating a default project file', async function() {
    const projectPath = makePathToFixture('invalid-source-folder', 'error_invalid_project_folder.kpj')
    const ncb = new NodeCompilerCallbacks({logLevel: 'silent'});
    await loadProject(projectPath, ncb);
    assert.isTrue(ncb.hasMessage(InfrastructureMessages.ERROR_InvalidProjectFolder),
      'ERROR_InvalidProjectFolder not generated, instead got: '+JSON.stringify(ncb.messages,null,2));
  });

  // ERROR_CannotCreateFolder

  it('should generate ERROR_CannotCreateFolder if the output folder cannot be created', async function() {
    const buildKeyboardInfo = new BuildKeyboardInfo();
    const targetFilename = makePathToFixture('invalid-projects', 'build', 'error_cannot_create_folder.xxx')
    const ncb = new NodeCompilerCallbacks({logLevel: 'silent'});
    buildKeyboardInfo['createOutputFolder'](targetFilename, ncb); // call private method
    assert.isTrue(ncb.hasMessage(InfrastructureMessages.ERROR_CannotCreateFolder),
      'ERROR_CannotCreateFolder not generated, instead got: '+JSON.stringify(ncb.messages,null,2));
  });

  // HINT_FilenameHasDifferingCase

  it('should generate HINT_FilenameHasDifferingCase if a referenced file has differing case', async function() {
    // This message is generated by NodeCompilerCallbacks, because that's where the filesystem is visible,
    // so we can't use our usual testForMessage pattern.
    const filename = makePathToFixture('invalid-keyboards', 'Hint_Filename_Has_Differing_Case.kmn');
    if(fs.existsSync(filename)) {
      // The file is actually named 'hint_filename_has_differing_case.kmn', so
      // if we can see it, then we know we are not on a case-sensitive
      // filesystem, and so we can test
      const ncb = new NodeCompilerCallbacks({logLevel: 'silent'});
      const expectedMessages = [InfrastructureMessages.HINT_FilenameHasDifferingCase];
      assert.isNotNull(ncb.loadFile(filename), `Expected to load 'hint_filename_has_differing_case.kmn' with mixed case '${filename}'`);
      assertMessagesEqual(ncb.messages, expectedMessages);
    } else {
      // We are on a case-sensitive filesystem, so this hint can never be generated
      assert.isTrue(true);
    }
  });

  // INFO_WarningsHaveFailedBuild

  it('should generate INFO_WarningsHaveFailedBuild if only warnings failed the build', async function() {
    const ncb = new NodeCompilerCallbacks({logLevel: 'silent'});
    const filename = makePathToFixture('compiler-warnings-as-errors', 'keyboard.kmn');
    const expectedMessages = [
      InfrastructureMessages.INFO_BuildingFile,
      KmnCompilerMessages.WARN_HeaderStatementIsDeprecated,
      InfrastructureMessages.INFO_WarningsHaveFailedBuild,
      InfrastructureMessages.INFO_FileNotBuiltSuccessfully
    ];
    await unitTestEndpoints.build(filename, null, ncb, {compilerWarningsAsErrors: true});
    assertMessagesEqual(ncb.messages, expectedMessages);
  });

  // ERROR_UnsupportedProjectVersion

  it('should generate ERROR_UnsupportedProjectVersion if a project file with an unsupported version is loaded', async function() {
    const ncb = new NodeCompilerCallbacks({logLevel: 'silent'});
    const filename = makePathToFixture('invalid-projects', 'error_unsupported_project_version.kpj');
    const expectedMessages = [
      InfrastructureMessages.INFO_BuildingFile,
      InfrastructureMessages.ERROR_UnsupportedProjectVersion,
      InfrastructureMessages.INFO_ProjectNotBuiltSuccessfully
    ];
    await unitTestEndpoints.build(filename, null, ncb, {compilerWarningsAsErrors: true});
    assertMessagesEqual(ncb.messages, expectedMessages);
  });

  // HINT_ProjectIsVersion10

  it('should generate HINT_ProjectIsVersion10 if a project file is not version 2.0', async function() {
    const ncb = new NodeCompilerCallbacks({logLevel: 'silent'});
    const filename = makePathToFixture('kpj-1.0', 'hint_project_is_version_10.kpj');
    await unitTestEndpoints.build(filename, null, ncb, {});
    assert.isTrue(ncb.hasMessage(InfrastructureMessages.HINT_ProjectIsVersion10),
      'HINT_ProjectIsVersion10 not generated, instead got: '+JSON.stringify(ncb.messages,null,2));
  });
});

function assertMessagesEqual(actualMessages: CompilerEvent[], expectedMessages: number[]) {
  assert.deepEqual(actualMessages.map(m => m.code), expectedMessages,
    `actual callbacks.messages:\n${JSON.stringify(actualMessages,null,2)}\n\n`+
    `did not match expected:\n${JSON.stringify(expectedMessages,null,2)}\n\n`);
}

function nodeCompilerMessage(ncb: NodeCompilerCallbacks, code: number): string {
  return ncb.messages.find((item) => item.code == code).message ?? '';
}