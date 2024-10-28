import * as fs from 'fs';
import 'mocha';
import {assert} from 'chai';
import { makePathToFixture } from '../helpers/index.js';
import { KPJFileReader } from "../../src/types/kpj/kpj-file-reader.js";
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { KPJFileWriter } from '../../src/types/kpj/kpj-file-writer.js';
import { KeymanDeveloperProjectOptions } from '../../src/types/kpj/keyman-developer-project.js';

const callbacks = new TestCompilerCallbacks();

describe('kpj-file-writer', function () {
  it('kpj-file-writer should write a valid v1.0 file', async function() {
    const kpjPath = 'khmer_angkor.kpj';
    const path = makePathToFixture('kpj', kpjPath);
    const input = fs.readFileSync(path);
    const reader = new KPJFileReader(callbacks);
    const inputKpj = reader.read(input);
    reader.validate(inputKpj);
    const project = await reader.transform(kpjPath, inputKpj);

    const writer = new KPJFileWriter();
    const output = writer.write(project);
    const outputKpj = reader.read(new TextEncoder().encode(output));

    // The outputKpj may not contain all the fields from the inputKpj, only the
    // essential fields. Many of the fields in .kpj are deprecated, when they
    // relate to file content (e.g. parented files, file details)

    assert.deepEqual(outputKpj.KeymanDeveloperProject.Options, {
      "BuildPath": "$PROJECTPATH\\build",
      "CompilerWarningsAsErrors": "True",
      "ProjectType": "keyboard"
    });

    assert.deepEqual(outputKpj.KeymanDeveloperProject.Files.File, [
      {
        "FileType": ".kmn",
        "Filename": "khmer_angkor.kmn",
        "Filepath": "source\\khmer_angkor.kmn",
        "ID": "khmer_angkor.kmn"
      },
      {
        "FileType": ".kps",
        "Filename": "khmer_angkor.kps",
        "Filepath": "source\\khmer_angkor.kps",
        "ID": "khmer_angkor.kps"
      },
    ]);
  });

  it('kpj-file-writer should write a valid v2.0 file', async function() {
    const kpjPath = 'khmer_angkor.kpj';
    const path = makePathToFixture('kpj', kpjPath);
    const input = fs.readFileSync(path);
    const reader = new KPJFileReader(callbacks);
    const inputKpj = reader.read(input);
    reader.validate(inputKpj);
    const project = await reader.transform(kpjPath, inputKpj);
    project.options = new KeymanDeveloperProjectOptions('2.0');

    const writer = new KPJFileWriter();
    const output = writer.write(project);
    const outputKpj = reader.read(new TextEncoder().encode(output));

    // The outputKpj may not contain all the fields from the inputKpj, only the
    // essential fields. Many of the fields in .kpj are deprecated, when they
    // relate to file content (e.g. parented files, file details)

    assert.deepEqual(outputKpj.KeymanDeveloperProject.Options, {
      "ProjectType": "keyboard",
      "Version": "2.0",
    });

    // reader.read adds an empty Files.File array
    assert.isEmpty(outputKpj.KeymanDeveloperProject.Files.File);
  });

});
