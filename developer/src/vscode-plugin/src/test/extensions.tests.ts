/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import * as assert from 'assert';

import * as vscode from 'vscode';

// TODO-LDML-EDITOR: import extension
// import * as myExtension from '../../extension';

suite('Extension Test Suite', () => {
	vscode.window.showInformationMessage('Start all tests.');

	test('Sample test', () => {
		assert.strictEqual(-1, [1, 2, 3].indexOf(5));
		assert.strictEqual(-1, [1, 2, 3].indexOf(0));
	});
});
