/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import * as assert from 'assert';
import { fixupIndexHtml, fixupUrl } from '../indexFixer.js';

suite('indexFixer Test Suite', () => {

	test('fixupUrl test', () => {
		const s = '/index.BA426DDA.css';
		const buildUri = 'file://foo/bar/baz/build';
		const u = fixupUrl(s, buildUri);
		assert.equal(u, 'file://foo/bar/baz/build/index.BA426DDA.css');
	});

	test('fixup HTML test', () => {
		const s = `<!doctype html>
<html lang="en">

<head>
    <meta charset="utf-8" />
    <title>LDML Editor</title>
    <script type="module" src="/index.js" nonce="@NONCE@"></script>
    <link href="/App1234.css" />
    <meta http-equiv="Content-Security-Policy" content="default-src 'none'; img-src @CSPSOURCE@; style-src @CSPSOURCE@; script-src 'nonce-@NONCE@';">
</head>

<body>
    <div id="root" />
</body>

</html>`;
		// the HTMl gets normalized a little bit
		const expect = `<html lang="en"><head>
    <meta charset="utf-8">
    <title>LDML Editor</title>
    <script type="module" src="file://foo/bar/baz/build/index.js" nonce="00-00-00"></script>
    <link href="file://foo/bar/baz/build/App1234.css">
    <meta http-equiv="Content-Security-Policy" content="default-src 'none'; img-src C-S-P-SOURCE; style-src C-S-P-SOURCE; script-src 'nonce-00-00-00';">
</head>

<body>
    <div id="root">


</div></body></html>`;
		const buildUri = 'file://foo/bar/baz/build';
		const cspSource = 'C-S-P-SOURCE';
		const nonce = '00-00-00';
		const actual = fixupIndexHtml(s, buildUri, nonce, cspSource);
		assert.equal(actual, expect);
	});
});
