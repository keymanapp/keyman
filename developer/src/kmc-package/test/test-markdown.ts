import { assert } from 'chai';
import 'mocha';
import { markdownToHTML } from '../src/compiler/markdown.js';

describe('markdownToHTML', function () {
  it('should convert markdown into HTML', function() {
    const html = markdownToHTML('# heading\n\n**bold** and _beautiful_', true);
    assert.equal(html, `<h1>heading</h1>\n<p><strong>bold</strong> and <em>beautiful</em></p>\n`);
  });

  it('should strip inline html if asked to do so', function() {
    const html = markdownToHTML(`# heading\n\n<script>alert('gotcha')</script>\n\n**bold** and _beautiful_`, false);
    assert.equal(html, `<h1>heading</h1>\n<p><strong>bold</strong> and <em>beautiful</em></p>\n`);
  });

  it('should keep inline html if asked to do so', function() {
    const html = markdownToHTML(`# heading\n\n<script>alert('gotcha')</script>\n\n**bold** and _beautiful_`, true);
    assert.equal(html, `<h1>heading</h1>\n<script>alert('gotcha')</script>\n\n<p><strong>bold</strong> and <em>beautiful</em></p>\n`);
  });
});
