# check-markdown

This tool is used to test the validity of internal links within product
documentation, e.g. `/android/docs/help/**/*.md`.

It currently tests that:

1. Markdown can be parsed
2. Links to other files in the same section exist (with or without .md extension)
3. Images exist

It will also optionally report on:

1. External absolute links (starting with http/https)
2. Relative links outside the root of the help documentation
3. Unnecessary use of .md extension in links

We could extend it to include:

1. Checks for anchor validity
