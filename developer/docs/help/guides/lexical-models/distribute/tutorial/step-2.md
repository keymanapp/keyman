---
title: Step 2: Editing .htm files for the package
---

When Keyman Developer created your lexical model project, it will have
created some of these files to go in the package. You will still need to
edit some of these templated files for the package.

readme.htm

: A short description of the package, its use restrictions, and what
  it includes. Try to keep the readme under 10 lines long. The readme
  should be an html file for optimal formatting.

  The intention of readme.htm is to describe why a user would want to
  install the lexical model.

  Create the HTML file in any HTML editor.

  > ### Tip
  If using Microsoft Word, choose HTML (clean) when saving to create a
  smaller file.

  Note, if your HTML editor puts images into a subfolder, you will
  need to edit the HTML source so that all files are in the same
  folder -- the package builder will not maintain subfolders. You can
  easily edit the HTML source in Keyman Developer.

  Also, if your welcome or readme files use embedded external images,
  stylesheets, javascript or other files, you will need to add these
  files to your package as well.

welcome.htm

: When including an introductory help file in your package, you must
  keep the name of the file "welcome.htm". This file will be displayed
  before the lexical model is installed. Make sure that you design
  your HTML file so that it can be viewed on a mobile device - avoid
  extra wide tables or wide fixed width elements.

  After package installation, the welcome.htm will also be accessible
  from the lexical model info pages.

  The intention of welcome.htm is to provide instructions on getting
  started with your lexical model.

  > ### Tip
  Useful information to include in welcome.htm is:
  -   names of languages associated with the lexical model.
  -   name of the lexical model in the package.
  -   links to additional help on your website or more extensive
      documentation files in the package.
  -   a link to an official distribution site for your package (even
      if it is the Keyman website) - so that users know where to find
      the latest version of your package. 

  <!-- This is here to divide the Tip sections in two. -->
    
  > ### Tip
  If you want links to your website to open in the user's preferred
  browser, preface the href link with `link:`, e.g.
  `<a href="link:http://keyman.com/">website</a>`  
  The `link:` sceheme will open the referred file in the default
  application - that is, a web browser for URLs and links, Notepad for
  .txt files, Adobe Reader for PDFs. You can use `link:` to open any
  of the files in the package, e.g. `link:docs.pdf` will open the file
  docs.pdf in Adobe Reader or the default PDF reader on the system.

  If you create documents in other formats, for example PDF or
  printable documentation, you should link to that in the welcome.htm.

[Step 3: Checking a package and adding files](step-3)