---
title: Step 2: Creating Additional Files for the Package
---

You should create or select some additional files for a package. Below
are a list of the typical files that you would create for or include in
a package.

readme.htm

:   A short description of the package, its use restrictions, and what
    it includes. Try to keep the readme under 10 lines long. The readme
    is displayed in the package install dialog and should be an html
    file for optimal formatting.

    > ### Tip
    As the package has not been installed at this stage, and you are using fonts that may not yet be available on the  target device, you should use CSS @font-face declarations to ensure that the text is readable on all devices.

    Create the HTML file in any HTML editor.

    > ### Tip
    If using Microsoft Word, choose HTML (clean) when saving to create a smaller file.

    Note, if your HTML editor puts images into a subfolder, you will need to edit the HTML source so that all files are in the same folder -- the package builder will not maintain subfolders. You can easily edit the HTML source in Keyman Developer.

    Also, if your welcome or readme files use embedded external images, stylesheets, javascript or other files, you will need to add these files to your package as well.

welcome.htm

:   When including an introductory help file in your package, you must
    name the file welcome.htm. This file will be detected during install
    and displayed (in a window roughly two thirds of the user's screen
    width) after the package install completes successfully. Make sure
    that you design your HTML file so that it can be resized to fit the
    user's screen - avoid extra wide tables or wide fixed width
    elements.

    At this stage of the installation, fonts in the package have been installed, so you can include text that uses those fonts.

    Welcome.htm will also be accessible after installation from Keyman Configuration under the Package Options menu, and from the Keyboard Help item and On Screen Keyboard toolbar button.

    The intention of welcome.htm is to provide instructions on getting started with your keyboards.

    > ### Tip
    Useful information to include in welcome.htm is:
    -   key sequences - especially for characters that may not be immediately obvious.
    -   names of fonts in the package.
    -   names of keyboards in the package.
    -   links to additional help on your website or more extensive documentation files in the package.
    -   a link to an official distribution site for your package (even
        if it is the Keyman website) - so that users know where to find
        the latest version of your package.

    You should avoid including instructions for the use of Keyman
    itself - although a basic "click the Keyman icon and choose Quick
    French" would be helpful.

    > ### Tip
    If you want links to your website to open in the user's preferred
    browser, preface the href link with `link:`, e.g.
    `<a href="https://keyman.com/">website</a>`  
    The `link:` sceheme will open the referred file in the default
    application - that is, a web browser for URLs and links, Notepad for
    .txt files, Adobe Reader for PDFs. You can use `link:` to open any
    of the files in the package, e.g. `link:docs.pdf` will open the file
    docs.pdf in Adobe Reader or the default PDF reader on the system.

    To save you the effort of writing a welcome and readme file for the
    Quick French example, we have placed some in the Samples/QFrench
    folder.

    If you create documents in other formats, for example PDF or
    printable documentation, you should link to that in the welcome.htm.

    > ### Tip
    You can include multiple "welcome.htm" files for different languages
    by appending a hyphen and the BCP 47 language code to the filename,
    for example welcome-fr.htm for French.

Fonts

:   A font is the single most important item to include with a keyboard
    – if the characters of the language you are supporting are not in
    fonts included with target operating systems. Installing fonts is
    tedious, so make sure that your users don't have to locate and
    install fonts themselves!

    .TTF, .OTF, and .TTC fonts will be installed by the package
    installer, and uninstalled when the package is uninstalled. A list
    of the fonts installed is displayed in the Install Package dialog.

Keyboards

:   Add the .kmx compiled Keyman keyboards to the package. You can add
    multiple keyboards to the package, but be judicious.

    > ### Tip
    Don't forget to add the On Screen keyboard .KVK file associated with
    your keyboard to the package. KVK files are not compiled into the
    .KMX file -- although the .ICO/.BMP is.

    Add the .js compiled Keyman touch layout keyboards to the package.

Documentation

:   The two preferred documentation formats are HTML and PDF. You should
    avoid .DOC, .RTF, and other formats -- .DOC files in particular are
    not recommended due to the possibility of macro viruses.

    Remember that HTML files can be displayed on any computer without
    additional software. PDF files require Adobe Reader or a compatible
    PDF viewer application. You may choose to include both and HTML
    documentation - PDF documents often print better than HTML
    documents, but HTML documentation is more accessible and translates
    better to on-screen or web use.

    > ### Tip
    If you use HTML, don't forget to also add all the included files
    such as images and stylesheets!

Splash Image
:   The splash image is a 140x250 pixel image that is displayed when the
    package is installed, at the left of the Package Install dialog.
    Including a splash image makes your package look more professional
    and polished, so a splash image is recommended!

[Step 3: Creating a package and adding files](step-3)