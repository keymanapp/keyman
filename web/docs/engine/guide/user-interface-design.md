---
title: User Interface Design
---

This page displays all the different KeymanWeb user interface options
available for non-mobile consumption. Use whichever one best complements
your website. Depending on the number of keyboards you are using and the
way your website is designed, the various interfaces will be more or
less suitable.

<table border=0 cellpadding=12>
  <tr>
    <td colspan="2"><h2>The Button Interface</h2></td>
  </tr>
  <tr>
    <td valign="top">
  <p style='text-align: center; font-size: 12pt; font-weight: bold'>
  <img border=0 src="../../images/ui-button.gif" style='vertical-align:middle' />
  <br/>Button Interface</p>
  <div>
  <p style='color:#A94827'>Display:<span style='font-weight:bold'> Fixed Element</span></p>
  <p style='color:#A94827'>No of Keyboards:<span style='font-weight:bold'> up to 20</span></p>
  <p style='color:#A94827'>HTML:<span style='font-weight:bold'> Header &amp; Control</span></p>
  </div>
    </td>
    <td valign="top">
  <p>For an interface which fits unobtrusively into your website while maintaining full functionality, try the Button Interface.  All KeymanWeb's functions are displayed in a menu appearing whenever the user hovers over the button, which can be placed anywhere on the website.</p>
  <p>To keep this interface as compact as possible, it is controlled through a single menu.  While there is technically no limit to the number of keyboards that can be used, the Button interface works best on sites using up to 20 different keyboards, which are listed alphabetically.</p>
      <p>The include for this interface is <code>&lt;script src="kmwuibutton.js"&gt;</code>.</p>
      <p>To select where the interface will be displayed on your page, add the following element where desired:  <code>&lt;div id="KeymanWebControl" display="block"&gt;&lt;/div&gt;</code></p>
      <p>The property <code>display</code> may take two values:  <code>block</code> and <code>none</code>, the latter of which temporarily hides the element.</p>
      <p>For information on how your site users will interact with the Button Interface, <a href="../ui/ui-button">click here</a>.</p>
    </td>
  </tr>

  <tr>
    <td colspan="2"><h2>The Floating Interface</h2></td>
  </tr>
  <tr>
    <td valign="top">
  <p style='text-align: center; font-size: 12pt; font-weight: bold'>
  <img border=0 src="../../images/ui-floating.gif" style='vertical-align:middle' />
  <br/>Floating Interface</p>
  <div>
  <p style='color:#A94827'>Display:<span style='font-weight:bold'> Floating Element</span></p>
  <p style='color:#A94827'>No of Keyboards:<span style='font-weight:bold'> up to 20</span></p>
  <p style='color:#A94827'>HTML:<span style='font-weight:bold'> Header Only</span></p>
  </div>
    </td>
    <td valign="top">
  <p>The Floating Interface provides a clear, menu-based approach to selecting from a list of keyboards.  This interface can be incorporated without changing the layout of your existing site, as it automatically appears when users click in a a supported control (such as a textbox) and disappears again when they click outside.  You can choose to have the interface float below (DefaultBelow) or to the right of (DefaultRight) the active control.</p>
  <p>This interface is simple to use, and with its single alphabetical list of keyboards, works most effectively on sites supporting up to around 20 keyboards.</p>
      <p>The include for this interface is <code>&lt;script src="kmwuifloat.js"&gt;</code>.</p>
      <p>For information on how your site users will interact with the Floating Interface, <a href="../ui/ui-floating">click here</a>.</p>
    </td>
  </tr>

  <tr>
    <td colspan="2"><h2>The Toggle Interface</h2></td>
  </tr>
  <tr>
    <td valign="top">
  <p style='text-align: center; font-size: 12pt; font-weight: bold'>
  <img border=0 src="../../images/ui-toggle.gif" style='vertical-align:middle' />
  <br/>Toggle Interface</p>
  <div>
  <p style='color:#A94827'>Display:<span style='font-weight:bold'> Floating Element</span></p>
  <p style='color:#A94827'>No of Keyboards:<span style='font-weight:bold'> 1</span></p>
  <p style='color:#A94827'>HTML:<span style='font-weight:bold'> Header Only</span></p>
  </div>
    </td>
    <td valign="top">
  <p>For a website incorporating KeymanWeb to support a single keyboard, the Toggle is the ideal solution.  The interface floats at the right of the active control, and is hidden when the mouse is clicked outside, which means that no changes have to be made to your site design.  A simple on-off interface makes it easy for users to switch between their default language and the supported keyboard.</p>
      <p>The include for this interface is <code>&lt;script src="kmwuitoggle.js"&gt;</code>.</p>
      <p>For information on how your site users will interact with the Toggle Interface, <a href="../ui/ui-toggle">click here</a>.</p>
    </td>
  </tr>

  <tr>
    <td colspan="2"><h2>The Toolbar Interface</h2></td>
  </tr>
  <tr>
    <td valign="top">
  <p style='text-align: center; font-size: 12pt; font-weight: bold'>
  <img border=0 src="../../images/ui-toolbar.gif" style='vertical-align:middle' />
  <br/>Toolbar Interface</p>
  <div>
  <p style='color:#A94827'>Display:<span style='font-weight:bold'> Fixed Element</span></p>
  <p style='color:#A94827'>No of Keyboards:<span style='font-weight:bold'> unlimited</span></p>
  <p style='color:#A94827'>HTML:<span style='font-weight:bold'> Header &amp; Control</span></p>
  </div>
    </td>
    <td valign="top">
  <p>The standard interface for many sites incorporating KeymanWeb is the Toolbar.  This interface, which can be positioned anywhere on a website by inserting the HTML code provided, provides an instant visual indication that the site allows users to type in a range of languages.</p>
  <p>The Toolbar's map-based menu, displayed whenever a user clicks on the Keyboards button, works particularly well with multilingual sites integrating languages from all over the world.  The most recently-used keyboard is remembered in a cookie, making it as easy to switch between English and another language as the Toggle interface, while maintaining full functionality and support for an essentially unlimited number of keyboards.</p>
      <p>The include for this interface is <code>&lt;script src="kmwuitoolbar.js"&gt;</code>.</p>
      <p>To select where the interface will be displayed on your page, add the following element where desired:  <code>&lt;div id="KeymanWebControl" display="block"&gt;&lt;/div&gt;</code>, as with the Button Interface above.</p>
      <p>A live example of the KeymanWeb Toolbar Interface is available on the <a href="http://keymanweb.com">KeymanWeb Demo Site</a>.  For information on how your site users will interact with the Toolbar Interface, <a href="../ui/ui-toolbar">click here</a>.</p>

    </td>
  </tr>

</table>
