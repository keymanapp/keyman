/*
 * TextEditor is copyright (C) SIL International. MIT License.
 * Implementation of the FontSelect Class
 */

#include "FontSelect.h"
#include "commdlg.h"
#include "framework.h"

font_select::font_select() {
  default_font();
}

font_select::font_select(HFONT current_font) {

  // we need to clone this HFONT
  LOGFONT log_font;
  if (current_font != NULL) {
    GetObject(current_font, sizeof(log_font), &log_font);
  }
  HFONT h_font = ::CreateFontIndirect(&log_font);
  if (h_font == NULL) {
    MessageBox(NULL, L"Warning", L"Failed to create font at intialisation\n", MB_OK);
  }
  m_hfont = h_font; // even if h_font is NULL

  // We still set the default value but can now drop back to the current font.
  // That is, if the system does not have that font installed we want the m_hfont
  // to be the font we passed into the constructor
  default_font();
}

font_select::~font_select() {
  if (m_hfont != NULL) {
    ::DeleteObject(m_hfont);
  }
}

void
font_select::default_font() {
  // Set the logical font information as "Charis SIL" size 14
  // A future enhancement could load default from a configuration file
  LOGFONT log_font;
  log_font.lfHeight         = -19;  // size 14
  log_font.lfWidth          = 0;
  log_font.lfEscapement     = 0;
  log_font.lfOrientation    = 0;
  log_font.lfWeight         = FW_NORMAL;
  log_font.lfItalic         = 0;
  log_font.lfUnderline      = 0;
  log_font.lfStrikeOut      = 0;
  log_font.lfCharSet        = 0;
  log_font.lfOutPrecision   = OUT_STRING_PRECIS;
  log_font.lfClipPrecision  = CLIP_STROKE_PRECIS;
  log_font.lfQuality        = DEFAULT_QUALITY;
  log_font.lfPitchAndFamily = FF_SWISS | VARIABLE_PITCH;
  wcscpy_s(log_font.lfFaceName, L"Charis SIL");

  if (set_font(&log_font) == NULL) {
    MessageBox(NULL, L"Error", L"Failed to set default font.\n", MB_OK);
  }
}

BOOL
font_select::choose_font() {
  CHOOSEFONT choose_font = {0};
  LOGFONT log_font;

  if (m_hfont != NULL) {
    GetObject(m_hfont, sizeof(log_font), &log_font);
  }
  // fill in the data needed for the Windows common font dialog
  choose_font.lStructSize = sizeof(choose_font);
  choose_font.hwndOwner   = ::GetActiveWindow();
  choose_font.lpLogFont   = &log_font;
  choose_font.Flags       = CF_SCREENFONTS | CF_INITTOLOGFONTSTRUCT;
  choose_font.nFontType   = SCREEN_FONTTYPE;

  // get new font selection from user
  if (::ChooseFont(&choose_font) != FALSE) {
    if (set_font(&log_font) != NULL) {
      return TRUE;
    }
  }
  // else
  return FALSE;
}

HFONT
font_select::set_font(LOGFONT* logfont) {
  HFONT h_font = ::CreateFontIndirect(logfont);
  if (h_font == NULL) {
    MessageBox(NULL, L"Error", L"Failed to create font while setting font.\n", MB_OK);
  } else {
    if (m_hfont != NULL) {
      ::DeleteObject(m_hfont);
    }
    m_hfont = h_font;
  }
  return h_font;
}

LOGFONT
font_select::get_log_font() {
  LOGFONT log_font = {0};

  if (m_hfont != NULL) {
    GetObject(m_hfont, sizeof(log_font), &log_font);
  }
  return log_font;
}

HFONT
font_select::get_font_handle() {
  return m_hfont;
}

void
font_select::save() {
}

void
font_select::restore() {
}
