/*
 * Keyman is copyright (C) SIL International. MIT License.
 * Implementation of the FontSelect Class
 */

#include "framework.h"
#include "FontSelect.h"
#include "commdlg.h"

font_select::font_select()
{
  // set default values
  default_font();
}

font_select::font_select(HFONT current_font)
{

	if (m_hfont != NULL) {
		::DeleteObject(m_hfont);
  }

	// store the font current font for the calling application
	m_hfont = current_font;
	default_font(); // We still set the default value but can now drop back to the current font
}

font_select::~font_select()
{
	if (m_hfont != NULL) {
		::DeleteObject(m_hfont);
  }
}

// set the logical font information as "Charsis SIL" size 14
void font_select::default_font()
{
	// Define the default font for the editor
	// Future enhancement could load default from a configuration file
	LOGFONT log_font;
	log_font.lfHeight = -19; // size 14
	log_font.lfWidth = 0;
	log_font.lfEscapement = 0;
	log_font.lfOrientation = 0;
	log_font.lfWeight = FW_NORMAL;
	log_font.lfItalic = 0;
	log_font.lfUnderline = 0;
	log_font.lfStrikeOut = 0;
	log_font.lfCharSet = 0;
	log_font.lfOutPrecision = OUT_STRING_PRECIS;
	log_font.lfClipPrecision = CLIP_STROKE_PRECIS;
	log_font.lfQuality = DEFAULT_QUALITY;
	log_font.lfPitchAndFamily = FF_SWISS | VARIABLE_PITCH;
	wcscpy_s(log_font.lfFaceName, L"Charsis SIL");
	// create the associated font
	create_font(&log_font);
}

BOOL font_select::choose_font()
{
	CHOOSEFONT  choose_font;
	LOGFONT log_font;

	if (m_hfont != NULL){
		GetObject(m_hfont, sizeof(log_font), &log_font);
	}
	// fill in the data needed for the Windows common font dialog
	choose_font.lStructSize = sizeof(choose_font);
	choose_font.hwndOwner = ::GetActiveWindow();
	choose_font.hDC = 0;
	choose_font.lpLogFont = &log_font;
	choose_font.iPointSize = 0;
	choose_font.Flags = CF_SCREENFONTS | CF_INITTOLOGFONTSTRUCT;
	choose_font.rgbColors = 0;
	choose_font.lCustData = 0;
	choose_font.lpfnHook = 0;
	choose_font.lpTemplateName = NULL;
	choose_font.hInstance = 0;
	choose_font.lpszStyle = 0;
	choose_font.nFontType = SCREEN_FONTTYPE;
	choose_font.nSizeMin = 0;
	choose_font.nSizeMax = 0;

	// get new font selection from user
	if (::ChooseFont(&choose_font) != FALSE){
		if (NULL != create_font(&log_font)){
			return TRUE;
		}
	}
	// else
	return FALSE;
}

// create the associated font
HFONT font_select::create_font(LOGFONT* logfont)
{
	HFONT h_font = ::CreateFontIndirect(logfont);
	if (h_font == NULL){
		MessageBox(NULL,L"Error",L"Failed to select font\n",MB_OK);
	}

	if (m_hfont != NULL) {
		::DeleteObject(m_hfont);
  }
	// store the font (or NULL if failed)
	m_hfont = h_font;
	return h_font;
}

// return the logical font description for the wrapped font
LOGFONT font_select::get_log_font()
{
	LOGFONT log_font;

	if (m_hfont != NULL) {
		GetObject(m_hfont, sizeof(log_font), &log_font);
	}
	return log_font;
}

// return the wrapped font
HFONT font_select::get_font_handle()
{
	return m_hfont;
}

void font_select::save()
{

}

void font_select::restore()
{

}
