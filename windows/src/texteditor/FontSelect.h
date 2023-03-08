/*
 * Keyman is copyright (C) SIL International. MIT License.
 * Description: A light way wrapper class for selecting fonts
 */

#pragma once
#ifndef _FONTSELECT_H
#define _FONTSELECT_H
#include "framework.h"

/**
 * This class exists as simple class for allowing a user to 
 * select a font making use of the common dialog box
 * 
 */
class font_select {

private:
  HFONT m_hfont;

public:
  font_select();
  /**
   * Construct a new font select object the caller can
   * set the current font to fallback.
   * 
   * @param current_font  Font used for fallback should configured default not be
   *                      fond on the system.   
   */
  font_select(HFONT current_font);
  ~font_select();

  /**
   * Set the default font for our editor. Currently hard coded to Charsis SIL
   * but could be upgraded to read from a configuration file, when implementing
   * save and restore methods.
   */
  void default_font();
  /**
   * Create the HFONT for the current logical font of the private member m_log_font
   * 
   * @return HFONT 
   */
  HFONT create_font(LOGFONT* logfont);
 
  /**
   * Load the Font dialog box
   * 
   * @return BOOL 
   */
  BOOL choose_font();
  
  /**
   * Get the logical font object
   * 
   * @param log_font 
   * @return LOGFONT 
   */
  LOGFONT get_log_font();
  
  /**
   * Get the font handle object
   * 
   * @return HFONT 
   */
  HFONT get_font_handle();
  // TODO: Save font to disk for restoring upon editor restart
  
  /**
   * persist the current font to disk
   */
  void save();
  
  /**
   * restore font from to disk
   */
  void restore();
};


#endif