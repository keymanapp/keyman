package com.tavultesoft.kmea.util;

public class INIParseException extends RuntimeException {

  /**
   * For serialization.  (Prevents warnings.)
   */
  private static final long serialVersionUID = -7442561465081434855L;

  public INIParseException() {
  }

  public INIParseException(String message) {
    super(message);
  }
}
