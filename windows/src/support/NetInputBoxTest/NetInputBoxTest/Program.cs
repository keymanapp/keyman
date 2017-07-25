/*
  Name:             Program
  Copyright:        Copyright (C) 2003-2012 Software For Specialists Ltd.
  Documentation:    
  Description:      
  Create Date:      29 Mar 2015

  Modified Date:    29 Mar 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          29 Mar 2015 - mcdurdin - I4642 - V9.0 - In Logos, generated backspace receives a scan of 0x00 instead of 0xFF
*/
using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;

namespace NetInputBoxTest
{
  static class Program
  {
    /// <summary>
    /// The main entry point for the application.
    /// </summary>
    [STAThread]
    static void Main()
    {
      Application.EnableVisualStyles();
      Application.SetCompatibleTextRenderingDefault(false);
      Application.Run(new Form1());
    }
  }
}
