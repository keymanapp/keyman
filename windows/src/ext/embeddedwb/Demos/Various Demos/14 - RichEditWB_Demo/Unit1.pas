//****************************************************
//                   EwbRichEdit Demo                *
//                                                   *
//                                                   *
//  By:                                              *
//  Eran Bodankin (bsalsa)                           *
//  bsalsa@bsalsa.com                                *
//                                                   *
// Documentation and updated versions:               *
//               http://www.bsalsa.com               *
//****************************************************

{*******************************************************************************}
{LICENSE:
THIS SOFTWARE IS PROVIDED TO YOU "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED INCLUDING BUT NOT LIMITED TO THE APPLIED
WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
YOU ASSUME THE ENTIRE RISK AS TO THE ACCURACY AND THE USE OF THE SOFTWARE
AND ALL OTHER RISK ARISING OUT OF THE USE OR PERFORMANCE OF THIS SOFTWARE
AND DocUMENTATION. BSALSA PRODUCTIONS DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SystemS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SystemS. BSALSA PRODUCTIONS SPECIFICALLY
DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use, change or modify the component under 4 conditions:
1. In your website, add a Link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@bsalsa.com) any code change in the unit
   for the benefit of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}

//I used 2 method to get the info :
//1. StringList (So you can saveToFile)
//2. String so you use it simple Add procedure
// Both of them do the job for every parsed item.

unit Unit1;

interface

uses
   SysUtils, Classes, Graphics, Forms, Dialogs, Menus, RichEditBrowser,
   EmbeddedWB, ImgList, Controls, ComCtrls, StdCtrls, OleCtrls, ExtCtrls,
   SHDocVw_EWB, EwbCore;

type
   TForm2 = class(TForm)
      Panel1: TPanel;
      Panel2: TPanel;
      StatusBar1: TStatusBar;
      MainMenu1: TMainMenu;
      Fonts1: TMenuItem;
      SelectFonts1: TMenuItem;
      SetFontColor1: TMenuItem;
      SetSize1: TMenuItem;
      SetBold1: TMenuItem;
      SetItalic1: TMenuItem;
      SetUnderLine1: TMenuItem;
      AddBackroundColor1: TMenuItem;
      ResetFontsFormat1: TMenuItem;
      File1: TMenuItem;
      New1: TMenuItem;
      Open3: TMenuItem;
      Save1: TMenuItem;
      SaveAs2: TMenuItem;
      N20: TMenuItem;
      Print2: TMenuItem;
      Tools1: TMenuItem;
      CreateASnapshot1: TMenuItem;
      PreviewRichEditLinesInTheBrowser1: TMenuItem;
      LoadCodeFromBrowserStream1: TMenuItem;
      N21: TMenuItem;
      MailSelectedText1: TMenuItem;
      Mail1: TMenuItem;
      N22: TMenuItem;
      HighlighHTML1: TMenuItem;
      HighLightXML1: TMenuItem;
      HighLightURL1: TMenuItem;
      N23: TMenuItem;
      SetColor1: TMenuItem;
      Themes1: TMenuItem;
      Edit1: TMenuItem;
      Find1: TMenuItem;
      Replace1: TMenuItem;
      GoToLineNumber1: TMenuItem;
      SetSelectionAsAHyperLink1: TMenuItem;
      SetWordAsAHyperLink1: TMenuItem;
      WrapLongLines1: TMenuItem;
      Add1: TMenuItem;
      InsertFromImageList1: TMenuItem;
      InsertFile1: TMenuItem;
      InsertBitmap1: TMenuItem;
      N24: TMenuItem;
      AddDateAndTime1: TMenuItem;
      N25: TMenuItem;
      AddBullets1: TMenuItem;
      AddRomanNumbers1: TMenuItem;
      AddLineNumbers1: TMenuItem;
      N26: TMenuItem;
      AddAButton1: TMenuItem;
      AddTEditBox1: TMenuItem;
      AddARadioButton1: TMenuItem;
      AddACheckBox1: TMenuItem;
      OpenDialog1: TOpenDialog;
      RichEditWB1: TRichEditWB;
      ilsSmilies: TImageList;
      Demo: TMenuItem;
      N1: TMenuItem;
      Exit1: TMenuItem;
      EmbeddedWB1: TEmbeddedWB;
      StatusBar2: TStatusBar;
      N2: TMenuItem;
      DesignMode: TMenuItem;
      procedure EmbeddedWB1CommandStateChange(ASender: TObject; Command: Integer;
         Enable: WordBool);
      procedure DesignModeClick(Sender: TObject);
      procedure EmbeddedWB1StatusTextChange(ASender: TObject;
         const Text: WideString);
      procedure Exit1Click(Sender: TObject);
      procedure FormShow(Sender: TObject);
      procedure Themes1Click(Sender: TObject);
      procedure SetColor1Click(Sender: TObject);
      procedure HighLightURL1Click(Sender: TObject);
      procedure HighLightXML1Click(Sender: TObject);
      procedure HighlighHTML1Click(Sender: TObject);
      procedure Mail1Click(Sender: TObject);
      procedure MailSelectedText1Click(Sender: TObject);
      procedure LoadCodeFromBrowserStream1Click(Sender: TObject);
      procedure PreviewRichEditLinesInTheBrowser1Click(Sender: TObject);
      procedure CreateASnapshot1Click(Sender: TObject);
      procedure AddARadioButton1Click(Sender: TObject);
      procedure AddACheckBox1Click(Sender: TObject);
      procedure AddTEditBox1Click(Sender: TObject);
      procedure AddAButton1Click(Sender: TObject);
      procedure AddLineNumbers1Click(Sender: TObject);
      procedure AddRomanNumbers1Click(Sender: TObject);
      procedure AddBullets1Click(Sender: TObject);
      procedure InsertBitmap1Click(Sender: TObject);
      procedure InsertFile1Click(Sender: TObject);
      procedure InsertFromImageList1Click(Sender: TObject);
      procedure SetWordAsAHyperLink1Click(Sender: TObject);
      procedure SetSelectionAsAHyperLink1Click(Sender: TObject);
      procedure Replace1Click(Sender: TObject);
      procedure GoToLineNumber1Click(Sender: TObject);
      procedure Find1Click(Sender: TObject);
      procedure ResetFontsFormat1Click(Sender: TObject);
      procedure AddBackroundColor1Click(Sender: TObject);
      procedure AddDateAndTime1Click(Sender: TObject);
      procedure SetUnderLine1Click(Sender: TObject);
      procedure SetSize1Click(Sender: TObject);
      procedure SetBold1Click(Sender: TObject);
      procedure SetItalic1Click(Sender: TObject);
      procedure SetFontColor1Click(Sender: TObject);
      procedure SelectFonts1Click(Sender: TObject);
      procedure Print2Click(Sender: TObject);
      procedure Save1Click(Sender: TObject);
      procedure SaveAs2Click(Sender: TObject);
      procedure Open3Click(Sender: TObject);
      procedure New1Click(Sender: TObject);
   private
    { Private declarations }
      procedure RunDemo();
   public
    { Public declarations }
   end;

var
   Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.RunDemo();
begin
   RichEditWB1.AddBitmapFromImagelist(ilsSmilies, 0);
   RichEditWB1.AddEmptyLine;
   RichEditWB1.SelAttributes.Color := clYellow;
   RichEditWB1.AddEmptyLine;
   RichEditWB1.SelText := ' Beatles ';
   RichEditWB1.AddEmptyLine;
   RichEditWB1.AddBitmapFromImagelist(ilsSmilies, 15);
   RichEditWB1.AddEmptyLine;
   RichEditWB1.SelAttributes.Color := clPurple;
   RichEditWB1.AddEmptyLine;
   RichEditWB1.SelText := ' Deep Purple ';
   RichEditWB1.AddEmptyLine;
   RichEditWB1.AddBitmapFromImagelist(ilsSmilies, 5);
   RichEditWB1.AddEmptyLine;
   RichEditWB1.AddFormatedText('Led Zepelin', true, true, true, true, clred);
   RichEditWB1.AddEmptyLine;
   RichEditWB1.AddEmptyLine;
   RichEditWB1.AddFormatedText('C U L8r', True, false, false, false, clGreen);
   RichEditWB1.AddEmptyLine;
   RichEditWB1.RemoveTextFormats;
end;

procedure TForm2.New1Click(Sender: TObject);
begin
   RichEditWB1.New;
end;

procedure TForm2.Open3Click(Sender: TObject);
begin
   RichEditWB1.Open;
end;

procedure TForm2.SaveAs2Click(Sender: TObject);
begin
   RichEditWB1.SaveAs;
end;

procedure TForm2.Save1Click(Sender: TObject);
begin
   RichEditWB1.Save;
end;

procedure TForm2.Print2Click(Sender: TObject);
begin
   RichEditWB1.PrintAll;
end;

procedure TForm2.SelectFonts1Click(Sender: TObject);
begin
   RichEditWB1.SelectFont;
end;

procedure TForm2.SetFontColor1Click(Sender: TObject);
begin
   RichEditWB1.SetFontColor;
end;

procedure TForm2.SetItalic1Click(Sender: TObject);
begin
   RichEditWB1.SetFontItalic;
end;

procedure TForm2.SetBold1Click(Sender: TObject);
begin
   RichEditWB1.SetFontBold;
end;

procedure TForm2.SetSize1Click(Sender: TObject);
begin
   RichEditWB1.SetFontSize;
end;

procedure TForm2.SetUnderLine1Click(Sender: TObject);
begin
   RichEditWB1.SetFontUnderLine;
end;

procedure TForm2.AddDateAndTime1Click(Sender: TObject);
begin
   RichEditWB1.AddDateAndTime;
end;

procedure TForm2.AddBackroundColor1Click(Sender: TObject);
begin
   RichEditWB1.SetSelectedBgColor;
end;

procedure TForm2.ResetFontsFormat1Click(Sender: TObject);
begin
   RichEditWB1.RemoveTextFormats;
end;

procedure TForm2.Find1Click(Sender: TObject);
begin
   RichEditWB1.Find;
end;

procedure TForm2.GoToLineNumber1Click(Sender: TObject);
var
   Value: string;
begin
   InputQuery('Select A Line', 'Please Enter a line number to select..', Value);
   RichEditWB1.SelectLine(StrToInt(Trim(Value)));
end;

procedure TForm2.Replace1Click(Sender: TObject);
begin
   RichEditWB1.Replace;
end;

procedure TForm2.SetSelectionAsAHyperLink1Click(Sender: TObject);
begin
   RichEditWB1.SetSelectionHyperlink(true);
end;

procedure TForm2.SetWordAsAHyperLink1Click(Sender: TObject);
begin
   RichEditWB1.SetWordHyperlink(true);
end;

procedure TForm2.InsertFromImageList1Click(Sender: TObject);
begin
   RunDemo();
end;

procedure TForm2.InsertFile1Click(Sender: TObject);
begin
   if OpenDialog1.Execute then
      RichEditWB1.AddFile(OpenDialog1.FileName, true, true);
end;

procedure TForm2.InsertBitmap1Click(Sender: TObject);
begin
   if OpenDialog1.Execute then
      RichEditWB1.AddFile(OpenDialog1.FileName, false, false);
end;

procedure TForm2.AddBullets1Click(Sender: TObject);
begin
   RichEditWB1.AddBullets;
end;

procedure TForm2.AddRomanNumbers1Click(Sender: TObject);
begin
   RichEditWB1.AddRomanNumbering;
end;

procedure TForm2.AddLineNumbers1Click(Sender: TObject);
begin
   RichEditWB1.AddLineNumbering;
end;

procedure TForm2.AddAButton1Click(Sender: TObject);
begin
   RichEditWB1.AddButton('bsalsa', 'btn', 20, 30, 130);
end;

procedure TForm2.AddTEditBox1Click(Sender: TObject);
begin
   RichEditWB1.AddEditBox('bsalsa', 'edt', 20, 30, 80);
end;

procedure TForm2.AddACheckBox1Click(Sender: TObject);
begin
   RichEditWB1.AddCheckBox(' bsalsa', 'cb', 20, 30, 170, true);
end;

procedure TForm2.AddARadioButton1Click(Sender: TObject);
begin
   RichEditWB1.AddRadioButton('bsalsa', 'rb', 20, 30, 40, true);
end;

procedure TForm2.CreateASnapshot1Click(Sender: TObject);
var
   image1: TImage;
begin
   Image1 := Timage.Create(self);
   RichEditWB1.CreateSnapShot(Image1.Picture.Bitmap);
   Image1.Picture.SaveToFile('Image1');
   image1.free;
end;

procedure TForm2.DesignModeClick(Sender: TObject);
begin
   if EmbeddedWB1.DesignMode then
      Statusbar2.Panels[1].Text := 'Design Mode';
end;

procedure TForm2.PreviewRichEditLinesInTheBrowser1Click(Sender: TObject);
begin
   RichEditWB1.PreviewInBrowser;
end;

procedure TForm2.LoadCodeFromBrowserStream1Click(Sender: TObject);
begin
   RichEditWB1.LoadStreamFromBrowser;
end;

procedure TForm2.MailSelectedText1Click(Sender: TObject);
begin
   RichEditWB1.MailSelected;
end;

procedure TForm2.Mail1Click(Sender: TObject);
begin
   RichEditWB1.MailContext;
end;

procedure TForm2.HighlighHTML1Click(Sender: TObject);
begin
   RichEditWB1.LoadHTMLFromBrowser;
   if RichEditWB1.HighlightHTML then
      RichEditWB1.DoHighlightHTML;
   RichEditWB1.ScrollToTop;
end;

procedure TForm2.HighLightXML1Click(Sender: TObject);
begin
   RichEditWB1.HighLightURL := false;
   EmbeddedWB1.Navigate('http://rss.groups.yahoo.com/group/delphi-webbrowser/rss');
   RichEditWB1.LoadAsCopyFromBrowser;
   if RichEditWB1.HighlightXML then
      RichEditWB1.DoHighlightXML;
end;

procedure TForm2.HighLightURL1Click(Sender: TObject);
begin
   RichEditWB1.HighLightURL := True;
   EmbeddedWB1.ViewPageLinksToStrings(RichEditWB1.Lines);
   RichEditWB1.AddLineNumbering;
end;

procedure TForm2.SetColor1Click(Sender: TObject);
begin
   RichEditWB1.SetColor;
end;

procedure TForm2.Themes1Click(Sender: TObject);
begin
   RichEditWB1.SetThemes(tBlack);
end;

procedure TForm2.FormShow(Sender: TObject);
begin
   EmbeddedWB1.Navigate('bsalsa.com');
end;

procedure TForm2.EmbeddedWB1CommandStateChange(ASender: TObject;
   Command: Integer; Enable: WordBool);
begin
   if EmbeddedWB1.Modified then
      StatusBar2.Panels[2].Text := 'Modified';
end;

procedure TForm2.EmbeddedWB1StatusTextChange(ASender: TObject;
   const Text: WideString);
begin
   StatusBar2.Panels[0].Text := Text;
end;

procedure TForm2.Exit1Click(Sender: TObject);
begin
   Close;
end;

end.

