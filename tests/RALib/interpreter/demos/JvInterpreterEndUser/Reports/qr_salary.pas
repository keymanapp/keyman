unit qr_salary;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, Quickrpt, QRCtrls, Db, DBTables;

type
  TrSalary = class(TForm)
    QuickRep1 : TQuickRep;
    PageFooterBand1 : TQRBand;
    QRSysData1 : TQRSysData;
    ColumnHeaderBand1 : TQRBand;
    DetailBand1 : TQRBand;
    QRLabel1 : TQRLabel;
    QRExpr1 : TQRExpr;
    QRLabel2 : TQRLabel;
    QRExpr2 : TQRExpr;
    QRLabel3 : TQRLabel;
    QRExpr3 : TQRExpr;
    QRLabel4 : TQRLabel;
    QRExpr4 : TQRExpr;
    QRLabel5 : TQRLabel;
    QRExpr5 : TQRExpr;
    QRLabel6 : TQRLabel;
    QRExpr6 : TQRExpr;
    lblTitle: TQRLabel;
    Query1: TQuery;
  private
  end;

var
  rSalary: TrSalary;

implementation

//uses fReports, Unit1;

{$R *.DFM}

end.
