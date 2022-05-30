Var ExcelWindow, NewWorkBooks, WorkBook, Cell, Range, Border, WorkSheet,
    Diagram, MyDiagram : Variant;
    Line, Column, LineCount, ColumnCount : Integer;
begin
ExcelWindow := CreateOleObject('Excel.Application');
// ������� ���� Excel ������� � ��������
ExcelWindow.Visible := 1;
// ������������� ��� ���� Excel
ExcelWindow.Caption := '�����';
// �������� ����� ������� �����
NewWorkBooks := ExcelWindow.Workbooks;
WorkBook := NewWorkBooks.Add;
LineCount := 10;
ColumnCount := 5;
// ��������� �������� �����
For Line := 1 to LineCount do
begin
   Cell := ExcelWindow.Cells(Line + 1, 1);
   Cell.Value := '������ ' + Line;
end;
// ��������� �������� ��������
for column := 1 to ColumnCount do
begin
   Cell := ExcelWindow.Cells(1, Column + 1);
   Cell.Value := '������� ' + Column;
end;
// �������� ������ ������� ����������
for Line := 1 to LineCount do
   for Column := 1 to ColumnCount do
   begin
      Cell := ExcelWindow.Cells(Line + 1, Column + 1);
      Cell.Value := Line + Column;
   end;

// ������� ������� � ������� � �������� �� ���������� �����
Range := ExcelWindow.Range(ExcelWindow.Cells(1, 1),
                          ExcelWindow.Cells(LineCount + 1, ColumnCount + 1));
// ������� ��� ���������� �������
Range.Name := '�������������';
// ��������� ����� ���������� ������� � �������� �� ���������� �����
Border := Range.Borders;
// ��������� ����� ��� ����� ���������� �������
Border.LineStyle := 1;
Border.ColorIndex := 3;
// �������� ��������� 
WorkSheet := WorkBook.Worksheets(1);
Diagram := WorkSheet.ChartObjects;
Diagram := Diagram.Add(5, 5 + Range.Top + Range.Height,
                          Range.Width, Range.Height);
MyDiagram := Diagram.Chart;
MyDiagram.ChartWizard('������������� ', -4102, 6, 1, 1, 1, 1, '�����');
end;
