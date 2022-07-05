Public Class ConfigForm

    Private Sub ConfigForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Dim Reg As Microsoft.Win32.RegistryKey, s As String
        Reg = My.Computer.Registry.CurrentUser.OpenSubKey("Software\Microsoft\Windows\CurrentVersion\Run", False)
        s = Reg.GetValue("LaoUnicode.exe", "")
        Reg.Close()

        ' We won't worry about correct path comparisons here
        StartWithWindowsCheckbox.Checked = s.Equals(Process.GetCurrentProcess().MainModule.FileName, StringComparison.InvariantCultureIgnoreCase)
    End Sub

    Private Sub OKButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles OKButton.Click
        Dim Reg As Microsoft.Win32.RegistryKey
        Reg = My.Computer.Registry.CurrentUser.OpenSubKey("Software\Microsoft\Windows\CurrentVersion\Run", False)

        If StartWithWindowsCheckbox.Checked Then
            Reg.SetValue("LaoUnicode", Process.GetCurrentProcess().MainModule.FileName)
        Else
            Reg.DeleteValue("LaoUnicode", False)
        End If

        Reg.Close()

        DialogResult = Windows.Forms.DialogResult.OK
    End Sub
End Class