Public Class ActivateForm

    Private Sub ActivateButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ActivateButton.Click
        Dim arb As New String("")
        Try
            SplashForm.ki.ActiveProduct.GetActivationRequestCode(LicenceTextBox.Text, arb)
        Catch ex As Exception
            MsgBox("Invalid licence code: " + ex.Message)
            Exit Sub
        End Try
        Dim wc As New System.Net.WebClient
        Dim response As String = wc.DownloadString("https://secure.tavultesoft.com/prog/70/activation.php?LicenceNumber=" + LicenceTextBox.Text + "&ActivationRequestBlob=" + arb)
        If response.StartsWith("<error>") Then
            MsgBox(response)
        Else
            Try
                SplashForm.ki.ActiveProduct.Activate(response)
            Catch ex As Exception
                MsgBox("Unable to activate: " + ex.Message)
                Exit Sub
            End Try
            DialogResult = Windows.Forms.DialogResult.OK
        End If
    End Sub
End Class