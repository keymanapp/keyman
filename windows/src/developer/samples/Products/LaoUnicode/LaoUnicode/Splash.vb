Public Class SplashForm
    Friend ki As KeymanInterface.KeymanInterface

    Private Sub Label1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label1.Click

    End Sub

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        ki = New KeymanInterface.KeymanInterface
        If My.Application.CommandLineArgs.Contains("-c") Then
            Dim f As New ConfigForm
            f.ShowDialog()
            f.Dispose()
            Close()
        Else
            CheckActivatedState()
        End If
    End Sub

    Private Sub SplashHideTimer_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SplashHideTimer.Tick
        SplashHideTimer.Enabled = False
        ki.ActiveProduct.Start()
        Close()
    End Sub

    Private Sub ExitButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ExitButton.Click
        Close()
    End Sub

    Private Sub EvaluateButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles EvaluateButton.Click
        ki.ActiveProduct.Start()
        Close()
    End Sub

    Private Sub CheckActivatedState()
        Dim ActivatedState As Integer, TrialDayCount As Integer, DaysUsed As Integer

        ki.ActiveProduct.GetActivationData(ActivatedState, TrialDayCount, DaysUsed)

        If ActivatedState = 1 Then
            TrialDayLabel.Visible = False
            BuyButton.Visible = False
            ActivateButton.Visible = False
            EvaluateButton.Visible = False
            ExitButton.Visible = False
            SplashHideTimer.Enabled = True
        Else
            TrialDayLabel.Text = "You are on day " + DaysUsed.ToString + " of " + TrialDayCount.ToString
            If DaysUsed > TrialDayCount Then EvaluateButton.Enabled = False
        End If
    End Sub

    Private Sub ActivateButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ActivateButton.Click
        Dim f As ActivateForm
        f = New ActivateForm
        f.ShowDialog()

        If f.DialogResult = Windows.Forms.DialogResult.OK Then
            CheckActivatedState()
        End If
        f.Dispose()
    End Sub

    Private Sub BuyButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BuyButton.Click
        System.Diagnostics.Process.Start("https://secure.tavultesoft.com/buy/onlineproduct.php?OnlineProductID=1001")
    End Sub
End Class
