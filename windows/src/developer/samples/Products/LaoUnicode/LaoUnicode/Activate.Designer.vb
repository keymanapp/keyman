<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class ActivateForm
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then
            components.Dispose()
        End If
        MyBase.Dispose(disposing)
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.ActivateLabel = New System.Windows.Forms.Label
        Me.LicenceTextBox = New System.Windows.Forms.MaskedTextBox
        Me.ActivateButton = New System.Windows.Forms.Button
        Me.FCancelButton = New System.Windows.Forms.Button
        Me.SuspendLayout()
        '
        'ActivateLabel
        '
        Me.ActivateLabel.AutoSize = True
        Me.ActivateLabel.Location = New System.Drawing.Point(15, 22)
        Me.ActivateLabel.Name = "ActivateLabel"
        Me.ActivateLabel.Size = New System.Drawing.Size(180, 13)
        Me.ActivateLabel.TabIndex = 0
        Me.ActivateLabel.Text = "Please enter your licence key below:"
        '
        'LicenceTextBox
        '
        Me.LicenceTextBox.AllowPromptAsInput = False
        Me.LicenceTextBox.HidePromptOnLeave = True
        Me.LicenceTextBox.Location = New System.Drawing.Point(18, 48)
        Me.LicenceTextBox.Mask = "AAAA-AAAA-AAAA-AAAA-AAAA"
        Me.LicenceTextBox.Name = "LicenceTextBox"
        Me.LicenceTextBox.Size = New System.Drawing.Size(252, 20)
        Me.LicenceTextBox.TabIndex = 1
        '
        'ActivateButton
        '
        Me.ActivateButton.Location = New System.Drawing.Point(114, 99)
        Me.ActivateButton.Name = "ActivateButton"
        Me.ActivateButton.Size = New System.Drawing.Size(75, 23)
        Me.ActivateButton.TabIndex = 2
        Me.ActivateButton.Text = "Activate"
        Me.ActivateButton.UseVisualStyleBackColor = True
        '
        'FCancelButton
        '
        Me.FCancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.FCancelButton.Location = New System.Drawing.Point(195, 99)
        Me.FCancelButton.Name = "FCancelButton"
        Me.FCancelButton.Size = New System.Drawing.Size(75, 23)
        Me.FCancelButton.TabIndex = 3
        Me.FCancelButton.Text = "Cancel"
        Me.FCancelButton.UseVisualStyleBackColor = True
        '
        'ActivateForm
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(287, 143)
        Me.Controls.Add(Me.FCancelButton)
        Me.Controls.Add(Me.ActivateButton)
        Me.Controls.Add(Me.LicenceTextBox)
        Me.Controls.Add(Me.ActivateLabel)
        Me.Name = "ActivateForm"
        Me.Text = "Activate"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents ActivateLabel As System.Windows.Forms.Label
    Friend WithEvents LicenceTextBox As System.Windows.Forms.MaskedTextBox
    Friend WithEvents ActivateButton As System.Windows.Forms.Button
    Friend WithEvents FCancelButton As System.Windows.Forms.Button
End Class
