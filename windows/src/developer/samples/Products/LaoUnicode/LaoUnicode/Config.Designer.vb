<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class ConfigForm
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
        Me.StartWithWindowsCheckbox = New System.Windows.Forms.CheckBox
        Me.OKButton = New System.Windows.Forms.Button
        Me.FCancelButton = New System.Windows.Forms.Button
        Me.SuspendLayout()
        '
        'StartWithWindowsCheckbox
        '
        Me.StartWithWindowsCheckbox.AutoSize = True
        Me.StartWithWindowsCheckbox.Location = New System.Drawing.Point(28, 37)
        Me.StartWithWindowsCheckbox.Name = "StartWithWindowsCheckbox"
        Me.StartWithWindowsCheckbox.Size = New System.Drawing.Size(117, 17)
        Me.StartWithWindowsCheckbox.TabIndex = 0
        Me.StartWithWindowsCheckbox.Text = "Start with Windows"
        Me.StartWithWindowsCheckbox.UseVisualStyleBackColor = True
        '
        'OKButton
        '
        Me.OKButton.Location = New System.Drawing.Point(156, 82)
        Me.OKButton.Name = "OKButton"
        Me.OKButton.Size = New System.Drawing.Size(75, 23)
        Me.OKButton.TabIndex = 1
        Me.OKButton.Text = "OK"
        Me.OKButton.UseVisualStyleBackColor = True
        '
        'FCancelButton
        '
        Me.FCancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.FCancelButton.Location = New System.Drawing.Point(237, 82)
        Me.FCancelButton.Name = "FCancelButton"
        Me.FCancelButton.Size = New System.Drawing.Size(75, 23)
        Me.FCancelButton.TabIndex = 2
        Me.FCancelButton.Text = "Cancel"
        Me.FCancelButton.UseVisualStyleBackColor = True
        '
        'ConfigForm
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(329, 122)
        Me.Controls.Add(Me.FCancelButton)
        Me.Controls.Add(Me.OKButton)
        Me.Controls.Add(Me.StartWithWindowsCheckbox)
        Me.Name = "ConfigForm"
        Me.Text = "Lao Unicode Configuration"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents StartWithWindowsCheckbox As System.Windows.Forms.CheckBox
    Friend WithEvents OKButton As System.Windows.Forms.Button
    Friend WithEvents Button2 As System.Windows.Forms.Button
    Friend WithEvents FCancelButton As System.Windows.Forms.Button
End Class
