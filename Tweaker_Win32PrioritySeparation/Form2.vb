Public Class Form2
    Private Sub Form2_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        TextBox1.Text = My.Resources.help
        TextBox1.Select(0, 0)
    End Sub
End Class