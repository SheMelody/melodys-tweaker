Public Class Form1
    Public DAM_Supported As Boolean = False
    Public BAM_Supported As Boolean = False
    Public GPF_Supported As Boolean = False
    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Dim f As New Form2()
        f.ShowDialog()
        f.Dispose()
        f = Nothing
    End Sub

    Sub GetCase(ByVal md0 As Integer, ByVal md1 As Integer, rb1 As RadioButton, rb2 As RadioButton, rb3 As RadioButton)
        If (md0 = 0 And md1 = 0) Or (md0 = 1 And md1 = 1) Then
            rb1.Select()
        ElseIf md0 = 1 And md1 = 0 Then
            rb2.Select()
        ElseIf md0 = 0 And md1 = 1 Then
            rb3.Select()
        End If
    End Sub

    Function SupportsDAM_BAM_GraphicsPerfSvc() As Boolean
        Dim k = My.Computer.Registry.LocalMachine.OpenSubKey("SYSTEM\CurrentControlSet\Services\")
        Try
            Dim k1 = k.OpenSubKey("dam")
            Dim k2 = k.OpenSubKey("bam")
            Dim k3 = k.OpenSubKey("GraphicsPerfSvc")
            If IsNothing(k1) Or IsNothing(k2) Or IsNothing(k3) Then
                Throw New Exception
            End If
            Return True
        Catch ex As Exception
            Return False
        End Try
    End Function

    Function SvcExists(ByVal svc As String)
        Dim k = My.Computer.Registry.LocalMachine.OpenSubKey("SYSTEM\CurrentControlSet\Services\")
        Try
            Dim k1 = k.OpenSubKey(svc)
            If IsNothing(k1) Then
                Throw New Exception
            End If
            Return True
        Catch ex As Exception
            Return False
        End Try
    End Function

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Me.Text &= " (v" & Me.ProductVersion & ")"
        If Environment.OSVersion.Version.Major <> 10 Then
            MsgBox("This tool was projected to work well in Windows 10 and hasn't been tested in other versions of Windows. Use at your own risk.", MsgBoxStyle.Exclamation, "Warning")
        End If
        If SvcExists("bam") Then
            BAM_Supported = True
        End If
        If SvcExists("dam") Then
            DAM_Supported = True
        End If
        If SvcExists("GraphicsPerfSvc") Then
            GPF_Supported = True
        End If
        Dim CurrentSep = 0
        Dim k As Microsoft.Win32.RegistryKey
        Try
            k = My.Computer.Registry.LocalMachine.OpenSubKey("SYSTEM\CurrentControlSet\Control\PriorityControl")
            CurrentSep = k.GetValue("Win32PrioritySeparation", 0)
            CurrentSep = CurrentSep - (CurrentSep \ 64)
        Catch ex As Exception
            CurrentSep = 0
        End Try
        For i = 0 To 5
            Dim md0 = CurrentSep Mod 2
            Dim res = CurrentSep \ 2
            Dim md1 = res Mod 2
            res = res \ 2
            GetCase(md0, md1, RadioButton7, RadioButton8, RadioButton9)
            md0 = res Mod 2
            res = res \ 2
            md1 = res Mod 2
            res = res \ 2
            GetCase(md0, md1, RadioButton4, RadioButton5, RadioButton6)
            md0 = res Mod 2
            res = res \ 2
            md1 = res Mod 2
            GetCase(md0, md1, RadioButton1, RadioButton2, RadioButton3)
        Next
        Comp_Service("MMCSS", "MMCSS", RadioButton10, RadioButton11, "Start", 4, 2)

        Comp_Service("AudioSrv", "AudioSrv", RadioButton20, RadioButton21, "ErrorControl", 2, 1)
        Comp_Prio("CSRSS", "csrss.exe", RadioButton12, RadioButton13)
        Comp_Prio("DWM", "dwm.exe", RadioButton14, RadioButton15)
        If DAM_Supported Then
            Comp_Service("DAM", "dam", RadioButton16, RadioButton17, "Start", 4, 1)
        Else
            GroupBox7.Enabled = False
        End If
        If BAM_Supported Then
            Comp_Service("BAM", "bam", RadioButton18, RadioButton19, "Start", 4, 1)
        Else
            GroupBox8.Enabled = False
        End If
        If GPF_Supported Then
            Comp_Service("GraphicsPerfSvc", "GraphicsPerfSvc", RadioButton22, RadioButton23, "Start", 4, 3)
        Else
            GroupBox10.Enabled = False
        End If
    End Sub

    Sub Comp_Service(ByVal Name As String, ByVal svc As String, ByVal rb1 As RadioButton, ByVal rb2 As RadioButton, Optional ByVal ValueToGet As String = "Start", Optional ByVal Value1st As Integer = 4, Optional ByVal Value2nd As Integer = 2)
        Try
            Dim k = My.Computer.Registry.LocalMachine.OpenSubKey("SYSTEM\CurrentControlSet\Services\" & svc)
            Dim st = k.GetValue(ValueToGet)
            If st = Value1st Then
                rb2.Select()
            Else
                rb1.Select()
                If st <> Value2nd Then
                    MsgBox("Unsupported configuration for " & Name & " detected. Applying change will override custom registry settings.", MsgBoxStyle.Exclamation, "Warning")
                End If
            End If
        Catch ex As Exception
            RadioButton10.Select()
            MsgBox("Failed to get " & Name & " configuration.", MsgBoxStyle.Critical, "Error")
        End Try
    End Sub

    Sub Comp_Prio(ByVal Name As String, ByVal process As String, ByVal rb1 As RadioButton, ByVal rb2 As RadioButton)
        Try
            Dim k = My.Computer.Registry.LocalMachine.OpenSubKey("SOFTWARE\Microsoft\Windows NT\CurrentVersion\Image File Execution Options\" & process & "\PerfOptions")
            Dim Io = k.GetValue("IoPriority", 0)
            Dim Prio = k.GetValue("CpuPriorityClass", 0)
            If Io = 3 And Prio = 4 Then
                rb2.Select()
            Else
                MsgBox("Unsupported configuration for " & Name & " detected. Applying change will override custom registry settings.", MsgBoxStyle.Exclamation, "Warning")
                Throw New Exception
            End If
        Catch ex As Exception
            rb1.Select()
        End Try
    End Sub

    Function MakeBin(ByVal rb1 As RadioButton, ByVal rb2 As RadioButton, ByVal rb3 As RadioButton, ByVal level As Integer) As Integer
        If rb1.Checked Then
            Return 0
        End If
        If rb2.Checked Then
            Return 2 ^ level
        End If
        If rb3.Checked Then
            Return 2 ^ (level + 1)
        End If
        Return 0
    End Function


    Sub SetSvc(ByVal Name As String, ByVal svc As String, ByVal rb1 As RadioButton, ByVal rb2 As RadioButton, ByVal value1 As Integer, ByVal value2 As Integer, Optional ByVal valname As String = "Start")
        Dim value As Integer
        If rb1.Checked Then
            value = value1
        ElseIf rb2.Checked Then
            value = value2
        End If
        Try
            Dim k = My.Computer.Registry.LocalMachine.OpenSubKey("SYSTEM\CurrentControlSet\Services\" & svc, True)
            k.SetValue(valname, value)
        Catch ex As Exception
            MsgBox("Failed to set configuration for " & Name & ". Please check registry access permissions and try again.", MsgBoxStyle.Critical, "Error")
        End Try
    End Sub
    Sub SetPrio(ByVal Name As String, ByVal process As String, ByVal rb2 As RadioButton)
        Try
            Dim k = My.Computer.Registry.LocalMachine.OpenSubKey("SOFTWARE\Microsoft\Windows NT\CurrentVersion\Image File Execution Options\", True)
            k.DeleteSubKeyTree(process, False)
            If rb2.Checked Then
                k = k.CreateSubKey(process)
                k = k.CreateSubKey("PerfOptions")
                k.SetValue("CpuPriorityClass", 4)
                k.SetValue("IoPriority", 3)
            End If
        Catch ex As Exception
            MsgBox("Failed to set priorities for " & Name & ". Please check registry access permissions and try again.", MsgBoxStyle.Critical, "Error")
        End Try
    End Sub
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim prio = MakeBin(RadioButton7, RadioButton8, RadioButton9, 0) + MakeBin(RadioButton4, RadioButton5, RadioButton6, 2) + MakeBin(RadioButton1, RadioButton2, RadioButton3, 4)
        Dim k
        Try
            k = My.Computer.Registry.LocalMachine.OpenSubKey("SYSTEM\CurrentControlSet\Control\PriorityControl", True)
            k.SetValue("Win32PrioritySeparation", prio, Microsoft.Win32.RegistryValueKind.DWord)
        Catch ex As Exception
            MsgBox("An error has occurred while writing Win32PrioritySeparation. Please check registry permissions and try again", MsgBoxStyle.Critical, "Cannot write to registry!")
        End Try
        SetSvc("MMCSS", "MMCSS", RadioButton10, RadioButton11, 2, 4)
        Try
            Dim aud = My.Computer.Registry.LocalMachine.OpenSubKey("SYSTEM\CurrentControlSet\Services\Audiosrv", True)
            Dim str As String() = aud.GetValue("DependOnService")
            If str.Contains("MMCSS") Then
                Dim NewL As New List(Of String)
                For Each st In str
                    If st <> "MMCSS" Then
                        NewL.Add(st)
                    End If
                Next
                aud.SetValue("DependOnService", NewL.ToArray(), Microsoft.Win32.RegistryValueKind.MultiString)
            End If
        Catch ex As Exception
        End Try

        SetPrio("CSRSS", "csrss.exe", RadioButton13)
        SetPrio("DWM", "dwm.exe", RadioButton15)
        SetSvc("AudioSrv", "AudioSrv", RadioButton20, RadioButton21, 1, 2, "ErrorControl")
        If DAM_Supported Then
            SetSvc("DAM", "dam", RadioButton16, RadioButton17, 1, 4)
        End If
        If BAM_Supported Then
            SetSvc("BAM", "bam", RadioButton18, RadioButton19, 1, 4)
        End If
        If GPF_Supported Then
            SetSvc("GraphicsPerfSvc", "GraphicsPerfSvc", RadioButton22, RadioButton23, 3, 4)
        End If
        Dim res = MsgBox("The changes have been applied. You need to reboot your computer for the changes to take effect.", MsgBoxStyle.Information, "Applied")
    End Sub


End Class
