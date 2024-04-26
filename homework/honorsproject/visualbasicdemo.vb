' visualbasicdemo.vb
' Spencer Baysinger
' 2024-04-21

' For CS 331 Spring 2024
' Honors Project  - Purpose is to demonstrate understandings of basic syntax and semantics of language
'                 - Code will be high level, compile-ready, operational, well documented, and readable.

' Project Purpose - To explore an additional language not covered in class.

' To run program on Windows
'   1.  Install .NET : https://dotnet.microsoft.com/en-us/download
'   2.  Add compiler 'vbc' to your path 
'       C:\Windows\Microsoft.NET\Framework64\v4.0.30319\vbc.exe
'   3.  Compile : "vbc filename.vb"
'       Run     : "filename.exe"


' Features - Directions
' - Add 3 more message boxes, perhaps load through loop
' - Then make some minor graphic display
' - Something else idk.


Imports System.Windows.Forms
Imports System.Drawing

Module Program


  ' ****************************************************************************************************************************************
  '
  ' Main
  '
  ' ****************************************************************************************************************************************


  Sub Main()
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault(False)

    ' Create the Form and set its properties
    Dim form As New Form()
      form.Text = "CS-331 Honors Project Demo"
      form.ClientSize = New Size(250, 300) ' Adjust size as needed
      form.MinimumSize = New Size(250, 150) ' Set minimum size for readability
      AddHandler form.Load, Sub(sender, e) CreateFirstLine(form)

    ' Load and set the form icon
    ' TODO: Make this work with a URL
    ' form.Icon = New Icon("https://github.com/sowens23/CS-F331/tree/main/homework/honorsproject/media/Straleos.ico")
    form.Icon = New Icon("E:\OneDrive\Documents\GitHub\CS-F331\homework\honorsproject\media\Straleos.ico")


    Dim exitbutton As New Button()
      exitbutton.Text = "Exit"
      exitbutton.Size = New Size(50, 20)
      exitbutton.Location = New Point(form.ClientSize.Width - exitbutton.Width, form.ClientSize.Height - exitbutton.Height)
      AddHandler exitbutton.Click, Sub(sender, e) form.Close()
      form.Controls.Add(exitbutton)

    StyleWindow(form)
    Application.Run(form)
  End Sub


  ' ****************************************************************************************************************************************
  '
  ' Utility Functions
  '
  ' ****************************************************************************************************************************************


	' Function to adjust control button positions when box is scaled
  Sub AdjustControlPositions(form As Form)
      ' This handles dynamic control adjustments when resizing
      Dim exitbutton As Button = form.Controls.OfType(Of Button)().FirstOrDefault(Function(x) x.Text = "Exit")
      If exitbutton IsNot Nothing Then
          If form.ClientSize.Width < 50 Then
              exitbutton.Width = form.ClientSize.Width
          Else
              exitbutton.Width = 50
          End If
          exitbutton.Location = New Point(form.ClientSize.Width - exitbutton.Width, form.ClientSize.Height - exitbutton.Height)
      End If
  End Sub

	' Function to style window at start of main
	Sub StyleWindow(ByVal form As Form) 
		' Set the FormBorderStyle property to None to remove borders and title bar
		'form.FormBorderStyle = FormBorderStyle.None

		' This will make the form borderless and remove the control box
		form.ControlBox = False

		' Optionally, you can also ensure the form cannot be resized
		form.MaximizeBox = False
		form.MinimizeBox = False
	End Sub

	' Function called to remove/dispose of existing form elements
	Sub RemoveButton(ByVal button As Button, ByVal form As Form)
    form.Controls.Remove(button)
    button.Dispose()  ' Optionally dispose the button if it's no longer needed
	End Sub


  ' ****************************************************************************************************************************************
  '
  ' Creating Interactable Text Boxes
  '
  ' ****************************************************************************************************************************************
  

  ' Creating the first interaction
  Sub CreateFirstLine(form As Form)
    ' Setup all controls on the form here
    Dim message1 As New Label()
      message1.Text = "Hello, world!"
      message1.AutoSize = True
      message1.Location = New Point(60, 20)
      form.Controls.Add(message1)

    Dim nextButton1 As New Button()
      nextButton1.Text = "Next"
      nextButton1.Size = New Size(50, 20)
      nextButton1.Location = New Point(160, 20)
      AddHandler nextButton1.Click, Sub(sender, e) 
        CreateSecondLine(form)
        RemoveButton(nextButton1, form)
      End Sub
      form.Controls.Add(nextButton1)

    ' Adjust control positions based on form size
    AddHandler form.Resize, Sub(sender, e) AdjustControlPositions(form)
  End Sub

  ' Creating the second interaction
  Sub CreateSecondLine(form As Form)
    ' Create the second label and set its properties
    Dim message2 As New Label()
      message2.Text = "Here I will demonstrate."
      message2.AutoSize = True
      message2.Location = New Point(10, 50) ' Position below the first message

    ' Create the Next button for the second line
    Dim nextButton2 As New Button()
      nextButton2.Text = "Next"
      nextButton2.Size = New Size(50, 20)
      nextButton2.Location = New Point(160, 50) ' Positioned to the right of the second message

    ' Add second line controls to the form
    form.Controls.Add(message2)
    form.Controls.Add(nextButton2)
  End Sub


End Module

