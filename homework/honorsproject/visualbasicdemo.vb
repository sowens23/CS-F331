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

Imports System.Windows.Forms
Imports System.Drawing

Module Program

  Sub Main()
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault(False)

    ' ****************************************************************************************************************************************
		'
    ' WINDOW TITLE
		'
    ' ****************************************************************************************************************************************

    ' Create the Form and set its properties
    Dim page1 As New Form()
      page1.Text = "CS-331 Honors Project Demo"
      page1.ClientSize = New Size(300, 200) ' Adjust size as needed
      page1.MinimumSize = New Size(50, 50) ' Set minimum size for readability

    ' ****************************************************************************************************************************************
		'
    ' WINDOW BODY
		'
    ' ****************************************************************************************************************************************

    ' Create the Next button for the first line
    Dim nextButton1 As New Button()
      nextButton1.Text = "Next"
      nextButton1.Size = New Size(50, 20)
      nextButton1.Location = New Point(160, 20) ' Positioned to the right of the first message
      AddHandler nextButton1.Click, Sub(sender, e) CreateSecondLine(page1)

    ' Create a label for "Hello, World!"
    Dim message1 As New Label()
      message1.Text = "Hello, world!"
      message1.AutoSize = True
      message1.Location = New Point(60, 20) ' Shifted right by 50 pixels

    ' ****************************************************************************************************************************************
		'
    ' WINDOW FOOTER
		'
    ' ****************************************************************************************************************************************

    ' Create the Exit button and set its properties
    Dim exitbutton As New Button()
      exitbutton.Text = "Exit"
      exitbutton.Size = New Size(50, 20)
      exitbutton.Location = New Point(page1.ClientSize.Width - exitbutton.Width, page1.ClientSize.Height - exitbutton.Height)
      AddHandler exitbutton.Click, Sub(sender, e) page1.Close()

    ' ****************************************************************************************************************************************
		'
    ' WINDOW HANDLERS
		'
    ' ****************************************************************************************************************************************

    ' Create the Exit button and set its properties

    ' Handler for resizing
    AddHandler page1.Resize, Sub(sender, e)
		AddHandler nextButton1.Click, Sub(senderArg, eArg) RemoveButton(nextButton1, page1)


    ' Add controls to the page1
    page1.Controls.Add(message1)
    page1.Controls.Add(nextButton1)
    page1.Controls.Add(exitbutton)

		' button.Width = page1.ClientSize.Width * 0.1
		If page1.ClientSize.Width < 50 Then
			exitbutton.width = page1.ClientSize.Width
		Else
			exitbutton.width = 50 
		End If
		exitbutton.Location = New Point(page1.ClientSize.Width - exitbutton.Width, page1.ClientSize.Height - exitbutton.Height)
  	End Sub

    ' Apply custom styling to the page1
    StyleWindow(page1)

    ' Run the application
    Application.Run(page1)
  End Sub

	' ****************************************************************************************************************************************
	'
	' Display 2nd Line
	'
	' ****************************************************************************************************************************************

  Sub CreateSecondLine(page1 As Form)
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

    ' Add second line controls to the page1
    page1.Controls.Add(message2)
    page1.Controls.Add(nextButton2)

		
  End Sub

	' ****************************************************************************************************************************************
	'
	' SUB FUNCTIONS
	'
	' ****************************************************************************************************************************************

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

End Module

