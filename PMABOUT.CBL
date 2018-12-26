      *==============================================================*
       IDENTIFICATION DIVISION.
      *==============================================================*
       Program-ID.    PMABOUT.
       Author.        Chris Fierros.
       Installation.  OS/2 AAC.
       Date-Written.  August 1, 1992.
       Date-Compiled. August 1, 1992.
       Security.      Public.
      *==============================================================*
       ENVIRONMENT DIVISION.
      *==============================================================*
      *--------------------------------------------------------------*
       CONFIGURATION SECTION.
      *--------------------------------------------------------------*
        Source-Computer.  IBM-PS2.
        Object-Computer.  IBM-PS2.
        Special-Names.    Call-Convention 3 is OS2API.
      *--------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *--------------------------------------------------------------*
        COPY 'PMABOUT.CPY'.

      *--------------------------------------------------------------*
       LOCAL-STORAGE SECTION.
      *--------------------------------------------------------------*
       01 hps                       pic s9(9) Comp-5.
       01 Mresult                   pic x(4)  Comp-5.
       01 Rectl.
          05 xLeft                  pic s9(4) Comp-5.
          05 yBottom                pic s9(4) Comp-5.
          05 xRight                 pic s9(4) Comp-5.
          05 yTop                   pic s9(4) Comp-5.
       01 SizeWide                  pic s9(9) Comp-5.
       01 SizeTall                  pic s9(9) Comp-5.
       01 Color                     pic s9(9) Comp-5.
      *--------------------------------------------------------------*
       LINKAGE SECTION.
      *--------------------------------------------------------------*
       01 hwnd                      pic 9(9) Comp-5.
       01 Msg                       pic 9(4) Comp-5.
       01 MsgParm1                  pic 9(9) Comp-5.
       01 Redefines MsgParm1.
          05 MsgParm1w1             pic 9(4) Comp-5.
          05 MsgParm1w2             pic 9(4) Comp-5.
       01 MsgParm2                  pic 9(9) Comp-5.
       01 Redefines MsgParm2.
          05 MsgParm2w1             pic 9(4) Comp-5.
          05 MsgParm2w2             pic 9(4) Comp-5.
       01 MsgTime                   pic 9(9) Comp-5.
       01 MsgXPoint                 pic 9(9) Comp-5.
       01 MsgYPoint                 pic 9(9) Comp-5.
      
      *==============================================================*
       PROCEDURE DIVISION OS2API.
      *==============================================================*
      *  Initialization Routine.
      *  WinInitialize tells OS/2 this program will be using PM calls
      *  WinCreateMsgQue creates the applications message queque
      *  Set WindowProc... sets the procedure pointer to the programs
      *      MainWndProc Entry point defined in the Procedure Division
      *  WinRegisterClass registers the applications main window and
      *      associates the MainWndProc with it.
      *  WinCreateStdWindow creates the application main window
      ****************************************************************
           Call OS2API '__WinInitialize' 
                 using  by value UShortNull
                 returning hab
 
           Call OS2API '__WinCreateMsgQueue' 
                 using  by value hab
                        by value ShortNull
                 returning hmq.
 
           Set WindowProc to ENTRY 'MainWndProc'.
           Compute MainWndStyle = CS-SizeRedraw
                                + CS-ClipChildren.

           Call OS2API '__WinRegisterClass' 
                 using  by value hab
                        by reference MainWndClass
                        by value WindowProc
                        by value MainWndStyle
                        by value 0 size 2
                 returning ReturnData.

           If ReturnTrue
              Compute MainWndControls = FCF-TITLEBAR
                       + FCF-SYSMENU  + FCF-SIZEBORDER
                       + FCF-MINMAX   + FCF-SHELLPOSITION
                       + FCF-TASKLIST + FCF-MENU + FCF-ICON.

           Call OS2API '__WinCreateStdWindow' 
                 using  by value HWND-DESKTOP
                        by value ULongNull
                        by reference MainWndControls
                        by reference MainWndClass
                        by reference MainWndTitle
                        by value ULongNull
                        by value UShortNull
                        by value Wnd-MainWnd
                        by reference hwndClient
                 returning hwndFrame.
 
      ****************************************************************
      *  Center Window.
      *  The following APIs query the sysem for the width and height
      *  of the display.  These values are then used to center the
      *  main window on the Desktop via the WinSetWindowPos API.
      ****************************************************************
           Call OS2API '__WinQuerySysValue' 
                 using  by value HWND-DESKTOP
                        by value SV-CXSCREEN
                 returning SizeWide.
      
           Call OS2API '__WinQuerySysValue' 
                 using  by value HWND-DESKTOP
                        by value SV-CYSCREEN
                 returning SizeTall.
 
           Compute YTop    = SizeTall * .5.
           Compute XRight  = sizeWide * .6.
           Compute XLeft   = (SizeWide - XRight) / 2.
           Compute YBottom = (SizeTall - YTop) / 2.
 
           Compute MainWndPosition = SWP-SIZE     
                        + SWP-MOVE + SWP-ZORDER
                        + SWP-SHOW + SWP-ACTIVATE.

           Call OS2API '__WinSetWindowPos' 
                 using  by value hwndFrame
                        by value HWND-TOP
                        by value XLeft
                        by value YBottom
                        by value XRight
                        by value YTop
                        by value MainWndPosition
                 returning ReturnData.

      ****************************************************************
      *  Main Message Routine.
      *  If the handle to the windows frame is valid then the program
      *  begins the Main Message loop of retrieving messages from it
      *  message queue one by one via the WinGetMsg API until it
      *  receives a message of WM-Quit.  WinDispatchMsg is used to
      *  send the message to the appropriate procedure.
      ****************************************************************
           If hwndFrame not = 0
               Perform until EndFlagYes 
                 Call OS2API '__WinGetMsg' 
                       using  by value hab
                              by reference QMSG
                              by value ULongNull
                              by value UShortNull
                              by value UShortNull
                       returning ReturnData
              
                 If (QMSG-MSGID = WM-QUIT)
                     Set EndFlagYes to true
                 Else
                     Call OS2API '__WinDispatchMsg' 
                           using  by value hab
                                  by reference QMSG
                           returning ReturnData
                 End-If
               End-perform
           End-If.

      ****************************************************************
      *  Termination Routine
      *  WinDestroyWindow kills the applications main window
      *  WinDestroyMsgQueue kills the applications message que
      *  WinTerminate tells OS/2 that PM services are not longer needed
      ****************************************************************

           Call OS2API '__WinDestroyWindow'  
                 using  by value hwndFrame.
           Call OS2API '__WinDestroyMsgQueue' 
                 using  by value hmq.
           Call OS2API '__WinTerminate' 
                 using by value hab.
           STOP RUN.
      
      *==============================================================*
       MainWndProc section.
      *==============================================================*
      *  The Entry 'MainWndProc'... statement defines this procedures
      *  entry point.  Messages are passed to this procedure via the
      *  Msg, MsgParm1, and MsgParm2 parameters.  All procedures will
      *  receive, evaluate, and act on messages.
      *  This procedure is looking for a WM-Command, WM-Close, and
      *  a WM-Paint messages.  If a WM-Command message is received
      *  further evaluation as to what specific WM-Command message
      *  is performed.  If a AB-About message is received (meaning the
      *  user has selected "About" form the Action Bar pull down) then
      *  the DialogProc entry point is set and the WinDlgBox API
      *  loads the About Dialog box.
      *  If a message is not acted on via WHEN then it is a WHEN OTHER
      *  and the WinDEfWindowProc API tells PM to perform default
      *  processing on the message or discard it.
      ****************************************************************
           Entry 'MainWndProc' using by value hwnd
                                     by value Msg
                                     by value MsgParm1
                                     by value MsgParm2.
           Move 0 to Mresult.

           Evaluate Msg
               When WM-Command
                    Evaluate MsgParm1
                        When AB-About
                             Set DialogProc to ENTRY 'AboutDlgProc'
                             Call OS2API '__WinDlgBox'
                                   using by value hwnd-DeskTop
                                         by value hWnd
                                         by value DialogProc
                                         by value UShortNull
                                         by value Dlg-About
                                         by value LongNull
                                   returning ReturnData
                        When AB-Exit
                             Perform ExitProgram
                    End-Evaluate

               When WM-CLOSE
                    Perform ExitProgram

               When WM-PAINT
                    Perform PaintWindow

               When Other
                    Call OS2API '__WinDefWindowProc'
                          using by value  hwnd
                                by value  Msg
                                by value  MsgParm1
                                by value  MsgParm2
                          returning Mresult
           End-Evaluate.
           Exit program returning Mresult.

      *--------------------------------------------------------------*
       PaintWindow Section.
      *--------------------------------------------------------------*
      *  This routine performs the necessary API calls to re-paint
      *  the client window area.  This seciton is performed only
      *  when a WM-Pain message is received from the applications
      *  message queue.  PM sends a WM-Paint message anytime the
      *  window is moved, sizeds, maximized, minimized, restored, ect.
      ****************************************************************
           Move 0 to XLeft, YBottom, XRight, Ytop.
           Call OS2API '__WinBeginPaint'
                 using  by value hwnd
                        by value LongNull
                        by reference Rectl
                 returning hps.

           Call OS2API '__WinFillRect' 
                 using  by value hps
                        by reference Rectl
                        by value MainWndColor
                 returning ReturnData.
      
           Call OS2API '__WinEndPaint' 
                 using  by value hps
                 returning ReturnData.

      *--------------------------------------------------------------*
       ExitProgram Section.
      *--------------------------------------------------------------*
      *  This routine displays a message box which asks the user
      *  to confirm the request to exit the application.
      *  The MsgBoxControls define the attibutes of the message box 
      *  such as push buttons and icons. MsgBoxMsg is defined in the 
      *  copy book as the text displayed. MsgBoxTitle is defined in the
      *  copy book as the title text.
      *  MsgBoxButton is returned after the user selects a button from
      *  the message box.  If the user select 'Yes' from the message
      *  box the the WHEN MB-Yes is true and a call to post the WM-QUIT
      *  message is executed.  This message is retrieved from the
      *  Main Message Routine and causes the program to terminate.
      ****************************************************************
            Compute MsgBoxControls = MB-YESNO + MB-ICONQUESTION
                                   + MB-APPLMODAL + MB-MOVEABLE.

            Call OS2API '__WinMessageBox'
                  using  by value HWND-DESKTOP
                         by value hwndClient
                         by reference MsgBoxMsg
                         by reference MsgBoxTitle
                         by value UShortNull
                         by value MsgBoxControls
                  returning MsgBoxButton.

            Evaluate MsgBoxButton
                When MBID-YES
                     Call OS2API '__WinPostMsg'
                           using by Value hwndFrame
                                 by Value WM-Quit size 2
                                 by Value LongNull
                                 by Value LongNull
                           returning ReturnData
                When Other
                     Continue
            End-Evaluate.

      *==============================================================*
       AboutDlgProc Section.
      *==============================================================*
      *  The About Dialog Procedure routine demonstrated how to process
      *  dialogs.  This procedure is looking for a WM-Command message
      *  with the first message parameter (MsgParm1) equal to Did-Ok
      *  or a WM-Close message.   The WM-Close message is sent to the
      *  application when the use selects "Close" from the windows
      *  system menu.
      ****************************************************************
       Entry 'AboutDlgProc' using by value hwnd
                                  by value Msg
                                  by value MsgParm1
                                  by value MsgParm2.
       Move 0 to Mresult

       Evaluate Msg
           When WM-COMMAND
                Evaluate MsgParm1
                    When DID-OK
                         Call OS2API '__WinDismissDlg'
                               using by value Hwnd
                                     by value UShortNull
                    When Other
                         Continue
                End-Evaluate

          When WM-CLOSE
               Call OS2API '__WinDismissDlg'
                     using by value Hwnd
                           by value UShortNull
          When Other
               Call OS2API '__WinDefDlgProc'
                     using by value hwnd
                           by value Msg
                           by value MsgParm1
                           by value MsgParm2
                     returning Mresult
       End-Evaluate.
       Exit Program Returning Mresult.

      *==============================================================*
      *  END OF CODE MARKER: This marks the end of all code for this
      *  program.  Any code executing after this marker has done so
      *  without the knowledge or consent of the author.
      *==============================================================*
