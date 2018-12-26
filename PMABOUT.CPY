      ***************************************************************
      *   Main Window Action Bar Definitions
      ***************************************************************
       77 WND-MainWnd             Pic 9(4) Comp-5 Value 100.
       77 AB-File                 Pic 9(4) Comp-5 Value 110.
       77 AB-New                  Pic 9(4) Comp-5 Value 111.
       77 AB-Open                 Pic 9(4) Comp-5 Value 112.
       77 AB-Save                 Pic 9(4) Comp-5 Value 113.
       77 AB-Saveas               Pic 9(4) Comp-5 Value 114.
       77 AB-Exit                 Pic 9(4) Comp-5 Value 115.
       77 AB-Help                 Pic 9(4) Comp-5 Value 190.
       77 AB-Hhelp                Pic 9(4) Comp-5 Value 191.
       77 AB-Xhelp                Pic 9(4) Comp-5 Value 192.
       77 AB-Khelp                Pic 9(4) Comp-5 Value 193.
       77 AB-Ihelp                Pic 9(4) Comp-5 Value 194.
       77 AB-About                Pic 9(4) Comp-5 Value 195.
      ***************************************************************
      *   About Dialog Box Definitions
      ***************************************************************
       77 DLG-About               Pic 9(4) Comp-5 Value 900.
      ****************************************************************
      *   Miscellaneous Definitions
      ****************************************************************
       77 ReturnData                Pic s9(4) comp-5.
          88 ReturnTrue             Value 1.
          88 ReturnFalse            Value 0.
       77 EndFlag                   Pic x Value "N".
          88 EndFlagYes             Value "Y".
       01 ShortNull                 Pic s9(4) Comp-5 Value 0.
       01 LongNull                  Pic s9(9) Comp-5 Value 0.
       01 UShortNull                Pic  9(4) Comp-5 Value 0.
       01 ULongNull                 Pic  9(9) Comp-5 Value 0.
       01 NullString                Pic  x    Value Low-Values.

      ****************************************************************
      *   System Value Definitions
      ****************************************************************
       77 SV-CXSCREEN               pic s9(4) comp-5 value 20.
       77 SV-CYSCREEN               pic s9(4) comp-5 value 21.

      ****************************************************************
      *   Presentation Manager window handles
      ****************************************************************
       77 hab                       Pic s9(9) comp-5.
       77 hmq                       Pic s9(9) comp-5.
       77 hwndClient                Pic s9(9) comp-5.
       77 hwndFrame                 Pic s9(9) comp-5.
       77 HWND-DESKTOP              Pic s9(9) comp-5 value 1.
       77 HWND-TOP                  Pic s9(9) comp-5 value 3.
      ****************************************************************
      *   COBOL Procedure pointers
      ****************************************************************
       77 WindowProc                Procedure-Pointer.
       77 DialogProc                Procedure-Pointer.

      ****************************************************************
      *   Presentation Manager message structure
      ****************************************************************
       01 QMSG.
          05 QMSG-HWND              Pic 9(9) comp-5.
          05 QMSG-MSGID             Pic 9(4) comp-5.
          05 QMSG-PARAM1            Pic 9(9) comp-5.
          05 QMSG-PARAM2            Pic 9(9) comp-5.
          05 QMSG-TIME              Pic 9(9) comp-5.
          05 QMSG-POINT.
             10 QMSG-X              Pic 9(9) comp-5.
             10 QMSG-Y              Pic 9(9) comp-5.

      ***************************************************************
      *   Main Window Parameter Definitions
      ***************************************************************
       01 MainWndClass.
          03 Pic X(12)     Value 'MainWndClass'.
          03 Pic X         Value Low-Values.
       01 MainWndStyle     Pic 9(9)  Comp-5 Value 0.
       01 MainWndControls  Pic 9(9)  Comp-5 Value 0.
       01 MainWndTitle.
          03 Pic X(21)     Value 'PMABOUT Version 1.0'.
          03 Pic X         Value Low-Values.
       01 MainWndTxt.
          03 Pic X(21)     Value 'PMABOUT Version 1.0'.
          03 Pic X         Value Low-Values.
       01 MainWndTxtSize   Pic S9(4) Comp-5 Value 21.
       01 MainWndTxtColor  Pic S9(9) Comp-5 Value 0.
       01 MainWndColor     Pic S9(9) Comp-5 Value 0.
       77 MainWndPosition  Pic S9(4) comp-5 value 0.

      ***************************************************************
      *   Exit Message Box Definitions
      ***************************************************************
       01 MsgBoxTitle.
          05 Pic x(11) Value 'Exit Dialog'.
          05 Pic x(01) Value Low-Values.
       01 MsgBoxMsg.
          05 Pic x(30) Value 'Are you sure you want to exit?'.
          05 Pic x(01) Value Low-Values.
       01 MsgBoxControls  Pic 9(4) Comp-5 Value 0.
       01 MsgBoxButton    Pic 9(4) Comp-5 Value 0.

      *=============================================================*
      *  Presentation Manager header file definitions converted
      *  to COBOL define statements.
      *=============================================================*
      ***************************************************************
      *   Color Definitions
      ***************************************************************
       78 CLR-NOINDEX                       VALUE -254.
       78 CLR-FALSE                         VALUE -5.
       78 CLR-TRUE                          VALUE -4.
       78 CLR-ERROR                         VALUE -255.
       78 CLR-DEFAULT                       VALUE -3.
       78 CLR-WHITE                         VALUE -2.
       78 CLR-BLACK                         VALUE -1.
       78 CLR-BACKGROUND                    VALUE 0.
       78 CLR-BLUE                          VALUE 1.
       78 CLR-RED                           VALUE 2.
       78 CLR-PINK                          VALUE 3.
       78 CLR-GREEN                         VALUE 4.
       78 CLR-CYAN                          VALUE 5.
       78 CLR-YELLOW                        VALUE 6.
       78 CLR-NEUTRAL                       VALUE 7.
       78 CLR-DARKGRAY                      VALUE 8.
       78 CLR-DARKBLUE                      VALUE 9.
       78 CLR-DARKRED                       VALUE 10.
       78 CLR-DARKPINK                      VALUE 11.
       78 CLR-DARKGREEN                     VALUE 12.
       78 CLR-DARKCYAN                      VALUE 13.
       78 CLR-BROWN                         VALUE 14.
       78 CLR-PALEGRAY                      VALUE 15.

      ***************************************************************
      *   Control Syte Definitions
      ***************************************************************
       78 CS-MOVENOTIFY                     VALUE H"01".
       78 CS-SIZEREDRAW                     VALUE H"04".
       78 CS-HITTEST                        VALUE H"08".
       78 CS-PUBLIC                         VALUE H"10".
       78 CS-FRAME                          VALUE H"20".
       78 CS-CLIPCHILDREN                   VALUE H"20000000".
       78 CS-CLIPSIBLINGS                   VALUE H"10000000".
       78 CS-PARENTCLIP                     VALUE H"08000000".
       78 CS-SAVEBITS                       VALUE H"04000000".
       78 CS-SYNCPAINT                      VALUE H"02000000".

      ***************************************************************
      *   Message Box push button definitions
      ***************************************************************
       78 DID-OK                            VALUE 1.
       78 DID-CANCEL                        VALUE 2.
       78 DID-ERROR                         VALUE H"FFFF".

      ***************************************************************
      *   Frame Create Flags Definitions
      ***************************************************************
       78 FCF-TITLEBAR                      VALUE H"01".
       78 FCF-SYSMENU                       VALUE H"02".
       78 FCF-MENU                          VALUE H"04".
       78 FCF-SIZEBORDER                    VALUE H"08".
       78 FCF-MINBUTTON                     VALUE H"10".
       78 FCF-MAXBUTTON                     VALUE H"20".
       78 FCF-MINMAX                        VALUE H"30".
       78 FCF-VERTSCROLL                    VALUE H"40".
       78 FCF-HORZSCROLL                    VALUE H"80".
       78 FCF-DLGBORDER                     VALUE H"0100".
       78 FCF-BORDER                        VALUE H"0200".
       78 FCF-SHELLPOSITION                 VALUE H"0400".
       78 FCF-TASKLIST                      VALUE H"0800".
       78 FCF-NOBYTEALIGN                   VALUE H"1000".
       78 FCF-NOMOVEWITHOWNER               VALUE H"2000".
       78 FCF-ICON                          VALUE H"4000".
       78 FCF-ACCELTABLE                    VALUE H"8000".
       78 FCF-SYSMODAL                      VALUE H"010000".
       78 FCF-SCREENALIGN                   VALUE H"020000".
       78 FCF-MOUSEALIGN                    VALUE H"040000".
       78 FCF-PALETTE-NORMAL                VALUE H"080000".
       78 FCF-PALETTE-HELP                  VALUE H"100000".
       78 FCF-PALETTE-POPUPODD              VALUE H"200000".
       78 FCF-PALETTE-POPUPEVEN             VALUE H"400000".
       78 FCF-STANDARD                      VALUE H"08CC3F".

      ***************************************************************
      *   Message Box Control Definitions
      ***************************************************************
       78 MB-OK                             VALUE 0.
       78 MB-OKCANCEL                       VALUE H"01".
       78 MB-RETRYCANCEL                    VALUE H"02".
       78 MB-ABORTRETRYIGNORE               VALUE H"03".
       78 MB-YESNO                          VALUE H"04".
       78 MB-YESNOCANCEL                    VALUE H"05".
       78 MB-CANCEL                         VALUE H"06".
       78 MB-ENTER                          VALUE H"07".
       78 MB-ENTERCANCEL                    VALUE H"08".
       78 MB-NOICON                         VALUE 0.
       78 MB-CUANOTIFICATION                VALUE 0.
       78 MB-ICONQUESTION                   VALUE H"10".
       78 MB-ICONEXCLAMATION                VALUE H"20".
       78 MB-CUAWARNING                     VALUE H"20".
       78 MB-ICONASTERISK                   VALUE H"30".
       78 MB-ICONHAND                       VALUE H"40".
       78 MB-CUACRITICAL                    VALUE H"40".
       78 MB-QUERY                          VALUE H"10".
       78 MB-WARNING                        VALUE H"20".
       78 MB-INFORMATION                    VALUE H"30".
       78 MB-CRITICAL                       VALUE H"40".
       78 MB-ERROR                          VALUE H"40".
       78 MB-DEFBUTTON1                     VALUE 0.
       78 MB-DEFBUTTON2                     VALUE H"0100".
       78 MB-DEFBUTTON3                     VALUE H"0200".
       78 MB-APPLMODAL                      VALUE 0.
       78 MB-SYSTEMMODAL                    VALUE H"1000".
       78 MB-HELP                           VALUE H"2000".
       78 MB-MOVEABLE                       VALUE H"4000".

      ***************************************************************
      *   Message Box Message Definitions
      ***************************************************************
       78 MBID-OK                           VALUE 1.
       78 MBID-CANCEL                       VALUE 2.
       78 MBID-ABORT                        VALUE 3.
       78 MBID-RETRY                        VALUE 4.
       78 MBID-IGNORE                       VALUE 5.
       78 MBID-YES                          VALUE 6.
       78 MBID-NO                           VALUE 7.
       78 MBID-HELP                         VALUE 8.
       78 MBID-ENTER                        VALUE 9.
       78 MBID-ERROR                        VALUE H"FFFF".

      ****************************************************************
      *   Set Window Position Flag Definitions
      ****************************************************************
       78 SWP-SIZE                          VALUE H"01".
       78 SWP-MOVE                          VALUE H"02".
       78 SWP-ZORDER                        VALUE H"04".
       78 SWP-SHOW                          VALUE H"08".
       78 SWP-HIDE                          VALUE H"10".
       78 SWP-NOREDRAW                      VALUE H"20".
       78 SWP-NOADJUST                      VALUE H"40".
       78 SWP-ACTIVATE                      VALUE H"80".
       78 SWP-DEACTIVATE                    VALUE H"0100".
       78 SWP-EXTSTATECHANGE                VALUE H"0200".
       78 SWP-MINIMIZE                      VALUE H"0400".
       78 SWP-MAXIMIZE                      VALUE H"0800".
       78 SWP-RESTORE                       VALUE H"1000".
       78 SWP-FOCUSACTIVATE                 VALUE H"2000".
       78 SWP-FOCUSDEACTIVATE               VALUE H"4000".

      ***************************************************************
      *   PM Message definitions
      ***************************************************************
       78 WM-NULL                           VALUE 0.
       78 WM-CREATE                         VALUE H"01".
       78 WM-DESTROY                        VALUE H"02".
       78 WM-OTHERWINDOWDESTROYED           VALUE H"03".
       78 WM-ENABLE                         VALUE H"04".
       78 WM-SHOW                           VALUE H"05".
       78 WM-MOVE                           VALUE H"06".
       78 WM-SIZE                           VALUE H"07".
       78 WM-ADJUSTWINDOWPOS                VALUE H"08".
       78 WM-CALCVALIDRECTS                 VALUE H"09".
       78 WM-SETWINDOWPARAMS                VALUE H"0A".
       78 WM-QUERYWINDOWPARAMS              VALUE H"0B".
       78 WM-HITTEST                        VALUE H"0C".
       78 WM-ACTIVATE                       VALUE H"0D".
       78 WM-SETFOCUS                       VALUE H"0F".
       78 WM-SETSELECTION                   VALUE H"10".
       78 WM-PPAINT                         VALUE H"11".
       78 WM-PSETFOCUS                      VALUE H"12".
       78 WM-PSYSCOLORCHANGE                VALUE H"13".
       78 WM-PSIZE                          VALUE H"14".
       78 WM-PACTIVATE                      VALUE H"15".
       78 WM-PCONTROL                       VALUE H"16".
       78 WM-COMMAND                        VALUE H"20".
       78 WM-SYSCOMMAND                     VALUE H"21".
       78 WM-HELP                           VALUE H"22".
       78 WM-PAINT                          VALUE H"23".
       78 WM-TIMER                          VALUE H"24".
       78 WM-SEM1                           VALUE H"25".
       78 WM-SEM2                           VALUE H"26".
       78 WM-SEM3                           VALUE H"27".
       78 WM-SEM4                           VALUE H"28".
       78 WM-CLOSE                          VALUE H"29".
       78 WM-QUIT                           VALUE H"2A".
       78 WM-SYSCOLORCHANGE                 VALUE H"2B".
       78 WM-SYSVALUECHANGED                VALUE H"2D".
       78 WM-APPTERMINATENOTIFY             VALUE H"2E".
       78 WM-PRESPARAMCHANGED               VALUE H"2F".
       78 WM-CONTROL                        VALUE H"30".
       78 WM-VSCROLL                        VALUE H"31".
       78 WM-HSCROLL                        VALUE H"32".
       78 WM-INITMENU                       VALUE H"33".
       78 WM-MENUSELECT                     VALUE H"34".
       78 WM-MENUEND                        VALUE H"35".
       78 WM-DRAWITEM                       VALUE H"36".
       78 WM-MEASUREITEM                    VALUE H"37".
       78 WM-CONTROLPOINTER                 VALUE H"38".
       78 WM-CONTROLHEAP                    VALUE H"39".
       78 WM-QUERYDLGCODE                   VALUE H"3A".
       78 WM-INITDLG                        VALUE H"3B".
       78 WM-SUBSTITUTESTRING               VALUE H"3C".
       78 WM-MATCHMNEMONIC                  VALUE H"3D".
       78 WM-SAVEAPPLICATION                VALUE H"3E".
       78 WM-HELPBASE                       VALUE H"0F00".
       78 WM-HELPTOP                        VALUE H"0FFF".
       78 WM-USER                           VALUE H"1000".

