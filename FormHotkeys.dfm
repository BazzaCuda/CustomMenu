object HotkeyForm: THotkeyForm
  Left = 0
  Top = 0
  Caption = 'CustomMenu Hotkey'
  ClientHeight = 138
  ClientWidth = 307
  Color = 2829099
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 17
  object Label1: TLabel
    Left = 29
    Top = 8
    Width = 247
    Height = 34
    Alignment = taCenter
    Caption = 
      'Press a key combination to select a hotkey and click OK with the' +
      ' mouse'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object hkHotkey: THotKey
    Left = 93
    Top = 60
    Width = 121
    Height = 19
    HotKey = 0
    Modifiers = []
    TabOrder = 0
  end
  object pnlOK: TPanel
    Left = 114
    Top = 94
    Width = 78
    Height = 27
    Caption = 'OK'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clSilver
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = pnlOKClick
  end
end
