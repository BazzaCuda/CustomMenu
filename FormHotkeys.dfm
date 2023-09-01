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
  object btnOK: TButton
    Left = 116
    Top = 105
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 1
    TabStop = False
    OnClick = btnOKClick
  end
end
