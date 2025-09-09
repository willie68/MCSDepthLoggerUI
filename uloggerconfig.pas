unit uloggerconfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Buttons, StdCtrls, Spin, LazNumEdit, ulogger;

type

  { TFrmLoggerConfig }

  TFrmLoggerConfig = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    cbBaudB: TComboBox;
    CheckBox1: TCheckBox;
    cbBaudA: TComboBox;
    cgFeatures: TCheckGroup;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    sedVesselID: TSpinEdit;
  private
    mloggerconfig: TLoggerConfig;
  public

  end;

var
  frmLoggerConfig: TFrmLoggerConfig;

implementation

{$R *.lfm}

{ TForm3 }

end.
