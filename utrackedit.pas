unit utrackedit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Buttons, StdCtrls, Spin, umcslogger;

type

  { TfrmTrackEdit }

  TfrmTrackEdit = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    cbGroup: TComboBox;
    edName: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    mmDescription: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    sedVesselID: TSpinEdit;
  private

  public

  end;

var
  frmTrackEdit: TfrmTrackEdit;

implementation

{$R *.lfm}

{ TfrmTrackEdit }

end.

