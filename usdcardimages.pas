unit usdcardimages;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Buttons, StdCtrls, ShellCtrls, Menus, ActnList, umcslogger;

type

  { TfrmSDCard }

  TfrmSDCard = class(TForm)
    actExplorer: TAction;
    actClose: TAction;
    actBackup: TAction;
    actDelete: TAction;
    actRefresh: TAction;
    actRestore: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    Image1: TImage;
    Label2: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pmShellView: TPopupMenu;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    ShellListView1: TShellListView;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure actBackupExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actExplorerExecute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actRestoreExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  frmSDCard: TfrmSDCard;

implementation

{$R *.lfm}

uses uconst, uwait, LCLIntf, LCLProc;

  { TfrmSDCard }

procedure TfrmSDCard.actRestoreExecute(Sender: TObject);
var
  filepath: string;
begin
  if ShellListView1.ItemIndex >= 0 then
  begin
    filepath := ShellListView1.GetPathFromItem(ShellListView1.Selected);
    if not (filepath = '') then
    begin
      if MessageDlg('Wiederherstellung',
        'Soll die aktuelle SD Karte mit den Daten aus dieser ZIP überschrieben werden?',
        mtConfirmation, mbOKCancel, '') = mrOk then
      begin
        frmWait.Show;
        frmSDCard.Enabled := False;
        Application.ProcessMessages;
        try
          HWLogger.Restore(filepath);
        finally
          frmWait.Hide;
          frmSDCard.Enabled := True;
        end;
      end;
    end;
  end;
end;

procedure TfrmSDCard.actBackupExecute(Sender: TObject);
begin
  if HWLogger.IsLoggerCard then
  begin
    if MessageDlg('Backup', 'Soll die aktuelle SD Karte gesichert werden?',
      mtConfirmation, mbOKCancel, '') = mrOk then
    begin
      frmWait.Show;
      frmSDCard.Enabled := False;
      Application.ProcessMessages;
      try
        HWLogger.Backup(ConfigPathes.Backuppath);
      finally
        frmWait.Hide;
        frmSDCard.Enabled := True;
        ShellListView1.UpdateView();
      end;
    end;
  end
  else
  begin
    MessageDlg('Backup',
      'Die aktuelle SD Karte ist keine Loggerkarte!',
      mtWarning, [mbOK], '');
  end;
end;

procedure TfrmSDCard.actCloseExecute(Sender: TObject);
begin
  frmSDCard.Close();
end;

procedure TfrmSDCard.actDeleteExecute(Sender: TObject);
var
  filepath: string;
begin
  if ShellListView1.ItemIndex >= 0 then
  begin
    filepath := ShellListView1.GetPathFromItem(ShellListView1.Selected);
    if not (filepath = '') then
    begin
      if MessageDlg('Löschen', 'Soll das Backup gelöscht werden?',
        mtConfirmation, mbOKCancel, '') = mrOk then
      begin
        if not DeleteFile(filepath) then
          MessageDlg('Backup konnte nicht gelöscht werden.',
            mtError, [mbOK], 0);

      end;
    end;
    ShellListView1.UpdateView();
  end;
end;

procedure TfrmSDCard.actExplorerExecute(Sender: TObject);
begin
  OpenDocument(ConfigPathes.Backuppath);
end;

procedure TfrmSDCard.actRefreshExecute(Sender: TObject);
begin
  ShellListView1.UpdateView();
end;

procedure TfrmSDCard.FormShow(Sender: TObject);
begin
  ShellListView1.Root := ConfigPathes.Backuppath;
  ShellListView1.UseBuiltInIcons := True;
end;

end.
