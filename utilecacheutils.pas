unit utilecacheutils;

interface

uses
  Classes, SysUtils, FileUtil, DateUtils, MCSDBGLog;

type

  { TFileCleanupThread }

  TFileCleanupThread = class(TThread)
  private
    FLog: TLogger;
    FRootPath: string;
    FMaxAgeDays: integer;
    FStopRequested: boolean;
    procedure TraverseAndDelete(const Path: string);
  protected
    procedure Execute; override;
  public
    constructor Create(const RootPath: string; MaxAgeDays: integer);
    destructor Destroy; override;
    procedure RequestStop;
  end;

implementation

uses LazLoggerBase;

constructor TFileCleanupThread.Create(const RootPath: string; MaxAgeDays: integer);
begin
  FLog := TLOgger.Create('FileCleanupThread');
  FreeOnTerminate := False;
  FRootPath := RootPath;
  FMaxAgeDays := MaxAgeDays;
  FStopRequested := False;
  inherited Create(False); // Start immediately
end;

destructor TFileCleanupThread.Destroy;
begin
  inherited Destroy;
  FLog.Free();
end;

procedure TFileCleanupThread.RequestStop;
begin
  FStopRequested := True;
end;

procedure TFileCleanupThread.Execute;
begin
  FLog.Info('File cleanup started');
  TraverseAndDelete(FRootPath);
end;


procedure TFileCleanupThread.TraverseAndDelete(const Path: string);
var
  SearchRec: TSearchRec;
  CurrentDate: TDateTime;
  FileDate: TDateTime;
  FullPath: string;
  HasFilesOrDirs: boolean;
begin
  CurrentDate := Now;
  HasFilesOrDirs := False;

  if FindFirst(Path + DirectorySeparator + '*', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if FStopRequested then Exit;

      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        FullPath := Path + DirectorySeparator + SearchRec.Name;

        if (SearchRec.Attr and faDirectory) <> 0 then
        begin
          // Recurse into subdirectory
          TraverseAndDelete(FullPath);
        end
        else
        begin
          // Check file age
          if FileAge(FullPath, FileDate) then
          begin
            if DaysBetween(CurrentDate, FileDate) > FMaxAgeDays then
            begin
              try
                DeleteFile(FullPath);
              except
                // Handle deletion error if needed
              end;
            end;
          end;
        end;

        // Mark that this folder contains something
        HasFilesOrDirs := True;
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;

  // After processing, check if folder is now empty
  if not HasFilesOrDirs then
  begin
    try
      RemoveDir(Path); // Deletes empty directory
    except
      // Optional: log or ignore errors
    end;
  end;
end;

end.
