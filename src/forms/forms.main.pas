unit Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, SQLDB, DB, Forms, Controls, Graphics, Dialogs,
  Menus, ActnList, StdCtrls, ExtCtrls, DBGrids, StdActns, fgl;

type

  TPathList = specialize TFPGMap<String, String>;

  { TfrmMain }

  TfrmMain = class(TForm)
    actCookiesFind: TAction;
    actCookiesRead: TAction;
    alMain: TActionList;
    btnCookiesFind: TButton;
    btnCookiesRead: TButton;
    comboboxPath: TComboBox;
    dsCookies: TDataSource;
    DBGridCookies: TDBGrid;
    actFileExit: TFileExit;
    gbPath: TGroupBox;
    gbCookies: TGroupBox;
    mnuCookiesRead: TMenuItem;
    mnuCookiesFind: TMenuItem;
    mnuCookies: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuFile: TMenuItem;
    mmMain: TMainMenu;
    panButtons: TPanel;
    SQLite3Connection: TSQLite3Connection;
    SQLQueryCookies: TSQLQuery;
    SQLTransaction: TSQLTransaction;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure alMainUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure actCookiesFindExecute(Sender: TObject);
    procedure actCookiesReadExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FTmpProfiles: String;
    FPathList: TPathList;
    procedure MemoGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure OpenDataSet(aDataSet: TSQLQuery);
    procedure InitShortCuts;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  LCLType
, IniFiles
, FileUtil
;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin                           
  FPathList := TPathList.Create;
  InitShortCuts;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SQLite3Connection.Close;
  if FileExists(FTmpProfiles) then
    DeleteFile(FTmpProfiles);
end;

procedure TfrmMain.InitShortCuts;
begin
{$IFDEF UNIX}
  actFileExit.ShortCut := KeyToShortCut(VK_Q, [ssCtrl]);
{$ENDIF}
{$IFDEF WINDOWS}
  actFileExit.ShortCut := KeyToShortCut(VK_X, [ssAlt]);
{$ENDIF}
end;

procedure TfrmMain.alMainUpdate(AAction: TBasicAction; var Handled: Boolean);
begin
  if FPathList.Count = 0 then
  begin
    actCookiesFind.Enabled:= True;
    actCookiesRead.Enabled:= False;
  end
  else
  begin
    actCookiesFind.Enabled:= False;
    actCookiesRead.Enabled:= True;
  end;
  Handled:= True;
end;

procedure TfrmMain.actCookiesFindExecute(Sender: TObject);
var
  profilesFileTest, profilesFile, section: String;
  profilesINI: TIniFile;
  sections: TStringList;
  i: Integer;
begin
  FPathList.Clear;
  profilesFile:= EmptyStr;
  profilesFileTest:= EmptyStr;
{$IFDEF WINDOWS}
  profilesFileTest:= GetUserDir;
  profilesFileTest:= ConcatPaths([
    profilesFileTest,
    'AppData',
    'Roaming',
    'Mozilla',
    'Firefox',
    'profiles.ini'
  ]);
  if FileExists(profilesFileTest) then
    profilesFile:= profilesFileTest;
{$ENDIF}
{$IFDEF LINUX}
  // snapProfilesPath := filepath.Join(usr.HomeDir, "snap", "firefox", "common", ".mozilla", "firefox", "profiles.ini")
  // regularProfilesPath := filepath.Join(usr.HomeDir, ".mozilla", "firefox", "profiles.ini")
  // Snap
  profilesFileTest:= GetUserDir;
  profilesFileTest:= ConcatPaths([
    profilesFileTest,
    'snap',
    'firefox',
    'common',
    '.mozilla',
    'firefox',
    'profiles.ini'
  ]);
  if FileExists(profilesFileTest) then
    profilesFile:= profilesFileTest;

  // From dist repo
  profilesFileTest:= GetUserDir;
  profilesFileTest:= ConcatPaths([
    profilesFileTest,
    '.mozilla',
    'firefox',
    'profiles.ini'
  ]);
  if FileExists(profilesFileTest) then
    profilesFile:= profilesFileTest;
{$ENDIF}
  if Length(profilesFile) <> 0 then
  begin
    profilesINI:= TIniFile.Create(profilesFile);
    try
      sections:= TStringList.Create;
      try
        profilesINI.ReadSections(sections);
        for section in sections do
        begin
          if Pos('Profile', section) > 0 then
          begin
            profilesFileTest:= ConcatPaths([
              ExtractFileDir(profilesFile),
              profilesINI.ReadString(section, 'Path', ''),
              'cookies.sqlite'
            ]);
            if FileExists(profilesFileTest) then
              FPathList.Add(profilesINI.ReadString(section, 'name', ''), profilesFileTest);
          end;
        end;
      finally
        sections.Free;
      end;
    finally
      profilesINI.Free;
    end;
  end;
  comboboxPath.Items.Clear;
  for i := 0 to FPathList.Count - 1 do
    comboboxPath.Items.Add(FPathList.Keys[i] + ' [' + FPathList.Data[i] + ']');
  if FPathList.Count > 0 then
    comboboxPath.ItemIndex := 0;
end;

procedure TfrmMain.actCookiesReadExecute(Sender: TObject);
var
  TmpProfilePath: String;
begin
  if comboboxPath.ItemIndex < 0 then
    TmpProfilePath := comboboxPath.Text
  else
    TmpProfilePath := FPathList.Data[comboboxPath.ItemIndex];
  if (TmpProfilePath = '') or not FileExists(TmpProfilePath) then
    exit;
  FTmpProfiles:= GetTempFileName;
  CopyFile(TmpProfilePath, FTmpProfiles);

  SQLite3Connection.DatabaseName:= FTmpProfiles;
  SQLite3Connection.Open;

  SQLQueryCookies.SQL.Add(
    'SELECT host, name, value, path, datetime(expiry, ''unixepoch'', ''localtime'') expiry, isSecure, isHttpOnly ' +
    'FROM moz_cookies;'
  );
  OpenDataSet(SQLQueryCookies);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FPathList.Free;
end;

procedure TfrmMain.MemoGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  aText := Sender.AsString;
end;

procedure TfrmMain.OpenDataSet(aDataSet: TSQLQuery);
var
  i: Integer;
begin
  aDataSet.Open;
  for i := 0 to aDataSet.FieldCount - 1 do
  begin
    case aDataSet.Fields[i].DataType of
      ftMemo, ftString: aDataSet.Fields[i].OnGetText := @MemoGetText;
    end;
  end;
end;

end.

