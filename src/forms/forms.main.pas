unit Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, SQLDB, DB, Forms, Controls, Graphics, Dialogs,
  Menus, ActnList, StdCtrls, ExtCtrls, DBGrids, StdActns;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actCookiesFind: TAction;
    actCookiesRead: TAction;
    alMain: TActionList;
    btnCookiesFind: TButton;
    btnCookiesRead: TButton;
    dsCookies: TDataSource;
    DBGridCookies: TDBGrid;
    edtPath: TEdit;
    actFileExit: TFileExit;
    gbPath: TGroupBox;
    gbCookies: TGroupBox;
    mnuCookies: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuFile: TMenuItem;
    mmMain: TMainMenu;
    panButtons: TPanel;
    SQLite3Connection: TSQLite3Connection;
    SQLQueryCookies: TSQLQuery;
    SQLTransaction: TSQLTransaction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure alMainUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure actCookiesFindExecute(Sender: TObject);
    procedure actCookiesReadExecute(Sender: TObject);
  private
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
;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  InitShortCuts;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  { #todo -ogcarreno : Disconnect Database }
  { #todo -ogcarreno : Delete database temp file }
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
  if Length(edtPath.Text) = 0 then
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
  //index: Integer;
begin
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
            begin
              edtPath.Text:= profilesFileTest;
              break;
            end;
          end;
        end;
      finally
        sections.Free;
      end;
    finally
      profilesINI.Free;
    end;
  end;
end;

procedure TfrmMain.actCookiesReadExecute(Sender: TObject);
begin

end;

end.

