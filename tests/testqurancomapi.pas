unit testqurancomapi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, qurancomapi, IniFiles
  ;

type

  { TTestQuranComAPI }

  TTestQuranComAPI= class(TTestCase)
  private
    FConf: TMemIniFile;
    FQuranComAPI: TQuranComAPI;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ListChapters;
    procedure ListVerses;
  end;

implementation

{ TTestQuranComAPI }

procedure TTestQuranComAPI.ListVerses;
begin
  FQuranComAPI.ListVerses(1);
end;

procedure TTestQuranComAPI.SetUp;
begin
  FConf:=TMemIniFile.Create('test.ini');
  FQuranComAPI:=TQuranComAPI.Create;
end;

procedure TTestQuranComAPI.TearDown;
var
  AStrings: TStringList;
begin
  AStrings:=TStringList.Create;
  AStrings.Text:=FQuranComAPI.Response;
  AStrings.SaveToFile(TestName+'.json');
  AStrings.Free;
  FConf.Free;
  FQuranComAPI.Free;
end;

procedure TTestQuranComAPI.ListChapters;
begin
  FQuranComAPI.ListChapters();
end;

initialization

  RegisterTest(TTestQuranComAPI);
end.

