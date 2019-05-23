unit testqurancomapi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, qurancomapi, IniFiles
  ;

type

  { TTestListVerses }

  TTestListVerses= class(TTestCase)
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

{ TTestListVerses }

procedure TTestListVerses.ListVerses;
begin
  FQuranComAPI.ListVerses(1);
end;

procedure TTestListVerses.SetUp;
begin
  FConf:=TMemIniFile.Create('test.ini');
  FQuranComAPI:=TQuranComAPI.Create;
end;

procedure TTestListVerses.TearDown;
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

procedure TTestListVerses.ListChapters;
begin
  FQuranComAPI.ListChapters();
end;

initialization

  RegisterTest(TTestListVerses);
end.

