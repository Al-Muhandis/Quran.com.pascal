program qurancomapi_gui;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, testqurancomapi, qurancomapi;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

