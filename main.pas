unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
  { private declarations }
  public
  { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Finder;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  stf: TSqlTextFinder;
begin
  stf := TSqlTextFinder.Create;
  try
    stf.FileName := 'C:\Fontes\Sistemas\SIS_FATURAMENTO\auxEnvelopes.dfm';
    stf.Run;
  finally
    stf.Free;
  end;
end;

end.

