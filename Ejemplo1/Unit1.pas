unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  MotGraf2d;

type

  { TForm1 }

  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  public
    mot: TMotGraf;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  mot:= TMotGraf.Create(PaintBox1);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  mot. Destroy;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  mot.Line(0,0,100,100);
end;


end.

