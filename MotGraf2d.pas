{Define un objeto "Motor gráfico", que permite dibujar en un lienzo virtual,
de coordenadas de tipo Single, que luego se transformarán a coordenadas de la
pantalla (en pixeles).
El sistema de coordenadas, sigue la dirección usual en geometría:

Y  /|\
    |
    |
    |
    |
    +--------------------> X
}
unit MotGraf2d;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Graphics, ExtCtrls, Controls;
type
  { TMotGraf }
  TMotGraf = class
    //Parámetros de la cámara (perspectiva)
    x_cam   : Single;  //coordenadas de la camara
    y_cam   : Single;

    Zoom    : Single;     //factor de ampliación


    {Desplazamiento para ubicar el centro virtual de la pantalla (0,0)
    Se indica en pixeles. Si por ejemplo, se fija:
    x_Des = 10 y y_Des = 10
    Hará que cuando se dibuje algo virtualmente en (0,0), aparecerá desplazado
    10 pixeles a la derecha del borde izquierdo y 10 pixeles arriba del borde inferior}
    x_des      : integer;
    y_des      : Integer;
  private
    gControl: TGraphicControl;   //Control gráfico, en donde se va a dibujar
    cv      : Tcanvas;           //referencia al lienzo
    function GetPenColor: TColor;
    procedure SetPenColor(AValue: TColor);
  public    //Funciones de transformación
    function XPant(x: Single): Integer; inline;
    function YPant(y: Single): Integer; inline;
    procedure XYpant(xv, yv: Single; var xp, yp: Integer);
  public  //Configuración
    property PenColor: TColor read GetPenColor write SetPenColor;
    procedure Clear;
    procedure Line(const x1,y1,x2,y2: Double);
  public  //Inicialización
    constructor Create(gContrl0: TGraphicControl);
    destructor Destroy; override;
  end;

implementation

{ TMotGraf }
procedure TMotGraf.Clear;
begin
  gControl.Canvas.Brush.Color := clBlack;
  gControl.Canvas.FillRect(0,0,gControl.Width,gControl.Height);
end;
function TMotGraf.XPant(x:Single): Integer; inline;   //INLINE Para acelerar las llamadas
//Función de la geometría del motor. Da la transformación lineal de la coordenada x.
begin
//   XPant := Round((x - x_cam) * Zoom + x_des);
  Result := Round(x+x_des);
end;
function TMotGraf.YPant(y:Single): Integer; inline;  //INLINE Para acelerar las llamadas
//Función de la geometría del motor. Da la transformación lineal de la coordenada y.
begin
//   YPant := Round((y - y_cam) * Zoom + y_des);
  Result := Round(gControl.Height-(y+y_des));
end;
procedure TMotGraf.XYpant(xv, yv: Single; var xp, yp: Integer);
//Devuelve las coordenadas de pantalla para un punto virtual (x,y,z).
begin
    xp := Xpant(xv);
    yp := Ypant(yv);
end;

procedure TMotGraf.SetPenColor(AValue: TColor);
begin
  cv.Pen.Color:=AValue;
end;
function TMotGraf.GetPenColor: TColor;
begin
  Result := cv.Pen.Color;
end;
procedure TMotGraf.Line(const x1, y1, x2, y2: Double);
begin
  cv.Line(XPant(x1), YPant(y1), XPant(x2), YPant(y2));
end;
constructor TMotGraf.Create(gContrl0: TGraphicControl);
begin
  gControl := gContrl0;
  cv := gControl.Canvas;
  x_des := 10;
  y_des := 10;
end;
destructor TMotGraf.Destroy;
begin
  inherited Destroy;
end;

end.

