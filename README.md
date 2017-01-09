MotGraf 0.2
===========

Librería en Lazarus, para el dibujo de objetos en 2 y 3 dimensiones.

MotGraf es una librería que encapsula las funciones de dibujo, para transformarlas a un espacio de trabajo 2D o 3D.

Trabaja asociada a un control TGraphicControl, como un PaintBox, que es a donde dirige su salida, usando el lienzo "Canvas", somo soporte gráfico. Mo utiliza OpenGL u otra librería externa.

Para su uso debe incluirse la unidad MotGraf2d o MotGraf3d, de acuerdo al tipo de gráficos que se desee usar. Luego debe crearse un objeto de la clase "TMotGraf" y asociarla a un componente como un PaintBox. Finalmente se deben usar los métodos del motor, para generar los dibujos, de preferecnia usando el evento Paint del control asociado:

```
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
  mot.Line(0,0,0, 100,100, 0);
end;
```

Esta librería aún está en fase de desarrollo. La versión 3D, está más avanzada que la 2D.