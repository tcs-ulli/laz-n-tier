unit ClientUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  DBCtrls, StdCtrls, ExtCtrls, NetConnection, OnLineQuery, DB, SynaCSockets;

type

  { TClientForm }

  TClientForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Datasource1: TDatasource;
    DBGrid1: TDBGrid;
    Image1: TImage;
    OnlineConnection1: TOnlineConnection;
    OnlineQuery1: TOnlineQuery;
    ScrollBox1: TScrollBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Datasource1DataChange(Sender: TObject; Field: TField);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OnlineConnection1Connect(Sender: TObject; FSSock: TCSocketClient;
      Value: string);
    procedure OnlineConnection1DataAvailable(Sender: TObject;
      ClientThrd: TObject; FDSock: TCSocketClient; ReceiveData: string;
      Error: word);
    procedure OnlineConnection1SocketClose(Sender: TObject;
      FSSock: TCSocketClient; Value: string);
  private
    { private declarations }
    IsBiolife: boolean;
    procedure ReDrawDBImages;
  public
    { public declarations }
  end;

var
  ClientForm: TClientForm;

implementation

{$R *.lfm}

{ TClientForm }

procedure TClientForm.ReDrawDBImages;
var
  mStream: TMemoryStream;
  bb: string;
  a: TStream;
begin
  if IsBiolife = False then
    Exit;
  if OnlineQuery1.RecordCount < 1 then
    Exit;
  ;
  Datasource1.OnDataChange := @Datasource1DataChange;
  mStream := TMemoryStream.Create;
  bb := OnlineQuery1.FieldByName('Graphic').AsString;
  if Length(bb) = 0 then
    Exit;
  mStream.WriteBuffer(bb[9], Length(bb) - 8);
  mStream.Position := 0;
  //  mStream.SaveToFile('T:\dataonline\lazdemo\aaa.bmp');     Exit;
  Image1.Picture.Clear;
  Image1.Picture.LoadFromStream(mStream);
  Image1.AutoSize := True;
  mStream.Free;
end;

procedure TClientForm.Button4Click(Sender: TObject);
begin
  IsBiolife := False;
  with OnlineQuery1 do
  begin
    Close;
    SQL.Text := 'select * from customer';
    Open;
  end;
end;

procedure TClientForm.Button1Click(Sender: TObject);
begin
  with OnlineQuery1 do
  begin
    Close;
    SQL.Text := 'insert into industry values (8001, ' + QuotedStr('industry1') + ', ' + QuotedStr('industry2') + ')';
    ExecSQL;
    ShowMessage(IntToStr(OnlineQuery1.RowsAffected));
  end;
end;

procedure TClientForm.Button2Click(Sender: TObject);
var i: integer;
begin
  with OnlineQuery1 do
  begin
    Close;
    SQL.Clear;
    for i := 0 to 6 do
      SQL.Add('insert into industry values (' + IntToStr(8006 + i) + ', ' + QuotedStr('industry1') + ', ' + QuotedStr('industry2') + ')');
    ExecScript;
    ShowMessage(IntToStr(OnlineQuery1.RowsAffected));
  end;
end;

procedure TClientForm.Button5Click(Sender: TObject);
begin
  IsBiolife := False;
  with OnlineQuery1 do
  begin
    try
      Close;
      SQL.Text := 'select * from vendors000';
      Open;
    except
      on E: Exception do
        Application.MessageBox(PChar(E.Message + '.'), 'Error', $10);
    end;
  end;
end;

procedure TClientForm.Button6Click(Sender: TObject);
begin
  IsBiolife := True;
  with OnlineQuery1 do
  begin
    Close;
    SQL.Text := 'select * from biolife';
    Open;
  end;
  ReDrawDBImages;
end;

procedure TClientForm.Datasource1DataChange(Sender: TObject; Field: TField);
begin
  ReDrawDBImages;
end;

procedure TClientForm.FormCreate(Sender: TObject);
begin

end;

procedure TClientForm.FormShow(Sender: TObject);
begin
  IsBiolife := True;
  if OnlineQuery1.Active then
    ReDrawDBImages;
end;

procedure TClientForm.OnlineConnection1Connect(Sender: TObject;
  FSSock: TCSocketClient; Value: string);
begin
  Caption := 'Connected';
end;

procedure TClientForm.OnlineConnection1DataAvailable(Sender: TObject;
  ClientThrd: TObject; FDSock: TCSocketClient; ReceiveData: string; Error: word);
begin

end;

procedure TClientForm.OnlineConnection1SocketClose(Sender: TObject;
  FSSock: TCSocketClient; Value: string);
begin
  Caption := 'DisConnected';
end;

end.

