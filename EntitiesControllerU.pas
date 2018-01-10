unit EntitiesControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons;

type

  [MVCPath('/api')]
  TEntitiesController = class(TMVCController)
  public
    [MVCPath('/($entityname)')]
    [MVCHTTPMethods([httpGET])]
    procedure GetEntities(const entityname: string);

    [MVCPath('/($entityname)/($entityid)')]
    [MVCHTTPMethods([httpGET])]
    procedure GetEntity(const entityname: string; const entityid: string);

    [MVCPath('/($entityname)')]
    [MVCHTTPMethods([httpPOST])]
    procedure CreateEntities(const entityname: string);
  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils, DataAccessLayerU,
  JsonDataObjects;

{ TEntitiesController }

procedure TEntitiesController.CreateEntities(const entityname: string);
var
  lJson: TJsonObject;
  lOID: string;
begin
  lJson := TJSONObject.Parse(Context.Request.Body) as TJSONObject;
  try
    Context.Response.StatusCode := http_status.Created;
    lOID := GetDAL.CreateEntity(entityname, lJson);
    Context.Response.SetCustomHeader('X-REF', '/' + entityname + '/' + lOID);
  finally
    lJson.Free;
  end;
end;

procedure TEntitiesController.GetEntities(const entityname: string);
begin
  try
    Render(GetDAL.GetEntities(entityname), True);
  except
    on E: ENotFound do
    begin
      Render(http_status.NotFound, e.Message);
    end;
  end;
end;

procedure TEntitiesController.GetEntity(const entityname: string; const entityid: string);
begin
  try
    Render(GetDAL.GetEntity(entityname, entityid), True);
  except
    on E: ENotFound do
    begin
      Render(http_status.NotFound, e.Message);
    end;
  end;
end;

end.
