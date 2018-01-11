unit EntitiesControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons;

type

  [MVCPath('/api')]
  TEntitiesController = class(TMVCController)
  public
    [MVCPath('/($resourcename)')]
    [MVCHTTPMethods([httpGET])]
    procedure GetEntities(const resourcename: string);

    [MVCPath('/($resourcename)/($entityid)')]
    [MVCHTTPMethods([httpGET])]
    procedure GetEntity(const resourcename: string; const entityid: string);

    [MVCPath('/($resourcename)/($entityid)')]
    [MVCHTTPMethods([httpDELETE])]
    procedure DeleteEntity(const resourcename: string; const entityid: string);

    [MVCPath('/($resourcename)/($entityid)')]
    [MVCHTTPMethods([httpPUT])]
    procedure UpdateEntity(const resourcename: string; const entityid: string);

    [MVCPath('/($resourcename)')]
    [MVCHTTPMethods([httpDELETE])]
    procedure DeleteResource(const resourcename: string);

    [MVCPath('/($resourcename)')]
    [MVCHTTPMethods([httpPOST])]
    procedure CreateEntities(const resourcename: string);
  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils, DataAccessLayerU,
  JsonDataObjects;

{ TEntitiesController }

procedure TEntitiesController.CreateEntities(const resourcename: string);
var
  lJson: TJsonObject;
  lOID: string;
begin
  lJson := TJSONObject.Parse(Context.Request.Body) as TJSONObject;
  try
    Context.Response.StatusCode := http_status.Created;
    lOID := GetDAL.CreateEntity(resourcename, lJson);
    Context.Response.SetCustomHeader('X-REF', '/api/' + resourcename + '/' + lOID);
  finally
    lJson.Free;
  end;
end;

procedure TEntitiesController.DeleteEntity(const resourcename, entityid: string);
begin
  GetDAL.DeleteEntity(resourcename, entityid);
  Context.Response.StatusCode := http_status.OK;
end;

procedure TEntitiesController.DeleteResource(const resourcename: string);
begin
  GetDAL.DeleteResource(resourcename);
  StatusCode := http_status.OK;
end;

procedure TEntitiesController.GetEntities(const resourcename: string);
begin
  try
    Render(GetDAL.GetEntities(resourcename), True);
  except
    on E: ENotFound do
    begin
      Render(http_status.NotFound, e.Message);
    end;
  end;
end;

procedure TEntitiesController.GetEntity(const resourcename: string; const entityid: string);
begin
  try
    Render(GetDAL.GetEntity(resourcename, entityid), True);
  except
    on E: ENotFound do
    begin
      Render(http_status.NotFound, e.Message);
    end;
  end;
end;

procedure TEntitiesController.UpdateEntity(const resourcename,
  entityid: string);
var
  lJson: TJsonObject;
  lOID: string;
begin
  lJson := TJSONObject.Parse(Context.Request.Body) as TJSONObject;
  try
    GetDAL.UpdateEntity(lJson, resourcename, entityid);
    Context.Response.StatusCode := http_status.OK;
  finally
    lJson.Free;
  end;
end;

end.
