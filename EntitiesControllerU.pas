// ***************************************************************************
//
// MockJSONAPI
//
// Copyright (c) 2018 Daniele Teti
//
// https://github.com/danieleteti/mockjsonapi
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// *************************************************************************** }

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
