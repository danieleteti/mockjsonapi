// ***************************************************************************
//
// MockJSONAPI
//
// Copyright (c) 2020 Daniele Teti
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

unit DataAccessLayerU;

interface

uses
  JsonDataObjects, System.SysUtils, System.Classes;

const
  OID_KEY_NAME = '_oid';

type
  ENotFound = class(Exception)

  end;

  EInvalidJSON = class(Exception)

  end;

{$SCOPEDENUMS ON}

  TExecAction = (readonly, ReadAndWrite);

  IDALService = interface
    ['{B03B66EF-C713-403B-A09B-8EDE0A6385B3}']
    function GetEntity(const EntityName: string; const EntityID: string)
      : TJsonObject;
    procedure DeleteEntity(const EntityName: string; const EntityID: string);
    procedure DeleteResource(const EntityName: string);
    function GetEntities(const EntityName: string): TJsonArray;
    function CreateEntity(const EntityName: string;
      const JsonObject: TJsonObject): string;
    procedure UpdateEntity(const JsonData: TJsonObject;
      const EntityName, EntityID: string);
  end;

  TDALService = class(TInterfacedObject, IDALService)
  private
    class var cLock: TObject;

  const
    cJSONFileName: string = 'data.json';
  protected
    function GetJSONObjectByOID(const JsonData: TJsonObject;
      const EntityName, EntityID: string): TJsonObject;
    function IndexOfObject(const JsonData: TJsonObject;
      const EntityName, EntityID: string): Integer;
    function GetNewOID: string;
    procedure ExecWithLock(const Proc: TProc<TJsonObject>;
      const Action: TExecAction = TExecAction.ReadOnly);
    function GetEntity(const EntityName: string; const EntityID: string)
      : TJsonObject;
    procedure DeleteEntity(const EntityName: string; const EntityID: string);
    procedure DeleteResource(const EntityName: string);
    function GetEntities(const EntityName: string): TJsonArray;
    function CreateEntity(const EntityName: string;
      const JsonObject: TJsonObject): string;
    procedure UpdateEntity(const JsonData: TJsonObject;
      const EntityName, EntityID: string);
  public
    class constructor Create;
    class destructor Destroy;

  end;

function GetDAL: IDALService;

implementation

function GetDAL: IDALService;
begin
  Result := TDALService.Create;
end;

{ TDALService }

class constructor TDALService.Create;
begin
  cLock := TObject.Create;
end;

function TDALService.CreateEntity(const EntityName: string;
  const JsonObject: TJsonObject): string;
var
  lRes: string;
begin
  lRes := '';
  ExecWithLock(
    procedure(JsonData: TJsonObject)
    var
      lObj: TJsonObject;
    begin
      lObj := JsonData.A[EntityName].AddObject;
      lObj.Assign(JsonObject);
      if not lObj.Contains(OID_KEY_NAME) then
        lObj.S[OID_KEY_NAME] := GetNewOID;
      lRes := lObj.S[OID_KEY_NAME];
    end, TExecAction.ReadAndWrite);
  Result := lRes;
end;

procedure TDALService.DeleteEntity(const EntityName, EntityID: string);
begin
  ExecWithLock(
    procedure(JsonData: TJsonObject)
    var
      lEntityIndex: Integer;
    begin
      lEntityIndex := IndexOfObject(JsonData, EntityName, EntityID);
      if lEntityIndex > -1 then
        JsonData.A[EntityName].Delete(lEntityIndex);
      { no exception if no data found }
    end, TExecAction.ReadAndWrite);
end;

procedure TDALService.DeleteResource(const EntityName: string);
begin
  ExecWithLock(
    procedure(JsonData: TJsonObject)
    begin
      JsonData.ExtractArray(EntityName).Free;
    end, TExecAction.ReadAndWrite);
end;

class destructor TDALService.Destroy;
begin
  FreeAndNil(cLock);
end;

procedure TDALService.ExecWithLock(const Proc: TProc<TJsonObject>;
const Action: TExecAction);
var
  lJSONData: TJsonObject;
begin
  TMonitor.Enter(cLock);
  try
    try
      lJSONData := TJsonObject.ParseFromFile(cJSONFileName) as TJsonObject;
    except
      on E: EJsonParserException do
      begin
        raise EInvalidJSON.Create('data.json is not a valid json file. ' +
          E.Message);
      end;
    end;
    try
      Proc(lJSONData);
      if Action = TExecAction.ReadAndWrite then
      begin
        lJSONData.SaveToFile(cJSONFileName, False);
      end;
    finally
      lJSONData.Free;
    end;
  finally
    TMonitor.Exit(cLock);
  end;
end;

function TDALService.GetEntities(const EntityName: string): TJsonArray;
var
  lRes: TJsonArray;
begin
  lRes := nil;
  ExecWithLock(
    procedure(JsonObject: TJsonObject)
    begin
      lRes := TJsonArray.Create;
      lRes.Assign(JsonObject.A[EntityName]);
    end);
  Result := lRes;
end;

function TDALService.GetEntity(const EntityName: string; const EntityID: string)
  : TJsonObject;
var
  lRes: TJsonObject;
begin
  ExecWithLock(
    procedure(JsonObject: TJsonObject)
    var
      lJSON: TJsonObject;
    begin
      lJSON := GetJSONObjectByOID(JsonObject, EntityName, EntityID);
      lRes := TJsonObject.Create;
      lRes.Assign(lJSON);
    end);
  Result := lRes;
end;

function TDALService.GetJSONObjectByOID(const JsonData: TJsonObject;
const EntityName, EntityID: string): TJsonObject;
var
  lArr: TJsonArray;
  lEntityIndex: Integer;
begin
  lArr := JsonData.A[EntityName];
  lEntityIndex := IndexOfObject(JsonData, EntityName, EntityID);
  if lEntityIndex > -1 then
  begin
    Result := lArr.O[lEntityIndex];
  end
  else
  begin
    raise ENotFound.Create('Not Found');
  end;
end;

function TDALService.GetNewOID: string;
begin
  Result := TGuid.NewGuid.ToString.Replace('{', '').Replace('}', '');
end;

function TDALService.IndexOfObject(const JsonData: TJsonObject;
const EntityName, EntityID: string): Integer;
var
  lArr: TJsonArray;
  I: Integer;
begin
  Result := -1;
  lArr := JsonData.A[EntityName];
  for I := 0 to lArr.Count - 1 do
  begin
    if lArr.O[I].S[OID_KEY_NAME] = EntityID then
    begin
      Result := I;
      break;
    end;
  end;
end;

procedure TDALService.UpdateEntity(

  const JsonData: TJsonObject;

const EntityName, EntityID: string);
begin
  ExecWithLock(
    procedure(FullJsonData: TJsonObject)
    var
      lJSON: TJsonObject;
    begin
      lJSON := GetJSONObjectByOID(FullJsonData, EntityName, EntityID);
      lJSON.Assign(JsonData);
      lJSON.S[OID_KEY_NAME] := EntityID;
    end, TExecAction.ReadAndWrite);
end;

end.
