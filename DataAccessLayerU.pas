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
    function GetEntity(const EntityName: string; const EntityID: string): TJsonObject;
    function GetEntities(const EntityName: string): TJsonArray;
    function CreateEntity(const EntityName: string; const JsonObject: TJsonObject): string;
    procedure UpdateEntity(const JsonData: TJSONObject; const EntityName, EntityID: string);
  end;

  TDALService = class(TInterfacedObject, IDALService)
  private
    class var cLock: TObject;

  const
    cJSONFileName: string = 'data.json';
  protected
    function GetJSONObjectByOID(const JsonData: TJSONObject; const EntityName, EntityID: string): TJsonObject;
    function GetNewOID: string;
    procedure ExecWithLock(const Proc: TProc<TJSONObject>; const Action: TExecAction = TExecAction.ReadOnly);
    function GetEntity(const EntityName: string; const EntityID: string): TJsonObject;
    function GetEntities(const EntityName: string): TJsonArray;
    function CreateEntity(const EntityName: string; const JsonObject: TJsonObject): string;
    procedure UpdateEntity(const JsonData: TJSONObject; const EntityName, EntityID: string);
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

function TDALService.CreateEntity(const EntityName: string; const JsonObject: TJsonObject): string;
var
  lRes: string;
begin
  lRes := '';
  ExecWithLock(
    procedure(JSONData: TJsonObject)
    var
      lObj: TJsonObject;
    begin
      lObj := JSONData.A[EntityName].AddObject;
      lObj.Assign(JsonObject);
      if not lObj.Contains(OID_KEY_NAME) then
        lObj.S[OID_KEY_NAME] := GetNewOID;
      lRes := lObj.S[OID_KEY_NAME];
    end, TExecAction.ReadAndWrite);
  Result := lRes;
end;

class destructor TDALService.Destroy;
begin
  FreeAndNil(cLock);
end;

procedure TDALService.ExecWithLock(const Proc: TProc<TJSONObject>; const Action: TExecAction);
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
        raise EInvalidJSON.Create('data.json is not a valid json file. ' + e.Message);
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
    procedure(JSONObject: TJsonObject)
    begin
      lRes := TJsonArray.Create;
      lRes.Assign(JSONObject.A[EntityName]);
    end);
  Result := lRes;
end;

function TDALService.GetEntity(const EntityName: string; const EntityID: string): TJsonObject;
var
  lRes: TJsonObject;
begin
  Result := nil;
  ExecWithLock(
    procedure(JSONObject: TJsonObject)
    var
      lJSON: TJsonObject;
    begin
      lJSON := GetJSONObjectByOID(JSONObject, EntityName, EntityID);
      lRes := TJsonObject.Create;
      lRes.Assign(lJSON);
    end);
  Result := lRes;
end;

function TDALService.GetJSONObjectByOID(

  const
  JsonData: TJSONObject;

const
  EntityName, EntityID: string): TJsonObject;
var
  lArr: TJsonArray;
  I: Integer;
begin
  Result := nil;
  lArr := JsonData.A[EntityName];
  for I := 0 to lArr.Count - 1 do
  begin
    if lArr.O[I].S[OID_KEY_NAME] = EntityID then
    begin
      Result := lArr.O[I];
      break;
    end;
  end;
  if not Assigned(Result) then
    raise ENotFound.Create('Not Found');
end;

function TDALService.GetNewOID: string;
begin
  Result := TGuid.NewGuid.ToString.Replace('{', '').Replace('}', '');
end;

procedure TDALService.UpdateEntity(

  const
  JsonData: TJSONObject;

const
  EntityName, EntityID: string);
begin
  ExecWithLock(
    procedure(FullJsonData: TJSONObject)
    var
      lJSON: TJsonObject;
    begin
      lJSON := GetJSONObjectByOID(FullJsonData, EntityName, EntityID);
      lJson.Assign(JsonData);
      lJson.S[OID_KEY_NAME] := EntityID;
    end, TExecAction.ReadAndWrite);
end;

end.
