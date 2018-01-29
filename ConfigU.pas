unit ConfigU;

interface

uses
  JsonDataObjects;

type
  TConfig = class sealed
  private
    class var sInstance: TConfig;

  var
    fConfigDict: TJsonObject;
    constructor Create;
    destructor Destroy;
    class function GetConfig: TConfig; static;
  public
    function GetString(const ConfigName: string): string;
    function GetInteger(const ConfigName: string): Integer;
    class destructor Destroy;
    class property Instance: TConfig read GetConfig;
  end;

implementation

uses
  System.IOUtils, System.SysUtils;

{ TConfig }

constructor TConfig.Create;
begin
  fConfigDict := nil;
  if TFile.Exists('config.json') then
  begin
    fConfigDict := TJsonObject.ParseUtf8(TFile.ReadAllText('config.json')) as TJsonObject;
  end
  else
    raise Exception.Create('Cannot find "config.json" file');
end;

destructor TConfig.Destroy;
begin
  fConfigDict.Free;
  fConfigDict := nil;
end;

class destructor TConfig.Destroy;
begin
  sInstance.Free;
end;

class function TConfig.GetConfig: TConfig;
begin
  if not Assigned(sInstance) then
    sInstance := TConfig.Create;
  Result := sInstance;
end;

function TConfig.GetInteger(const ConfigName: string): Integer;
begin
  Result := fConfigDict.I[ConfigName];
end;

function TConfig.GetString(const ConfigName: string): string;
begin
  Result := fConfigDict.S[ConfigName];
end;

end.
