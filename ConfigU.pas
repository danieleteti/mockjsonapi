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

unit ConfigU;

interface

uses
  JsonDataObjects;

const
  VERSION = '1.1.0';

type
  TConfig = class sealed
  private
    class var sInstance: TConfig;

  var
    fConfigDict: TJsonObject;
    constructor Create;
    class function GetConfig: TConfig; static;
  public
    destructor Destroy; override;
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
    fConfigDict := TJsonObject.ParseUtf8(UTF8Encode(TFile.ReadAllText('config.json'))) as TJsonObject;
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
