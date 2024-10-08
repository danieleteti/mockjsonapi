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

unit WebModuleU;

interface

uses System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  TMainWM = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVC: TMVCEngine;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TMainWM;

implementation

{$R *.dfm}

uses System.IOUtils, MVCFramework.Commons, MVCFramework.Middleware.CORS,
  EntitiesControllerU,
  SecurityHeadersMiddlewareU;

procedure TMainWM.WebModuleCreate(Sender: TObject);
begin
  FMVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      // session timeout (0 means session cookie)
      Config[TMVCConfigKey.SessionTimeout] := '0';
      // default content-type
      Config[TMVCConfigKey.DefaultContentType] :=
        TMVCConstants.DEFAULT_CONTENT_TYPE;
      // default content charset
      Config[TMVCConfigKey.DefaultContentCharset] :=
        TMVCConstants.DEFAULT_CONTENT_CHARSET;
      // unhandled actions are permitted?
      Config[TMVCConfigKey.AllowUnhandledAction] := 'false';
      // default view file extension
      Config[TMVCConfigKey.DefaultViewFileExtension] := 'html';
      // view path
      Config[TMVCConfigKey.ViewPath] := 'templates';
      // Enable Server Signature in response
      Config[TMVCConfigKey.ExposeServerSignature] := 'true';

      Config[TMVCConfigKey.LoadSystemControllers] := 'false';
    end);
  FMVC
    .AddController(TEntitiesController)
    .AddMiddleware(TCORSMiddleware.Create)
    .AddMiddleware(TSecurityHeadersMiddleware.Create);
end;

procedure TMainWM.WebModuleDestroy(Sender: TObject);
begin
  FMVC.Free;
end;

end.
