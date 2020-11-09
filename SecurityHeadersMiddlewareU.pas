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

unit SecurityHeadersMiddlewareU;

interface

uses
  MVCFramework;

type
  TSecurityHeadersMiddleware = class(TInterfacedObject, IMVCMiddleware)
  public
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      var AHandled: Boolean);
    procedure OnAfterControllerAction(AContext: TWebContext;
      const AActionName: string; const AHandled: Boolean);
    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);

  end;

implementation

{ TSecurityHeadersMiddleware }

uses ConfigU;

procedure TSecurityHeadersMiddleware.OnAfterControllerAction
  (AContext: TWebContext; const AActionName: string; const AHandled: Boolean);
begin
end;

procedure TSecurityHeadersMiddleware.OnAfterRouting(AContext: TWebContext;
  const AHandled: Boolean);
begin
  //
end;

procedure TSecurityHeadersMiddleware.OnBeforeControllerAction
  (AContext: TWebContext; const AControllerQualifiedClassName,
  AActionName: string; var AHandled: Boolean);
begin
  //
end;

procedure TSecurityHeadersMiddleware.OnBeforeRouting(AContext: TWebContext;
  var AHandled: Boolean);
begin
  AContext.Response.SetCustomHeader('X-XSS-Protection', '1; mode = block');
  AContext.Response.SetCustomHeader('X-Content-Type-Options', 'nosniff');
  AContext.Response.SetCustomHeader('X-MOCK-JSON-API', VERSION);
end;

end.
