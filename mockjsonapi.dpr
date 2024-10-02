// ***************************************************************************
//
// MockJSONAPI
//
// Copyright (c) 2020-2023 Daniele Teti
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

program mockjsonapi;

{$APPTYPE CONSOLE}

uses
  MVCFramework.Console,
  System.SysUtils,
  MVCFramework.Logger,
  MVCFramework.Commons,
  MVCFramework.Signal,
  Web.ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
  EntitiesControllerU in 'EntitiesControllerU.pas',
  WebModuleU in 'WebModuleU.pas' {MainWM: TWebModule},
  DataAccessLayerU in 'DataAccessLayerU.pas',
  ConfigU in 'ConfigU.pas',
  SecurityHeadersMiddlewareU in 'SecurityHeadersMiddlewareU.pas';

{$R *.res}



procedure Logo;
begin
  WriteLn;
  TextColor(DarkRed);
  WriteLn('  __  __  ____   _____ _  __    _  _____  ____  _   _          _____ _____ ');
  WriteLn(' |  \/  |/ __ \ / ____| |/ /   | |/ ____|/ __ \| \ | |   /\   |  __ \_   _|');
  WriteLn(' | \  / | |  | | |    | '' /    | | (___ | |  | |  \| |  /  \  | |__) || |  ');
  WriteLn(' | |\/| | |  | | |    |  < _   | |\___ \| |  | | . ` | / /\ \ |  ___/ | |  ');
  TextColor(Red);
  WriteLn(' | |  | | |__| | |____| . \ |__| |____) | |__| | |\  |/ ____ \| |    _| |_ ');
  WriteLn(' |_|  |_|\____/ \_____|_|\_\____/|_____/ \____/|_| \_/_/    \_\_|   |_____|');
  WriteLn('                                                                           ');
  TextColor(White);
  WriteLn(' version ' + VERSION);
  WriteLn;
end;

procedure RunServer(APort: Integer = 8080);
var
  lServer: TIdHTTPWebBrokerBridge;
begin
  TextColor(White);
  TextBackground(Black);
  Logo;
  TextColor(Green);
  Writeln('** Built with DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);
  TextColor(White);
  Writeln('Usage help: https://github.com/danieleteti/mockjsonapi');

  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.KeepAlive := True;
    LServer.MaxConnections := 0;
    LServer.ListenQueue := 200;
    lServer.Active := True;
    Writeln('mockjsonapi is listening on http://localhost:' + APort.ToString);
    TextColor(Red);
    WriteLn('CTRL+C to EXIT');
    WaitForTerminationSignal;
    TextColor(Yellow);
    WriteLn('bye bye...');
  finally
    LServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 1024;
    RunServer(TConfig.Instance.GetInteger('port'));
  except
    on E: Exception do
    begin
      LogE(E.ClassName + ': ' + E.Message);
      Writeln(E.ClassName, ': ', E.Message);
    end;
  end;
  try
    ResetConsole;
  except
  end;

end.
