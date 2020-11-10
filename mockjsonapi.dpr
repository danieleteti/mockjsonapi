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

program mockjsonapi;

{$APPTYPE CONSOLE}

uses
  MVCFramework.Console,
  System.SysUtils,
  MVCFramework.Logger,
  MVCFramework.Commons,
  MVCFramework.REPLCommandsHandlerU,
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
  TextColor(Red);
  WriteLn(' | |\/| | |  | | |    |  < _   | |\___ \| |  | | . ` | / /\ \ |  ___/ | |  ');
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
  lCustomHandler: TMVCCustomREPLCommandsHandler;
  lCmd: string;
begin
  TextColor(White);
  TextBackground(Black);
  Logo;
  TextColor(Green);
  Writeln('** Built with DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);
  TextColor(White);
  Writeln('Usage help: https://github.com/danieleteti/mockjsonapi');
  if ParamCount >= 1 then
    lCmd := ParamStr(1)
  else
    lCmd := 'start';

  lCustomHandler := function(const Value: string; const Server: TIdHTTPWebBrokerBridge; out Handled: Boolean): THandleCommandResult
    begin
      Handled := False;
      Result := THandleCommandResult.Unknown;
    end;

  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.KeepAlive := True;
    LServer.MaxConnections := 0;
    LServer.ListenQueue := 200;

    WriteLn('Write "quit" or "exit" to shutdown the server');
    repeat
      if lCmd.IsEmpty then
      begin
        TextColor(TConsoleColor.Green);
        write('-> ');
        TextColor(Yellow);
        ReadLn(lCmd)
      end;
      try
        TextColor(TConsoleColor.Yellow);
        case HandleCommand(lCmd.ToLower, LServer, lCustomHandler) of
          THandleCommandResult.Continue:
            begin
              Continue;
            end;
          THandleCommandResult.Break:
            begin
              Break;
            end;
          THandleCommandResult.Unknown:
            begin
              SaveColors;
              TextColor(TConsoleColor.Red);
              REPLEmit('Unknown command: ' + lCmd);
              RestoreSavedColors;
            end;
        end;
      finally
        lCmd := '';
      end;
    until false;

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
