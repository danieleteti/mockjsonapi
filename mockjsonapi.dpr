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
  WebModuleU in 'WebModuleU.pas' {MyWebModule: TWebModule} ,
  DataAccessLayerU in 'DataAccessLayerU.pas';

{$R *.res}

const
  VERSION = '1.0.1';

procedure Logo;
begin
  TextColor(Red);
  WriteLn;
  WriteLn('  __  __  ____   _____ _  __    _  _____  ____  _   _          _____ _____ ');
  WriteLn(' |  \/  |/ __ \ / ____| |/ /   | |/ ____|/ __ \| \ | |   /\   |  __ \_   _|');
  WriteLn(' | \  / | |  | | |    | '' /    | | (___ | |  | |  \| |  /  \  | |__) || |  ');
  WriteLn(' | |\/| | |  | | |    |  < _   | |\___ \| |  | | . ` | / /\ \ |  ___/ | |  ');
  WriteLn(' | |  | | |__| | |____| . \ |__| |____) | |__| | |\  |/ ____ \| |    _| |_ ');
  WriteLn(' |_|  |_|\____/ \_____|_|\_\____/|_____/ \____/|_| \_/_/    \_\_|   |_____|');
  WriteLn('                                                                           ');
  WriteLn(' version ' + VERSION);
  WriteLn;
end;

procedure RunServer(APort: Integer);
var
  lServer: TIdHTTPWebBrokerBridge;
  lCustomHandler: TMVCCustomREPLCommandsHandler;
  lCmd: string;
begin
  SetMode(TConsoleMode.Bright);
  TextBackground(Black);
  Logo;
  TextColor(Green);
  Writeln('** Built with DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);
  TextColor(White);
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

    { more info about MaxConnections
      http://www.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=TIdCustomTCPServer_MaxConnections.html }
    LServer.MaxConnections := 0;

    { more info about ListenQueue
      http://www.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=TIdCustomTCPServer_ListenQueue.html }
    LServer.ListenQueue := 200;

    WriteLn('Write "quit" or "exit" to shutdown the server');
    repeat
      if lCmd.IsEmpty then
      begin
        TextColor(Yellow);
        write('-> ');
        TextColor(White);
        ReadLn(lCmd)
      end;
      try
        TextColor(TConsoleColor.Cyan);
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
              TextColor(TConsoleColor.Cyan);
              REPLEmit('Unknown command: ' + lCmd);
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
    RunServer(8080);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  try
    ResetConsole;
  except
  end;

end.
