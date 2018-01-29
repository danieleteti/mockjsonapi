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
  end;

implementation

{ TSecurityHeadersMiddleware }

procedure TSecurityHeadersMiddleware.OnAfterControllerAction(
  AContext: TWebContext; const AActionName: string; const AHandled: Boolean);
begin
end;

procedure TSecurityHeadersMiddleware.OnBeforeControllerAction(
  AContext: TWebContext; const AControllerQualifiedClassName,
  AActionName: string; var AHandled: Boolean);
begin

end;

procedure TSecurityHeadersMiddleware.OnBeforeRouting(AContext: TWebContext;
  var AHandled: Boolean);
begin
  AContext.Response.SetCustomHeader('X-XSS-Protection', '1; mode = block');
  AContext.Response.SetCustomHeader('X-Content-Type-Options', 'nosniff');
end;

end.
