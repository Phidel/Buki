unit uConst;

interface

uses
  Classes, SysUtils, StrUtils, dea.Tools, Cromis.SimpleLog, PropFilerEh, PropStorageEh;

const
  main_log = 'log.txt';
  arrow_down = char(8595); // '↓';

procedure Log(s: string = ''; IntParam: Integer = MaxInt); overload;
procedure Log(s: string; BoolParam: Boolean); overload;
procedure Log(s: string; FloatParam: Double); overload;
procedure Err(s: string = '');

procedure SetDefStorage;

const
  BuildNo = {$I BuildNo.inc};

function AppName: string;

implementation

procedure Log(s: string = ''; IntParam: Integer = MaxInt);
begin
  SimpleLog.LogInfo('log', s + IfThen(IntParam = MaxInt, '', ': ' + IntToStr(IntParam)));
end;

procedure Log(s: string; BoolParam: Boolean); overload;
begin
  SimpleLog.LogInfo('log', s + ': ' + IfThen(BoolParam, 'Y', 'N'));
end;

procedure Log(s: string; FloatParam: Double); overload;
begin
  SimpleLog.LogInfo('log', s +  ': ' + FloatToStr(FloatParam));
end;

procedure Err(s: string = '');
begin
  SimpleLog.LogError('log', s);
end;

procedure SetDefStorage;
var
  IniPropStorageMan1: TIniPropStorageManEh;
begin
  IniPropStorageMan1 := TIniPropStorageManEh.Create(nil);
  IniPropStorageMan1.IniFileName := ExtractFilepath(GetModuleName(0)) + 'settings.ini';
  SetDefaultPropStorageManager(IniPropStorageMan1);
end;

var
  BuildNoAsString: string = '';

function AppName: string;
begin
  if BuildNoAsString = '' then
    BuildNoAsString := FormatFloat('0.0#', BuildNo);
  Result := 'Buki, ' + BuildNoAsString;
end;

end.