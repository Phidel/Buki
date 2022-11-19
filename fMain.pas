unit fMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  PropFilerEh, PropStorageEh, sSkinManager, sSkinProvider,
  System.UITypes, Cromis.SimpleLog,
  dea.Status, Vcl.StdCtrls, sButton, Vcl.ExtCtrls, sPanel, Vcl.ComCtrls, sStatusBar,
  acProgressBar, ABSMain, Data.DB, DBGridEhGrouping,
  ToolCtrlsEh, DBGridEhToolCtrls, DynVarsEh, EhLibVCL, GridsEh, DBAxisGridsEh, DBGridEh,
  Vcl.Menus, sCheckBox, sEdit, sSpinEdit, sLabel;

type
  TMainForm = class(TForm)
    sSkinProvider1: TsSkinProvider;
    sSkinManager1: TsSkinManager;
    PropStorageEh1: TPropStorageEh;
    TopPanel: TsPanel;
    StartButton: TsButton;
    StopButton: TsButton;
    ProgressBar2: TsProgressBar;
    StatusBar1: TsStatusBar;
    ABSDatabase1: TABSDatabase;
    tbSettings: TABSTable;
    Grid1: TDBGridEh;
    dsOrders: TDataSource;
    tbOrders: TABSTable;
    TrayIcon1: TTrayIcon;
    TrayPopupMenu: TPopupMenu;
    NExit: TMenuItem;
    TimerCheckBox: TsCheckBox;
    Timer1: TTimer;
    IntervalSpinEdit: TsSpinEdit;
    ConnectLabel: TsLabel;
    SkypeCheckBox: TsCheckBox;
    SuvorovCheckBox: TsCheckBox;
    SummaEdit: TsSpinEdit;
    TimerMin: TTimer;
    SaveButton: TsButton;
    procedure Grid1GetCellParams(Sender: TObject; Column: TColumnEh; AFont:
      TFont; var Background: TColor; State: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Grid1DblClick(Sender: TObject);
    procedure IntervalSpinEditChange(Sender: TObject);
    procedure NExitClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TimerMinTimer(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
  private
    Status: TProgressBarConnector;
    procedure EnableButtons(AEnable: Boolean);
    procedure DoWork;
    procedure WMSysCommand(var AMessage: TWMSysCommand); message WM_SYSCOMMAND;
    function DoParse(url: string; var NewOrder: string): Boolean;
    function xGet(url: string): string;
  public
  end;

var
  MainForm: TMainForm;

implementation

uses
  dea.Tools, dea.Debug, uConst, System.StrUtils, dea.cl;

{$R *.dfm}


const
  Base_Url = 'https://buki.com.ua/';

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SimpleLog.RegisterLog('log', WorkingPath + main_log, 2000, 5, [lpTimestamp, lpType]);
  SimpleLog.LockType := ltProcess; // ltMachine, ltNone;
  MainForm.Caption := AppName;

  Log('start ' + AppName + ' - - - - - - - - - - - - - - - - - - - - -');

  StopButton.Visible := false;
  ProgressBar2.Position := 0;
  ProgressBar2.Step := 1;
  Status.Bind(StatusBar1, StopButton, 1);
  Status.Bind(StatusBar1, ProgressBar2, 2);
{$WARN SYMBOL_PLATFORM OFF}
  SaveStringOn := DebugHook <> 0;
{$WARN SYMBOL_PLATFORM ON}
  ABSDatabase1.DatabaseFileName := WorkingPath + 'data.abs';
  tbSettings.Open;
  tbSettings.IndexName := 'xName';
  tbSettings.First;
  tbOrders.Open;
  tbOrders.IndexName := 'xAppended'; // 'xOrderNo';
  tbOrders.First;

  TrayIcon1.Hint := MainForm.Caption;
  TrayIcon1.Visible := true;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ABSDatabase1.CloseDataSets;
  Log('finish');
end;

procedure TMainForm.Grid1GetCellParams(Sender: TObject; Column: TColumnEh;
  AFont: TFont; var Background: TColor; State: TGridDrawState);
begin
  // if tbOrders.FieldByName('bold').AsBoolean then
  // AFont.Color := clBlack
  // else
  // AFont.Color := clWebLightSkyBlue;

  // if (now - tbOrders.FieldByName('appended').AsDateTime) <= 1 then // ��������� ����� 24 � �����
  // AFont.Style := [fsBold];
  if tbOrders.FieldByName('comment').AsString = '' then
    if (Pos('� ����������', tbOrders.FieldByName('place').AsString) > 0) and
      (Pos('�����', tbOrders.FieldByName('place').AsString) > 0) then
      Background := $00E1F7FD // ������
    else
      Background := clWhite
  else
    Background := $F5FAFF;
end;

procedure TMainForm.EnableButtons(AEnable: Boolean);
begin
  StartButton.Enabled := AEnable;
end;

function TMainForm.xGet(url: string): string;
begin
  Result := '';
  ConnectLabel.Visible := false;
  Status.Update(arrow_down + url);
  try
    Result := clDownloadFileToString(Base_Url + url, Base_Url, false);
    Status.Update('');
  except
    on E: Exception do begin
      ConnectLabel.Caption := E.Message;
      ConnectLabel.Visible := true;
      Log(E.Message);
    end;
  end;
end;

procedure TMainForm.StartButtonClick(Sender: TObject);
begin
  Status.Stopped := false;
  EnableButtons(false);
  StopButton.Visible := true;
  ProgressBar2.Position := 0;
  ProgressBar2.Visible := true;
  try
    DoWork;
  finally
    Status.Update('ok');
    ProgressBar2.Visible := false;
    StopButton.Visible := false;
    EnableButtons(true);
  end;
end;

procedure TMainForm.StopButtonClick(Sender: TObject);
begin
  Status.Stopped := true;
  StopButton.Visible := false;
  Log('�������� �������������');
end;

procedure TMainForm.Grid1DblClick(Sender: TObject);
begin
  // link := 'https://buki.com.ua/zayavka/' + OrderNo; // �������� ������ ��� ������
  OpenUrl(ReplaceStr(tbOrders.FieldByName('link').AsString, '/zayavka/', '/z/')); // ��� ������
  OpenUrl(tbOrders.FieldByName('link').AsString);
end;

procedure TMainForm.IntervalSpinEditChange(Sender: TObject);
begin
  Timer1.Interval := IntervalSpinEdit.Value * 1000;
end;

procedure TMainForm.NExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  if TimerCheckBox.Checked then
    StartButton.Click;
end;

procedure TMainForm.TrayIcon1Click(Sender: TObject);
begin
  ShowWindow(Application.MainForm.Handle, SW_RESTORE);
  // ������� ����� ��������
  SetForegroundWindow(Application.MainForm.Handle);
  TrayIcon1.Visible := true;
end;

procedure TMainForm.WMSysCommand(var AMessage: TWMSysCommand);
begin
  if AMessage.CmdType = SC_MINIMIZE then begin
    // �������� ����� ������� ��� �������� (���� �������)
    Application.Minimize;
    ShowWindow(Application.MainForm.Handle, SW_HIDE);
    TrayIcon1.Visible := true;
  end
  else
    inherited;
end;

var
  AlreadyRun: Boolean = false;

procedure TMainForm.DoWork;
var
  NewOrder: string; // �� ����� ����� ������������������
begin
  if AlreadyRun then
    exit;
  AlreadyRun := true;
  try
    Screen.Cursor := crHourGlass;
    Grid1.StartLoadingStatus;
    try
      Grid1.SaveVertPos('orderno');

      NewOrder := '-';
      // DoParse('vacancy/matematyka/odesa/', NewOrder);
      DoParse('vacancy/matematyka/', NewOrder);
      DoParse('vacancy/vyshcha-matematyka/', NewOrder);
      // if SkypeCheckBox.Checked then begin
      DoParse('vacancy/prohramuvannia/', NewOrder);
      DoParse('vacancy/informatyka/', NewOrder);

      // DoParse('vacancy/matematyka/', NewOrder);
      // end;

      Grid1.RestoreVertPos('orderno');

      if NewOrder <> '-' then
        tbOrders.Locate('OrderNo', NewOrder, []);

      Grid1.Col := 7; // comment
    finally
      Screen.Cursor := crDefault;
      Grid1.FinishLoadingStatus;
    end;
  finally
    AlreadyRun := false;
  end;

  if NewOrder <> '-' then begin
    TrayIcon1Click(nil);
  end;
  Grid1.SetFocus;
end;

function TMainForm.DoParse(url: string; var NewOrder: string): Boolean;
var
  s, item: string;
  I, added: Integer;
  link: string;
  OrderNo: string;
  Desc, Price, place, subj, times: string;
  // lNew: Boolean; // ������� ����� - �����
  need: Boolean; // ����� �������� � ���� - �� ������ ��� � ����������� ������ � �.�.
  //Suvorov: Boolean; // �������� ������ ����������� �����
  List: TStringList;
  Sum: Integer;
begin
  Result := false;
  //Suvorov := SuvorovCheckBox.Checked;
  Sum := SummaEdit.Value;

  s := xGet(url); // ��������� �������� � ������������ (������ ������)
  if s = '' then
    exit;

  added := 0;
  tbOrders.IndexName := 'xOrderNo'; // desc
  tbOrders.DisableControls;
  List := TStringList.Create;
  try
    // >������ <
    // ="baseSalary" ...content="200 ���/���"

    ReprepList(s, '<div class="styles_vacancyItem__', '>���������', List); // ���� ����������
    ProgressBar2.Max := List.Count;
    for I := 0 to List.Count - 1 do begin
      ProgressBar2.Position := I + 1;
      item := StripText('href=''https://buki.com.ua/nz/''', List[I]);
      OrderNo := reprep(item, '>� ', '<');
      Price := reprep(item, '="baseSalary"*"value" content="', '"');

      subj := reprep(item, '<div itemProp="title">', '</div');
      times := HTMLStrip(reprep(item, '>���������*">', '</div'));

      // place := reprep(item, '<span>̳��� ����������*</div>', '</div');
      place := reprep(item, '>������� ������</p>', '�������');
      Desc := HTMLStrip(reprep(item, '>�������� ������� �</div>', '</div')) + ' '
        + HTMLStrip(reprep(item, '>��������� ����������</div><div*>', '</div'));

      place := Trim(HTMLStrip(place)); // ������, ��� (� ����������)
      subj := Trim(HTMLStrip(subj));
      Desc := ReplaceHTMLChars(HTMLStrip(Desc));

      link := 'https://buki.com.ua/zayavka/' + OrderNo; // �������� ������ ��� ������
      // https://buki.com.ua/z/348174/ - �������� ��� ������

      if (Pos('� ���������', place) > 0) and (Pos('�����', place) = 0) and (Pos('������', place) = 0) then
        need := false
      else
        if Pos('������', place) > 0 then
          need := true
        else
          need := (Pos('� ���������', place) > 0) {and
            (not Suvorov or (Pos('�����������', place) > 0) or (Pos('�����', place) > 0))};
      if (Pos('������', subj) > 0) or (Pos('5 - 6 �����', subj) > 0) then // ������� ������ 1-4
        need := false;

      if not need then
        continue;

      if tbOrders.FindKey([OrderNo]) then begin
        tbOrders.Edit;
        // lNew := false;
      end
      else begin
        tbOrders.Append;
        tbOrders.FieldByName('appended').AsDateTime := now;
        tbOrders.FieldByName('OrderNo').AsString := OrderNo;
        Inc(added);
        Status.Update('���������: ' + added.ToString);
        // lNew := true;
        if (StrToFloatDef(StrBeforePos(' ', Price), 0) >= Sum) or Result { ��� ��� ����� � ���������� ����� } then begin
          Result := true; // �������� ����� ����� - ���������
          NewOrder := OrderNo; // �������� �����-������ �� ����������� ������� ��� ����������������
        end;
      end;
      tbOrders.FieldByName('price').AsString := Price;
      tbOrders.FieldByName('link').AsString := link;
      tbOrders.FieldByName('desc').AsString := Desc;
      tbOrders.FieldByName('place').AsString := place; // � ����������. �����������, �����������; �� ������
      tbOrders.FieldByName('subj').AsString := subj;
      tbOrders.FieldByName('bold').AsBoolean := need;
      tbOrders.FieldByName('times').AsString := times;
      tbOrders.Post;

      (* ������ ��� ��������� �� ��������� �������
        s := xGet('z/' + OrderNo); // �������� ��� ������, ������ ������ https://i.imgur.com/VbJwmGk.png

        if s = '' then
        Break;

        tbOrders.Edit;
        // � ��������� ����� ��������� ������� ��� �������: ����������, �����, ����������
        title := reprep(s, '<h1>', '</h1');
        tbOrders.FieldByName('name').AsString := Trim(StrAfterLastPos(',', title));
        // ������� �������: �������� 1 ���. / 2 �� ����.
        tbOrders.FieldByName('times').AsString := HTMLStrip(reprep(s, '���������*</span>', '</span'));
        tbOrders.Post; *)
    end;
  finally
    List.Free;
    tbOrders.IndexName := 'xAppended'; // desc
    tbOrders.EnableControls;
  end;
end;

procedure TMainForm.SaveButtonClick(Sender: TObject);
begin
  PropStorageEh1.SaveProperties;
end;

procedure TMainForm.TimerMinTimer(Sender: TObject);
begin
  TimerMin.Enabled := false;
  if LowerCase(ParamStr(1)) = '/min' then begin
    Application.Minimize;
    ShowWindow(Application.MainForm.Handle, SW_HIDE);
    TrayIcon1.Visible := true;
  end;
end;

initialization

SetDefStorage;

end.
