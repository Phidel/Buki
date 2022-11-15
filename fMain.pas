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
  dea.Tools, uConst, System.StrUtils, dea.cl;

{$R *.dfm}


const
  Base_Url = 'https://buki.com.ua/';

procedure TMainForm.Grid1GetCellParams(Sender: TObject; Column: TColumnEh;
  AFont: TFont; var Background: TColor; State: TGridDrawState);
begin
  if tbOrders.FieldByName('bold').AsBoolean then
    AFont.Color := clBlack
  else
    AFont.Color := clGray;

  if (now - tbOrders.FieldByName('appended').AsDateTime) <= 1 then // добавлено менее 24 ч назад
    AFont.Style := [fsBold];
  if tbOrders.FieldByName('comment').AsString = '' then
    if (Pos('у репетитора', tbOrders.FieldByName('place').AsString) > 0) and
      (Pos('Одеса', tbOrders.FieldByName('place').AsString) > 0) then
      Background := $00E1F7FD // желтым
    else
      Background := clWhite
end;

procedure TMainForm.EnableButtons(AEnable: Boolean);
begin
  StartButton.Enabled := AEnable;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SimpleLog.RegisterLog('log', WorkingPath + main_log, 2000, 5, [lpTimestamp, lpType]);
  SimpleLog.LockType := ltProcess; // ltMachine, ltNone;
  Log('start ' + MainForm.Caption + ' - - - - - - - - - - - - - - - - - - - - -');

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
  tbOrders.IndexName := 'xOrderNo';
  tbOrders.First;

  TrayIcon1.Hint := MainForm.Caption;
  TrayIcon1.Visible := true;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ABSDatabase1.CloseDataSets;
  Log('finish');
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
  Log('Прервано пользователем');
end;

procedure TMainForm.Grid1DblClick(Sender: TObject);
begin
  OpenUrl(tbOrders.FieldByName('link').AsString);
// link := 'https://buki.com.ua/zayavka/' + OrderNo; // доступно только при логине
  OpenUrl(ReplaceStr( tbOrders.FieldByName('link').AsString,'/zayavka/','/z/')); // без логина
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
  // Сделать форму активной
  SetForegroundWindow(Application.MainForm.Handle);
  TrayIcon1.Visible := true;
end;

procedure TMainForm.WMSysCommand(var AMessage: TWMSysCommand);
begin
  if AMessage.CmdType = SC_MINIMIZE then begin
    // Свернуть форму включая все дочерние (если открыты)
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
  NewOrder: string; // на какой ордер спозиционироваться
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
      DoParse('vacancy/matematyka/odesa/', NewOrder);
      DoParse('vacancy/vyshcha-matematyka/odesa/', NewOrder);
      if SkypeCheckBox.Checked then begin
        DoParse('vacancy/prohramuvannia/', NewOrder);
        DoParse('vacancy/matematyka/', NewOrder);
      end;

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

// <div class="order_wrapper"><div class="title_block"><span>№ 143926</span><span class="rate">150 грн/год</span></div><div class="orders-item"><div class="row"><div class="key"><span>Предмет / Рівень підготовки</span></div><div class="value"><b>Математика / <i>Підготовка до ЗНО</i></b></div></div><div class="row"><div class="key"><span>Місце проведення занять</span></div><div class="value">Одеса, <i>Суворівський, Котовського</i><br>у репетитора</div></div><div class="row"><div class="key"><span>Додаткова інформація</span></div><div class="value">Понедельник, среда. ученик - курсант</div></div><div class="row"><div class="wrapper"><div class="breads_inline"><a class="b_btn btn-warning"
// href="https://buki.com.ua/nz/">Бажаю взяти заявку</a></div><div class="breads_inline"><a class="b_btn btn-green" href="https://buki.com.ua/z/143926/">Детальніше »</a></div></div></div></div></div>
// <div class="order_wrapper"><div class="title_block"><span>№ 143812</span><span class="rate">170 грн/год</span></div><div class="orders-item"><div class="row"><div class="key"><span>Предмет / Рівень підготовки</span></div><div class="value"><b>Математика / <i>5 - 6 класи</i></b></div></div><div class="row"><div class="key"><span>Місце проведення занять</span></div><div class="value">Одеса, <i>Приморський, Центр</i><br>у репетитора</div></div><div class="row"><div class="wrapper"><div class="breads_inline"><a class="b_btn btn-warning" href="https://buki.com.ua/nz/">Бажаю взяти заявку</a></div><div class="breads_inline"><a class="b_btn btn-green" href="https://buki.com.ua/z/143812/">Детальніше »</a></div></div></div></div></div>
// <div class="order_wrapper"><div class="title_block"><span>№ 143727</span><span class="rate">150 грн/год</span></div><div class="orders-item"><div class="row"><div class="key"><span>Предмет / Рівень підготовки</span></div><div class="value"><b>Математика / <i>7 - 9 класи</i></b></div></div><div class="row"><div class="key"><span>Місце проведення занять</span></div><div class="value">Одеса, <i>Суворівський, Котовського</i><br>у репетитора</div></div><div class="row"><div class="key"><span>Додаткова інформація</span></div><div class="value">Пн, ср, пятница----; 8 класс.</div></div><div class="row"><div class="wrapper"><div class="breads_inline"><a class="b_btn btn-warning" href="https://buki.com.ua/nz/">Бажаю взяти заявку</a></div><div class="breads_inline">
// <a class="b_btn btn-green" href="https://buki.com.ua/z/143727/">Детальніше »</a></div></div></div></div></div><div class="earn">
// <div class="order_wrapper"><div class="title_block"><span>№ 143668</span><span class="rate">150 грн/год</span></div><div class="orders-item"><div class="row"><div class="key"><span>Предмет / Рівень підготовки</span></div><div class="value"><b>Математика / <i>10-11 класи</i></b></div></div><div class="row"><div class="key"><span>Місце проведення занять</span></div><div class="value">Одеса, <i>Київський, Великий Фонтан, Київський, Середній Фонтан</i><br>у репетитора</div></div><div class="row"><div class="wrapper"><div class="breads_inline"><a class="b_btn btn-warning" href="https://buki.com.ua/nz/">Бажаю взяти заявку</a></div><div class="breads_inline"><a class="b_btn btn-green" href="https://buki.com.ua/z/143668/">Детальніше »</a></div></div></div></div></div>
// <div class="order_wrapper"><div class="title_block"><span>№ 141984</span><span class="rate">170 грн/год</span></div><div class="orders-item"><div class="row"><div class="key"><span>Предмет / Рівень підготовки</span></div><div class="value"><b>Математика / <i>Підготовка до ЗНО</i></b></div></div><div class="row"><div class="key"><span>Місце проведення занять</span></div><div class="value">Одеса, <i>Малиновський, Молдаванка, Приморський, Центр</i><br>у репетитора</div></div><div class="row"><div class="key"><span>Додаткова інформація</span></div><div class="value">в любой день после 17:00 Підготовка до ЗНО</div></div><div class="row"><div class="wrapper"><div class="breads_inline"><a class="b_btn btn-warning" href="https://buki.com.ua/nz/">Бажаю взяти заявку</a>
// </div><div class="breads_inline"><a class="b_btn btn-green" href="https://buki.com.ua/z/141984/">Детальніше »</a></div></div></div></div></div></div>
function TMainForm.DoParse(url: string; var NewOrder: string): Boolean;
var
  s, item: string;
  I, added: Integer;
  link, title: string;
  OrderNo: string;
  Desc, Price, place, subj: string;
  // lNew: Boolean; // текущий ордер - новый
  need: Boolean; // нужно записать в базу - по скайпу или в суворовском районе и т.п.
  Suvorov: Boolean; // выделять только Суворовский район
  List: TStringList;
  Sum: Integer;
begin
  Result := false;
  Suvorov := SuvorovCheckBox.Checked;
  Sum := SummaEdit.Value;

  s := xGet(url); // загружаем страницу с объявлениями (только первую)
  if s = '' then
    exit;

  added := 0;
  tbOrders.IndexName := 'xOrderNo'; // desc
  tbOrders.DisableControls;
  List := TStringList.Create;
  try
    ReprepList(s, '<div class="order_wrapper"', '>Детальніше', List); // одно объявление
    ProgressBar2.Max := List.Count;
    for I := 0 to List.Count - 1 do begin
      ProgressBar2.Position := I + 1;
      item := StripText('href=''https://buki.com.ua/nz/''', List[I]);
      OrderNo := reprep(item, '<span>№ ', '</span');
      Price := reprep(item, 'class="rate"*itemprop="value" content="', '"');
      subj := reprep(item, '<span>Предмет*</div>', '</div');
      // place := reprep(item, '<span>Місце проведення*</div>', '</div');
      place := reprep(item, 'addressLocality*content=''', '''');
      Desc := reprep(item, '<span>Допомога потрібна з</span></div>', '</div');

      place := Trim(HTMLStrip(place));
      subj := Trim(HTMLStrip(subj));
      Desc := HTMLStrip(Desc);

      link := 'https://buki.com.ua/zayavka/' + OrderNo; // доступно только при логине
      // https://buki.com.ua/z/348174/ - доступно без логина

      if (Pos('у репетитор', place) > 0) and (Pos('Одеса', place) = 0) and (Pos('онлайн', place) = 0) then
        need := false
      else
        if Pos('онлайн', place) > 0 then
        need := true
      else
        need := (Pos('у репетитор', place) > 0) and
          (not Suvorov or (Pos('Суворівський', place) > 0) or (Pos('Котов', place) > 0));
      if (Pos('Молодш', subj) > 0) or (Pos('5 - 6 класи', subj) > 0) then // младшие классы 1-4
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
        Status.Update('добавлено: ' + added.ToString);
        // lNew := true;
        if (StrToFloatDef(StrBeforePos(' ', Price), 0) >= Sum) or Result { был уже ордер с подходящей ценой } then begin
          Result := true; // добавлен новый ордер - всплываем
          NewOrder := OrderNo; // запомним какой-нибудь из добавленных заказов для позиционирования
        end;
      end;
      tbOrders.FieldByName('price').AsString := Price;
      tbOrders.FieldByName('link').AsString := link;
      tbOrders.FieldByName('desc').AsString := Desc;
      tbOrders.FieldByName('place').AsString := place; // у репетитора. Суворівський, Котовського; по скайпу
      tbOrders.FieldByName('subj').AsString := subj;
      tbOrders.FieldByName('bold').AsBoolean := need;

      tbOrders.Post;

      s := xGet('z/' + OrderNo); // доступно без логина, пример заявки https://i.imgur.com/VbJwmGk.png

      if s = '' then
        Break;

      tbOrders.Edit;
      // в заголовке после последней запятой имя ученика: Математика, Харків, Владислава
      title := reprep(s, '<h1>', '</h1');
      tbOrders.FieldByName('name').AsString := Trim(StrAfterLastPos(',', title));
      // частота занятий: например 1 год. / 2 на тижд.
      tbOrders.FieldByName('times').AsString := HTMLStrip(reprep(s, 'Тривалість*</span>', '</span'));
      tbOrders.Post;
    end;
  finally
    List.Free;
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

фильр по сумме
следить за заявкой
запросы в другом потоке
