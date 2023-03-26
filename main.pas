
unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ExtCtrls, ActnList, FileUtil, AsyncProcess, Process;

type
  TToolCommand = class
  private
    FCommand: string;
  public
    constructor CreateToolCommand(const ACommand: string);
    property Command: string read FCommand write FCommand;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    BTExecute: TButton;
    BTSelectInput: TButton;
    BTSelectOutput: TButton;
    CBShell: TCheckBox;
    CBRedirectErr: TCheckBox;
    CBDirIn: TCheckBox;
    CBDirOut: TCheckBox;
    EDInput: TLabeledEdit;
    EDOutput: TLabeledEdit;
    EDCommand: TLabeledEdit;
    LBOperations: TListBox;
    MMMenu: TMainMenu;
    MIUI: TMenuItem;
    MIQuit: TMenuItem;
    MMOutput: TMemo;
    OFDialog: TOpenDialog;
    SFDialog: TSaveDialog;
    ODDialog: TSelectDirectoryDialog;
    procedure AsyncProcess1ReadData(Sender: TObject);
    procedure BTExecuteClick(Sender: TObject);
    procedure BTSelectInputClick(Sender: TObject);
    procedure BTSelectOutputClick(Sender: TObject);
    procedure EDOutputChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure LBOperationsClick(Sender: TObject);
    procedure MIQuitClick(Sender: TObject);
  private
    FCommandPattern: string;
    procedure BuildCommandLine;
  public

  end;

var
  Form1: TForm1;

implementation

uses
  DefaultTranslator;

{$R *.lfm}

resourcestring
  rs_wav_to_aac_q098_nero = 'Convert WAV to AAC q0.98 using Nero';
  rs_wav_to_aac_q099_nero = 'Convert WAV to AAC q0.99 using Nero';
  rs_dir_to_rar_rar       = 'Pack directory to RAR format using RAR';

constructor TToolCommand.CreateToolCommand(const ACommand: string);
{ https://stackoverflow.com/a/16128750 }
begin
  inherited Create;
  FCommand := ACommand;
end;

{ TForm1 }

procedure TForm1.AsyncProcess1ReadData(Sender: TObject);
{ https://forum.lazarus.freepascal.org/index.php/topic,53716.msg397941.html#msg397941 }
var
  LProcess: TAsyncProcess absolute Sender;
  LStr: string = '';
  LNum: integer;
begin
  LNum := LProcess.Output.NumBytesAvailable;
  SetLength(LStr, LNum);
  LNum := LProcess.Output.Read(LStr[1], LNum);
  MMOutput.Append(Trim(LStr));
end;

procedure TForm1.BTExecuteClick(Sender: TObject);
var
  LProcess: TAsyncProcess;
  LList: TStringList;
  i: integer;
begin
  MMOutput.Clear;

  LProcess := TAsyncProcess.Create(Self);
  LProcess.OnReadData := @AsyncProcess1ReadData;
  LProcess.Options := [poUsePipes, poNoConsole];

  if CBShell.Checked then
  begin
    LProcess.Executable := FindDefaultExecutablePath('sh');
    LProcess.Parameters.Add('-c');
    LProcess.Parameters.Add(EDCommand.Text);
  end else
  begin
    LList := TStringList.Create;
    LList.Delimiter := ' ';
    LList.QuoteChar := '"';
    LList.DelimitedText := EDCommand.Text;
    LProcess.Executable := LList[0];
    for i := 1 to Pred(LList.Count) do
      LProcess.Parameters.Add(LList[i]);
    LList.Free;
  end;

  LProcess.Execute;
end;

procedure TForm1.BTSelectInputClick(Sender: TObject);
begin
  case CBDirIn.Checked of
    FALSE:
      if OFDialog.Execute then
        EDInput.Text := OFDialog.Filename;
    TRUE:
      if ODDialog.Execute then
        EDInput.Text := ODDialog.Filename;
  end;
end;

procedure TForm1.BTSelectOutputClick(Sender: TObject);
begin
  case CBDirOut.Checked of
    FALSE:
      if SFDialog.Execute then
        EDOutput.Text := SFDialog.Filename;
    TRUE:
      if ODDialog.Execute then
        EDOutput.Text := ODDialog.Filename;
  end;
end;

procedure TForm1.EDOutputChange(Sender: TObject);
begin
{$IFDEF DEBUG}
  if Sender is TLabeledEdit then
    WriteLn('DEBUG TForm1.EDOutputChange Sender = ', TLabeledEdit(Sender).Name)
  else if Sender is TCheckBox then
    WriteLn('DEBUG TForm1.EDOutputChange Sender = ', TCheckBox(Sender).Name)
  else
    WriteLn('DEBUG TForm1.EDOutputChange Sender = ?');
{$ENDIF}
  CBRedirectErr.Enabled := CBShell.Checked;
  CBRedirectErr.Checked := CBRedirectErr.Checked and CBRedirectErr.Enabled;
  BuildCommandLine;
end;

procedure TForm1.FormActivate(Sender: TObject);
{ https://www.developpez.net/forums/d2089611/autres-langages/pascal/lazarus/ajuster-hauteur-d-fenetre-contenant-barre-menu/#post11610251 }
var
  LMenuHeight: integer;
begin
  LMenuHeight := Self.Height - Self.ClientHeight;
  Self.Height := MMOutput.Top + MMOutput.Height + 8 + LMenuHeight;
  MMOutput.Anchors := [akTop, akLeft, akRight, akBottom];
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: integer;
begin
  for i := 0 to Pred(LBOperations.Items.Count) do
    LBOperations.Items.Objects[i].Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCommandPattern := EmptyStr;
  LBOperations.Items.AddObject(rs_wav_to_aac_q098_nero, TToolCommand.CreateToolCommand('nero -q 0.98 -lc -if "_INPUT_" -of "_OUTPUT_"'));
  LBOperations.Items.AddObject(rs_wav_to_aac_q099_nero, TToolCommand.CreateToolCommand('nero -q 0.99 -lc -if "_INPUT_" -of "_OUTPUT_"'));
  LBOperations.Items.AddObject(rs_dir_to_rar_rar, TToolCommand.CreateToolCommand('rar a -ma4 -dh -m1 -md64k -htc -ow -s- -mt1 -ep1 -scUL -tsma -o- -cfg- -ierr -idp "_OUTPUT_" "_INPUT_"'));
end;

procedure TForm1.LBOperationsClick(Sender: TObject);
var
  LToolCommand: TToolCommand;
begin
  if LBOperations.ItemIndex <> -1 then
  begin
    LToolCommand := TToolCommand(LBOperations.Items.Objects[LBOperations.ItemIndex]);
    FCommandPattern := LToolCommand.Command;
  end;
end;

procedure TForm1.MIQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.BuildCommandLine;

  function InsertFileNames(const ACommandPattern, AInputFile, AOutputFile: string): string;
  begin
    result := StringReplace(ACommandPattern, '_INPUT_', AInputFile, [rfIgnoreCase]);
    result := StringReplace(result, '_OUTPUT_', AOutputFile, [rfIgnoreCase]);
  end;

begin
  if  (Length(FCommandPattern) > 0)
  and (Length(EDInput.Text)    > 0)
  and (Length(EDOutput.Text)   > 0) then
  begin
    EDCommand.Text := InsertFileNames(FCommandPattern, EDInput.Text, EDOutput.Text);

    if CBShell.Checked then
    begin
      if CBRedirectErr.Checked then
        EDCommand.Text := EDCommand.Text + ' 2>&1'
    end;
  end;
end;

end.

