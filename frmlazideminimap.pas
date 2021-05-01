{  Minimap for lazarus ide.

  Copyright (C) 2021 Domingo Galmés dgalmesp@gmail.com

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}

unit frmlazideminimap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazLoggerBase, FileUtil, Forms, Controls, Graphics,
  Dialogs, LCLType, Spin, StdCtrls, Buttons, IDECommands, IDEWindowIntf,
  LazIDEIntf, SrcEditorIntf, MenuIntf, SynEdit, SynEditMarkupSpecialLine, SynEditTypes, SynEditMiscClasses;

type

  { TlazIdeMiniMap }

  TlazIdeMiniMap = class(TForm)
    btnFGColor: TColorButton;
    btnSave: TBitBtn;
    btnColor: TColorButton;
    lbFontSize: TLabel;
    seFontSize: TSpinEdit;
    edMiniMap: TSynEdit;
    procedure btnSaveClick(Sender: TObject);
    procedure edMiniMapSpecialLineMarkup(Sender: TObject; Line: integer;
      var Special: boolean; Markup: TSynSelectedColor);
    //procedure edMiniMapSpecialLineColors(Sender: TObject; Line: integer;
    //  var Special: boolean; var FG, BG: TColor);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edMiniMapClick(Sender: TObject);
    procedure seFontSizeChange(Sender: TObject);
    procedure btnColorColorChanged(Sender: TObject);
    procedure ActiveEditorChanged(Sender: TObject);
    procedure OnEditorDestroy(Sender: TObject);
  private
    { private declarations }
    FSourceEditor: TSourceEditorInterface;
    FSourceSynEdit: TCustomSynEdit;
    procedure AfterScroll(Sender: TObject; EventType: TSynScrollEvent; dx, dy: integer;
      const rcScroll, rcClip: TRect);
    procedure CenterMiniMap;
    procedure SetUpMiniMap(aOriginEditor: TCustomSynEdit);
    procedure ResetMiniMap;
    procedure LoadSettings;
    procedure SaveSettings;
  public
    { public declarations }
  end;

var
  lazIdeMiniMap1: TlazIdeMiniMap;
  lazIdeMiniMap1Creator: TIDEWindowCreator; // set by Register procedure

procedure ShowlazIdeMiniMap1(Sender: TObject);
procedure Register;

implementation

uses
  LazConfigStorage, laz_xmlcfg,BaseIDEIntf,
  IDEMsgIntf, IDEExternToolIntf,
  IDEHelpIntf, IDEImagesIntf;

{$R *.lfm}

resourcestring
  SMiniMapMenuCaption = 'MiniMap';

procedure ShowlazIdeMiniMap1(Sender: TObject);
begin
  IDEWindowCreators.ShowForm(lazIdeMiniMap1Creator.FormName, True);
end;

procedure CreatelazIdeMiniMap1(Sender: TObject; aFormName: string; var AForm: TCustomForm;
  DoDisableAutoSizing: boolean);
begin
  // sanity check to avoid clashing with another package that has registered a window with the same name
  if CompareText(aFormName, 'lazIdeMiniMap') <> 0 then
  begin
    DebugLn(['ERROR: CreatelazIdeMiniMap: there is already a form with this ' + 'name']);
    exit;
  end;
  IDEWindowCreators.CreateForm(AForm, TlazIdeMiniMap, DoDisableAutoSizing,
    LazarusIDE.OwningComponent);
  AForm.Name := aFormName;
  lazIdeMiniMap1 := AForm as TlazIdeMiniMap;
end;

procedure Register;
var
  CmdCatViewMenu: TIDECommandCategory;
  ViewlazIdeMiniMap1Command: TIDECommand;
  MenuItemCaption: string;
begin
  // register shortcut and menu item
  MenuItemCaption := SMiniMapMenuCaption;
  // search shortcut category
  CmdCatViewMenu := IDECommandList.FindCategoryByName(CommandCategoryViewName);
  // register shortcut
  ViewlazIdeMiniMap1Command := RegisterIDECommand(CmdCatViewMenu, 'ViewlazIdeMiniMap',
    MenuItemCaption, IDEShortCut(VK_UNKNOWN, []), // <- set here your default shortcut
    CleanIDEShortCut, nil, @ShowlazIdeMiniMap1);
  // register menu item in View menu
  RegisterIDEMenuCommand(itmViewMainWindows,
    'ViewlazIdeMiniMap1',
    MenuItemCaption, nil, nil, ViewlazIdeMiniMap1Command);
  // register dockable Window
  lazIdeMiniMap1Creator := IDEWindowCreators.Add('lazIdeMiniMap', @CreatelazIdeMiniMap1,
    nil, '100', '100', '420', '620'  // default place at left=100, top=100, right=300, bottom=300
    // you can also define percentage values of screen or relative positions, see wiki
    );
end;

{
* _______ _               _____    _      __  __ _       _ __  __              *
*|__   __| |             |_   _|  | |    |  \/  (_)     (_)  \/  |             *
*   | |  | |     __ _ ____ | |  __| | ___| \  / |_ _ __  _| \  / | __ _ _ __   *
*   | |  | |    / _` |_  / | | / _` |/ _ \ |\/| | | '_ \| | |\/| |/ _` | '_ \  *
*   | |  | |___| (_| |/ / _| || (_| |  __/ |  | | | | | | | |  | | (_| | |_) | *
*   |_|  |______\__,_/___|_____\__,_|\___|_|  |_|_|_| |_|_|_|  |_|\__,_| .__/  *
*                                                                      | |     *
*                                                                      |_|     *
}

procedure TlazIdeMiniMap.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SourceEditorManagerIntf.UnRegisterChangeEvent(semEditorActivate, @ActiveEditorChanged);
  SourceEditorManagerIntf.UnRegisterChangeEvent(semEditorDestroy, @OnEditorDestroy);
  ResetMiniMap;
  //FSourceSynEdit := nil;
  //FSourceEditor := nil;
  CloseAction := caFree;
end;

procedure TlazIdeMiniMap.btnSaveClick(Sender: TObject);
begin
  SaveSettings;
end;

procedure TlazIdeMiniMap.FormCreate(Sender: TObject);
begin
  LoadSettings;
  btnSave.LoadGlyphFromStock(idButtonSave);
  if btnSave.Glyph.Empty then
    IDEImages.AssignImage(btnSave, 'laz_save');
  edMiniMap.ReadOnly := True;
  edMiniMap.OnClick := @edMiniMapClick;
  edMiniMap.OnSpecialLineMarkup := @edMiniMapSpecialLineMarkup;
  //edMiniMap.OnSpecialLineColors:=@edMiniMapSpecialLineColors;
  edMiniMap.Gutter.Parts[4].Visible := False; // code folding disabled.
  FSourceSynEdit := nil;
  FSourceEditor := SourceEditorManagerIntf.ActiveEditor;
  if FSourceEditor <> nil then
    FSourceSynEdit := TCustomSynEdit(FSourceEditor.EditorControl);
  SetUpMiniMap(FSourceSynEdit);
  SourceEditorManagerIntf.RegisterChangeEvent(semEditorActivate, @ActiveEditorChanged);
  SourceEditorManagerIntf.RegisterChangeEvent(semEditorDestroy, @OnEditorDestroy);
end;

procedure TlazIdeMiniMap.FormDestroy(Sender: TObject);
begin
end;

{ //deprecated.
procedure TlazIdeMiniMap.edMiniMapSpecialLineColors(Sender: TObject; Line: integer;
  var Special: boolean; var FG, BG: TColor);
begin
  if edOrigin = nil then
    Exit;
  if (Line>=edOrigin.TopLine) and (Line<=edOrigin.TopLine + edOrigin.LinesInWindow) then
  begin
    Special:=true;
    BG:=btnColor.ButtonColor;
    FG:=btnFGColor.ButtonColor;
  end;
end;
}

procedure TlazIdeMiniMap.edMiniMapSpecialLineMarkup(Sender: TObject;
  Line: integer; var Special: boolean; Markup: TSynSelectedColor);
begin
  if FSourceSynEdit = nil then
    Exit;
  if (Line >= FSourceSynEdit.TopLine) and (Line <= FSourceSynEdit.TopLine + FSourceSynEdit.LinesInWindow) then
  begin
    Special := True;
    Markup.Background:=btnColor.ButtonColor;
    Markup.Foreground:=btnFGColor.ButtonColor;
  end;
end;

procedure TlazIdeMiniMap.SetUpMiniMap(aOriginEditor: TCustomSynEdit);
begin
  if aOriginEditor = nil then
    Exit;
  edMiniMap.Font := aOriginEditor.Font;
  edMiniMap.Font.Size := seFontSize.Value;
  //edMiniMap.ShareOptions:= [eosShareMarks];
  edMiniMap.ShareTextBufferFrom(aOriginEditor);
  aOriginEditor.RegisterScrollEventHandler(@AfterScroll, [peAfterScroll]);
  edMiniMap.Highlighter := aOriginEditor.Highlighter;
  edMiniMap.RightEdge := aOriginEditor.RightEdge;
  edMiniMap.RightEdgeColor := aOriginEditor.RightEdgeColor;
  CenterMiniMap;
end;

procedure TlazIdeMiniMap.ResetMiniMap;
var
  wI: integer;
begin
  if (FSourceSynEdit <> nil) then
  begin
    // search if editor exits now
    for wI := 0 to Pred(SourceEditorManagerIntf.SourceEditorCount) do
    begin
      if SourceEditorManagerIntf.SourceEditors[wI].EditorControl = FSourceSynEdit then
      begin
        FSourceSynEdit.UnRegisterScrollEventHandler(@AfterScroll);
        break;
      end;
    end;
  end;
  edMiniMap.UnShareTextBuffer;
  edMiniMap.Lines.Add('');  //HACK: WORKARROUD BUG IN SHARETEXTBUFFER.
end;

procedure TlazIdeMiniMap.edMiniMapClick(Sender: TObject);
begin
  if FSourceSynEdit = nil then
    Exit;
  FSourceSynEdit.TopLine := edMiniMap.CaretY - (FSourceSynEdit.LinesInWindow div 2); //centered.
  CenterMiniMap;
end;

procedure TlazIdeMiniMap.btnColorColorChanged(Sender: TObject);
begin
  edMiniMap.Invalidate;
end;

procedure TlazIdeMiniMap.ActiveEditorChanged(Sender: TObject);
begin
  // this function is called more than one time when change the tab.
  //ShowMessage('Changed  Sender:'+ IntToHex(Cardinal(Sender),8)+' FSourceEditor: '+IntToHex(Cardinal(FSourceEditor),8)+'Active: '+IntTOHex(Cardinal(SourceEditorManagerIntf.ActiveEditor),8));
  if TSourceEditorInterface(Sender) = FSourceEditor then
    Exit;
  ResetMiniMap;
  FSourceSynEdit := nil;
  FSourceEditor := TSourceEditorInterface(Sender);
  if FSourceEditor = nil then
    Exit;
  FSourceSynEdit := TCustomSynEdit(FSourceEditor.EditorControl);
  SetUpMiniMap(FSourceSynEdit);
end;

procedure TlazIdeMiniMap.OnEditorDestroy(Sender: TObject);
begin
  //ShowMessage('Destroy');
  if TSourceEditorInterface(Sender) = FSourceEditor then
  begin
    ResetMiniMap;
    FSourceSynEdit := nil;
    FSourceEditor := nil;
  end;
end;

procedure TlazIdeMiniMap.seFontSizeChange(Sender: TObject);
begin
  edMiniMap.Font.Size := seFontSize.Value;
  edMiniMap.Invalidate;
  CenterMiniMap;
end;

procedure TlazIdeMiniMap.AfterScroll(Sender: TObject; EventType: TSynScrollEvent; dx, dy: integer;
  const rcScroll, rcClip: TRect);
begin
  CenterMiniMap;
end;

procedure TlazIdeMiniMap.CenterMiniMap;
begin
  if FSourceSynEdit = nil then
    Exit;
  if SourceEditorManagerIntf.ActiveEditor = nil then
    Exit;
  if SourceEditorManagerIntf.ActiveEditor.EditorControl <> FSourceSynEdit then
    Exit;
  if FSourceSynEdit.TopLine < edMiniMap.TopLine then
    edMiniMap.TopLine := FSourceSynEdit.TopLine
  else if (FSourceSynEdit.TopLine + FSourceSynEdit.LinesInWindow) > (edMiniMap.TopLine + edMiniMap.LinesInWindow) then
    edMiniMap.TopLine := FSourceSynEdit.TopLine + FSourceSynEdit.LinesInWindow - edMiniMap.LinesInWindow;
  edMiniMap.Invalidate;
end;

const
  CONFIGURATION_FILE = 'minimap.xml';

procedure TlazIdeMiniMap.SaveSettings;
var
  Config: TConfigStorage;
const
  Version = 1;
begin
  try
    Config := GetIDEConfigStorage(CONFIGURATION_FILE, False);
    try
      // store the version number so future extensions can handle old config files
      Config.SetDeleteValue('Version', Version, 0);
      // store string variable "SomeValue"
      // if SomeValue has the default value the entry is not stored,
      // so only the differences to the default are stored.
      // This way the xml is kept short and defaults may change in future.
      Config.SetDeleteValue('FontSize', seFontSize.Value, 3);
      Config.SetDeleteValue('Color', btnColor.ButtonColor, $00fffbff);
      Config.SetDeleteValue('ColorFG', btnFGColor.ButtonColor, clBlack);
    finally
      Config.Free;
    end;
  except
    on E: Exception do
    begin
      AddIDEMessage(mluWarning, 'Saving minimap.xml failed: ' + E.Message);
    end;
  end;
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TlazIdeMiniMap.LoadSettings;
var
  Config: TConfigStorage;
  Version: integer;
begin
  try
    Config := GetIDEConfigStorage(CONFIGURATION_FILE, True);
    try
      Version := Config.GetValue('Version', 1);
      seFontSize.Value := Config.GetValue('FontSize', 3);
      btnColor.ButtonColor := Config.GetValue('Color', $00bffbff);
      btnFGColor.ButtonColor := Config.GetValue('ColorFG', clBlack);
    finally
      Config.Free;
    end;
  except
    on E: Exception do
    begin
      AddIDEMessage(mluWarning, 'Loading minimap.xml failed: ' + E.Message);
    end;
  end;
end;

end.
