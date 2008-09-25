(**

  This module contains a class which represents the application interface
  using a tree view for the profile outline and a list view to display the
  highlighted sections of the profiles information in a list report.

  @Author  David Hoyle
  @Date    25 Sep 2008
  @Version 1.0

**)
unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, ComCtrls, ExtCtrls, Menus, ImgList, ToolWin, ProgressForm,
  AggregateList;

type
  (** A class to represent the main application form. **)
  TfrmMainForm = class(TForm)
    mmMenu: TMainMenu;
    mmiFile: TMenuItem;
    mmiHelp: TMenuItem;
    tvProfileTree: TTreeView;
    sptrSplitter: TSplitter;
    lvProfileInformation: TListView;
    alActions: TActionList;
    actFileOpen: TAction;
    actFileExit: TAction;
    actHelpAbout: TAction;
    mmiFileOpen: TMenuItem;
    mmiFileSep1: TMenuItem;
    mmiFileExit: TMenuItem;
    mmiHelpAbout: TMenuItem;
    ilImages: TImageList;
    dlgOpen: TOpenDialog;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    actFileClose: TAction;
    mmiFileClose: TMenuItem;
    actFileDelete: TAction;
    mmiFileDelete: TMenuItem;
    actFileRefresh: TAction;
    mmiFileRefresh: TMenuItem;
    tbtnFileClose: TToolButton;
    tbtnFileRefresh: TToolButton;
    tbtnFileDelete: TToolButton;
    pnlTreeProfile: TPanel;
    lvAggregateList: TListView;
    sptSortable: TSplitter;
    pnlSortable: TPanel;
    actHelpCheckForUpdates: TAction;
    CheckForUpdates1: TMenuItem;
    N1: TMenuItem;
    ilSortImages: TImageList;
    procedure actFileRefreshExecute(Sender: TObject);
    procedure actFileDeleteExecute(Sender: TObject);
    procedure actFileCloseExecute(Sender: TObject);
    procedure lvProfileInformationCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvAggregateListColumnClick(Sender: TObject; Column: TListColumn);
    procedure tvProfileTreeClick(Sender: TObject);
    procedure tvProfileTreeKeyPress(Sender: TObject; var Key: Char);
    procedure actHelpCheckForUpdatesExecute(Sender: TObject);
  private
    { Private declarations }
    FProfileFile : TStringList;
    FFileName : String;
    FFileDate : TDateTime;
    FRootKey: String;
    FParams: TStringList;
    FProgress : TfrmProgress;
    FAggregateList : TAggregateList;
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure OpenFile(strFileName : String);
    Procedure DeleteProfile;
    Procedure PopulateTreeView;
    Procedure PopulateListView;
    Procedure PopulateAggregateList;
    Procedure ExceptionProc(strExceptionMsg : String);
    procedure OutputListFields(iBaseTickTime : Int64; iLine : Integer;
      boolOutputToListView : Boolean);
  public
    { Public declarations }
  end;

var
  (** A delphi defined global variable for the auto form creation process. **)
  frmMainForm: TfrmMainForm;

implementation

Uses
  DGHLibrary, About, IniFiles, checkforupdates;

ResourceString
  (** A resource string for prompting that a file has not been found. **)
  strFileNotFoundMsg = 'The profile file "%s" was not found.';
  (** A resource string for letting the user know that they need to select a
      tree node. **)
  strSelectProfileNode = 'You need to select an item in the profile tree vie' +
  'w.';
  (** A resource string for to let the user know that the file has changed. **)
  strFileHasChanged = 'The file has changed since loading and needs to be re' +
  'loaded before a branch can be deleted.';
  (** This is the software ID for checking updates on the internet. **)
  strSoftwareID = 'ProfileViewer';
  (** A format message for building item x **)
  strBuildingItem = 'Building item %d...';
  (** A format message for building item x but too many stack items to view **)
  strBuildingItemTooMany = 'Building item %d... WARNING Toom many stack item' +
  's to view in list!';
  (** A message for loading a profile **)
  strLoadingProfile = 'Loading Profile';
  (** A message for building the listview **)
  strBuildingListview = 'Building Listview...';
  (** A message for no file open. **)
  strNoFile = ' - (no file)';
  (** A message for deleting a profile. **)
  strDeletingProfile = 'Deleting Profile';
  (** A message for selecting the selected profile. **)
  strDeleteTheSelectedProfile = 'Delete the selected profile';
  (** A message for processing the line x **)
  strProcessingLine = 'Processing line %d...';
  (** A message for the tree profile header **)
  strProfileRecords = '%s, TT: %1.0n (%d Records)';
  (** A message for building the tree view **)
  strBuildingTreeview = 'Building Treeview...';
  (** A message for processing the tree item. **)
  strProcessingTreeItem = 'Processing tree item %d...';

{$R *.dfm}

(**

  This method is an on execute event handler for the File Close action.

  @precon  None.
  @postcon Closes the current file.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actFileCloseExecute(Sender: TObject);
begin
  FProfileFile.Clear;
  Caption := Application.Title + strNoFile;
  PopulateTreeView;
end;

(**

  This is an on execute event handler for the File Delete action.

  @precon  None.
  @postcon Deletes the current root profile from the file.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actFileDeleteExecute(Sender: TObject);

begin
  DeleteProfile;
end;

(**

  This method is an on execute event handler for the File Exit action.

  @precon  None.
  @postcon Closes the application.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

(**

  This method is an on execute event handler for the File Open action.

  @precon  None.
  @postcon Displays a dialogue to open a file and if confirmed opens the file.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actFileOpenExecute(Sender: TObject);

begin
  If dlgOpen.Execute Then
    OpenFile(dlgOpen.FileName);
end;

(**

  This is an on execute event handler for the File Refresh action.

  @precon  None.
  @postcon Refreshes the current profile.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actFileRefreshExecute(Sender: TObject);
begin
  OpenFile(FFileName);
end;

(**

  This is an on execute event handler for the Help About action.

  @precon  None.
  @postcon Displays the About dialogue.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actHelpAboutExecute(Sender: TObject);
begin
  TfrmAbout.ShowAbout(FRootKey);
end;

(**

  This is an on execute event handler for the Help Check for Updates action.

  @precon  None.
  @postcon Checks the internet for updates.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actHelpCheckForUpdatesExecute(Sender: TObject);
begin
  TCheckForUpdates.Execute(strSoftwareID, FRootKey, Sender = actHelpCheckForUpdates);
end;

(**

  This method deletes the selected profile from the file.

  @precon  None.
  @postcon Deletes the selected profile from the file.

**)
Procedure TfrmMainForm.DeleteProfile;

Var
  firstRoot : TTreeNode;
  iFirstLine : Integer;
  iLine : Integer;
  dtDate: TDateTime;
  nextRoot: TTreeNode;
  iNextLine: Integer;
  slNewFile : TStringList;

Begin
  firstRoot := tvProfileTree.Selected;
  If firstRoot = Nil Then
    Begin
      MessageDlg(strSelectProfileNode, mtWarning, [mbOK], 0);
      Exit;
    End;
  // Find root node
  While firstRoot.Parent <> Nil Do
    firstRoot := firstRoot.Parent;
  iFirstLine := Integer(firstRoot.Data);
  nextRoot := firstRoot.GetNextSibling;
  If nextRoot <> Nil Then
    iNextLine := Integer(NextRoot.Data) - 1
  Else
    iNextLine := FProfileFile.Count - 1;
  FProgress.Init(iNextLine - iFirstLine, strDeletingProfile,
    strDeleteTheSelectedProfile);
  Try
    FileAge(FFilename, dtDate);
    If (dtDate <> FFileDate) And (FFileDate > 0) Then
      Begin
        MessageDlg(strFileHasChanged, mtWarning, [mbOK], 0);
        Exit;
      End;
      slNewFile := TStringList.Create;
      Try
        FProfileFile.BeginUpdate;
        Try
          For iLine := 0 To FProfileFile.Count - 1 Do
            Begin
              If iLine Mod 1000 = 0 Then
                FProgress.UpdateProgress(iLine, Format(strProcessingLine,
                  [iLine]));
              If (iLine < iFirstLine) Or (iLine > iNextLine) Then
                slNewFile.Add(FProfileFile[iLine]);
            End;
        Finally
          FProfileFile.EndUpdate;
        End;
        slNewFile.SaveToFile(FFileName);
      Finally
        slNewFile.Free;
      End;
    OpenFile(FFileName);
  Finally
    FProgress.Hide;
  End;
End;

(**

  This is an on exception message handler for the BuildRootKey method.

  @precon  None.
  @postcon Displays the exception message in a dialogue.

  @param   strExceptionMsg as a String

**)
procedure TfrmMainForm.ExceptionProc(strExceptionMsg: String);
begin
  MessageDlg(strExceptionMsg, mtError, [mbOK], 0);
end;

(**

  This method outputs the individual fields of the list view associated with 
  the selected tree item. 

  @precon  None. 
  @postcon Outputs the individual fields of the list view associated with the 
           selected tree item. 

  @param   iBaseTickTime        as an Int64
  @param   iLine                as an Integer
  @param   boolOutputToListView as a Boolean

**)
procedure TfrmMainForm.OutputListFields(iBaseTickTime : Int64; iLine : Integer;
  boolOutputToListView : Boolean);

Type
  TProfileInfo = Record
    FClassName  : String;
    FMethodName : String;
    FTTT        : Extended;
    FIPTT       : Extended;
    FCC         : Extended;
  End;

Const
  iStackDepth               = 1;
  iClassName                = 2;
  iMethodName               = 3;
  iTotalTickTime            = 4;
  iInProcessTickTime        = 5;
  iCallCount                = 6;

var
  recInfo : TProfileInfo;
  liProfile: TListItem;
  iErrorCode: Integer;
  dblValue: Extended;

Begin
  recInfo.FClassName := GetField(FProfileFile[iLine], ',', iClassName);
  recInfo.FMethodName := GetField(FProfileFile[iLine], ',', iMethodName);
  Val(GetField(FProfileFile[iLine], ',', iTotalTickTime), dblValue, iErrorCode);
  recInfo.FTTT := Trunc(dblValue);
  Val(GetField(FProfileFile[iLine], ',', iInProcessTickTime), dblValue, iErrorCode);
  recInfo.FIPTT := Trunc(dblValue);
  Val(GetField(FProfileFile[iLine], ',', iCallCount), dblValue, iErrorCode);
  recInfo.FCC := Trunc(dblValue);
  If boolOutputToListView Then
    Begin
      liProfile := lvProfileInformation.Items.Add;
      liProfile.Caption := GetField(FProfileFile[iLine], ',', iStackDepth);
      liProfile.SubItems.Add(recInfo.FClassName);
      liProfile.SubItems.Add(recInfo.FMethodName);
      If iBaseTickTime > 0 Then
        liProfile.SubItems.Add(Format('%1.0n (%1.0f%%)', [recInfo.FTTT,
          100 * recInfo.FTTT / Int(iBaseTickTime)]))
      Else
        liProfile.SubItems.Add(Format('%1.0n (100%%)', [recInfo.FTTT]));
      If iBaseTickTime > 0 Then
        liProfile.SubItems.Add(Format('%1.0n (%1.0f%%)', [recInfo.FIPTT,
          100 * recInfo.FIPTT / Int(iBaseTickTime)]))
      Else
        liProfile.SubItems.Add(Format('%1.0n (100%%)', [recInfo.FIPTT]));
      liProfile.SubItems.Add(Format('%1.0n', [recInfo.FCC]));
      dblValue := Int(recInfo.FTTT) / Int(recInfo.FCC);
      If iBaseTickTime > 0 Then
        liProfile.SubItems.Add(Format('%1.1n (%1.0f%%)', [dblValue,
          100 * dblValue / Int(iBaseTickTime)]))
      Else
        liProfile.SubItems.Add(Format('%1.1n (100%%)', [dblValue]));
      dblValue := Int(recInfo.FIPTT) / Int(recInfo.FCC);
      If iBaseTickTime > 0 Then
        liProfile.SubItems.Add(Format('%1.1n (%1.0f%%)', [dblValue,
          100 * dblValue / Int(iBaseTickTime)]))
      Else
        liProfile.SubItems.Add(Format('%1.1n (100%%)', [dblValue]));
      liProfile.SubItems.Add(IntToStr(iLine));
    End;
  With recInfo Do
    FAggregateList.Add(FClassName + '.' + FMethodName, FTTT, FIPTT, FCC);
End;

(**

  This is the forms on create event handler.

  @precon  None.
  @postcon Loads the applications settings from the registry.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.FormCreate(Sender: TObject);
begin
  FParams := TStringList.Create;
  FRootKey := BuildRootKey(FParams, ExceptionProc);
  TfrmAbout.ShowAbout(FRootKey);
  actHelpCheckForUpdatesExecute(Self);
  FProfileFile := TStringList.Create;
  FProgress := TfrmProgress.Create(Nil);
  FAggregateList := TAggregateList.Create;
  Caption := Application.Title + strNofile;
  LoadSettings;
  If ParamStr(1) <> '' Then
    OpenFile(ParamStr(1))
  Else
    If FileExists(FFileName) Then
      OpenFile(FFileName)
end;

(**

  This is the forms on destroy event handler

  @precon  None.
  @postcon Saves the applications settings to the registry.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.FormDestroy(Sender: TObject);
begin
  FAggregateList.Free;
  FProgress.Free;
  FParams.Free;
  SaveSettings;
  FProfileFile.Free;
end;

(**

  This method loads the applications settings from the registry to restore the
  state of the application to the same state as when it was last closed.

  @precon  None.
  @postcon Loads the applications settings from the registry.

**)
procedure TfrmMainForm.LoadSettings;

begin
  With TIniFile.Create(FRootKey) Do
    Try
      Top := ReadInteger('Setup', 'Top', 100);
      Left := ReadInteger('Setup', 'Left', 100);
      Height := ReadInteger('Setup', 'Height', 300);
      Width := ReadInteger('Setup', 'Width', 400);
      tvProfileTree.Width :=  ReadInteger('Setup', 'TreeWidth', 100);
      lvProfileInformation.Column[0].Width := ReadInteger('ColumnWidths', 'StackDepth', 50);
      lvProfileInformation.Column[1].Width := ReadInteger('ColumnWidths', 'Class', 50);
      lvProfileInformation.Column[2].Width := ReadInteger('ColumnWidths', 'Method', 50);
      lvProfileInformation.Column[3].Width := ReadInteger('ColumnWidths', 'TotalTickCount', 50);
      lvProfileInformation.Column[4].Width := ReadInteger('ColumnWidths', 'InProcessTickCount', 50);
      lvProfileInformation.Column[5].Width := ReadInteger('ColumnWidths', 'CallCount', 50);
      lvProfileInformation.Column[6].Width := ReadInteger('ColumnWidths', 'AverageTotalTickCount', 50);
      lvProfileInformation.Column[7].Width := ReadInteger('ColumnWidths', 'AverageInProcessTickCount', 50);
      lvProfileInformation.Column[8].Width := ReadInteger('ColumnWidths', 'Line', 50);
      FFileName := ReadString('Setup', 'FileName', '');
      lvAggregateList.Height :=  ReadInteger('Setup', 'AggregateHeight', 100);
      lvAggregateList.Column[0].Width := ReadInteger('AggregateColumnWidths', 'Class.Method', 50);
      lvAggregateList.Column[1].Width := ReadInteger('AggregateColumnWidths', 'TotalTickCount', 50);
      lvAggregateList.Column[2].Width := ReadInteger('AggregateColumnWidths', 'InProcessTickCount', 50);
      lvAggregateList.Column[3].Width := ReadInteger('AggregateColumnWidths', 'CallCount', 50);
      lvAggregateList.Column[4].Width := ReadInteger('AggregateColumnWidths', 'AverageTotalTickCount', 50);
      lvAggregateList.Column[5].Width := ReadInteger('AggregateColumnWidths', 'AverageInProcessTickCount', 50);
    Finally;
      Free;
    End;
end;

(**

  This method sorts the aggregate list when the lists columns are clicked.

  @precon  None.
  @postcon Sorts the aggregate list when the lists columns are clicked.

  @param   Sender as a TObject
  @param   Column as a TListColumn

**)
procedure TfrmMainForm.lvAggregateListColumnClick(Sender: TObject;
  Column: TListColumn);

var
  i: Integer;

begin
  Case Column.Index Of
    0 : FAggregateList.Sort(asMethod);
    1 : FAggregateList.Sort(asTTT);
    2 : FAggregateList.Sort(asIPTT);
    3 : FAggregateList.Sort(asCC);
    4 : FAggregateList.Sort(asATTT);
    5 : FAggregateList.Sort(asAIPTT);
  End;
  For i := 0 To lvAggregateList.Columns.Count - 1 Do
    lvAggregateList.Columns[i].ImageIndex := -1;
  If Not FAggregateList.Backward Then
    Column.ImageIndex := 0
  Else
    Column.ImageIndex := 1;
  PopulateAggregateList;
end;

(**

  This is an on Custom Draw Item event handler for the list view.

  @precon  None.
  @postcon This highlights each of the profile lines with a different colour
           based on its stack depth.

  @param   Sender      as a TCustomListView
  @param   Item        as a TListItem
  @param   State       as a TCustomDrawState
  @param   DefaultDraw as a Boolean as a reference

**)
procedure TfrmMainForm.lvProfileInformationCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
Var
  iStackDepth : Integer;

begin
  iStackDepth := StrToInt(Item.Caption);
  Case iStackDepth Mod 12 Of
    0: lvProfileInformation.Canvas.Brush.Color := $FFFFDD;
    1: lvProfileInformation.Canvas.Brush.Color := $FFDDFF;
    2: lvProfileInformation.Canvas.Brush.Color := $DDFFFF;
    3: lvProfileInformation.Canvas.Brush.Color := $FFDDD;
    4: lvProfileInformation.Canvas.Brush.Color := $DDDDFF;
    5: lvProfileInformation.Canvas.Brush.Color := $DDDDDD;
    6: lvProfileInformation.Canvas.Brush.Color := $FFFFBB;
    7: lvProfileInformation.Canvas.Brush.Color := $FFBBFF;
    8: lvProfileInformation.Canvas.Brush.Color := $BBFFFF;
    9: lvProfileInformation.Canvas.Brush.Color := $FFBBD;
    10: lvProfileInformation.Canvas.Brush.Color := $BBBBFF;
    11: lvProfileInformation.Canvas.Brush.Color := $BBBBBB;
  End;
  lvProfileInformation.Canvas.FillRect(Item.DisplayRect(drBounds));
  If Item.Index = 0 Then
    lvProfileInformation.Canvas.Font.Style := [fsBold]
  Else
    lvProfileInformation.Canvas.Font.Style := [];
end;

(**

  This method opens the profile file passed and populates the tree view with
  the information.

  @precon  None.
  @postcon Opens the profile file passed and populates the tree view with
           the information.

  @param   strFileName as a String

**)
procedure TfrmMainForm.OpenFile(strFileName: String);
begin
  If FileExists(strFileName) Then
    Begin
      FProfileFile.LoadFromFile(strFileName);
      FFileName := strFileName;
      FileAge(strFileName, FFileDate);
      Caption := Application.Title + ' - ' + strFileName;
      PopulateTreeView;
    End Else
      MessageDlg(Format(strFileNotFoundMsg, [strFileName]), mtWarning, [mbOK], 0);
end;

(**

  This method outputs the aggregate list to a list view control.

  @precon  None.
  @postcon Outputs the aggregate list to a list view control.

**)
Procedure TfrmMainForm.PopulateAggregateList;

Var
  i : Integer;
  Item : TListItem;
  dblPercentage : Double;

begin
  lvAggregateList.Items.BeginUpdate;
  Try
    lvAggregateList.Clear;
    For i := 1 To FAggregateList.Count Do
      Begin
        Item := lvAggregateList.Items.Add;
        Item.ImageIndex := -1;
        Item.Caption := FAggregateList[i].Method;
        If FAggregateList.TotalTime > 0 Then
          dblPercentage := Int(FAggregateList[i].TotalTime) /
            FAggregateList.TotalTime * 100.0
        Else
          dblPercentage := 100;
        Item.SubItems.Add(Format('%1.0n (%1.0f%%)', [
          FAggregateList[i].TotalTime, dblPercentage
        ]));
        If FAggregateList.TotalTime > 0 Then
          dblPercentage := Int(FAggregateList[i].InProcessTime) /
            FAggregateList.TotalTime * 100.0
        Else
          dblPercentage := 100;
        Item.SubItems.Add(Format('%1.0n (%1.0f%%)', [
          FAggregateList[i].InProcessTime, dblPercentage
        ]));
        Item.SubItems.Add(Format('%1.0n', [FAggregateList[i].CallCount]));
        If FAggregateList.TotalTime > 0 Then
          dblPercentage := Int(FAggregateList[i].AverageTotalTime) /
            FAggregateList.TotalTime * 100.0
        Else
          dblPercentage := 100;
        Item.SubItems.Add(Format('%1.1n (%1.0f%%)', [
          FAggregateList[i].AverageTotalTime, dblPercentage
        ]));
        If FAggregateList.TotalTime > 0 Then
          dblPercentage := Int(FAggregateList[i].AverageInProcessTime) /
            FAggregateList.TotalTime * 100.0
        Else
          dblPercentage := 100;
        Item.SubItems.Add(Format('%1.1n (%1.0f%%)', [
          FAggregateList[i].AverageInProcessTime, dblPercentage
        ]));
      End;
    lvAggregateList.Columns[0].ImageIndex := 0;
  Finally
    lvAggregateList.Items.EndUpdate;
  End;
end;

(**

  This method populates the list view with information the lies on or below the
  selected item in the tree view.

  @precon  None.
  @postcon Populates the list view with information the lies on or below the
           selected item in the tree view.

**)
procedure TfrmMainForm.PopulateListView;

Const
  iMaxLinesToView = 4096; // Limit the list as it takes too long to populate.

Var
  iStartLine : Integer;
  iEndLine: Integer;
  iLine : Integer;
  strField: String;
  iStackDepth : Integer;
  iErrorCode : Integer;
  iStartStackDepth : Integer;
  iBaseTickTime : Integer;
  TN : TTreeNode;

begin
  FProgress.Init(1, strLoadingProfile, strBuildingListview);
  Try
    FAggregateList.Clear;
    iStartStackDepth := 0;
    iBaseTickTime := 0;
    lvProfileInformation.Items.BeginUpdate;
    Try
      lvProfileInformation.Items.Clear;
      If tvProfileTree.Selected <> Nil Then
        Begin
          iStartLine := Integer(tvProfileTree.Selected.Data);
          TN := tvProfileTree.Selected.getNextSibling;
          If TN <> Nil Then
            iEndLine := Integer(TN.Data) - 1
          Else
            iEndLine := FProfileFile.Count - 1;
          FProgress.Init(iEndLine - iStartLine, strLoadingProfile,
            strBuildingListview);
          For iLine := iStartLine To iEndLine Do
            Begin
              If iLine Mod 1000 = 0 Then
                Begin
                  If iLine <= iStartLine + iMaxLinesToView Then
                    FProgress.UpdateProgress(iLine - iStartLine,
                      Format(strBuildingItem, [iLine]))
                  Else
                    FProgress.UpdateProgress(iLine - iStartLine,
                      Format(strBuildingItemTooMany, [iLine]));
                End;
              strField := GetField(FProfileFile[iLine], ',', 1);
              Val(strField, iStackDepth, iErrorCode);
              If iErrorCode =  0 Then
                Begin
                  If iBaseTickTime = 0 Then
                    Begin
                      strField := GetField(FProfileFile[iLine], ',', 4);
                      Val(strField, iBaseTickTime, iErrorCode);
                    End;
                  If (iStackDepth > 0) And (iStartStackDepth = 0) Then
                    iStartStackDepth := iStackDepth;
                  OutputListFields(iBaseTickTime, iLine,
                    iLine <= iStartLine + iMaxLinesToView);
                End;
            End;
        End
    Finally
      lvProfileInformation.Items.EndUpdate;
    End;
  Finally
    FProgress.Hide;
  End;
  PopulateAggregateList;
end;

(**

  This method populates the tree view with information loaded in the
  FProfileFile string list.

  @precon  None.
  @postcon Populates the tree view with information loaded in the
           FProfileFile string list.

**)
procedure TfrmMainForm.PopulateTreeView;

  (**

    This function extracts the Application name and the date on which the set of
    profiles was recorded.

    @precon  None.
    @postcon Extracts the Application name and the date on which the set of
             profiles was recorded.

    @param   strText as a String
    @return  a String

  **)
  Function GetAppAndDate(strText : String) : String;

  Var
    iCharCount : Integer;
    iPos : Integer;

  Begin
    strText := strText;
    iCharCount := CharCount('\', strText);
    iPos := PosOfNthChar(strText, '\', iCharCount);
    Result := Copy(strText, iPos + 1, Length(strText));
  end;

  (**

    This procedure updates the root nodes of the tree view with the number of
    profiles underneath the root.

    @precon  None.
    @postcon Updates the root nodes of the tree view with the number of
             profiles underneath the root.

    @param   tnRoot       as a TTreeNode
    @param   iLastLine    as an Integer
    @param   iRootLine    as an Integer
    @param   dblTotalTime as an Extended

  **)
  Procedure UpdateRootWithCount(tnRoot : TTreeNode; iLastLine, iRootLine : Integer;
    dblTotalTime : Extended);
  Begin
    If tnRoot <> Nil Then
      tnRoot.Text := Format(strProfileRecords, [tnRoot.Text,
        dblTotalTime, iLastLine - iRootLine - 2]);
  End;

Var
  tnProfileNode : TTreeNode;
  iLine : Integer;
  iFields : Integer;
  strFirstField: String;
  iErrorCode : Integer;
  iStackDepth: Integer;
  iLastStackDepth : Integer;
  i: Integer;
  tnParent : TTreeNode;
  iStartLine: Integer;
  tnProfileRoot: TTreeNode;
  dblTT : Extended;
  dbl: Extended;
  dblNTT: Extended;
  dblNIP: Extended;
  dblNCC: Extended;

begin
  FProgress.Init(FProfileFile.Count, strLoadingProfile, strBuildingTreeview);
  Try
    tvProfileTree.Items.BeginUpdate;
    Try
      tvProfileTree.Items.Clear;
      If FProfileFile.Count = 0 Then
        Exit;
      tnProfileNode := Nil;
      tnParent := Nil;
      iLastStackDepth := 0;
      iStartLine := 0;
      tnProfileRoot := Nil;
      dblTT := 0;
      For iLine := 0 To FProfileFile.Count - 1 Do
        Begin
          If iline Mod 1000 =  0 Then
            FProgress.UpdateProgress(iLine, Format(strProcessingTreeItem,
              [iLine]));
          iFields := CharCount(',', FProfileFile[iLine]) + 1;
          If iFields = 1 Then
            Begin
              If FProfileFile[iLine] <> '' Then
                Begin
                  UpdateRootWithCount(tnProfileRoot, iLine, iStartLine, dblTT);
                  tnProfileNode := tvProfileTree.Items.AddObject(Nil,
                    GetAppAndDate(FProfileFile[iLine]), TObject(iLine));
                  tnProfileRoot := tnProfileNode;
                  dblTT := 0;
                  iStartLine := iLine;
                End;
              iLastStackDepth := 0;
            End Else
            Begin
              strFirstField := GetField(FProfileFile[iLine], ',', 1);
              Val(strFirstField, iStackDepth, iErrorCode);
              If iErrorCode = 0 Then
                Begin
                  If iStackDepth = 1 Then
                    Begin
                      Val(GetField(FProfileFile[iLine], ',', 4), dbl, iErrorCode);
                      dblTT := dblTT + dbl;
                    End;
                  If iStackDepth > iLastStackDepth Then
                    tnParent := tnProfileNode;
                  For i := iStackDepth To iLastStackDepth - 1 Do
                    tnProfileNode := tnProfileNode.Parent;
                  If iStackDepth <= iLastStackDepth Then
                    tnParent := tnProfileNode.Parent;
                  Val(GetField(FProfileFile[iLine], ',', 4), dblNTT, iErrorCode);
                  Val(GetField(FProfileFile[iLine], ',', 5), dblNIP, iErrorCode);
                  Val(GetField(FProfileFile[iLine], ',', 6), dblNCC, iErrorCode);
                  tnProfileNode := tvProfileTree.Items.AddChildObject(
                    tnParent, Format('%s.%s (TT: %1.0n, IPT: %1.0n, CC: %1.0n)', [
                      GetField(FProfileFile[iLine], ',', 2),
                      GetField(FProfileFile[iLine], ',', 3),
                      dblNTT, dblNIP, dblNCC]),
                    TObject(iLine));
                  iLastStackDepth := iStackDepth;
                End;
            End;
        End;
      UpdateRootWithCount(tnProfileRoot, FProfileFile.Count - 1, iStartLine, dblTT);
      tvProfileTree.Selected := Nil;
    Finally
      tvProfileTree.Items.EndUpdate;
    End;
  Finally
    FProgress.Hide;
  End;
end;

(**

  This method saves the applications settings to the registry so that the
  state of the application can be restored when next opened.

  @precon  None.
  @postcon Saves the applications settings to the registry.

**)
procedure TfrmMainForm.SaveSettings;
begin
  With TIniFile.Create(FRootKey) Do
    Try
      WriteInteger('Setup', 'Top', Top);
      WriteInteger('Setup', 'Left', Left);
      WriteInteger('Setup', 'Height', Height);
      WriteInteger('Setup', 'Width', Width);
      WriteInteger('Setup', 'TreeWidth', tvProfileTree.Width);
      WriteInteger('ColumnWidths', 'StackDepth', lvProfileInformation.Column[0].Width);
      WriteInteger('ColumnWidths', 'Class', lvProfileInformation.Column[1].Width);
      WriteInteger('ColumnWidths', 'Method', lvProfileInformation.Column[2].Width);
      WriteInteger('ColumnWidths', 'TotalTickCount', lvProfileInformation.Column[3].Width);
      WriteInteger('ColumnWidths', 'InProcessTickCount', lvProfileInformation.Column[4].Width);
      WriteInteger('ColumnWidths', 'CallCount', lvProfileInformation.Column[5].Width);
      WriteInteger('ColumnWidths', 'AverageTotalTickCount', lvProfileInformation.Column[6].Width);
      WriteInteger('ColumnWidths', 'AverageInProcessTickCount', lvProfileInformation.Column[7].Width);
      WriteInteger('ColumnWidths', 'Line', lvProfileInformation.Column[8].Width);
      WriteString('Setup', 'FileName', FFileName);
      WriteInteger('Setup', 'AggregateHeight', lvAggregateList.Height);
      WriteInteger('AggregateColumnWidths', 'Class.Method', lvAggregateList.Column[0].Width);
      WriteInteger('AggregateColumnWidths', 'TotalTickCount', lvAggregateList.Column[1].Width);
      WriteInteger('AggregateColumnWidths', 'InProcessTickCount', lvAggregateList.Column[2].Width);
      WriteInteger('AggregateColumnWidths', 'CallCount', lvAggregateList.Column[3].Width);
      WriteInteger('AggregateColumnWidths', 'AverageTotalTickCount', lvAggregateList.Column[4].Width);
      WriteInteger('AggregateColumnWidths', 'AverageInProcessTickCount', lvAggregateList.Column[5].Width);
    Finally
      Free;
    End;
end;

(**

  This is an on click event handler for the Tree View.

  @precon  None.
  @postcon Updates the list view when the item in the tree view is changed.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.tvProfileTreeClick(Sender: TObject);
begin
  PopulateListView;
end;

(**

  This is an on key press event handler for the tree view.

  @precon  None.
  @postcon Invokes the ProfileTreeClick event IF the enter key is pressed.

  @param   Sender as a TObject
  @param   Key    as a Char as a reference

**)
procedure TfrmMainForm.tvProfileTreeKeyPress(Sender: TObject; var Key: Char);
begin
  If Key = #13 Then
    Begin
      tvProfileTreeClick(Sender);
      Key := #0;
    End;
end;

end.
