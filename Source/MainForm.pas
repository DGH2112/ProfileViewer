(**

  This module contains a class which represents the application interface
  using a tree view for the profile outline and a list view to display the
  highlighted sections of the profiles information in a list report.

  @Author  David Hoyle
  @Date    02 May 2009
  @Version 1.0

**)
unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, ComCtrls, ExtCtrls, Menus, ImgList, ToolWin, ProgressForm,
  AggregateList, StdCtrls, Contnrs, VirtualTrees;

type
  (** This is a base class for the profile record and header. **)
  TProfileBase = Class
  Strict Private
    FTotalTime : Double;
    FCallCount : Double;
    FLine      : Integer;
    FIndex     : Integer;
  Strict Protected
    Function GetName : String; Virtual;
  Public
    Constructor Create(dblTotalTime, dblCallCount : Double; iIndex, iLine : Integer);
    (**
      This property returns the Total Time in micro seconds of the profile
      record.
      @precon  None.
      @postcon Returns the Total Time in micro seconds of the profile record.
      @return  an Double
    **)
    Property TotalTime : Double Read FTotalTime Write FTotalTime;
    (**
      This property return the call count of the profile record.
      @precon  None.
      @postcon Return the call count of the profile record.
      @return  an Double
    **)
    Property CallCount : Double Read FCallCount Write FCallCount;
    (**
      This property returns the line numbner of the profile record in the
      Source file.
      @precon  None.
      @postcon Returns the line numbner of the profile record in the
               Source file.
      @return  an Integer
    **)
    Property Line : Integer Read FLine;
    (**
      This property returns the index of the item in the collection.
      @precon  None.
      @postcon Returns the index of the item in the collection.
      @return  an Integer
    **)
    Property Index : Integer Read FIndex;
    (**
      This property returns a string representation of the class.
      @precon  None.
      @postcon Returns a string representation of the class.
      @return  a String
    **)
    Property Name : String Read GetName;
  End;

  (** A class to hold the profile header. **)
  TProfileHeader = Class(TProfileBase)
  Strict Private
    FHeader : String;
  Strict Protected
    Function GetName : String; Override;
  Public
    Constructor Create(strHeader : String; iIndex, iLine : Integer);
    (**
      This property returns the file, date and time of the profile.
      @precon  None.
      @postcon Returns the file, date and time of the profile.
      @return  a String
    **)
    Property Header : String Read FHeader Write FHeader;
  End;

  (** A class to hold a single piece of profile information. **)
  TProfileRecord = Class(TProfileBase)
  Strict Private
    FStackDepth           : Integer;
    FClsName              : String;
    FMthdName             : String;
    FInProcessTime        : Double;
    FAverageTotalTime     : Double;
    FAverageInProcessTime : Double;
  Strict Protected
    Function GetName : String; Override;
  Public
    Constructor Create(iStackDepth : Integer; strClassName, strMethodName : String;
      dblTotalTime, dblInProcessTime, dblCallCount : Double; iIndex, iLine : Integer);
    (**
      This property returns the stack depth of the profile record.
      @precon  None.
      @postcon Returns the stack depth of the profile record.
      @return  an Integer
    **)
    Property StackDepth           : Integer  Read FStackDepth;
    (**
      This property returns the class name of the profile record.
      @precon  None.
      @postcon Returns the class name of the profile record.
      @return  a String
    **)
    Property ClsName              : String   Read FClsName;
    (**
      This method returns the method name of the profile record.
      @precon  None.
      @postcon Returns the method name of the profile record.
      @return  a String
    **)
    Property MthdName             : String   Read FMthdName;
    (**
      This property returns the In Process Time in micro seconds of the profile
      record.
      @precon  None.
      @postcon Returns the In Process Time in micro seconds of the profile
               record.
      @return  an Double
    **)
    Property InProcessTime        : Double Read FInProcessTime;
    (**
      This property returns the Average Total Time in micro seconds of the
      profile record.
      @precon  None.
      @postcon Returns the Average Total Time in micro seconds of the profile
               record.
      @return  an Double
    **)
    Property AverageTotalTime     : Double Read FAverageTotalTime;
    (**
      This property returns the Average In Process Time in micro seconds of the
      profile record.
      @precon  None.
      @postcon Returns the Average In Process Time in micro seconds of the
               profile record.
      @return  an Double
    **)
    Property AverageInProcessTime : Double Read FAverageInProcessTime;
  End;

  (** A class to represent the main application form. **)
  TfrmMainForm = class(TForm)
    mmMenu: TMainMenu;
    mmiFile: TMenuItem;
    mmiHelp: TMenuItem;
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
    lvAggregateList: TListView;
    sptSortable: TSplitter;
    actHelpCheckForUpdates: TAction;
    CheckForUpdates1: TMenuItem;
    N1: TMenuItem;
    ilSortImages: TImageList;
    vstProfileRecords: TVirtualStringTree;
    ilTreeIcons: TImageList;
    procedure actFileRefreshExecute(Sender: TObject);
    procedure actFileDeleteExecute(Sender: TObject);
    procedure actFileCloseExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvAggregateListColumnClick(Sender: TObject; Column: TListColumn);
    procedure tvProfileTreeClick(Sender: TObject);
    procedure tvProfileTreeKeyPress(Sender: TObject; var Key: Char);
    procedure actHelpCheckForUpdatesExecute(Sender: TObject);
    procedure vstProfileRecordsGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure vstProfileRecordsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vstProfileRecordsGetHint(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex;
      var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: WideString);
  private
    { Private declarations }
    FFileName : String;
    FFileDate : TDateTime;
    FRootKey: String;
    FParams: TStringList;
    FProgress : TfrmProgress;
    FAggregateList : TAggregateList;
    FProfileInfoList : TObjectList;
    FSortColumn: Integer;
    FSortDirection: Boolean;
    FLastFocusedNode: PVirtualNode;
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure OpenFile(strFileName : String);
    Procedure DeleteProfile;
    Procedure PopulateTreeView;
    Procedure PopulateListView;
    Procedure PopulateAggregateList;
    Procedure ExceptionProc(strExceptionMsg : String);
    Procedure BuildProfileList(strFileName : String);
    Function  NodePath(Node : PVirtualNode) : String;
    procedure FocusNode(strFocusedNode: String);
  public
    { Public declarations }
  end;

  (** A record to describe the structure of the virtual string tree data. **)
  TTreeData = Record
    FProfileRecord : TProfileBase;
  End;

var
  (** A delphi defined global variable for the auto form creation process. **)
  frmMainForm: TfrmMainForm;

implementation

Uses
  DGHLibrary, About, IniFiles, checkforupdates
  {$IFDEF PROFILECODE}, Profiler {$ENDIF};

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
  (** A message for building the tree view **)
  strBuildingTreeview = 'Building Treeview...';
  (** A message for processing the tree item. **)
  strProcessingTreeItem = 'Processing tree item %d...';

{$R *.dfm}

{ TProfileBase }

(**

  This is a constructor for the TProfileBase class.

  @precon  None.
  @postcon Sets line number for the record and header.

  @param   dblTotalTime as a Double
  @param   dblCallCount as a Double
  @param   iIndex       as an Integer
  @param   iLine        as an Integer

**)
constructor TProfileBase.Create(dblTotalTime, dblCallCount : Double; iIndex, iLine: Integer);

begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TProfileBase.Create');
  Try
  {$ENDIF}
  FTotalTime := dblTotalTime;
  FCallCount := dblCallCount;
  FIndex := iIndex;
  FLine := iLine;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This is a getter method for the Name property.

  @precon  None.
  @postcon Return a null string.

  @return  a String

**)
Function TProfileBase.GetName: String;
Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TProfileBase.GetName');
  Try
  {$ENDIF}
  Result := '';
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
End ;

{ TProfileHeader }

(**

  This is a constructor for the TProfileHeader class.

  @precon  None.
  @postcon Initialises the class with information.

  @param   strHeader as a String
  @param   iIndex    as an Integer
  @param   iLine     as an Integer

**)
Constructor TProfileHeader.Create(strHeader : String; iIndex, iLine : Integer);

Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TProfileHeader.Create');
  Try
  {$ENDIF}
  Inherited Create(0.0, 0.0, iIndex, iLine);
  FHeader := strHeader;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
End;

(**

  This is a getter method for the Name property.

  @precon  None.
  @postcon Returns the header.

  @return  a String

**)
Function TProfileHeader.GetName: String;
Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TProfileHeader.GetName');
  Try
  {$ENDIF}
  Result := FHeader;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
End ;

{ TProfileRecord }

(**

  This is a constructor for the TProfileRecord class.

  @precon  None.
  @postcon Initialises the class with information.

  @param   iStackDepth      as an Integer
  @param   strClassName     as a String
  @param   strMethodName    as a String
  @param   dblTotalTime     as a Double
  @param   dblInProcessTime as a Double
  @param   dblCallCount     as a Double
  @param   iIndex           as an Integer
  @param   iLine            as an Integer

**)
Constructor TProfileRecord.Create(iStackDepth : Integer; strClassName,
  strMethodName : String; dblTotalTime, dblInProcessTime,
  dblCallCount : Double; iIndex, iLine : Integer);

Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TProfileRecord.Create');
  Try
  {$ENDIF}
  Inherited Create(dblTotalTime, dblCallCount, iIndex, iLine);
  FStackDepth := iStackDepth;
  FClsName := strClassName;
  FMthdName := strMethodName;
  FInProcessTime := dblInProcessTime;
  FAverageTotalTime := dblTotalTime / CallCount;
  FAverageInProcessTime := dblInProcessTime / CallCount;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
End;

(**

  This is a getter method for the Name property.

  @precon  None.
  @postcon Returns the class and method name.

  @return  a String

**)
Function TProfileRecord.GetName: String;
Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TProfileRecord.GetName');
  Try
  {$ENDIF}
  Result := FClsName + '.' + FMthdName;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
End ;

(**

  This method is an on execute event handler for the File Close action.

  @precon  None.
  @postcon Closes the current file.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actFileCloseExecute(Sender: TObject);
begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.actFileCloseExecute');
  Try
  {$ENDIF}
  Caption := Application.Title + strNoFile;
  PopulateTreeView;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This is an on execute event handler for the File Delete action.

  @precon  None.
  @postcon Deletes the current root profile from the file.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actFileDeleteExecute(Sender: TObject);

begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.actFileDeleteExecute');
  Try
  {$ENDIF}
  DeleteProfile;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This method is an on execute event handler for the File Exit action.

  @precon  None.
  @postcon Closes the application.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actFileExitExecute(Sender: TObject);
begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.actFileExitExecute');
  Try
  {$ENDIF}
  Close;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This method is an on execute event handler for the File Open action.

  @precon  None.
  @postcon Displays a dialogue to open a file and if confirmed opens the file.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actFileOpenExecute(Sender: TObject);

begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.actFileOpenExecute');
  Try
  {$ENDIF}
  If dlgOpen.Execute Then
    OpenFile(dlgOpen.FileName);
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This is an on execute event handler for the File Refresh action.

  @precon  None.
  @postcon Refreshes the current profile.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actFileRefreshExecute(Sender: TObject);
begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.actFileRefreshExecute');
  Try
  {$ENDIF}
  OpenFile(FFileName);
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This is an on execute event handler for the Help About action.

  @precon  None.
  @postcon Displays the About dialogue.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actHelpAboutExecute(Sender: TObject);
begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.actHelpAboutExecute');
  Try
  {$ENDIF}
  TfrmAbout.ShowAbout(FRootKey);
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This is an on execute event handler for the Help Check for Updates action.

  @precon  None.
  @postcon Checks the internet for updates.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actHelpCheckForUpdatesExecute(Sender: TObject);
begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.actHelpCheckForUpdatesExecute');
  Try
  {$ENDIF}
  TCheckForUpdates.Execute(strSoftwareID, FRootKey, Sender = actHelpCheckForUpdates);
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This method deletes the selected profile from the file.

  @precon  None.
  @postcon Deletes the selected profile from the file.

**)
Procedure TfrmMainForm.DeleteProfile;

Var
  firstRoot  : PVirtualNode;
  nextRoot   : PVirtualNode;
  iFirstLine : Integer;
  iNextLine  : Integer;
  dtDate     : TDateTime;
  slNewFile  : TStringList;
  iLine      : Integer;
  slOldFile: TStringList;
  NodeData : ^TTreeData;

Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.DeleteProfile');
  Try
  {$ENDIF}
  firstRoot := vstProfileRecords.FocusedNode;
  If firstRoot = Nil Then
    Begin
      MessageDlg(strSelectProfileNode, mtWarning, [mbOK], 0);
      Exit;
    End;
  // Find root node
  While vstProfileRecords.NodeParent[firstRoot] <> Nil Do
    firstRoot := vstProfileRecords.NodeParent[firstRoot];
  NodeData := vstProfileRecords.GetNodeData(FirstRoot);
  iFirstLine := NodeData.FProfileRecord.Line;
  nextRoot := firstRoot.NextSibling;
  If nextRoot <> Nil Then
    Begin
      NodeData := vstProfileRecords.GetNodeData(nextRoot);
      If NodeData.FProfileRecord <> Nil Then
        iNextLine := NodeData.FProfileRecord.Line - 1
      Else
        iNextLine := (FProfileInfoList[FProfileInfoList.Count - 1] As
          TProfileBase).Line;
    End Else
    Begin
      iNextLine := (FProfileInfoList[FProfileInfoList.Count - 1] As
        TProfileBase).Line;
    End;
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
      slOldFile := TStringList.Create;
      Try
        slOldFile.LoadFromFile(FFileName);
        For iLine := 0 To slOldFile.Count - 1 Do
          Begin
            If iLine Mod 1000 = 0 Then
              FProgress.UpdateProgress(iLine, Format(strProcessingLine, [iLine]));
            If (iLine < iFirstLine) Or (iLine > iNextLine) Then
              slNewFile.Add(slOldFile[iLine]);
          End;
        slNewFile.SaveToFile(FFileName);
      Finally
        slOldFile.Free;
      End;
    Finally
      slNewFile.Free;
    End;
    OpenFile(FFileName);
  Finally
    FProgress.Hide;
  End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
End;

(**

  This is an on exception message handler for the BuildRootKey method.

  @precon  None.
  @postcon Displays the exception message in a dialogue.

  @param   strExceptionMsg as a String

**)
procedure TfrmMainForm.ExceptionProc(strExceptionMsg: String);
begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.ExceptionProc');
  Try
  {$ENDIF}
  MessageDlg(strExceptionMsg, mtError, [mbOK], 0);
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This is the forms on create event handler.

  @precon  None.
  @postcon Loads the applications settings from the registry.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.FormCreate(Sender: TObject);
begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.FormCreate');
  Try
  {$ENDIF}
  FLastFocusedNode := Nil;
  vstProfileRecords.NodeDataSize := SizeOf(TTreeData);
  FParams := TStringList.Create;
  FRootKey := BuildRootKey(FParams, ExceptionProc);
  If DebugHook = 0 Then TfrmAbout.ShowAbout(FRootKey);
  actHelpCheckForUpdatesExecute(Self);
  FProgress := TfrmProgress.Create(Nil);
  FAggregateList := TAggregateList.Create;
  FProfileInfoList := TObjectList.Create(True);
  Caption := Application.Title + strNofile;
  LoadSettings;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This is the forms on destroy event handler

  @precon  None.
  @postcon Saves the applications settings to the registry.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.FormDestroy(Sender: TObject);
begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.FormDestroy');
  Try
  {$ENDIF}
  SaveSettings;
  FProfileInfoList.Free;
  FAggregateList.Free;
  FProgress.Free;
  FParams.Free;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This method loads the applications settings from the registry to restore the
  state of the application to the same state as when it was last closed.

  @precon  None.
  @postcon Loads the applications settings from the registry.

**)
procedure TfrmMainForm.LoadSettings;

begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.LoadSettings');
  Try
  {$ENDIF}
  With TIniFile.Create(FRootKey) Do
    Try
      Top := ReadInteger('Setup', 'Top', 100);
      Left := ReadInteger('Setup', 'Left', 100);
      Height := ReadInteger('Setup', 'Height', 300);
      Width := ReadInteger('Setup', 'Width', 400);
      WindowState := TWindowState(ReadInteger('Setup', 'WindowState', Byte(wsNormal)));
      FFileName := ReadString('Setup', 'FileName', '');
      lvAggregateList.Height :=  ReadInteger('Setup', 'AggregateHeight', 100);
      lvAggregateList.Column[0].Width := ReadInteger('AggregateColumnWidths', 'Class.Method', 50);
      lvAggregateList.Column[1].Width := ReadInteger('AggregateColumnWidths', 'TotalTickCount', 50);
      lvAggregateList.Column[2].Width := ReadInteger('AggregateColumnWidths', 'InProcessTickCount', 50);
      lvAggregateList.Column[3].Width := ReadInteger('AggregateColumnWidths', 'CallCount', 50);
      lvAggregateList.Column[4].Width := ReadInteger('AggregateColumnWidths', 'AverageTotalTickCount', 50);
      lvAggregateList.Column[5].Width := ReadInteger('AggregateColumnWidths', 'AverageInProcessTickCount', 50);
      vstProfileRecords.Header.Columns[0].Width := ReadInteger('Columns', 'Class.Method', 200);
      vstProfileRecords.Header.Columns[1].Width := ReadInteger('Columns', 'Total Time', 50);
      vstProfileRecords.Header.Columns[2].Width := ReadInteger('Columns', 'In Process', 50);
      vstProfileRecords.Header.Columns[3].Width := ReadInteger('Columns', 'Call Count', 50);
      vstProfileRecords.Header.Columns[4].Width := ReadInteger('Columns', 'Average Total Time', 50);
      vstProfileRecords.Header.Columns[5].Width := ReadInteger('Columns', 'Average In Process Time', 50);
      FSortColumn := ReadInteger('Setup', 'SortColumn', 0);
      FSortDirection := ReadBool('Setup', 'SortDirection', False);
      If ParamStr(1) <> '' Then
        OpenFile(ParamStr(1))
      Else
        If FileExists(FFileName) Then
          OpenFile(FFileName);
      FocusNode(ReadString('Setup', 'SelectedNode', ''));
    Finally;
      Free;
    End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This method attempts to find the previously focused node in the opened
  profile file.

  @precon  None.
  @postcon Attempts to find the previously focused node in the opened
           profile file.

  @param   strFocusedNode as a String

**)
Procedure TfrmMainForm.FocusNode(strFocusedNode : String);

var
  strNode : String;
  iPos: Integer;
  Node: PVirtualNode;
  N: PVirtualNode;
  NodeData: ^TTreeData;
  boolFound: Boolean;

Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.FocusNode');
  Try
  {$ENDIF}
  If strFocusedNode <> '' Then
    Begin
      Node := Nil;
      N := vstProfileRecords.RootNode;
      iPos := Pos('|', strFocusedNode);
      strNode := Copy(strFocusedNode, 1, iPos - 1);
      boolFound := False;
      While (N <> Nil) And Not boolFound Do
        Begin
          Node := vstProfileRecords.GetFirstChild(N);
          While Node <> Nil Do
            Begin
              NodeData := vstProfileRecords.GetNodeData(Node);
              If AnsiCompareText(NodeData.FProfileRecord.Name, strNode) = 0 Then
                Begin
                  If iPos > 0 Then
                    Delete(strFocusedNode, 1, iPos)
                  Else
                    strFocusedNode := '';
                  iPos := Pos('|', strFocusedNode);
                  If iPos > 0 Then
                    strNode := Copy(strFocusedNode, 1, iPos - 1)
                  Else
                    strNode := strFocusedNode;
                  If strNode = '' Then
                    boolFound := True;
                  Break;
                End;
              Node := vstProfileRecords.GetNextSibling(Node);
            End;
          N := Node;
        End;
      If Node <> Nil Then
        Begin
          vstProfileRecords.FocusedNode := Node;
          vstProfileRecords.Selected[Node] := True;
          PopulateListView;
        End;
    End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
End;

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
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.lvAggregateListColumnClick');
  Try
  {$ENDIF}
  FAggregateList.Backward := FSortDirection;
  Case Column.Index Of
    0 : FAggregateList.Sort(asMethod);
    1 : FAggregateList.Sort(asTTT);
    2 : FAggregateList.Sort(asIPTT);
    3 : FAggregateList.Sort(asCC);
    4 : FAggregateList.Sort(asATTT);
    5 : FAggregateList.Sort(asAIPTT);
  End;
  FSortColumn := Column.Index;
  FSortDirection := FAggregateList.Backward;
  For i := 0 To lvAggregateList.Columns.Count - 1 Do
    lvAggregateList.Columns[i].ImageIndex := -1;
  If Not FAggregateList.Backward Then
    Column.ImageIndex := 0
  Else
    Column.ImageIndex := 1;
  PopulateAggregateList;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

   This method returns a string presentation of the path to the passed node
   from the root.

   @precon  None.
   @postcon Returns a string presentation of the path to the passed node from
            the root.

   @param   Node as a PVirtualNode
   @return  a String

 **)
Function TfrmMainForm.NodePath(Node : PVirtualNode) : String;

Var
  NodeData : ^TTreeData;

Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.NodePath');
  Try
  {$ENDIF}
  Result := '';
  While Node <> Nil Do
    Begin
      NodeData := vstProfileRecords.GetNodeData(Node);
      If NodeData <> Nil Then
        Begin
          If Result <> '' Then
            Result := '|' + Result;
          If NodeData.FProfileRecord <> Nil Then
            Result := NodeData.FProfileRecord.Name + Result;
        End;
      Node := vstProfileRecords.NodeParent[Node];
    End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
End;

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
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.OpenFile');
  Try
  {$ENDIF}
  If FileExists(strFileName) Then
    Begin
      FFileName := strFileName;
      FileAge(strFileName, FFileDate);
      Caption := Application.Title + ' - ' + strFileName;
      BuildProfileList(strFileName);
    End Else
      MessageDlg(Format(strFileNotFoundMsg, [strFileName]), mtWarning, [mbOK], 0);
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This method builds the internal list of profile information from the source
  text file.

  @precon  strFileName must be a valid text file..
  @postcon Builds the internal list of profile information from the source
           text file.

  @param   strFileName as a String

**)
Procedure TfrmMainForm.BuildProfileList(strFileName : String);

Var
  sl: TStringList;
  iLine: Integer;
  iFields : Integer;
  strLine : String;
  iErrorCode, iStackDepth : Integer;
  strClassName, strMethodName : String;
  dblTotalTime, dblInProcessTime, dblCallCount : Double;

Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.BuildProfileList');
  Try
  {$ENDIF}
  vstProfileRecords.Clear;
  lvAggregateList.Clear;
  FProfileInfoList.Clear;
  sl := TStringList.Create;
  Try
    FProgress.Init(1, strLoadingProfile, 'Opening Profile file...');
    sl.LoadFromFile(strFileName);
    FProgress.Init(sl.Count * 2, strLoadingProfile, 'Parsing Data...');
    Try
      For iLine := 0 To sl.Count - 1 Do
        Begin
          If iLine Mod 100 = 0 Then
            FProgress.UpdateProgress(iLine, Format('Parsing item %d...', [iLine]));
          strLine := sl[iLine];
          iFields := CharCount(',', strLine) + 1;
          If iFields > 1 Then
            Begin
              Val(GetField(strLine, ',', 1), iStackDepth, iErrorCode);
              If iErrorCode > 0 Then
                Continue; // Miss out field headers
              strClassName := GetField(strLine, ',', 2);
              strMethodName := GetField(strLine, ',', 3);
              Val(GetField(strLine, ',', 4), dblTotalTime, iErrorCode);
              Val(GetField(strLine, ',', 5), dblInProcessTime, iErrorCode);
              Val(GetField(strLine, ',', 6), dblCallCount, iErrorCode);
              FProfileInfoList.Add(
                TProfileRecord.Create(iStackDepth, strClassName, strMethodName,
                  dblTotalTime, dblInProcessTime, dblCallCount,
                  FProfileInfoList.Count - 1, iLine)
              );
            End Else
              FProfileInfoList.Add(TProfileHeader.Create(sl[iLine],
                FProfileInfoList.Count - 1, iLine));
        End;
      PopulateTreeView;
    Finally
      FProgress.Hide;
    End;
  Finally
    sl.Free;
  End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
End;

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
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.PopulateAggregateList');
  Try
  {$ENDIF}
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
        Item.SubItems.Add(Format('%1.3n (%1.2f%%)', [
          FAggregateList[i].TotalTime, dblPercentage
        ]));
        If FAggregateList.TotalTime > 0 Then
          dblPercentage := Int(FAggregateList[i].InProcessTime) /
            FAggregateList.TotalTime * 100.0
        Else
          dblPercentage := 100;
        Item.SubItems.Add(Format('%1.3n (%1.2f%%)', [
          FAggregateList[i].InProcessTime, dblPercentage
        ]));
        Item.SubItems.Add(Format('%1.0n', [FAggregateList[i].CallCount]));
        If FAggregateList.TotalTime > 0 Then
          dblPercentage := Int(FAggregateList[i].AverageTotalTime) /
            FAggregateList.TotalTime * 100.0
        Else
          dblPercentage := 100;
        Item.SubItems.Add(Format('%1.4n (%1.2f%%)', [
          FAggregateList[i].AverageTotalTime, dblPercentage
        ]));
        If FAggregateList.TotalTime > 0 Then
          dblPercentage := Int(FAggregateList[i].AverageInProcessTime) /
            FAggregateList.TotalTime * 100.0
        Else
          dblPercentage := 100;
        Item.SubItems.Add(Format('%1.4n (%1.2f%%)', [
          FAggregateList[i].AverageInProcessTime, dblPercentage
        ]));
      End;
  Finally
    lvAggregateList.Items.EndUpdate;
  End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This method populates the list view with information the lies on or below the
  selected item in the tree view.

  @precon  None.
  @postcon Populates the list view with information the lies on or below the
           selected item in the tree view.

**)
procedure TfrmMainForm.PopulateListView;

Var
  iStartRecord : Integer;
  TN : PVirtualNode;
  iEndRecord: Integer;
  iRecord : Integer;
  rec : TProfileRecord;
  iStartStackDepth : Integer;
  dblBaseTickTime : Double;
  NodeData: ^TTreeData;

begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.PopulateListView');
  Try
  {$ENDIF}
  dblBaseTickTime := 0;
  iStartStackDepth := 0;
  If vstProfileRecords.FocusedNode <> Nil Then
    If vstProfileRecords.FocusedNode <> FLastFocusedNode Then
      Begin
        FProgress.Init(1, strLoadingProfile, strBuildingListview);
        Try
          FLastFocusedNode := vstProfileRecords.FocusedNode;
          FAggregateList.Clear;
          NodeData := vstProfileRecords.GetNodeData(vstProfileRecords.FocusedNode);
          iStartRecord := (NodeData.FProfileRecord As TProfileBase).Index + 1;
          TN := vstProfileRecords.FocusedNode;
          While (TN.NextSibling = Nil) And
            (vstProfileRecords.NodeParent[TN] <> Nil) Do
            TN := vstProfileRecords.NodeParent[TN];
          If TN <> Nil Then
            TN := TN.NextSibling;
          If TN <> Nil Then
            Begin
              NodeData := vstProfileRecords.GetNodeData(TN);
              If NodeData <> Nil Then
                iEndRecord := (NodeData.FProfileRecord As TProfileBase).Index
              Else
                iEndRecord := FProfileInfoList.Count - 1;
            End Else
              iEndRecord := FProfileInfoList.Count - 1;
          FProgress.Init(iEndRecord - iStartRecord, strLoadingProfile,
            strBuildingListview);
          For iRecord := iStartRecord To iEndRecord Do
            Begin
              If iRecord Mod 1000 = 0 Then
                FProgress.UpdateProgress(iRecord - iStartRecord,
                  Format(strBuildingItem, [iRecord]));
              If FProfileinfoList[iRecord] Is TProfileRecord Then
                Begin
                  rec := FProfileinfoList[iRecord] As TProfileRecord;
                  If dblBaseTickTime = 0 Then
                    dblBaseTickTime := rec.TotalTime;
                  If (rec.StackDepth > 0) And (iStartStackDepth = 0) Then
                    iStartStackDepth := rec.StackDepth;
                  FAggregateList.Add(rec.ClsName + '.' + rec.MthdName, rec.TotalTime,
                    rec.InProcessTime, rec.CallCount);
                End;
            End;
        Finally
          FProgress.Hide;
        End;
        lvAggregateListColumnClick(Self, lvAggregateList.Columns[FSortColumn]);
      End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
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

    This procedure updates the root nodes of the tree view with the number of
    profiles underneath the root.

    @precon  None.
    @postcon Updates the root nodes of the tree view with the number of
             profiles underneath the root.

    @param   Root         as a TProfileHeader
    @param   iLastLine    as an Integer
    @param   iRootLine    as an Integer
    @param   dblTotalTime as a Double

  **)
  Procedure UpdateRootWithCount(Root : TProfileHeader; iLastLine, iRootLine : Integer;
    dblTotalTime : Double);
  Begin
    {$IFDEF PROFILECODE}
    CodeProfiler.Start('UpdateRootWithCount');
    Try
    {$ENDIF}
    If Root <> Nil Then
      Begin
        Root.TotalTime := dblTotalTime;
        Root.CallCount := iLastLine - iRootLine - 1;
      End;
    {$IFDEF PROFILECODE}
    Finally
      CodeProfiler.Stop;
    End;
    {$ENDIF}
  End;

Var
  i, j: Integer;
  dblTT : Double;
  iStartRecord: Integer;
  tnProfileRoot: TProfileHeader;
  tnProfileNode : PVirtualNode;
  rec : TProfileRecord;
  iLastStackDepth : Integer;
  tnParent : PVirtualNode;
  NodeData: ^TTreeData;

begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.PopulateTreeView');
  Try
  {$ENDIF}
  FProgress.Init(FProfileInfoList.Count * 2, strLoadingProfile, strBuildingTreeview);
  vstProfileRecords.BeginUpdate;
  Try
    iStartRecord := 0;
    tnProfileRoot := Nil;
    iLastStackDepth := 0;
    tnParent := Nil;
    tnProfileNode := Nil;
    dblTT := 0;
    vstProfileRecords.Clear;
    If FProfileInfoList.Count = 0 Then
      Exit;
    For i := 0 To FProfileInfoList.Count - 1 Do
      Begin
        If i Mod 1000 =  0 Then
          FProgress.UpdateProgress(FProfileInfoList.Count + i,
            Format(strProcessingTreeItem, [i]));
        If FProfileInfoList[i] Is TProfileHeader Then
          Begin
            UpdateRootWithCount(tnProfileRoot, i, iStartRecord, dblTT);
            tnProfileNode := vstProfileRecords.AddChild(Nil);
            NodeData := vstProfileRecords.GetNodeData(tnProfileNode);
            NodeData.FProfileRecord := FProfileInfoList[i] As TProfileHeader;
            tnProfileRoot := FProfileInfoList[i] As TProfileHeader;
            dblTT := 0;
            iLastStackDepth := 0;
            iStartRecord := i;
          End Else
          Begin
            rec := FProfileInfoList[i] As TProfileRecord;
            If rec.StackDepth = 1 Then
              dblTT := dblTT + rec.TotalTime;
            If rec.StackDepth > iLastStackDepth Then
              tnParent := tnProfileNode;
            For j := rec.StackDepth To iLastStackDepth - 1 Do
              tnProfileNode := vstProfileRecords.NodeParent[tnProfileNode];
            If rec.StackDepth <= iLastStackDepth Then
              tnParent := vstProfileRecords.NodeParent[tnProfileNode];
            tnProfileNode := vstProfileRecords.AddChild(tnParent);
            NodeData := vstProfileRecords.GetNodeData(tnProfileNode);
            NodeData.FProfileRecord := FProfileInfoList[i] As TProfileRecord;
            iLastStackDepth := rec.StackDepth;
          End;
      End;
    UpdateRootWithCount(tnProfileRoot, FProfileInfoList.Count, iStartRecord, dblTT);
  Finally
    vstProfileRecords.EndUpdate;
  End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This method saves the applications settings to the registry so that the
  state of the application can be restored when next opened.

  @precon  None.
  @postcon Saves the applications settings to the registry.

**)
procedure TfrmMainForm.SaveSettings;

Var
  recWndPlmt : TWindowPlacement;

begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.SaveSettings');
  Try
  {$ENDIF}
  With TIniFile.Create(FRootKey) Do
    Try
      recWndPlmt.Length := SizeOf(TWindowPlacement);
      GetWindowPlacement(Handle, @recWndPlmt);
      WriteInteger('Setup', 'Top', recWndPlmt.rcNormalPosition.Top);
      WriteInteger('Setup', 'Left', recWndPlmt.rcNormalPosition.Left);
      WriteInteger('Setup', 'Height',
        recWndPlmt.rcNormalPosition.Bottom - recWndPlmt.rcNormalPosition.Top);
      WriteInteger('Setup', 'Width',
        recWndPlmt.rcNormalPosition.Right - recWndPlmt.rcNormalPosition.Left);
      WriteInteger('Setup', 'WindowState', Byte(WindowState));
      WriteString('Setup', 'FileName', FFileName);
      WriteInteger('Setup', 'AggregateHeight', lvAggregateList.Height);
      WriteInteger('AggregateColumnWidths', 'Class.Method', lvAggregateList.Column[0].Width);
      WriteInteger('AggregateColumnWidths', 'TotalTickCount', lvAggregateList.Column[1].Width);
      WriteInteger('AggregateColumnWidths', 'InProcessTickCount', lvAggregateList.Column[2].Width);
      WriteInteger('AggregateColumnWidths', 'CallCount', lvAggregateList.Column[3].Width);
      WriteInteger('AggregateColumnWidths', 'AverageTotalTickCount', lvAggregateList.Column[4].Width);
      WriteInteger('AggregateColumnWidths', 'AverageInProcessTickCount', lvAggregateList.Column[5].Width);
      WriteInteger('Columns', 'Class.Method', vstProfileRecords.Header.Columns[0].Width);
      WriteInteger('Columns', 'Total Time', vstProfileRecords.Header.Columns[1].Width);
      WriteInteger('Columns', 'In Process', vstProfileRecords.Header.Columns[2].Width);
      WriteInteger('Columns', 'Call Count', vstProfileRecords.Header.Columns[3].Width);
      WriteInteger('Columns', 'Average Total Time', vstProfileRecords.Header.Columns[4].Width);
      WriteInteger('Columns', 'Average In Process Time', vstProfileRecords.Header.Columns[5].Width);
      WriteInteger('Setup', 'SortColumn', FSortColumn);
      WriteBool('Setup', 'SortDirection', FSortDirection);
      WriteString('Setup', 'SelectedNode', NodePath(vstProfileRecords.FocusedNode));
    Finally
      Free;
    End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This is an on click event handler for the Tree View.

  @precon  None.
  @postcon Updates the list view when the item in the tree view is changed.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.tvProfileTreeClick(Sender: TObject);
begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.tvProfileTreeClick');
  Try
  {$ENDIF}
  PopulateListView;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
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
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.tvProfileTreeKeyPress');
  Try
  {$ENDIF}
  If Key = #13 Then
    Begin
      tvProfileTreeClick(Sender);
      Key := #0;
    End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This is an on get hint event handler for the virtual string tree.

  @precon  None.
  @postcon Gets the same text as GetText for the Hint.

  @param   Sender         as a TBaseVirtualTree
  @param   Node           as a PVirtualNode
  @param   Column         as a TColumnIndex
  @param   LineBreakStyle as a TVTTooltipLineBreakStyle as a reference
  @param   HintText       as a WideString as a reference

**)
procedure TfrmMainForm.vstProfileRecordsGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: WideString);
begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.vstProfileRecordsGetHint');
  Try
  {$ENDIF}
  vstProfileRecordsGetText(Sender, Node, Column, ttNormal, HintText);
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This is an on get image index event handler for the virtual tree control.

  @precon  None.
  @postcon Returns different image indexes for Records and Headers.

  @param   Sender     as a TBaseVirtualTree
  @param   Node       as a PVirtualNode
  @param   Kind       as a TVTImageKind
  @param   Column     as a TColumnIndex
  @param   Ghosted    as a Boolean as a reference
  @param   ImageIndex as an Integer as a reference

**)
procedure TfrmMainForm.vstProfileRecordsGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);

Var
  NodeData : ^TTreeData;

begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.vstProfileRecordsGetImageIndex');
  Try
  {$ENDIF}
  NodeData := Sender.GetNodeData(Node);
  If Column = 0 Then
    Begin
      If NodeData.FProfileRecord Is TProfileHeader Then
        ImageIndex := 0
      Else
        ImageIndex := 1;
    End Else
      ImageIndex := -1;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This is an on get text event handler for the virtual tree view.

  @precon  None.
  @postcon Returns the text for the tree node depending upon the record
           attached.

  @param   Sender   as a TBaseVirtualTree
  @param   Node     as a PVirtualNode
  @param   Column   as a TColumnIndex
  @param   TextType as a TVSTTextType
  @param   CellText as a WideString as a reference

**)
procedure TfrmMainForm.vstProfileRecordsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);

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
    {$IFDEF PROFILECODE}
    CodeProfiler.Start('GetAppAndDate');
    Try
    {$ENDIF}
    strText := strText;
    iCharCount := CharCount('\', strText);
    iPos := PosOfNthChar(strText, '\', iCharCount);
    Result := Copy(strText, iPos + 1, Length(strText));
    {$IFDEF PROFILECODE}
    Finally
      CodeProfiler.Stop;
    End;
    {$ENDIF}
  end;

Var
  NodeData : ^TTreeData;
  rec : TProfileRecord;

begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TfrmMainForm.vstProfileRecordsGetText');
  Try
  {$ENDIF}
  NodeData := Sender.GetNodeData(Node);
  If NodeData.FProfileRecord Is TProfileHeader Then
    Begin
      Case Column Of
        0: CellText := GetAppAndDate((NodeData.FProfileRecord As
          TProfileHeader).Header);
        1: CellText := Format('%1.3n', [NodeData.FProfileRecord.TotalTime]);
        3: CellText := Format('%1.0n', [NodeData.FProfileRecord.CallCount]);
      Else
        CellText := '';
      End
    End Else
    Begin
      rec := NodeData.FProfileRecord As TProfileRecord;
      Case Column Of
        1: CellText := Format('%1.3n', [rec.TotalTime]);
        2: CellText := Format('%1.3n', [rec.InProcessTime]);
        3: CellText := Format('%1.0n', [rec.CallCount]);
        4: CellText := Format('%1.4n', [rec.AverageTotalTime]);
        5: CellText := Format('%1.4n', [rec.AverageInProcessTime]);
      Else
        CellText := Format('%s.%s', [rec.ClsName, rec.MthdName]);
      End;
    End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

end.
