(**

  This module contains a class which represents the application interface
  using a tree view for the profile outline and a list view to display the
  highlighted sections of the profiles information in a list report.

  @Author  David Hoyle
  @Date    30 Dec 2005
  @Version 1.0

**)
unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Registry, ActnList, ComCtrls, ExtCtrls, Menus, ImgList, ToolWin;

type
  (** A class to represent the main application form. **)
  TfrmMainForm = class(TForm)
    mmMenu: TMainMenu;
    mmiFile: TMenuItem;
    mmiHelp: TMenuItem;
    stbStatusBar: TStatusBar;
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
    procedure actFileRefreshExecute(Sender: TObject);
    procedure actFileDeleteExecute(Sender: TObject);
    procedure actFileCloseExecute(Sender: TObject);
    procedure tvProfileTreeChange(Sender: TObject; Node: TTreeNode);
    procedure lvProfileInformationCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FProfileFile : TStringList;
    FFileName : String;
    FFileDate : TDate;
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure OpenFile(strFileName : String);
    Procedure PopulateTreeView;
    Procedure PopulateListView;
  public
    { Public declarations }
  end;

var
  (** A delphi defined global variable for the auto form creation process. **)
  frmMainForm: TfrmMainForm;

implementation

Uses
  DGHLibrary, About;

ResourceString
  (** A resource string for prompting that a file has not been found. **)
  strFileNotFoundMsg = 'The vba profile file "%s" was not found.';
  (** A resource string for letting the user know that they need to select a
      tree node. **)
  strSelectProfileNode = 'You need to select an item in the profile tree vie' +
  'w.';
  (** A resource string for to let the user know that the file has changed. **)
  strFileHasChanged = 'The file has changed since loading and needs to be re' +
  'loaded before a branch can be deleted.';

Const
  (** A private constant to define the root registry key for the applications
      settings **)
  strRootKey : String = 'Software\Season''s Fall\VBAProfileViewer\';

{$R *.dfm}

(**

  This function trims the given text of leading and trailing spaces and removes
  any double quotes that are presents in the first and last character position.

  @precon  None
  @postcon Trims the given text of leading and trailing spaces and removes
           any double quotes that are presents in the first and last character
           position.

  @param   strText as a String
  @return  a String

**)
Function N(strText : String) : String;

Begin
  Result := Trim(strText);
  If Length(Result) > 0 Then
    If Result[1] = '"' Then
      Result := Copy(Result, 2, Length(Result) - 2);
End;

(**

  This method is an on execute event handler for the File Close action.

  @precon  None.
  @postcon Closes the current file.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actFileCloseExecute(Sender: TObject);
begin
  FProfileFile.Clear;
  Caption := Application.Title + ' - (no file)';
  PopulateTreeView;
end;

(**

  This is an on execute event handler for the File Delete action.

  @precon  None.
  @postcon Deletes the current root profile from the file.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actFileDeleteExecute(Sender: TObject);

Var
  tnRoot : TTreeNode;
  iFirstLine, iLastLine : Integer;
  iLine : Integer;
  iFields: Integer;

begin
  tnRoot := tvProfileTree.Selected;
  If tnRoot = Nil Then
    MessageDlg(strSelectProfileNode, mtWarning, [mbOK], 0)
  Else
    Begin
      If (FileAge(FFilename) <> FFileDate) And (FFileDate > 0) Then
        Raise Exception.Create(strFileHasChanged);
      // Find root node
      While tnRoot.Parent <> Nil Do
        tnRoot := tnRoot.Parent;
      iFirstLine := Integer(tnRoot.Data);
      iLastLine := iFirstLine - 1;
      For iLine := iFirstLine + 1 To FProfileFile.Count - 1 Do
        Begin
          iFields := CharCount(',', FProfileFile[iLine]) + 1;
          If iFields = 1 Then
            Begin
              iLastLine := iLine - 1;
              Break;
            End;
        End;
      For iLine := iLastLine DownTo iFirstLine Do
        FProfileFile.Delete(iLine);
      FProfileFile.SaveToFile(FFileName);
      PopulateTreeView;
    End;
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
  TfrmAbout.ShowAbout;
end;

(**

  This is the forms on create event handler.

  @precon  None.
  @postcon Loads the applications settings from the registry.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.FormCreate(Sender: TObject);
begin
  TfrmAbout.ShowAbout;
  FProfileFile := TStringList.Create;
  Caption := Application.Title + ' - (no file)';
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
  With TRegIniFile.Create(strRootKey) Do
    Begin
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
      FFileName := ReadString('Setup', 'FileName', '')
    End;
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
      FFileDate := FileAge(strFileName);
      Caption := Application.Title + ' - ' + strFileName;
      PopulateTreeView;
      PopulateListView;
    End Else
      MessageDlg(Format(strFileNotFoundMsg, [strFileName]), mtWarning, [mbOK], 0);
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
  iStartLine : Integer;
  iLine : Integer;
  iField, iFields : Integer;
  liProfile : TListItem;
  strFirstField: String;
  iStackDepth : Integer;
  iErrorCode : Integer;
  iStartStackDepth : Integer;

begin
  iStartStackDepth := 0;
  lvProfileInformation.Items.BeginUpdate;
  Try
    lvProfileInformation.Items.Clear;
    If tvProfileTree.Selected <> Nil Then
      Begin
        iStartLine := Integer(tvProfileTree.Selected.Data);
        For iLine := iStartLine To FProfileFile.Count - 1 Do
          Begin
            iFields := CharCount(',', FProfileFile[iLine]) + 1;
            strFirstField := N(GetField(FProfileFile[iLine], ',', 1));
            Val(strFirstField, iStackDepth, iErrorCode);
            If (iStackDepth <= iStartStackDepth) and (iLine > iStartLine + 1) Then
              Break;
            If (iStackDepth > 0) And (iStartStackDepth = 0) Then
              iStartStackDepth := iStackDepth;
            If (iFields > 1) And (iErrorCode = 0) Then
              Begin
                liProfile := lvProfileInformation.Items.Add;
                For iField := 1 To iFields Do
                  Case iField Of
                    1: liProfile.Caption := N(GetField(FProfileFile[iLine], ',', iField));
                    2..9: liProfile.SubItems.Add(N(GetField(FProfileFile[iLine], ',', iField)));
                  End;
                liProfile.SubItems.Add(IntToStr(iLine));
              End
          End;
      End
  Finally
    lvProfileInformation.Items.EndUpdate;
  End;
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
    strText := N(strText);
    iCharCount := CharCount('\', strText);
    iPos := PosOfNthChar(strText, '\', iCharCount);
    Result := Copy(strText, iPos + 1, Length(strText));
  end;

  (**

    This procedure updates the root nodes of the tree view with the number
    of profiles underneath the root.

    @precon  None.
    @postcon Updates the root nodes of the tree view with the number
             of profiles underneath the root.

    @param   tnRoot    as a TTreeNode
    @param   iLastLine as an Integer
    @param   iRootLine as an Integer

  **)
  Procedure UpdateRootWithCount(tnRoot : TTreeNode; iLastLine, iRootLine : Integer);
  Begin
    If tnRoot <> Nil Then
      tnRoot.Text := tnRoot.Text + ' (' + IntToStr(iLastLine - iRootLine - 2) + ')'
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

begin
  tvProfileTree.Items.BeginUpdate;
  Try
    tvProfileTree.OnChange := Nil;
    tvProfileTree.Items.Clear;
    tnProfileNode := Nil;
    tnParent := Nil;
    iLastStackDepth := 0;
    iStartLine := 0;
    tnProfileRoot := Nil;
    For iLine := 0 To FProfileFile.Count - 1 Do
      Begin
        iFields := CharCount(',', FProfileFile[iLine]) + 1;
        If iFields = 1 Then
          Begin
            If FProfileFile[iLine] <> '' Then
              Begin
                UpdateRootWithCount(tnProfileRoot, iLine, iStartLine);
                tnProfileNode := tvProfileTree.Items.AddObject(Nil,
                  GetAppAndDate(FProfileFile[iLine]), TObject(iLine));
                tnProfileRoot := tnProfileNode;
                iStartLine := iLine;
              End;
            iLastStackDepth := 0;
          End Else
          Begin
            strFirstField := GetField(FProfileFile[iLine], ',', 1);
            Val(strFirstField, iStackDepth, iErrorCode);
            If iErrorCode = 0 Then
              Begin
                If iStackDepth > iLastStackDepth Then
                  tnParent := tnProfileNode;
                For i := iStackDepth To iLastStackDepth - 1 Do
                  tnProfileNode := tnProfileNode.Parent;
                If iStackDepth <= iLastStackDepth Then
                  tnParent := tnProfileNode.Parent;
                tnProfileNode := tvProfileTree.Items.AddChildObject(
                  tnParent, strFirstField + ') ' +
                  N(GetField(FProfileFile[iLine], ',', 2)) + '.' +
                  N(GetField(FProfileFile[iLine], ',', 3)) + ' (' +
                  N(GetField(FProfileFile[iLine], ',', 4)) + ',' +
                  N(GetField(FProfileFile[iLine], ',', 5)) + ',' +
                  N(GetField(FProfileFile[iLine], ',', 6)) + ')', TObject(iLine));
                iLastStackDepth := iStackDepth;
              End;
          End;
      End;
    UpdateRootWithCount(tnProfileRoot, FProfileFile.Count - 1, iStartLine);
    tvProfileTree.OnChange := tvProfileTreeChange;
    tvProfileTree.Selected := Nil;
    PopulateListView;
  Finally
    tvProfileTree.Items.EndUpdate;
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
  With TRegIniFile.Create(strRootKey) Do
    Begin
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
    End;
end;

(**

  This is an on change event handler for the Tree View.

  @precon  None.
  @postcon Updates the list view when the item in the tree view is changed.

  @param   Sender as a TObject
  @param   Node   as a TTreeNode

**)
procedure TfrmMainForm.tvProfileTreeChange(Sender: TObject; Node: TTreeNode);
begin
  PopulateListView;
end;

end.
