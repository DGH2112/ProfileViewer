(**

  This module contains a class to monitor profiling information in a tree of
  profiles.

  @Version 1.0
  @Date    29 Apr 2018
  @Author  David Hoyle

**)
Unit Profiler;

Interface

Uses
  {$IFNDEF CONSOLE}
  Forms,
  StdCtrls,
  {$ENDIF}
  Classes,
  Contnrs;

{$IFDEF PROFILECODE}
Type
  (** This class represent a single element of profile information. It can
      contain sub elements of itself to build up and stack similar to that
      of the code being profiled. **)
  TProfile = Class
  Strict Private
    FMethodName   : String;
    FStartTick    : Double;
    FDurationTick : Double;
    FInProcessTick: Double;
    FCallCount    : Int64;
    FProfiles     : TObjectList;
    FParent       : TProfile;
    FStackDepth   : Int64;
  Strict Protected
    Function FindProfile(Const strMethodName: String; Const iStackDepth: Integer): TProfile;
    Procedure StartTiming;
    (**
      This property returns the total duration of the profiles calls, i.e. TickCount.
      @precon  None.
      @postcon Returns the total duration of the profiles calls.
      @return  a Double
    **)
    Property DurationTick: Double Read FDurationTick;
  Public
    Constructor Create(Const strMethod: String; Const iStackDepth: Integer; Const objParent: TProfile);
    Destructor Destroy; Override;
    Function StartProfile(Const strMethodName: String; Const iStackDepth: Integer): TProfile;
    Function StopProfile: TProfile;
    Procedure DumpProfileInformation(Const slProfileFile: TStringList);
    (**
      This property returns the number of Call Counts on the profile.
      @precon  None.
      @postcon Returns the number of Call Counts on the profile.
      @return  an Int64
    **)
    Property CallCount: Int64 Read FCallCount;
  End;

  (** This class handles all the TProfile instances in a tree structure. **)
  TProfiler = Class
  Strict Private
    FStackTop      : Integer;
    FRootProfile   : TProfile;
    FCurrentProfile: TProfile;
    {$IFNDEF CONSOLE}
    FProgressForm  : TForm;
    FLabel         : TLabel;
    {$ENDIF}
  Strict Protected
    Procedure DumpProfileInformation;
    {$IFNDEF CONSOLE}
    Procedure Msg(Const strMsg: String; Const boolForce : Boolean = False);
    {$ENDIF}
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Start(Const strMethodName: String);
    Procedure Stop;
  End;

Var
  (** This is a public variable which give the calling code access to the
      instance of the TPRofiler class. **)
  CodeProfiler: TProfiler;
{$ENDIF}

Implementation

Uses
  {$IFNDEF CONSOLE}
  Controls,
  ExtCtrls,
  {$ENDIF}
  SysUtils,
  Windows;

{$IFDEF PROFILECODE}

Const
  (** This is the time in millisecond between outputting messages to the form. **)
  iUpdateTime = 10;

Var
  (** This is the time in milliseconds of the last output message. **)
  iLastMsg : Int64;

(**

  This method returns the performance tick count from the system.

  @precon  None.
  @postcon Returns the performance tick count from the system.

  @return  an Double

**)
Function TickTime: Double;

Const
  dblMutliplier = 1000.0;

Var
  t, f: Int64;

Begin
  QueryPerformanceCounter(t);
  QueryPerformanceFrequency(f);
  Result := dblMutliplier * Int(t) / Int(f);
End;

(**

  This is a constructor for the TProfile class.

  @precon  None.
  @postcon Creates a container for the sub profiles.

  @param   strMethod   as a String as a constant
  @param   iStackDepth as an Integer as a constant
  @param   objParent   as a TProfile as a constant

**)
Constructor TProfile.Create(Const strMethod: String; Const iStackDepth: Integer;
  Const objParent: TProfile);

Begin
  FProfiles      := TObjectList.Create(True);
  FDurationTick  := 0;
  FInProcessTick := 0;
  FMethodName    := strMethod;
  FParent        := objParent;
  FStackDepth    := iStackDepth;
End;

(**


  This is a destructor for the TProfile class.

  @precon  None.
  @postcon Frees sub profiles.


**)
Destructor TProfile.Destroy;
Begin
  FProfiles.Free;
  Inherited Destroy;
End;

(**

  This method outputs the profiles information to the given file handle.

  @precon  None.
  @postcon Outputs the profiles information to the given file handle.

  @param   slProfileFile as a TStringList as a constant

**)
Procedure TProfile.DumpProfileInformation(Const slProfileFile: TStringList);

Var
  i   : Integer;
  P   : TProfile;
  iPos: Integer;

Begin
  If FMethodName <> '' Then
    Begin
      For i := 0 To FProfiles.Count - 1 Do
        Begin
          P              := FProfiles[i] As TProfile;
          FInProcessTick := FInProcessTick - P.DurationTick;
        End;
      iPos := Pos('.', FMethodName);
      slProfileFile.Add(Format('%d,%s,%s,%1.4f,%1.4f,%d', [
        FStackDepth,
        Copy(FMethodName, 1, iPos - 1),
        Copy(FMethodName, iPos + 1, Length(FMethodName) - iPos),
        FDurationTick,
        FInProcessTick,
        FCallCount
        ]));
    End;
  For i := 0 To FProfiles.Count - 1 Do
    (FProfiles[i] As TProfile).DumpProfileInformation(slProfileFile);
End;

(**

  This method attempts to find the named method in the profile collection. If found the profile is 
  returned else a new profile is added and that profile returned.

  @precon  None.
  @postcon Attempts to find the named method in the profile collection. If found the profile is returned
           else a new profile is added and that profile returned.

  @param   strMethodName as a String as a constant
  @param   iStackDepth   as an Integer as a constant
  @return  a TProfile

**)
Function TProfile.FindProfile(Const strMethodName: String; Const iStackDepth: Integer): TProfile;

Var
  i: Integer;
  P: TProfile;

Begin
  For i := 0 To FProfiles.Count - 1 Do
    Begin
      P := FProfiles[i] As TProfile;
      If AnsiCompareText(P.FMethodName, strMethodName) = 0 Then
        Begin
          Result := FProfiles[i] As TProfile;
          Exit;
        End;
    End;
  Result := TProfile.Create(strMethodName, iStackDepth, Self);
  FProfiles.Add(Result);
End;

(**

  This method starts the process of monitoring the current methods profile session.

  @precon  None.
  @postcon Starts the process of monitoring the current methods profile session.

  @param   strMethodName as a String as a constant
  @param   iStackDepth   as an Integer as a constant
  @return  a TProfile

**)
Function TProfile.StartProfile(Const strMethodName: String; Const iStackDepth: Integer): TProfile;

Begin
  Result := FindProfile(strMethodName, iStackDepth);
  Result.StartTiming;
End;

(**

  This method starts the timing of the current profile session.

  @precon  None.
  @postcon Starts the timing of the current profile session.

**)
Procedure TProfile.StartTiming;

Begin
  FStartTick := TickTime;
  Inc(FCallCount);
End;

(**

  This method stop profiling the current method and returns the parent profile.

  @precon  None.
  @postcon Stop profiling the current method and returns the parent profile.

  @return  a TProfile

**)
Function TProfile.StopProfile: TProfile;

Begin
  FDurationTick  := FDurationTick + (TickTime - FStartTick);
  FInProcessTick := FDurationTick; { Make the same as FDuration. A call to
                                     calculate this needs to be made after
                                     profiling and before dumping the
                                     information. }
  Result := FParent;
End;

{ TProfiler }

(**


  This is a constructor for the TProfiler class.

  @precon  None.
  @postcon Creates an initial TProfile instance for the tree structure.


**)
Constructor TProfiler.Create;

ResourceString
  strCodeProfiling = 'Code Profiling...';
  strLoading = 'Loading...';
  strProfilerStarted = 'Profiler started!';

Const
  strFontName = 'Tahoma';
  iDefaultFormWidth = 400;
  iDefaultFormHeight = 50;
  iMargin = 5;
  iFontSize = 10;

Var
  P: TPanel;

Begin
  {$IFNDEF CONSOLE}
  FProgressForm               := TForm.CreateNew(Application.MainForm);
  FProgressForm.BorderStyle   := bsNone;
  FProgressForm.Caption       := strCodeProfiling;
  FProgressForm.ClientWidth   := iDefaultFormWidth;
  FProgressForm.ClientHeight  := iDefaultFormHeight;
  FProgressForm.Top           := Screen.WorkAreaHeight - FProgressForm.Height;
  FProgressForm.Left          := Screen.WorkAreaWidth - FProgressForm.Width;
  FProgressForm.FormStyle     := fsStayOnTop;
  FProgressForm.Margins.Left  := iMargin;
  FProgressForm.Margins.Right := iMargin;
  FProgressForm.BorderIcons   := [];
  P                           := TPanel.Create(FProgressForm);
  P.Parent                    := FProgressForm;
  P.BevelInner                := bvLowered;
  P.BevelOuter                := bvRaised;
  P.Align                     := alClient;
  FLabel                      := TLabel.Create(FProgressForm);
  FLabel.Parent               := P;
  FLabel.Align                := alClient;
  FLabel.Layout               := tlCenter;
  FLabel.Caption              := strLoading;
  FLabel.Font.Name            := strFontName;
  FLabel.Font.Size            := iFontSize;
  FLabel.WordWrap             := False;
  FLabel.EllipsisPosition     := epEndEllipsis;
  FLabel.AlignWithMargins     := True;
  FLabel.Margins.SetBounds(iMargin, iMargin, iMargin, iMargin);
  FProgressForm.Show;
  Msg(strProfilerStarted);
  {$ENDIF}
  FRootProfile := TProfile.Create('', 0, Nil);
End;

(**


  This is a destructor for the TProfiler class.

  @precon  None.
  @postcon Frees the memory used by the profile tree.


**)
Destructor TProfiler.Destroy;

Begin
  DumpProfileInformation;
  FRootProfile.Free;
  {$IFNDEF CONSOLE}
  FProgressForm.Free;
  {$ENDIF}
  Inherited Destroy;
End;

(**


  This method starts the process of calculating and outputting the profile tree.

  @precon  None.
  @postcon Starts the process of calculating and outputting the profile tree.

**)
Procedure TProfiler.DumpProfileInformation;

ResourceString
  strProcessingProfileInformation = 'Processing the profile information...';
  strLoadingExistingData = 'Loading existing data...';
  strProfileDumpForApplication = 'Profile Dump For Application ';
  strOn = ' on ';
  strStackDepth = 'Stack Depth';
  strClass = 'Class';
  strMethodName = 'Method Name';
  strTotalTickCountMs = 'Total Tick Count (ms)';
  strInProcessTickCountMs = 'In Process Tick Count (ms)';
  strCallCount = 'Call Count';
  strProcessingNewData = 'Processing new data...';
  strSavingData = 'Saving data...';

Const
  strProfileExt = '.profile';
  strDateFmt = 'ddd dd/mmm/yyyy @ hh:mm:ss';
  iSleepInternal = 250;

Var
  strBuffer                     : Array [0 .. MAX_PATH] Of Char;
  strModuleFileName, strFileName: String;
  slProfile                     : TStringList;

Begin
  {$IFDEF CONSOLE}
  WriteLn(strProcessingProfileInformation);
  {$ELSE}
  Msg(strProcessingProfileInformation);
  {$ENDIF}
  GetModuleFileName(hInstance, strBuffer, MAX_PATH);
  strFileName       := StrPas(strBuffer);
  strModuleFileName := strFileName;
  strFileName       := ChangeFileExt(strFileName, strProfileExt);
  slProfile         := TStringList.Create;
  Try
    {$IFNDEF CONSOLE}
    Msg(strLoadingExistingData);
    {$ELSE}
    WriteLn(strLoadingExistingData);
    {$ENDIF}
    If FileExists(strFileName) Then
      slProfile.LoadFromFile(strFileName);
    slProfile.Add(strProfileDumpForApplication + strModuleFileName + strOn +
      FormatDateTime(strDateFmt, Now));
    slProfile.Add(Format('%s,%s,%s,%s,%s,%s', [
      strStackDepth,
      strClass,
      strMethodName,
      strTotalTickCountMs,
      strInProcessTickCountMs,
      strCallCount
      ]));
    {$IFNDEF CONSOLE}
    Msg(strProcessingNewData);
    {$ELSE}
    WriteLn(strProcessingNewData);
    {$ENDIF}
    FRootProfile.DumpProfileInformation(slProfile);
    {$IFNDEF CONSOLE}
    Msg(strSavingData);
    {$ELSE}
    WriteLn(strSavingData);
    {$ENDIF}
    slProfile.SaveToFile(strFileName);
  Finally
    slProfile.Free;
  End;
  Sleep(iSleepInternal);
End;

{$IFNDEF CONSOLE}
(**

  This method outputs a message to the profile form.

  @precon  None.
  @postcon Outputs a message to the profile form.

  @param   strMsg    as a String as a constant
  @param   boolForce as a Boolean as a constant

**)
Procedure TProfiler.Msg(Const strMsg: String; Const boolForce : Boolean = False);

Begin
  If (iLastMsg + iUpdateTime < GetTickCount) Or boolForce Then
    Begin
      FLabel.Caption := strMsg;
      Application.ProcessMessages;
      iLastMsg := GetTickCount;
    End;
End;
{$ENDIF}

(**

  This method starts the profiling of the current method.

  @precon  None.
  @postcon Starts the profiling of the current method.

  @param   strMethodName as a String as a constant

**)
Procedure TProfiler.Start(Const strMethodName: String);

ResourceString
  strProfiling = 'Profiling: ';

Begin
  If FStackTop = 0 Then
    FCurrentProfile := FRootProfile;
  FStackTop         := FStackTop + 1;
  FCurrentProfile   := FCurrentProfile.StartProfile(strMethodName, FStackTop);
  Msg(strProfiling + strMethodName);
End;

(**

  This method stops the profiling of the current method.

  @precon  None.
  @postcon Stops the profiling of the current method.

**)
Procedure TProfiler.Stop;

ResourceString
  strIdle = 'Idle.';

Begin
  FStackTop := FStackTop - 1;
  If FStackTop < 0 Then
    FStackTop     := 0;
  FCurrentProfile := FCurrentProfile.StopProfile;
  If FStackTop <= 0 Then
    Msg(strIdle, True);
End;
{$ENDIF}

(** Creates the profiler on loading the module. **)
Initialization
  {$IFDEF PROFILECODE}
  iLastMsg := 0;
  CodeProfiler := TProfiler.Create;
  {$ENDIF}
(** Frees (and writes data) the profiler on unloading the module **)
Finalization
  {$IFDEF PROFILECODE}
  CodeProfiler.Free;
  {$ENDIF}
End.
