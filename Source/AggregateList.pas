(**

  This module contains code to create a list of aggregated information from
  the profile information.

  @Version 1.0
  @Author  David Hoyle
  @Date    11 Oct 2008

**)
Unit AggregateList;

Interface

Uses
  Contnrs;

Type
  (** A class to represent a record of aggregated information. **)
  TAggregateRecord = Class
  Strict Private
    FMethodName    : String;
    FTotalTime     : Extended;
    FInProcessTime : Extended;
    FCallCount     : Extended;
  Strict Protected
    Function GetAverageInProcessTime: Extended;
    Function GetAverageTotalTime: Extended;
  Public
    Constructor Create(strMethodName : String);
    (**
      This property returns the class + method name of the record.
      @precon  None.
      @postcon Returns the class + method name of the record.
      @return  a String
    **)
    Property Method : String Read FMethodName;
    (**
      This property gets and sets the TotalTime of the record.
      @precon  None.
      @postcon Gets and sets the TotalTime of the record.
      @return  an Extended
    **)
    Property TotalTime : Extended Read FTotalTime Write FTotalTime;
    (**
      This property gets and sets the InProcess Time for the record.
      @precon  None.
      @postcon Gets and sets the InProcess Time for the record.
      @return  an Extended
    **)
    Property InProcessTime : Extended Read FInProcessTime Write FInProcessTime;
    (**
      This property gets and sets the call count for the record.
      @precon  None.
      @postcon Gets and sets the call count for the record.
      @return  an Extended
    **)
    Property CallCount : Extended Read FCallCount Write FCallCount;
    (**
      This property returns the average Total Time over all the call counts.
      @precon  None.
      @postcon Returns the average Total Time over all the call counts.
      @return  an Extended
    **)
    Property AverageTotalTime : Extended Read GetAverageTotalTime;
    (**
      This property returns the average InProcess Time over all the call counts.
      @precon  None.
      @postcon Returns the average InProcess Time over all the call counts.
      @return  an Extended
    **)
    Property AverageInProcessTime : Extended Read GetAverageInProcessTime;
  End;

  (** An enumerate to define sorting on the list. **)
  TAggregateSort = (asUnknown, asMethod, asTTT, asIPTT, asCC, asATTT, asAIPTT);

  (** A class to represnt the collection of aggregate records. **)
  TAggregateList = Class
  Strict Private
    FAggregateList : TObjectList;
    FTotalTime: Extended;
    FLastSort : TAggregateSort;
    FBackward : Boolean;
  Strict Protected
    Function GetItem(iIndex: Integer): TAggregateRecord;
    Function GetCount : Integer;
    Function Find(strMethodName : String) : Integer;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Add(strMethodName: String; iTTT, iIPTT, iCC : Extended);
    Procedure Clear;
    Procedure Sort(AggregateSort : TAggregateSort);
    (**
      This property returns the instance of the indexed profile record.
      @precon  None.
      @postcon Returns the instance of the indexed profile record.
      @param   iIndex as       an Integer
      @return  a TAggregateRecord
    **)
    Property Item[iIndex : Integer] : TAggregateRecord Read GetItem; Default;
    (**
      This method outputs the number of records in the profile.
      @precon  None.
      @postcon Returns the number of records in the profile.
      @return  an Integer
    **)
    Property Count : Integer Read GetCount;
    (**
      This property gets and sets the total time for the profile.
      @precon  None.
      @postcon Gets and sets the total time for the profile.
      @return  an Extended
    **)
    Property TotalTime : Extended Read FTotalTime Write FTotalTime;
    (**
      This property returns whether the list is sorted backwards.
      @precon  None.
      @postcon Returns whether the list is sorted backwards.
      @return  a Boolean
    **)
    Property Backward : Boolean Read FBackward Write FBackward;
  End;

Implementation

Uses
  SysUtils, Windows {$IFDEF PROFILECODE}, Profiler {$ENDIF};

Var
  (** A private variable to define the sort type in the AggregateSort procedure. **)
  ASort : TAggregateSort;
  (** A private variable to define the direction of the sort in the
      AggregateSort procedure. **)
  boolBackward : Boolean;

(**

  This is a sort procedure for the TObjectList class.

  @precon  None.
  @postcon Sorts the list based on the external variable ASort.

  @param   Item1 as a Pointer
  @param   Item2 as a Pointer
  @return  an Integer

**)
Function AggretateSort(Item1, Item2: Pointer): Integer;

Var
  A1, A2 : TAggregateRecord;
  R : Extended;

Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('AggretateSort');
  Try
  {$ENDIF}
  If Not boolBackward Then
    Begin
      A1 := TAggregateRecord(Item2);
      A2 := TAggregateRecord(Item1);
    End Else
    Begin
      A1 := TAggregateRecord(Item1);
      A2 := TAggregateRecord(Item2);
    End;
  Case ASort Of
    asTTT   : R := A2.TotalTime - A1.TotalTime;
    asIPTT  : R := A2.InProcessTime - A1.InProcessTime;
    asCC    : R := A2.CallCount - A1.CallCount;
    asATTT  : R := A2.AverageTotalTime - A1.AverageTotalTime;
    asAIPTT : R := A2.AverageInProcessTime - A1.AverageInProcessTime;
  Else
    R := AnsiCompareText(A1.Method, A2.Method);
  End;
  If R = 0 Then
    Result := 0
  Else If R > 0 Then
    Result := 1
  Else
    Result := -1;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
End;

{ TAggregateRecord }

(**

  This is a constructor for the TAggregateClass class.

  @precon  None.
  @postcon Creates an instance of an aggregate record with zero times.

  @param   strMethodName as a String

**)
Constructor TAggregateRecord.Create(strMethodName: String);

Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TAggregateRecord.Create');
  Try
  {$ENDIF}
  FMethodName := strMethodName;
  FTotalTime := 0;
  FInProcessTime := 0;
  FCallCount := 0;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
End;

(**

  This is a getter method for the AvaerageInProcessTime property.

  @precon  None.
  @postcon Returns the InProcess Time divided by the Call Count.

  @return  an Extended

**)
function TAggregateRecord.GetAverageInProcessTime: Extended;
begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TAggregateRecord.GetAverageInProcessTime');
  Try
  {$ENDIF}
  Result := FInProcessTime / Int(FCallCount);
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This is a getter method for the AvergaeTotalTime property.

  @precon  None.
  @postcon Returns the Total Time divided by the Call Count.

  @return  an Extended

**)
function TAggregateRecord.GetAverageTotalTime: Extended;
begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TAggregateRecord.GetAverageTotalTime');
  Try
  {$ENDIF}
  Result := FTotalTime / Int(FCallCount);
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

{ TAggregateList }

(**

  This method adds the method information to either an already created record
  with the same name or to an new record.

  @precon  None.
  @postcon Adds the method information to either an already created record
           with the same name or to an new record.

  @param   strMethodName as a String
  @param   iTTT          as an Extended
  @param   iIPTT         as an Extended
  @param   iCC           as an Extended

**)
procedure TAggregateList.Add(strMethodName: String; iTTT, iIPTT, iCC : Extended);

Var
  iIndex: Integer;

Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TAggregateList.Add');
  Try
  {$ENDIF}
  iIndex := Find(strMethodName);
  If iIndex < 0 Then
    FAggregateList.Insert(Abs(iIndex) - 1,
      TAggregateRecord.Create(strMethodName));
  TotalTime := TotalTime + iTTT;
  Item[Abs(iIndex)].TotalTime := Item[Abs(iIndex)].TotalTime + iTTT;
  Item[Abs(iIndex)].InProcessTime := Item[Abs(iIndex)].InProcessTime + iIPTT;
  Item[Abs(iIndex)].CallCount := Item[Abs(iIndex)].CallCount + iCC;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
End;

(**

  This method clears the aggregate list of all current profile information.

  @precon  None.
  @postcon Clears the aggregate list of all current profile information.

**)
procedure TAggregateList.Clear;
begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TAggregateList.Clear');
  Try
  {$ENDIF}
  FAggregateList.Clear;
  FTotalTime := 0;
  FLastSort  := asUnknown;
  FBackward  := False;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This is a constructor for the TAggregateList class.

  @precon  None.
  @postcon Creates an empty list.

**)
Constructor TAggregateList.Create;

Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TAggregateList.Create');
  Try
  {$ENDIF}
  FAggregateList := TObjectList.Create(True);
  FTotalTime := 0;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
End;

(**

  This is a destructor for the TAggregateList class.

  @precon  None.
  @postcon Frees the lists memory.

**)
Destructor TAggregateList.Destroy;

Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TAggregateList.Destroy');
  Try
  {$ENDIF}
  FAggregateList.Free;
  Inherited Destroy;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
End;

(**

  This method finds the index of the passed method if its in the list else
  returns a negative number which is the position this record should be inserted
  into the list.

  @precon  None.
  @postcon Finds the index of the passed method if its in the list else
           returns a negative number which is the position this record should be
           inserted into the list.

  @param   strMethodName as a String
  @return  an Integer

**)
Function TAggregateList.Find(strMethodName: String): Integer;

Var
  iFirst, iMid, iLast : Integer;

Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TAggregateList.Find');
  Try
  {$ENDIF}
  iFirst := 1;
  iLast := FAggregateList.Count;
  While iLast >= iFirst Do
    Begin
      iMid := (iFirst + iLast) Div 2;
      If AnsiCompareText(Item[iMid].Method, strMethodName) = 0 Then
        Begin
          Result := iMid;
          Exit;
        End
      Else If AnsiCompareText(Item[iMid].Method, strMethodName) < 0 Then
        iFirst := iMid + 1
      Else
        iLast := iMid - 1;
    End;
  Result := -iFirst;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
End;

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon Returns the number of items in the list.

  @return  an Integer

**)
function TAggregateList.GetCount: Integer;
begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TAggregateList.GetCount');
  Try
  {$ENDIF}
  Result := FAggregateList.Count;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This is a getter method for the Item property.

  @precon  iIndex must be a valid index between 1 and Count.
  @postcon Returns an instance of the indexed aggregate Record.
  @param   iIndex as an Integer
  @return  a TAggregateRecord

**)
function TAggregateList.GetItem(iIndex: Integer): TAggregateRecord;
begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TAggregateList.GetItem');
  Try
  {$ENDIF}
  Result := FAggregateList[iIndex - 1] As TAggregateRecord;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This method sorts the aggregate list by the sort parameter.

  @precon  None.
  @postcon Sorts the aggregate list.

  @param   AggregateSort as a TAggregateSort

**)
procedure TAggregateList.Sort(AggregateSort: TAggregateSort);
begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TAggregateList.Sort');
  Try
  {$ENDIF}
  ASort := AggregateSort;
  If FLastSort = AggregateSort Then
    FBackward := Not FBackward;
  boolBackward := FBackward;
  FAggregateList.Sort(AggretateSort);
  FLastSort := AggregateSort;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

End.
