(**

  This module contains numerous library functions, procedures and classes that
  can be used within many applications.

  @Version 1.0
  @Author  David Hoyle
  @Date    29 Apr 2018

**)
Unit ProfileViewer.Functions;

Interface

Uses
  SysUtils,
  Classes,
  Windows,
  Forms,
  Graphics;

Type
  (** A custom exception for errors getting the building number. **)
  EBuildNumberException = Class(Exception);

  Function CharCount(Const cChar : Char; Const strText : String;
    Const boolIgnoreQuotes : Boolean = True) : Integer;
  Function GetField(Const strText : String; Const Ch : Char; Const iIndex : Integer;
    Const boolIgnoreQuotes : Boolean = True): String;
  Function PosOfNthChar(Const strText : String; Const Ch : Char; Const iIndex : Integer;
    Const boolIgnoreQuotes : Boolean = True): Integer;
  Function BuildRootKey : String;
  Function CalcColour(Const dblValue, dblLowCriteria, dblMiddleCriteria,
    dblUpperCriteria : Double; Const iLowColour, iMiddleColour, iHighColour : TColor) : TColor;
  Procedure CheckFormIsOnDesktop(Const AForm : TForm);

ResourceString
  (** A list of bug fix letters as a string array. **)
  strBugFix = ' abcdefghijklmnopqrstuvwxyz';

Implementation

Uses
  SHFolder;

(**

  This routine returns the number of occurrances of the char found in the string .

  @precon  None.
  @postcon Returns the number of occurrances of the char found in the string.

  @param   cChar            as a Char as a constant
  @param   strText          as a String as a constant
  @param   boolIgnoreQuotes as a Boolean as a constant
  @return  an Integer

**)
Function CharCount(Const cChar : Char; Const strText : String;
  Const boolIgnoreQuotes : Boolean = True) : Integer;

Var
  iCount : Integer;
  boolInQuotes : Boolean;

Begin
  Result := 0;
  boolInQuotes := False;
  For iCount := 1 to Length(strText) Do
    Begin
      If Not boolIgnoreQuotes Then
        If strText[iCount] = '"' Then
          boolInQuotes := Not boolInQuotes;
      If strText[iCount] = cChar Then
        If Not boolInQuotes Then
          Inc(Result);
    End;
End;


(**

  This routine returns the position of the Nth occurrance of the character in the text.

  @precon  None.
  @postcon Returns the position of the Nth occurrance of the character in the text.

  @param   strText          as a String as a constant
  @param   Ch               as a Char as a constant
  @param   iIndex           as an Integer as a constant
  @param   boolIgnoreQuotes as a Boolean as a constant
  @return  an Integer

**)
Function PosOfNthChar(Const strText : String; Const Ch : Char; Const iIndex : Integer;
  Const boolIgnoreQuotes : Boolean = True): Integer;

Var
  i : Integer;
  iCount : Integer;
  boolInQuotes : Boolean;

Begin
  Result := 0;
  iCount := 0;
  boolInQuotes := False;
  For i := 1 To Length(strText) Do
    Begin
      If Not boolIgnoreQuotes Then
        If strText[i] = '"' Then
          boolInQuotes := Not boolInQuotes;
      If strText[i] = Ch Then
        If Not boolInQuotes Then
          Inc(iCount);
      If iIndex = iCount Then
        Begin
          Result := i;
          Exit;
        End;
    End;
End;

(**

  This function returns the contents of the specified field in the delimited text.

  @precon  None.
  @postcon Returns the contents of the specified field in the delimited text.

  @param   strText          as a String as a constant
  @param   Ch               as a Char as a constant
  @param   iIndex           as an Integer as a constant
  @param   boolIgnoreQuotes as a Boolean as a constant
  @return  a String

**)
Function GetField(Const strText : String; Const Ch : Char; Const iIndex : Integer;
    Const boolIgnoreQuotes : Boolean = True): String;

Var
  iNumOfFields : Integer;
  iStart, iEnd : Integer;

Begin
  Result := '';
  iNumOfFields := CharCount(Ch, strText, boolIgnoreQuotes) + 1;
  If iIndex = 1 Then
    Begin
      If iNumOfFields > 1  Then
        Begin
          iEnd := PosOfNthChar(strText, Ch, 1, boolIgnoreQuotes);
          Result := Copy(strText, 1, iEnd - 1);
        End Else
          Result := strText;
    End
  Else If (iIndex > 1) And (iIndex < iNumOfFields) Then
    Begin
      iStart := PosOfNthChar(strText, Ch, iIndex - 1, boolIgnoreQuotes);
      iEnd := PosOfNthChar(strText, Ch, iIndex, boolIgnoreQuotes);
      Result := Copy(strText, iStart + 1, iEnd - iStart - 1);
    End
  Else If iIndex = iNumOfFields Then
    Begin
      iStart := PosOfNthChar(strText, Ch, iIndex - 1, boolIgnoreQuotes);
      Result := Copy(strText, iStart + 1, Length(strText) - iStart);
    End;
End;

(**

  This routine extract the build number from the EXE resources for display in the app title.

  @precon  None.
  @postcon Extract the build number from the EXE resources for display in the app title.

  @param   strFileName as a String as a constant
  @param   iMajor      as an Integer as a reference
  @param   iMinor      as an Integer as a reference
  @param   iBugfix     as an Integer as a reference
  @param   iBuild      as an Integer as a reference
  @return  a String

**)
Function GetBuildNumber(Const strFileName : String; Var iMajor, iMinor, iBugfix,
  iBuild : Integer) : String;

Const
  strBuild = '%d.%d.%d.%d';
  iShiftR16 = 16;
  iMask = $FFFF;

ResourceString
  strExeHasNoVerInfo = 'The executable "%s" does not contain any version information.';

Var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;

Begin
  VerInfoSize := GetFileVersionInfoSize(PChar(strFileName), Dummy);
  If VerInfoSize <> 0 Then
    Begin
      GetMem(VerInfo, VerInfoSize);
      GetFileVersionInfo(PChar(strFileName), 0, VerInfoSize, VerInfo);
      VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
      iMajor := VerValue^.dwFileVersionMS shr iShiftR16;
      iMinor := VerValue^.dwFileVersionMS and iMask;
      iBugfix := VerValue^.dwFileVersionLS shr iShiftR16;
      iBuild := VerValue^.dwFileVersionLS and iMask;
      Result := Format(strBuild, [iMajor, iMinor, iBugfix, iBuild]);
      FreeMem(VerInfo, VerInfoSize);
    End Else
      Raise EBuildNumberException.CreateFmt(
        strExeHasNoVerInfo, [strFileName]);
End;

(**

  This function returns the users logon name as a String.

  @precon  None.
  @postcon Returns the users logon name as a String.

  @return  a String

**)
Function UserName : String;

Var
  i : Cardinal;

Begin
  i := MAX_PATH;
  SetLength(Result, i);
  GetUserName(@Result[1], i);
  Win32Check(LongBool(i));
  SetLength(Result, i - 1);
End;

(**

  This function returns the users computer name as a String.

  @precon  None.
  @postcon Returns the users computer name as a String.

  @return  a String

**)
Function ComputerName : String;

Var
  i : Cardinal;

Begin
  i := MAX_PATH;
  SetLength(Result, i);
  GetComputerName(@Result[1], i);
  Win32Check(LongBool(i));
  SetLength(Result, i);
End;

(**

  This method builds the root key INI filename for the loading and saving of settings from the instance 
  handle for the module.

  @precon  slParams must be a valid instance of a TStringList class.
  @postcon Builds the root key INI filename for the loading and saving of settings from the instance 
           handle for the module.

  @return  a String

**)
Function BuildRootKey : String;

ResourceString
  strINIPattern = '%s Settings for %s on %s.INI';
  strSeasonsFall = '\Season''s Fall\';

var
  strModuleName : String;
  strINIFileName : String;
  strUserAppDataPath : String;
  strBuffer : String;
  iSize : Integer;                                                              

{$IFDEF D0007}
// Delphi 7s SHFolder.pas file is missing this constant.
Const
  SHGFP_TYPE_CURRENT = 0; { current value for user, verify it exists }
{$ENDIF}

begin
  SetLength(strBuffer, MAX_PATH);
  iSize := GetModuleFileName(hInstance, PChar(strBuffer), MAX_PATH);
  SetLength(strBuffer, iSize);
  strModuleName := strBuffer;
  strINIFileName := ChangeFileExt(ExtractFileName(strBuffer), '');
  While (Length(strIniFilename) > 0) And
    (CharInSet(strIniFileName[Length(strIniFilename)], ['0'..'9'])) Do
    strIniFileName := Copy(strIniFileName, 1, Length(strIniFileName) - 1);
  strINIFileName :=  Format(strINIPattern, [strIniFileName, UserName, ComputerName]);
  SetLength(strBuffer, MAX_PATH);
  SHGetFolderPath(0, CSIDL_APPDATA Or CSIDL_FLAG_CREATE, 0, SHGFP_TYPE_CURRENT,
    PChar(strBuffer));
  strBuffer := StrPas(PChar(strBuffer));
  strUserAppDataPath := strBuffer + strSeasonsFall;
  If Not DirectoryExists(strUserAppDataPath) Then
    ForceDirectories(strUserAppDataPath);
  Result := strUserAppDataPath + strINIFileName;
end;

(**

  This method interpolates a colour for the specified percentage position within the colour and position 
  information passed.

  @precon  None.
  @postcon Interpolates a colour for the specified percentage position within the colour and position 
           information passed..

  @param   dblValue          as a Double as a constant
  @param   dblLowCriteria    as a Double as a constant
  @param   dblMiddleCriteria as a Double as a constant
  @param   dblUpperCriteria  as a Double as a constant
  @param   iLowColour        as a TColor as a constant
  @param   iMiddleColour     as a TColor as a constant
  @param   iHighColour       as a TColor as a constant
  @return  a TColor

**)
Function CalcColour(Const dblValue, dblLowCriteria, dblMiddleCriteria, dblUpperCriteria : Double;
  Const iLowColour, iMiddleColour, iHighColour : TColor) : TColor;

  (**

    This function calculate the intepolation of a single colour value between 2 colours based on value 
    for those colour positions.

    @precon  None.
    @postcon Returns the colour which is an interpolation between the input colours.

    @param   iLow     as a TColor as a constant
    @param   iHigh    as a TColor as a constant
    @param   iMask    as a TColor as a constant
    @param   dblLow   as a Double as a constant
    @param   dblValue as a Double as a constant
    @param   dblHigh  as a Double as a constant
    @return  a TColor

  **)
  Function InterpolateColour(Const iLow, iHigh, iMask : TColor; Const dblLow,
    dblValue, dblHigh : Double) : TColor;

  Var
    iColourDiff : TColor;

  Begin
    iColourDiff := iHigh And iMask - iLow And iMask;
    Result := Round(iLow And iMask + iColourDiff * (dblValue - dblLow) /
      (dblHigh - dblLow)) And iMask;
  End;

  (**

    This function calculate the intepolation of a colour value between 2 colours based on value for those
    colour positions.

    @precon  None.
    @postcon Returns the colour which is an interpolation between the input colours.

    @param   iLow     as a TColor as a constant
    @param   iHigh    as a TColor as a constant
    @param   dblLow   as a Double as a constant
    @param   dblValue as a Double as a constant
    @param   dblHigh  as a Double as a constant
    @return  a TColor

  **)
  Function InterpolateColours(Const iLow, iHigh : TColor; Const dblLow,
    dblValue, dblHigh : Double) : TColor;

  Const
    iBlueMask = $FF0000;
    iGreenMask = $00FF00;
    iRedMask = $0000FF;

  Begin
    Result :=
      InterpolateColour(iLow, iHigh, iBlueMask, dblLow, dblValue, dblHigh) +
      InterpolateColour(iLow, iHigh, iGreenMask, dblLow, dblValue, dblHigh) +
      InterpolateColour(iLow, iHigh, iRedMask, dblLow, dblValue, dblHigh);
  End;

Begin
  If dblValue <= dblLowCriteria Then
    Result := iLowColour
  Else If dblValue <= dblMiddleCriteria Then
    Result := InterpolateColours(
      ColorToRGB(iLowColour),
      ColorToRGB(iMiddleColour),
      dblLowCriteria,
      dblValue,
      dblMiddleCriteria)
  Else If dblValue <= dblUpperCriteria then
    Result := InterpolateColours(
      ColorToRGB(iMiddleColour),
      ColorToRGB(iHighColour),
      dblMiddleCriteria,
      dblValue,
      dblUpperCriteria)
  Else
    Result := iHighColour;
End;

(**

  This method makes sure the given form is visible on the Desktop WorkArea.

  @precon  AForm must be a valid instance.
  @postcon If the form is not fully visible on the desktop it is moved onto the desktop.

  @param   AForm as a TForm as a constant

**)
Procedure CheckFormIsOnDesktop(Const AForm : TForm);

Var
  R : TRect;

Begin
  R := Screen.WorkAreaRect;
  // Check Left and then Width
  If AForm.Left < R.Left Then
    AForm.Left := R.Left;
  If AForm.Width > R.Width Then
    AForm.Width := R.Width;
  // Check Top and then Height
  If AForm.Top < R.Top Then
    AForm.Top := R.Top;
  If AForm.Height > R.Height Then
    AForm.Height := R.Height;
  // Check Right
  If AForm.Left + AForm.Width > R.Left + R.Width Then
    AForm.Left := R.Left + R.Width - AForm.Width;
  // Check Top
  If AForm.Top + AForm.Height > R.Top + R.Height Then
    AForm.Top := R.Top + R.Height - AForm.Height;
End;

End.


