IMPLEMENTATION MODULE ezScript;   (* MEW 881105 *)

FROM FileSystem IMPORT
    File, Response, Close, Lookup, ReadChar;


VAR
  ScriptFile : File;
  IsOpen : BOOLEAN;

(*$D-*)
PROCEDURE OpenScript(name : ARRAY OF CHAR) : BOOLEAN;
  BEGIN
    Lookup(ScriptFile,name,FALSE);
    IsOpen := (ScriptFile.res = done);
    RETURN IsOpen
  END OpenScript;
(*$D+*)

PROCEDURE CloseScript;
  BEGIN
    IF IsOpen THEN
      Close(ScriptFile);
      IsOpen := FALSE
    END
  END CloseScript;

PROCEDURE GetNextToken(VAR str : ARRAY OF CHAR);
  CONST quote = 42C;
  VAR
    i : CARDINAL;
    ch : CHAR;
    inquotes : BOOLEAN;
  BEGIN
    FOR i := 0 TO HIGH(str) DO  str[i] := 0C  END;
    i := 0;
    inquotes := FALSE;
    ReadChar(ScriptFile,ch);
    WHILE (ScriptFile.res = done) AND (ch <= " ") DO
      ReadChar(ScriptFile,ch)
    END;
    WHILE (ScriptFile.res = done) AND ((ch > " ") OR inquotes) DO
      IF (ch = quote) THEN
        inquotes := NOT inquotes
      ELSE
        IF (i < HIGH(str)) THEN  str[i] := ch  END;
        INC(i)
      END;
      ReadChar(ScriptFile,ch)
    END
  END GetNextToken;

BEGIN
  IsOpen := FALSE
END ezScript.
