IMPLEMENTATION MODULE ZGlobal  (* 881031 *);

FROM Conversions IMPORT ConvNumberToString;
FROM Strings IMPORT DeleteSubString, CopyString, InsertSubString;


PROCEDURE NumToStr(VAR s : ARRAY OF CHAR; n : LONGCARD);
  VAR blanks:CARDINAL;
  BEGIN
    ConvNumberToString(s, n, FALSE, 10, HIGH(s), " ");
    blanks := 0;
    WHILE (s[blanks] = " ") DO  INC(blanks)  END;
    DeleteSubString(s, 0, blanks);
  END NumToStr;

PROCEDURE Ratio(range,a,b : CARDINAL) : CARDINAL;
  BEGIN
    IF (b # 0) AND (a < b) THEN
      RETURN CARDINAL(LONGCARD(range) * LONGCARD(a) DIV LONGCARD(b))
    ELSE
      RETURN range
    END
  END Ratio;

PROCEDURE ZErrDescription(err : ZERR; VAR str : ARRAY OF CHAR);
  BEGIN
    IF    (err = ZOK)        THEN CopyString(str,"OK")
    ELSIF (err = ZNoMem)     THEN CopyString(str,"Not Enough Memory")
    ELSIF (err = ZBadSource) THEN CopyString(str,"Bad Source Directory")
    ELSIF (err = ZBadDest)   THEN CopyString(str,"Bad Destination Directory")
    ELSIF (err = ZBadName)   THEN CopyString(str,"Bad Name")
    ELSIF (err = ZExists)    THEN CopyString(str,"Already Exists")
    ELSIF (err = ZAbort)     THEN CopyString(str,"INTERRUPTED")
    ELSIF (err = ZTooMany)   THEN CopyString(str,"Too Many Files")
    ELSIF (err = ZFileOnly)  THEN CopyString(str,"Only Files Allowed")
    ELSIF (err = ZDelProt)   THEN CopyString(str,"Protected From Deletion")
    ELSE
      NumToStr(str,LONGCARD(err));
      InsertSubString(str,"DOS error ",0)
    END
  END ZErrDescription;

BEGIN
  SourceLock := NIL; OrigLock := NIL
END ZGlobal.
