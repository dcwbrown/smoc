MODULE Fonts;

TYPE
   Font* = POINTER TO FontDesc;

   FontDesc* = RECORD name*: ARRAY 64 OF CHAR END;

VAR
  Default*: Font;


PROCEDURE Load*(namesize: ARRAY OF CHAR): Font;
BEGIN RETURN Default END Load;


BEGIN NEW(Default);  Default.name := "default 10";
END Fonts.
