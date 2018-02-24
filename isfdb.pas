program ISFDB;

uses IsfDbUI, sysutils;

{Ensure schema is created before any operation!}

procedure InitializeFile(name: string);
begin
  if not FileExists(name) then FileClose(FileCreate(name));
end;

procedure Initialize();
begin
  InitializeFile('table.bin');
  InitializeFile('table.md');
  InitializeFile('table.overflow');
  InitializeFile('table.index');
  InitializeFile('table.temp');
end;

begin
  Initialize;
  LoadUI;
end.

