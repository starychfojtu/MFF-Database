program isfdb;

const
  PRIMARY_FILE_NAME = 'test.isfdb';
  BLOCK_SIZE = 65535;

type
  TByteFile = file of byte;
  person = record
    id: byte;
    name: string;
    male: boolean;
  end;
  PLongInt = ^longint;
  PString = ^string;
  PBoolean = ^boolean;
  PChar = ^char;

var
  primaryFile: TByteFile;
  test: person;
  buffer: array[1..BLOCK_SIZE] of byte;
  bytesRead: longword;

procedure writeString(item: string);
var i: longint;
begin
     for i:=1 to length(item) do
         write(primaryFile, byte(item[i]));
end;

function insert(id: longint; name: string; male: boolean): boolean;
begin
  write(primaryFile, id);
  writeString(name);
  write(primaryFile, byte(male));

  insert := true;
end;

procedure selectAll();
var id: byte;
    name: PString;
    male: PBoolean;
    pointer, i, length : longint;
begin
  pointer := 1;
  new(name);
  new(male);
  while pointer < bytesRead do
  begin
       id := buffer[pointer];
       pointer := pointer + sizeof(id);
       write(id);

       length := 5;
       for i:= 0 to length - 1 do
           write((PChar(@buffer[pointer + i]))^);

       pointer := pointer + length;

       male := PBoolean(@buffer[pointer]);
       pointer := pointer + sizeof(male^);

       write(male^);
  end;
end;

begin
  assign(primaryFile, PRIMARY_FILE_NAME);
  rewrite(primaryFile);

  test.id := 1;
  test.name := 'Josef';
  test.male := true;

  insert(test.id, test.name, test.male);
  insert(2, 'Lucie', false);
  close(primaryFile);

  assign(primaryFile, PRIMARY_FILE_NAME);
  reset(primaryFile);
  blockread(primaryFile, buffer, BLOCK_SIZE, bytesRead);
  close(primaryFile);

  selectAll();
  readln();
end.

