unit TableMetadata;

interface

uses ByteFileUtils, TableField;

const
  TABLE_METADATA_FILE = 'table.md';

type
  { Table metadata }
  PTableName = ^TTableName;
  TTableName = string[20];
  PTableMetadata = ^TTableMetadata;
  TTableMetadata = record
    name: TTableName;
    fields: PField;
  end;

  { TField representation in metadata file }
  PMetadataField = ^TMetadataField;
  TMetadataField = record
    name: TFieldName;
    fieldType: byte;
    length: byte;
  end;

  PMetadataFieldArray = ^TMetadataFieldArray;
  TMetadataFieldArray = array of TMetadataField;

{ Creates new table metadata }
function CreateNewTable(name: TTableName): PTableMetadata;

{ Adds a field to given table metadata }
procedure AddField(var table: PTableMetadata; name: TFieldName; fieldType: TFieldType);

{ Completely resets database and creates a new one by given metadata }
procedure SaveTable(var table: PTableMetadata);

{ Returns current table metadata }
function GetCurrentTableMetadata(): PTableMetadata;

{ Returns current record size }
function GetRecordSize(tableMetadata: PTableMetadata): LongWord;

implementation

function CreateNewTable(name: TTableName): PTableMetadata;
var newTable: PTableMetadata;
begin
  new(newTable);
  newTable^.name := name;
  newTable^.fields := nil;

  createNewTable := newTable;
end;

procedure AddField(var table: PTableMetadata; name: TFieldName; fieldType: TFieldType);
var newField, lastField: PField;
begin
  newField := CreateField(name, fieldType);
  lastField := table^.fields;

  if lastField = nil then
  begin
    table^.fields := newField;
    exit;
  end;

  while (lastField^.next <> nil) do
  begin
    lastField := lastField^.next;
  end;

  lastField^.next := newField;
end;

procedure SaveTable(var table: PTableMetadata);
var metadataFile: file of byte;
  field: PField;
begin
  Assign(metadataFile, TABLE_METADATA_FILE);
  Rewrite(metadataFile);
  BlockWrite(metadataFile, table^.name, SizeOf(TTableName));

  field := table^.fields;
  while(field <> nil) do
  begin
       WriteField(metadataFile, field);
       field := field^.next;
  end;

  close(metadataFile);
end;

function GetField(metadataField: PMetadataField): PField;
var field: PField;
begin
     new(field);
     field^.name := metadataField^.name;
     field^.length := metadataField^.length;
     field^.fieldType := EFieldType(metadataField^.fieldType);
     field^.next := nil;

     getField := field;
end;

function GetCurrentTableMetadata(): PTableMetadata;
var metadataFile: file of byte;
    buffer: array[0..4096] of byte;
    bytesRead, i: longword;
    tableMetadata: PTableMetadata;
    field: PField;
begin
  Assign(metadataFile, TABLE_METADATA_FILE);
  Reset(metadataFile);
  BlockRead(metadataFile, buffer, FileSize(metadataFile), bytesRead);
  Close(metadataFile);

  new(tableMetadata);
  tableMetadata^.name := (PTableName(@buffer[0]))^;

  field := nil;
  i:=sizeof(TTableName);
  while (i < bytesRead) do begin
       if (field = nil) then begin
          field := GetField(PMetadataField(@buffer[i]));
          tableMetadata^.fields := field;
       end else begin
           field^.next := GetField(PMetadataField(@buffer[i]));
           field := field^.next;
       end;
       i := i + sizeof(TMetadataField);
  end;

  getCurrentTableMetadata := tableMetadata;
end;

function GetRecordSize(tableMetadata: PTableMetadata): LongWord;
var field: PField;
    size: LongWord;
begin
     field := tableMetadata^.fields;
     while field <> nil do begin
          size := size + field^.length;
          field := field^.next;
     end;

     GetRecordSize := size + 4 + 1; { size + id + isDeleted }
end;

end.

