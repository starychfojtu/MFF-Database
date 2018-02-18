unit TableField;

interface

uses
    ByteFileUtils, DbRecord, SysUtils;

type
  PLongWord = ^LongWord;
  PString = ^String;

  EFieldType = (tint, tstring, tbool);
  TFieldName = string[10];
  TFieldType = string[10];

  PField = ^TField;
  TField = record
    name: TFieldName;
    length: byte;
    fieldType: EFieldType;
    id: boolean; // TODO : Delete
    autoincrement: boolean; // TODO : Delete
    next: PField;
  end;

  // TODO: Delete
  PRecordData = ^TRecordData;
  TRecordData = array[0..4096] of byte;

const
   SUPPORTED_TYPES_COUNT = 3;
   SUPPORTED_TYPES: array [1 .. SUPPORTED_TYPES_COUNT] of TFieldName = ('int', 'string', 'bool');


function createField(name: TFieldName; fieldType: TFieldType; id, autoincrement: boolean): PField;
function isSupportedTypeName(name: TFieldName): boolean;
function getFieldTypeFromName(name: TFieldName): EFieldType;
function getFieldLength(name: TFieldName): byte;
procedure LoadBool(var dbRecord: TDbRecord; fieldSize: byte);
procedure LoadInt(var dbRecord: TDbRecord; fieldSize: byte);
procedure LoadString(var dbRecord: TDbRecord; fieldSize: byte);
procedure WriteField(var f: TFile; item: PField);
function GetFieldValueAsString(field: PField; dbRecord: PDbRecord; address: LongWord): string;

implementation

function createField(name: TFieldName; fieldType: TFieldType; id, autoincrement: boolean): PField;
var newField: PField;
begin
    if not isSupportedTypeName(fieldType) then
    begin
      createField := nil;
      exit;
    end;

    new(newField);
    newField^.name := name;
    newField^.id := id;
    newField^.autoincrement := autoincrement;
    newField^.fieldType := getFieldTypeFromName(fieldType);
    newField^.length := getFieldLength(fieldType);
    newField^.next := nil;

    createField := newField;
end;

function isSupportedTypeName(name: TFieldName): boolean;
var i: byte;
begin
     isSupportedTypeName := false;

     for i:=1 to SUPPORTED_TYPES_COUNT do
       if SUPPORTED_TYPES[i] = name then
         isSupportedTypeName := true;
end;

function getFieldTypeFromName(name: TFieldName): EFieldType;
var i: byte;
begin
     case name of
       'int': getFieldTypeFromName := EFieldType.tint;
       'string': getFieldTypeFromName := EFieldType.tstring;
       'bool': getFieldTypeFromName := EFieldType.tbool;
     end;
end;

function getFieldLength(name: TFieldName): byte;
begin
     case name of
       'int': getFieldLength := 4;
       'string' : getFieldLength := 32;
       'bool' : getFieldLength := 1;
     end;
end;

procedure LoadBool(var dbRecord: TDbRecord; fieldSize: byte);
var val: string;
    dataLength : LongWord;
begin
    ReadLn(val);
    while (val <> 'true') and (val <> 'false') do begin
          writeln('Incorrect - try it again');
          ReadLn(val);
    end;

    dataLength := Length(dbRecord.Data);
    SetLength(dbRecord.data, dataLength + fieldSize);

    if (val = 'true') then
         dbRecord.data[dataLength] := 1
    else if (val = 'false') then
         dbRecord.data[dataLength] := 0;
end;

procedure LoadInt(var dbRecord: TDbRecord; fieldSize: byte);
var val, dataLength: LongWord;
begin
    ReadLn(val);

    dataLength := Length(dbRecord.data);
    SetLength(dbRecord.data, dataLength + fieldSize);

    PLongWord(@dbRecord.data[dataLength])^ := val;
end;

procedure LoadString(var dbRecord: TDbRecord; fieldSize: byte);
var val: String;
    dataLength: LongWord;
begin
    ReadLn(val);

    dataLength := Length(dbRecord.data);
    SetLength(dbRecord.data, dataLength + fieldSize);

    PString(@dbRecord.data[dataLength])^ := val;
end;

procedure WriteField(var f: TFile; item: PField);
begin
     writeString(f, item^.name, sizeof(TFieldName));
     write(f, byte(item^.fieldType));
     write(f, item^.length);
     write(f, byte(item^.id));
     write(f, byte(item^.autoincrement));

     writeln(
             'DEBUG | Writing ',
             item^.name, ' ',
             byte(item^.fieldType), ' ',
             item^.length, ' ',
             byte(item^.id), ' ',
             byte(item^.autoincrement)
     );
end;

function GetFieldValueAsString(field: PField; dbRecord: PDbRecord; address: LongWord): string;
begin
    case field^.fieldType of
      EFieldType.tint: GetFieldValueAsString := IntToStr(PLongWord(@(dbRecord^.data[address]))^);
      EFieldType.tstring: GetFieldValueAsString := PString(@(dbRecord^.data[address]))^;
      EFieldType.tbool: begin
            if dbRecord^.data[address] = 1 then
               GetFieldValueAsString := 'true'
            else
               GetFieldValueAsString := 'false';
      end;
    end;
end;

begin
end.
