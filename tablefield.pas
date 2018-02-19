unit TableField;

interface

uses
    ByteFileUtils, DbRecord, SysUtils;

type
  PLongWord = ^LongWord;
  PString = ^String;

  EFieldType = (tint, tstring, tbool);
  PRecordString = ^TRecordString;
  TRecordString = String[31];
  TFieldName = string[10];
  TFieldType = string[10];

  PField = ^TField;
  TField = record
    name: TFieldName;
    length: byte;
    fieldType: EFieldType;
    next: PField;
  end;

const
   SUPPORTED_TYPES_COUNT = 3;
   SUPPORTED_TYPES: array [1 .. SUPPORTED_TYPES_COUNT] of TFieldName = ('int', 'string', 'bool');


function CreateField(name: TFieldName; fieldType: TFieldType): PField;
function IsSupportedTypeName(name: TFieldName): boolean;
function GetFieldTypeFromName(name: TFieldName): EFieldType;
function GetFieldLength(name: TFieldName): byte;
procedure LoadBool(var dbRecord: TDbRecord; fieldSize: byte);
procedure LoadInt(var dbRecord: TDbRecord; fieldSize: byte);
procedure LoadString(var dbRecord: TDbRecord; fieldSize: byte);
procedure WriteField(var f: TFile; item: PField);
function GetFieldValueAsString(field: PField; dbRecord: PDbRecord; address: LongWord): string;

implementation

function createField(name: TFieldName; fieldType: TFieldType): PField;
var newField: PField;
begin
    if not isSupportedTypeName(fieldType) then
    begin
      createField := nil;
      exit;
    end;

    new(newField);
    newField^.name := name;
    newField^.fieldType := getFieldTypeFromName(fieldType);
    newField^.length := getFieldLength(fieldType);
    newField^.next := nil;

    createField := newField;
end;

function IsSupportedTypeName(name: TFieldName): boolean;
var i: byte;
begin
     isSupportedTypeName := false;

     for i:=1 to SUPPORTED_TYPES_COUNT do
       if SUPPORTED_TYPES[i] = name then
         isSupportedTypeName := true;
end;

function GetFieldTypeFromName(name: TFieldName): EFieldType;
var i: byte;
begin
     case name of
       'int': getFieldTypeFromName := EFieldType.tint;
       'string': getFieldTypeFromName := EFieldType.tstring;
       'bool': getFieldTypeFromName := EFieldType.tbool;
     end;
end;

function GetFieldLength(name: TFieldName): byte;
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

    PLongWord(@(dbRecord.data[dataLength]))^ := val;
end;

procedure LoadString(var dbRecord: TDbRecord; fieldSize: byte);
var val: String[31];
    dataLength, i: LongWord;
begin
    ReadLn(val);

    dataLength := Length(dbRecord.data);
    SetLength(dbRecord.data, dataLength + fieldSize);

    for i:=0 to fieldSize-1 do dbRecord.data[i+dataLength] := Byte(val[i]);
end;

procedure WriteField(var f: TFile; item: PField);
begin
     BlockWrite(f, item^.name, SizeOf(TFieldName));
     Write(f, byte(item^.fieldType));
     Write(f, item^.length);
end;

function GetFieldValueAsString(field: PField; dbRecord: PDbRecord; address: LongWord): string;
begin
    case field^.fieldType of
      EFieldType.tint: GetFieldValueAsString := IntToStr(PLongWord(@(dbRecord^.data[address]))^);
      EFieldType.tstring: GetFieldValueAsString := PRecordString(@(dbRecord^.data[address]))^;
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
