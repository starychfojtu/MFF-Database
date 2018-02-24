unit IsfDbUI;

interface

uses TableMetadata, TableField, DBMS, DbRecord, BinaryDBMS, OverflowFile;

{ Loads the user interface }
procedure LoadUI();

implementation

const
     DB_VERSION = 1;
     COMMAND_HELP = 'help';
     COMMAND_EXIT = 'exit';
     COMMAND_CREATE_TABLE = 'create table';
     COMMAND_SHOW_SCHEMA = 'schema';
     COMMAND_SELECT_ALL = 'select all';
     COMMAND_SEARCH = 'search';
     COMMAND_INSERT_ROW = 'insert row';
     COMMAND_BUILD_INDEX = 'build index';
     COMMAND_REORGANIZE = 'reorganize';
     COMMAND_DELETE = 'delete';
     COMMAND_COUNT = 'count';

procedure Help();
begin
  writeln('help - shows help');
  writeln('exit - exits the dbms');
  writeln('create table - will ask you for values to create a new table, destroys the previous');
  writeln('schema - shows current table metadata');
  writeln('insert row - will show you current columns and ask you for values');
  writeln('search - search for a row with given id');
  writeln('select all - print all rows');
  writeln('delete - deletes record with given id');
  writeln('count - returns records count');
  writeln('build index - builds new index file (not recommended to use)');
  writeln('reorganize - forces the database reorganization (not recommended to use)');
end;

function EnsureSchemaCreated(): boolean;
var f: file of byte;
begin
     Assign(f, TABLE_METADATA_FILE);
     Reset(f);

     if (FileSize(f) = 0) then begin
        Writeln('No table has been created - type: create table');
        EnsureSchemaCreated := false;
        Close(f);
        exit;
     end;

     EnsureSchemaCreated := true;
     Close(f);
end;

procedure CreateTable();
var name: TTableName;
    table: PTableMetadata;
    i: byte;
    fieldName: TFieldName;
    fieldType: TFieldType;
    input: string[5];
    id, autoincrement: boolean;
begin
  writeln('Type the name of the table (max 20 chars):');
  readln(name);
  table := CreateNewTable(name);

  writeln('Now add field by this pattern per line:');
  writeln('name(string)');
  writeln('type(enum)');
  write('Supported types are: ');
  for i:=1 to SUPPORTED_TYPES_COUNT do write(SUPPORTED_TYPES[i], ' ');
  writeln();
  writeln('End by typing: create instead of Name');
  writeln('Note that ID (int) is already added automatically.');

  while true do begin
        {Load field name}
        Write('Name: ');
        ReadLn(fieldName);

        {Break loop}
        if (fieldName = 'create') then begin
           if (table^.fields <> nil) then break
           else begin
              WriteLn('You must provide at least one field');
              continue;
           end;
        end;

        {Load field type}
        Write('Type: ');
        ReadLn(fieldType);
        if (not isSupportedTypeName(fieldType)) then begin
           Writeln('Unsupported type: ', fieldType);
           continue;
        end;

        {Add field}
        AddField(table, fieldName, fieldType);
  end;

  SaveTable(table);
  ResetDb();
  writeln('Table ', table^.name, ' successfully created. (Metedata size: ', sizeof(table),')');
end;

procedure ShowSchema();
var tableMetadata: PTableMetadata;
    field: PField;
    i: byte;
begin
     tableMetadata := getCurrentTableMetadata();
     writeln('Table Name: ', tableMetadata^.name);

     field := tableMetadata^.fields;
     while field <> nil do begin
           writeln('-------------------');
           writeln('Name: ', field^.name);
           writeln('Type: ', SUPPORTED_TYPES[byte(field^.fieldType) + 1]);
           writeln('Length: ', field^.length);
           field := field^.next;
     end;
end;

procedure Search();
var id: LongWord;
    dbRecord: PdBRecord;
begin
     Write('ID: ');
     ReadLn(id);

     dbRecord := SearchById(id);
     PrintRecord(dbRecord, GetCurrentTableMetadata());
end;

procedure ShowAll();
begin
     SelectAll();
end;

procedure InsertDbRecord();
var tableMetadata: PTableMetadata;
    currentField: PField;
    dbRecord: TDbRecord;
    addrIndex,i: LongWord;
    result: boolean;
begin
  WriteLn('Creating new record:');
  tableMetadata := GetCurrentTableMetadata();
  currentField := tableMetadata^.fields;

  write('Type value for ID (int) ');
  Readln(dbRecord.id);
  dbRecord.isDeleted := false;
  SetLength(dbRecord.data, 0);

  while currentField <> nil do begin
        write('Type value for ', currentField^.name, ' (',currentField^.fieldType, '): ');
        case currentField^.fieldType of
             EFieldType.tbool: LoadBool(dbRecord, currentField^.length);
             EFieldType.tint: LoadInt(dbRecord, currentField^.length);
             EFieldType.tstring: LoadString(dbRecord, currentField^.length);
        end;
        currentField := currentField^.next;
  end;

  result := insert(dbRecord);

  if result then
     Writeln('Record with id ',dbRecord.id, 'successfuly inserted.')
  else
     Writeln('Record with id ',dbRecord.id, ' already exists.')
end;

procedure DeletebyId();
var id: LongWord;
begin
     Write('ID: ');
     ReadLn(id);
     if Delete(id) then WriteLn('Record deleted.') else Write('Record not found');
end;

procedure LoadUI();
var command: string[20];
begin
  writeln('/*******************************************/');
  writeln('    Starting ISFDB - version ', DB_VERSION);
  writeln('    By Josef Starychfojtu, MFF UK project');
  writeln('    Type help for possible commands');
  writeln();
  writeln('/*******************************************/');

  Help();
  writeln('/*******************************************/');

  readln(command);
  while (command <> COMMAND_EXIT)do begin
        case command of
             COMMAND_HELP: Help();
             COMMAND_CREATE_TABLE: CreateTable();
             COMMAND_SHOW_SCHEMA: begin
                   if EnsureSchemaCreated() then ShowSchema();
             end;
             COMMAND_SELECT_ALL: begin
                   if EnsureSchemaCreated() then ShowAll();
             end;
             COMMAND_INSERT_ROW: begin
                   if EnsureSchemaCreated() then InsertDbRecord();
             end;
             COMMAND_SEARCH: begin
                   if EnsureSchemaCreated() then Search();
             end;
             COMMAND_BUILD_INDEX: begin
                   if EnsureSchemaCreated() then begin
                     BuildIndex();
                     WriteLn('Index builded.');
                   end;
             end;
             COMMAND_REORGANIZE: begin
                   if EnsureSchemaCreated() then begin
                     Reorganize();
                     WriteLn('Database reorganized.');
                   end;
             end;
             COMMAND_DELETE: begin
                   if EnsureSchemaCreated() then DeletebyId();
             end;
             COMMAND_COUNT: begin
                   if EnsureSchemaCreated() then
                      WriteLn('Total count: ', GetOverflowRecordCount() + GetRecordCount());
             end
             else writeln('Invalid command ', command, ', see help for possible commands.');
        end;
        WriteLn();
        ReadLn(command);
        WriteLn();
  end;

  writeln('Bye.');
end;

end.

