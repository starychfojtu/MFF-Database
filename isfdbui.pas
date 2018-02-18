unit IsfDbUI;

interface

uses TableMetadata, TableField, DBMS, DbRecord, BinaryDBMS;

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

procedure Help();
begin
  writeln('exit - exits the dbms');
  writeln('search - search for a row with given id');
  writeln('insert row - will show you current columns and ask you for values');
  writeln('create table - will ask you for values to create a new table, destroys the previous');
  writeln('schema - shows current table metadata');
  writeln('select all - print all rows');
  writeln('build index - builds new index file');
  writeln('reorganize - forces the database reorganization');
  writeln('delete - deletes record with given id');
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
  writeln('isId(true/false)');
  writeln('autoincrement(true/false)');
  write('Supported types are: ');
  for i:=1 to SUPPORTED_TYPES_COUNT do write(SUPPORTED_TYPES[i], ' ');
  writeln();
  writeln('End by typing: create');

  while true do begin
        {Load field name}
        readln(fieldName);

        {Break loop}
        if (fieldName = 'create') then break;

        {Load field type}
        readln(fieldType);
        if (not isSupportedTypeName(fieldType)) then begin
           writeln('Unsupported type: ', fieldType);
           continue;
        end;

        {Load Id}
        readln(input);
        if (input = 'true') then id := true else id := false;
        writeln(id);

        {Load Autoincrement}
        readln(input);
        if (input = 'true') then autoincrement := true else autoincrement := false;
        writeln(autoincrement);

        {Add field}
        AddField(table, fieldName, fieldType, id, autoincrement);
  end;

  SaveTable(table);
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
           writeln('ID: ', field^.id);
           writeln('AutoIncrement: ', field^.autoincrement);
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
             COMMAND_SHOW_SCHEMA: ShowSchema();
             COMMAND_SELECT_ALL: ShowAll();
             COMMAND_INSERT_ROW: InsertDbRecord();
             COMMAND_SEARCH: Search();
             COMMAND_BUILD_INDEX: begin
                   BuildIndex();
                   WriteLn('Index builded.');
             end;
             COMMAND_REORGANIZE: begin
                   Reorganize();
                   WriteLn('Database reorganized.');
             end;
             COMMAND_DELETE: DeletebyId();
             else writeln('Invalid command ', command, ', see help for possible commands.');
        end;
        readln(command);
  end;

  writeln('Bye.');
end;

end.

