unit OverflowFile;

interface

uses
    ByteFileUtils, DbRecord, TableMetadata;

{ Returns true when overflow area is full }
function IsOverflowFileFull(): boolean;

{ Inserts given record in the overflow area }
procedure InsertIntoOverflowFile(dbRecord: TDbRecord);

{ Returns the overflow area }
function GetOverflowBlock(): PBlock;

{ Returns count of all records in overflow area }
function GetOverflowRecordCount(): LongWord;

{ Saves given block as overflow block }
procedure SaveOverflowBlock(block: PBlock);

const
  OVERFLOW_FILE = 'table.overflow';

implementation

function IsOverflowFileFull(): boolean;
var f: file of byte;
begin
     Assign(f, OVERFLOW_FILE);
     Reset(f);

     IsOverflowFileFull := FileSize(f) + GetRecordSize(GetCurrentTableMetadata()) >= BLOCK_SIZE;

     Close(f);
end;

procedure InsertIntoOverflowFile(dbRecord: TDbRecord);
var f: file of byte;
begin
     Assign(f, OVERFLOW_FILE);
     Reset(f);

     Seek(f, FileSize(f));
     WriteRecord(f, dbRecord);

     Close(f);
end;

function GetOverflowBlock(): PBlock;
var f: file of byte;
begin
     Assign(f, OVERFLOW_FILE);
     Reset(f);

     GetOverflowBlock := GetBlock(f, 0);

     Close(f);
end;

function GetOverflowRecordCount(): LongWord;
var f: file of byte;
    recordSize: LongWord;
begin
     Assign(f, OVERFLOW_FILE);
     Reset(f);

     recordSize := GetRecordSize(GetCurrentTableMetadata());
     GetOverflowRecordCount := FileSize(f) div recordSize;

     Close(f);
end;

procedure SaveOverflowBlock(block: PBlock);
var f: file of byte;
begin
     Assign(f, OVERFLOW_FILE);
     Rewrite(f);

     BlockWrite(f, block^, FileSize(f));

     Close(f);
end;

end.

