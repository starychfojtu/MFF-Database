unit DBMS;

interface

uses BinaryDBMS, OverflowFile, TableField, TableMetadata, ByteFileUtils, DbRecord;

const
  TABLE_DATA_FILE = 'table.bin';

{ Inserts a given record into the database, returns false if record with given ID exists }
function Insert(dbRecord: TDbRecord): boolean;

{ Searches for record with given id }
function SearchById(id: LongWord): PDbRecord;

{ Prints all table records to stdout}
procedure SelectAll();

{ Prints given record to stdout }
procedure PrintRecord(dbRecord: PDbRecord; metadata: PTableMetadata);

{ Marks record with given id as deleted, returns false when no record found }
function Delete(id: LongWord): boolean;

implementation

function Insert(dbRecord: TDbRecord): boolean;
begin
     if (SearchById(dbRecord.id) <> nil) then begin
        Insert := false;
        exit;
     end;
     if (IsOverflowFileFull()) then Reorganize();
     InsertIntoOverflowFile(dbRecord);
     Insert := true;
end;

function SearchById(id: LongWord): PDbRecord;
var blockNumber, i: Longint;
    block: PBlock;
    dbRecord: PDbRecord;
    f: file of byte;
begin
     block := GetOverflowBlock();
     dbRecord := GetDbRecordWithId(block, id);

     if (dbRecord <> nil) then begin
        SearchById := dbRecord;
        exit;
     end;

     Assign(f, TABLE_DATA_FILE);
     Reset(f);

     blockNumber := GetBlockNumberByRecordId(id);
     block := GetBlock(f, blockNumber);

     SearchById := GetDbRecordWithId(block, id);

     Close(f);
end;

procedure SelectAll();
var f: file of byte;
    metadata: PTableMetadata;
    dbRecord: TDbRecord;
    blockNumber, recordCount, recordSize, blockCount, address, maxAddress, blockFactor: LongWord;
    block: PBlock;
begin
  Assign(f, TABLE_DATA_FILE);
  Reset(f);

  blockCount := GetBlockCount(f);
  recordCount := GetRecordsCount(f);
  metadata := GetCurrentTableMetadata();
  recordSize := GetRecordSize(metadata);
  blockFactor := BLOCK_SIZE div recordSize;
  address := 0;

  if (blockCount = 0) then begin
     WriteLn('No records found');
     Close(f);
     exit;
  end;

  for blockNumber:=0 to blockCount-1 do begin
      block := GetBlock(f, blockNumber);
      maxAddress := BLOCK_SIZE;
      if (blockNumber = blockCount-1) and ((FileSize(f) mod BLOCK_SIZE) <> 0 ) then
         maxAddress := FileSize(f) mod BLOCK_SIZE;
      while address < maxAddress do begin
          PrintRecord(CreateFromDbData(block, address, recordSize), metadata);
          address := address + recordSize;
      end;
  end;

  Close(f);
end;

procedure PrintRecord(dbRecord: PDbRecord; metadata: PTableMetadata);
var address: LongWord;
    field : PField;
begin
  if (dbRecord = nil) then begin
     WriteLn('Record with id not found.');
     exit;
  end;

  WriteLn('Found record with id ', dbRecord^.id);

  address := 0;
  field := metadata^.fields;
  while field <> nil do begin
        Writeln(field^.name, ': ', GetFieldValueAsString(field, dbRecord, address));
        address := address + field^.length;
        field := field^.next;
  end;
end;

function Delete(id: LongWord): boolean;
var overflowDelete: boolean;
begin
     {oveflowDelete := DeleteDbRecordInOverflowWithId(id);
     Delete := oveflowDelete;

     if (oveflowDelete) then exit;

     Delete := DeleteDbRecordInOverflowWithId(id);}
end;

end.
