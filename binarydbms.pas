unit BinaryDBMS;

interface

uses
  SysUtils, ByteFileUtils, DbRecord, TableMetadata, OverflowFile;

{ Returns a block number which could contain a record with given id or -1 when NotFound }
function GetBlockNumberByRecordId(id: LongWord): Longint;

{ Returns pointer to record with given id in given block or nil if not found or the record is marked deleted }
function GetDbRecordWithId(block: PBlock; id: LongWord): PDbRecord;

{ Returns pointer to record with given id in given block or -1 id not found or the record is marked deleted }
function GetDbRecordAddressWithId(block: PBlock; id: LongWord): SmallInt;

{ Returns count of records in given file }
function GetRecordsCount(var f: TFile): LongWord;

{ Deletes the index file and builds a new }
procedure BuildIndex();

{ Reorganizes whole database to clear overflow area }
procedure Reorganize();

{ Creates record from data on given address in block }
function CreateFromDbData(block: PBlock; address: LongWord; size: LongWord): PDbRecord;

const
  INDEX_FILE = 'table.index';
  TEMP_FILE = 'table.temp';

type
  PLongWord = ^LongWord;

  PIndex = ^TIndex;
  TIndex = record
    id: LongWord;
    blockNumber: LongWord;
  end;

  { REORGANIZATION }

  { Helper type for sorting a block }
  TBlockSortItem = record
    id: LongWord;
    address: LongWord;
  end;
  PBlockSortItemArray = ^TBlockSortItemArray;
  TBlockSortItemArray = array of TBlockSortItem;

var
  indexes: array of TIndex; { In-memory index file }

implementation

function CreateFromDbData(block: PBlock; address: LongWord; size: LongWord): PDbRecord;
var dbRecord: PdbRecord;
    i: LongWord;
begin
     new(dbRecord);
     dbRecord^.isDeleted:=boolean(block^[address]);
     dbRecord^.id:=PLongWord(@(block^[address + SizeOf(dbRecord^.isDeleted)]))^;

     SetLength(dbRecord^.data, size - SizeOf(dbRecord^.isDeleted) - SizeOf(dbRecord^.id));
     for i:=0 to Length(dbRecord^.data)-1 do begin
         dbRecord^.data[i] := block^[address + SizeOf(dbRecord^.isDeleted) + SizeOf(dbRecord^.id) + i];
     end;

     CreateFromDbData := dbRecord;
end;

{ Load indexes into memory }
procedure LoadIndexes();
var f: file of LongWord;
    count, i: LongWord;
begin
     Assign(f, INDEX_FILE);
     Reset(f);

     count := FileSize(f) div 2;
     SetLength(indexes, count);

     if (count = 0) then begin
        Close(f);
        exit;
     end;

     for i:=0 to count-1 do begin
         Read(f, indexes[i].id);
         Read(f, indexes[i].blockNumber);
     end;

     Close(f);
end;

function GetBlockNumberByRecordId(id: LongWord): Longint;
var i: LongWord;
begin
     LoadIndexes();

     GetBlockNumberByRecordId := -1;
     i := 0;

     if (Length(indexes) = 0) then exit;

     while id >= indexes[i].id do begin
         GetBlockNumberByRecordId := indexes[i].blockNumber;
         if (i = Length(indexes) - 1) then break;
         Inc(i);
     end;
end;

function GetDbRecordWithId(block: PBlock; id: LongWord): PDbRecord;
var address, blockFactor, recordSize, recordId: LongWord;
    isDeleted: boolean;
begin
     recordSize := GetRecordSize(GetCurrentTableMetadata());
     blockFactor := BLOCK_SIZE div recordSize;
     address := 0;

     while address < BLOCK_SIZE do begin
         isDeleted := boolean(block^[address]);
         recordId := PLongWord(PByte(block) + 1 + address)^;

         if ((recordId = id) and (isDeleted = false)) then begin
            GetDbRecordWithId := CreateFromDbData(block, address, recordSize);
            exit;
         end;

         if (recordId > id) then begin
            GetDbRecordWithId := nil;
            exit;
         end;

         address := address + recordSize;
     end;

     getDbRecordWithId := nil;
end;

function GetDbRecordAddressWithId(block: PBlock; id: LongWord): SmallInt;
var address, blockFactor, recordSize, recordId: LongWord;
    isDeleted: boolean;
begin
     recordSize := GetRecordSize(GetCurrentTableMetadata());
     blockFactor := BLOCK_SIZE div recordSize;
     address := 0;

     while address < BLOCK_SIZE do begin
         isDeleted := boolean(block^[address]);
         recordId := PLongWord(PByte(block) + 1 + address)^;

         if ((recordId = id) and (isDeleted = false)) then begin
            GetDbRecordAddressWithId := Smallint(address);
            exit;
         end;

         if (recordId > id) then begin
            GetDbRecordAddressWithId := -1;
            exit;
         end;

         address := address + recordSize;
     end;

     GetDbRecordAddressWithId := -1;
end;

function GetRecordsCount(var f: TFile): LongWord;
var lastBlockSize, recordSize, blockFactor, blockCount: LongWord;
begin
     blockCount := GetBlockCount(f);

     if blockCount = 0 then begin
        GetRecordsCount := 0;
        exit;
     end;

     lastBlockSize := FileSize(f) mod BLOCK_SIZE;
     recordSize := GetRecordSize(GetCurrentTableMetadata());
     blockFactor := BLOCK_SIZE div recordSize;

     if (lastBlockSize = 0) then begin
        GetRecordsCount := blockCount * blockFactor;
        exit;
     end;

     GetRecordsCount := (blockCount-1)*blockFactor + lastBlockSize div recordSize;
end;

procedure BuildIndex();
var blockCount, i, id: LongWord;
    indexFile: file of LongWord;
    dataFile: file of byte;
    block: PBlock;
begin
     Assign(indexFile, INDEX_FILE);
     Rewrite(indexFile);

     Assign(dataFile, 'table.bin');
     Reset(dataFile);

     blockCount := GetBlockCount(dataFile);

     if (blockCount = 0) then exit;

     for i:=0 to blockCount-1 do begin
           block := GetBlock(dataFile, i);
           id := PLongWord(@(block^[1]))^;
           Write(indexFile, id);
           Write(indexFile, i);
     end;

     Close(indexFile);
     Close(dataFile);
end;

{ ================== }
{ | REORGANIZATION | }
{ ================== }

{ Merge two parts of the array }
procedure Merge(items: PBlockSortItemArray; startIndex: LongWord; endIndex: LongWord);
var half, index, index1, index2, i: LongWord;
    sorted: TBlockSortItemArray;
begin
     index := 0;
     index1 := startIndex;
     half := endIndex div 2;
     index2 := half + 1;
     SetLength(sorted, endIndex - startIndex + 1);

     while ((index1 <= half) or (index2 <= endIndex)) do begin
         if (items^[index1].id > items^[index2].id) then begin
            sorted[index] := items^[index2];
            inc(index2);
         end else begin
            sorted[index] := items^[index1];
            inc(index1);
         end;

         { All items of the first half were marged }
         if (index1 > half) then begin
            for index2:=index2 to endIndex do begin
                Inc(index);
                sorted[index] := items^[index2];
            end;
            break;
         end;

         { All items of the second half were marged }
         if (index2 > endIndex) then begin
            for index1:=index1 to half do begin
                Inc(index);
                sorted[index] := items^[index1];
            end;
            break;
         end;

         Inc(index);
     end;

     { Copy the sorted part to the original array }
     for i:=startIndex to endIndex do begin
         items^[i] := sorted[i - startIndex];
     end;
end;

{ Classic merge sort }
procedure MergeSort(items: PBlockSortItemArray; startIndex: LongWord; endIndex: LongWord);
begin
     if (startIndex = endIndex) then exit;

     MergeSort(items, startIndex, endIndex div 2);
     MergeSort(items, (endIndex div 2) + 1 , endIndex);

     Merge(items, startIndex, endIndex);
end;

{ Sorts given array of blockSortItems }
procedure SortBlockSortItemArray(items: PBlockSortItemArray);
begin
     MergeSort(items, 0, Length(items^)-1);
end;

{ Creates a sorted array of blockSortItems }
function SortBlock(block: PBlock; recordSize: LongWord; recordCount: LongWord): PBlockSortItemArray;
var sortItemArray: PBlockSortItemArray;
    i, blockFactor: LongWord;
begin
     blockFactor := BLOCK_SIZE div recordSize;
     new(sortItemArray);
     SetLength(sortItemArray^, recordCount);

     for i:=0 to recordCount-1 do begin
         sortItemArray^[i].id := block^[(i * recordSize) + 1];
         sortItemArray^[i].address := i * recordSize;
     end;

     SortBlockSortItemArray(sortItemArray);
     SortBlock := sortItemArray;
end;

{ Checks if merge block needs to be commited }
procedure CheckMergeBlock(var newPrimaryFile: TFile; var mergeBlock: TBlock; var mergeAddr, maxAddr: LongWord);
begin
     if mergeAddr >= maxAddr then begin
        BlockWrite(newPrimaryFile, mergeBlock, BLOCK_SIZE);
        mergeAddr := 0;

        Writeln('DEBUG | I detected merge block overflow and commited it');
     end;
end;

procedure Reorganize();
var overflowBlock, block: PBlock;
    mergeBlock: TBlock;
    overflowSortItems: PBlockSortItemArray;
    newPrimaryFile, oldFile, overflowFile: file of byte;
    blockCount, blockNumber, blockAddr, overflowAddr, mergeAddr, maxMergeAddr: LongWord;
    recordSize, blockFactor, maxAddr, i, overflowRecordCount, recordCount: LongWord;
    deletedItemOnTop: boolean;
begin
     { Setup files }
     Assign(oldFile, 'table.bin');
     Reset(oldFile);
     Assign(overflowFile, 'table.overflow');
     Assign(newPrimaryFile, TEMP_FILE);
     Rewrite(newPrimaryFile);

     { Setup helper vars }
     recordSize := GetRecordSize(GetCurrentTableMetadata());
     blockFactor := BLOCK_SIZE div recordSize;
     blockAddr := 0;
     mergeAddr := 0;
     overflowAddr := 0;
     maxMergeAddr := BLOCK_SIZE - (BLOCK_SIZE mod recordSize);
     deletedItemOnTop := false;

     { Setup overflow area }
     overflowBlock := GetOverflowBlock();
     overflowRecordCount := GetOverflowRecordCount();
     overflowSortItems := SortBlock(overFlowBlock, recordSize, overflowRecordCount);

     { Check if primary file is empty}
     if (FileSize(oldFile) = 0) then begin
        while overflowAddr < Length(overflowSortItems^) do begin
           for i:=0 to recordSize-1 do mergeBlock[mergeAddr + i] := overflowBlock^[overflowSortItems^[overflowAddr].address + i];
           mergeAddr := mergeAddr + recordSize;
           Inc(overflowAddr);

           Writeln('DEBUG | PRIMARY FILE is empty, inserting OVERFLOW with ', overflowSortItems^[overflowAddr-1].id);
         end;

        { Commit last block }
        BlockWrite(newPrimaryFile, mergeBlock, mergeAddr);

        { Close files }
        Close(newPrimaryFile);
        Close(oldFile);

        Rename(oldFile, 'tempname.temp');
        Rename(newPrimaryFile, 'table.bin');
        Rename(oldFile, TEMP_FILE);
        Rewrite(overflowFile);
        Close(overflowFile);

        { Build index }
        BuildIndex();

        exit;
     end;

     { Setup primary block }
     recordCount := GetRecordsCount(oldFile);
     blockCount := GetBlockCount(oldFile);
     blockNumber := 0;
     block := GetBlock(oldFile, blockNumber);

     if (blockNumber = blockCount-1) and ((FileSize(oldFile) mod BLOCK_SIZE) <> 0) then
        maxAddr := FileSize(oldFile) mod BLOCK_SIZE
     else
        maxAddr := BLOCK_SIZE - (BLOCK_SIZE mod recordSize);

     { Reorganize }
     while (true) do begin
         { Detect deletions }
         if overflowBlock^[overflowSortItems^[overflowAddr].address] = 1 then begin
            Inc(overflowAddr);
            deletedItemOnTop := true;

            Writeln('DEBUG | ', 'Detected deletion on OVERFLOW item with id: ', overflowSortItems^[overflowAddr-1].id);
         end;

         { Detect deletions }
         if block^[blockAddr] = 1 then begin
            blockAddr := blockAddr + recordSize;
            deletedItemOnTop := true;

            Writeln('DEBUG | ', 'Detected deletion on PRIMARY item with id: ', PLongWord(@(block^[blockAddr - recordSize + 1]))^);
         end;

         { Compare if not deleted }
         if not deletedItemOnTop then begin
           if overflowSortItems^[overflowAddr].id > (PLongWord(@(block^[blockAddr + 1]))^) then begin
              for i:=0 to recordSize-1 do mergeBlock[mergeAddr + i] := block^[blockAddr + i];
              mergeAddr := mergeAddr + recordSize;
              blockAddr := blockAddr + recordSize;

              Writeln('DEBUG | ', 'Inserting PRIMARY item with ', (PLongWord(@(block^[blockAddr - recordSize + 1]))^), ' over ', overflowSortItems^[overflowAddr].id);
           end else begin
              for i:=0 to recordSize-1 do mergeBlock[mergeAddr + i] := overflowBlock^[overflowSortItems^[overflowAddr].address + i];
              mergeAddr := mergeAddr + recordSize;
              Inc(overflowAddr);

               Writeln('DEBUG | ', 'Inserting OVERFLOW item with ', overflowSortItems^[overflowAddr-1].id, ' over ', (PLongWord(@(block^[blockAddr + 1]))^));
           end;
         end else begin
             deletedItemOnTop := false;
         end;

         { Check if merge block is full and commit it }
         CheckMergeBlock(newPrimaryFile, mergeBlock, mergeAddr, maxMergeAddr);

         { Check if primary block ended and loads upcoming }
         if blockAddr >= maxAddr then begin
            if blockNumber = blockCount-1 then begin
               { Commit rest of the overflow block}
               while overflowAddr < Length(overflowSortItems^) do begin
                 for i:=0 to recordSize-1 do mergeBlock[mergeAddr + i] := overflowBlock^[overflowSortItems^[overflowAddr].address + i];
                 mergeAddr := mergeAddr + recordSize;
                 Inc(overflowAddr);

                 Writeln('DEBUG | PRIMARY FILE is empty, inserting OVERFLOW with ', overflowSortItems^[overflowAddr-1].id);

                 { Check if merge block is full and commit it }
                 CheckMergeBlock(newPrimaryFile, mergeBlock, mergeAddr, maxMergeAddr);
               end;
               break;
            end else begin
                Inc(blockNumber);
                if (blockNumber = blockCount-1) and ((FileSize(oldFile) mod BLOCK_SIZE) <> 0) then
                   maxAddr := FileSize(oldFile) mod BLOCK_SIZE;
                block := GetBlock(oldFile, blockNumber);
                blockAddr := 0;

                 Writeln('DEBUG | Last block coming');
            end;
         end;

         { Check if overflow area is empty }
         if overflowAddr >= Length(overflowSortItems^) then begin
            { Commit rest of the primary file}
            while blockAddr <= maxAddr do begin
               for i:=0 to recordSize-1 do mergeBlock[mergeAddr + i] := block^[blockAddr + i];
               mergeAddr := mergeAddr + recordSize;
               blockAddr := blockAddr + recordSize;

               Writeln('DEBUG | OVERFLOW AREA is empty, inserting PRIMARY with ', PLongWord(@(block^[blockAddr - recordSize + 1]))^);

               { Check if merge block is full and commit it }
               CheckMergeBlock(newPrimaryFile, mergeBlock, mergeAddr, maxMergeAddr);

               { Check if primary block ended and loads upcoming }
               if blockAddr >= maxAddr then begin
                  if blockNumber = blockCount-1 then begin
                     break;
                  end else begin
                      Inc(blockNumber);
                      if (blockNumber = blockCount-1) and ((FileSize(oldFile) mod BLOCK_SIZE) <> 0) then
                         maxAddr := FileSize(oldFile) mod BLOCK_SIZE;
                      block := GetBlock(oldFile, blockNumber);
                      blockAddr := 0;
                  end;
               end;
             end;
            break;
         end;
     end;

     { Commit last block }
     BlockWrite(newPrimaryFile, mergeBlock, mergeAddr);

     { Close files }
     Close(newPrimaryFile);
     Close(oldFile);

     Rename(oldFile, 'tempname.temp');
     Rename(newPrimaryFile, 'table.bin');
     Rename(oldFile, TEMP_FILE);
     Rewrite(overflowFile);
     Close(overflowFile);

     { Build index }
     BuildIndex();
end;

end.

