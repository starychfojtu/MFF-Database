unit ByteFileUtils;

interface

uses
    DbRecord;

const
  BLOCK_SIZE = 4096;

type
  TFile = file of byte;
  PBlock = ^TBlock;
  TBlock = array[0..BLOCK_SIZE - 1] of byte;


{ Writes given record in given file }
procedure WriteRecord(var f: TFile; dbRecord: TDbRecord);

{ Returns n-th block of given file }
function GetBlock(var readFile: TFile; blockNumber: LongWord): PBlock;

{ Returns the amount of blocks in given file }
function GetBlockCount(var f: TFile): LongWord;

{ Writes a string in given file}
procedure WriteString(var f: TFile; item: string; totalLength: byte);

implementation

procedure WriteRecord(var f: TFile; dbRecord: TDbRecord);
var i: LongWord;
begin
     writeln('DEBUG | Inserting record');
     for i:=0 to Length(dbRecord.data)-1 do Write(' ',dbRecord.data[i]);

     BlockWrite(f, byte(dbRecord.IsDeleted), 1);
     BlockWrite(f, dbRecord.id, SizeOf(dbRecord.id));
     for i:=0 to Length(dbRecord.data)-1 do Write(f, dbRecord.data[i]);
end;

function GetBlock(var readFile: TFile; blockNumber: LongWord): PBlock;
var block: PBlock;
    bytesRead, bytesToRead: longword;
begin
     new(block);
     Seek(readFile, blockNumber * BLOCK_SIZE);
     BlockRead(readFile, block^, BLOCK_SIZE, bytesRead);

     GetBlock := block;
end;

function GetBlockCount(var f: TFile): LongWord;
begin
     if ((FileSize(f) mod BLOCK_SIZE) = 0) then
        getBlockCount := (FileSize(f) div BLOCK_SIZE)
     else
        getBlockCount := (FileSize(f) div BLOCK_SIZE) + 1;
end;

function ReadBlockByAddress(var readFile: TFile; address: Int64): PBlock;
var block: TBlock;
    bytesRead, bytesToRead: longword;
begin
     address := address AND $000;
     Seek(readFile, address);
     BlockRead(readFile, block, BLOCK_SIZE, bytesRead);

     ReadBlockByAddress := PBlock(@block);
end;

procedure WriteString(var f: TFile; item: string; totalLength: byte);
var i: longint;
begin
     BlockWrite(f, item, totalLength);
     {writeln('DEBUG | Writing ', item, ' of length ', length(item));
     write(f, byte(length(item)));
     for i:=1 to totalLength - 1 do
         write(f, byte(item[i]));}
end;

end.

