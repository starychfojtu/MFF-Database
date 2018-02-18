unit DbRecord;

interface

uses
  SysUtils;

type
  PLongWord = ^LongWord;
  PByte = ^byte;

  PDbRecordData = ^TDbRecordData;
  TDbRecordData = array of byte;

  { Type representing database record }
  PDbRecord = ^TDbRecord;
  TDbRecord = record
    isDeleted: boolean;
    id: LongWord;
    data: TDbRecordData;
  end;

implementation

end.

