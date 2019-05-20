unit qurancomapi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, fpjson
  ;

type

  { TQuranAPIMeta }

  TQuranAPIMeta = class
  private
    Fcurrent_page: Integer;
    Fnext_page: Integer;
    Fprev_page: Integer;
    Ftotal_count: Integer;
    Ftotal_page: Integer;
  published
    property current_page: Integer read Fcurrent_page write Fcurrent_page;
    property next_page: Integer read Fnext_page write Fnext_page;
    property prev_page: Integer read Fprev_page write Fprev_page;
    property total_page: Integer read Ftotal_page write Ftotal_page;
    property total_count: Integer read Ftotal_count write Ftotal_count;
  end;

  { TQuranComAPIObject }

  TQuranComAPIObject=class
  private
    FMeta: TQuranAPIMeta;
    procedure SetMeta(AValue: TQuranAPIMeta);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property Meta: TQuranAPIMeta read FMeta write SetMeta;
  end;

  { TQuranComVersesItem }

  TQuranComVersesItem=class(TCollectionItem)
  private
    Fchapter_id: Integer;
    Fhizb_number: Integer;
    Fid: Integer;
    Fjuz_number: Integer;
    Fpage_number: Integer;
    Frub_number: Integer;
    Fsajdah: Integer;
    //Fsajdah_number: Integer;
    Ftext_indopak: String;
    Ftext_madani: String;
    Ftext_simple: String;
    Fverse_key: String;
    Fverse_number: Integer;
  published
    property id: Integer read Fid write Fid;
    property verse_number: Integer read Fverse_number write Fverse_number;
    property chapter_id: Integer read Fchapter_id write Fchapter_id;
    property verse_key: String read Fverse_key write Fverse_key;
    property text_madani: String read Ftext_madani write Ftext_madani;
    property text_indopak: String read Ftext_indopak write Ftext_indopak;
    property text_simple: String read Ftext_simple write Ftext_simple;
    property juz_number: Integer read Fjuz_number write Fjuz_number;
    property hizb_number: Integer read Fhizb_number write Fhizb_number;
    property rub_number: Integer read Frub_number write Frub_number;
    property sajdah: Integer read Fsajdah write Fsajdah;
    //property sajdah_number: Integer read Fsajdah_number write Fsajdah_number;
    property page_number: Integer read Fpage_number write Fpage_number;
  end;

  TQuranComVerses=class(TCollection)
  public
    constructor Create;
  end;

  { TQuranComVersesResponse }

  TQuranComVersesResponse=class
  private
    Fmeta: TQuranAPIMeta;
    Fverses: TQuranComVerses;
    procedure Setmeta(AValue: TQuranAPIMeta);
    procedure Setverses(AValue: TQuranComVerses);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property verses: TQuranComVerses read Fverses write Setverses;
    property meta: TQuranAPIMeta read Fmeta write Setmeta;
  end;

  { TQuranComAPIPages }

  TQuranComAPIPages = class(TStringList)
  private
    function GetItem(Index: Byte): Word;
  public
    property Item[Index: Byte]: Word read GetItem;
  end;

  TQuranComChapterItem=class(TCollectionItem)
  published
    property id: Word;
    property chapter_number: Word;
    property bismillah_pre: Boolean;
    property revelation_order: Word;
    property revelation_place: Word;
    property name_arabic: String;
    property verses_count: Word;
    property pages: TQuranComAPIPages;
  end;

  { TQuranComChaptersResponse }

  TQuranComChaptersResponse=class(TCollection)
  public
    constructor Create;
  end;

  { TQuranComAPI }

  TQuranComAPI = class
  private
    FQuranComVersesResponse: TQuranComVersesResponse;
    FResponse: String;
  public
    procedure DeStreamerRestoreProperty(Sender: TObject; AObject: TObject;
      Info: PPropInfo; AValue: TJSONData; var Handled: Boolean);
    destructor Destroy; override;
    function ListChapters(const Language: String): TQuranComChaptersResponse;
    function ListVerses(ChapterID: Word): TQuranComVersesResponse;
    property Response: String read FResponse write FResponse;
  end;

implementation

uses
  fphttpclient, fpjsonrtti
  ;

const
  API_ENDPOINT='http://staging.quran.com:3000/api/v3/';

function EndPoint_chapters(ChapterID: Word): String;
begin
  Result:=API_ENDPOINT+'chapters/'+ChapterID.ToString+'/verses';
end;

{ TQuranComAPIPages }

function TQuranComAPIPages.GetItem(Index: Byte): Word;
begin
  if Index>Count-1 then
    Exit(0);
  Result:=StrToIntDef(Strings[Index], 0);
end;

{ TQuranComChaptersResponse }

constructor TQuranComChaptersResponse.Create;
begin
  inherited Create(TQuranComChapterItem);
end;

{ TQuranComVersesResponse }

procedure TQuranComVersesResponse.Setmeta(AValue: TQuranAPIMeta);
begin
  if Fmeta=AValue then Exit;
  Fmeta:=AValue;
end;

procedure TQuranComVersesResponse.Setverses(AValue: TQuranComVerses);
begin
  if Fverses=AValue then Exit;
  Fverses:=AValue;
end;

constructor TQuranComVersesResponse.Create;
begin
  Fverses:=TQuranComVerses.Create;
  Fmeta:=TQuranAPIMeta.Create;
end;

destructor TQuranComVersesResponse.Destroy;
begin
  Fmeta.Free;
  Fverses.Free;
  inherited Destroy;
end;

{ TQuranComVerses }

constructor TQuranComVerses.Create;
begin
  inherited Create(TQuranComVersesItem);
end;

{ TQuranComAPIObject }

procedure TQuranComAPIObject.SetMeta(AValue: TQuranAPIMeta);
begin
  if FMeta=AValue then Exit;
  FMeta:=AValue;
end;

constructor TQuranComAPIObject.Create;
begin
  FMeta:=TQuranAPIMeta.Create;
end;

destructor TQuranComAPIObject.Destroy;
begin
  FMeta.Free;
  inherited Destroy;
end;

{ TQuranComAPI }

procedure TQuranComAPI.DeStreamerRestoreProperty(Sender: TObject;
  AObject: TObject; Info: PPropInfo; AValue: TJSONData; var Handled: Boolean);
begin
  if AValue.IsNull then
    Handled:=True
  else
    Handled:=False;
end;

destructor TQuranComAPI.Destroy;
begin
  FreeAndNil(FQuranComVersesResponse);
  inherited Destroy;
end;

function TQuranComAPI.ListChapters(const Language: String
  ): TQuranComChaptersResponse;
begin

end;

function TQuranComAPI.ListVerses(ChapterID: Word): TQuranComVersesResponse;
var
  aHTTP: TFPHTTPClient;
  DeStreamer: TJSONDeStreamer;
begin
  aHTTP:=TFPHTTPClient.Create(nil);
  DeStreamer:=TJSONDeStreamer.Create(nil);
  DeStreamer.Options:=[jdoIgnorePropertyErrors];
  DeStreamer.OnRestoreProperty:=@DeStreamerRestoreProperty;
  FreeAndNil(FQuranComVersesResponse);
  FQuranComVersesResponse:=TQuranComVersesResponse.Create;
  try
    FResponse:=aHTTP.Get(EndPoint_chapters(ChapterID));
    DeStreamer.JSONToObject(FResponse, FQuranComVersesResponse);
    Result:=FQuranComVersesResponse;
  finally
    aHTTP.Free;
    DeStreamer.Free;
  end;
end;

end.

