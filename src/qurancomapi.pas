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
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property Meta: TQuranAPIMeta read FMeta write FMeta;
  end;

  { TQuranComVerseItem }

  TQuranComVerseItem=class(TCollectionItem)
  private
    Fchapter_id: Integer;
    Fhizb_number: Integer;
    Fid: Integer;
    Fjuz_number: Integer;
    Fpage_number: Integer;
    Frub_number: Integer;
    //Fsajdah: Integer;
    //Fsajdah_number: Integer;
    Ftext_indopak: String;
    Ftext_madani: String;
    Ftext_simple: String;
    Fverse_key: String;
    Fverse_number: Integer;
  public
    constructor Create;
    constructor Create(ACollection: TCollection); override;
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
    //property sajdah: Integer read Fsajdah write Fsajdah;
    //property sajdah_number: Integer read Fsajdah_number write Fsajdah_number;
    property page_number: Integer read Fpage_number write Fpage_number;
  end;

  TQuranComVerses=class(TCollection)
  public
    constructor Create;
  end;

  { TQuranComResponse }

  TQuranComResponse=class
  public
    constructor Create; virtual; abstract;
  end;

  { TQuranComVersesResponse }

  TQuranComVersesResponse=class(TQuranComResponse)
  private
    Fmeta: TQuranAPIMeta;
    Fverses: TQuranComVerses;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property verses: TQuranComVerses read Fverses write Fverses;
    property meta: TQuranAPIMeta read Fmeta write Fmeta;
  end;

  { TQuranComAPIPages }

  TQuranComAPIPages = class(TStringList)
  private
    function GetItem(Index: Byte): Word;
  public
    property Item[Index: Byte]: Word read GetItem;
  end;

  { TQuranComChapterItem }

  TQuranComChapterItem=class(TCollectionItem)
  private
    Fbismillah_pre: Boolean;
    Fchapter_number: Word;
    Fid: Word;
    Fname_arabic: String;
    Fpages: TQuranComAPIPages;
    Frevelation_order: Word;
    Frevelation_place: String;
    Fverses_count: Word;
  public
    constructor Create(ACollection: TCollection); override;
    constructor Create;
    destructor Destroy; override;
  published
    property id: Word read Fid write Fid;
    property chapter_number: Word read Fchapter_number write Fchapter_number;
    property bismillah_pre: Boolean read Fbismillah_pre write Fbismillah_pre;
    property revelation_order: Word read Frevelation_order write Frevelation_order;
    property revelation_place: String read Frevelation_place write Frevelation_place;
    property name_arabic: String read Fname_arabic write Fname_arabic;
    property verses_count: Word read Fverses_count write Fverses_count;
    property pages: TQuranComAPIPages read Fpages write Fpages;
  end;

  { TQuranComChapters }

  TQuranComChapters=class(TCollection)
  public
    constructor Create;
  end;

  { TQuranComChaptersResponse }

  TQuranComChaptersResponse=class(TQuranComResponse)
  private
    Fchapters: TQuranComChapters;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property chapters: TQuranComChapters read Fchapters write Fchapters;
  end;

  { TDeStreamResponse }

  generic TDeStreamResponse<TResponse>=class
  private
    procedure DeStreamerRestoreProperty(Sender: TObject; {%H-}AObject: TObject;
      {%H-}Info: PPropInfo; AValue: TJSONData; var Handled: Boolean);
  public
    class function DeStreamResponse(const aURL: String; var aStringResponse: String): TResponse;
  end;

  TDeStreamChaptersResponse=specialize TDeStreamResponse<TQuranComChaptersResponse>;
  TDeStreamVersesResponse=specialize TDeStreamResponse<TQuranComVersesResponse>;

  { TQuranComAPI }

  TQuranComAPI = class
  private
    FQuranComVersesResponse: TQuranComVersesResponse;
    FQuranComChaptersResponse: TQuranComChaptersResponse;
    FResponse: String;
  public
    procedure DeStreamerRestoreProperty(Sender: TObject; {%H-}AObject: TObject;
      {%H-}Info: PPropInfo; AValue: TJSONData; var Handled: Boolean);
    destructor Destroy; override;
    function ListChapters(const aLanguage: String = ''): TQuranComChaptersResponse;
    function ListVerses(ChapterID: Word; aPage: Word = 0): TQuranComVersesResponse;
    property Response: String read FResponse write FResponse;
  end;

implementation

uses
  fphttpclient, fpjsonrtti
  ;

const
  API_ENDPOINT='http://staging.quran.com:3000/api/v3/';

function EndPoint_verses(ChapterID: Word; aPage: Word = 0): String;
begin
  Result:=API_ENDPOINT+'chapters/'+ChapterID.ToString+'/verses';
  if aPage<>0 then
    Result+='?page='+aPage.ToString;
end;

function EndPoint_chapters(const aLanguage: String = ''): String;
begin
  Result:=API_ENDPOINT+'chapters';
  if aLanguage<>EmptyStr then
    Result+='&language='+aLanguage;
end;

{ TQuranComVerseItem }

constructor TQuranComVerseItem.Create;
begin
  Create(nil);
end;

constructor TQuranComVerseItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

{ TDeStreamResponse }

procedure TDeStreamResponse.DeStreamerRestoreProperty(Sender: TObject;
  AObject: TObject; Info: PPropInfo; AValue: TJSONData; var Handled: Boolean);
var
  iValue: TJSONEnum;
begin
  if AValue.IsNull then
  begin
    Handled:=True;
    Exit;
  end;
  Handled:=False;
  if (AObject is TQuranComChapterItem) and (Info^.Name='pages') then
    if (AValue.JSONType=jtArray) then
    begin
      Handled:=True;
      for iValue in (AValue as TJSONArray) do
        TQuranComChapterItem(AObject).pages.Add(iValue.Value.AsString);
    end;
end;

class function TDeStreamResponse.DeStreamResponse(const aURL: String;
  var aStringResponse: String): TResponse;
var
  aHTTP: TFPHTTPClient;
  DeStreamer: TJSONDeStreamer;
begin
  aHTTP:=TFPHTTPClient.Create(nil);
  DeStreamer:=TJSONDeStreamer.Create(nil);
  DeStreamer.Options:=[];
  DeStreamer.OnRestoreProperty:=@DeStreamerRestoreProperty;
  Result:=TResponse.Create;
  try
    aStringResponse:=aHTTP.Get(aURL);
    DeStreamer.JSONToObject(aStringResponse, Result);
  finally
    aHTTP.Free;
    DeStreamer.Free;
  end;
end;

{ TQuranComChaptersResponse }

constructor TQuranComChaptersResponse.Create;
begin
  Fchapters:=TQuranComChapters.Create;
end;

destructor TQuranComChaptersResponse.Destroy;
begin
  Fchapters.Free;
  inherited Destroy;
end;

{ TQuranComChapterItem }

constructor TQuranComChapterItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  Fpages:=TQuranComAPIPages.Create;
end;

constructor TQuranComChapterItem.Create;
begin
  Create(nil);
end;

destructor TQuranComChapterItem.Destroy;
begin
  Fpages.Free;
  inherited Destroy;
end;

{ TQuranComAPIPages }

function TQuranComAPIPages.GetItem(Index: Byte): Word;
begin
  if Index>Count-1 then
    Exit(0);
  Result:=StrToIntDef(Strings[Index], 0);
end;

{ TQuranComChapters }

constructor TQuranComChapters.Create;
begin
  inherited Create(TQuranComChapterItem);
end;

{ TQuranComVersesResponse }

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
  inherited Create(TQuranComVerseItem);
end;

{ TQuranComAPIObject }

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
  FQuranComVersesResponse.Free;
  FQuranComChaptersResponse.Free;
  inherited Destroy;
end;

function TQuranComAPI.ListChapters(const aLanguage: String
  ): TQuranComChaptersResponse;
var
  aDestreamer: TDeStreamChaptersResponse;
begin
  aDestreamer:=TDeStreamChaptersResponse.Create;
  try
    FreeAndNil(FQuranComChaptersResponse);
    FQuranComChaptersResponse:=aDestreamer.DeStreamResponse(EndPoint_chapters(aLanguage), FResponse);
    Result:=FQuranComChaptersResponse;
  finally
    aDestreamer.Free;
  end;
end;

function TQuranComAPI.ListVerses(ChapterID: Word; aPage: Word
  ): TQuranComVersesResponse;
var
  aDestreamer: TDeStreamVersesResponse;
begin
  aDestreamer:=TDeStreamVersesResponse.Create;
  try
    FreeAndNil(FQuranComVersesResponse);
    FQuranComVersesResponse:=aDestreamer.DeStreamResponse(EndPoint_verses(ChapterID, aPage), FResponse);
    Result:=FQuranComVersesResponse;
  finally
    aDestreamer.Free;
  end;
end;

end.

