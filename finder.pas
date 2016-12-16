unit Finder;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils;

type
  EExtException = class(Exception);

  TExtVerifier = class
    class var Ext: string;
	  class function Check(AFileName: TFileName): Boolean;
  end;

  { TSqlTextFinder }

  TSqlTextFinder = class(TObject)
  private
    m_fileName: TFileName;
    m_strings: TStrings;
    m_CurrentLine: string;
    m_CurrentWord: string;
    m_Copying: Boolean;
    m_LastSymbol: Char;
    procedure SetFileName(Value: TFileName);
    function GetFileName: TFileName;
    function FindNextWord(const StartPos: Integer): Boolean;
    function IsSqlMainWord: Boolean;
  protected
    procedure LoadFile;
    function Process: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
    property FileName: TFileName read GetFileName write SetFileName;
  end;

implementation

resourcestring
  SInvalidExtensionFmt = 'Extensão inválida.';

const
  ExtToAccept: array[0..3] of string = ('.txt', '.pas', '.dfm', '.sql');
  SqlMainWords: array[0..3] of string = ('SELECT', 'INSERT', 'UPDATE', 'DELETE');


procedure TSqlTextFinder.SetFileName(Value: TFileName);
begin
  if m_fileName <> Value then
  begin
    if not TExtVerifier.Check(Value) then
    begin
      raise EExtException.CreateFmt(SInvalidExtensionFmt, [TExtVerifier.Ext]);
    end;

    m_fileName := Value;

    LoadFile;
  end;
end;

function TSqlTextFinder.GetFileName: TFileName;
begin
  Result := m_fileName;
end;

function TSqlTextFinder.FindNextWord(const StartPos: Integer): Boolean;
var
  k: Integer;
  leave, scanning: Boolean;
begin
  m_CurrentWord := '';
  k := StartPos;
  leave := False;
  scanning := False;

  while not leave do
  begin
    m_LastSymbol := m_CurrentLine[k];

    leave := CharInSet(m_LastSymbol, [Chr($20), Chr($22), Chr($27)]) and scanning;
    scanning := CharInSet(m_LastSymbol, ['A'..'Z', 'a', 'z']);

    if not leave then
    begin
      m_CurrentWord := m_CurrentWord + m_LastSymbol;
    end;

    Inc(k);
  end;
  Result := m_CurrentWord > '';
end;

function TSqlTextFinder.IsSqlMainWord: Boolean;
var
  tempStr, curWord : string;
begin
  tempStr := UpperCase(m_CurrentWord);
  Result := False;
  for curWord in SqlMainWords do
  begin
    if curWord = tempStr then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TSqlTextFinder.LoadFile;
begin
  m_strings.LoadFromFile(m_fileName);
end;

function TSqlTextFinder.Process: Boolean;
var
  i, j: Integer;
  curSymbol: Char;
begin
  for i := 0 to m_strings.Count - 1 do
  begin
    m_CurrentLine := m_strings[i];
    j := 1;
    while j < Length(m_CurrentLine) do
    begin
      curSymbol := m_CurrentLine[j];
      case curSymbol of
        Chr($22), Chr($27):
          begin
            m_LastSymbol := curSymbol;
            if FindNextWord(j + 1) then
            begin
              j := j + Length(m_CurrentWord);
              if IsSqlMainWord then
              begin
                m_Copying := True;
              end;
            end;
          end;
        else
          j := j + 1;
      end;
    end;
  end;
end;

constructor TSqlTextFinder.Create;
begin
  inherited Create;
  m_strings := TStringList.Create;
end;

destructor TSqlTextFinder.Destroy;
begin
  m_strings.Free;
  inherited Destroy;
end;

procedure TSqlTextFinder.Run;
begin
  Process;
end;

class function TExtVerifier.Check(AFileName: TFileName): Boolean;
var
  //i: Byte;
  fe, w: string;
begin
  fe := LowerCase(ExtractFileExt(AFileName));
  Ext := fe;
  Result := False;
  for w in ExtToAccept do
  begin
    if w = fe then
    begin
      Result := True;
      Break;
    end;
  end;
end;

end.
