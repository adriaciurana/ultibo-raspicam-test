unit RaspiCamWrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
raspiCamHandle = type pointer;
function newRaspiCam:raspiCamHandle; cdecl;
implementation
{$linklib libraspicam}
{$IFDEF MSWINDOWS}
  {$linklib libmsvcrt}
{$ELSE}
  {$linklib c}
{$ENDIF}
function newRaspiCam:raspiCamHandle; cdecl; external;
end.

