program RaspiCamUltibo;

{$mode objfpc}{$H+}

{ Raspberry Pi Zero Application                                                }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }
{                                                                              }
{  To compile your program select Run, Compile (or Run, Build) from the menu.  }

uses
  RaspberryPi,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Ultibo,
  RaspiCamWrapper;

var r: raspiCamHandle;

begin
     r := newRaspiCam;
end.
