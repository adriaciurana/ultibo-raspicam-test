unit RaspiCamWrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CTypes;

type
raspiCamHandle = type pointer;
imageArray = ^uint8;
ptrImageArray = ^imageArray;

function newRaspiCam:raspiCamHandle; cdecl;
procedure RaspiCam_release(handle : raspiCamHandle); cdecl;
procedure deleteRaspiCam(handle : raspiCamHandle); cdecl;
function RaspiCam_startCapture(handle : raspiCamHandle) : Boolean; cdecl;
function RaspiCam_open(handle : raspiCamHandle; start_capture : Boolean) : Boolean; cdecl;
function RaspiCam_grab(handle : raspiCamHandle) : Boolean; cdecl;
function RaspiCam_getImageTypeSize(handle : raspiCamHandle) : uint32; cdecl;
function RaspiCam_retrieve(handle : raspiCamHandle; data : ptrImageArray) : uint32; cdecl;
function RaspiCam_saveAsPGM(handle : raspiCamHandle; data : imageArray; size_of_data : uint32) : uint32; cdecl;

implementation
{$linklib libraspicam.a}
function newRaspiCam:raspiCamHandle; cdecl; external;
procedure RaspiCam_release(handle : raspiCamHandle); cdecl; external;
procedure deleteRaspiCam(handle : raspiCamHandle); cdecl; external;
function RaspiCam_startCapture(handle : raspiCamHandle) : Boolean; cdecl; external;
function RaspiCam_open(handle : raspiCamHandle; start_capture : Boolean) : Boolean; cdecl; external;
function RaspiCam_grab(handle : raspiCamHandle) : Boolean; cdecl; external;
function RaspiCam_getImageTypeSize(handle : raspiCamHandle) : uint32; cdecl; external;
function RaspiCam_retrieve(handle : raspiCamHandle; data : ptrImageArray) : uint32; cdecl; external;
function RaspiCam_saveAsPGM(handle : raspiCamHandle; data : imageArray; size_of_data : uint32) : uint32; cdecl; external;
end.
