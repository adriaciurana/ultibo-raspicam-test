unit RaspiCamWrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CTypes;
const
  MMAL_PARAMETER_CAMERA_INFO_MAX_STR_LEN = 16;


type RaspiCam_Format = uint8;
RaspiCam_Image_obj = record
      data: ^uint8;
      length: uint32;
      format: uint8;
      width: uint32;
      height: uint32;
end;
RaspiCam_obj = record
      name: array[0..MMAL_PARAMETER_CAMERA_INFO_MAX_STR_LEN] of char;
      format: RaspiCam_Format;
      width: uint32;
      height: uint32;
      framerate: uint8;
end;
RaspiCam_Image = ^RaspiCam_Image_obj;
RaspiCam = ^RaspiCam_obj;

function newRaspiCam: RaspiCam; cdecl;
procedure RaspiCam_release(handle: RaspiCam); cdecl;
procedure deleteRaspiCam(handle: RaspiCam); cdecl;
function RaspiCam_startCapture(handle: RaspiCam): cuint8; cdecl;
function RaspiCam_open(handle: RaspiCam; startCapture: cuint8): cuint8; cdecl;
function RaspiCam_grab(handle: RaspiCam): cuint8; cdecl;
function RaspiCam_getImageTypeSize(handle: RaspiCam): uint32; cdecl;
function RaspiCam_retrieve(handle: RaspiCam): RaspiCam_Image; cdecl;
function RaspiCam_save(image: RaspiCam_Image; filename: PChar): cuint8; cdecl;
function RaspiCam_getImage(handle: RaspiCam): RaspiCam_Image; cdecl;

implementation
{$linklib libraspicam.a}
function newRaspiCam: RaspiCam; cdecl; external 'libraspicam' name 'newRaspiCam';
procedure RaspiCam_release(handle: RaspiCam); cdecl; external 'libraspicam' name 'RaspiCam_release';
procedure deleteRaspiCam(handle: RaspiCam); cdecl; external 'libraspicam' name 'deleteRaspiCam';
function RaspiCam_startCapture(handle: RaspiCam): cuint8; cdecl; external 'libraspicam' name 'RaspiCam_startCapture';
function RaspiCam_open(handle: RaspiCam; startCapture: cuint8) : cuint8; cdecl; external 'libraspicam' name 'RaspiCam_open';
function RaspiCam_grab(handle: RaspiCam): cuint8; cdecl; external 'libraspicam' name 'RaspiCam_grab';
function RaspiCam_getImageTypeSize(handle: RaspiCam): uint32; cdecl; external 'libraspicam' name 'RaspiCam_getImageTypeSize';
function RaspiCam_retrieve(handle: RaspiCam): RaspiCam_Image; cdecl; external 'libraspicam' name 'RaspiCam_retrieve';
function RaspiCam_save(image: RaspiCam_Image; filename: PChar): cuint8; cdecl; external 'libraspicam' name 'RaspiCam_save';
function RaspiCam_getImage(handle: RaspiCam): RaspiCam_Image; cdecl; cdecl; external 'libraspicam' name 'RaspiCam_getImage';
end.

