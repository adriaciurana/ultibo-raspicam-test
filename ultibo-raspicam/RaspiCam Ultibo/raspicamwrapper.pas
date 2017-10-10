unit RaspiCamWrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
raspiCamHandle = type pointer;
function newRaspiCam:raspiCamHandle; cdecl;
implementation
{$linklib c}
{$linklib mmal}
{$linklib mmal_components}
{$linklib mmal_vc_client}
{$linklib mmal_core}
{$linklib mmal_util}
{$linklib vcos}
{$linklib libraspicam.a}
function newRaspiCam:raspiCamHandle; cdecl; external;
end.
