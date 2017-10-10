program RaspiCamUltibo;

{$mode objfpc}{$H+}

{ Raspberry Pi Zero Application                                                }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }
{                                                                              }
{  To compile your program select Run, Compile (or Run, Build) from the menu.  }

uses
  RaspberryPi, {Include RaspberryPi to make sure all standard functions are included}
  GlobalConst,
  GlobalTypes,
  Threads,
  Console,
  SysUtils,
  UltiboUtils,  {Include Ultibo utils for some command line manipulation}
  Syscalls,     {Include the Syscalls unit to provide C library support}
  VC4,          {Include the VC4 unit to enable access to the GPU}
  RaspiCamWrapper;

var camera: raspiCamHandle;
var image : imageArray;
var size_image : uint32;

begin
     camera := newRaspiCam;
     RaspiCam_open(camera, TRUE);
     RaspiCam_grab(camera);
     size_image := RaspiCam_retrieve(camera, @image);
     RaspiCam_saveAsPGM(camera, image, size_image);
end.

