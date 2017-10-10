program raspicamUltibo;

{$mode objfpc}{$H+}

{ Raspberry Pi Zero Application                                                }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }
{                                                                              }
{  To compile your program select Run, Compile (or Run, Build) from the menu.  }

uses
  GlobalConst,
  GlobalTypes,
  Threads,
  Console,
  Framebuffer,
  BCM2837,
  BCM2710,
  SysUtils,
  UltiboUtils,  {Include Ultibo utils for some command line manipulation}
  Syscalls,     {Include the Syscalls unit to provide C library support}
  VC4,          {Include the VC4 unit to enable access to the GPU}
  RaspiCamWrapper;

var camera: raspiCamHandle;
var image : imageArray;
var size_image : uint32;
var WindowHandle:TWindowHandle;

{We also need to declare a variable to hold a console window handle.}

begin
     WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);
     camera := newRaspiCam;
     ConsoleWindowWriteLn(WindowHandle,'Init camera');
     RaspiCam_open(camera, TRUE);
     ConsoleWindowWriteLn(WindowHandle,'Open camera!');
     RaspiCam_grab(camera);
     ConsoleWindowWriteLn(WindowHandle,'Grab camera!');
     size_image := RaspiCam_retrieve(camera, @image);
     ConsoleWindowWriteLn(WindowHandle,'Obtain image');
     RaspiCam_saveAsPGM(camera, image, size_image);
     ConsoleWindowWriteLn(WindowHandle,'Save PGM');
     ThreadHalt(0);
end.

