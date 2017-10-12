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
  RaspiCamWrapper,
  ctypes,
  FileSystem,  {Include the file system core and interfaces}
  FATFS,       {Include the FAT file system driver}
  MMC;         {Include the MMC/SD core to access our SD card}

var camera: Raspicam;
var image : ptrImage;
var WindowHandle:TWindowHandle;
var response : cuint8;

{We also need to declare a variable to hold a console window handle.}

begin
     Sleep(5000);
     WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

     {Wait a couple of seconds for C:\ drive to be ready}
     ConsoleWindowWriteLn(WindowHandle,'Waiting for drive C:\');
     while not DirectoryExists('C:\') do
     begin
          {Sleep for a second}
          Sleep(1000);
     end;
     ConsoleWindowWriteLn(WindowHandle,'C:\ drive is ready');
     ConsoleWindowWriteLn(WindowHandle,'');

     MMALIncludeComponentVideocore;
     {Sleep(5000);}
     camera := newRaspiCam;
     ConsoleWindowWriteLn(WindowHandle,'Init camera 2!');

     response := RaspiCam_open(camera, 1);

     if (response = 0) then
     begin
        ConsoleWindowWriteLn(WindowHandle,'Error when camera open!');
        exit;
     end
     else
     begin
          ConsoleWindowWriteLn(WindowHandle,'Open camera!');
     end;

     response := RaspiCam_grab(camera);
     if(response = 0) then
     begin
          ConsoleWindowWriteLn(WindowHandle,'Error when camera grab!');
          exit;
     end
     else
     begin
          ConsoleWindowWriteLn(WindowHandle,'Grab camera!');
     end;

     image := RaspiCam_retrieve(camera);
     if(image = nil) then
     begin
          ConsoleWindowWriteLn(WindowHandle,'Error when camera retrieve!');
          exit;
     end
     else
     begin
          ConsoleWindowWriteLn(WindowHandle,'Retrieve camera!');
     end;

     {ConsoleWindowWriteLn(WindowHandle,'Obtain image with buffer size: ' + IntToStr(image^.length)); }
     RaspiCam_save(image, 'camera.jpg');
     ConsoleWindowWriteLn(WindowHandle,'Save JPEG');
     ThreadHalt(0);
end.


