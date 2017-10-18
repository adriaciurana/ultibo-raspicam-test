program raspicamUltibo;

{$mode objfpc}{$H+}

{ Raspberry Pi Zero Application                                                }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }
{                                                                              }
{  To compile your program select Run, Compile (or Run, Build) from the menu.  }

uses
 RaspberryPi3,
 GlobalConfig,
 GlobalConst,
 GlobalTypes,
 Platform,
 Threads,
 Console,
 Syscalls,
 VC4,
 ctypes,
 SysUtils,
 RaspiCamWrapper,
 Logging;

var camera: Raspicam;
var image : RaspiCam_Image;
var WindowHandle:TWindowHandle;
var response : cuint8;

{We also need to declare a variable to hold a console window handle.}

begin
     MMALIncludeComponentVideocore;
     WindowHandle := ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_FULL, True);

     LoggingDeviceSetTarget(LoggingDeviceFindByType(LOGGING_TYPE_FILE),'c:\ultibo.log');
     //The next line normally isn't required but FileSysLoggingStart currently has
     // a bug that causes it to fail if no target is specified on the command line
     LoggingDeviceStart(LoggingDeviceFindByType(LOGGING_TYPE_FILE));
     LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_FILE));

     {Wait a couple of seconds for C:\ drive to be ready}
     ConsoleWindowWriteLn(WindowHandle, 'Waiting for drive C:\');
     while not DirectoryExists('C:\') do
     begin
          {Sleep for a second}
          Sleep(1000);
     end;
     ConsoleWindowWriteLn(WindowHandle, 'C:\ drive is ready');
     ConsoleWindowWriteLn(WindowHandle, '');

     camera := newRaspiCam;

     {response := RaspiCam_open(camera, 1);
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
     if (response = 0) then
     begin
          ConsoleWindowWriteLn(WindowHandle,'Error when camera grab!');
          exit;
     end
     else
     begin
          ConsoleWindowWriteLn(WindowHandle,'Grab camera!');
     end;

     image := RaspiCam_retrieve(camera);
     if (image = nil) then
     begin
          ConsoleWindowWriteLn(WindowHandle,'Error when camera retrieve!');
          exit;
     end
     else
     begin
          ConsoleWindowWriteLn(WindowHandle,'Retrieve camera!');
     end;

     response := RaspiCam_save(image, 'C:\camera.jpg');
     if (response = 0) then
     begin
          ConsoleWindowWriteLn(WindowHandle,'Error when camera save!');
          exit;
     end
     else
     begin
          ConsoleWindowWriteLn(WindowHandle,'Save image!');
     end;}
     image := RaspiCam_getImage(camera);
     ConsoleWindowWriteLn(WindowHandle,'Get image!');
     RaspiCam_save(image, 'C:\camera.jpg');
     ConsoleWindowWriteLn(WindowHandle,'Save image!');

     image := RaspiCam_getImage(camera);
     ConsoleWindowWriteLn(WindowHandle,'Get image 2!');
     RaspiCam_save(image, 'C:\camera2.jpg');
     ConsoleWindowWriteLn(WindowHandle,'Save image 2!');
     ThreadHalt(0);
end.
