unit RaspiCamWrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CTypes;
const
  MMAL_PARAMETER_CAMERA_INFO_MAX_STR_LEN = 16;
  MMAL_CAMERA_ANNOTATE_MAX_TEXT_LEN_V2 = 256;
  ENCODING_ERROR = -1;

// TYPES
type
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

ptrUint8 = ^cuint8;

// Format of the image (TODO)
RaspiCam_Format = (NONE,
	FORMAT_RGB,
	FORMAT_BGR,
	FORMAT_GRAY,
	FORMAT_YUV420);

// Raspicam_Camera
RaspiCam = record
      name: array[0..MMAL_PARAMETER_CAMERA_INFO_MAX_STR_LEN] of char;
      format: RaspiCam_Format;
      width: cuint16;
      height: cuint16;
      num: cuint8;
      framerate: cuint8;

      params_gap: ^uint32;
      component_gap: ^cuint32;
      port_gap: ^cuint32;
      pool_gap: ^cuint32;
      callback_data_gap: ^cuint32;
      flg_opened_gap: cuint8;
      flg_capture_gap: cuint8;
end;
PRaspiCam = ^RaspiCam;

// Raspicam_Image
RaspiCam_Image = record
      data: ^cuint8;
      length: cuint32;
      format: RaspiCam_Format;
      width: cuint16;
      height: cuint16;
end;
PRaspiCam_Image = ^RaspiCam_Image;

// Raspicam_exposure_mode
RaspiCam_Exposure_Mode = (MMAL_PARAM_EXPOSUREMODE_OFF,
   MMAL_PARAM_EXPOSUREMODE_AUTO,
   MMAL_PARAM_EXPOSUREMODE_NIGHT,
   MMAL_PARAM_EXPOSUREMODE_NIGHTPREVIEW,
   MMAL_PARAM_EXPOSUREMODE_BACKLIGHT,
   MMAL_PARAM_EXPOSUREMODE_SPOTLIGHT,
   MMAL_PARAM_EXPOSUREMODE_SPORTS,
   MMAL_PARAM_EXPOSUREMODE_SNOW,
   MMAL_PARAM_EXPOSUREMODE_BEACH,
   MMAL_PARAM_EXPOSUREMODE_VERYLONG,
   MMAL_PARAM_EXPOSUREMODE_FIXEDFPS,
   MMAL_PARAM_EXPOSUREMODE_ANTISHAKE,
   MMAL_PARAM_EXPOSUREMODE_FIREWORKS,
   MMAL_PARAM_EXPOSUREMODE_MAX);

// Raspicam_flicker_avoid
RaspiCam_Flicker_Avoid = (MMAL_PARAM_FLICKERAVOID_OFF,
   MMAL_PARAM_FLICKERAVOID_AUTO,
   MMAL_PARAM_FLICKERAVOID_50HZ,
   MMAL_PARAM_FLICKERAVOID_60HZ,
   MMAL_PARAM_FLICKERAVOID_MAX);

// Raspicam_AWB_Mode
Raspicam_AWB_Mode = (MMAL_PARAM_AWBMODE_OFF,
   MMAL_PARAM_AWBMODE_AUTO,
   MMAL_PARAM_AWBMODE_SUNLIGHT,
   MMAL_PARAM_AWBMODE_CLOUDY,
   MMAL_PARAM_AWBMODE_SHADE,
   MMAL_PARAM_AWBMODE_TUNGSTEN,
   MMAL_PARAM_AWBMODE_FLUORESCENT,
   MMAL_PARAM_AWBMODE_INCANDESCENT,
   MMAL_PARAM_AWBMODE_FLASH,
   MMAL_PARAM_AWBMODE_HORIZON,
   MMAL_PARAM_AWBMODE_MAX);

// Raspicam_ImageFX
Raspicam_ImageFX = (MMAL_PARAM_IMAGEFX_NONE,
   MMAL_PARAM_IMAGEFX_NEGATIVE,
   MMAL_PARAM_IMAGEFX_SOLARIZE,
   MMAL_PARAM_IMAGEFX_POSTERIZE,
   MMAL_PARAM_IMAGEFX_WHITEBOARD,
   MMAL_PARAM_IMAGEFX_BLACKBOARD,
   MMAL_PARAM_IMAGEFX_SKETCH,
   MMAL_PARAM_IMAGEFX_DENOISE,
   MMAL_PARAM_IMAGEFX_EMBOSS,
   MMAL_PARAM_IMAGEFX_OILPAINT,
   MMAL_PARAM_IMAGEFX_HATCH,
   MMAL_PARAM_IMAGEFX_GPEN,
   MMAL_PARAM_IMAGEFX_PASTEL,
   MMAL_PARAM_IMAGEFX_WATERCOLOUR,
   MMAL_PARAM_IMAGEFX_FILM,
   MMAL_PARAM_IMAGEFX_BLUR,
   MMAL_PARAM_IMAGEFX_SATURATION,
   MMAL_PARAM_IMAGEFX_COLOURSWAP,
   MMAL_PARAM_IMAGEFX_WASHEDOUT,
   MMAL_PARAM_IMAGEFX_POSTERISE,
   MMAL_PARAM_IMAGEFX_COLOURPOINT,
   MMAL_PARAM_IMAGEFX_COLOURBALANCE,
   MMAL_PARAM_IMAGEFX_CARTOON,
   MMAL_PARAM_IMAGEFX_DEINTERLACE_DOUBLE,
   MMAL_PARAM_IMAGEFX_DEINTERLACE_ADV,
   MMAL_PARAM_IMAGEFX_DEINTERLACE_FAST,
   MMAL_PARAM_IMAGEFX_MAX);

// Raspicam_ColorFX
Raspicam_ColorFX = record
	enable: Boolean;
	u: cint32;
	v: cint32;
end;
PRaspicam_ColorFX = ^Raspicam_ColorFX;

// RaspiCam_Exposure_Metering_Mode
RaspiCam_Exposure_Metering_Mode = (MMAL_PARAM_EXPOSUREMETERINGMODE_AVERAGE,
   MMAL_PARAM_EXPOSUREMETERINGMODE_SPOT,
   MMAL_PARAM_EXPOSUREMETERINGMODE_BACKLIT,
   MMAL_PARAM_EXPOSUREMETERINGMODE_MATRIX,
   MMAL_PARAM_EXPOSUREMETERINGMODE_MAX);

// Raspicam_rect
RaspiCam_Rect = record
	x: cfloat;
	y: cfloat;
	width: cfloat;
	height: cfloat;
end;
PRaspiCam_Rect = ^RaspiCam_Rect;

// RaspiCam_DCR_Strength
RaspiCam_DCR_Strength = (MMAL_PARAMETER_DRC_STRENGTH_OFF,
   MMAL_PARAMETER_DRC_STRENGTH_LOW,
   MMAL_PARAMETER_DRC_STRENGTH_MEDIUM,
   MMAL_PARAMETER_DRC_STRENGTH_HIGH,
   MMAL_PARAMETER_DRC_STRENGTH_MAX);

// RaspiCam_Annotate
RaspiCam_Color = record
	r: cuint8;
	g: cuint8;
	b: cuint8;
end;
RaspiCam_Annotate = record
	enable: Boolean;
	string_value: array[0..MMAL_PARAMETER_CAMERA_INFO_MAX_STR_LEN] of char;
	size: cuint8;
	color: RaspiCam_Color;
	bg_color: RaspiCam_Color;
end;
PRaspiCam_Annotate = ^RaspiCam_Annotate;

// GENERAL FUNCTIONS
function newRaspiCam: PRaspiCam; cdecl;
procedure RaspiCam_release(handle: PRaspiCam); cdecl;
procedure deleteRaspiCam(handle: PRaspiCam); cdecl;
function RaspiCam_startCapture(handle: PRaspiCam): Boolean; cdecl;
function RaspiCam_open(handle: PRaspiCam; startCapture: Boolean): Boolean; cdecl;
function RaspiCam_grab(handle: PRaspiCam): Boolean; cdecl;
function RaspiCam_getImageTypeSize(handle: PRaspiCam): uint32; cdecl;
function RaspiCam_retrieve(handle: PRaspiCam): PRaspiCam_Image; cdecl;
function RaspiCam_getImage(handle: PRaspiCam): PRaspiCam_Image; cdecl;
procedure RaspiCam_demo(); cdecl;

// CAMERA PROPERTIES
function RaspiCam_getName(handle: PRaspiCam): PChar;
procedure RaspiCam_setWidth(handle: PRaspicam; width: cuint16);
function RaspiCam_getWidth(handle: PRaspiCam): cuint16;
procedure RaspiCam_setHeight(handle: PRaspicam; height: cuint16);
function RaspiCam_getHeight(handle: PRaspiCam): cuint16;
//function getFormat(handle: PRaspiCam); cdecl;
function RaspiCam_getNum(handle: PRaspicam): cuint8;
function RaspiCam_getFrameRate(handle:PRaspicam): cuint8;

// IMAGE PROPERTIES
function RaspiCam_Image_getData(handle: PRaspiCam_Image): ptrUint8;
function RaspiCam_Image_getLength(handle: PRaspiCam_Image): cuint32;
function RaspiCam_Image_getWidth(handle: PRaspiCam_Image): cuint16;
function RaspiCam_Image_getHeight(handle: PRaspiCam_Image): cuint16;

// ColorFX
function newRaspiCam_ColorFX(enabled: Boolean; u: cint32; v: cint32): PRaspiCam_ColorFX;
procedure RaspiCam_ColorFX_setEnabled(handle: PRaspiCam_ColorFX; enabled: Boolean);
function RaspiCam_ColorFX_isEnabled(handle: PRaspiCam_ColorFX): Boolean;
procedure RaspiCam_ColorFX_setU(handle: PRaspiCam_ColorFX; u: cint32);
function RaspiCam_ColorFX_getU(handle: PRaspiCam_ColorFX): cint32;
procedure RaspiCam_ColorFX_setV(handle: PRaspiCam_ColorFX; v: cint32);
function RaspiCam_ColorFX_getV(handle: PRaspiCam_ColorFX): cint32;

// RECT
function newRaspiCam_Rect(x: cfloat; y: cfloat; width: cfloat; height: cfloat): PRaspiCam_Rect;
procedure RaspiCam_Rect_setX(handle: PRaspiCam_Rect; x: cfloat);
function RaspiCam_Rect_getX(handle: PRaspiCam_Rect): cfloat;
procedure RaspiCam_Rect_setY(handle: PRaspiCam_Rect; y: cfloat);
function RaspiCam_Rect_getY(handle: PRaspiCam_Rect): cfloat;
procedure RaspiCam_Rect_setWidth(handle: PRaspiCam_Rect; width: cfloat);
function RaspiCam_Rect_getWidth(handle: PRaspiCam_Rect): cfloat;
procedure RaspiCam_Rect_setHeight(handle: PRaspiCam_Rect; height: cfloat);
function RaspiCam_Rect_getHeight(handle: PRaspiCam_Rect): cfloat;

// Color
function newRaspiCam_Color(r: cuint8; g: cuint8; b: cuint8): RaspiCam_Color;
procedure RaspiCam_Color_setR(handle: RaspiCam_Color; r: cuint8);
function RaspiCam_Color_getR(handle: RaspiCam_Color): cuint8;
procedure RaspiCam_Color_setG(handle: RaspiCam_Color; g: cuint8);
function RaspiCam_Color_getG(handle: RaspiCam_Color): cuint8;
procedure RaspiCam_Color_setB(handle: RaspiCam_Color; b: cuint8);
function RaspiCam_Color_getB(handle: RaspiCam_Color): cuint8;

// Annotate
function newRaspiCam_Annotate(enable: Boolean; string_value: PChar; size: cuint8; color: RaspiCam_Color; bg_color: RaspiCam_Color): PRaspiCam_Annotate;
procedure RaspiCam_Annotate_setEnabled(handle: PRaspiCam_Annotate; enabled: Boolean);
function RaspiCam_Annotate_isEnabled(handle: PRaspiCam_Annotate): Boolean;
procedure RaspiCam_Annotate_setString(handle: PRaspiCam_Annotate; string_value: PChar);
function RaspiCam_Annotate_getString(handle: PRaspiCam_Annotate): PChar;
procedure RaspiCam_Annotate_setSize(handle: PRaspiCam_Annotate; size: cuint8);
function RaspiCam_Annotate_getSize(handle: PRaspiCam_Annotate): cuint8;
procedure RaspiCam_Annotate_setTextColor(handle: PRaspiCam_Annotate; color: RaspiCam_Color);
function RaspiCam_Annotate_getTextColor(handle: PRaspiCam_Annotate): RaspiCam_Color;
procedure RaspiCam_Annotate_setBgColor(handle: PRaspiCam_Annotate; color: RaspiCam_Color);
function RaspiCam_Annotate_getBgColor(handle: PRaspiCam_Annotate): RaspiCam_Color;

// ENCODER
function RaspiCam_save(image: PRaspiCam_Image; filename: PChar): Boolean; cdecl;

// FUNCTIONS GET/SET
function RaspiCam_setSharpness(handle: PRaspiCam; param: cint8): Boolean; cdecl;
function RaspiCam_getSharpness(handle: PRaspiCam): cint8; cdecl;

function RaspiCam_setContrast(handle: PRaspiCam; param: cint8): Boolean; cdecl;
function RaspiCam_getContrast(handle: PRaspiCam): cint8; cdecl;

function RaspiCam_setBrightness(handle: PRaspiCam; param: cuint8): Boolean; cdecl;
function RaspiCam_getBrightness(handle: PRaspiCam): cuint8; cdecl;

function RaspiCam_setSaturation(handle: PRaspiCam; param: cint8): Boolean; cdecl;
function RaspiCam_getSaturation(handle: PRaspiCam): cint8; cdecl;

function RaspiCam_setISO(handle: PRaspiCam; param: cint32): Boolean; cdecl;
function RaspiCam_getISO(handle: PRaspiCam): cint32; cdecl;

function RaspiCam_setVideoStab(handle: PRaspiCam; param: Boolean): Boolean; cdecl;
function RaspiCam_getVideoStab(handle: PRaspiCam): Boolean; cdecl;

function RaspiCam_setEVComp(handle: PRaspiCam; param: cuint8): Boolean; cdecl;
function RaspiCam_getEVComp(handle: PRaspiCam): cuint8; cdecl;

function _RaspiCam_setExposure_internal(handle: PRaspiCam; param: cint32): Boolean; cdecl;
function _RaspiCam_getExposure_internal(handle: PRaspiCam): cint32; cdecl;
function RaspiCam_setExposure(handle: PRaspiCam; param: RaspiCam_Exposure_Mode): Boolean;
function RaspiCam_getExposure(handle: PRaspiCam): RaspiCam_Exposure_Mode;

function _RaspiCam_setFlickerAvoid_internal(handle: PRaspiCam; param: cint32): Boolean; cdecl;
function _RaspiCam_getFlickerAvoid_internal(handle: PRaspiCam): cint32; cdecl;
function RaspiCam_setFlickerAvoid(handle: PRaspiCam; param: RaspiCam_Flicker_Avoid): Boolean;
function RaspiCam_getFlickerAvoid(handle: PRaspiCam): RaspiCam_Flicker_Avoid;

function _RaspiCam_setAWBMode_internal(handle: PRaspiCam; param: cint32): Boolean; cdecl;
function _RaspiCam_getAWBMode_internal(handle: PRaspiCam): cint32; cdecl;
function RaspiCam_setAWBMode(handle: PRaspiCam; param: Raspicam_AWB_Mode): Boolean;
function RaspiCam_getAWBMode(handle: PRaspiCam): Raspicam_AWB_Mode;

function _RaspiCam_setImageFX_internal(handle: PRaspiCam; param: cint32): Boolean; cdecl;
function _RaspiCam_getImageFX_internal(handle: PRaspiCam): cint32; cdecl;
function RaspiCam_setImageFX(handle: PRaspiCam; param: Raspicam_ImageFX): Boolean;
function RaspiCam_getImageFX(handle: PRaspiCam): Raspicam_ImageFX;

function RaspiCam_setColorFX(handle: PRaspiCam; param: Raspicam_ColorFX): Boolean; cdecl;
function RaspiCam_getColorFX(handle: PRaspiCam): PRaspicam_ColorFX; cdecl;

function _RaspiCam_setMeteringMode_internal(handle: PRaspiCam; param: cint32): Boolean; cdecl;
function _RaspiCam_getMeteringMode_internal(handle: PRaspiCam): cint32; cdecl;
function RaspiCam_setMeteringMode(handle: PRaspiCam; param: RaspiCam_Exposure_Metering_Mode): Boolean;
function RaspiCam_getMeteringMode(handle: PRaspiCam): RaspiCam_Exposure_Metering_Mode;

function RaspiCam_setRotation(handle: PRaspiCam; param: cint16): Boolean; cdecl;
function RaspiCam_getRotation(handle: PRaspiCam): cint16; cdecl;

function RaspiCam_setFlip(handle: PRaspiCam; horizontal: Boolean; vertical: Boolean): Boolean; cdecl;
function RaspiCam_getHorizontalFlip(handle: PRaspiCam): Boolean; cdecl;
function RaspiCam_getVerticalFlip(handle: PRaspiCam): Boolean; cdecl;

function RaspiCam_setROI(handle: PRaspiCam; rect: RaspiCam_Rect): Boolean; cdecl;
function RaspiCam_getROI(handle: PRaspiCam): PRaspiCam_Rect; cdecl;

function RaspiCam_setShutterSpeed(handle: PRaspiCam; param: cint32): Boolean; cdecl;
function RaspiCam_getShutterSpeed(handle: PRaspiCam): cint32; cdecl;

function RaspiCam_setAWBGains(handle: PRaspiCam; r: cfloat; b: cfloat): Boolean; cdecl;
function RaspiCam_getAWBGainR(handle: PRaspiCam): cfloat; cdecl;
function RaspiCam_getAWBGainB(handle: PRaspiCam): cfloat; cdecl;

function _RaspiCam_setDRCLevel_internal(handle: PRaspiCam; param: cint32): Boolean; cdecl;
function _RaspiCam_getDRCLevel_internal(handle: PRaspiCam): cint32; cdecl;
function RaspiCam_setDRCLevel(handle: PRaspiCam; param: RaspiCam_DCR_Strength): Boolean;
function RaspiCam_getDRCLevel(handle: PRaspiCam): RaspiCam_DCR_Strength;

function RaspiCam_setStatsPass(handle: PRaspiCam; param: Boolean): Boolean; cdecl;
function RaspiCam_getStatsPass(handle: PRaspiCam): Boolean; cdecl;

function RaspiCam_setAnnotate(handle: PRaspiCam; annotate: RaspiCam_Annotate): Boolean; cdecl;
function RaspiCam_getAnnotate(handle: PRaspiCam): PRaspiCam_Annotate; cdecl;




implementation
{$linklib libraspicam.a}

// GENERAL FUNCTIONS
function newRaspiCam: PRaspiCam; cdecl; external 'libraspicam' name 'newRaspiCam';
procedure RaspiCam_release(handle: PRaspiCam); cdecl; external 'libraspicam' name 'RaspiCam_release';
procedure deleteRaspiCam(handle: PRaspiCam); cdecl; external 'libraspicam' name 'deleteRaspiCam';
function RaspiCam_startCapture(handle: PRaspiCam): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_startCapture';
function RaspiCam_open(handle: PRaspiCam; startCapture: Boolean): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_open';
function RaspiCam_grab(handle: PRaspiCam): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_grab';
function RaspiCam_getImageTypeSize(handle: PRaspiCam): uint32; cdecl; external 'libraspicam' name 'RaspiCam_getImageTypeSize';
function RaspiCam_retrieve(handle: PRaspiCam): PRaspiCam_Image; cdecl; external 'libraspicam' name 'RaspiCam_retrieve';
function RaspiCam_getImage(handle: PRaspiCam): PRaspiCam_Image; cdecl; external 'libraspicam' name 'RaspiCam_getImage';
procedure RaspiCam_demo(); cdecl; external 'libraspicam' name 'RaspiCam_demo';

// CAMERA PROPERTIES
function RaspiCam_getName(handle: PRaspiCam): PChar;
begin;
	RaspiCam_getName := handle^.name;
end;
procedure RaspiCam_setWidth(handle: PRaspicam; width: cuint16);
begin;
	handle^.width := width;
end;
function RaspiCam_getWidth(handle: PRaspiCam): cuint16;
begin;
	RaspiCam_getWidth := handle^.width;
end;
procedure RaspiCam_setHeight(handle: PRaspicam; height: cuint16);
begin;
	handle^.height := height;
end;
function RaspiCam_getHeight(handle: PRaspiCam): cuint16;
begin;
	RaspiCam_getHeight := handle^.height;
end;
//function getFormat(handle: PRaspiCam); cdecl;
function RaspiCam_getNum(handle: PRaspicam): cuint8;
begin;
	RaspiCam_getNum := handle^.num;
end;
function RaspiCam_getFrameRate(handle:PRaspicam): cuint8;
begin;
	RaspiCam_getFrameRate := handle^.framerate;
end;

// IMAGE PROPERTIES
function RaspiCam_Image_getData(handle: PRaspiCam_Image): ptrUint8;
begin;
	RaspiCam_Image_getData := handle^.data;
end;
function RaspiCam_Image_getLength(handle: PRaspiCam_Image): cuint32;
begin;
	RaspiCam_Image_getLength := handle^.length;
end;
function RaspiCam_Image_getWidth(handle: PRaspiCam_Image): cuint16;
begin;
	RaspiCam_Image_getWidth := handle^.width;
end;
function RaspiCam_Image_getHeight(handle: PRaspiCam_Image): cuint16;
begin;
	RaspiCam_Image_getHeight := handle^.height;
end;

// ColorFX
function newRaspiCam_ColorFX(enabled: Boolean; u: cint32; v: cint32): PRaspiCam_ColorFX;
var
	ret: PRaspicam_ColorFX;
begin;
	ret := New(PRaspicam_ColorFX);
	ret^.enable := enabled;
	ret^.u := u;
	ret^.v := v;
	newRaspiCam_ColorFX := ret;
end;
procedure RaspiCam_ColorFX_setEnabled(handle: PRaspiCam_ColorFX; enabled: Boolean);
begin;
	handle^.enable := enabled;
end;
function RaspiCam_ColorFX_isEnabled(handle: PRaspiCam_ColorFX): Boolean;
begin;
	RaspiCam_ColorFX_isEnabled := handle^.enable;
end;
procedure RaspiCam_ColorFX_setU(handle: PRaspiCam_ColorFX; u: cint32);
begin;
	handle^.u := u;
end;
function RaspiCam_ColorFX_getU(handle: PRaspiCam_ColorFX): cint32;
begin;
	RaspiCam_ColorFX_getU := handle^.u;
end;
procedure RaspiCam_ColorFX_setV(handle: PRaspiCam_ColorFX; v: cint32);
begin;
	handle^.v := v;
end;
function RaspiCam_ColorFX_getV(handle: PRaspiCam_ColorFX): cint32;
begin;
	RaspiCam_ColorFX_getV := handle^.v;
end;
// RECT
function newRaspiCam_Rect(x: cfloat; y: cfloat; width: cfloat; height: cfloat): PRaspiCam_Rect;
var
	ret: PRaspiCam_Rect;
begin;
	ret := New(PRaspiCam_Rect);
	ret^.x := x;
	ret^.y := y;
	ret^.width := width;
	ret^.height := height;
	newRaspiCam_Rect := ret;
end;
procedure RaspiCam_Rect_setX(handle: PRaspiCam_Rect; x: cfloat);
begin;
	handle^.x := x;
end;
function RaspiCam_Rect_getX(handle: PRaspiCam_Rect): cfloat;
begin;
	RaspiCam_Rect_getX := handle^.x;
end;
procedure RaspiCam_Rect_setY(handle: PRaspiCam_Rect; y: cfloat);
begin;
	handle^.y := y;
end;
function RaspiCam_Rect_getY(handle: PRaspiCam_Rect): cfloat;
begin;
	RaspiCam_Rect_getY := handle^.y;
end;
procedure RaspiCam_Rect_setWidth(handle: PRaspiCam_Rect; width: cfloat);
begin;
	handle^.width := width;
end;
function RaspiCam_Rect_getWidth(handle: PRaspiCam_Rect): cfloat;
begin;
	RaspiCam_Rect_getWidth := handle^.width;
end;
procedure RaspiCam_Rect_setHeight(handle: PRaspiCam_Rect; height: cfloat);
begin;
	handle^.height := height;
end;
function RaspiCam_Rect_getHeight(handle: PRaspiCam_Rect): cfloat;
begin;
	RaspiCam_Rect_getHeight := handle^.height;
end;
// Color
function newRaspiCam_Color(r: cuint8; g: cuint8; b: cuint8): RaspiCam_Color;
var
        ret: RaspiCam_Color;
begin;
	ret.r := r;
	ret.g := g;
	ret.b := b;
	newRaspiCam_Color := ret;
end;
procedure RaspiCam_Color_setR(handle: RaspiCam_Color; r: cuint8);
begin;
	handle.r := r;
end;
function RaspiCam_Color_getR(handle: RaspiCam_Color): cuint8;
begin;
	RaspiCam_Color_getR := handle.r;
end;
procedure RaspiCam_Color_setG(handle: RaspiCam_Color; g: cuint8);
begin;
	handle.g := g;
end;
function RaspiCam_Color_getG(handle: RaspiCam_Color): cuint8;
begin;
	RaspiCam_Color_getG := handle.g;
end;
procedure RaspiCam_Color_setB(handle: RaspiCam_Color; b: cuint8);
begin;
	handle.b := b;
end;
function RaspiCam_Color_getB(handle: RaspiCam_Color): cuint8;
begin;
	RaspiCam_Color_getB := handle.b;
end;
// Annotate
function newRaspiCam_Annotate(enable: Boolean; string_value: PChar; size: cuint8; color: RaspiCam_Color; bg_color: RaspiCam_Color): PRaspiCam_Annotate;
var
        ret: PRaspiCam_Annotate;
begin;
	ret := New(PRaspiCam_Annotate);
	ret^.enable := enable;
	StrPCopy(ret^.string_value, string_value);
	ret^.size := size;
	ret^.color := color;
	ret^.bg_color := bg_color;
	newRaspiCam_Annotate := ret;
end;
procedure RaspiCam_Annotate_setEnabled(handle: PRaspiCam_Annotate; enabled: Boolean);
begin;
	handle^.enable := enabled;
end;
function RaspiCam_Annotate_isEnabled(handle: PRaspiCam_Annotate): Boolean;
begin;
	RaspiCam_Annotate_isEnabled := handle^.enable;
end;
procedure RaspiCam_Annotate_setString(handle: PRaspiCam_Annotate; string_value: PChar);
begin;
	StrPCopy(handle^.string_value, string_value);
end;
function RaspiCam_Annotate_getString(handle: PRaspiCam_Annotate): PChar;
begin;
	RaspiCam_Annotate_getString := handle^.string_value;
end;
procedure RaspiCam_Annotate_setSize(handle: PRaspiCam_Annotate; size: cuint8);
begin;
	handle^.size := size;
end;
function RaspiCam_Annotate_getSize(handle: PRaspiCam_Annotate): cuint8;
begin;
	RaspiCam_Annotate_getSize := handle^.size;
end;
procedure RaspiCam_Annotate_setTextColor(handle: PRaspiCam_Annotate; color: RaspiCam_Color);
begin;
	handle^.color := color;
end;
function RaspiCam_Annotate_getTextColor(handle: PRaspiCam_Annotate): RaspiCam_Color;
begin;
	RaspiCam_Annotate_getTextColor := handle^.color;
end;
procedure RaspiCam_Annotate_setBgColor(handle: PRaspiCam_Annotate; color: RaspiCam_Color);
begin;
	handle^.bg_color := color;
end;
function RaspiCam_Annotate_getBgColor(handle: PRaspiCam_Annotate): RaspiCam_Color;
begin;
	RaspiCam_Annotate_getBgColor := handle^.bg_color;
end;

// ENCODER
function RaspiCam_save(image: PRaspiCam_Image; filename: PChar): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_save';

// FUNCTIONS GET/SET
function RaspiCam_setSharpness(handle: PRaspiCam; param: cint8): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_getSharpness';
function RaspiCam_getSharpness(handle: PRaspiCam): cint8; cdecl; external 'libraspicam' name 'RaspiCam_setSharpness';

function RaspiCam_setContrast(handle: PRaspiCam; param: cint8): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_setContrast';
function RaspiCam_getContrast(handle: PRaspiCam): cint8; cdecl; external 'libraspicam' name 'RaspiCam_getContrast';

function RaspiCam_setBrightness(handle: PRaspiCam; param: cuint8): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_setBrightness';
function RaspiCam_getBrightness(handle: PRaspiCam): cuint8; cdecl; external 'libraspicam' name 'RaspiCam_getBrightness';

function RaspiCam_setSaturation(handle: PRaspiCam; param: cint8): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_setSaturation';
function RaspiCam_getSaturation(handle: PRaspiCam): cint8; cdecl; external 'libraspicam' name 'RaspiCam_getSaturation';

function RaspiCam_setISO(handle: PRaspiCam; param: cint32): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_setISO';
function RaspiCam_getISO(handle: PRaspiCam): cint32; cdecl; external 'libraspicam' name 'RaspiCam_getISO';

function RaspiCam_setVideoStab(handle: PRaspiCam; param: Boolean): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_setVideoStab';
function RaspiCam_getVideoStab(handle: PRaspiCam): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_getVideoStab';

function RaspiCam_setEVComp(handle: PRaspiCam; param: cuint8): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_setEVComp';
function RaspiCam_getEVComp(handle: PRaspiCam): cuint8; cdecl; external 'libraspicam' name 'RaspiCam_getEVComp';

function _RaspiCam_setExposure_internal(handle: PRaspiCam; param: cint32): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_setExposure';
function _RaspiCam_getExposure_internal(handle: PRaspiCam): cint32; cdecl; external 'libraspicam' name 'RaspiCam_getExposure';
function RaspiCam_setExposure(handle: PRaspiCam; param: RaspiCam_Exposure_Mode): Boolean;
begin;
 	RaspiCam_setExposure := _RaspiCam_setExposure_internal(handle, ord(param));
end;
function RaspiCam_getExposure(handle: PRaspiCam): RaspiCam_Exposure_Mode;
begin;
	RaspiCam_getExposure := RaspiCam_Exposure_Mode(_RaspiCam_getExposure_internal(handle));
end;

function _RaspiCam_setFlickerAvoid_internal(handle: PRaspiCam; param: cint32): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_setFlickerAvoid';
function _RaspiCam_getFlickerAvoid_internal(handle: PRaspiCam): cint32; cdecl; external 'libraspicam' name 'RaspiCam_getFlickerAvoid';
function RaspiCam_setFlickerAvoid(handle: PRaspiCam; param: RaspiCam_Flicker_Avoid): Boolean;
begin;
 	RaspiCam_setFlickerAvoid := _RaspiCam_setFlickerAvoid_internal(handle, ord(param));
end;
function RaspiCam_getFlickerAvoid(handle: PRaspiCam): RaspiCam_Flicker_Avoid;
begin;
	RaspiCam_getFlickerAvoid := RaspiCam_Flicker_Avoid(_RaspiCam_getFlickerAvoid_internal(handle));
end;

function _RaspiCam_setAWBMode_internal(handle: PRaspiCam; param: cint32): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_setAWB';
function _RaspiCam_getAWBMode_internal(handle: PRaspiCam): cint32; cdecl; external 'libraspicam' name 'RaspiCam_setAWB';
function RaspiCam_setAWBMode(handle: PRaspiCam; param: Raspicam_AWB_Mode): Boolean;
begin;
 	RaspiCam_setAWBMode := _RaspiCam_setAWBMode_internal(handle, ord(param));
end;
function RaspiCam_getAWBMode(handle: PRaspiCam): Raspicam_AWB_Mode;
begin;
	RaspiCam_getAWBMode := Raspicam_AWB_Mode(_RaspiCam_getAWBMode_internal(handle));
end;

function _RaspiCam_setImageFX_internal(handle: PRaspiCam; param: cint32): Boolean; cdecl;  external 'libraspicam' name 'RaspiCam_setImageFX';
function _RaspiCam_getImageFX_internal(handle: PRaspiCam): cint32; cdecl; external 'libraspicam' name 'RaspiCam_getImageFX';
function RaspiCam_setImageFX(handle: PRaspiCam; param: Raspicam_ImageFX): Boolean;
begin;
 	RaspiCam_setImageFX := _RaspiCam_setImageFX_internal(handle, ord(param));
end;
function RaspiCam_getImageFX(handle: PRaspiCam): Raspicam_ImageFX;
begin;
	RaspiCam_getImageFX := Raspicam_ImageFX(_RaspiCam_getImageFX_internal(handle));
end;

function RaspiCam_setColorFX(handle: PRaspiCam; param: Raspicam_ColorFX): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_setColorFX';
function RaspiCam_getColorFX(handle: PRaspiCam): PRaspicam_ColorFX; cdecl; external 'libraspicam' name 'RaspiCam_getColorFX';

function _RaspiCam_setMeteringMode_internal(handle: PRaspiCam; param: cint32): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_setMeterMode';
function _RaspiCam_getMeteringMode_internal(handle: PRaspiCam): cint32; cdecl; external 'libraspicam' name 'RaspiCam_getMeterMode';
function RaspiCam_setMeteringMode(handle: PRaspiCam; param: RaspiCam_Exposure_Metering_Mode): Boolean;
begin;
 	RaspiCam_setMeteringMode := _RaspiCam_setMeteringMode_internal(handle, ord(param));
end;
function RaspiCam_getMeteringMode(handle: PRaspiCam): RaspiCam_Exposure_Metering_Mode;
begin;
	RaspiCam_getMeteringMode := RaspiCam_Exposure_Metering_Mode(_RaspiCam_getMeteringMode_internal(handle));
end;
function RaspiCam_setRotation(handle: PRaspiCam; param: cint16): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_setRotation';
function RaspiCam_getRotation(handle: PRaspiCam): cint16; cdecl; external 'libraspicam' name 'RaspiCam_getRotation';

function RaspiCam_setFlip(handle: PRaspiCam; horizontal: Boolean; vertical: Boolean): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_setFlip';
function RaspiCam_getHorizontalFlip(handle: PRaspiCam): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_getHorizontalFlip';
function RaspiCam_getVerticalFlip(handle: PRaspiCam): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_getVerticalFlip';

function RaspiCam_setROI(handle: PRaspiCam; rect: RaspiCam_Rect): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_setROI';
function RaspiCam_getROI(handle: PRaspiCam): PRaspiCam_Rect; cdecl; external 'libraspicam' name 'RaspiCam_getROI';

function RaspiCam_setShutterSpeed(handle: PRaspiCam; param: cint32): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_setShutterSpeed';
function RaspiCam_getShutterSpeed(handle: PRaspiCam): cint32; cdecl; external 'libraspicam' name 'RaspiCam_getShutterSpeed';

function RaspiCam_setAWBGains(handle: PRaspiCam; r: cfloat; b: cfloat): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_setAWBGains';
function RaspiCam_getAWBGainR(handle: PRaspiCam): cfloat; cdecl; external 'libraspicam' name 'RaspiCam_getAWBGainR';
function RaspiCam_getAWBGainB(handle: PRaspiCam): cfloat; cdecl; external 'libraspicam' name 'RaspiCam_getAWBGainB';

function _RaspiCam_setDRCLevel_internal(handle: PRaspiCam; param: cint32): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_setDRCLevel';
function _RaspiCam_getDRCLevel_internal(handle: PRaspiCam): cint32; cdecl; external 'libraspicam' name 'RaspiCam_getDRCLevel';
function RaspiCam_setDRCLevel(handle: PRaspiCam; param: RaspiCam_DCR_Strength): Boolean;
begin;
 	RaspiCam_setDRCLevel := _RaspiCam_setDRCLevel_internal(handle, ord(param));
end;
function RaspiCam_getDRCLevel(handle: PRaspiCam): RaspiCam_DCR_Strength;
begin;
	RaspiCam_getDRCLevel := RaspiCam_DCR_Strength(_RaspiCam_getDRCLevel_internal(handle));
end;

function RaspiCam_setStatsPass(handle: PRaspiCam; param: Boolean): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_setStatsPass';
function RaspiCam_getStatsPass(handle: PRaspiCam): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_getStatsPass';

function RaspiCam_setAnnotate(handle: PRaspiCam; annotate: RaspiCam_Annotate): Boolean; cdecl; external 'libraspicam' name 'RaspiCam_setAnnotate';
function RaspiCam_getAnnotate(handle: PRaspiCam): PRaspiCam_Annotate; cdecl;  external 'libraspicam' name 'RaspiCam_getAnnotate';

//

end.
