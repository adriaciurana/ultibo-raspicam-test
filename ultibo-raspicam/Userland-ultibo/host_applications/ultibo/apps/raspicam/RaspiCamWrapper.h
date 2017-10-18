//
//  RaspiCamWrapper.h
//  
//
//  Created by Adrià on 9/10/17.
//
//

#ifndef RaspiCamWrapper_h
#define RaspiCamWrapper_h
#define TRUE 1
#define FALSE 0

//#include "RaspiStill.c"
#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#ifndef ULTIBO
#include <memory.h>
#endif
#include <unistd.h>
#include <errno.h>
#ifdef ULTIBO

/* From posix/sysexits.h */
#define EX_OK		0	/* successful termination */

#define EX__BASE	64	/* base value for error messages */

#define EX_USAGE	64	/* command line usage error */
#define EX_DATAERR	65	/* data format error */
#define EX_NOINPUT	66	/* cannot open input */
#define EX_NOUSER	67	/* addressee unknown */
#define EX_NOHOST	68	/* host name unknown */
#define EX_UNAVAILABLE	69	/* service unavailable */
#define EX_SOFTWARE	70	/* internal software error */
#define EX_OSERR	71	/* system error (e.g., can't fork) */
#define EX_OSFILE	72	/* critical OS file missing */
#define EX_CANTCREAT	73	/* can't create (user) output file */
#define EX_IOERR	74	/* input/output error */
#define EX_TEMPFAIL	75	/* temp failure; user is invited to retry */
#define EX_PROTOCOL	76	/* remote error in protocol */
#define EX_NOPERM	77	/* permission denied */
#define EX_CONFIG	78	/* configuration error */

#define EX__MAX	78	/* maximum listed value */

#else
#include <sysexits.h>
#endif

#define VERSION_STRING "v1.3.11"

#include "bcm_host.h"
#include "interface/vcos/vcos.h"

#include "interface/mmal/mmal.h"
#include "interface/mmal/mmal_logging.h"
#include "interface/mmal/mmal_buffer.h"
#include "interface/mmal/util/mmal_util.h"
#include "interface/mmal/util/mmal_util_params.h"
#include "interface/mmal/util/mmal_default_components.h"
#include "interface/mmal/util/mmal_component_wrapper.h"
#include "interface/mmal/util/mmal_connection.h"
#include "interface/mmal/mmal_parameters_camera.h"


#include "RaspiCamControl.h"
#include "RaspiPreview.h"
#include "RaspiCLI.h"
#include "RaspiTex.h"

#include <semaphore.h>

// Standard port setting for the camera component
#define MMAL_CAMERA_PREVIEW_PORT 0
#define MMAL_CAMERA_VIDEO_PORT 1
#define MMAL_CAMERA_CAPTURE_PORT 2


// Stills format information
// 0 implies variable
#define STILLS_FRAME_RATE_NUM 0
#define STILLS_FRAME_RATE_DEN 1

/// Video render needs at least 2 buffers.
#define VIDEO_OUTPUT_BUFFERS_NUM 3

#define MAX_USER_EXIF_TAGS      32
#define MAX_EXIF_PAYLOAD_LENGTH 128

/// Frame advance method
#define FRAME_NEXT_SINGLE        0
#define FRAME_NEXT_TIMELAPSE     1
#define FRAME_NEXT_KEYPRESS      2
#define FRAME_NEXT_FOREVER       3
#define FRAME_NEXT_GPIO          4
#define FRAME_NEXT_SIGNAL        5
#define FRAME_NEXT_IMMEDIATELY   6

#define FORMAT_RGB 1
#define FORMAT_BGR 2
#define FORMAT_GRAY 3
#define FORMAT_YUV420 4

# define ENCODING_ERROR -1

#include "RaspiCamControl.h"

typedef unsigned char BOOLEAN;
typedef unsigned char RASPICAM_FORMAT;

typedef struct
{
	uint8_t *buffer_data;
	unsigned int buffer_length;
	MMAL_PORT_T *port;
	MMAL_POOL_T *pool;
	VCOS_SEMAPHORE_T complete_semaphore; /// semaphore which is posted when we reach end of frame (indicates end of capture or fault)
	BOOLEAN wantToGrab;
} PORT_USERDATA;

typedef struct{
	VCOS_SEMAPHORE_T complete_semaphore;
} PORT_USERDATA_ENCODER;

typedef struct{
	char name[MMAL_PARAMETER_CAMERA_INFO_MAX_STR_LEN];
	RASPICAM_CAMERA_PARAMETERS *params;
	RASPICAM_FORMAT format;
	MMAL_COMPONENT_T *component;
	MMAL_PORT_T *port;
	MMAL_POOL_T *pool;

	PORT_USERDATA *callback_data;
	unsigned int width;
	unsigned int height;
	unsigned int num;
	unsigned char framerate;

	BOOLEAN _isOpened;
	BOOLEAN _isCapturing;

	/* encoder */
	//MMAL_WRAPPER_T* encoder;
	//PORT_USERDATA_ENCODER *callback_data_encoder;
} RASPICAM_CAMERA;

typedef struct{
	unsigned char *data;
	size_t length;

	RASPICAM_FORMAT format;
	unsigned int width;
	unsigned int height;

} RASPICAM_IMAGE;
//unsigned char *data, size_t size_of_data, const char *filename, MMAL_FOURCC_T encoding

BOOLEAN _RaspiCam_create_camera(RASPICAM_CAMERA *camera);
BOOLEAN _RaspiCam_create_sensor(RASPICAM_CAMERA *camera);

MMAL_FOURCC_T _RaspiCam_getFormat(unsigned char format);
BOOLEAN _RaspiCam_create(RASPICAM_CAMERA *camera);
RASPICAM_CAMERA *newRaspiCam();
void RaspiCam_release(RASPICAM_CAMERA *camera);
void deleteRaspiCam(RASPICAM_CAMERA *camera);
BOOLEAN RaspiCam_startCapture(RASPICAM_CAMERA *camera);
BOOLEAN RaspiCam_open(RASPICAM_CAMERA *camera, BOOLEAN StartCapture);
BOOLEAN RaspiCam_grab(RASPICAM_CAMERA *camera);
size_t RaspiCam_getImageTypeSize(RASPICAM_CAMERA *camera);
RASPICAM_IMAGE *RaspiCam_retrieve(RASPICAM_CAMERA *camera);
RASPICAM_IMAGE *RaspiCam_getImage(RASPICAM_CAMERA *camera);

/* ENCODER */
const char *_RaspiCam_obtainExtension(const char *filename);
MMAL_FOURCC_T _RaspiCam_checkExtension(const char *filename);
BOOLEAN _RaspiCam_createImageWithEncoder(MMAL_WRAPPER_T *encoder, RASPICAM_IMAGE *image, const char *filename, MMAL_FOURCC_T encoding);
BOOLEAN RaspiCam_save(RASPICAM_IMAGE *image, const char *filename);


// SETS
/// Structure to cross reference exposure strings against the MMAL parameter equivalent
static XREF_T  exposure_map[] =
{
   {"off",           MMAL_PARAM_EXPOSUREMODE_OFF},
   {"auto",          MMAL_PARAM_EXPOSUREMODE_AUTO},
   {"night",         MMAL_PARAM_EXPOSUREMODE_NIGHT},
   {"nightpreview",  MMAL_PARAM_EXPOSUREMODE_NIGHTPREVIEW},
   {"backlight",     MMAL_PARAM_EXPOSUREMODE_BACKLIGHT},
   {"spotlight",     MMAL_PARAM_EXPOSUREMODE_SPOTLIGHT},
   {"sports",        MMAL_PARAM_EXPOSUREMODE_SPORTS},
   {"snow",          MMAL_PARAM_EXPOSUREMODE_SNOW},
   {"beach",         MMAL_PARAM_EXPOSUREMODE_BEACH},
   {"verylong",      MMAL_PARAM_EXPOSUREMODE_VERYLONG},
   {"fixedfps",      MMAL_PARAM_EXPOSUREMODE_FIXEDFPS},
   {"antishake",     MMAL_PARAM_EXPOSUREMODE_ANTISHAKE},
   {"fireworks",     MMAL_PARAM_EXPOSUREMODE_FIREWORKS}
};

static const int exposure_map_size = sizeof(exposure_map) / sizeof(exposure_map[0]);

/// Structure to cross reference flicker avoid strings against the MMAL parameter equivalent

static XREF_T  flicker_avoid_map[] =
{
   {"off",           MMAL_PARAM_FLICKERAVOID_OFF},
   {"auto",          MMAL_PARAM_FLICKERAVOID_AUTO},
   {"50hz",          MMAL_PARAM_FLICKERAVOID_50HZ},
   {"60hz",          MMAL_PARAM_FLICKERAVOID_60HZ}
};

static const int flicker_avoid_map_size = sizeof(flicker_avoid_map) / sizeof(flicker_avoid_map[0]);

/// Structure to cross reference awb strings against the MMAL parameter equivalent
static XREF_T awb_map[] =
{
   {"off",           MMAL_PARAM_AWBMODE_OFF},
   {"auto",          MMAL_PARAM_AWBMODE_AUTO},
   {"sun",           MMAL_PARAM_AWBMODE_SUNLIGHT},
   {"cloud",         MMAL_PARAM_AWBMODE_CLOUDY},
   {"shade",         MMAL_PARAM_AWBMODE_SHADE},
   {"tungsten",      MMAL_PARAM_AWBMODE_TUNGSTEN},
   {"fluorescent",   MMAL_PARAM_AWBMODE_FLUORESCENT},
   {"incandescent",  MMAL_PARAM_AWBMODE_INCANDESCENT},
   {"flash",         MMAL_PARAM_AWBMODE_FLASH},
   {"horizon",       MMAL_PARAM_AWBMODE_HORIZON}
};

static const int awb_map_size = sizeof(awb_map) / sizeof(awb_map[0]);

/// Structure to cross reference image effect against the MMAL parameter equivalent
static XREF_T imagefx_map[] =
{
   {"none",          MMAL_PARAM_IMAGEFX_NONE},
   {"negative",      MMAL_PARAM_IMAGEFX_NEGATIVE},
   {"solarise",      MMAL_PARAM_IMAGEFX_SOLARIZE},
   {"sketch",        MMAL_PARAM_IMAGEFX_SKETCH},
   {"denoise",       MMAL_PARAM_IMAGEFX_DENOISE},
   {"emboss",        MMAL_PARAM_IMAGEFX_EMBOSS},
   {"oilpaint",      MMAL_PARAM_IMAGEFX_OILPAINT},
   {"hatch",         MMAL_PARAM_IMAGEFX_HATCH},
   {"gpen",          MMAL_PARAM_IMAGEFX_GPEN},
   {"pastel",        MMAL_PARAM_IMAGEFX_PASTEL},
   {"watercolour",   MMAL_PARAM_IMAGEFX_WATERCOLOUR},
   {"film",          MMAL_PARAM_IMAGEFX_FILM},
   {"blur",          MMAL_PARAM_IMAGEFX_BLUR},
   {"saturation",    MMAL_PARAM_IMAGEFX_SATURATION},
   {"colourswap",    MMAL_PARAM_IMAGEFX_COLOURSWAP},
   {"washedout",     MMAL_PARAM_IMAGEFX_WASHEDOUT},
   {"posterise",     MMAL_PARAM_IMAGEFX_POSTERISE},
   {"colourpoint",   MMAL_PARAM_IMAGEFX_COLOURPOINT},
   {"colourbalance", MMAL_PARAM_IMAGEFX_COLOURBALANCE},
   {"cartoon",       MMAL_PARAM_IMAGEFX_CARTOON}
 };

static const int imagefx_map_size = sizeof(imagefx_map) / sizeof(imagefx_map[0]);

static XREF_T metering_mode_map[] =
{
   {"average",       MMAL_PARAM_EXPOSUREMETERINGMODE_AVERAGE},
   {"spot",          MMAL_PARAM_EXPOSUREMETERINGMODE_SPOT},
   {"backlit",       MMAL_PARAM_EXPOSUREMETERINGMODE_BACKLIT},
   {"matrix",        MMAL_PARAM_EXPOSUREMETERINGMODE_MATRIX}
};

static const int metering_mode_map_size = sizeof(metering_mode_map)/sizeof(metering_mode_map[0]);

static XREF_T drc_mode_map[] =
{
   {"off",           MMAL_PARAMETER_DRC_STRENGTH_OFF},
   {"low",           MMAL_PARAMETER_DRC_STRENGTH_LOW},
   {"med",           MMAL_PARAMETER_DRC_STRENGTH_MEDIUM},
   {"high",          MMAL_PARAMETER_DRC_STRENGTH_HIGH}
};

static const int drc_mode_map_size = sizeof(drc_mode_map)/sizeof(drc_mode_map[0]);

static XREF_T stereo_mode_map[] =
{
   {"off",           MMAL_STEREOSCOPIC_MODE_NONE},
   {"sbs",           MMAL_STEREOSCOPIC_MODE_SIDE_BY_SIDE},
   {"tb",            MMAL_STEREOSCOPIC_MODE_TOP_BOTTOM},
};

static const int stereo_mode_map_size = sizeof(stereo_mode_map)/sizeof(stereo_mode_map[0]);

// FUNCTIONS
BOOLEAN _RaspiCam_clamp(int param, int minv, int maxv);
BOOLEAN RaspiCam_setSharpness(RASPICAM_CAMERA *camera, int param);
int RaspiCam_getSharpness(RASPICAM_CAMERA *camera);
BOOLEAN RaspiCam_setContrast(RASPICAM_CAMERA *camera, int param);
int RaspiCam_getContrast(RASPICAM_CAMERA *camera);
BOOLEAN RaspiCam_setBrightness(RASPICAM_CAMERA *camera, int param);
int RaspiCam_getBrightness(RASPICAM_CAMERA *camera);
BOOLEAN RaspiCam_setSaturation(RASPICAM_CAMERA *camera, int param);
int RaspiCam_getSaturation(RASPICAM_CAMERA *camera);
BOOLEAN RaspiCam_setISO(RASPICAM_CAMERA *camera, int param);
int RaspiCam_getISO(RASPICAM_CAMERA *camera);
BOOLEAN RaspiCam_setVideoStab(RASPICAM_CAMERA *camera, BOOLEAN param);
BOOLEAN RaspiCam_getVideoStab(RASPICAM_CAMERA *camera);
BOOLEAN RaspiCam_setEVComp(RASPICAM_CAMERA *camera, int param);
BOOLEAN RaspiCam_getEVComp(RASPICAM_CAMERA *camera);
BOOLEAN RaspiCam_setExposure(RASPICAM_CAMERA *camera, int param);
MMAL_PARAM_EXPOSUREMODE_T RaspiCam_getExposure(RASPICAM_CAMERA *camera);
BOOLEAN RaspiCam_setFlickerAvoid(RASPICAM_CAMERA *camera, int param);
MMAL_PARAM_FLICKERAVOID_T RaspiCam_getFlickerAvoid(RASPICAM_CAMERA *camera);
BOOLEAN RaspiCam_setAWB(RASPICAM_CAMERA *camera, int param);
BOOLEAN RaspiCam_setImageFX(RASPICAM_CAMERA *camera, int param);
MMAL_PARAM_IMAGEFX_T RaspiCam_getImageFX(RASPICAM_CAMERA *camera);
BOOLEAN RaspiCam_setColorFX(RASPICAM_CAMERA *camera, BOOLEAN enable, int u, int v);
MMAL_PARAM_COLOURFX_T RaspiCam_getColorFX(RASPICAM_CAMERA *camera);
BOOLEAN RaspiCam_setMeterMode(RASPICAM_CAMERA *camera, int param);
MMAL_PARAM_EXPOSUREMETERINGMODE_T RaspiCam_getMeterMode(RASPICAM_CAMERA *camera);
BOOLEAN RaspiCam_setRotation(RASPICAM_CAMERA *camera, int param);
int RaspiCam_getRotation(RASPICAM_CAMERA *camera);
BOOLEAN RaspiCam_setFlip(RASPICAM_CAMERA *camera, BOOLEAN horizontal, BOOLEAN vertical);
BOOLEAN RaspiCam_setROI(RASPICAM_CAMERA *camera, float x, float y, float width, float height);
PARAM_FLOAT_RECT_T RaspiCam_getROI(RASPICAM_CAMERA *camera);
BOOLEAN RaspiCam_setShutterSpeed(RASPICAM_CAMERA *camera, int param);
int RaspiCam_getShutterSpeed(RASPICAM_CAMERA *camera);
BOOLEAN RaspiCam_setAwbGains(RASPICAM_CAMERA *camera, float awb_gains_r, float awb_gains_b);
float RaspiCam_getAwbGainR(RASPICAM_CAMERA *camera);
float RaspiCam_getAwbGainB(RASPICAM_CAMERA *camera);
BOOLEAN RaspiCam_setDRCLevel(RASPICAM_CAMERA *camera, int param);
MMAL_PARAMETER_DRC_STRENGTH_T RaspiCam_getDRCLevel(RASPICAM_CAMERA *camera);
BOOLEAN RaspiCam_setStatsPass(RASPICAM_CAMERA *camera, BOOLEAN param);
BOOLEAN RaspiCam_getStatsPass(RASPICAM_CAMERA *camera);
BOOLEAN RaspiCam_setAnnotate(RASPICAM_CAMERA *camera, int enable, const char *str, int size, int color, int bg_color);
BOOLEAN RaspiCam_getAnnotateIsEnable(RASPICAM_CAMERA *camera);
char *RaspiCam_getAnnotateText(RASPICAM_CAMERA *camera);
int RaspiCam_getAnnotateTextSize(RASPICAM_CAMERA *camera);
int RaspiCam_getAnnotateTextColor(RASPICAM_CAMERA *camera);
int RaspiCam_getAnnotateBgColor(RASPICAM_CAMERA *camera);
#endif /* RaspiCamWrapper_h */
