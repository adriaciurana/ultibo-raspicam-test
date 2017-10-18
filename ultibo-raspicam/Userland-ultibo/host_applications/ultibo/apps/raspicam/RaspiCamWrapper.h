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

#endif /* RaspiCamWrapper_h */
