#ifndef CRASPICAM_H
#define CRASPICAM_H

#ifdef __cplusplus
#include "raspicam/raspicam.h"
typedef raspicam::RaspiCam *raspiCamHandle;
#else
typedef struct RaspiCam *raspiCamHandle;
#define TRUE 1
#define FALSE 0
#endif

#define ERROR 50

// FORMATS
#define FORMAT_YUV420 0
#define FORMAT_GRAY 1
#define FORMAT_BGR 2
#define FORMAT_RGB 3
#define FORMAT_IGNORE 4

// EXPOSURES
#define EXPOSURE_OFF 0
#define EXPOSURE_AUTO 1
#define EXPOSURE_NIGHT 2
#define EXPOSURE_NIGHTPREVIEW 3
#define EXPOSURE_BACKLIGHT 4
#define EXPOSURE_SPOTLIGHT 5
#define EXPOSURE_SPORTS 6
#define EXPOSURE_SNOW 7
#define EXPOSURE_BEACH 8
#define EXPOSURE_VERYLONG 9
#define EXPOSURE_FIXEDFPS 10
#define EXPOSURE_ANTISHAKE 11
#define EXPOSURE_FIREWORKS 12

// AWB
#define AWB_OFF 0
#define AWB_AUTO 1
#define AWB_SUNLIGHT 2
#define AWB_CLOUDY 3
#define AWB_SHADE 4
#define AWB_TUNGSTEN 5
#define AWB_FLUORESCENT 6
#define AWB_INCANDESCENT 17
#define AWB_FLASH 8
#define AWB_HORIZON 9

// IMAGE EFFECT
#define IMAGE_EFFECT_NONE 0
#define IMAGE_EFFECT_NEGATIVE 1
#define IMAGE_EFFECT_SOLARIZE 2
#define IMAGE_EFFECT_SKETCH 3
#define IMAGE_EFFECT_DENOISE 4
#define IMAGE_EFFECT_EMBOSS 5
#define IMAGE_EFFECT_OILPAINT 6
#define IMAGE_EFFECT_HATCH 7
#define IMAGE_EFFECT_GPEN 8
#define IMAGE_EFFECT_PASTEL 9
#define IMAGE_EFFECT_WATERCOLOR 10
#define IMAGE_EFFECT_FILM 11
#define IMAGE_EFFECT_BLUR 12
#define IMAGE_EFFECT_SATURATION 13
#define IMAGE_EFFECT_COLORSWAP 14
#define IMAGE_EFFECT_WASHEDOUT 15
#define IMAGE_EFFECT_POSTERISE 16
#define IMAGE_EFFECT_COLORPOINT 17
#define IMAGE_EFFECT_COLORBALANCE 18
#define IMAGE_EFFECT_CARTOON 19

// METERING
#define METERING_AVERAGE 0
#define METERING_SPOT 1
#define METERING_BACKLIT 2
#define METERING_MATRIX 3


#ifdef __cplusplus
int RaspiCam_RASPICAM_FORMAT2int(raspicam::RASPICAM_FORMAT value);

int RaspiCam_RASPICAM_EXPOSURE2int(raspicam::RASPICAM_EXPOSURE value);

int RaspiCam_RASPICAM_AWB2int(raspicam::RASPICAM_AWB value);

int RaspiCam_RASPICAM_IMAGE_EFFECT2int(raspicam::RASPICAM_IMAGE_EFFECT value);
int RaspiCam_RASPICAM_METERING2int(raspicam::RASPICAM_METERING value);


extern "C"
{
#endif

    raspiCamHandle newRaspiCam();
    void deteleRaspiCam(raspiCamHandle handle);
    unsigned char RaspiCam_open(raspiCamHandle handle, unsigned char StartCapture);
    unsigned char RaspiCam_isOpened(raspiCamHandle handle);
    unsigned char RaspiCam_grab(raspiCamHandle handle);
    void RaspiCam_retrieve(raspiCamHandle handle, unsigned char *data, int type);
    unsigned char * RaspiCam_getImageBufferData(raspiCamHandle handle);
    
    size_t RaspiCam_getImageBufferSize(raspiCamHandle handle);
    
    void RaspiCam_release(raspiCamHandle handle);
    
    void RaspiCam_setFormat(raspiCamHandle handle, int fmt);
    
    void RaspiCam_setWidth(raspiCamHandle handle, unsigned int width);
    
    void RaspiCam_setHeight(raspiCamHandle handle, unsigned int height);
    
    void RaspiCam_setCaptureSize(raspiCamHandle handle, unsigned int width, unsigned int height);
    
    
    void RaspiCam_setBrightness(raspiCamHandle handle, unsigned int brightness);
    
    void RaspiCam_setSharpness(raspiCamHandle handle, int sharpness);
    
    void RaspiCam_setContrast(raspiCamHandle handle, int contrast);
    
    void RaspiCam_setISO(raspiCamHandle handle, int iso);
    
    void RaspiCam_setSaturation(raspiCamHandle handle, int saturation);
    
    void RaspiCam_setVideoStabilization(raspiCamHandle handle, unsigned char v);
    
    void RaspiCam_setExposureCompensation(raspiCamHandle handle, int val);
    
    void RaspiCam_setRotation(raspiCamHandle handle, int rotation);
    
    void RaspiCam_setExposure(raspiCamHandle handle, int exposure);
    
    void RaspiCam_setShutterSpeed(raspiCamHandle handle, unsigned int ss);
    
    void RaspiCam_setAWB(raspiCamHandle handle, int awb);
    
    void RaspiCam_setAWB_RB(raspiCamHandle handle, float r, float b);
    
    void RaspiCam_setImageEffect(raspiCamHandle handle, int imageEffect);
    
    void RaspiCam_setMetering(raspiCamHandle handle, int metering);
    
    void RaspiCam_setHorizontalFlip(raspiCamHandle handle, unsigned char hFlip);
    
    void RaspiCam_setVerticalFlip(raspiCamHandle handle, unsigned char vFlip);
    
    void RaspiCam_setFrameRate(raspiCamHandle handle, unsigned int fr);
    
    const unsigned int RaspiCam_getFormat(raspiCamHandle handle);
    
    const unsigned int RaspiCam_getWidth(raspiCamHandle handle);
    
    const unsigned int RaspiCam_getHeight(raspiCamHandle handle);
    
    const unsigned int RaspiCam_getBrightness(raspiCamHandle handle);
    
    const unsigned int RaspiCam_getRotation(raspiCamHandle handle);
    
    const int RaspiCam_getISO(raspiCamHandle handle);
    
    const int RaspiCam_getSharpness(raspiCamHandle handle);
    
    const int RaspiCam_getContrast(raspiCamHandle handle);
    
    const int RaspiCam_getSaturation(raspiCamHandle handle);
    
    const unsigned int RaspiCam_getShutterSpeed(raspiCamHandle handle);
    
    const unsigned int RaspiCam_getExposure(raspiCamHandle handle);
    
    const unsigned int RaspiCam_getAWB(raspiCamHandle handle);
    
    const float RaspiCam_getAWBG_red(raspiCamHandle handle);
    const float RaspiCam_getAWBG_blue(raspiCamHandle handle);
    
    const unsigned int RaspiCam_getImageEffect(raspiCamHandle handle);
    
    const unsigned int RaspiCam_getMetering(raspiCamHandle handle);
    
    const unsigned char RaspiCam_isHorizontallyFlipped(raspiCamHandle handle);
    const unsigned char RaspiCam_isVerticallyFlipped(raspiCamHandle handle);
    
    const unsigned int RaspiCam_getFrameRate(raspiCamHandle handle);
    
    const char * RaspiCam_getId(raspiCamHandle handle);
    
    const size_t RaspiCam_getImageTypeSize(raspiCamHandle handle, int type);
    
    
#ifdef __cplusplus
}
#endif
#endif/*CRASPICAM_H*/
