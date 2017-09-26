#ifndef CRASPICAM_H
#define CRASPICAM_H

#ifdef __cplusplus
#include "raspicam/raspicam.h"
typedef raspicam::RaspiCam *raspiCamHandle;
#else
typedef struct RaspiCam *raspiCamHandle;
#endif

#ifdef __cplusplus
int RaspiCam_RASPICAM_FORMAT2int(raspicam::RASPICAM_FORMAT value);

int RaspiCam_RASPICAM_EXPOSURE2int(raspicam::RASPICAM_EXPOSURE value);

int RaspiCam_RASPICAM_AWB2int(raspicam::RASPICAM_AWB value);

int RaspiCam_RASPICAM_IMAGE_EFFECT2int(raspicam::RASPICAM_IMAGE_EFFECT value);
int RaspiCam_RASPICAM_METERING2int(raspicam::RASPICAM_METERING value);


extern "C"
{
#endif
    void hola();
    
    raspiCamHandle newRaspiCam();
    void deteleRaspiCam(raspiCamHandle handle);
    unsigned char RaspiCam_open(raspiCamHandle handle, unsigned char StartCapture);
    unsigned char RaspiCam_isOpened(raspiCamHandle handle);
    unsigned char RaspiCam_grab(raspiCamHandle handle);
    void RaspiCam_retrieve(raspiCamHandle handle, unsigned char *data, int type);
    unsigned char * RaspiCam_getImageBufferData(raspiCamHandle handle);
    
    unsigned long int RaspiCam_getImageBufferSize(raspiCamHandle handle);
    
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
    
    const unsigned long int RaspiCam_getImageTypeSize(raspiCamHandle handle, int type);
    
    
#ifdef __cplusplus
}
#endif
#endif/*CRASPICAM_H*/
