#include "craspicam.h"
#ifdef __cplusplus
using namespace raspicam;
int RaspiCam_RASPICAM_FORMAT2int(RASPICAM_FORMAT value){
    switch(value){
        case raspicam::RASPICAM_FORMAT_YUV420:
            return 0;
        case raspicam::RASPICAM_FORMAT_GRAY:
            return 1;
        case raspicam::RASPICAM_FORMAT_BGR:
            return 2;
        case raspicam::RASPICAM_FORMAT_RGB:
            return 3;
        case raspicam::RASPICAM_FORMAT_IGNORE:
            return 4;
    }
    return -1;
}

int RaspiCam_RASPICAM_EXPOSURE2int(RASPICAM_EXPOSURE value){
    switch(value){
        case raspicam::RASPICAM_EXPOSURE_OFF:
            return 0;
        case raspicam::RASPICAM_EXPOSURE_AUTO:
            return 1;
        case raspicam::RASPICAM_EXPOSURE_NIGHT:
            return 2;
        case raspicam::RASPICAM_EXPOSURE_NIGHTPREVIEW:
            return 3;
        case raspicam::RASPICAM_EXPOSURE_BACKLIGHT:
            return 4;
        case raspicam::RASPICAM_EXPOSURE_SPOTLIGHT:
            return 5;
        case raspicam::RASPICAM_EXPOSURE_SPORTS:
            return 6;
        case raspicam::RASPICAM_EXPOSURE_SNOW:
            return 7;
        case raspicam::RASPICAM_EXPOSURE_BEACH:
            return 8;
        case raspicam::RASPICAM_EXPOSURE_VERYLONG:
            return 9;
        case raspicam::RASPICAM_EXPOSURE_FIXEDFPS:
            return 10;
        case raspicam::RASPICAM_EXPOSURE_ANTISHAKE:
            return 11;
        case raspicam::RASPICAM_EXPOSURE_FIREWORKS:
            return 12;
    }
    return -1;
}

int RaspiCam_RASPICAM_AWB2int(RASPICAM_AWB value){
    switch(value){
        case raspicam::RASPICAM_AWB_OFF:
            return 0;
        case raspicam::RASPICAM_AWB_AUTO:
            return 1;
        case raspicam::RASPICAM_AWB_SUNLIGHT:
            return 2;
        case raspicam::RASPICAM_AWB_CLOUDY:
            return 3;
        case raspicam::RASPICAM_AWB_SHADE:
            return 4;
        case raspicam::RASPICAM_AWB_TUNGSTEN:
            return 5;
        case raspicam::RASPICAM_AWB_FLUORESCENT:
            return 6;
        case raspicam::RASPICAM_AWB_INCANDESCENT:
            return 7;
        case raspicam::RASPICAM_AWB_FLASH:
            return 8;
        case raspicam::RASPICAM_AWB_HORIZON:
            return 9;
    }
    return -1;
}

int RaspiCam_RASPICAM_IMAGE_EFFECT2int(RASPICAM_IMAGE_EFFECT value){
    switch(value){
        case raspicam::RASPICAM_IMAGE_EFFECT_NONE:
            return 0;
        case raspicam::RASPICAM_IMAGE_EFFECT_NEGATIVE:
            return 1;
        case raspicam::RASPICAM_IMAGE_EFFECT_SOLARIZE:
            return 2;
        case raspicam::RASPICAM_IMAGE_EFFECT_SKETCH:
            return 3;
        case raspicam::RASPICAM_IMAGE_EFFECT_DENOISE:
            return 4;
        case raspicam::RASPICAM_IMAGE_EFFECT_EMBOSS:
            return 5;
        case raspicam::RASPICAM_IMAGE_EFFECT_OILPAINT:
            return 6;
        case raspicam::RASPICAM_IMAGE_EFFECT_HATCH:
            return 7;
        case raspicam::RASPICAM_IMAGE_EFFECT_GPEN:
            return 8;
        case raspicam::RASPICAM_IMAGE_EFFECT_PASTEL:
            return 9;
        case raspicam::RASPICAM_IMAGE_EFFECT_WATERCOLOR:
            return 10;
        case raspicam::RASPICAM_IMAGE_EFFECT_FILM:
            return 11;
        case raspicam::RASPICAM_IMAGE_EFFECT_BLUR:
            return 12;
        case raspicam::RASPICAM_IMAGE_EFFECT_SATURATION:
            return 13;
        case raspicam::RASPICAM_IMAGE_EFFECT_COLORSWAP:
            return 14;
        case raspicam::RASPICAM_IMAGE_EFFECT_WASHEDOUT:
            return 15;
        case raspicam::RASPICAM_IMAGE_EFFECT_POSTERISE:
            return 16;
        case raspicam::RASPICAM_IMAGE_EFFECT_COLORPOINT:
            return 17;
        case raspicam::RASPICAM_IMAGE_EFFECT_COLORBALANCE:
            return 18;
        case raspicam::RASPICAM_IMAGE_EFFECT_CARTOON:
            return 19;
    }
    return -1;
}

int RaspiCam_RASPICAM_METERING2int(raspicam::RASPICAM_METERING value){
    switch(value){
        case raspicam::RASPICAM_METERING_AVERAGE:
            return 0;
        case raspicam::RASPICAM_METERING_SPOT:
            return 1;
        case raspicam::RASPICAM_METERING_BACKLIT:
            return 2;
        case raspicam::RASPICAM_METERING_MATRIX:
            return 3;
    }
    return -1;
}
#endif

extern "C"{
    void hola(){
        printf("hola");
    }
    raspiCamHandle newRaspiCam(){
        return new raspicam::RaspiCam;
    };
    void deteleRaspiCam(raspiCamHandle handle){
        delete handle;
    };
    unsigned char RaspiCam_open(raspiCamHandle handle, unsigned char StartCapture){
        return handle->open(StartCapture);
    };
    unsigned char RaspiCam_isOpened(raspiCamHandle handle){
        return handle->isOpened();
    };
    unsigned char RaspiCam_grab(raspiCamHandle handle){
        return handle->grab();
    };
    void RaspiCam_retrieve(raspiCamHandle handle, unsigned char *data, int type){
        handle->retrieve(data, static_cast<raspicam::RASPICAM_FORMAT>(type));
    };
    unsigned char * RaspiCam_getImageBufferData(raspiCamHandle handle){
        return handle->getImageBufferData();
    };
    
    unsigned long int RaspiCam_getImageBufferSize(raspiCamHandle handle){
        return (unsigned long int)handle->getImageBufferSize();
    };
    
    void RaspiCam_release(raspiCamHandle handle){
        handle->release();
    };
    
    void RaspiCam_setFormat(raspiCamHandle handle, int fmt){
        handle->setFormat(static_cast<raspicam::RASPICAM_FORMAT>(fmt));
    };
    
    void RaspiCam_setWidth(raspiCamHandle handle, unsigned int width){
        handle->setWidth(width);
    };
    
    void RaspiCam_setHeight(raspiCamHandle handle, unsigned int height){
        handle->setHeight(height);
    };
    
    void RaspiCam_setCaptureSize(raspiCamHandle handle, unsigned int width, unsigned int height){
        handle->setCaptureSize(width, height);
    };
    
    
    void RaspiCam_setBrightness(raspiCamHandle handle, unsigned int brightness){
        handle->setBrightness(brightness);
    };
    
    void RaspiCam_setSharpness(raspiCamHandle handle, int sharpness){
        handle->setSharpness(sharpness);
    };
    
    void RaspiCam_setContrast(raspiCamHandle handle, int contrast){
        handle->setContrast(contrast);
    };
    
    void RaspiCam_setISO(raspiCamHandle handle, int iso){
        handle->setISO(iso);
    };
    
    void RaspiCam_setSaturation(raspiCamHandle handle, int saturation){
        handle->setSaturation(saturation);
    };
    
    void RaspiCam_setVideoStabilization(raspiCamHandle handle, unsigned char v){
        handle->setVideoStabilization(v);
    };
    
    void RaspiCam_setExposureCompensation(raspiCamHandle handle, int val){
        handle->setExposureCompensation(val);
    };
    
    void RaspiCam_setRotation(raspiCamHandle handle, int rotation){
        handle->setRotation(rotation);
    };
    
    void RaspiCam_setExposure(raspiCamHandle handle, int exposure){
        handle->setExposure(static_cast<raspicam::RASPICAM_EXPOSURE>(exposure));
    };
    
    void RaspiCam_setShutterSpeed(raspiCamHandle handle, unsigned int ss){
        handle->setShutterSpeed(ss);
    };
    
    void RaspiCam_setAWB(raspiCamHandle handle, int awb){
        handle->setAWB(static_cast<raspicam::RASPICAM_AWB>(awb));
    };
    
    void RaspiCam_setAWB_RB(raspiCamHandle handle, float r, float b){
        handle->setAWB_RB(r, b);
    };
    
    void RaspiCam_setImageEffect(raspiCamHandle handle, int imageEffect){
        handle->setImageEffect(static_cast<raspicam::RASPICAM_IMAGE_EFFECT>(imageEffect));
    };
    
    void RaspiCam_setMetering(raspiCamHandle handle, int metering){
        handle->setMetering(static_cast<raspicam::RASPICAM_METERING>(metering));
    };
    
    void RaspiCam_setHorizontalFlip(raspiCamHandle handle, unsigned char hFlip){
        handle->setHorizontalFlip(hFlip);
    };
    
    void RaspiCam_setVerticalFlip(raspiCamHandle handle, unsigned char vFlip){
        handle->setVerticalFlip(vFlip);
    };
    
    void RaspiCam_setFrameRate(raspiCamHandle handle, unsigned int fr){
        handle->setFrameRate(fr);
    };
    
    const unsigned int RaspiCam_getFormat(raspiCamHandle handle){
        return handle->getFormat();
    };
    
    const unsigned int RaspiCam_getWidth(raspiCamHandle handle){
        return handle->getWidth();
    };
    
    const unsigned int RaspiCam_getHeight(raspiCamHandle handle){
        return handle->getHeight();
    };
    
    const unsigned int RaspiCam_getBrightness(raspiCamHandle handle){
        return handle->getBrightness();
    };
    
    const unsigned int RaspiCam_getRotation(raspiCamHandle handle){
        return handle->getRotation();
    };
    
    const int RaspiCam_getISO(raspiCamHandle handle){
        return handle->getISO();
    };
    
    const int RaspiCam_getSharpness(raspiCamHandle handle){
        return handle->getSharpness();
    };
    
    const int RaspiCam_getContrast(raspiCamHandle handle){
        return handle->getContrast();
    };
    
    const int RaspiCam_getSaturation(raspiCamHandle handle){
        return handle->getSaturation();
    };
    
    const unsigned int RaspiCam_getShutterSpeed(raspiCamHandle handle){
        return handle->getShutterSpeed();
    };
    
    const unsigned int RaspiCam_getExposure(raspiCamHandle handle){
        return handle->getExposure();
    };
    
    const unsigned int RaspiCam_getAWB(raspiCamHandle handle){
        return handle->getAWB();
    };
    
    const float RaspiCam_getAWBG_red(raspiCamHandle handle){
        return handle->getAWBG_red();
    };
    const float RaspiCam_getAWBG_blue(raspiCamHandle handle){
        return handle->getAWBG_blue();
    };
    
    const unsigned int RaspiCam_getImageEffect(raspiCamHandle handle){
        return handle->getImageEffect();
    };
    
    const unsigned int RaspiCam_getMetering(raspiCamHandle handle){
        return handle->getMetering();
    };
    
    const unsigned char RaspiCam_isHorizontallyFlipped(raspiCamHandle handle){
        return handle->isHorizontallyFlipped();
    };
    const unsigned char RaspiCam_isVerticallyFlipped(raspiCamHandle handle){
        return handle->isVerticallyFlipped();
    };
    
    const unsigned int RaspiCam_getFrameRate(raspiCamHandle handle){
        return handle->getFrameRate();
    };
    
    const char * RaspiCam_getId(raspiCamHandle handle){
        return handle->getId().c_str();
    };
    
    const unsigned long int RaspiCam_getImageTypeSize(raspiCamHandle handle, int type){
        return (unsigned long int)handle->getImageTypeSize(static_cast<raspicam::RASPICAM_FORMAT>(type));
    };
}
