#include "craspicam.h"
#ifdef __cplusplus
int RaspiCam_RASPICAM_FORMAT2int(raspicam::RASPICAM_FORMAT value){
    switch(value){
        case raspicam::RASPICAM_FORMAT_YUV420:
            return FORMAT_YUV420;
        case raspicam::RASPICAM_FORMAT_GRAY:
            return FORMAT_GRAY;
        case raspicam::RASPICAM_FORMAT_BGR:
            return FORMAT_BGR;
        case raspicam::RASPICAM_FORMAT_RGB:
            return FORMAT_RGB;
        case raspicam::RASPICAM_FORMAT_IGNORE:
            return FORMAT_IGNORE;
    }
    return -1;
}

int RaspiCam_RASPICAM_EXPOSURE2int(raspicam::RASPICAM_EXPOSURE value){
    switch(value){
        case raspicam::RASPICAM_EXPOSURE_OFF:
            return EXPOSURE_OFF;
        case raspicam::RASPICAM_EXPOSURE_AUTO:
            return EXPOSURE_AUTO;
        case raspicam::RASPICAM_EXPOSURE_NIGHT:
            return EXPOSURE_NIGHT;
        case raspicam::RASPICAM_EXPOSURE_NIGHTPREVIEW:
            return EXPOSURE_NIGHTPREVIEW;
        case raspicam::RASPICAM_EXPOSURE_BACKLIGHT:
            return EXPOSURE_BACKLIGHT;
        case raspicam::RASPICAM_EXPOSURE_SPOTLIGHT:
            return EXPOSURE_SPOTLIGHT;
        case raspicam::RASPICAM_EXPOSURE_SPORTS:
            return EXPOSURE_SPORTS;
        case raspicam::RASPICAM_EXPOSURE_SNOW:
            return EXPOSURE_SNOW;
        case raspicam::RASPICAM_EXPOSURE_BEACH:
            return EXPOSURE_BEACH;
        case raspicam::RASPICAM_EXPOSURE_VERYLONG:
            return EXPOSURE_VERYLONG;
        case raspicam::RASPICAM_EXPOSURE_FIXEDFPS:
            return EXPOSURE_FIXEDFPS;
        case raspicam::RASPICAM_EXPOSURE_ANTISHAKE:
            return EXPOSURE_ANTISHAKE;
        case raspicam::RASPICAM_EXPOSURE_FIREWORKS:
            return EXPOSURE_FIREWORKS;
    }
    return ERROR;
}

int RaspiCam_RASPICAM_AWB2int(raspicam::RASPICAM_AWB value){
    switch(value){
        case raspicam::RASPICAM_AWB_OFF:
            return AWB_OFF;
        case raspicam::RASPICAM_AWB_AUTO:
            return AWB_AUTO;
        case raspicam::RASPICAM_AWB_SUNLIGHT:
            return AWB_SUNLIGHT;
        case raspicam::RASPICAM_AWB_CLOUDY:
            return AWB_CLOUDY;
        case raspicam::RASPICAM_AWB_SHADE:
            return AWB_SHADE;
        case raspicam::RASPICAM_AWB_TUNGSTEN:
            return AWB_TUNGSTEN;
        case raspicam::RASPICAM_AWB_FLUORESCENT:
            return AWB_FLUORESCENT;
        case raspicam::RASPICAM_AWB_INCANDESCENT:
            return AWB_INCANDESCENT;
        case raspicam::RASPICAM_AWB_FLASH:
            return AWB_FLASH;
        case raspicam::RASPICAM_AWB_HORIZON:
            return AWB_HORIZON;
    }
    return ERROR;
}

int RaspiCam_RASPICAM_IMAGE_EFFECT2int(raspicam::RASPICAM_IMAGE_EFFECT value){
    switch(value){
        case raspicam::RASPICAM_IMAGE_EFFECT_NONE:
            return IMAGE_EFFECT_NONE;
        case raspicam::RASPICAM_IMAGE_EFFECT_NEGATIVE:
            return IMAGE_EFFECT_NEGATIVE;
        case raspicam::RASPICAM_IMAGE_EFFECT_SOLARIZE:
            return IMAGE_EFFECT_SOLARIZE;
        case raspicam::RASPICAM_IMAGE_EFFECT_SKETCH:
            return IMAGE_EFFECT_SKETCH;
        case raspicam::RASPICAM_IMAGE_EFFECT_DENOISE:
            return IMAGE_EFFECT_DENOISE;
        case raspicam::RASPICAM_IMAGE_EFFECT_EMBOSS:
            return IMAGE_EFFECT_EMBOSS;
        case raspicam::RASPICAM_IMAGE_EFFECT_OILPAINT:
            return IMAGE_EFFECT_OILPAINT;
        case raspicam::RASPICAM_IMAGE_EFFECT_HATCH:
            return IMAGE_EFFECT_HATCH;
        case raspicam::RASPICAM_IMAGE_EFFECT_GPEN:
            return IMAGE_EFFECT_GPEN;
        case raspicam::RASPICAM_IMAGE_EFFECT_PASTEL:
            return IMAGE_EFFECT_PASTEL;
        case raspicam::RASPICAM_IMAGE_EFFECT_WATERCOLOR:
            return IMAGE_EFFECT_WATERCOLOR;
        case raspicam::RASPICAM_IMAGE_EFFECT_FILM:
            return IMAGE_EFFECT_FILM;
        case raspicam::RASPICAM_IMAGE_EFFECT_BLUR:
            return IMAGE_EFFECT_BLUR;
        case raspicam::RASPICAM_IMAGE_EFFECT_SATURATION:
            return IMAGE_EFFECT_SATURATION;
        case raspicam::RASPICAM_IMAGE_EFFECT_COLORSWAP:
            return IMAGE_EFFECT_COLORSWAP;
        case raspicam::RASPICAM_IMAGE_EFFECT_WASHEDOUT:
            return IMAGE_EFFECT_WASHEDOUT;
        case raspicam::RASPICAM_IMAGE_EFFECT_POSTERISE:
            return IMAGE_EFFECT_POSTERISE;
        case raspicam::RASPICAM_IMAGE_EFFECT_COLORPOINT:
            return IMAGE_EFFECT_COLORPOINT;
        case raspicam::RASPICAM_IMAGE_EFFECT_COLORBALANCE:
            return IMAGE_EFFECT_COLORBALANCE;
        case raspicam::RASPICAM_IMAGE_EFFECT_CARTOON:
            return IMAGE_EFFECT_CARTOON;
    }
    return ERROR;
}

int RaspiCam_RASPICAM_METERING2int(raspicam::RASPICAM_METERING value){
    switch(value){
        case raspicam::RASPICAM_METERING_AVERAGE:
            return METERING_AVERAGE;
        case raspicam::RASPICAM_METERING_SPOT:
            return METERING_SPOT;
        case raspicam::RASPICAM_METERING_BACKLIT:
            return METERING_BACKLIT;
        case raspicam::RASPICAM_METERING_MATRIX:
            return METERING_MATRIX;
    }
    return ERROR;
}
#endif

extern "C"{
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
    
    size_t RaspiCam_getImageBufferSize(raspiCamHandle handle){
        return handle->getImageBufferSize();
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
        return RaspiCam_RASPICAM_FORMAT2int(handle->getFormat());
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
        return RaspiCam_RASPICAM_EXPOSURE2int(handle->getExposure());
    };
    
    const unsigned int RaspiCam_getAWB(raspiCamHandle handle){
        return RaspiCam_RASPICAM_AWB2int(handle->getAWB());
    };
    
    const float RaspiCam_getAWBG_red(raspiCamHandle handle){
        return handle->getAWBG_red();
    };
    const float RaspiCam_getAWBG_blue(raspiCamHandle handle){
        return handle->getAWBG_blue();
    };
    
    const unsigned int RaspiCam_getImageEffect(raspiCamHandle handle){
        return RaspiCam_RASPICAM_IMAGE_EFFECT2int(handle->getImageEffect());
    };
    
    const unsigned int RaspiCam_getMetering(raspiCamHandle handle){
        return RaspiCam_RASPICAM_METERING2int(handle->getMetering());
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
    
    const size_t RaspiCam_getImageTypeSize(raspiCamHandle handle, int type){
        return handle->getImageTypeSize(static_cast<raspicam::RASPICAM_FORMAT>(type));
    };
}
