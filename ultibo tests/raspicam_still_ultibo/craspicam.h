#ifndef CRASPICAM_H
	#define CRASPICAM_H

	#ifdef __cplusplus
		#include "raspicam.h"
		#define EXPORTCALL __attribute__((stdcall))
		typedef raspicam::RaspiCam *raspiCamHandle;
	#else
		typedef struct raspicam::RaspiCam *raspiCamHandle;
		#define EXPORTCALL
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
		extern raspiCamHandle EXPORTCALL newRaspiCam();
		extern void EXPORTCALL deteleRaspiCam(raspiCamHandle handle);
		extern bool EXPORTCALL RaspiCam_open(raspiCamHandle handle, bool StartCapture);
		extern bool EXPORTCALL RaspiCam_isOpened(raspiCamHandle handle);
		extern bool EXPORTCALL RaspiCam_grab(raspiCamHandle handle);
		extern void EXPORTCALL RaspiCam_retrieve(raspiCamHandle handle, unsigned char *data, int type);
		extern unsigned char * EXPORTCALL RaspiCam_getImageBufferData(raspiCamHandle handle);

		extern unsigned long int EXPORTCALL RaspiCam_getImageBufferSize(raspiCamHandle handle);

		extern void EXPORTCALL RaspiCam_release(raspiCamHandle handle);

		extern void EXPORTCALL RaspiCam_setFormat(raspiCamHandle handle, int fmt);

		extern void EXPORTCALL RaspiCam_setWidth(raspiCamHandle handle, unsigned int width);

		extern void EXPORTCALL RaspiCam_setHeight(raspiCamHandle handle, unsigned int height);

		extern void EXPORTCALL RaspiCam_setCaptureSize(raspiCamHandle handle, unsigned int width, unsigned int height);


		extern void EXPORTCALL RaspiCam_setBrightness(raspiCamHandle handle, unsigned int brightness);

		extern void EXPORTCALL RaspiCam_setSharpness(raspiCamHandle handle, int sharpness);

		extern void EXPORTCALL RaspiCam_setContrast(raspiCamHandle handle, int contrast);

		extern void EXPORTCALL RaspiCam_setISO(raspiCamHandle handle, int iso);

		extern void EXPORTCALL RaspiCam_setSaturation(raspiCamHandle handle, int saturation);

		extern void EXPORTCALL RaspiCam_setVideoStabilization(raspiCamHandle handle, bool v);

		extern void EXPORTCALL RaspiCam_setExposureCompensation(raspiCamHandle handle, int val);

		extern void EXPORTCALL RaspiCam_setRotation(raspiCamHandle handle, int rotation);

		extern void EXPORTCALL RaspiCam_setExposure(raspiCamHandle handle, int exposure);

		extern void EXPORTCALL RaspiCam_setShutterSpeed(raspiCamHandle handle, unsigned int ss);

		extern void EXPORTCALL RaspiCam_setAWB(raspiCamHandle handle, int awb);

		extern void EXPORTCALL RaspiCam_setAWB_RB(raspiCamHandle handle, float r, float b);

		extern void EXPORTCALL RaspiCam_setImageEffect(raspiCamHandle handle, int imageEffect);

		extern void EXPORTCALL RaspiCam_setMetering(raspiCamHandle handle, int metering);

		extern void EXPORTCALL RaspiCam_setHorizontalFlip(raspiCamHandle handle, bool hFlip);

		extern void EXPORTCALL RaspiCam_setVerticalFlip(raspiCamHandle handle, bool vFlip);

		extern void EXPORTCALL RaspiCam_setFrameRate(raspiCamHandle handle, unsigned int fr);

		extern const unsigned int EXPORTCALL RaspiCam_getFormat(raspiCamHandle handle);

		extern const unsigned int EXPORTCALL RaspiCam_getWidth(raspiCamHandle handle);

		extern const unsigned int EXPORTCALL RaspiCam_getHeight(raspiCamHandle handle);

		extern const unsigned int EXPORTCALL RaspiCam_getBrightness(raspiCamHandle handle);

		extern const unsigned int EXPORTCALL RaspiCam_getRotation(raspiCamHandle handle);

		extern const int EXPORTCALL RaspiCam_getISO(raspiCamHandle handle);

		extern const int EXPORTCALL RaspiCam_getSharpness(raspiCamHandle handle);

		extern const int EXPORTCALL RaspiCam_getContrast(raspiCamHandle handle);

		extern const int EXPORTCALL RaspiCam_getSaturation(raspiCamHandle handle);

		extern const unsigned int EXPORTCALL RaspiCam_getShutterSpeed(raspiCamHandle handle);

		extern const unsigned int EXPORTCALL RaspiCam_getExposure(raspiCamHandle handle);

		extern const unsigned int EXPORTCALL RaspiCam_getAWB(raspiCamHandle handle);

		extern const float EXPORTCALL RaspiCam_getAWBG_red(raspiCamHandle handle);
		extern const float EXPORTCALL RaspiCam_getAWBG_blue(raspiCamHandle handle);

		extern const unsigned int EXPORTCALL RaspiCam_getImageEffect(raspiCamHandle handle);

		extern const unsigned int EXPORTCALL RaspiCam_getMetering(raspiCamHandle handle);

		extern const bool EXPORTCALL RaspiCam_isHorizontallyFlipped(raspiCamHandle handle);
		extern const bool EXPORTCALL RaspiCam_isVerticallyFlipped(raspiCamHandle handle);

		extern const unsigned int EXPORTCALL RaspiCam_getFrameRate(raspiCamHandle handle);

		extern const char * EXPORTCALL RaspiCam_getId(raspiCamHandle handle);

		extern const unsigned long int EXPORTCALL RaspiCam_getImageTypeSize(raspiCamHandle handle, int type);


		#ifdef __cplusplus
			}
		#endif
#endif/*CRASPICAM_H*/