//
//  RaspiCamWrapper.c
//  
//
//  Created by Adrià on 9/10/17.
//
//

#include "RaspiCamWrapper.h"


// GENERAL FUNCTIONS
RASPICAM_BOOLEAN _RaspiCam_create_sensor(RASPICAM_CAMERA *camera){
    MMAL_COMPONENT_T *camera_info;
    MMAL_STATUS_T status = MMAL_SUCCESS;

    strncpy(camera->name, "OV5647", MMAL_PARAMETER_CAMERA_INFO_MAX_STR_LEN);

    // Try to get the camera name and maximum supported resolution
    status = mmal_component_create(MMAL_COMPONENT_DEFAULT_CAMERA_INFO, &camera_info);
    if (status == MMAL_SUCCESS){
        MMAL_PARAMETER_CAMERA_INFO_T param;

        param.hdr.id = MMAL_PARAMETER_CAMERA_INFO;
        param.hdr.size = sizeof(param) - 4;  // Deliberately undersize to check firmware veresion
        status = mmal_port_parameter_get(camera_info->control, &param.hdr);

        if (status != MMAL_SUCCESS){
            // Running on newer firmware
            param.hdr.size = sizeof(param);
            status = mmal_port_parameter_get(camera_info->control, &param.hdr);
            if (status == MMAL_SUCCESS && param.num_cameras > camera->num){
                if (camera->width == 0)
                    camera->width = param.cameras[camera->num].max_width;
                if (camera->height == 0)
                    camera->height = param.cameras[camera->num].max_height;
                strncpy(camera->name, param.cameras[camera->num].camera_name, MMAL_PARAMETER_CAMERA_INFO_MAX_STR_LEN);
                camera->name[MMAL_PARAMETER_CAMERA_INFO_MAX_STR_LEN - 1] = NULL;
            }else{
                printf("Cannot read camera info, keeping the defaults for OV5647\n");
                goto error_s;
            }
        }
        mmal_component_destroy(camera_info);
    }else{
        printf("Failed to create camera_info component\n");
        goto error_s;
    }

    // older version
    if (camera->width == 0)
        camera->width = 2592;
    if (camera->height == 0)
        camera->height = 1944;

    return TRUE;
    error_s:
        if (camera_info)
            mmal_component_destroy(camera_info);
        return FALSE;
}

MMAL_FOURCC_T _RaspiCam_getFormat(unsigned char format){
    switch(format){
        case FORMAT_RGB:
            return MMAL_ENCODING_RGB24;
        case FORMAT_BGR:
            return MMAL_ENCODING_BGR24;
        case FORMAT_GRAY:
            return MMAL_ENCODING_I420;
        case FORMAT_YUV420:
            return MMAL_ENCODING_I420;
   }
   return -1;
}

void _RaspiCam_buffer_callback(MMAL_PORT_T *port, MMAL_BUFFER_HEADER_T *buffer){
    PORT_USERDATA *pData = (PORT_USERDATA *)port->userdata;
    RASPICAM_BOOLEAN hasGrabbed = FALSE;

    if(pData){
        if(pData->wantToGrab && buffer->length){
            mmal_buffer_header_mem_lock(buffer);
            pData->buffer_data = (uint8_t *)malloc(buffer->length);
            memcpy(pData->buffer_data, buffer->data, buffer->length);
            pData->buffer_length = buffer->length;

            pData->wantToGrab = FALSE;
            hasGrabbed = TRUE;

            mmal_buffer_header_mem_unlock(buffer);

            //if (buffer->flags & (MMAL_BUFFER_HEADER_FLAG_FRAME_END | MMAL_BUFFER_HEADER_FLAG_TRANSMISSION_FAILED))
            //    hasGrabbed = TRUE;
        }
    }else{
        printf("Received a encoder buffer callback with no state\n");
    }

    mmal_buffer_header_release(buffer);

    if (port->is_enabled){
        MMAL_STATUS_T status;
        MMAL_BUFFER_HEADER_T *new_buffer;

        new_buffer = mmal_queue_get(pData->pool->queue);

        if (new_buffer){
            status = mmal_port_send_buffer(port, new_buffer);
        }
        if (!new_buffer || status != MMAL_SUCCESS)
            printf("Unable to return a buffer to the encoder port\n");
   }

    if(hasGrabbed)
        vcos_semaphore_post(&pData->complete_semaphore);
}

RASPICAM_BOOLEAN _RaspiCam_create_camera(RASPICAM_CAMERA *camera){
    MMAL_STATUS_T status = MMAL_SUCCESS;

    camera->component = NULL;
    status = mmal_component_create(MMAL_COMPONENT_DEFAULT_CAMERA, &camera->component);
    
    // init del componente de la camara
    if (status != MMAL_SUCCESS){
      printf("Failed to create camera component\n");
      goto error;
   }

   // paramsscasc

   // init de los modos stereo
   status = raspicamcontrol_set_stereo_mode(camera->component->output[0], &camera->params->stereo_mode);
   status += raspicamcontrol_set_stereo_mode(camera->component->output[1], &camera->params->stereo_mode);
   status += raspicamcontrol_set_stereo_mode(camera->component->output[2], &camera->params->stereo_mode);

   if (status != MMAL_SUCCESS){
      printf("Could not set stereo mode : error %d", status);
      goto error;
   }

   if (!camera->component->output_num){
      status = MMAL_ENOSYS;
      printf("Camera doesn't have output ports\n");
      goto error;
   }

   camera->port = camera->component->output[MMAL_CAMERA_VIDEO_PORT];

   /*MMAL_PARAMETER_CHANGE_EVENT_REQUEST_T change_event_request =
         {{MMAL_PARAMETER_CHANGE_EVENT_REQUEST, sizeof(MMAL_PARAMETER_CHANGE_EVENT_REQUEST_T)},
          MMAL_PARAMETER_CAMERA_SETTINGS, 1};

    status = mmal_port_parameter_set(camera->component->control, &change_event_request.hdr);
    if ( status != MMAL_SUCCESS ){
         printf("No camera settings events\n");
    }*/

   {
       MMAL_PARAMETER_CAMERA_CONFIG_T config =
       {
            /* header */
            {MMAL_PARAMETER_CAMERA_CONFIG, sizeof(config)},
            .max_stills_w = camera->width,
            .max_stills_h = camera->height,
            .stills_yuv422 = 0,
            .one_shot_stills = 0,
            .max_preview_video_w = camera->width,
            .max_preview_video_h = camera->height,
            .num_preview_video_frames = 3,
            .stills_capture_circular_buffer_height = 0,
            .fast_preview_resume = 0,
            .use_stc_timestamp = MMAL_PARAM_TIMESTAMP_MODE_RESET_STC,
       };

       mmal_port_parameter_set(camera->component->control, &config.hdr);
    }
    raspicamcontrol_set_all_parameters(camera->component, camera->params);

    // Now set up the port formats
   // Set our stills format on the stills (for encoder) port
   MMAL_ES_FORMAT_T * format = camera->port->format;
   //format->encoding_variant =   MMAL_ENCODING_RGB24;
   //format->encoding = MMAL_ENCODING_RGB24;

   format->encoding_variant = _RaspiCam_getFormat(camera->format);
   format->encoding = _RaspiCam_getFormat(camera->format);
   if(format->encoding == -1){
        printf("not reconized format\n");
        goto error;
   }

   format->es->video.width = camera->width;
   format->es->video.height = camera->height;
   format->es->video.crop.x = 0;
   format->es->video.crop.y = 0;
   format->es->video.crop.width = camera->width;
   format->es->video.crop.height = camera->height;
   format->es->video.frame_rate.num = camera->framerate;
   format->es->video.frame_rate.den = STILLS_FRAME_RATE_DEN;

   status = mmal_port_format_commit(camera->port);

   if (status != MMAL_SUCCESS){
      printf("camera format couldn't be set\n");
      goto error;
   }

   /*status = mmal_port_enable(camera->port, _RaspiCam_buffer_callback);
   if (status != MMAL_SUCCESS){
      printf("camera callback error\n");
      goto error;
   }*/

   if (camera->port->buffer_num < VIDEO_OUTPUT_BUFFERS_NUM)
       camera->port->buffer_num = VIDEO_OUTPUT_BUFFERS_NUM;

   camera->port->buffer_size = camera->port->buffer_size_recommended;
   camera->port->buffer_num = camera->port->buffer_num_recommended;
   camera->pool = mmal_port_pool_create(camera->port, camera->port->buffer_num, camera->port->buffer_size);

   if (!camera->pool) {
       printf("failed to create buffer header pool for camera\n");
       goto error;
   }

   /* Enable component */
    status = mmal_component_enable(camera->component);
    if (status != MMAL_SUCCESS){
        printf("camera component couldn't be enabled\n");
        goto error;
    }

    camera->callback_data = NULL;

    

    return TRUE;
    
    error:
        if (camera->component)
            mmal_component_destroy(camera->component);
        return FALSE;
}

RASPICAM_BOOLEAN _RaspiCam_create(RASPICAM_CAMERA *camera) {
    // SENSOR
    if(!_RaspiCam_create_sensor(camera))
        return FALSE;

    //printf("W: %d, H: %d", camera->width, camera->height);
    
    // CAMERA
    if(!_RaspiCam_create_camera(camera))
        return FALSE;

    return TRUE;
}

RASPICAM_CAMERA *newRaspiCam(){
    bcm_host_init();
    //printf("Init\n");
    RASPICAM_CAMERA *camera = (RASPICAM_CAMERA *)malloc(sizeof(RASPICAM_CAMERA));
    
    camera->params = NULL;
    camera->format = FORMAT_RGB;
    camera->component = NULL;
    camera->port = NULL;
    camera->pool = NULL;
    camera->callback_data = NULL;
    camera->width = 1280;
    camera->height = 960;
    camera->num = 0;
    camera->framerate = 30;

    camera->_isOpened = FALSE;
    camera->_isCapturing = FALSE;

    camera->params = (RASPICAM_CAMERA_PARAMETERS *)malloc(sizeof(RASPICAM_CAMERA_PARAMETERS));
    raspicamcontrol_set_defaults(camera->params);

    return camera;
}

void RaspiCam_release(RASPICAM_CAMERA *camera){
    if(!camera->_isOpened) return;

    if(camera->port && camera->port->is_enabled){
        mmal_port_disable(camera->port);
        camera->port = NULL;
    }

    if(camera->component){
        mmal_component_disable(camera->component);
        camera->component = NULL;
    }

    if (camera->pool)
        mmal_port_pool_destroy(camera->component->output[MMAL_CAMERA_VIDEO_PORT], camera->pool);
    
    if (camera->component) {
        mmal_component_destroy(camera->component);
        camera->component = NULL;
    }

    if(camera->callback_data != NULL){
        vcos_semaphore_delete(&camera->callback_data->complete_semaphore);
    }
}

void deleteRaspiCam(RASPICAM_CAMERA *camera){
    return RaspiCam_release(camera);
}

void _RaspiCam_enable_camera_port(RASPICAM_CAMERA *camera){
    if (mmal_port_enable(camera->port, _RaspiCam_buffer_callback) != MMAL_SUCCESS){
      printf("camera callback error\n");
    }

    if(mmal_port_parameter_set_boolean(camera->port, MMAL_PARAMETER_CAPTURE, 1) != MMAL_SUCCESS){
        RaspiCam_release(camera);
        return FALSE;
    }

    int num = mmal_queue_length(camera->pool->queue);
    int q;
    for(q = 0; q < num; q++) {
        MMAL_BUFFER_HEADER_T *buffer = mmal_queue_get(camera->pool->queue);
        if (!buffer)
            printf("Unable to get a required buffer %d from pool queue", q);
            
        if (mmal_port_send_buffer(camera->port, buffer) != MMAL_SUCCESS) // Punto de excepcion:  Unitialized mutex in call to pthread_mutex_lock
            printf("Unable to send to the output port\n");
    }
}

RASPICAM_BOOLEAN RaspiCam_startCapture(RASPICAM_CAMERA *camera){
    //printf("Start capture\n");
    //printf("Start capture\n");
    if(!camera->_isOpened){
        printf("camera not open.\n");
        return FALSE;
    }

    _RaspiCam_enable_camera_port(camera);
    

    camera->_isCapturing = TRUE;
    //printf("End capture\n");
    //printf("End capture\n");
    return TRUE;   
}

RASPICAM_BOOLEAN RaspiCam_open(RASPICAM_CAMERA *camera, RASPICAM_BOOLEAN startCapture){
    if(camera->_isOpened)
        return FALSE; // already open
    
    if(!_RaspiCam_create(camera))
        return FALSE;

    PORT_USERDATA *callback_data = (PORT_USERDATA *)malloc(sizeof(PORT_USERDATA));
    callback_data->buffer_length = 0;
    callback_data->buffer_data = NULL;
    callback_data->port = camera->port;
    callback_data->pool = camera->pool;
    callback_data->wantToGrab = FALSE;

    VCOS_STATUS_T vcos_status = vcos_semaphore_create(&callback_data->complete_semaphore, "RaspiCamWrapper-semaphore", 0);
    vcos_assert(vcos_status == VCOS_SUCCESS);

    camera->callback_data = callback_data;

    camera->port->userdata = (struct MMAL_PORT_USERDATA_T *)camera->callback_data;
    camera->_isOpened = TRUE;
    
    if(startCapture)//StartCapture)
        return RaspiCam_startCapture(camera);
    return TRUE;
}

RASPICAM_BOOLEAN RaspiCam_grab(RASPICAM_CAMERA *camera){
    if (!camera->_isCapturing)
        return FALSE;

    //if(!camera->port->is_enabled){
    //    _RaspiCam_enable_camera_port(camera);
    //}
    camera->callback_data->wantToGrab = TRUE;
    vcos_semaphore_wait(&((PORT_USERDATA *)camera->callback_data)->complete_semaphore);
    return TRUE;

}

uint32_t RaspiCam_getImageTypeSize(RASPICAM_CAMERA *camera){
    switch (camera->format) {
        case FORMAT_YUV420:
            return (uint32_t)(camera->width*camera->height + 2* ( ( camera->width /2 *camera->height /2 ) ));
        case FORMAT_GRAY:
            return (uint32_t)(camera->width*camera->height);
        case FORMAT_RGB:
        case FORMAT_BGR:
            return (uint32_t)(3*camera->width*camera->height);
        default:
            return 0;
    };
}

RASPICAM_IMAGE *RaspiCam_retrieve(RASPICAM_CAMERA *camera){
    //if(camera->port->is_enabled){
    //    mmal_port_disable(camera->port);
    //}

    if(camera->callback_data == NULL || camera->callback_data->buffer_length == 0)
        return NULL;
    //printf("A\n");
    RASPICAM_IMAGE *image = (RASPICAM_IMAGE *)malloc(sizeof(RASPICAM_IMAGE));
    //printf("B\n");
    image->length = RaspiCam_getImageTypeSize(camera);
    //printf("C\n");
    //printf("E\n");
    image->data = (uint8_t *)malloc(image->length);
    image->format = camera->format;
    image->width = camera->width;
    image->height = camera->height;
    //printf("F\n");
    memcpy(image->data, camera->callback_data->buffer_data, image->length);
    //printf("G\n");
    free(camera->callback_data->buffer_data);
    camera->callback_data->buffer_data = NULL;
    camera->callback_data->buffer_length = 0;
    
    return image;
}

RASPICAM_IMAGE *RaspiCam_getImage(RASPICAM_CAMERA *camera){
    RASPICAM_IMAGE *image = NULL;
    if(RaspiCam_grab(camera)){
        image = RaspiCam_retrieve(camera);
    } else {
        printf("Error in grab camera.\n");
    }
    return image;
}

/* ENCODER */
const char *_RaspiCam_obtainExtension(const char *filename){
    char *i;
    for(i = (char *)((unsigned long)filename + strlen(filename)-1); (*i) != '.'; i--){}
    if(*i == '.')
        return i + 1;
    return NULL;
}

MMAL_FOURCC_T _RaspiCam_checkExtension(const char *filename){
    const char *ext = _RaspiCam_obtainExtension(filename);
    if(ext == 0)
        return ENCODING_ERROR;
    if(strcmp(ext, "png") == 0)
        return MMAL_ENCODING_PNG;
    else if(strcmp(ext, "jpeg") == 0)
        return MMAL_ENCODING_JPEG;
    else if(strcmp(ext, "jpg") == 0)
        return MMAL_ENCODING_JPEG;
    else if(strcmp(ext, "gif") == 0)
        return MMAL_ENCODING_GIF;
    else if(strcmp(ext, "bmp") == 0)
        return MMAL_ENCODING_BMP;
    else if(strcmp(ext, "raw") == 0)
        return MMAL_ENCODING_BMP;
    else if(strcmp(ext, "ppm") == 0)
        return MMAL_ENCODING_PPM;
    else if(strcmp(ext, "tca") == 0)
        return MMAL_ENCODING_TGA;
    return ENCODING_ERROR;
}

void _RaspiCam_encoder_callback(MMAL_WRAPPER_T* encoder){
    printf("callback encoder\n");
    PORT_USERDATA_ENCODER* pData = (PORT_USERDATA_ENCODER *)encoder->user_data;
    vcos_semaphore_post(&pData->complete_semaphore);
}

RASPICAM_BOOLEAN _RaspiCam_createImageWithEncoder(MMAL_WRAPPER_T *encoder, RASPICAM_IMAGE *image, const char *filename, MMAL_FOURCC_T encoding){
    MMAL_PORT_T* portIn;
    MMAL_PORT_T* portOut;
    MMAL_BUFFER_HEADER_T* in;
    MMAL_BUFFER_HEADER_T* out;
    MMAL_STATUS_T status;
    RASPICAM_BOOLEAN eos = FALSE;
    RASPICAM_BOOLEAN isSend = FALSE;
    int outputWritten = 0;
    FILE* outFile;
    int nw;

    // Configure input
    portIn = encoder->input[0];
    encoder->status = MMAL_SUCCESS;

    if(portIn->is_enabled) {
        if(mmal_wrapper_port_disable(portIn) != MMAL_SUCCESS) {
            printf("Failed to disable input port\n");
            goto error;
        }
    }
    printf("S2\n");

    portIn->format->encoding = _RaspiCam_getFormat(image->format);
    portIn->format->es->video.width = VCOS_ALIGN_UP(image->width, 32);
    portIn->format->es->video.height = VCOS_ALIGN_UP(image->height, 16);
    portIn->format->es->video.crop.x = 0;
    portIn->format->es->video.crop.y = 0;
    portIn->format->es->video.crop.width = image->width;
    portIn->format->es->video.crop.height = image->height;
    if(mmal_port_format_commit(portIn) != MMAL_SUCCESS) {
        printf("Failed to commit input port format\n");
        goto error;
    }

    portIn->buffer_size = portIn->buffer_size_recommended;
    portIn->buffer_num = portIn->buffer_num_recommended;

    if(mmal_wrapper_port_enable(portIn, MMAL_WRAPPER_FLAG_PAYLOAD_ALLOCATE) != MMAL_SUCCESS) {
        printf("Failed to enable input port\n");
        goto error;
    }
    printf("S3\n");

    // Configure output
    portOut = encoder->output[0];

    if(portOut->is_enabled) {
        if(mmal_wrapper_port_disable(portOut) != MMAL_SUCCESS) {
            printf("Failed to disable output port\n");
            goto error;
        }
    }

    portOut->format->encoding = encoding;
    if(mmal_port_format_commit(portOut) != MMAL_SUCCESS) {
        printf("Failed to commit output port format\n");
        goto error;
    }

    mmal_port_parameter_set_uint32(portOut, MMAL_PARAMETER_JPEG_Q_FACTOR, 100);
  
    portOut->buffer_size = portOut->buffer_size_recommended;
    portOut->buffer_num = portOut->buffer_num_recommended;

    if (mmal_wrapper_port_enable(portOut, MMAL_WRAPPER_FLAG_PAYLOAD_ALLOCATE) != MMAL_SUCCESS) {
        printf("Failed to enable output port\n");
        goto error;
    }
    printf("S4\n");

    // Perform the encoding
    outFile = fopen(filename, "wb");
    printf("S4.a\n");
    printf("%s\n", filename);
    printf("S4.b\n");
    if(outFile == NULL) {
        printf("Failed to open file %s (%s)", filename, strerror(errno));
        goto error;
    }
    printf("S5\n");
    while(!eos) {
        printf("S6\n");
        // Send output buffers to be filled with encoded image.
        while(mmal_wrapper_buffer_get_empty(portOut, &out, 0) == MMAL_SUCCESS) {
            if(mmal_port_send_buffer(portOut, out) != MMAL_SUCCESS) {
                printf("Failed to send buffer\n");
                break;
            }
        }

        // Send image to be encoded.
        if(!isSend && mmal_wrapper_buffer_get_empty(portIn, &in, 0) == MMAL_SUCCESS) {
            memcpy(in->data, image->data, image->length);
            in->length = in->alloc_size;
            in->flags = MMAL_BUFFER_HEADER_FLAG_EOS;
            if(mmal_port_send_buffer(portIn, in) != MMAL_SUCCESS) {
                printf("Failed to send buffer\n");
                break;
            }
            isSend = TRUE;
        }

        // Get filled output buffers.
        status = mmal_wrapper_buffer_get_full(portOut, &out, 0);
        if(status == MMAL_EAGAIN) {
            // No buffer available, wait for callback and loop.
            vcos_semaphore_wait(&((PORT_USERDATA_ENCODER *)encoder->user_data)->complete_semaphore);
            continue;

        } else if (status != MMAL_SUCCESS) {
            printf("Failed to get full buffer\n");
            goto error;
        }

        eos = out->flags & MMAL_BUFFER_HEADER_FLAG_EOS;

        nw = fwrite(out->data, 1, out->length, outFile);
        
        if(nw != out->length) {
            printf("Failed to write complete buffer\n");
            goto error;
        }
    
        outputWritten += nw;
    
        mmal_buffer_header_release(out);
    }

    mmal_port_flush(portOut);

    fclose(outFile);

    return TRUE;

    error:
        return FALSE;
}

RASPICAM_BOOLEAN RaspiCam_save(RASPICAM_IMAGE *image, const char *filename){
    printf("length: %d, width: %d, height: %d\n", image->length, image->width, image->height);
    //printf("IN SAVE\n");
    MMAL_WRAPPER_T *encoder;
    MMAL_FOURCC_T extension = _RaspiCam_checkExtension(filename);
    if(extension == ENCODING_ERROR){
        printf("Failed to use the extension\n");
        goto error;
    }

    PORT_USERDATA_ENCODER *callback_data_encoder = (PORT_USERDATA_ENCODER *)malloc(sizeof(PORT_USERDATA_ENCODER));
    if (vcos_semaphore_create(&callback_data_encoder->complete_semaphore, "encoder sem", 0) != VCOS_SUCCESS) {
        printf("Failed to create semaphore\n");
        goto error;
    }
    if (mmal_wrapper_create(&encoder, MMAL_COMPONENT_DEFAULT_IMAGE_ENCODER) != MMAL_SUCCESS) {
        printf("Failed to create mmal component\n");
        goto error;
    }
    encoder->user_data = callback_data_encoder;
    encoder->callback = _RaspiCam_encoder_callback;
  
    // Perform test encodings in various formats
    printf("S1\n");
    if(!_RaspiCam_createImageWithEncoder(encoder, image, filename, extension)){
        printf("Error creating the image\n");
        goto error;
    }

    //printf("OUT SAVE\n");
    mmal_wrapper_destroy(encoder);
    vcos_semaphore_delete(&callback_data_encoder->complete_semaphore);

    return TRUE;

    error:
        if(encoder)
            mmal_wrapper_destroy(encoder);
        vcos_semaphore_delete(&callback_data_encoder->complete_semaphore);
        return FALSE;
}


// FUNCTIONS GET/SET
RASPICAM_BOOLEAN _RaspiCam_clamp(int param, int minv, int maxv){
    return (param >= minv && param <= maxv);
}

RASPICAM_BOOLEAN RaspiCam_setSharpness(RASPICAM_CAMERA *camera, int8_t param){
    if(_RaspiCam_clamp(param, -100, 100)){
        camera->params->sharpness = param;
    }else{
        camera->params->sharpness = 0;
    }
    return raspicamcontrol_set_sharpness(camera->component, camera->params->sharpness);
}

int8_t RaspiCam_getSharpness(RASPICAM_CAMERA *camera){
    return (int8_t)camera->params->sharpness;
}

RASPICAM_BOOLEAN RaspiCam_setContrast(RASPICAM_CAMERA *camera, int8_t param){
    if(_RaspiCam_clamp(param, -100, 100)){
        camera->params->contrast = param;
    }else{
        camera->params->contrast = 0;
    }
    return raspicamcontrol_set_contrast(camera->component, camera->params->contrast);
}

int8_t RaspiCam_getContrast(RASPICAM_CAMERA *camera){
    return (int8_t)camera->params->contrast;
}

RASPICAM_BOOLEAN RaspiCam_setBrightness(RASPICAM_CAMERA *camera, uint8_t param){
    if(_RaspiCam_clamp(param, 0, 100)){
        camera->params->brightness = param;
    }else{
        camera->params->brightness = 50;
    }
    return raspicamcontrol_set_brightness(camera->component, camera->params->brightness);
}

uint8_t RaspiCam_getBrightness(RASPICAM_CAMERA *camera){
    return camera->params->brightness;
}

RASPICAM_BOOLEAN RaspiCam_setSaturation(RASPICAM_CAMERA *camera, int8_t param){
    if(_RaspiCam_clamp(param, -100, 100)){
        camera->params->saturation = param;
    }else{
        camera->params->saturation = 0;
    }
    return raspicamcontrol_set_saturation(camera->component, camera->params->saturation);
}

int8_t RaspiCam_getSaturation(RASPICAM_CAMERA *camera){
    return camera->params->saturation;
}

RASPICAM_BOOLEAN RaspiCam_setISO(RASPICAM_CAMERA *camera, int32_t param){
    camera->params->ISO = param;
    return raspicamcontrol_set_ISO(camera, camera->params->ISO);
}

int32_t RaspiCam_getISO(RASPICAM_CAMERA *camera){
    return camera->params->ISO;
}

RASPICAM_BOOLEAN RaspiCam_setVideoStab(RASPICAM_CAMERA *camera, RASPICAM_BOOLEAN param){
    if(param){
        camera->params->videoStabilisation = TRUE;
    }else{
        camera->params->videoStabilisation = FALSE;
    }  
    return raspicamcontrol_set_video_stabilisation(camera->component, camera->params->videoStabilisation);
}

RASPICAM_BOOLEAN RaspiCam_getVideoStab(RASPICAM_CAMERA *camera){
    return (RASPICAM_BOOLEAN)camera->params->videoStabilisation;
}

RASPICAM_BOOLEAN RaspiCam_setEVComp(RASPICAM_CAMERA *camera, uint8_t param){
    if(_RaspiCam_clamp(param, -10, 10)){
        camera->params->exposureCompensation = param;
    }else{
        camera->params->exposureCompensation = 0;
    }
    return raspicamcontrol_set_exposure_compensation(camera->component, camera->params->exposureCompensation);
}

uint8_t RaspiCam_getEVComp(RASPICAM_CAMERA *camera){
    return camera->params->exposureCompensation;
}

RASPICAM_BOOLEAN RaspiCam_setExposure(RASPICAM_CAMERA *camera, int32_t param){
    if(_RaspiCam_clamp(param, 0, exposure_map_size)){
        camera->params->exposureMode = exposure_map[param].mmal_mode;
    }else{
        camera->params->exposureMode = MMAL_PARAM_EXPOSUREMODE_AUTO;
    }
    return raspicamcontrol_set_exposure_mode(camera->component, camera->params->exposureMode);
}

int32_t RaspiCam_getExposure(RASPICAM_CAMERA *camera){
    return (int32_t)camera->params->exposureMode;
}

RASPICAM_BOOLEAN RaspiCam_setFlickerAvoid(RASPICAM_CAMERA *camera, int32_t param){
    if(_RaspiCam_clamp(param, 0, flicker_avoid_map_size)){
        camera->params->flickerAvoidMode = flicker_avoid_map[param].mmal_mode;
    }else{
        camera->params->flickerAvoidMode = MMAL_PARAM_FLICKERAVOID_AUTO;
    }
    return raspicamcontrol_set_flicker_avoid_mode(camera->component, camera->params->flickerAvoidMode);
}

int32_t RaspiCam_getFlickerAvoid(RASPICAM_CAMERA *camera){
    return (int32_t)camera->params->flickerAvoidMode;
}

RASPICAM_BOOLEAN RaspiCam_setAWB(RASPICAM_CAMERA *camera, int32_t param){
     if(_RaspiCam_clamp(param, 0, awb_map_size)){
        camera->params->awbMode = awb_map[param].mmal_mode;
    }else{
        camera->params->awbMode = MMAL_PARAM_AWBMODE_AUTO;
    }
    return raspicamcontrol_set_awb_mode(camera->component, camera->params->awbMode);
}

int32_t RaspiCam_getAWB(RASPICAM_CAMERA *camera){
    return (int32_t)camera->params->awbMode;
}

RASPICAM_BOOLEAN RaspiCam_setImageFX(RASPICAM_CAMERA *camera, int32_t param){
    if(_RaspiCam_clamp(param, 0, imagefx_map_size)){
        camera->params->imageEffect = imagefx_map[param].mmal_mode;
    }else{
        camera->params->imageEffect = MMAL_PARAM_IMAGEFX_NONE;
    }
    return raspicamcontrol_set_imageFX(camera->component, camera->params->imageEffect);
}

int32_t RaspiCam_getImageFX(RASPICAM_CAMERA *camera){
    return (int32_t)camera->params->imageEffect;
}

RASPICAM_BOOLEAN RaspiCam_setColorFX(RASPICAM_CAMERA *camera, RASPICAM_COLORFX colorfx){
    MMAL_PARAM_COLOURFX_T param = {colorfx.enable, colorfx.u, colorfx.v};
    camera->params->colourEffects = param;
    return raspicamcontrol_set_colourFX(camera->component, &camera->params->colourEffects);
}

RASPICAM_COLORFX *RaspiCam_getColorFX(RASPICAM_CAMERA *camera){
    RASPICAM_COLORFX *ret = (RASPICAM_COLORFX *)malloc(sizeof(RASPICAM_COLORFX));
    ret->enable = camera->params->colourEffects.enable;
    ret->u = camera->params->colourEffects.u;
    ret->v = camera->params->colourEffects.v;
    return ret;
}

RASPICAM_BOOLEAN RaspiCam_setMeterMode(RASPICAM_CAMERA *camera, int32_t param){
    if(_RaspiCam_clamp(param, 0, metering_mode_map_size)){
        camera->params->exposureMeterMode = metering_mode_map[param].mmal_mode;
    }else{
        camera->params->exposureMeterMode = MMAL_PARAM_EXPOSUREMETERINGMODE_AVERAGE;
    }
    return raspicamcontrol_set_metering_mode(camera->component, camera->params->exposureMeterMode);
}

int32_t RaspiCam_getMeterMode(RASPICAM_CAMERA *camera){
    return (int32_t)camera->params->exposureMeterMode;
}

RASPICAM_BOOLEAN RaspiCam_setRotation(RASPICAM_CAMERA *camera, int16_t param){
    if(param < 0){
        param = 360 - param;
    }
    param = ((param % 360 ) / 90) * 90;
    camera->params->rotation = param;
    return raspicamcontrol_set_rotation(camera, camera->params->rotation);
}

int16_t RaspiCam_getRotation(RASPICAM_CAMERA *camera){
    return camera->params->rotation;
}

RASPICAM_BOOLEAN RaspiCam_setFlip(RASPICAM_CAMERA *camera, RASPICAM_BOOLEAN horizontal, RASPICAM_BOOLEAN vertical){
    camera->params->hflip = horizontal > 0;
    camera->params->vflip = vertical > 0;
    return raspicamcontrol_set_flips(camera->component, camera->params->hflip, camera->params->vflip);
}

RASPICAM_BOOLEAN RaspiCam_getHorizontalFlip(RASPICAM_CAMERA *camera){
    return (RASPICAM_BOOLEAN)camera->params->hflip;
}

RASPICAM_BOOLEAN RaspiCam_getVerticalFlip(RASPICAM_CAMERA *camera){
    return (RASPICAM_BOOLEAN)camera->params->hflip;
}


RASPICAM_BOOLEAN RaspiCam_setROI(RASPICAM_CAMERA *camera, RASPICAM_RECT rect){
    PARAM_FLOAT_RECT_T roi;
    if(_RaspiCam_clamp(rect.x, 0, 1) && _RaspiCam_clamp(rect.y, 0, 1)
        && _RaspiCam_clamp(rect.width, 0, 1) && _RaspiCam_clamp(rect.height, 0, 1)){
        roi.x = rect.x;
        roi.y = rect.y;
        roi.w = rect.width;
        roi.h = rect.height;
    }else{
        roi.x = 0;
        roi.y = 1;
        roi.w = 1;
        roi.h = 1;
    } 
    camera->params->roi = roi;
    return raspicamcontrol_set_ROI(camera->component, camera->params->roi);
}

RASPICAM_RECT *RaspiCam_getROI(RASPICAM_CAMERA *camera){
    RASPICAM_RECT *ret = (RASPICAM_RECT *)malloc(sizeof(RASPICAM_RECT));
    ret->x = camera->params->roi.x;
    ret->y = camera->params->roi.y;
    ret->width = camera->params->roi.w;
    ret->height = camera->params->roi.h;
    return ret;
}

RASPICAM_BOOLEAN RaspiCam_setShutterSpeed(RASPICAM_CAMERA *camera, int32_t param){
    camera->params->shutter_speed = param;
    return raspicamcontrol_set_shutter_speed(camera->component, camera->params->shutter_speed);
}

int32_t RaspiCam_getShutterSpeed(RASPICAM_CAMERA *camera){
    return camera->params->shutter_speed;
}

RASPICAM_BOOLEAN RaspiCam_setAwbGains(RASPICAM_CAMERA *camera, float awb_gains_r, float awb_gains_b){
    if(_RaspiCam_clamp(awb_gains_r, 0, 1))
        camera->params->awb_gains_r = awb_gains_r;
    else
        camera->params->awb_gains_r = 0;

    if(_RaspiCam_clamp(awb_gains_b, 0, 1))
        camera->params->awb_gains_b = awb_gains_b;
    else
        camera->params->awb_gains_b = 0;
    return raspicamcontrol_set_awb_gains(camera->component, camera->params->awb_gains_r, camera->params->awb_gains_b);
}

float RaspiCam_getAwbGainR(RASPICAM_CAMERA *camera){
    return camera->params->awb_gains_r;
}

float RaspiCam_getAwbGainB(RASPICAM_CAMERA *camera){
    return camera->params->awb_gains_b;
}

RASPICAM_BOOLEAN RaspiCam_setDRCLevel(RASPICAM_CAMERA *camera, int32_t param){
    if(_RaspiCam_clamp(param, 0, drc_mode_map_size)){
        camera->params->drc_level = drc_mode_map[param].mmal_mode;
    }else{
        camera->params->drc_level = MMAL_PARAMETER_DRC_STRENGTH_OFF;
    }
    return raspicamcontrol_set_DRC(camera->component, camera->params->drc_level);
}

int32_t RaspiCam_getDRCLevel(RASPICAM_CAMERA *camera){
    return (int32_t)camera->params->drc_level;
}

RASPICAM_BOOLEAN RaspiCam_setStatsPass(RASPICAM_CAMERA *camera, RASPICAM_BOOLEAN param){
    camera->params->stats_pass = param > 0;
    return raspicamcontrol_set_metering_mode(camera->component, camera->params->stats_pass);
}

RASPICAM_BOOLEAN RaspiCam_getStatsPass(RASPICAM_CAMERA *camera){
    return camera->params->stats_pass;
}

int _RaspiCam_RGB2HEXYUV(uint8_t R, uint8_t G, uint8_t B){
    int Y =  (0.257 * R) + (0.504 * G) + (0.098 * B) + 16;
    int V =  (0.439 * R) - (0.368 * G) - (0.071 * B) + 128;
    int U = -(0.148 * R) - (0.291 * G) + (0.439 * B) + 128;
    return (0xFF & Y) | ((0xFF & U) << 8) | (( 0xFF & V) << 16);
}

RASPICAM_BOOLEAN RaspiCam_setAnnotate(RASPICAM_CAMERA *camera, RASPICAM_ANNOTATE annotate){
    camera->params->enable_annotate = annotate.enable;
    strcpy(camera->params->annotate_string, annotate.string);
    camera->params->annotate_text_size = annotate.size;
    camera->params->annotate_text_colour = _RaspiCam_RGB2HEXYUV(annotate.color.r, annotate.color.g, annotate.color.b);
    camera->params->annotate_bg_colour = _RaspiCam_RGB2HEXYUV(annotate.bg_color.r, annotate.bg_color.g, annotate.bg_color.b);

    return raspicamcontrol_set_annotate(camera->component, camera->params->enable_annotate,
        camera->params->annotate_string, camera->params->annotate_text_size,
        camera->params->annotate_text_colour, camera->params->annotate_bg_colour);
}

void _RaspiCam_HEXYUV2RGB(int hexyuv, uint8_t *r, uint8_t *g, uint8_t *b){
    int y = hexyuv & 0xff;
    int cr = (hexyuv >> 8) & 0xff;
    int cb = (hexyuv >> 16) & 0xff;

    double r_aux, g_aux, b_aux;
    r_aux = y + (1.4065 * (cr - 128));
    g_aux = y - (0.3455 * (cb - 128)) - (0.7169 * (cr - 128));
    b_aux = y + (1.7790 * (cb - 128));

    //This prevents colour distortions in your rgb image
    if (r_aux < 0) r_aux = 0;
    else if (r_aux > 255) r_aux = 255;
    if (g_aux < 0) g_aux = 0;
    else if (g_aux > 255) g_aux = 255;
    if (b_aux < 0) b_aux = 0;
    else if (b_aux > 255) b_aux = 255;

    *r = (uint8_t)r_aux;
    *g = (uint8_t)g_aux;
    *b = (uint8_t)b_aux;
}

RASPICAM_ANNOTATE RaspiCam_getAnnotate(RASPICAM_CAMERA *camera){
    RASPICAM_ANNOTATE *ret = (RASPICAM_ANNOTATE *)malloc(sizeof(RASPICAM_ANNOTATE));
    ret->string = camera->params->annotate_string;
    ret->size = (uint8_t)camera->params->annotate_text_size;
    _RaspiCam_HEXYUV2RGB(camera->params->annotate_text_colour, &ret->color.r, &ret->color.g, &ret->color.b);
    _RaspiCam_HEXYUV2RGB(camera->params->annotate_bg_colour, &ret->bg_color.r, &ret->bg_color.g, &ret->bg_color.b);
    return *ret;
}

/*uint8_t RaspiCam_getAnnotateTextSize(RASPICAM_CAMERA *camera){
    return (uint8_t)camera->params->annotate_text_size;
}

uint8_t RaspiCam_getAnnotateTextColor(RASPICAM_CAMERA *camera){
    return (uint8_t)camera->params->annotate_text_colour;
}

uint8_t RaspiCam_getAnnotateBgColor(RASPICAM_CAMERA *camera){
    return (uint8_t)camera->params->annotate_bg_colour;
}*/

/*RASPICAM_BOOLEAN RaspiCam_setStereoMode(RASPICAM_CAMERA *camera, int param){
    if(_RaspiCam_clamp(param, 0, stereo_mode_map_size)){
        camera->params->stereo_mode = stereo_mode_map[param].mmal_mode;
    }else{
        camera->params->stereo_mode = MMAL_STEREOSCOPIC_MODE_NONE;
    }
    raspicamcontrol_set_stereo_mode(camera->component->output[0], &camera->params->stereo_mode);
    raspicamcontrol_set_stereo_mode(camera->component->output[1], &camera->params->stereo_mode);
    raspicamcontrol_set_stereo_mode(camera->component->output[2], &camera->params->stereo_mode);

}

MMAL_PARAMETER_STEREOSCOPIC_MODE_T RaspiCam_getStereoMode(RASPICAM_CAMERA *camera){
    return camera->params->stereo_mode;
}*/

void RaspiCam_demo(){
    RASPICAM_CAMERA *camera = newRaspiCam();
    //camera->num = 0;
    //camera->format = FORMAT_RGB;
    RaspiCam_open(camera, TRUE);
    RaspiCam_grab(camera);
    RASPICAM_IMAGE *image = RaspiCam_retrieve(camera);
    printf("A\n");
    //printf("%d ", (int)data[i]);
    RaspiCam_save(image, "demo.jpeg");
    //free(image);
    printf("B\n");
}
