//
//  RaspiCamWrapper.c
//  
//
//  Created by Adrià on 9/10/17.
//
//

#include "RaspiCamWrapper.h"

BOOLEAN _RaspiCam_create_sensor(RASPICAM_CAMERA *camera){
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
                vcos_log_error("Cannot read camera info, keeping the defaults for OV5647");
                goto error_s;
            }
        }
        mmal_component_destroy(camera_info);
    }else{
        vcos_log_error("Failed to create camera_info component");
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
    printf("callback\n");
    PORT_USERDATA *pData = (PORT_USERDATA *)port->userdata;
    BOOLEAN hasGrabbed = FALSE;

    if(pData){
        if(pData->wantToGrab && buffer->length){
            mmal_buffer_header_mem_lock(buffer);
            pData->buffer_data = (uint8_t *)malloc(sizeof(uint8_t)*buffer->length);
            memcpy(pData->buffer_data, buffer->data, buffer->length);
            pData->buffer_length = buffer->length;

            pData->wantToGrab = FALSE;
            hasGrabbed = TRUE;

            mmal_buffer_header_mem_unlock(buffer);

            //if (buffer->flags & (MMAL_BUFFER_HEADER_FLAG_FRAME_END | MMAL_BUFFER_HEADER_FLAG_TRANSMISSION_FAILED))
            //    hasGrabbed = TRUE;
        }
    }else{
        vcos_log_error("Received a encoder buffer callback with no state");
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
            vcos_log_error("Unable to return a buffer to the encoder port");
   }


    if(hasGrabbed)
        vcos_semaphore_post(&pData->complete_semaphore);
}

BOOLEAN _RaspiCam_create_camera(RASPICAM_CAMERA *camera){
    MMAL_STATUS_T status = MMAL_SUCCESS;

    camera->component = NULL;
    status = mmal_component_create(MMAL_COMPONENT_DEFAULT_CAMERA, &camera->component);
    
    // init del componente de la camara
    if (status != MMAL_SUCCESS){
      vcos_log_error("Failed to create camera component");
      goto error;
   }

   // paramsscasc

   // init de los modos stereo
   status = raspicamcontrol_set_stereo_mode(camera->component->output[0], &camera->params->stereo_mode);
   status += raspicamcontrol_set_stereo_mode(camera->component->output[1], &camera->params->stereo_mode);
   status += raspicamcontrol_set_stereo_mode(camera->component->output[2], &camera->params->stereo_mode);

   if (status != MMAL_SUCCESS){
      vcos_log_error("Could not set stereo mode : error %d", status);
      goto error;
   }

   if (!camera->component->output_num){
      status = MMAL_ENOSYS;
      vcos_log_error("Camera doesn't have output ports");
      goto error;
   }

   camera->port = camera->component->output[MMAL_CAMERA_VIDEO_PORT];

   /*MMAL_PARAMETER_CHANGE_EVENT_REQUEST_T change_event_request =
         {{MMAL_PARAMETER_CHANGE_EVENT_REQUEST, sizeof(MMAL_PARAMETER_CHANGE_EVENT_REQUEST_T)},
          MMAL_PARAMETER_CAMERA_SETTINGS, 1};

    status = mmal_port_parameter_set(camera->component->control, &change_event_request.hdr);
    if ( status != MMAL_SUCCESS ){
         vcos_log_error("No camera settings events");
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
        vcos_log_error("not reconized format");
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
      vcos_log_error("camera format couldn't be set");
      goto error;
   }

   status = mmal_port_enable(camera->port, _RaspiCam_buffer_callback);
   if (status != MMAL_SUCCESS){
      vcos_log_error("camera callback error");
      goto error;
   }

   if (camera->port->buffer_num < VIDEO_OUTPUT_BUFFERS_NUM)
       camera->port->buffer_num = VIDEO_OUTPUT_BUFFERS_NUM;

   camera->port->buffer_size = camera->port->buffer_size_recommended;
   camera->port->buffer_num = camera->port->buffer_num_recommended;
   camera->pool = mmal_port_pool_create(camera->port, camera->port->buffer_num, camera->port->buffer_size);

   if (!camera->pool) {
       vcos_log_error("failed to create buffer header pool for camera");
       goto error;
   }

   /* Enable component */
    status = mmal_component_enable(camera->component);
    if (status != MMAL_SUCCESS){
        vcos_log_error("camera component couldn't be enabled");
        goto error;
    }

    camera->callback_data = NULL;

    

    return TRUE;
    
    error:
        if (camera->component)
            mmal_component_destroy(camera->component);
        return FALSE;
}

BOOLEAN _RaspiCam_create(RASPICAM_CAMERA *camera) {
    // SENSOR
    if(!_RaspiCam_create_sensor(camera))
        return FALSE;

    //vcos_log_error("W: %d, H: %d", camera->width, camera->height);
    
    // CAMERA
    if(!_RaspiCam_create_camera(camera))
        return FALSE;

    return TRUE;
}

RASPICAM_CAMERA *newRaspiCam(){
    bcm_host_init();
    vcos_log_error("Init\n");
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

BOOLEAN RaspiCam_startCapture(RASPICAM_CAMERA *camera){
    printf("Start capture\n");
    vcos_log_error("Start capture\n");
    if(!camera->_isOpened){
        vcos_log_error("camera not open.");
        return FALSE;
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
            printf("Unable to send to the output port");
    }

    camera->_isCapturing = TRUE;
    printf("End capture\n");
    vcos_log_error("End capture\n");
    return TRUE;   
}

BOOLEAN RaspiCam_open(RASPICAM_CAMERA *camera, BOOLEAN StartCapture){
    if(camera->_isOpened)
        return FALSE; // already open
    
    if(!_RaspiCam_create(camera))
        return FALSE;

    //camera->port = camera->component->output[MMAL_CAMERA_VIDEO_PORT];

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
    
    if(StartCapture)//StartCapture)
        return RaspiCam_startCapture(camera);
    return TRUE;
};

BOOLEAN RaspiCam_grab(RASPICAM_CAMERA *camera){
    if (!camera->_isCapturing)
        return FALSE;

    vcos_log_error("Start grab");
    camera->callback_data->wantToGrab = TRUE;
    vcos_semaphore_wait(&((PORT_USERDATA *)camera->callback_data)->complete_semaphore);
    vcos_log_error("end grab");
    //vcos_log_error("B_Z: %d, W = %d, H = %d, Total = %d", camera->callback_data->buffer_length,
    //                                              camera->width,
    //                                              camera->height,
    //                                              camera->width*camera->height*3);

    return TRUE;

    /*MMAL_PORT_T *camera_still_port = NULL;
    MMAL_POOL_T *pool;
    
    camera_still_port = camera->component->output[MMAL_CAMERA_CAPTURE_PORT];
    pool = mmal_port_pool_create(camera_still_port, camera_still_port->buffer_num, camera_still_port->buffer_size);
    // Send all the buffers to the encoder output port
    int q;
    int num = mmal_queue_length(pool->queue);
    vcos_log_error("%d\n", num);
    for (q=0;q<num;q++)
    {
        MMAL_BUFFER_HEADER_T *buffer = mmal_queue_get(pool->queue);
        
        
        if (!buffer)
            vcos_log_error("Unable to get a required buffer %d from pool queue", q);
        
        //if (mmal_port_send_buffer(camera_still_port, buffer)!= MMAL_SUCCESS)
        //    vcos_log_error("Unable to send a buffer to encoder output port (%d)", q);
        vcos_log_error("Buffer = %d, width = %d, height = %d", buffer->length, camera->width, camera->height); //, buffer->length
    }*/

}

size_t RaspiCam_getImageTypeSize(RASPICAM_CAMERA *camera){
    switch (camera->format) {
        case FORMAT_YUV420:
            return (size_t)(camera->width*camera->height + 2* ( ( camera->width /2 *camera->height /2 ) ));
        case FORMAT_GRAY:
            return (size_t)(camera->width*camera->height);
        case FORMAT_RGB:
        case FORMAT_BGR:
            return (size_t)(3*camera->width*camera->height);
        default:
            return 0;
    };
}

RASPICAM_IMAGE *RaspiCam_retrieve(RASPICAM_CAMERA *camera){
    if(camera->callback_data == NULL || camera->callback_data->buffer_length == 0)
        return NULL;
    printf("A\n");
    RASPICAM_IMAGE *image = (RASPICAM_IMAGE *)malloc(sizeof(RASPICAM_IMAGE));
    printf("B\n");
    image->length = RaspiCam_getImageTypeSize(camera);
    printf("C\n");
    printf("E\n");
    image->data = (unsigned char *)malloc(image->length);
    image->format = camera->format;
    image->width = camera->width;
    image->height = camera->height;
    printf("F\n");
    memcpy(image->data, camera->callback_data->buffer_data, image->length);
    printf("G\n");
    free(camera->callback_data->buffer_data);
    camera->callback_data->buffer_length = 0;
    return image;
}

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
    PORT_USERDATA_ENCODER* pData = (PORT_USERDATA_ENCODER *)encoder->user_data;
    vcos_semaphore_post(&pData->complete_semaphore);
}

BOOLEAN _RaspiCam_createImageWithEncoder(MMAL_WRAPPER_T *encoder, RASPICAM_IMAGE *image, const char *filename, MMAL_FOURCC_T encoding){
    MMAL_PORT_T* portIn;
    MMAL_PORT_T* portOut;
    MMAL_BUFFER_HEADER_T* in;
    MMAL_BUFFER_HEADER_T* out;
    MMAL_STATUS_T status;
    BOOLEAN eos = FALSE;
    BOOLEAN isSend = FALSE;
    int outputWritten = 0;
    FILE* outFile;
    int nw;

    // Configure input
    portIn = encoder->input[0];
    encoder->status = MMAL_SUCCESS;

    if(portIn->is_enabled) {
        if(mmal_wrapper_port_disable(portIn) != MMAL_SUCCESS) {
            vcos_log_error("Failed to disable input port");
            goto error_encoder_create_image;
        }
    }

    portIn->format->encoding = _RaspiCam_getFormat(image->format);
    portIn->format->es->video.width = VCOS_ALIGN_UP(image->width, 32);
    portIn->format->es->video.height = VCOS_ALIGN_UP(image->height, 16);
    portIn->format->es->video.crop.x = 0;
    portIn->format->es->video.crop.y = 0;
    portIn->format->es->video.crop.width = image->width;
    portIn->format->es->video.crop.height = image->height;
    if(mmal_port_format_commit(portIn) != MMAL_SUCCESS) {
        vcos_log_error("Failed to commit input port format");
        goto error_encoder_create_image;
    }

    portIn->buffer_size = portIn->buffer_size_recommended;
    portIn->buffer_num = portIn->buffer_num_recommended;

    if(mmal_wrapper_port_enable(portIn, MMAL_WRAPPER_FLAG_PAYLOAD_ALLOCATE) != MMAL_SUCCESS) {
        vcos_log_error("Failed to enable input port");
        goto error_encoder_create_image;
    }

    // Configure output
    portOut = encoder->output[0];

    if(portOut->is_enabled) {
        if(mmal_wrapper_port_disable(portOut) != MMAL_SUCCESS) {
            vcos_log_error("Failed to disable output port");
            goto error_encoder_create_image;
        }
    }

    portOut->format->encoding = encoding;
    if(mmal_port_format_commit(portOut) != MMAL_SUCCESS) {
        vcos_log_error("Failed to commit output port format");
        goto error_encoder_create_image;
    }

    mmal_port_parameter_set_uint32(portOut, MMAL_PARAMETER_JPEG_Q_FACTOR, 100);
  
    portOut->buffer_size = portOut->buffer_size_recommended;
    portOut->buffer_num = portOut->buffer_num_recommended;

    if (mmal_wrapper_port_enable(portOut, MMAL_WRAPPER_FLAG_PAYLOAD_ALLOCATE) != MMAL_SUCCESS) {
        vcos_log_error("Failed to enable output port");
        goto error_encoder_create_image;
    }

    // Perform the encoding
    outFile = fopen(filename, "w");
    if(!outFile) {
        vcos_log_error("Failed to open file %s (%s)", filename, strerror(errno));
        goto error_encoder_create_image;
    }
  
    while(!eos) {
        // Send output buffers to be filled with encoded image.
        while(mmal_wrapper_buffer_get_empty(portOut, &out, 0) == MMAL_SUCCESS) {
            if(mmal_port_send_buffer(portOut, out) != MMAL_SUCCESS) {
                vcos_log_error("Failed to send buffer");
                break;
            }
        }

        // Send image to be encoded.
        if(!isSend && mmal_wrapper_buffer_get_empty(portIn, &in, 0) == MMAL_SUCCESS) {
            memcpy(in->data, image->data, image->length);
            in->length = in->alloc_size;
            in->flags = MMAL_BUFFER_HEADER_FLAG_EOS;
            if(mmal_port_send_buffer(portIn, in) != MMAL_SUCCESS) {
                vcos_log_error("Failed to send buffer");
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
            vcos_log_error("Failed to get full buffer");
            goto error_encoder_create_image;
        }

        eos = out->flags & MMAL_BUFFER_HEADER_FLAG_EOS;

        nw = fwrite(out->data, 1, out->length, outFile);
        if(nw != out->length) {
            vcos_log_error("Failed to write complete buffer");
            goto error_encoder_create_image;
        }
    
        outputWritten += nw;
    
        mmal_buffer_header_release(out);
    }

    mmal_port_flush(portOut);

    fclose(outFile);

    return TRUE;

    error_encoder_create_image:
    return FALSE;
}

BOOLEAN RaspiCam_save(RASPICAM_IMAGE *image, const char *filename){
    MMAL_WRAPPER_T *encoder;
    MMAL_FOURCC_T extension = _RaspiCam_checkExtension(filename);
    if(extension == ENCODING_ERROR){
        vcos_log_error("Failed to use the extension");
        goto error_encoder;
    }

    PORT_USERDATA_ENCODER *callback_data_encoder = (PORT_USERDATA_ENCODER *)malloc(sizeof(PORT_USERDATA_ENCODER));
    if (vcos_semaphore_create(&callback_data_encoder->complete_semaphore, "encoder sem", 0) != VCOS_SUCCESS) {
        vcos_log_error("Failed to create semaphore");
        goto error_encoder;
    }
    if (mmal_wrapper_create(&encoder, MMAL_COMPONENT_DEFAULT_IMAGE_ENCODER) != MMAL_SUCCESS) {
        vcos_log_error("Failed to create mmal component");
        goto error_encoder;
    }
    encoder->user_data = callback_data_encoder;
    encoder->callback = _RaspiCam_encoder_callback;
  
    // Perform test encodings in various formats
    if(!_RaspiCam_createImageWithEncoder(encoder, image, filename, extension)){
        vcos_log_error("Error creating the image");
        goto error_encoder;
    }

    mmal_wrapper_destroy(encoder);
    vcos_semaphore_delete(&callback_data_encoder->complete_semaphore);

    return TRUE;

    error_encoder:
        if(encoder)
            mmal_wrapper_destroy(encoder);
        vcos_semaphore_delete(&callback_data_encoder->complete_semaphore);
        return FALSE;
}



/*int main(int argc, const char **argv){
    RASPICAM_CAMERA *camera = newRaspiCam();
    //camera->num = 0;
    //camera->format = FORMAT_RGB;
    RaspiCam_open(camera, TRUE);
    RaspiCam_grab(camera);
    RASPICAM_IMAGE *image = RaspiCam_retrieve(camera);
    printf("A\n");
    //vcos_log_error("%d ", (int)data[i]);
    //RaspiCam_save(image, "n.jpeg");
    //free(image);
    printf("B\n");
}*/
