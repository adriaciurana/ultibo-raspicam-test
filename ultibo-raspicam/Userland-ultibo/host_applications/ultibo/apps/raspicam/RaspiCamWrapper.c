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

BOOLEAN _RaspiCam_create_camera(RASPICAM_CAMERA *camera){
    MMAL_STATUS_T status = MMAL_SUCCESS;

    camera->component = 0;
    status = mmal_component_create(MMAL_COMPONENT_DEFAULT_CAMERA, &camera->component);
    camera->params = (RASPICAM_CAMERA_PARAMETERS *)malloc(sizeof(RASPICAM_CAMERA_PARAMETERS));
    
    // init del componente de la camara
    if (status != MMAL_SUCCESS){
      vcos_log_error("Failed to create camera component");
      goto error;
   }

   // params
   raspicamcontrol_set_defaults(camera->params);

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

   {
       MMAL_PARAMETER_CAMERA_CONFIG_T config =
       {
            /* header */
            {MMAL_PARAMETER_CAMERA_CONFIG, sizeof(config)},
            .max_stills_w = camera->width,
            .max_stills_h = camera->height,
            .stills_yuv422 = 0,
            .one_shot_stills = 1,
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

   switch(camera->format){
        case FORMAT_RGB:
            format->encoding_variant = MMAL_ENCODING_RGB24;
            format->encoding = MMAL_ENCODING_RGB24;
            break;
        case FORMAT_BGR:
            format->encoding_variant = MMAL_ENCODING_BGR24;
            format->encoding = MMAL_ENCODING_BGR24;
            break;
        case FORMAT_GRAY:
            format->encoding_variant = MMAL_ENCODING_I420;
            format->encoding = MMAL_ENCODING_I420;
            break;
        case FORMAT_YUV420:
            format->encoding_variant = MMAL_ENCODING_I420;
            format->encoding = MMAL_ENCODING_I420;
            break;
        default:
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

   status = mmal_port_enable(camera->port, _video_buffer_callback);
   if (status != MMAL_SUCCESS){
      vcos_log_error("camera callback error");
      goto error;
   }

   if (camera->port->buffer_num < VIDEO_OUTPUT_BUFFERS_NUM)
       camera->port->buffer_num = VIDEO_OUTPUT_BUFFERS_NUM;

   camera->port->buffer_size = camera->port->buffer_size_recommended;
   camera->port->buffer_num = camera->port->buffer_num_recommended;
   camera->pool = mmal_port_pool_create(camera->port, camera->port->buffer_num, camera->port->buffer_size);

   if (!(camera->pool)) {
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

    //printf("W: %d, H: %d\n", camera->width, camera->height);
    
    // CAMERA
    if(!_RaspiCam_create_camera(camera))
        return FALSE;

    return TRUE;
}

RASPICAM_CAMERA *newRaspiCam(){
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
}

void deleteRaspiCam(RASPICAM_CAMERA *camera){
    return RaspiCam_release(camera);
}

void _video_buffer_callback(MMAL_PORT_T *port, MMAL_BUFFER_HEADER_T *buffer){
    PORT_USERDATA *pData = ( PORT_USERDATA * ) port->userdata;

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
        vcos_semaphore_post(&(pData->complete_semaphore));
}

BOOLEAN RaspiCam_startCapture(RASPICAM_CAMERA *camera){
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
            vcos_log_error("Unable to get a required buffer %d from pool queue", q);
             
        if (mmal_port_send_buffer(camera->port, buffer) != MMAL_SUCCESS)
            vcos_log_error("Unable to send to the output port X");
    }
    camera->_isCapturing = TRUE;
    return TRUE;

        
}

BOOLEAN RaspiCam_open(RASPICAM_CAMERA *camera, BOOLEAN StartCapture){
    if(camera->_isOpened) return FALSE; // already open
    if(!_RaspiCam_create(camera))
        return FALSE;

    camera->port = camera->component->output[MMAL_CAMERA_VIDEO_PORT];

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
    if(StartCapture)
        return RaspiCam_startCapture(camera);
    return TRUE;
};

BOOLEAN RaspiCam_grab(RASPICAM_CAMERA *camera){
    if (!camera->_isCapturing)
        return FALSE;

    camera->callback_data->wantToGrab = TRUE;
    vcos_semaphore_wait(&camera->callback_data->complete_semaphore);

    //printf("B_Z: %d, W = %d, H = %d, Total = %d", camera->callback_data->buffer_length,
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
    printf("%d\n", num);
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

size_t RaspiCam_retrieve(RASPICAM_CAMERA *camera, unsigned char **data){
    if(camera->callback_data == NULL || camera->callback_data->buffer_length == 0)
        return;
    
    size_t size_to_cpy = RaspiCam_getImageTypeSize(camera);
    (*data) = (unsigned char *)malloc(size_to_cpy);
    memcpy(*data, camera->callback_data->buffer_data, size_to_cpy);
    return size_to_cpy;
}


BOOLEAN RaspiCam_saveAsPGM(RASPICAM_CAMERA *camera, unsigned char *data, size_t size_of_data, const char *filename){
     // Save pgm
    FILE * fp;
    //const char *filename = "n.pgm";
    fp = fopen(filename, "wb");
    /* write header to the file */
    const char *comment = "# this is my new binary pgm file";
    printf("%d\n", camera->height);
    fprintf(fp, "P5\n %s\n %d\n %d\n %d\n", comment, camera->width, camera->height,
            255);
    /* write image data bytes to the file */
    fwrite(data, size_of_data, 1, fp);
    fclose(fp);
    printf("OK - file %s saved\n", filename);
}



/*int main(int argc, const char **argv){
    RASPICAM_CAMERA *camera = newRaspiCam();
    //camera->num = 0;
    //camera->format = FORMAT_RGB;
    RaspiCam_open(camera, TRUE);
    RaspiCam_grab(camera);
    unsigned char *data;
    size_t data_length = RaspiCam_retrieve(camera, &data);
    //printf("%d ", (int)data[i]);
    RaspiCam_saveAsPGM(camera, data, data_length, "n.pgm");
    free(data);
}*/

