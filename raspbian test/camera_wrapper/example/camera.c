//#include <ctime>
//#include <fstream>
//#include <iostream>
//#include "craspicam.h"
//using namespace std;
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "craspicam.h"
int main ( int argc,char **argv ) {
    raspiCamHandle camera = newRaspiCam();
    //Open camera
    printf("Opening Camera...\n");
    if(!RaspiCam_open(camera, TRUE)){
        printf("Error opening camera\n");
        return -1;
    }
    //wait a while until camera stabilizes
    printf("Sleeping for 3 secs\n");
    sleep(3);
    // capture
    RaspiCam_grab(camera);
    //allocate memory
    //unsigned char *data=new unsigned char[  Camera.getImageTypeSize ( raspicam::RASPICAM_FORMAT_RGB )];
    unsigned char *data = (unsigned char *) malloc(RaspiCam_getImageTypeSize(camera, FORMAT_RGB));
    
    
    
    
    
    /*raspicam::RaspiCam Camera; //Cmaera object
     //Open camera
     cout<<"Opening Camera..."<<endl;
     if ( !Camera.open()) {cerr<<"Error opening camera"<<endl;return -1;}
     //wait a while until camera stabilizes
     cout<<"Sleeping for 3 secs"<<endl;
     sleep(3);
     //capture
     Camera.grab();
     //allocate memory
     unsigned char *data=new unsigned char[  Camera.getImageTypeSize ( raspicam::RASPICAM_FORMAT_RGB )];
     //extract the image in rgb format
     Camera.retrieve ( data,raspicam::RASPICAM_FORMAT_RGB );//get camera image
     //save
     std::ofstream outFile ( "raspicam_image.ppm",std::ios::binary );
     outFile<<"P6\n"<<Camera.getWidth() <<" "<<Camera.getHeight() <<" 255\n";
     outFile.write ( ( char* ) data, Camera.getImageTypeSize ( raspicam::RASPICAM_FORMAT_RGB ) );
     cout<<"Image saved at raspicam_image.ppm"<<endl;
     //free resrources
     delete data;*/
    
    return 0;
}
