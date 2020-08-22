chcp 65001 > nul
set cam2="USB Video Device"
ffmpeg -f dshow -show_video_device_dialog true -i video=%cam2%
