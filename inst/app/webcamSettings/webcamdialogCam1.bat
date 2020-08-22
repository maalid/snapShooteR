chcp 65001 > nul
set cam="Microsoft® LifeCam Studio(TM)"
ffmpeg -f dshow -show_video_device_dialog true -i video=%cam%
