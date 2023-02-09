sudo modprobe -r v4l2loopback
sudo modprobe v4l2loopback
ffmpeg -f video4linux2 -video_size 1980x1080 -input_format mjpeg -i /dev/video0 -codec copy -f v4l2 /dev/video3 -codec copy -f v4l2 /dev/video4
