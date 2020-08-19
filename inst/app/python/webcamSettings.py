import cv2
cam0 = cv2.VideoCapture(0 + cv2.CAP_DSHOW)
cam1= cv2.VideoCapture(1 + cv2.CAP_DSHOW)
cam0.set(37, 0)
cam1.set(37, 0)
cam0.release()
cam1.release()