import cv2
import os

def center_square_crop (img):
        height, width = img.shape[:2]
        crp = int(min(height, width)/2)
        xmedio, ymedio = int(width/2), int(height/2)
        cropped = img[ymedio-crp:ymedio+crp, xmedio-crp:xmedio+crp]
        return cropped

def center_square_scale(img, pixel=300):
    if img.shape[0]!=img.shape[1]:
        return False
    factor = pixel/img.shape[0]
    return cv2.resize(img,(int(img.shape[1]*factor), int(img.shape[0]*factor)))

dir = os.path.dirname(os.path.abspath(__file__))
imgout = os.path.join(dir, 'dataset')
if not os.path.exists(imgout):
    os.mkdir (imgout)
print (dir)
filein = os.path.join(dir, 'names.txt')
file1 = open(filein, 'r')
Lines = file1.readlines()
for line in Lines:
    if (line.endswith('.jpg\n') or line.endswith('.png\n')):
        imgin = os.path.join(dir, line.strip())
        img = cv2.imread(imgin)
        new_img = center_square_scale(center_square_crop (img))
        imgout = os.path.join(dir, 'dataset', line.strip())
        success = cv2.imwrite(imgout, new_img)
        if success == True:
            print ('\b'*200 + line.strip() + " saved successfully", end='')

print('\b'*200 + "Everything went smooth, \nConversion complete!")