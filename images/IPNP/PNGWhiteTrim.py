from PIL import Image # pip install Pillow
import sys
import glob
from PIL import ImageOps
import numpy as np
import os
import ntpath

# Trim all png images with white background in a folder

_thisDir = os.path.dirname(os.path.abspath(__file__))
os.chdir(_thisDir)
folderName = os.path.join(_thisDir, "original")

maxsize = 300

min_height = 200

filePaths = glob.glob(folderName + "/*.png")

newfolderName = os.path.join(_thisDir, "trimmed")

os.mkdir(newfolderName)

for filePath in filePaths:
    image = Image.open(filePath)
    image.load()

    # remove alpha channel
    invert_im = image.convert("RGB")

    # invert image (so that white is 0)
    invert_im = ImageOps.invert(invert_im)
    imageBox = invert_im.getbbox()
    imageBox = tuple(np.asarray(imageBox))

    cropped=image.crop(imageBox)

    image_width = (imageBox[2] - imageBox[0])
    image_height = (imageBox[3] - imageBox[1])

    if image_width >= image_height:
        mult_ratio = (float(maxsize) / image_width)
    else:
        mult_ratio = (float(maxsize) / image_height)

    new_height = int(image_height * mult_ratio)
    if new_height < min_height:
        new_height = min_height

    new_width = int(image_width * mult_ratio)


    upscaled = cropped.resize((new_width, new_height))

    filePath_basename = (ntpath.basename(filePath))
    upscaled.save(newfolderName + os.sep + filePath_basename)
