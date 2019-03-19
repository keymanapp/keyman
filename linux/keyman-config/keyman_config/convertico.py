#!/usr/bin/python3

import logging
import numpy as np
import os
import sys
import struct
from PIL import Image


def changeblacktowhite(im):
    data = np.array(im)   # "data" is a height x width x 4 numpy array
    red, green, blue, alpha = data.T # Temporarily unpack the bands for readability

    # Replace black with white... (leaves alpha values alone...)
    white_areas = (red == 0) & (blue == 0) & (green == 0)
    data[..., :-1][white_areas.T] = (255, 255, 255) # Transpose back needed

    im2 = Image.fromarray(data)
    return im2

def checkandsaveico(icofile):
    """
    Convert keyman ico file to png to work in IBus
    The ico file may be ico or bmp format

    Args:
        icofile (str): path to ico file
    """
    im = Image.open(icofile)
    im = im.convert('RGBA')
    im2 = im
    num, colour = max(im.getcolors(im.size[0]*im.size[1]))
    logging.debug("checkandsaveico maxcolour: num {0}: colour {1}".format(num, colour))
    if num > 160 and colour == (0, 0, 0, 0):
        logging.info("checkandsaveico:" + icofile + " mostly black so changing black to white")
        im2 = changeblacktowhite(im)
    im2.save(icofile + ".bmp")
    im3 = Image.open(icofile + ".bmp")
    im4 = im3.resize((64, 64), Image.ANTIALIAS)
    im4.save(icofile + ".png")
    os.remove(icofile + ".bmp")

def extractico(kmxfile):
    """
    Extract icon file from compiled kmx keyboard

    Args:
        kmxfile (str): path to kmx file
    """
    name, ext = os.path.splitext(kmxfile)
    icofilename = name+".ico"
    with open(kmxfile, mode='rb') as file: # b is important -> binary
        fileContent = file.read()

        kmxstart = struct.unpack_from("<16I", fileContent, 0)
        if kmxstart[0] != 0x5354584B:
            logging.debug("bad kmx identifier")
            return False
        bitmapOffset = kmxstart[14]
        bitmapSize = kmxstart[15]
        logging.debug("bitmap offset is %d", bitmapOffset)
        logging.debug("bitmap size is %d", bitmapSize)
        file.seek(bitmapOffset, 0)
        bitmap = file.read(bitmapSize)
        if not bitmap:
            logging.debug("unreadable bitmap in kmx")
            return False
        with open(icofilename, mode='wb') as iconfile:
            iconfile.write(bitmap)
        checkandsaveico(icofilename)
        return True


def main(argv):
    if len(sys.argv) != 2:
        logging.error("convertico.py <ico file | kmx file>")
        sys.exit(2)
    logging.basicConfig(level=logging.INFO)
    name, ext = os.path.splitext(sys.argv[1])
    if ext == ".kmx":
        extractico(sys.argv[1])
    else:
        checkandsaveico(sys.argv[1])

if __name__ == "__main__":
    main(sys.argv[1:])
