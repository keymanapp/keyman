#!/usr/bin/python3

import numpy as np
import sys
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
    im = Image.open(icofile)
    im = im.convert('RGBA')
    im2 = im
    num, colour = max(im.getcolors(im.size[0]*im.size[1]))
    print("{0}: {1}".format(num, colour))
    if num > 160 and colour == (0, 0, 0, 0):
        print("mostly black so changing black to white")
        im2 = changeblacktowhite(im)
    im2.save(icofile + ".bmp")


def main(argv):
    if len(sys.argv) != 2:
        print("convertico.py <ico file>")
        sys.exit(2)
    checkandsaveico(sys.argv[1])

if __name__ == "__main__":
    main(sys.argv[1:])  