#!/usr/bin/python3

import logging
import os
import struct
import sys

import numpy as np
from PIL import Image, ImageFile

ImageFile.LOAD_TRUNCATED_IMAGES = True


def _changeblacktowhite(image):
    data = np.array(image)   # "data" is a height x width x 4 numpy array
    red, green, blue, alpha = data.T  # Temporarily unpack the bands for readability

    # Replace black with white... (leaves alpha values alone...)
    white_areas = (red == 0) & (blue == 0) & (green == 0)
    data[..., :-1][white_areas.T] = (255, 255, 255)  # Transpose back needed

    return Image.fromarray(data)


def checkandsaveico(icofile):
    """
    Convert keyman icofile to 64x64 png to work in IBus
    The starting icofile may be ico or bmp format
    and follows the progression:
    .ico -> .bmp -> .bmp.png (png format)

    Args:
        icofile (str): path to ico file
    """
    name, ext = os.path.splitext(icofile)
    bmpfile = f"{name}.bmp"
    if ext == '.ico':
        _convert_ico_to_bmp(icofile, bmpfile)
    try:
        with Image.open(bmpfile) as im3:
            with im3.resize((64, 64), Image.LANCZOS) as im4:
                # Using .bmp.png file extension so it won't conflict if the package already contains .png
                im4.save(f'{bmpfile}.png', 'png')
    except (IOError, OSError):
        logging.error("Cannot convert %s to png", icofile)
    finally:
        # Clean up intermediary .bmp file if it was generated
        if ext == '.ico':
            os.remove(bmpfile)


def _convert_ico_to_bmp(icofile, bmpfile):
    with Image.open(icofile) as image:
        with image.convert('RGBA') as image2:
            num, colour = max(image.getcolors(image2.size[0] * image2.size[1]))
            logging.debug(f"checkandsaveico maxcolour: num {num}: colour {colour}")
            if num > 160 and colour == (0, 0, 0, 0):
                logging.info(f"checkandsaveico:{icofile} mostly black so changing black to white")
                image2.close()
                image2 = _changeblacktowhite(image)
            image2.save(bmpfile)


def extractico(kmxfile):
    """
    Extract icon file from compiled kmx keyboard
    The icon file will be converted into .bmp.png

    Args:
        kmxfile (str): path to kmx file
    """
    name, ext = os.path.splitext(kmxfile)
    imagefilename = name
    bitmap = None
    try:
        with open(kmxfile, mode='rb') as file:  # b is important -> binary
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
            if not bitmap or file.tell() != bitmapOffset + bitmapSize:
                logging.debug("unreadable bitmap in kmx")
                return False
    except Exception as e:
        logging.warning('Exception %s extracting icon %s %s', type(e), kmxfile, e.args)
        return False

    # Read first two bytes to determine if icon is .bmp or .ico
    if bitmap.startswith(b'BM'):
        imagefilename = f"{imagefilename}.bmp"
    else:
        imagefilename = f"{imagefilename}.ico"

    try:
        with open(imagefilename, mode='wb') as imagefile:
            imagefile.write(bitmap)
        checkandsaveico(imagefilename)
    except Exception as e:
        logging.warning('Exception %s writing imagefile %s %s', type(e), imagefilename, e.args)
        return False
    return True


def main():
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
    main()
