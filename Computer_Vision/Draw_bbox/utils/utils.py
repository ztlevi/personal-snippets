#!/usr/bin/env python3
import urllib

import numpy as np

import cv2


# METHOD #1: OpenCV, NumPy, and urllib
def url_to_image(url):
    # download the image, convert it to a NumPy array, and then read
    # it into OpenCV format
    resp = urllib.urlopen(url)
    image = np.asarray(bytearray(resp.read()), dtype="uint8")
    image = cv2.imdecode(image, cv2.IMREAD_COLOR)

    # return the image
    return image


def ratio_bbox_to_xyxy(im, bbox):
    H, W = im.shape[:2]
    x1, y1, x2, y2 = bbox
    x1 = int(x1 * W)
    y1 = int(y1 * H)
    x2 = int(x2 * W)
    y2 = int(y2 * H)
    return [x1, y1, x2, y2]


def ratio_bbox_to_xywh(im, bbox):
    """
    ratio_bbox consists of four numbers: [x1/W, y1/H, x2/W, y2/H]
    Buttom left point: (x1, y1), top right point: (x2, y2)
    """
    H, W = im.shape[:2]
    x1, y1, x2, y2 = bbox
    x1 *= W
    y1 *= H
    x2 *= W
    y2 *= H
    x = int(x1)
    y = int(y1)
    w = int(x2 - x1)
    h = int(y2 - y1)
    return [x, y, w, h]
