"""CPU-only Qwen-distilled region proxy. Not a scientific concentration product."""
from __future__ import annotations

import hashlib
import json
import math
from pathlib import Path
import re
import time

import numpy as np
from PIL import Image

VERSION = 'region-colour-texture-v1'
NAME = 'Camera visible ice · experimental (%)'
TYPES = ('open_water','thin_new_ice','broken_ice','consolidated_ice','unknown')
TYPE_NAMES = {'thin_new_ice':'Camera thin/new ice · experimental (%)',
              'broken_ice':'Camera broken ice · experimental (%)',
              'consolidated_ice':'Camera consolidated ice · experimental (%)'}


def crop_region(image):
    # Same camera-3 footprint as the reviewed 2025 experiment. Refuse other
    # dimensions rather than silently moving the footprint onto ship/sky.
    if image.size != (3648, 2052):
        raise ValueError('Unreviewed camera dimensions; expected 3648x2052 camera 3')
    u, v = math.cos(math.radians(-30)), math.sin(math.radians(-30))
    ox = .45*image.width-600*u+300*v
    oy = .62*image.height-600*v-300*u
    return image.convert('RGB').transform((1200,600), Image.Transform.AFFINE,
        (u,-v,ox,v,u,oy), Image.Resampling.BICUBIC)


def features(crop, size=(240,120)):
    """89 colour/texture features; default unchanged, size=None keeps native ROI.

    Alternate resolutions are experiments, not compatible model inputs: pixel
    lags stay at 1,2,4,8,16 in the chosen resolution.
    """
    image=crop.convert('RGB')
    if size is not None:
        if len(size)!=2 or min(size)<=16: raise ValueError('Feature dimensions must exceed largest pixel lag (16)')
        image=image.resize(size,Image.Resampling.BOX)
    if min(image.size)<=16: raise ValueError('Feature image too small')
    rgb = np.asarray(image,dtype=float)/255
    gray = rgb @ np.array([.299,.587,.114])
    result = []
    for plane in (*rgb.transpose(2,0,1),gray,rgb.max(2)-rgb.min(2)):
        result.extend([plane.mean(),plane.std(),*np.quantile(plane,[.05,.25,.5,.75,.95])])
    for lag in (1,2,4,8,16):
        for axis in (0,1):
            diff = np.abs(np.diff(gray,axis=axis)) if lag==1 else np.abs(
                np.take(gray,range(lag,gray.shape[axis]),axis=axis)-
                np.take(gray,range(gray.shape[axis]-lag),axis=axis))
            result.extend([diff.mean(),diff.std(),np.quantile(diff,.9)])
    for block in np.array_split(gray,3,axis=0):
        for tile in np.array_split(block,4,axis=1):
            result.extend([tile.mean(),tile.std()])
    return np.asarray(result)


def quality(crop):
    gray = np.asarray(crop.convert('L').resize((240,120),Image.Resampling.BOX),dtype=float)/255
    contrast = float(np.quantile(gray,.95)-np.quantile(gray,.05))
    edge = float((np.abs(np.diff(gray,axis=0)).mean()+np.abs(np.diff(gray,axis=1)).mean())/2)
    flags = []
    if np.quantile(gray,.95) < .08: flags.append('too_dark')
    if contrast < .035: flags.append('low_contrast_or_featureless')
    if edge < .002: flags.append('blur_or_smooth_surface')
    return dict(flags=flags,contrast=contrast,edge=edge,
        note='Heuristic screening only: fog/wet lens not reliably distinguished from smooth ice/water')


