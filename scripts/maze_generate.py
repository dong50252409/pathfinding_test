#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import matplotlib.pyplot as plt
import numpy as np
import erlang
import sys


def rect(pos):
    r = plt.Rectangle(pos - 0.6, 1, 1, facecolor='none', edgecolor='k', linewidt=1)
    plt.gca().add_patch(r)


if __name__ == '__main__':
    with open(sys.argv[1], "rb") as f:
        a = np.array(erlang.binary_to_term(f.read())[0])
    im = plt.imshow(a, cmap=plt.cm.gray, interpolation='none', vmin=0, vmax=1, aspect='equal')
    x, y = np.meshgrid(np.arange(a.shape[1]), np.arange(a.shape[0]))
    m = np.c_[x[a.astype(bool)], y[a.astype(bool)]]
    for pos in m:
        rect(pos)
    plt.show()
