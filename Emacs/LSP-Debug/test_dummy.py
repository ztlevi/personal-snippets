#!/usr/bin/env python3

import pytest


@pytest.mark.parametrize("i", (1, 2, 3))
def test_dummy(i):
    print()
    print(i)
    a = 2

