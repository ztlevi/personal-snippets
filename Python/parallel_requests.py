#!/usr/bin/env python3

import multiprocessing

import requests

BASE_URI = "http://placewherestuff.is/?q="


def internet_resource_getter(stuff_to_get):
    session = session.Session()
    stuff_got = []

    for thing in stuff_to_get:
        response = session.get(BASE_URI + thing)
        stuff_got.append(response.json())

    return stuff_got


stuff_that_needs_getting = ["a", "b", "c"]

pool = multiprocessing.Pool(processes=3)
pool_outputs = pool.map(internet_resource_getter, stuff_that_needs_getting)
pool.close()
pool.join()
print(pool_outputs)
