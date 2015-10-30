#!/usr/bin/env racket
#lang racket

(require profile)  ;see also rerequire / dynamic-rerequire
(profile (dynamic-require "simple-cxx-test.rkt" #f))
