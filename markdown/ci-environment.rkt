;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: BSD-2-Clause

#lang racket/base

(provide ci-environment?)

(define ci-environment?
  (ormap getenv '("CI"
                  "TRAVIS"
                  "PLT_PKG_BUILD_SERVICE")))
