#lang racket/base

(provide ci-environment?)

(define ci-environment?
  (ormap getenv '("CI"
                  "TRAVIS"
                  "PLT_PKG_BUILD_SERVICE")))
