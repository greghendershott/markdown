#lang racket

(require "main.rkt")

(provide (all-from-out "main.rkt")
         (rename-out [module-begin/markdown #%module-begin]))
