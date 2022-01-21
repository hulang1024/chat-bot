#lang racket

(provide host restart-shell)


(define host "localhost:9999")
(define restart-shell "sh /home/eval-server/startup.sh")