#lang racket


(provide message-event
         group-message-event
         friend-message-event)


(struct message-event (bot subject sender message))

(struct group-message-event message-event (group))

(struct friend-message-event message-event (friend))