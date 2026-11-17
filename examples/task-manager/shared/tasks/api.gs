;; -*- mode: scheme; coding: utf-8; -*-

;;
;; The task manager API (see server.scm). Every procedure returns a promise.
;; Failed requests raise an Error with the problems reported by the server.
;;

(define-module (tasks api)
  #:export (fetch-tasks
            create-task
            update-task
            delete-task))

(define (request method url body)
  (let ((response (js-await
                   (fetch url
                          #:method method
                          #:headers (js-obj "Content-Type" "application/json")
                          #:body (if body (.stringify JSON body) undefined)))))
    (cond
     ((= (.-status response) 204) undefined)
     ((.-ok response) (js-await (.json response)))
     (else
      (let ((data (js-await (.json response))))
        (raise (js-new Error (.join (.-errors data) " "))))))))

(define (task-url id)
  (string-append "/api/tasks/" (number->string id)))

(define (fetch-tasks)
  (request "GET" "/api/tasks" #f))

(define (create-task title)
  (request "POST" "/api/tasks" (js-obj #:title title)))

;; CHANGES is an object with the new title and/or done.
(define (update-task id changes)
  (request "PATCH" (task-url id) changes))

(define (delete-task id)
  (request "DELETE" (task-url id) #f))
