;; -*- mode: scheme; coding: utf-8; -*-

;;
;; The task manager, using the DOM directly (no framework).
;;

(define-module (app)
  #:use-module (guilescript html)
  #:use-module (dom)
  #:use-module (tasks api)
  #:use-module (tasks validation))

(js-global document)

;;
;; State
;;

(define tasks '())           ; the tasks, from the server
(define current-filter 'all) ; all, active or done
(define search "")

(define (visible? task)
  (and (case current-filter
         ((active) (not (.-done task)))
         ((done) (.-done task))
         (else #t))
       (.includes (.to-lower-case (.-title task)) (.to-lower-case search))))

(define (replace-task! updated)
  (set! tasks (map (lambda (task)
                     (if (= (.-id task) (.-id updated)) updated task))
                   tasks)))

;;
;; Elements that are created only once
;;

(define title-input
  (html (input #:placeholder "What needs to be done?"
               #:max-length max-title-length)))

(define errors-list (html (ul #:class-name "errors")))
(define task-list (html (ul #:class-name "tasks")))
(define counter (html (span #:class-name "counter")))
(define filter-buttons (html (div #:class-name "filters")))

(define (show-errors! errors)
  (set-children! errors-list (map (lambda (error) (html (li error))) errors)))

;;
;; Actions
;;

(define (add-task! event)
  (.prevent-default event)
  (let* ((title (.-value title-input))
         (errors (task-title-errors title)))
    (if (null? errors)
        (guard (e ((error-object? e) (show-errors! (list (error-object-message e)))))
          (let ((task (js-await (create-task (string-trim-both title)))))
            (set! tasks (append tasks (list task)))
            (set! (.-value title-input) "")
            (show-errors! '())
            (render!)))
        (show-errors! errors))))

(define (toggle-task! task done)
  (replace-task! (js-await (update-task (.-id task) (js-obj #:done done))))
  (render!))

(define (remove-task! task)
  (js-await (delete-task (.-id task)))
  (set! tasks (remove (lambda (t) (= (.-id t) (.-id task))) tasks))
  (render!))

(define (clear-done!)
  (js-await (.all Promise (map (lambda (task) (delete-task (.-id task)))
                              (filter (lambda (task) (.-done task)) tasks))))
  (set! tasks (remove (lambda (task) (.-done task)) tasks))
  (render!))

;; Replace the title of a task by an input to edit it.
(define (edit-title! task label)
  (let ((input (html (input #:class-name "edit" #:value ,(.-title task))))
        (done? #f))
    (define (finish! save?)
      (unless done?
        (set! done? #t)
        (let* ((title (.-value input))
               (errors (task-title-errors title)))
          (cond
           ((not save?) (render!))
           ((null? errors)
            (replace-task! (js-await (update-task (.-id task)
                                                  (js-obj #:title (string-trim-both title)))))
            (render!))
           (else
            (show-errors! errors)
            (render!))))))
    (set! (.-onkeydown input)
          (lambda (event)
            (case (.-key event)
              (("Enter") (finish! #t))
              (("Escape") (finish! #f)))))
    (set! (.-onblur input) (lambda (event) (finish! #t)))
    (.replace-with label input)
    (.focus input)))

;;
;; Rendering
;;

;; Properties are DOM properties (e.g. onclick, not React's onClick).
(define (task-item task)
  (letrec ((label (html (span #:class-name "title"
                              #:title "Double-click to edit"
                              #:ondblclick ,(lambda (event) (edit-title! task label))
                              ,(.-title task)))))
    (html (li #:class-name ,(if (.-done task) "task done" "task")
              (input #:type "checkbox"
                     #:checked ,(.-done task)
                     #:onchange ,(lambda (event)
                                   (toggle-task! task (.-checked (.-target event)))))
              label
              (button #:class-name "delete"
                      #:title "Delete"
                      #:onclick ,(lambda (event) (remove-task! task))
                      "×")))))

(define (filter-button name label)
  (html (button #:class-name ,(if (eq? name current-filter) "selected" "")
                #:onclick ,(lambda (event)
                             (set! current-filter name)
                             (render!))
                label)))

(define (render!)
  (set-children! task-list (map task-item (filter visible? tasks)))
  (let ((left (count (lambda (task) (not (.-done task))) tasks)))
    (set! (.-text-content counter)
          (string-append (number->string left) (if (= left 1) " task left" " tasks left"))))
  (set-children! filter-buttons
                     (filter-button 'all "All")
                     (filter-button 'active "Active")
                     (filter-button 'done "Done")))

;;
;; Start
;;

(define app (.get-element-by-id document "app"))

(define (show-page!)
  (set-children!
   app
   (html (main #:class-name "app"
               (h1 "Tasks")
               (form #:onsubmit add-task!
                     title-input
                     (button #:type "submit" "Add"))
               errors-list
               (input #:class-name "search"
                      #:type "search"
                      #:placeholder "Search"
                      #:oninput ,(lambda (event)
                                   (set! search (.-value (.-target event)))
                                   (render!)))
               task-list
               (footer counter
                       filter-buttons
                       (button #:onclick ,(lambda (event) (clear-done!))
                               "Clear done"))))))

(set! (.-oninput title-input)
      (lambda (event)
        (let ((title (.-value title-input)))
          (show-errors! (if (string-null? title) '() (task-title-errors title))))))

;; The page is shown once the tasks are loaded (so they can't be changed
;; before).
(set-children! app (html (main #:class-name "app" (p "Loading…"))))

(guard (e ((error-object? e)
           (set-children! app (html (main #:class-name "app"
                                          (p #:class-name "errors"
                                             ,(string-append "Can't load the tasks: "
                                                             (error-object-message e))))))))
  (set! tasks (js-await (fetch-tasks)))
  (show-page!)
  (render!))
