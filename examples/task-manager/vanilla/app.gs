;; -*- mode: scheme; coding: utf-8; -*-

;;
;; The task manager, using the DOM directly (no framework).
;;

(define-module (app)
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
  (h "input" (js-obj #:placeholder "What needs to be done?"
                     #:max-length max-title-length)))

(define errors-list (h "ul" (js-obj #:class-name "errors")))
(define task-list (h "ul" (js-obj #:class-name "tasks")))
(define counter (h "span" (js-obj #:class-name "counter")))
(define filter-buttons (h "div" (js-obj #:class-name "filters")))

(define (show-errors! errors)
  (set-children! errors-list
                     (map (lambda (error) (h "li" (js-obj) error)) errors)))

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
  (let ((input (h "input" (js-obj #:class-name "edit" #:value (.-title task))))
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

(define (task-item task)
  (letrec ((label (h "span" (js-obj #:class-name "title"
                                    #:title "Double-click to edit"
                                    #:ondblclick (lambda (event) (edit-title! task label)))
                     (.-title task))))
    (h "li" (js-obj #:class-name (if (.-done task) "task done" "task"))
       (h "input" (js-obj #:type "checkbox"
                          #:checked (.-done task)
                          #:onchange (lambda (event)
                                       (toggle-task! task (.-checked (.-target event))))))
       label
       (h "button" (js-obj #:class-name "delete"
                           #:title "Delete"
                           #:onclick (lambda (event) (remove-task! task)))
          "×"))))

(define (filter-button name label)
  (h "button" (js-obj #:class-name (if (eq? name current-filter) "selected" "")
                      #:onclick (lambda (event)
                                  (set! current-filter name)
                                  (render!)))
     label))

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
   (h "main" (js-obj #:class-name "app")
      (h "h1" (js-obj) "Tasks")
      (h "form" (js-obj #:onsubmit add-task!)
         title-input
         (h "button" (js-obj #:type "submit") "Add"))
      errors-list
      (h "input" (js-obj #:class-name "search"
                         #:type "search"
                         #:placeholder "Search"
                         #:oninput (lambda (event)
                                     (set! search (.-value (.-target event)))
                                     (render!))))
      task-list
      (h "footer" (js-obj)
         counter
         filter-buttons
         (h "button" (js-obj #:onclick (lambda (event) (clear-done!)))
            "Clear done")))))

(set! (.-oninput title-input)
      (lambda (event)
        (let ((title (.-value title-input)))
          (show-errors! (if (string-null? title) '() (task-title-errors title))))))

;; The page is shown once the tasks are loaded (so they can't be changed
;; before).
(set-children! app (h "main" (js-obj #:class-name "app") (h "p" (js-obj) "Loading…")))

(guard (e ((error-object? e)
           (set-children! app (h "main" (js-obj #:class-name "app")
                                 (h "p" (js-obj #:class-name "errors")
                                    (string-append "Can't load the tasks: "
                                                   (error-object-message e)))))))
  (set! tasks (js-await (fetch-tasks)))
  (show-page!)
  (render!))
