;; -*- mode: scheme; coding: utf-8; -*-

;;
;; The task manager, with React components.
;;

(define-module (app)
  #:use-module (guilescript html)
  #:use-module (tasks api)
  #:use-module (tasks validation)
  #:export (<app>))

(js-import "react" #:rename '((createElement . h)) use-state use-effect <fragment>)

;; The form to add tasks. ON-ADD adds a task with the given title (and
;; returns a promise).
(define (<task-form> props)
  (define-values (title set-title!) (use-state ""))
  (define-values (errors set-errors!) (use-state '()))

  (define (change! event)
    (let ((value (.-value (.-target event))))
      (set-title! value)
      (set-errors! (if (string-null? value) '() (task-title-errors value)))))

  (define (submit! event)
    (.prevent-default event)
    (let ((problems (task-title-errors title)))
      (if (null? problems)
          (guard (e ((error-object? e) (set-errors! (list (error-object-message e)))))
            (js-await ((.-on-add props) (string-trim-both title)))
            (set-title! "")
            (set-errors! '()))
          (set-errors! problems))))

  (html
   (div
    (form #:on-submit submit!
          (input #:placeholder "What needs to be done?"
                 #:max-length max-title-length
                 #:value title
                 #:on-change change!)
          (button #:type "submit" "Add"))
    (ul #:class-name "errors"
        ,(map (lambda (error) (html (li #:key error error))) errors)))))

;; A task: a checkbox, its title (double-click to edit it) and a delete
;; button. ON-CHANGE is called with the changes (an object with the new title
;; or done) and ON-DELETE to delete it.
(define (<task-item> props)
  (define task (.-task props))
  (define-values (editing? set-editing!) (use-state #f))
  (define-values (title set-title!) (use-state (.-title task)))

  (define (save!)
    (set-editing! #f)
    (if (null? (task-title-errors title))
        ((.-on-change props) (js-obj #:title (string-trim-both title)))
        (set-title! (.-title task))))

  (define (cancel!)
    (set-editing! #f)
    (set-title! (.-title task)))

  (html
   (li #:class-name ,(if (.-done task) "task done" "task")
       (input #:type "checkbox"
              #:checked ,(.-done task)
              #:on-change ,(lambda (event)
                             ((.-on-change props)
                              (js-obj #:done (.-checked (.-target event))))))
       ,(if editing?
            (html (input #:class-name "edit"
                         #:value title
                         #:auto-focus #t
                         #:on-change ,(lambda (event) (set-title! (.-value (.-target event))))
                         #:on-blur ,(lambda (event) (save!))
                         #:on-key-down ,(lambda (event)
                                          (case (.-key event)
                                            (("Enter") (save!))
                                            (("Escape") (cancel!))))))
            (html (span #:class-name "title"
                        #:title "Double-click to edit"
                        #:on-double-click ,(lambda (event) (set-editing! #t))
                        ,(.-title task))))
       (button #:class-name "delete"
               #:title "Delete"
               #:on-click ,(lambda (event) ((.-on-delete props)))
               "×"))))

;; The filters: (name label).
(define filters '(("all" "All") ("active" "Active") ("done" "Done")))

(define (<app>)
  ;; The tasks are #f until they are loaded.
  (define-values (tasks set-tasks!) (use-state #f))
  (define-values (current-filter set-filter!) (use-state "all"))
  (define-values (search set-search!) (use-state ""))
  (define-values (error set-error!) (use-state #f))

  (define (load!)
    (guard (e ((error-object? e)
               (set-error! (string-append "Can't load the tasks: " (error-object-message e)))))
      (set-tasks! (js-await (fetch-tasks)))))

  ;; The effect function must not return anything (and it would return a
  ;; promise if it used js-await), so it calls load!.
  (use-effect (lambda () (load!) undefined) '())

  (define (add! title)
    (let ((task (js-await (create-task title))))
      (set-tasks! (lambda (tasks) (append tasks (list task))))))

  (define (change! task changes)
    (guard (e ((error-object? e) (set-error! (error-object-message e))))
      (let ((updated (js-await (update-task (.-id task) changes))))
        (set-tasks! (lambda (tasks)
                      (map (lambda (t) (if (= (.-id t) (.-id updated)) updated t))
                           tasks))))))

  (define (delete! task)
    (js-await (delete-task (.-id task)))
    (set-tasks! (lambda (tasks) (remove (lambda (t) (= (.-id t) (.-id task))) tasks))))

  (define (clear-done!)
    (js-await (.all Promise (map (lambda (task) (delete-task (.-id task)))
                                (filter (lambda (task) (.-done task)) tasks))))
    (set-tasks! (lambda (tasks) (remove (lambda (task) (.-done task)) tasks))))

  (define (visible? task)
    (and (cond
          ((equal? current-filter "active") (not (.-done task)))
          ((equal? current-filter "done") (.-done task))
          (else #t))
         (.includes (.to-lower-case (.-title task)) (.to-lower-case search))))

  (define (task-list)
    (define left (count (lambda (task) (not (.-done task))) tasks))
    (html
     (<fragment>
      (<task-form> #:on-add add!)
      ,(and error (html (p #:class-name "errors" error)))
      (input #:class-name "search"
             #:type "search"
             #:placeholder "Search"
             #:value search
             #:on-change ,(lambda (event) (set-search! (.-value (.-target event)))))
      (ul #:class-name "tasks"
          ,(map (lambda (task)
                  (html (<task-item> #:key ,(.-id task)
                                     #:task task
                                     #:on-change ,(lambda (changes) (change! task changes))
                                     #:on-delete ,(lambda () (delete! task)))))
                (filter visible? tasks)))
      (footer
       (span #:class-name "counter"
             ,(string-append (number->string left) (if (= left 1) " task left" " tasks left")))
       (div #:class-name "filters"
            ,(map (lambda (f)
                    (html (button #:key ,(car f)
                                  #:class-name ,(if (string=? (car f) current-filter) "selected" "")
                                  #:on-click ,(lambda (event) (set-filter! (car f)))
                                  ,(cadr f))))
                  filters))
       (button #:on-click ,(lambda (event) (clear-done!)) "Clear done")))))

  (html
   (main #:class-name "app"
         (h1 "Tasks")
         ,(cond
           (tasks (task-list))
           (error (html (p #:class-name "errors" error)))
           (else (html (p "Loading…")))))))
