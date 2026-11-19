;; -*- mode: scheme; coding: utf-8; -*-

;;
;; Task manager server.
;;
;; It serves a JSON API to manage the tasks (stored in SQLite) and the task
;; manager web page, whose code is written in GuileScript (vanilla/ and
;; shared/) and compiled into JavaScript when the server starts.
;;
;; The task validation (shared/tasks/validation.gs) is used both by the web
;; page (compiled into JavaScript) and by this server (as a Guile module).
;;
;; Run it from this directory:
;;
;;   guile server.scm
;;
;; and go to http://localhost:8080. The PORT and TASKS_DB environment
;; variables change the port and the database file (tasks.db).
;;
;; It needs GuileScript, guile-json and guile-sqlite3.
;;

;; The shared GuileScript modules are also Guile modules.
(eval-when (expand load eval)
  (define here (dirname (canonicalize-path (car (command-line)))))
  (add-to-load-path (string-append here "/shared"))
  (set! %load-extensions (cons ".gs" %load-extensions)))

(use-modules (ice-9 match)
             (ice-9 textual-ports)
             (json)
             (rnrs bytevectors)
             (sqlite3)
             (sxml simple)
             (web request)
             (web response)
             (web server)
             (web uri)
             (language guilescript compile)
             (tasks validation))

;;
;; Database
;;

(define db (sqlite-open (or (getenv "TASKS_DB") "tasks.db")))

(sqlite-exec db "CREATE TABLE IF NOT EXISTS tasks (
                   id INTEGER PRIMARY KEY,
                   title TEXT NOT NULL,
                   done INTEGER NOT NULL DEFAULT 0)")

;; The rows returned by the SQL statement, as JSON objects (alists).
(define (query sql . args)
  (let ((statement (sqlite-prepare db sql)))
    (apply sqlite-bind-arguments statement args)
    (let ((rows (sqlite-map (match-lambda
                              (#(id title done)
                               `(("id" . ,id) ("title" . ,title) ("done" . ,(= done 1)))))
                            statement)))
      (sqlite-finalize statement)
      rows)))

(define (all-tasks)
  (query "SELECT id, title, done FROM tasks ORDER BY id"))

(define (create-task title)
  (car (query "INSERT INTO tasks (title) VALUES (?) RETURNING id, title, done" title)))

;; TITLE and DONE are #f if they don't change. Returns #f if there's no such
;; task.
(define (update-task id title done)
  (match (query "UPDATE tasks
                 SET title = coalesce(?, title), done = coalesce(?, done)
                 WHERE id = ? RETURNING id, title, done"
                title (match done (#t 1) (#f 0) ('none #f)) id)
    ((task) task)
    (() #f)))

(define (delete-task id)
  (pair? (query "DELETE FROM tasks WHERE id = ? RETURNING id, title, done" id)))

;;
;; Client (GuileScript modules compiled into JavaScript)
;;

;; Module path (e.g. "tasks/api") -> (JS . SOURCE-MAP)
(define scripts
  (let ((table (make-hash-table)))
    (for-each (match-lambda
                ((path js source-map)
                 (hash-set! table path
                            (cons (string-append js "//# sourceMappingURL="
                                                 (basename path) ".js.map\n")
                                  source-map))))
              (compile-modules (list (string-append here "/vanilla/app.gs"))
                               #:load-path (list (string-append here "/shared"))
                               #:source-map? #t))
    table))

;;
;; Responses
;;

(define* (response content-type body #:key (code 200))
  (values (build-response #:code code #:headers `((content-type . ,content-type)))
          body))

(define* (json-response data #:key (code 200))
  (response '(application/json) (scm->json-string data) #:code code))

(define (errors-response code errors)
  (json-response `(("errors" . ,(list->vector errors))) #:code code))

(define (not-found)
  (errors-response 404 '("Not found.")))

(define (index-page)
  (response '(text/html (charset . "utf-8"))
            (lambda (port)
              (display "<!DOCTYPE html>\n" port)
              (sxml->xml
               '(html (@ (lang "en"))
                      (head (meta (@ (charset "utf-8")))
                            (meta (@ (name "viewport")
                                     (content "width=device-width, initial-scale=1")))
                            (title "Tasks")
                            (link (@ (rel "stylesheet") (href "/style.css"))))
                      (body (div (@ (id "app")) "")
                            (script (@ (type "module") (src "/js/app.js")) "")))
               port))))

(define (script-response path)
  (define (script suffix)
    (and (string-suffix? suffix path)
         (hash-ref scripts (string-drop-right path (string-length suffix)))))
  (cond
   ((script ".js.map") => (match-lambda
                            ((js . source-map) (response '(application/json) source-map))))
   ((script ".js") => (match-lambda
                        ((js . source-map)
                         (response '(text/javascript (charset . "utf-8")) js))))
   (else (not-found))))

(define (style-response)
  (response '(text/css (charset . "utf-8"))
            (call-with-input-file (string-append here "/style.css") get-string-all)))

;;
;; API
;;

(define (request-json body)
  (catch #t
    (lambda ()
      (match (json-string->scm (utf8->string body))
        (((key . value) ...) (map cons key value))
        (_ #f)))
    (const #f)))

(define (with-task-id id handler)
  (match (string->number id)
    ((? exact-integer? id) (handler id))
    (_ (not-found))))

(define (post-task body)
  (let* ((data (or (and body (request-json body)) '()))
         (title (assoc-ref data "title"))
         (errors (task-title-errors title)))
    (if (null? errors)
        (json-response (create-task (string-trim-both title)) #:code 201)
        (errors-response 400 errors))))

(define (patch-task id body)
  (let* ((data (or (and body (request-json body)) '()))
         (title (assoc "title" data))
         (done (assoc "done" data))
         (errors (append (if title (task-title-errors (cdr title)) '())
                         (if (and done (not (boolean? (cdr done))))
                             '("Done must be true or false.")
                             '()))))
    (if (null? errors)
        (match (update-task id
                            (and title (string-trim-both (cdr title)))
                            (if done (cdr done) 'none))
          (#f (not-found))
          (task (json-response task)))
        (errors-response 400 errors))))

;;
;; Server
;;

(define (handler request body)
  (match (cons (request-method request)
               (split-and-decode-uri-path (uri-path (request-uri request))))
    (('GET) (index-page))
    (('GET "style.css") (style-response))
    (('GET "js" path ...) (script-response (string-join path "/")))
    (('GET "api" "tasks") (json-response (list->vector (all-tasks))))
    (('POST "api" "tasks") (post-task body))
    (('PATCH "api" "tasks" id) (with-task-id id (lambda (id) (patch-task id body))))
    (('DELETE "api" "tasks" id)
     (with-task-id id (lambda (id)
                        (if (delete-task id)
                            (values (build-response #:code 204) "")
                            (not-found)))))
    (_ (not-found))))

(define port (string->number (or (getenv "PORT") "8080")))

(format #t "Task manager on http://localhost:~a~%" port)
(run-server handler 'http `(#:port ,port))
