;; chicken-clone [-chicken-release 5] [-egg-release 1.2.3] colorize [directory]

(import (only (srfi 1) filter-map)
        (srfi 193)
        (only (chicken io) read-string)
        (only (http-client) with-input-from-request))

(define (writeln x) (write x) (newline))

(define (disp . xs) (for-each display xs) (newline))

(define (read-all)
  (let loop ((forms '()))
    (let ((form (read)))
      (if (eof-object? form) (reverse forms) (loop (cons form forms))))))

(define (expand-template string alist)
  (let outer ((a 0) (result ""))
    (cond ((= a (string-length string))
           result)
          ((char=? #\} (string-ref string a))
           (error "Stray } in template"))
          ((char=? #\{ (string-ref string a))
           (let ((a (+ a 1)))
             (let inner ((b a))
               (cond ((char=? #\{ (string-ref string b))
                      (error "Double { in template"))
                     ((char=? #\} (string-ref string b))
                      (let* ((var (string->symbol (substring string a b)))
                             (entry (assq var alist)))
                        (if entry
                            (outer (+ b 1) (string-append result (cdr entry)))
                            (error "Template var not defined" var))))
                     (else
                      (inner (+ b 1)))))))
          (else
           (outer (+ a 1) (string-append result
                                         (substring string a (+ a 1))))))))

;;

(define chicken-releases '("4" "5"))

(define (egg-locations-url chicken-release)
  (string-append "https://anonymous@code.call-cc.org/"
                 "svn/chicken-eggs/release/"
                 chicken-release "/egg-locations"))

(define (raw-egg-locations->alist chicken-release raw-egg-locations)
  (map (lambda (entry)
         (let* ((egg-name (symbol->string (list-ref entry 0)))
                (url (expand-template
                      (list-ref entry 1)
                      (list (cons 'chicken-release chicken-release)
                            (cons 'egg-name egg-name)))))
           (cons egg-name url)))
       raw-egg-locations))

(define (get-egg-release-info-url-alist chicken-release)
  (let ((url (egg-locations-url chicken-release)))
    (raw-egg-locations->alist chicken-release
                              (with-input-from-request url #f read-all))))

(define (get-egg-release-info-url chicken-release egg-name)
  (let ((entry (assoc egg-name (get-egg-release-info-url-alist
                                chicken-release))))
    (and entry (cdr entry))))

(define (get-egg-release-alist chicken-release egg-name)
  (let* ((release-info-url (get-egg-release-info-url chicken-release egg-name))
         (alist (with-input-from-request release-info-url #f read-all))
         (uri-entry (or (assq 'uri alist)
                        (error "No uri entry for" egg-name)))
         (uri-kind (list-ref uri-entry 1))
         (uri-template (list-ref uri-entry 2))
         (egg-releases (filter-map (lambda (entry)
                                     (and (eq? 'release (list-ref entry 0))
                                          (list-ref entry 1)))
                                   alist)))
    (map (lambda (egg-release)
           (list egg-release
                 uri-kind
                 (expand-template
                  uri-template
                  (list (cons 'chicken-release chicken-release)
                        (cons 'egg-name egg-name)
                        (cons 'egg-release egg-release)))))
         egg-releases)))

(define (list-all)
  (let ((chicken-release "5"))
    (for-each (lambda (entry)
                (let* ((egg-name (symbol->string (list-ref entry 0)))
                       (url (expand-template
                             (list-ref entry 1)
                             (list (cons 'chicken-release chicken-release)
                                   (cons 'egg-name egg-name)))))
                  (disp url)))
              (download-and-parse-egg-locations "5"))))

(define (main)
  (let ((chicken-release "5")
        (egg-name (list-ref (command-args) 0)))
    (for-each writeln (get-egg-release-alist chicken-release egg-name))))

(main)
