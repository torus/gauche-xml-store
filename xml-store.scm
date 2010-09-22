(use util.list)
(use text.tree)
(use sxml.tools)
(use sxml.ssax)
(use dbm)
(use dbm.fsdbm)

(define (make-new-id)
  (let ((i 0))
    (lambda ()
      (inc! i)
      (number->string i))))

;; seed (id-stack . proc)

(define (set-next-sibling! db id sibling-id)
  (let1 data (assoc-set! (read-from-string (dbm-get db id)) 'next-sibling sibling-id)
    (dbm-put! db id (write-to-string data)))
  (print `(set-next-sibling! ,id ,sibling-id)))

(define (add-element! db id elem-gi parent-id attributes)
  (let1 data (list (cons 'id id)
                   (cons 'elem-gi elem-gi)
                   (cons 'parent parent-id)
                   (cons 'attributes attributes))
    (dbm-put! db id (write-to-string data)))
  (print `(add-element! ,id ,elem-gi <- ,parent-id ,attributes)))

(define (eat-new-level-seed db new-id)
  (lambda (elem-gi attributes namespaces expected-content seed)
    (let ((id (new-id))
          (parent-id (car (car seed)))
          (proc (cdr seed)))
      (add-element! db id elem-gi parent-id attributes)
      (proc id)
      (cons (cons id (car seed))
            (lambda (child-id)
              (let1 data (read-from-string (dbm-get db id))
                (when (not (assoc 'first-child data))
                  (let1 data2 (assoc-set! data 'first-child child-id)
                    (dbm-put! db id (write-to-string data2)))
                  )))))))

(define (eat-char-data new-level-handler finish-elem-handler)
  (lambda (string1 string2 seed)
    (let* ((attr `((content . ,string1)))
           (new-seed (new-level-handler '*TEXT* attr () () seed)))
      (finish-elem-handler '*TEXT* attr () seed new-seed))))

(define (eat-finish-element db)
  (lambda (elem-gi attributes namespaces parent-seed seed)
    (let ((id (car (car seed)))
          (parent-id (cadr (car seed))))
      ;; (print `(/ ,id ,elem-gi parent: ,parent-id))
      (cons (car parent-seed)
            (lambda (sibling)
              (set-next-sibling! db id sibling))))))

(define (p db new-id)
  (let ((new-level (eat-new-level-seed db new-id))
        (finish-elem (eat-finish-element db)))
    (ssax:make-parser
     NEW-LEVEL-SEED new-level
     FINISH-ELEMENT finish-elem
     CHAR-DATA-HANDLER (eat-char-data new-level finish-elem)
     )))

(define doc
  (sxml:sxml->xml '(a (b "text content"
                         (c))
                      (d (@ (a 123))
                         "another content")
                      (e))))

((p (dbm-open <fsdbm> :path "hoge" :rw-mode :write)
    (make-new-id))
 (open-input-string (tree->string doc))
 (cons (list 'TOP)
       (lambda (id) (print `(TOP ,id)))))
