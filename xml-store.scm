(use text.tree)
(use sxml.tools)
(use sxml.ssax)
(use dbm)
(use dbm.fsdbm)

(define (make-new-id)
  (let ((i 0))
    (lambda ()
      (inc! i)
      i)))

;; seed (id-stack . proc)

(define (set-parent! id parent-id)
  (print `(set-parent! ,id ,parent-id)))

(define (set-next-sibling! id sibling-id)
  (print `(set-next-sibling! ,id ,sibling-id)))

(define (eat-new-level-seed new-id)
  (lambda (elem-gi attributes namespaces expected-content seed)
    (let ((id (new-id))
          (parent-id (car (car seed)))
          (proc (cdr seed)))
      (print `(,id ,elem-gi parent: ,parent-id ,attributes ,expected-content))
      (proc id)
      (cons (cons id (car seed)) (lambda (child) (set-parent! child id))))))

(define (eat-finish-element elem-gi attributes namespaces parent-seed seed)
  (let ((id (car (car seed)))
        (parent-id (cadr (car seed))))
    (print `(/ ,id ,elem-gi parent: ,parent-id))
    (cons (car parent-seed)
          (lambda (sibling)
            (set-parent! sibling parent-id)
            (set-next-sibling! id sibling)))))

(define (p new-id)
  (ssax:make-parser
   NEW-LEVEL-SEED (eat-new-level-seed new-id)

   FINISH-ELEMENT eat-finish-element

   CHAR-DATA-HANDLER (lambda (string1 string2 seed)
                       seed)
   ))

(define doc
  (sxml:sxml->xml '(a (b (c)) (d (@ (a 123))) (e))))

((p (make-new-id)) (open-input-string (tree->string doc)) (cons (list 'TOP) (lambda (id) (print `(TOP ,id)))))
