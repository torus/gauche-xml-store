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

#;(define (set-parent! db id parent-id)
  (print `(set-parent! ,id ,parent-id)))

(define (set-next-sibling! db id sibling-id)
  (print `(set-next-sibling! ,id ,sibling-id)))

(define (add-element! db id elem-gi parent-id attributes)
  (print `(add-element! ,id ,elem-gi <- ,parent-id ,attributes)))

(define (eat-new-level-seed db new-id)
  (lambda (elem-gi attributes namespaces expected-content seed)
    (let ((id (new-id))
          (parent-id (car (car seed)))
          (proc (cdr seed)))
      (add-element! db id elem-gi parent-id attributes)
      (proc id)
      (cons (cons id (car seed)) (lambda (child) #;(set-parent! db child id))))))

(define (eat-finish-element db)
  (lambda (elem-gi attributes namespaces parent-seed seed)
    (let ((id (car (car seed)))
          (parent-id (cadr (car seed))))
      ;; (print `(/ ,id ,elem-gi parent: ,parent-id))
      (cons (car parent-seed)
            (lambda (sibling)
              #;(set-parent! db sibling parent-id)
              (set-next-sibling! db id sibling))))))

(define (p db new-id)
  (ssax:make-parser
   NEW-LEVEL-SEED (eat-new-level-seed db new-id)
   FINISH-ELEMENT (eat-finish-element db)
   CHAR-DATA-HANDLER (lambda (string1 string2 seed)
                       seed)
   ))

(define doc
  (sxml:sxml->xml '(a (b (c)) (d (@ (a 123))) (e))))

((p () (make-new-id))
 (open-input-string (tree->string doc))
 (cons (list 'TOP)
       (lambda (id) (print `(TOP ,id)))))
