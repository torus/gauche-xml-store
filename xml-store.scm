(use text.tree)
(use sxml.tools)
(use sxml.ssax)

(define new-id
  (let ((i 0))
    (lambda ()
      (inc! i)
      i)))

(define p
  (ssax:make-parser
   NEW-LEVEL-SEED (lambda (elem-gi attributes namespaces expected-content seed)
                    (let ((id (new-id)))
                      (print `(,id ,elem-gi ,seed))
                      (cons id seed)))
   FINISH-ELEMENT (lambda (elem-gi attributes namespaces parent-seed seed)
                    (print `(/ ,(car seed) ,elem-gi ,parent-seed ,seed))
                    parent-seed)
   CHAR-DATA-HANDLER (lambda (string1 string2 seed))
   ))

(define doc
  (sxml:sxml->xml '(a (b (c)) (d) (e))))

(p (open-input-string (tree->string doc)) ())
