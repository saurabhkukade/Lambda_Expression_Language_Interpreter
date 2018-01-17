(define ps '(lambda (x y) ((lambda (z) (x (y z))) x)))
(define ls '(lambda (x y) ((x (y z)) x)))

(define (lexical_address in-exp)
    (cond
        ((symbol? in-exp) in-exp)
        ((null? in-exp) in-exp)
        ((list? (car in-exp)) (cons (lexical_address (car in-exp)) (lexical_address (cdr in-exp))))
        ((eq? (car in-exp) 'lambda)
            (let ((new_exp (change (cadr in-exp) (caddr in-exp) 1)))
                (cons 'lambda (cons (length(cadr in-exp)) (lexical_address new_exp)))))
        ((symbol? (car in-exp)) (cons (lexical_address (car in-exp)) (lexical_address (cdr in-exp))))
        (else (cons (car in-exp) (lexical_address (cdr in-exp))))
    )
)

(define (change binds ls index)
    (define (changeOne sym lvIndex ls)
        (cond
            ((null? ls) ls)
            ((eq? (car ls) sym) (cons lvIndex (changeOne sym lvIndex (cdr ls))))
            ((list? (car ls)) (cons (changeOne sym lvIndex (car ls)) (changeOne sym lvIndex (cdr ls))))
            ((eq? (car ls) 'lambda)
                (cons 'lambda (cons (cadr ls) (cons (changeOne sym (cons (+ 1 (car lvIndex)) (cdr lvIndex)) (caddr ls)) '()))))
            (else (cons (car ls) (changeOne sym lvIndex (cdr ls))))
        )
    )
    (cond
        ((null? (car ls)) ls)
        ((null? binds) ls)
        (else (change (cdr binds) (changeOne (car binds) (cons 1 (cons index '())) ls) (+ index 1)))
    )
)

