#lang racket/base

;;; =============================== Imports ====================================

(require racket/list)

;;; =============================== Exports ====================================

(provide (struct-out lens)
         lens-compose
         lens-thread

         &hash
         &list

         (struct-out exn:fail:lens))

;;; ============================== Exceptions ==================================

(struct exn:fail:lens exn ()
  #:extra-constructor-name make-exn:fail:lens
  #:transparent)

(define (raise-exn:fail:lens message)
  (raise (exn:fail:lens message (current-continuation-marks))))

;;; ================================= Lens =====================================

(struct lens (getter setter)
  #:property prop:procedure (case-lambda
                              [(l o) ((lens-getter l) o)]
                              [(l o v) ((lens-setter l) o v)]))

(define (lens-compose l1 l2)
  (lens
   (λ (o)
     (l2 (l1 o)))
   (λ (o v)
     (l1 o (l2 (l1 o) v)))))

(define lens-thread
  (case-lambda
    [(l1 l2) (lens-compose l1 l2)]
    [(l1 l2 . args) (lens-compose l1 (apply lens-thread l2 args))]))

;;; ============================== Utilities ===================================

(define (&hash k [exception (λ ()
                              (raise-exn:fail:lens
                               (format
                                "Lens requires a hash table with key '~a'"
                                k)))])
  (lens
   (λ (o)
     (hash-ref o k (λ () (raise (exception)))))
   (λ (o v)
     (hash-set o k v))))

(define (&list i [exception (λ ()
                              (raise-exn:fail:lens
                               (format
                                "Lens requires a list with at least ~a elements"
                                (add1 i))))])
  (lens
   (λ (o)
     (with-handlers ([exn:fail:contract? (λ (e)
                                           (exception))])
       (list-ref o i)))
   (λ (o v)
     (with-handlers ([exn:fail:contract? (λ (e)
                                           (exception))])
       (list-set o i v)))))
