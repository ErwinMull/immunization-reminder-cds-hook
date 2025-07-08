#lang racket/base

;;; =============================== Imports ====================================

(require gregor
         racket/string

         "lens.rkt")

;;; =============================== Exports ====================================

(provide (struct-out result)
         check-fhir-immunization
         exn:fail:flu?)

;;; ============================== Exceptions ==================================

(struct exn:fail:flu exn ()
  #:extra-constructor-name make-exn:fail:flu
  #:transparent)

(define (raise-exn:fail:flu message)
  (raise (exn:fail:flu message (current-continuation-marks))))

;;; =========================== Result structure ===============================

(struct result
  (indicator message)
  #:extra-constructor-name make-result
  #:transparent)

;;; ===================== Flu vaccination expiry check =========================

(define (check-influenza-vaccination-expired-date expired-date)
  (define days (days-between expired-date (today)))
  (cond
    [(>= days 0)
     (make-result "critical"
                  (format "Influenza vaccination expired since ~a days"
                          days))]
    [(>= days -30)
     (make-result "warning"
                  (format "Influenza vaccination expires in ~a days"
                          (abs days)))]
    [else
     (make-result "info"
                  (format "Influenza vaccination still active for ~a days"
                          (abs days)))]))

(define (check-influenza-vaccination status expired)
  (cond
    [(not (string=? status "completed"))
     (make-result "critical"
                  "Last influenza vaccination not completed")]
    [expired (check-influenza-vaccination-expired-date expired)]
    [else
     (raise-exn:fail:flu
      (format "Invalid Immunization: completed but no datetimes given!"))]))

;;; ====================== FHIR immunization handling ==========================

(define status-lens
  (&hash 'status
         (λ ()
           (raise-exn:fail:flu "Invalid Immunization: no status"))))
(define expiration-lens
  (&hash 'expirationDate
         (λ ()
           (raise-exn:fail:flu "Invalid Immunization: no expiration date"))))

(define (check-fhir-immunization fhir-immunization-jsexpr)
  (let ([status (status-lens fhir-immunization-jsexpr)]
        [expired (iso8601->date (expiration-lens fhir-immunization-jsexpr))])
    (check-influenza-vaccination status expired)))
