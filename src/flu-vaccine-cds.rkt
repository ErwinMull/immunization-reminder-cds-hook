#lang racket/base

;;; =============================== Imports ====================================

(require gregor
         racket/string)

;;; =============================== Exports ====================================

(provide (struct-out result)
         check-fhir-immunization)

;;; =========================== Result structure ===============================

(struct result
  (indicator message)
  #:extra-constructor-name make-result
  #:transparent)

;;; ===================== Flu vaccination expiry check =========================

(define (check-influenza-vaccination-expired-date expired-date)
  (define days (days-between (today) expired-date))
  (cond
    [(>= days 0)
     (make-result 'critical
                  (format "Influenza vaccination expired since ~a days"
                          days))]
    [(>= days -30)
     (make-result 'warning
                  (format "Influenza vaccination expires in ~a days"
                          (abs days)))]
    [else
     (make-result 'info
                  (format "Influenza vaccination still active for ~a days"
                          (abs days)))]))

(define (check-influenza-vaccination-occurred-date occurred-date)
  (define days (days-between occurred-date (today)))
  (cond
    [(>= days 365)
     (make-result 'critical
                  (format "Last influenza vaccination taken ~a days ago"
                          days))]
    [(>= days 335)
     (make-result 'warning
                  (format "Last influenza vaccination taken ~a days ago"
                          days))]
    [else
     (make-result 'info
                  (format "Last influenza vaccination taken ~a days ago"
                          days))]))

(define (check-influenza-vaccination status occurred expired)
  (cond
    [(not (string=? status "completed"))
     (make-result 'critical
                  "Last influenza vaccination not completed")]
    [expired (check-influenza-vaccination-expired-date expired)]
    [occurred (check-influenza-vaccination-occurred-date occurred)]
    [else (raise "TODO: custom exception?")]))

;;; ====================== FHIR immunization handling ==========================

(define (get-date-from-fhir-immunization fhir-immunization-jsexpr key)
  (let ([tmp (hash-ref fhir-immunization-jsexpr key #f)])
    (and tmp (with-handlers ([exn:gregor? (λ (exn) #f)])
               (iso8601->date tmp)))))

(define (check-fhir-immunization fhir-immunization-jsexpr)
  (let ([status (hash-ref fhir-immunization-jsexpr 'status "not-done")]
        [occurred (get-date-from-fhir-immunization fhir-immunization-jsexpr
                                                   'occurrenceDateTime)]
        [expired (get-date-from-fhir-immunization fhir-immunization-jsexpr
                                                   'expirationDate)])
    (check-influenza-vaccination status occurred expired)))
