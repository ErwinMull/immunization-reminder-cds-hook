#lang racket/base

;;; ================================ Import ====================================

(require json
         web-server/http

         "flu-vaccine-cds.rkt")

;;; ================================ Export ====================================

(provide app)

;;; ================================ Source ====================================

(define source
  (hasheq 'label "Immunization Reminder CDS service"))

;;; ======================== CDS service discovery =============================

(define flu-vaccine-discovery
  (hasheq 'hook "patient-view"
          'title "Flu Vaccine CDS Service"
          'description "A service that checks whether the last known influenza vaccination has expired"
          'id "flu-vaccine-check"
          'prefetch (hasheq
                     'flu-vaccination "Immunization?patient=Patient/{{context.patientId}}&vaccine-code=86198006&_sort:desc=date&_count=1")))

(define (cds-service-discovery req)
  (response/jsexpr
   (hasheq 'services
           (list
            flu-vaccine-discovery))))

;;; ======================= Flu vaccination service ============================

(define (get-first-from-bundle-entries fhir-bundle-jsexpr)
  (define entries (hash-ref fhir-bundle-jsexpr 'total '()))
  (and (> (length entries) 0)
       (car entries)))

(define (flu-vaccine-service req)
  (define body (request-post-data/raw req))
  (define parsed (bytes->jsexpr body))
  (define prefetch (hash-ref parsed 'prefetch (hasheq)))
  (define flu-vaccination (hash-ref prefetch 'flu-vaccination #f))
  (cond
    [(not flu-vaccination) (precondition-failed req)]
    [else
     (define first-entry (get-first-from-bundle-entries flu-vaccination))
     (define result
       (check-fhir-immunization (if result
                                    (hash-ref first-entry 'resource (hasheq))
                                    (hasheq))))
     (response/jsexpr
      (hasheq 'cards
              (list
               (hasheq 'indicator (result-indicator result)
                       'summary (result-message result)
                       'source source))))]))

;;; ============================= Other routes =================================

(define (precondition-failed req)
  (response/full
   412 #"Precondition Failed"
   (current-seconds) #"text/plain; charset=utf-8"
   (list)
   (list #"Precondition Failed")))

(define (not-found req)
  (response/full
   404 #"Not Found"
   (current-seconds) #"text/plain; charset=utf-8"
   (list)
   (list #"Not Found")))

(define (options req)
  (response/full
   204 #"No Content"
   (current-seconds) #"text/plain; charset=utf-8"
   (list)
   (list)))

;;; ================================= CORS =====================================

(define cors-headers
  (list (make-header #"Access-Control-Allow-Origin"
                     #"https://sandbox.cds-hooks.org")
        (make-header #"Access-Control-Allow-Methods"
                     #"GET, POST, OPTIONS")
        (make-header #"Access-Control-Allow-Headers"
                     #"Content-Type, Authorization")))

(define (add-cors resp)
  (struct-copy response resp
               [headers (append cors-headers (response-headers resp))]))

;;; ========================== Top level routing ===============================

(define-values (cds-dispatch cds-url)
  (dispatch-rules
   [("cds-services") #:method "get" cds-service-discovery]
   [("cds-services" "flu-vaccine-check") #:method "post" flu-vaccine-service]
   [else not-found]))

(define (app req)
  (add-cors
   (if (bytes=? (request-method req) #"OPTIONS")
       (options req)
       (cds-dispatch req))))
