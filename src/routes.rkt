#lang racket/base

;;; ================================ Import ====================================

(require json
         web-server/http
         web-server/dispatch

         "flu-vaccine-cds.rkt"
         "lens.rkt")

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

(define &prefetch (&hash 'prefetch))
(define &flu (&hash 'flu-vaccination))
(define &entry (&hash 'entry))
(define &0 (&list 0))
(define &resource (&hash 'resource))

(define &fhir-immunization-resource (lens-thread &prefetch
                                                 &flu
                                                 &entry
                                                 &0
                                                 &resource))

(define (flu-vaccine-service req)
  (define parsed (bytes->jsexpr (request-post-data/raw req)))
  (with-handlers ([exn:fail:lens? (λ (e)
                                    (response/jsexpr
                                     (hasheq 'card
                                             (list
                                              (hasheq 'indicator "ciritcal"
                                                      'summary "Data error"
                                                      'details (exn-message e)
                                                      'source source)))))]
                  [exn:fail:flu? (λ (e)
                                   (response/jsexpr
                                    (hasheq 'cards
                                            (list
                                             (hasheq 'indicator "critical"
                                                     'summary "Workflow error"
                                                     'details (exn-message e)
                                                     'source source)))))])
    (define fhir-immunization-resource (&fhir-immunization-resource parsed))
    (define result
      (check-fhir-immunization fhir-immunization-resource))
    (response/jsexpr
     (hasheq 'cards
             (list
              (hasheq 'indicator (result-indicator result)
                      'summary (result-message result)
                      'source source))))))

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
