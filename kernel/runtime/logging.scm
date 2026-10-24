;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Kernel-Level Logging
;;;


(block kernel.logging


(c-declare #<<END-OF-DECLARE
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>

const char* logging_filename = "log.txt";

static bool is_logging = true;

static FILE *logging_stream = NULL;

static int logging_line(const char *line)
{
  if (!is_logging)
    return 0;
  
  va_list ap;
  int result;
  FILE *stream;
  
  if (!logging_stream)
    logging_stream = fopen(logging_filename, "w");

  stream = logging_stream;
  
  fprintf (stream, "%s\n", line);

  fflush (stream);

  return result;
}

static int logging(const char *format, ...)
{
  if (!is_logging)
    return 0;
  
  va_list ap;
  int result;
  FILE *stream;
  
  if (!logging_stream)
    logging_stream = fopen(logging_filename, "w");

  stream = logging_stream;
  
  va_start (ap, format);
  result = vfprintf (stream, format, ap);
  va_end (ap);

  fflush (stream);

  return result;
}
END-OF-DECLARE
)


(define enter-marker
  ";---> ")

(define exit-marker
  ";<--- ")


(define scheme-marker
  "")

(define c-marker
  " (C)")


(define zen:logging-depth
  (make-parameter 0))


(define zen:logging?
  (c-lambda () bool
    "___return(is_logging);"))

(define zen:set-logging?
  (c-lambda (bool) void
    "is_logging = ___arg1;"))


(define zen:logging-line
  (c-lambda (char-string) int
    "logging_line"))


(define (zen:logging-scheme->c name thunk . args)
  (define (marshall obj)
    (if (foreign? obj)
        (object->serial-number obj)
      (list 'quote obj)))
  
  (if (%%not (zen:logging?))
      (thunk)
    (let ((str (%%symbol->string name))
          (prefix (make-string (%%fx* (zen:logging-depth) 2) #\space)))
      (zen:logging-line (with-output-to-string ""
                           (lambda ()
                             (display "(")
                             (write (cons name (map marshall args))))))
      (let ((result (parameterize ((zen:logging-depth (%%fx+ (zen:logging-depth) 1)))
                      (thunk))))
        (zen:logging-line (with-output-to-string ""
                             (lambda ()
                               (display " ")
                               (write (marshall result))
                               (display ")"))))
        result))))


(define (zen:logging-c->scheme name thunk)
  (if (%%not (zen:logging?))
      (thunk)
    (let ((str (%%symbol->string name))
          (prefix (make-string (%%fx* (zen:logging-depth) 2) #\space)))
      (zen:logging-line (%%string-append prefix enter-marker str scheme-marker))
      (let ((result (parameterize ((zen:logging-depth (%%fx+ (zen:logging-depth) 1)))
                      (thunk))))
        (zen:logging-line (%%string-append prefix exit-marker str c-marker))
        result)))))
