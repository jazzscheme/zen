;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Kernel-Level Crash Handler
;;;


(block kernel.crash


(define (zen:log-backtrace ignore)
  (continuation-capture
    (lambda (k)
      (let ((port (current-error-port)))
        (display "Frames:" port)
        (newline port)
        ;; dump frames
        (display-continuation-backtrace k port)
        (newline port)
        (display "Variables:" port)
        (newline port)
        ;; dump variables
        (display-continuation-backtrace k port)))))


(zen:set-crash-reporter zen:log-backtrace)


(cond-expand
  (windows
    
    (define SEM_FAILCRITICALERRORS #x0001)
    
    (define SEM_NOGPFAULTERRORBOX #x0002)
    
    (c-external (SetErrorMode unsigned-int) unsigned-int)
    
    (define (zen:disable-crash-window)
      (SetErrorMode (bitwise-ior SEM_FAILCRITICALERRORS SEM_NOGPFAULTERRORBOX)))
    
    (c-definition (zen:call_crash_reporter ignore) ((pointer void)) void "zen_call_crash_reporter" "static"
      (zen:crash-reporter ignore))

    (c-declaration crash #<<end-of-code
      static void restore_low_level_windows_crash_handler()
      {
        SetUnhandledExceptionFilter(NULL);
      }
      
      static LONG WINAPI unhandled_exception_filter(LPEXCEPTION_POINTERS info)
      {
        restore_low_level_windows_crash_handler();
        zen_call_crash_reporter(info);
        return EXCEPTION_CONTINUE_SEARCH;
      }

      static void setup_low_level_windows_crash_handler()
      {
        SetUnhandledExceptionFilter(unhandled_exception_filter);
      }
end-of-code
    )
    (c-initialization crash "setup_low_level_windows_crash_handler();")
    
    (c-declaration crash "const DWORD CRASH_PROCESS = (DWORD) 0xE0000001L;")
    
    (define zen:enable-crash-handler
      (c-lambda () void "setup_low_level_windows_crash_handler"))
    
    (define zen:disable-crash-handler
      (c-lambda () void "restore_low_level_windows_crash_handler"))
    
    (c-external (zen:crash-process) void
      "RaiseException(CRASH_PROCESS, EXCEPTION_NONCONTINUABLE , 0, NULL);"))
  (else
   
   (define (zen:disable-crash-window)
     #!void)
   
   (c-definition (zen:call_crash_reporter ignore) (int) void "zen_call_crash_reporter" "static"
     (zen:crash-reporter ignore))

   (c-definition (zen:call_crash_exit) () void "zen_call_crash_exit" "static"
     (zen:exit-no-jobs))

   (c-declaration crash #<<end-of-code
      #include <stdio.h>
      #include <unistd.h>
      #include <sys/types.h>
      #include <signal.h>
      #include <execinfo.h>
      
      static void restore_low_level_unix_signals()
      {
       // core dumping signals
        signal(SIGQUIT, SIG_DFL);
        signal(SIGILL,  SIG_DFL);
        signal(SIGABRT, SIG_DFL);
        signal(SIGFPE,  SIG_DFL);
        signal(SIGBUS,  SIG_DFL);
        signal(SIGSEGV, SIG_DFL);
        signal(SIGSYS,  SIG_DFL);
        signal(SIGTRAP, SIG_DFL);
        }

      static void error_signal_handler(int sig_num)
      {
        restore_low_level_unix_signals();
                                      
        // Dump the C stack which is only really useful if
        // Gambit was built with --enable-debug for symbols
        // void *ret_adrs[100];
        // ___SIZE_T n = backtrace (ret_adrs, sizeof (ret_adrs) / sizeof (void*));
        // backtrace_symbols_fd (ret_adrs, n, STDOUT_FILENO);
        // printf("\n");
        
        zen_call_crash_reporter(sig_num);
        fflush(stdout);
        zen_call_crash_exit();
      }

      static void setup_low_level_unix_crash_handler()
      {
        // core dumping signals
        signal(SIGQUIT, error_signal_handler);
        signal(SIGILL,  error_signal_handler);
        signal(SIGABRT, error_signal_handler);
        signal(SIGFPE,  error_signal_handler);
        signal(SIGBUS,  error_signal_handler);
        signal(SIGSEGV, error_signal_handler);
        signal(SIGSYS,  error_signal_handler);
        signal(SIGTRAP, error_signal_handler);
      }
end-of-code
   )

   (c-initialization crash "setup_low_level_unix_crash_handler();")
   
   (define zen:enable-crash-handler
     (c-lambda () void "setup_low_level_unix_crash_handler"))
   
   (define zen:disable-crash-handler
     (c-lambda () void "restore_low_level_unix_signals"))

   (c-external (zen:crash-process) void
     "raise(SIGSEGV);"))))
