;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Kernel Install
;;;


(block kernel.install


(cond-expand
  (mac
    (c-include "<mach-o/dyld.h>"))
  (linux
    (c-include "<unistd.h>")
    (c-include "<stdio.h>"))
  (else))


(cond-expand
  (windows
   (c-include "<shlobj.h>")
   
   (define CSIDL_PERSONAL          5)
   (define CSIDL_DESKTOPDIRECTORY 16)
   (define CSIDL_APPDATA          26)
   (define CSIDL_LOCAL_APPDATA    28)
   (define CSIDL_PROGRAM_FILES    38)
   
   (c-external (get-special-folder int) wchar_t-string
     "wchar_t szDir[MAX_PATH];
      SHGetSpecialFolderPathW(0, szDir, ___arg1, FALSE);
      ___return(szDir);"))
  (else))


(cond-expand
  (mac
    (c-external (zen:platform-executable-path) char-string
            "char path[1024];
             uint32_t pathLength = 1023;
             _NSGetExecutablePath(path,&pathLength);
             path[1023] = 0;
             ___return(path);"))
  (windows
    (c-external (zen:platform-executable-path) wchar_t-string
            "wchar_t buf[MAX_PATH];
             GetModuleFileNameW(NULL, buf, 300);
              ___return(buf);"))
  (linux
    (c-external (zen:platform-executable-path) char-string
            "char link[64];
             char path[1024];
             pid_t pid;
             int result;
             pid = getpid();
             result = snprintf(link, sizeof(link), \"/proc/%i/exe\", pid);
             if (result >= 0) {
               result = readlink(link, path, sizeof(path));
               if( result < 0 || result >= (int)sizeof(path) )
                 ___return(0);
               else {
                 path[result] = 0;
                 ___return(path);
               }
             }
             else
               ___return(0);"))
  (else))


(zen:define-variable-override zen:executable-path
  (lambda ()
    (zen:platform-executable-path)))


(zen:define-variable-override zen:executable-directory
  (lambda ()
    (let ((path (zen:platform-executable-path)))
      (if path
          (zen:pathname-dir (zen:pathname-normalize path))
        #f)))))
