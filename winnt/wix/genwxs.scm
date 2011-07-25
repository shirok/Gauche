(use sxml.serializer)
(use sxml.sxpath)
(use sxml.ssax)
(use sxml.tools)
(use sxml.tree-trans)
(use file.util)
(use gauche.process)
(use util.match)

(define *wix-ns* "http://schemas.microsoft.com/wix/2006/wi")

(define (wix-template version file-tree)
  `(Wix
    (@ (xmlns ,*wix-ns*))
    (Product
     (@ (Id "83a47ff2-a9a0-4e1a-8043-716f3e2c30be")
        (Name "Gauche")
        (Language "1033")
        (Version ,version)
        (Manufacturer "Shiro Kawai")
        (UpgradeCode "79ee0cec-691c-4945-b47a-10b9f53b5859"))
     (Package
      (@ (Description "Gauche Scheme Script Engine")
         (Comments "Script-friendly R5RS-conformant Scheme Implementation")
         (Manufacturer "Shiro Kawai")
         (InstallerVersion "200")
         (Compressed "yes")))
     
     (Property (@ (Id "WIXUI_INSTALLDIR") (Value "INSTALLDIR")))
     (WixVariable (@ (Id "WixUILicenseRtf") (Value "Gauche\\COPYING.rtf")))
     (Media (@ (Id "1") (Cabinet "gauche.cab") (EmbedCab "yes")))
     (Icon (@ (Id "AppIcon.ico") (SourceFile "gauche-logo.ico")))
     (Property (@ (Id "ARPPRODUCTICON") (Value "AppIcon.ico")))

     ;;
     ;; The 'meat' of the installed components
     ;;
     (Directory
      (@ (Id "TARGETDIR") (Name "SourceDir"))
      (Directory
       (@ (Id "ProgramFilesFolder"))
       ;; Files under Gauche/ directory tree
       (Directory
        (@ (Id "INSTALLDIR") (Name "Gauche"))
        ,@(sxml:content file-tree)))
      ;; Add PATH environment variables
      (Component
       (@ (Id "SetEnvPath") (Guid "ca07df2e-9e51-493a-bfe6-d7e0d861b5c9"))
       (Environment (@ (Id "UpdatePath")
                       (Name "PATH")
                       (Action "set")
                       (Part "last")
                       (Permanent "no")
                       (System "yes")
                       (Value "[INSTALLDIR]bin"))))
      ;; Associate .scm extension to Gauche
      (Component
       (@ (Id "RegistryEntries") (Guid "aad33726-68ed-46c6-91cb-a88d9ad47c01"))
       ,(regkey 'HKLM "Software\\Scheme Arts\\Gauche"
                      '("InstallDir" "[INSTALLDIR]"))
       ,(regkey 'HKCR ".scm" "Scheme.script")
       ,(regkey 'HKCR "Scheme.script" "Scheme Script")
       ,(regkey 'HKCR "Scheme.script\\shell" "open")
       ,(regkey 'HKCR "Scheme.script\\DefaultIcon"
                "[INSTALLDIR]gauche-logo.ico")
       ,(regkey 'HKCR "Scheme.script\\shell\\open\\command"
                "[INSTALLDIR]bin\\gosh-noconsole.exe \"%1\""))
      ;; Shortcuts in the start menu
      (Directory
       (@ (Id "ProgramMenuFolder"))
       (Directory
        (@ (Id "ApplicationProgramsFolder") (Name "Gauche"))
        (Component
         (@ (Id "ApplicationShortcut")
            (Guid "505cd733-bb8b-443c-9637-988d74a87c3a"))
         (Shortcut (@ (Id "ApplicationStartMenuShortcut")
                      (Name "Gauche")
                      (Description "Gauche REPL")
                      (Target "[INSTALLDIR]bin\\gosh.exe")
                      (Icon "AppIcon.ico")
                      (WorkingDirectory "INSTALLDIR")))
         (Shortcut (@ (Id "UninstallProduct")
                      (Name "Uninstall Gauche")
                      (Target "[SystemFolder]msiexec.exe")
                      (Arguments "/x [ProductCode]")
                      (Description "Uninstall Gauche")))
         (RemoveFolder (@ (Id "ApplicationProgramsFolder") (On "uninstall")))
         (RegistryValue (@ (Root "HKCU")
                           (Key "Software\\Scheme Arts\\Gauche\\StartMenu")
                           (Name "installed") (Type "integer")
                           (Value "1") (KeyPath "yes")))
         )))
      ;; Shortcut in Desktop
      (Directory
       (@ (Id "DesktopFolder") (Name "Desktop")))
      (Component
       (@ (Id "DesktopShortcut")
          (Guid "197997bb-369e-4797-9ea6-e98287592d7d"))
       (Shortcut (@ (Id "desktopGauche") (Directory "DesktopFolder")
                    (Name "Gauche") (WorkingDirectory "INSTALLDIR")
                    (Icon "AppIcon.ico") (IconIndex "0")
                    (Target "[INSTALLDIR]bin\\gosh.exe")))
       (RegistryValue (@ (Root "HKCU")
                         (Key "Software\\Scheme Arts\\Gauche\\DesktopIcon")
                         (Name "installed") (Type "integer")
                         (Value "1") (KeyPath "yes")))
       ))

     ;;
     ;; Do the work
     ;;
     (Feature (@ (Id "Complete") (Title "Gauche") (Display "expand")
                 (Description "The complete package") (Level "1")
                 (ConfigurableDirectory "INSTALLDIR"))
              ,@(map (^i `(ComponentRef (@ (Id ,i))))
                     ((sxpath '(// Component @ Id *text*)) file-tree))
              (ComponentRef (@ (Id "ApplicationShortcut")))
              (ComponentRef (@ (Id "SetEnvPath")))
              (ComponentRef (@ (Id "RegistryEntries")))
              (ComponentRef (@ (Id "DesktopShortcut")))
              )

     )))

;; vals :: Val | (name Val)
(define (regkey root key vals)
  `(RegistryKey
    (@ (Root ,(x->string root)) (Key ,key)
       (Action "createAndRemoveOnUninstall"))
    ,(match vals
       [(name val)
        `(RegistryValue (@ (Type "string") (Name ,name) (Value ,val)))]
       [val
        `(RegistryValue (@ (Type "string") (Value ,val)))])))

;;
(define (generate version file-tree out-file)
  (call-with-output-file out-file
    (^p (display "<?xml version='1.0' encoding='utf-8'?>\n" p)
        (srl:sxml->xml (wix-template version file-tree) p))))

(define (get-version)
  (process-output->string '("Gauche/bin/gauche-config" "-V")))

;; Generate an XML Directory node of teh installation tree.
;; Currently we rely on the 'heat.exe' that comes with Wix SDK.
(define (gen-file-tree)
  (let1 tmpfile "file-tree.xml"
    (run-process `("heat" "dir" "Gauche" "-nologo" "-sfrag" "-gg" "-o" ,tmpfile)
                 :wait #t)
    (let1 sxml (call-with-input-file tmpfile
                 (^p (read-char p) ; skip BOM
                     (ssax:xml->sxml p `((#f . ,*wix-ns*)))))
      (begin0
          (xlate-file-tree
           ((if-car-sxpath '(// Fragment DirectoryRef Directory)) sxml))
        (sys-unlink tmpfile)))))

(define (xlate-file-tree sxml)
  (pre-post-order sxml
                  `((File . ,(^(n . cs)
                               (let* ([node (cons n cs)]
                                      [src (sxml:attr node 'Source)]
                                      [id (sxml:attr node 'Id)])
                                 (sxml:change-attrlist
                                  node
                                  `((Name ,(sys-basename src))
                                    (Id ,id))))))
                    (*text* . ,(^(n c) c))
                    (*default* . ,list))))

(define (main args)
  (match args
    [(_ outfile) (generate (get-version) (gen-file-tree) outfile)]
    [_ (exit 0 "Usage: ~a <outfile>" (car args))])
  0)
