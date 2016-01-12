(use srfi-13)
(use sxml.serializer)
(use sxml.sxpath)
(use sxml.ssax)
(use sxml.tools)
(use sxml.tree-trans)
(use file.util)
(use gauche.process)
(use util.match)
(use gauche.parseopt)

(define *wix-ns* "http://schemas.microsoft.com/wix/2006/wi")
(define *program-files-folder* "ProgramFilesFolder")

(define (wix-template version file-tree)
  `(Wix
    (@ (xmlns ,*wix-ns*))
    ;;
    ;; Main product description
    ;;
    (Product
     (@ (Id "*")
        (Name "Gauche")
        (Language "1033")
        (Version ,version)
        (Manufacturer "Shiro Kawai")
        (UpgradeCode "79ee0cec-691c-4945-b47a-10b9f53b5859"))
     (Package
      (@ (Description "Gauche Scheme Script Engine")
         (Comments "Script-friendly R7RS-conformant Scheme Implementation")
         (Manufacturer "Shiro Kawai")
         (InstallerVersion "200")
         (Compressed "yes")))

     (UIRef (@ (Id "WixUI_InstallDirCustom")))
     (Property (@ (Id "WIXUI_INSTALLDIR") (Value "INSTALLDIR")))
     (Property (@ (Id "CUSTOM_DESCTOPSHORTCUT") (Value "1")))
     (Property (@ (Id "CUSTOM_ASSOCIATEEXTENSION") (Value "1")))
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
       (@ (Id ,*program-files-folder*))
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
      ;; Set up registries
      (Component
       (@ (Id "RegistryEntries") (Guid "aad33726-68ed-46c6-91cb-a88d9ad47c01"))
       ,(regkey 'HKLM "Software\\Scheme Arts\\Gauche"
                      '("InstallDir" "[INSTALLDIR]")))
      (Component
       (@ (Id "RegistryEntries_AssociateExtension")
          (Guid "8bc70ca0-e3ff-11e3-b171-d31eca574265"))
       (Condition "CUSTOM_ASSOCIATEEXTENSION")
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
       (Condition "CUSTOM_DESCTOPSHORTCUT")
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
              (ComponentRef (@ (Id "RegistryEntries_AssociateExtension")))
              (ComponentRef (@ (Id "DesktopShortcut")))
              )

     (MajorUpgrade
      (@ (DowngradeErrorMessage "Newer version of Gauche is already installed.")
         (AllowSameVersionUpgrades "yes")))
     )
    ;;
    ;; Custom UI component
    ;;
    (Fragment
     (UI
      (Dialog
       (@ (Id "InstallDirDlg_Custom")
          (Width "370") (Height "270") (Title "!(loc.InstallDirDlg_Title)"))
       (Control (@ (Id "Next") (Type "PushButton")
                   (X "236") (Y "243") (Width "56") (Height "17")
                   (Default "yes") (Text "!(loc.WixUINext)")))
       (Control (@ (Id "Back") (Type "PushButton")
                   (X "180") (Y "243") (Width "56") (Height "17")
                   (Text "!(loc.WixUIBack)")))
       (Control (@ (Id "Cancel") (Type "PushButton")
                   (X "304") (Y "243") (Width "56") (Height "17")
                   (Cancel "yes") (Text "!(loc.WixUICancel)"))
                (Publish (@ (Event "SpawnDialog") (Value "CancelDlg")) 1))
       (Control (@ (Id "Description") (Type "Text")
                   (X "25") (Y "23") (Width "280") (Height "15")
                   (Transparent "yes") (NoPrefix "yes")
                   (Text "!(loc.InstallDirDlgDescription)")))
       (Control (@ (Id "Title") (Type "Text")
                   (X "15") (Y "6") (Width "200") (Height "15")
                   (Transparent "yes") (NoPrefix "yes")
                   (Text "!(loc.InstallDirDlgTitle)")))
       (Control (@ (Id "BannerBitmap") (Type "Bitmap")
                   (X "0") (Y "0") (Width "370") (Height "44") (TabSkip "no")
                   (Text "!(loc.InstallDirDlgBannerBitmap)")))
       (Control (@ (Id "BannerLine") (Type "Line")
                   (X "0") (Y "0") (Width "370") (Height "0")))
       (Control (@ (Id "BottomLine") (Type "Line")
                   (X "0") (Y "234") (Width "370") (Height "0")))
       (Control (@ (Id "FolderLabel") (Type "Text")
                   (X "20") (Y "60") (Width "290") (Height "30")
                   (NoPrefix "yes") (Text "!(loc.InstallDirDlgFolderLabel)")))
       (Control (@ (Id "Folder") (Type "PathEdit")
                   (X "20") (Y "100") (Width "320") (Height "18")
                   (Property "WIXUI_INSTALLDIR") (Indirect "yes")))
       (Control (@ (Id "ChangeFolder") (Type "PushButton")
                   (X "20") (Y "120") (Width "56") (Height "17")
                   (Text "!(loc.InstallDirDlgChange)")))
       (Control (@ (Id "DesktopShortcutCheckBox") (Type "CheckBox")
                   (X "20") (Y "160") (Width "290") (Height "17")
                   (Property "CUSTOM_DESCTOPSHORTCUT")
                   (CheckBoxValue "1")
                   (Text "Create a shortcut on the desktop.")))
       (Control (@ (Id "AssociateExtensionCheckBox") (Type "CheckBox")
                   (X "20") (Y "185") (Width "290") (Height "17")
                   (Property "CUSTOM_ASSOCIATEEXTENSION")
                   (CheckBoxValue "1")
                   (Text "Associate 'scm' extension to Gauche.")))
       )))
    (Fragment
     (UI
      (@ (Id "WixUI_InstallDirCustom"))
      (TextStyle (@ (Id "WixUI_Font_Normal") (FaceName "Tahoma") (Size "8")))
      (TextStyle (@ (Id "WixUI_Font_Bigger") (FaceName "Tahoma") (Size "12")))
      (TextStyle (@ (Id "WixUI_Font_Title") (FaceName "Tahoma") (Size "9") (Bold "yes")))
      (Property (@ (Id "DefaultUIFont") (Value "WixUI_Font_Normal")))
      (Property (@ (Id "WixUI_Mode") (Value "InstallDir")))
      (Property (@ (Id "ARPNOMODIFY") (Value "1")))
      ,@($ append-map gen-dialog
           '((BrowseDlg
              (OK DoAction WixUIValidatePath 3 1)
              (OK SpawnDialog InvalidDirDlg 4 "WIXUI_INSTALLDIR_VALID<>\"1\""))
             (DiskCostDlg)
             (.ExitDialog
              (Finish EndDialog Return 999 1))
             (.WelcomeDlg
              (Next NewDialog LicenseAgreementDlg #f "NOT Installed")
              (Next NewDialog VerifyReadyDlg #f "Installed AND PATCH"))
             (.LicenseAgreementDlg
              (Back NewDialog WelcomeDlg #f 1)
              (Next NewDialog InstallDirDlg_Custom #f "LicenseAccepted = \"1\""))
             (.InstallDirDlg_Custom
              (Back NewDialog LicenseAgreementDlg #f 1)
              (Next SetTargetPath "[WIXUI_INSTALLDIR]" 1 1)
              (Next DoAction "WixUIValidatePath" 2 1)
              (Next SpawnDialog "InvalidDirDlg" 3 "WIXUI_INSTALLDIR_VALID <> \"1\"")
              (Next NewDialog VerifyReadyDlg 4 "WIXUI_INSTALLDIR_VALID = \"1\"")
              (ChangeFolder _BrowseProperty "[WIXUI_INSTALLDIR]" 1 1)
              (ChangeFolder SpawnDialog BrowseDlg 2 1))
             (.VerifyReadyDlg
              (Back NewDialog InstallDirDlg_Custom 1 "NOT Installed")
              (Back NewDialog MaintenanceTypeDlg 2 "Installed AND NOT PATCH")
              (Back NewDialog WelcomeDlg 2 "Installed AND NOT PATCH"))
             (.MaintenanceWelcomeDlg
              (Next NewDialog MaintenanceTypeDlg #f 1))
             (.MaintenanceTypeDlg
              (RepairButton NewDialog VerifyReadyDlg #f 1)
              (RemoveButton NewDialog VerifyReadyDlg #f 1)
              (Back NewDialog MaintenanceWelcomeDlg #f 1))
             (ErrorDlg)
             (FatalError)
             (FilesInUse)
             (MsiRMFilesInUse)
             (PrepareDlg)
             (ProgressDlg)
             (ResumeDlg)
             (UserExit))))
     (UIRef (@ (Id "WixUI_Common"))))
    ))

;; generate DialogRef and/or Publish
;; from (dialog-name (ctrl event value order content) ...)
;; if dialog-name begins with '.', we don't emit DialogRef
(define (gen-dialog spec)
  (match-let1 (dlg . params) spec
      (let* ([s (x->string dlg)]
             [need-ref? (not (string-prefix? "." s))]
             [dlgname (if need-ref? s (string-drop s 1))])
        `(,@(cond-list [need-ref? `(DialogRef (@ (Id ,dlgname)))])
          ,@(map (^[param]
                   (match-let1 (ctrl event value order content) param
                     `(Publish (@ (Dialog ,dlgname)
                                  (Control ,(x->string ctrl))
                                  (,(if (eq? event '_BrowseProperty) 'Property 'Event) ,(x->string event))
                                  (Value ,(x->string value))
                                  ,@(cond-list
                                     [order `(Order ,(x->string order))]))
                               ,content)))
                 params)))))

;; vals :: Val | (name Val)
(define (regkey root key vals)
  `(RegistryKey
    (@ (Root ,(x->string root)) (Key ,key)
       (ForceDeleteOnUninstall "yes"))
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
  (regexp-replace
   #/^(\d+\.\d+(?:\.\d+)).*$/
   (process-output->string '("Gauche/bin/gauche-config" "-V"))
   "\\1"))
  

;; Generate an XML Directory node of the installation tree.
;; Currently we rely on the 'heat.exe' that comes with Wix SDK.
(define (gen-file-tree)
  (let1 tmpfile "file-tree.xml"
    (run-process `("heat" "dir" "Gauche" "-nologo" "-sfrag" "-gg" "-sreg" "-o" ,tmpfile)
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
  (define (usage) (exit 1 #"Usage: ~*program-name* [--arch=x64] <outfile>"))
  (let-args (cdr args)
      ([arch "a|arch=s"]
       [else (opt . _) (display #"Unknown option: ~opt\n" (current-error-port)) (usage)]
       . args)
    (match args
      [(outfile)
       (if (equal? arch "x64")
         (set! *program-files-folder* "ProgramFiles64Folder"))
       (generate (get-version) (gen-file-tree) outfile)]
      [_ (usage)])
    0))
