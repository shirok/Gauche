;;;
;;; file.elf - ELF file format
;;;
;;;   Copyright (c) 2026  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(define-module file.elf
  (use gauche.native-type)
  (use gauche.uvector)
  (use gauche.sequence)
  (use file.util)
  (export read-elf
          <elf-header> <elf-program-header> <elf-section-header>
          ;; File class
          ELFCLASS_NONE ELFCLASS_32 ELFCLASS_64
          ;; Data encoding
          ELFDATA_NONE ELFDATA_LSB ELFDATA_MSB
          ;; File version
          EV_NONE EV_CURRENT
          ;; OS ABI
          ABI_SystemV ABI_HPUX ABI_NetBSD ABI_Linux ABI_GNUHurd
          ABI_Solaris ABI_AIX ABI_IRIX ABI_FreeBSD ABI_Tru64
          ABI_NovellModesto ABI_OpenBSD ABI_OpenVMS ABI_NonStopKernel
          ABI_AROS ABI_FenixOS ABI_CloudABI ABI_OpenVOS
          ;; Object file type
          ET_NONE ET_REL ET_EXEC ET_DYN ET_CORE
          ET_LOOS ET_HIOS ET_LOPROC ET_HIPROC
          ;; Machine / ISA
          ISA_NONE ISA_M32 ISA_SPARC ISA_386 ISA_68K ISA_88K ISA_IAMCU
          ISA_860 ISA_MIPS ISA_S370 ISA_MIPS_RS3_LE ISA_PARISC
          ISA_VPP500 ISA_SPARC32PLUS ISA_960 ISA_PPC ISA_PPC64 ISA_S390
          ISA_ARM ISA_ALPHA ISA_SH ISA_SPARCV9 ISA_IA_64 ISA_X86_64
          ISA_VAX ISA_AVR ISA_M32R ISA_MN10300 ISA_OPENRISC ISA_XTENSA
          ISA_AARCH64 ISA_MICROBLAZE ISA_CUDA ISA_AMDGPU ISA_RISCV
          ISA_BPF ISA_LOONGARCH
          ;; Program header type / flags
          PT_NULL PT_LOAD PT_DYNAMIC PT_INTERP PT_NOTE PT_SHLIB
          PT_PHDR PT_TLS PT_LOOS PT_HIOS PT_LOPROC PT_HIPROC
          PT_GNU_EH_FRAME PT_GNU_STACK PT_GNU_RELRO PT_GNU_PROPERTY
          PF_X PF_W PF_R PF_MASKOS PF_MASKPROC
          ;; Section header type / flags
          SHT_NULL SHT_PROGBITS SHT_SYMTAB SHT_STRTAB SHT_RELA SHT_HASH
          SHT_DYNAMIC SHT_NOTE SHT_NOBITS SHT_REL SHT_SHLIB SHT_DYNSYM
          SHT_INIT_ARRAY SHT_FINI_ARRAY SHT_PREINIT_ARRAY SHT_GROUP
          SHT_SYMTAB_SHNDX SHT_LOOS SHT_HIOS SHT_LOPROC SHT_HIPROC
          SHT_LOUSER SHT_HIUSER
          SHF_WRITE SHF_ALLOC SHF_EXECINSTR SHF_MERGE SHF_STRINGS
          SHF_INFO_LINK SHF_LINK_ORDER SHF_OS_NONCONFORMING SHF_GROUP
          SHF_TLS SHF_COMPRESSED SHF_MASKOS SHF_MASKPROC)
  )
(select-module file.elf)

(define elf-ident-struct
  (native-type '(.struct elf-ident
                         (magic::(.array uint8_t (4))
                          class::uint8_t
                          endianness::uint8_t
                          version::uint8_t
                          osabi::uint8_t
                          abiversion::uint8_t
                          pad::(.array uint8_t (7))))))

(define (%types endian)
  (case endian
    [(big)    (values <uint16-be> <uint32-be> <uint64-be>)]
    [(little) (values <uint16-le> <uint32-le> <uint64-le>)]))


(define (make-elf32-header-struct endian)
  (define-values (u16 u32 u64) (%types endian))
  (native-type `(.struct elf32-header
                         (ident::,elf-ident-struct
                          type::,u16
                          machine::,u16
                          version::,u32
                          entry::,u32
                          phoff::,u32
                          shoff::,u32
                          flags::,u32
                          ehsize::,u16
                          phentsize::,u16
                          phnum::,u16
                          shentsize::,u16
                          shnum::,u16
                          shstrndx::,u16))))

(define (make-elf64-header-struct endian)
  (define-values (u16 u32 u64) (%types endian))
  (native-type `(.struct elf64-header
                         (ident::,elf-ident-struct
                          type::,u16
                          machine::,u16
                          version::,u32
                          entry::,u64
                          phoff::,u64
                          shoff::,u64
                          flags::,u32
                          ehsize::,u16
                          phentsize::,u16
                          phnum::,u16
                          shentsize::,u16
                          shnum::,u16
                          shstrndx::,u16))))

(define (make-elf32-program-header-struct endian)
  (define-values (u16 u32 u64) (%types endian))
  (native-type `(.struct elf32-program-header
                         (type::,u32
                          offset::,u32
                          vaddr::,u32
                          paddr::,u32
                          filesz::,u32
                          memsz::,u32
                          flags::,u32
                          align::,u32))))

;; Note: in ELF64 the program header reorders flags to follow type, so
;; that the 64-bit fields that follow are naturally aligned.
(define (make-elf64-program-header-struct endian)
  (define-values (u16 u32 u64) (%types endian))
  (native-type `(.struct elf64-program-header
                         (type::,u32
                          flags::,u32
                          offset::,u64
                          vaddr::,u64
                          paddr::,u64
                          filesz::,u64
                          memsz::,u64
                          align::,u64))))

(define (make-elf32-section-header-struct endian)
  (define-values (u16 u32 u64) (%types endian))
  (native-type `(.struct elf32-section-header
                         (name::,u32
                          type::,u32
                          flags::,u32
                          addr::,u32
                          offset::,u32
                          size::,u32
                          link::,u32
                          info::,u32
                          addralign::,u32
                          entsize::,u32))))

(define (make-elf64-section-header-struct endian)
  (define-values (u16 u32 u64) (%types endian))
  (native-type `(.struct elf64-section-header
                         (name::,u32
                          type::,u32
                          flags::,u64
                          addr::,u64
                          offset::,u64
                          size::,u64
                          link::,u32
                          info::,u32
                          addralign::,u64
                          entsize::,u64))))

;;
;; Magic bytes (ident.magic
;;
(define-constant ELF_MAGIC '#u8"\x7f;ELF")

;;
;; File class (ident.class)
;;
(define-constant ELFCLASS_NONE  0)
(define-constant ELFCLASS_32    1)
(define-constant ELFCLASS_64    2)

;;
;; Data encoding (ident.endianness)
;;
(define-constant ELFDATA_NONE   0)
(define-constant ELFDATA_LSB    1)      ; little-endian (a.k.a. ELFDATA2LSB)
(define-constant ELFDATA_MSB    2)      ; big-endian    (a.k.a. ELFDATA2MSB)

;;
;; File version (ident.version)
;;
(define-constant EV_NONE        0)
(define-constant EV_CURRENT     1)

;;
;; OS ABI (ident.osabi)
;;
(define-constant ABI_SystemV         0)
(define-constant ABI_HPUX            1)
(define-constant ABI_NetBSD          2)
(define-constant ABI_Linux           3)
(define-constant ABI_GNUHurd         4)
(define-constant ABI_Solaris         6)
(define-constant ABI_AIX             7)
(define-constant ABI_IRIX            8)
(define-constant ABI_FreeBSD         9)
(define-constant ABI_Tru64          10)
(define-constant ABI_NovellModesto  11)
(define-constant ABI_OpenBSD        12)
(define-constant ABI_OpenVMS        13)
(define-constant ABI_NonStopKernel  14)
(define-constant ABI_AROS           15)
(define-constant ABI_FenixOS        16)
(define-constant ABI_CloudABI       17)
(define-constant ABI_OpenVOS        18)

;;
;; Object file type (e_type)
;;
(define-constant ET_NONE        0)      ; No file type
(define-constant ET_REL         1)      ; Relocatable file
(define-constant ET_EXEC        2)      ; Executable file
(define-constant ET_DYN         3)      ; Shared object file
(define-constant ET_CORE        4)      ; Core file
(define-constant ET_LOOS   #xfe00)      ; OS-specific range start
(define-constant ET_HIOS   #xfeff)      ; OS-specific range end
(define-constant ET_LOPROC #xff00)      ; Processor-specific range start
(define-constant ET_HIPROC #xffff)      ; Processor-specific range end

;;
;; ISA / machine (e_machine).  Only a representative selection of
;; values are included; many obscure architectures are omitted.
;;
(define-constant ISA_NONE        0)     ; No machine
(define-constant ISA_M32         1)     ; AT&T WE 32100
(define-constant ISA_SPARC       2)     ; SPARC
(define-constant ISA_386         3)     ; Intel 80386
(define-constant ISA_68K         4)     ; Motorola 68000
(define-constant ISA_88K         5)     ; Motorola 88000
(define-constant ISA_IAMCU       6)     ; Intel MCU
(define-constant ISA_860         7)     ; Intel 80860
(define-constant ISA_MIPS        8)     ; MIPS R3000 big-endian
(define-constant ISA_S370        9)     ; IBM System/370
(define-constant ISA_MIPS_RS3_LE 10)    ; MIPS R3000 little-endian
(define-constant ISA_PARISC      15)    ; HP PA-RISC
(define-constant ISA_VPP500      17)    ; Fujitsu VPP500
(define-constant ISA_SPARC32PLUS 18)    ; SPARC v8+
(define-constant ISA_960         19)    ; Intel 80960
(define-constant ISA_PPC         20)    ; PowerPC
(define-constant ISA_PPC64       21)    ; 64-bit PowerPC
(define-constant ISA_S390        22)    ; IBM System/390
(define-constant ISA_ARM         40)    ; ARM 32-bit
(define-constant ISA_ALPHA       41)    ; DEC Alpha
(define-constant ISA_SH          42)    ; Hitachi SH
(define-constant ISA_SPARCV9     43)    ; SPARC v9 64-bit
(define-constant ISA_IA_64       50)    ; Intel Itanium
(define-constant ISA_X86_64      62)    ; AMD x86-64
(define-constant ISA_VAX         75)    ; DEC VAX
(define-constant ISA_AVR         83)    ; Atmel AVR
(define-constant ISA_M32R        88)    ; Mitsubishi M32R
(define-constant ISA_MN10300     89)    ; Matsushita MN10300
(define-constant ISA_OPENRISC    92)    ; OpenRISC
(define-constant ISA_XTENSA      94)    ; Tensilica Xtensa
(define-constant ISA_AARCH64     183)   ; ARM 64-bit
(define-constant ISA_MICROBLAZE  189)   ; Xilinx MicroBlaze
(define-constant ISA_CUDA        190)   ; NVIDIA CUDA
(define-constant ISA_AMDGPU      224)   ; AMD GPU
(define-constant ISA_RISCV       243)   ; RISC-V
(define-constant ISA_BPF         247)   ; Linux BPF
(define-constant ISA_LOONGARCH   258)   ; LoongArch

;;
;; Program header type (p_type)
;;
(define-constant PT_NULL              0)
(define-constant PT_LOAD              1)
(define-constant PT_DYNAMIC           2)
(define-constant PT_INTERP            3)
(define-constant PT_NOTE              4)
(define-constant PT_SHLIB             5)
(define-constant PT_PHDR              6)
(define-constant PT_TLS               7)
(define-constant PT_LOOS     #x60000000)
(define-constant PT_HIOS     #x6fffffff)
(define-constant PT_LOPROC   #x70000000)
(define-constant PT_HIPROC   #x7fffffff)
;; GNU extensions inside the OS-specific range
(define-constant PT_GNU_EH_FRAME #x6474e550)
(define-constant PT_GNU_STACK    #x6474e551)
(define-constant PT_GNU_RELRO    #x6474e552)
(define-constant PT_GNU_PROPERTY #x6474e553)

;;
;; Program header flags (p_flags) -- bit flags
;;
(define-constant PF_X          #x1)         ; Executable
(define-constant PF_W          #x2)         ; Writable
(define-constant PF_R          #x4)         ; Readable
(define-constant PF_MASKOS     #x0ff00000)  ; OS-specific
(define-constant PF_MASKPROC   #xf0000000)  ; Processor-specific

;;
;; Section header type (sh_type)
;;
(define-constant SHT_NULL              0)
(define-constant SHT_PROGBITS          1)
(define-constant SHT_SYMTAB            2)
(define-constant SHT_STRTAB            3)
(define-constant SHT_RELA              4)
(define-constant SHT_HASH              5)
(define-constant SHT_DYNAMIC           6)
(define-constant SHT_NOTE              7)
(define-constant SHT_NOBITS            8)
(define-constant SHT_REL               9)
(define-constant SHT_SHLIB             10)
(define-constant SHT_DYNSYM            11)
(define-constant SHT_INIT_ARRAY        14)
(define-constant SHT_FINI_ARRAY        15)
(define-constant SHT_PREINIT_ARRAY     16)
(define-constant SHT_GROUP             17)
(define-constant SHT_SYMTAB_SHNDX      18)
(define-constant SHT_LOOS     #x60000000)
(define-constant SHT_HIOS     #x6fffffff)
(define-constant SHT_LOPROC   #x70000000)
(define-constant SHT_HIPROC   #x7fffffff)
(define-constant SHT_LOUSER   #x80000000)
(define-constant SHT_HIUSER   #xffffffff)

;;
;; Section header flags (sh_flags) -- bit flags
;;
(define-constant SHF_WRITE             #x1)
(define-constant SHF_ALLOC             #x2)
(define-constant SHF_EXECINSTR         #x4)
(define-constant SHF_MERGE             #x10)
(define-constant SHF_STRINGS           #x20)
(define-constant SHF_INFO_LINK         #x40)
(define-constant SHF_LINK_ORDER        #x80)
(define-constant SHF_OS_NONCONFORMING  #x100)
(define-constant SHF_GROUP             #x200)
(define-constant SHF_TLS               #x400)
(define-constant SHF_COMPRESSED        #x800)
(define-constant SHF_MASKOS            #x0ff00000)
(define-constant SHF_MASKPROC          #xf0000000)

;;
;; High-level object representation
;;
;; These classes are a structured view of an ELF file.  Where the raw
;; struct uses file offsets and counts (phoff/phnum, shoff/shnum,
;; sh_offset/sh_size, shstrndx, ...) the classes hold direct references
;; to the corresponding instances or content bytevectors instead.
;; Fields whose values are fully determined by the class (ehsize,
;; phentsize, shentsize) or by list length (phnum, shnum) are not
;; stored as slots.
;;

(define-class <elf-header> ()
  (;; From e_ident:
   (class            :init-keyword :class)         ; ELFCLASS_32 / ELFCLASS_64
   (endian           :init-keyword :endian)        ; 'little or 'big
   (osabi            :init-keyword :osabi    :init-value ABI_SystemV)
   (abi-version      :init-keyword :abi-version :init-value 0)
   ;; Rest of the header:
   (type             :init-keyword :type)          ; ET_*
   (machine          :init-keyword :machine)       ; ISA_*
   (version          :init-keyword :version  :init-value EV_CURRENT)
   (entry            :init-keyword :entry    :init-value 0)
   (flags            :init-keyword :flags    :init-value 0)
   ;; Replaces phoff / phnum / phentsize:
   (program-headers  :init-keyword :program-headers :init-value '())
   ;; Replaces shoff / shnum / shentsize:
   (section-headers  :init-keyword :section-headers :init-value '())
   ;; Replaces shstrndx -- a reference to the section that holds the
   ;; section-name string table (#f if there is no section name table)
   (section-name-table :init-keyword :section-name-table :init-value #f)))

(define-class <elf-program-header> ()
  ((type             :init-keyword :type)          ; PT_*
   (flags            :init-keyword :flags    :init-value 0)  ; PF_* bits
   (vaddr            :init-keyword :vaddr    :init-value 0)
   (paddr            :init-keyword :paddr    :init-value 0)
   (align            :init-keyword :align    :init-value 0)
   ;; Replaces p_offset / p_filesz -- segment bytes as they appear in
   ;; the file.  #f means the segment has no file content (e.g. a pure
   ;; PT_GNU_STACK marker)
   (content          :init-keyword :content  :init-value #f)
   ;; Replaces p_memsz -- the in-memory size of the segment.  When this
   ;; exceeds (u8vector-length content) the remainder is zero-filled
   ;; (BSS-style extension).
   (mem-size         :init-keyword :mem-size :init-value 0)))

(define-class <elf-section-header> ()
  (;; Replaces sh_name -- the string itself, not an offset into the
   ;; section-name string table
   (name             :init-keyword :name     :init-value "")
   (type             :init-keyword :type)          ; SHT_*
   (flags            :init-keyword :flags    :init-value 0)  ; SHF_* bits
   (addr             :init-keyword :addr     :init-value 0)
   ;; Replaces sh_offset / sh_size -- the section's bytes.  #f for
   ;; SHT_NOBITS sections (which have a size but no file content); in
   ;; that case the `size' slot below carries the in-memory size.
   (content          :init-keyword :content  :init-value #f)
   (size             :init-keyword :size     :init-value 0)
   ;; Replaces sh_link -- a direct reference to the linked section, or
   ;; #f for SHN_UNDEF
   (link             :init-keyword :link     :init-value #f)
   ;; sh_info is left as a raw integer because its meaning depends on
   ;; sh_type (sometimes a section index, sometimes a symbol index,
   ;; sometimes a count)
   (info             :init-keyword :info     :init-value 0)
   (addr-align       :init-keyword :addr-align :init-value 0)
   (ent-size         :init-keyword :ent-size :init-value 0)))

;;
;; Reader
;;
;; read-elf maps the given file with sys-mmap, parses the raw structs
;; through gauche.native-type, and returns a populated <elf-header>.
;; Section/segment content slots are u8vectors that alias the mapped
;; memory (no extra copying); they remain valid as long as the
;; <memory-region> isn't reclaimed -- which is held alive transitively
;; through the u8vector views.
;;

(define (read-elf path)
  (let* ([size (file-size path)]
         [mem (call-with-input-file path
                (^p (sys-mmap p PROT_READ MAP_PRIVATE size)))]
         [u8  (make-view-uvector mem <u8vector> size 0 #t)])
    (let* ([ih          (%read-elf-header u8 path)]
           [class-val   (native. ih 'class)]
           [endian      (%elf-endian (native. ih 'endianness))]
           [hdr-type    (%elf-header-type class-val endian)]
           [ph-type     (%elf-phdr-type   class-val endian)]
           [sh-type     (%elf-shdr-type   class-val endian)]
           [hh          (cast-handle hdr-type ih)]
           [phoff       (native. hh 'phoff)]
           [phnum       (native. hh 'phnum)]
           [phentsize   (native. hh 'phentsize)]
           [shoff       (native. hh 'shoff)]
           [shnum       (native. hh 'shnum)]
           [shentsize   (native. hh 'shentsize)]
           [shstrndx    (native. hh 'shstrndx)]
           [pheaders    (%read-program-headers u8 ph-type phoff phnum phentsize)])
      (receive (sheaders name-offsets link-indices)
          (%read-section-headers u8 sh-type shoff shnum shentsize)
        (let* ([svec      (list->vector sheaders)]
               [name-tab  (and (> shnum 0)
                               (< 0 shstrndx shnum)
                               (vector-ref svec shstrndx))])
          (%resolve-section-names sheaders name-offsets name-tab)
          (%resolve-section-links sheaders link-indices svec shnum)
          (make <elf-header>
            :class       class-val
            :endian      endian
            :osabi       (native. ih 'osabi)
            :abi-version (native. ih 'abiversion)
            :type        (native. hh 'type)
            :machine     (native. hh 'machine)
            :version     (native. hh 'version)
            :entry       (native. hh 'entry)
            :flags       (native. hh 'flags)
            :program-headers    pheaders
            :section-headers    sheaders
            :section-name-table name-tab))))))

(define (%read-elf-header u8 path)
  (when (< (u8vector-length u8) (~ elf-ident-struct'size))
    (error "File is not ELF:" path))
  (rlet1 ih (uvector->native-handle u8 elf-ident-struct 0)
    (unless (equal? (coerce-to <u8vector> (wrap-native-handle (native. ih 'magic)))
                    ELF_MAGIC)
      (error "Not an ELF file:" path))))

(define (%elf-endian val)
  (case val
    [(1) 'little]
    [(2) 'big]
    [else (error "Unrecognized ELF data encoding:" val)]))

(define (%elf-header-type class-val endian)
  (case class-val
    [(1) (make-elf32-header-struct endian)]
    [(2) (make-elf64-header-struct endian)]
    [else (error "Unrecognized ELF class:" class-val)]))

(define (%elf-phdr-type class-val endian)
  (case class-val
    [(1) (make-elf32-program-header-struct endian)]
    [(2) (make-elf64-program-header-struct endian)]
    [else (error "Unrecognized ELF phdr type:" class-val)]))

(define (%elf-shdr-type class-val endian)
  (case class-val
    [(1) (make-elf32-section-header-struct endian)]
    [(2) (make-elf64-section-header-struct endian)]
    [else (error "Unrecognized ELF shdr type:" class-val)]))

(define (%read-program-headers u8 ph-type phoff phnum phentsize)
  (let loop ([i 0] [acc '()])
    (if (>= i phnum)
      (reverse acc)
      (let* ([off (+ phoff (* i phentsize))]
             [h   (uvector->native-handle u8 ph-type off)]
             [co  (native. h 'offset)]
             [fz  (native. h 'filesz)])
        (loop (+ i 1)
              (cons (make <elf-program-header>
                      :type     (native. h 'type)
                      :flags    (native. h 'flags)
                      :vaddr    (native. h 'vaddr)
                      :paddr    (native. h 'paddr)
                      :align    (native. h 'align)
                      :content  (and (positive? fz)
                                     (uvector-alias <u8vector> u8 co (+ co fz)))
                      :mem-size (native. h 'memsz))
                    acc))))))

;; Returns three parallel values: the section list, a list of name-offsets
;; (sh_name -- index into the section name string table), and a list of
;; sh_link integer values.  Name resolution and link resolution happen in
;; subsequent passes so that cross-references can point at instances.
(define (%read-section-headers u8 sh-type shoff shnum shentsize)
  (let loop ([i 0] [secs '()] [names '()] [links '()])
    (if (>= i shnum)
      (values (reverse secs) (reverse names) (reverse links))
      (let* ([off  (+ shoff (* i shentsize))]
             [h    (uvector->native-handle u8 sh-type off)]
             [t    (native. h 'type)]
             [co   (native. h 'offset)]
             [sz   (native. h 'size)]
             [sec  (make <elf-section-header>
                     :type       t
                     :flags      (native. h 'flags)
                     :addr       (native. h 'addr)
                     :content    (and (positive? sz)
                                      (not (= t SHT_NOBITS))
                                      (uvector-alias <u8vector> u8 co (+ co sz)))
                     :size       sz
                     :info       (native. h 'info)
                     :addr-align (native. h 'addralign)
                     :ent-size   (native. h 'entsize))])
        (loop (+ i 1)
              (cons sec secs)
              (cons (native. h 'name) names)
              (cons (native. h 'link) links))))))

(define (%resolve-section-names sheaders name-offsets name-tab)
  (let1 strtab (and name-tab (~ name-tab'content))
    (when strtab
      (for-each (^[sec off]
                  (set! (~ sec'name) (%read-c-string strtab off)))
                sheaders name-offsets))))

(define (%resolve-section-links sheaders link-indices svec shnum)
  (for-each (^[sec idx]
              (when (< 0 idx shnum)
                (set! (~ sec'link) (vector-ref svec idx))))
            sheaders link-indices))

;; Read a NUL-terminated byte string from U8 starting at OFF.
(define (%read-c-string u8 off)
  (u8vector->string u8 off -1 0))
