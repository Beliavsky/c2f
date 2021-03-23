! --------------------------------------------------------
PROGRAM C2F        ! attempts translation  C to FORTRAN 90
! --------------------------------------------------------
USE DFLIB, ONLY: RUNQQ
IMPLICIT  NONE     ! Programmer: David Frank, Cocoa Beach, FL.
                   !      Email: DaveGemini@aol.com

INTEGER,PARAMETER :: NUM_INTRINSICS = 5 ! intrinsic name translations
CHARACTER*10 search, intrinsic_names(2,NUM_INTRINSICS) /  &
'FABS      ', 'ABS       ', 'FMOD      ', 'MOD       ',   &
'GETCH     ', 'GETCHARQQ ', 'LABS      ', 'ABS       ',   &
'STRLEN    ', 'LEN_TRIM  '/

! - - - declare parameters - - -
INTEGER, PARAMETER :: &
   MAXREC=10000, RECSIZ=1000, MAXKEYLEN=10, MAXKEY=73, &
   MAXKEYSAVE=30, MAXARG=30, MAXFORM=99, MAXLAB=99, MAXMOD=199, MAXSTR=60, &
   MAXTYPEDEFS=199, MAXMEMBERS=199, MAXCOMMENTS=199, MID=500, INDENT=2

! ------------------
TYPE IDENTIFIER_INFO
! ------------------
   CHARACTER (LEN=MAXSTR) :: name(MID) ! identifier name
   CHARACTER (LEN=20)  :: name2(MID) ! structname, alias, etc.
   INTEGER(1) :: nc(MID)          ! #chars in identifier, 0=entry avail.
   INTEGER(1) :: type(MID)
   INTEGER(1) :: declare(MID)
   INTEGER(1) :: locate(MID)
   LOGICAL(1) :: ptr_void(MID)    ! void *arg
   LOGICAL(1) :: ptr_deref(MID)   ! confirmed by use
   LOGICAL(1) :: ptr_array(MID)   !   "
   LOGICAL(1) :: target(MID)      !   "     variable was a target
   LOGICAL(1) :: stored(MID)      !   "     variable was stored into
   LOGICAL(1) :: member(MID)      ! member of type declaration
   CHARACTER (LEN=30) :: dim(MID)  ! array dimension string (:)  (5,5)
   CHARACTER (LEN=80) :: init(MID) ! initialization string from declare
END TYPE
TYPE (IDENTIFIER_INFO) :: g, v     ! global, internal module identifiers

INTEGER,PARAMETER :: &
  TSHORT=1,TLONG=2,TINT=3,TFLOAT=4,TDOUBLE=5,TCHAR=6,TUNION=7,TSTRUCT=8,  &
  CONST=1, SCALAR=2, ARRAY1=3, ARRAY2=4, ARRAY3=5, PTR1=6, PTR2=7, FUN=8, SUB=9, &
  NONE=0, ARG=1, EXTERN=2, GLOBAL=3, LOCAL=4, STATIC=5

CHARACTER (LEN=11) :: Types(0:8) = &
   (/'NONE      ','INTEGER(2)','INTEGER   ','INTEGER   ', &
     'REAL(4)   ','REAL(8)   ','CHARACTER ','TYPE      ','TYPE      '/)
CHARACTER (LEN=2) :: Kinds(0:8) = (/'  ', '_2', '_4', '_4', '_4', '_8', &
                                          '_s', '  ', '_T'/)
CHARACTER (LEN=6) :: Declare_s(0:9)=(/'none  ','const ','scalar','array1','array2','array3', &
                                      'ptr1  ','ptr2  ','fun   ','sub   '/)
CHARACTER (LEN=6) :: Locate_s(0:5)= (/'none  ','arg   ','extern','global','local ','save  '/)
CHARACTER (LEN=18) :: TYPE_PTR_1D(0:6) = &
   (/'TYPE (???_PTR_1D)','TYPE (INT2_PTR_1D)','TYPE (INT_PTR_1D)', &
     'TYPE (INT_PTR_1D)','TYPE (FLT_PTR_1D)', 'TYPE (DBL_PTR_1D)', &
                                              'TYPE (CHR_PTR_1D)'/)
CHARACTER, PARAMETER :: HT=CHAR(9), SP=' ', CR=CHAR(13), LF=CHAR(10), &
                        QUOTE=CHAR(39), DQUOTE=CHAR(34), ENQ=CHAR(05), &
                        DOWN=CHAR(16), ZERO=CHAR(0)

INTEGER, PARAMETER ::      &
         C_leftbrace = 1,  &
         C_rightbrace= 2,  &
         C_auto      = 3,  &
         C_register  = 4,  &
         C_static    = 5,  &
         C_unsigned  = 6,  &
         C_void      = 7,  &
         C_short     = 8,  &
         C_long      = 9,  &
         C_int       = 10, &
         C_float     = 11, &
         C_double    = 12, &
         C_char      = 13, &
         C_FILE      = 14, &
         C_struct    = 15, &
         C_spare15   = 16, &
         C_cdecl     = 17, &
         C_const     = 18, &
         C_define    = 19, &
         C_enum      = 20, &
         C_extern    = 21, &
         C_include   = 22, &
         C_union     = 23, &
         C_typedef   = 24, &
         C_typedef_en= 25, &    ! set by pass1 for typedef enum
         C_spare26   = 26, &
         C_signed    = 27, &    ! end declaration keys
         C_case      = 28, &
         C_default   = 29, &
         C_do        = 30, &
         C_else      = 31, &
         C_for       = 32, &
         C_if        = 33, &
         C_spare34   = 34, &
         C_main      = 35, &
         C_switch    = 36, &
         C_spare     = 37, &
         C_while     = 38, &
         C_spare39   = 39, &
         C_spare40   = 40, &
         C_spare41   = 41, &
         C_spare43   = 42, &
         C_return    = 43, &      ! return is NOT appendable to if(...)
         C_break     = 44, &      ! begin section appendable to if(...)
         C_continue  = 45, &
         C_exit      = 46, &      ! STOP
         C_goto      = 47, &
         C_strcpy    = 48, &      ! x = y
         C_strncpy   = 49, &      ! x = y(n)
         C_strcat    = 50, &      ! x = x(1:n) // y
         C_spare51   = 51, &
         C_fclose    = 52, &      ! CLOSE
         C_spare53   = 53, &
         C_getchar   = 54, &      ! READ (*,*)   when no char returned
         C_fgets     = 55, &      ! READ (unit) string
         C_fprintf   = 56, &      ! WRITE
         C_fputs     = 57, &      ! WRITE (unit,'(A)') "  "
         C_printf    = 58, &      ! WRITE
         C_putc      = 59, &      ! WRITE (unit) ch
         C_putch     = 60, &      ! WRITE (*) ch
         C_puts      = 61, &      ! WRITE (*,'(A)') "   "
         C_fscanf    = 62, &      ! READ (unit,*)
         C_scanf     = 63, &      ! READ (*,*) x
         C_sscanf    = 64, &      ! READ (string,*) x
         C_malloc    = 65, &
         C_memcpy    = 66, &
         C_memmove   = 67, &
         C_calloc    = 68, &      ! currently same as malloc
         C_spare69   = 69, &
         C_$if       = 70, &      ! #if....
         C2F_insert  = 71, &      ! c2f prevkey signal to pass type
         C2F_attach  = 72, &      ! c2f generated, not in original source
         C2Fpointer  = 73         ! c2f generated, not in original source

CHARACTER (LEN=MAXKEYLEN) :: Keywords(MAXKEY) = (/ &  ! translation status
              '{         ', &   !   1
              '}         ', &   !   2
              'auto      ', &   !   3
              'register  ', &   !   4
              'static    ', &   !   5
              'unsigned  ', &   !   6
              'void      ', &   !   7
              'short     ', &   !   8
              'long      ', &   !   9
              'int       ', &   !   10
              'float     ', &   !   11
              'double    ', &   !   12
              'char      ', &   !   13
              'FILE      ', &   !   14
              'struct    ', &   !   15
              'spare15   ', &   !   16
              'cdecl     ', &   !   17
              'const     ', &   !   18
              '#define   ', &   !   19
              'enum      ', &   !   20
              'extern    ', &   !   21
              '#include  ', &   !   22
              'union     ', &   !   23
              'typedef   ', &   !   24
              'typedef_en', &   !   25   set by pass one for syn: typedef enum
              'spare26   ', &   !   26
              'signed    ', &   !   27   end declaration keys
              'case      ', &   !   28   begin execution keys
              'default   ', &   !   29
              'do        ', &   !   30
              'else      ', &   !   31
              'for       ', &   !   32
              'if        ', &   !   33
              'spare34   ', &   !   34
              'main      ', &   !   35
              'switch    ', &   !   36
              'spare37   ', &   !   37
              'while     ', &   !   38
              'spare39   ', &   !   39
              'spare40   ', &   !   40
              'spare41   ', &   !   41
              'spare42   ', &   !   42
              'return    ', &   !   43
              'break     ', &   !   44  begin if(...) append allowed section
              'continue  ', &   !   45
              'exit      ', &   !   46
              'goto      ', &   !   47
              'strcpy    ', &   !   48
              'strncpy   ', &   !   49
              'strcat    ', &   !   50
              'spare51   ', &   !   51
              'fclose    ', &   !   52
              'spare53   ', &   !   53
              'getchar   ', &   !   54
              'fgets     ', &   !   55
              'fprintf   ', &   !   56
              'fputs     ', &   !   57
              'printf    ', &   !   58
              'putc      ', &   !   59
              'putch     ', &   !   60
              'puts      ', &   !   61
              'fscanf    ', &   !   62
              'scanf     ', &   !   63
              'sscanf    ', &   !   64
              'malloc    ', &   !   65
              'memcpy    ', &   !   66
              'memmove   ', &   !   67
              'calloc    ', &   !   68
              'spare69   ', &   !   69
              'spare70   ', &   !   70
              'C2F_insert', &   !   71   ! text not actually generated
              'C2F_attach', &   !   72   ! c2f generated for special actions
              'C2Fpointer'/)    !   73   ! c2f generated for special actions

INTEGER, PARAMETER :: &
         C_program    = 201, &      ! set primarily by GetKey
         C_function   = 202, &      !  "
         C_subroutine = 203, &      !  "
         C_label      = 204, &
         C_data       = 205        ! n={..} or { or " or 0-9  set by GetKey

INTEGER :: Nsize,               &  ! #chars in source file name
           Nrec,                &  ! input file record index pass1,2
           Nmod,                &  ! #module being processed
           Nmod1,               &  ! #modules processed in pass1
           Keysize(MAXKEY),     &  ! #chars each key
           Key,                 &  ! c keyword in control of decode
           FunKey,              &  ! function type key  int fun1()
           KeyPrev,PrevKey,     &  !
           Nextkey,             &  ! additional keyword(s) found in record
           Key2, Key3,          &  ! next 2 record's 1st Keywords
           Keys(MAXREC),        &  ! contains above Keywords, gen'ed in pass1
           c, f,                &  ! current indices   to Cline & Fline
           Cends(MAXREC),       &  ! end   Cline op field each rec
           Cend,                &  ! end   Cline op field this rec
           Ceod,                &  ! end-of-data this rec
           Indentcol,           &  !
           Key_Save(MAXKEYSAVE),&  ! Push/Pull block controls
           Col_Save(MAXKEYSAVE),&  ! Push/Pull indentcol assoc with keysave
           Brk_Save(MAXKEYSAVE),&  ! Push/Pull Brk_expected assoc with keysave
           Nkeysave,            &  ! index to Push/Pull controls
           ErrCount,            &  ! total errors detected
           Nfs,                 &  ! # function/subroutine names from C2F.FI
           Nfunc,               &  ! # functions to generate in CONTAINS
           Npass,               &  ! identify pass # for error reporting
           Moduletypes(MAXMOD)     ! C_program,C_subroutine,C_function

LOGICAL :: Comment_cont,        &  ! comments continue flag
           Program_Sub_Fun,     &  ! key = C_program,C_subroutine,C_function
           Use_pointer2,        &  ! **x -> x%p(i)
           Lconst,              &  !
           Lstatic,             &  ! set by declare proc, static struct
           Lvoid,               &  ! set by syn:  void *x =
           Adjusted_Dimension,  &  ! by Dimension_Entry()
           Debug,               &  ! command line request for 1 debug rec.
           Brk_expected,        &  ! case is expecting a break
           Global_Target,       &  ! set if global var. is a target
           CommandLine,         &  ! gen command line code flag
           Insert(MAXREC),      &  !
           Recurse(MAXMOD)         ! module used recursively

CHARACTER (LEN=MAXSTR) ::       &  ! variable name string decoding
          ProgramName,          &  ! c source name (without .c)
          FunName,              &  ! set by GetKey
          SubName,              &  ! set by GetKey
          AssName,              &  ! name =
          Label,                &  ! statement label being processed
          Labels(MAXLAB),       &  ! statement char labels
          String, Structname,   &  ! last decoded struct name
          arg1, arg2, arg3,     &  ! used to separate  x=y=z=nnn
          Typedefs(2,MAXTYPEDEFS), & ! oldname=1, newname=2
          C2F_FI(MID),             & ! known sub/funnames  from C2F.FI
          FI_names(10),            & ! max 10 names/record from C2F.FI
          List_Types(20),          & ! types of functions to gen. in CONTAINS
          List_Suffixes(20),       & ! suffixes   "                   "
          Suffix,                  & ! specific 1 of List_Suffixes
          Modulenames(MAXMOD)        ! from pass 1 detect

CHARACTER (LEN=80) :: End_Do_Inserts(11)
CHARACTER (LEN=RECSIZ) :: Cline, Fline, Cline2, Fline2, &
                          Blank, Formats(MAXFORM) ! read/write format statements created
CHARACTER (LEN=2000) :: Bigline
CHARACTER :: BB(2000), CC(2000), FF(RECSIZ)
EQUIVALENCE (Bigline,BB), (Cline,CC), (Fline,FF)

CHARACTER :: Chends(MAXREC)        ! last op-field char

CHARACTER (LEN=2) :: Kind(5) = (/'CC','I2','I4','R4','R8'/)
CHARACTER (LEN=6) :: fcasts(6) = (/ 'INT2( ', 'INT(  ','INT(  ', &
                                    'FLOAT(', 'DBLE( ','CHAR( ' /)
INTEGER :: nccasts(6) = (/7,6,5,7,8,6/) ! len (short) (long) (int) (float) (double) (char)
INTEGER :: nfcasts(6) = (/5,4,4,6,5,5/) ! len fcasts

INTEGER :: i, j, k, n, m, proto, nid, nc, nid2, ii(5),jj(5),kk(5), input_unit
INTEGER :: c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, cast, cass(0:20), do_col
INTEGER :: f1, f2, f3, f4, f5, n1, n2, z, nass, narg1, narg2, narg3
INTEGER :: control, nstruc, indentsave, funtyp, ntype, ndeclare, nlocate
INTEGER :: nlocate1, nlocate2, openp, openbrace, openbracket, num_inserts
LOGICAL :: exists, casecase, equal, changed, use_C2F_status, output_record
LOGICAL :: args, ass, associated, quote_field, stringc, upchuck, brk_expectedsave
LOGICAL :: localsection, point1, point2, point3
LOGICAL :: delete_leftbrace, add_rightbrace(2)
LOGICAL :: comma, array_detect, target_detect, enclosed, pcheck, check
LOGICAL :: multi_dimension, pointer_arg, structf, run_pp, malloc
CHARACTER :: pch, ch, ch1, chend, chend2, intext*4, flag_s*4, trail*7
CHARACTER (LEN=MAXSTR) :: array_name, caselabel, argtype, argname, len_eqv, &
                          filename, dim_s, name, output_s, lastnidname
CHARACTER (LEN=80) :: init_s

INTEGER*1 :: a_z(0:127) = (/ 0,0,0,0,0,0,0,0, &
             0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0, &
             0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,1,1,1, &
             1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1, 1,1,1,0,0,0,0,0,0,1, &
             1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,0,0,0,0,0 /)

INTEGER*1 :: identifier(0:127) = (/ 0,0,0,0,0,0,0,0, &              !  $ %
             0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,1,1, &
             0,0,0,0,0,0,0,0,1,0, 1,1,1,1,1,1,1,1,1,1, 0,0,0,0,0,0,0,1,1,1, &
             1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1, 1,1,1,0,0,0,0,1,0,1, &
             1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,0,0,0,0,0 /)

INTEGER*1 :: multiply_allowed(0:127) = (/ 0,0,0,0,0,0,0,0, &
             0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0, &
             0,0,0,1,0,0,0,0,1,0, 1,1,1,1,1,1,1,1,1,1, 0,0,0,0,0,0,0,1,1,1, &
             1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1, 1,1,1,0,0,1,0,1,0,1, &
             1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,0,0,0,0,0 /)

INTEGER*1 :: assign_allowed(0:127) = (/ 0,0,0,0,0,0,0,0, &
             0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0, 0,0,0,0,1,0,0,0,0,0, &
             0,0,0,1,0,0,0,0,0,0, 1,1,1,1,1,1,1,1,1,1, 0,0,0,0,0,0,0,1,1,1, &
             1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1, 1,1,1,0,0,1,0,1,0,1, &
             1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,0,0,0,0,0 /)

! - - - Initialize - - -
Npass       = 1
Blank       = SP
Typedefs    = SP
Modulenames = SP
Moduletypes = 0
Recurse     = .FALSE.
v.name       = SP
v.name2      = SP
v.nc         = 0
v.type       = 0
v.declare    = 0
v.locate     = 0
v.ptr_void   = .FALSE.
v.ptr_deref  = .FALSE.
v.ptr_array  = .FALSE.
v.target     = .FALSE.
v.stored     = .FALSE.
v.member     = .FALSE.
v.dim        = SP
v.init       = SP

! - - - Get names of functions/subroutines in C2F.FI - - -
Nfs = 0
OPEN (1,FILE='F\C2F.FI')
DO
   READ (1,90,END=100) Cline
   IF (Cline(1:5) == SP) CYCLE
   Cend = LEN_TRIM(Cline)
   c = 1
   CALL BeginString()
   IF (Cline(c:c+7) == 'FUNCTION'.OR.Cline(c:c+9) == 'SUBROUTINE'.OR. &
                                     Cline(c:c+8) == 'INTERFACE') THEN
      IF (CC(c) == 'F') c = c+8
      IF (CC(c) == 'S') c = c+10
      IF (CC(c) == 'I') c = c+9
      k = Get()
      IF (string == SP) CYCLE
      Nfs = Nfs+1
      C2F_FI(Nfs) = String
   ELSE IF (Cline(1:3) == '!! ') THEN
      c = 4
      DO
         n = Get()
         IF (string == SP) EXIT
         Nfs = Nfs+1
         C2F_FI(Nfs) = STRING    ! shud be upper case already
      END DO
   END IF
END DO
100 CLOSE (1)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! - - - Get source file Name from command line or solicit name - - -
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

WRITE (*,90) 'Options:'
WRITE (*,90) '  filename '
WRITE (*,90) '  filename d      + declaration summary '
WRITE (*,90)
WRITE (*,'(A\)') '>>'

READ (*,90) String
ProgramName = SP
DO n = 1,MAXSTR
   ch = TOUPPER(String(n:n))
   IF (ch == SP) EXIT
   ProgramName(n:n) = ch
END DO
Nsize = n-1
ch = String(n+1:n+1)
IF (TOUPPER(ch) == 'P') Use_pointer2 = .TRUE.
IF (TOUPPER(ch) == 'D') Debug = .TRUE.

IF (ProgramName(Nsize-1:Nsize-1) == '.') ProgramName(Nsize-1:Nsize) = '  ' !remove .c
Nsize = LEN_TRIM(ProgramName)
filename = 'C\' // ProgramName(1:Nsize) // '.C'
INQUIRE (FILE=filename, EXIST=exists)
IF (.NOT.exists) THEN
   WRITE (*,90) ' CANNOT FIND -> C2F\', filename
   STOP
END IF
! - - - Set string size of each Keyword in Keysize - - -
DO Key = 1,MAXKEY
   String = Keywords(Key)
   DO n = 2,MAXKEYLEN
      IF (String(n:n) == SP) EXIT
   END DO
   Keysize(Key) = n-1
END DO

! ----------------------------------------------------------------
!  PASS 1 phase 0, Copies C\NAME.C to F\NAME.C
!  . removes tabs, blank and #include <....> records
!  . reduces blank field in ops field to 1 blank
!  . expands record with multi ; to multi-records;
!  . expands  case: x=1;  to 2 records
!  . expands  x=y=z=nnn;  to 3 records
!  . expands  statement { to 2 records
!  . reorders [][] arrays
!  . include "file.h" source records
! ----------------------------------------------------------------

input_unit = 11
OPEN (11,FILE='C\' // ProgramName(1:Nsize) // '.C', MODE='READ')
OPEN (2,FILE='F\' // ProgramName(1:Nsize) // '.C', MODE='WRITE')
IF (Debug) OPEN (9,FILE='F\' // ProgramName(1:Nsize) // '.ERR', MODE='WRITE')

1 CONTINUE     ! re-entered at EOF include.h file
comment_cont = .FALSE.

DO
   READ (input_unit,90,END=107) Cline    ! until eof

   c1 = 0
   DO c = 1,RECSIZ
      IF (CC(c) == HT) THEN
         CC(c) = SP                      ! replace TAB chars
      ELSE IF (c1 /= 0) THEN             ! already found non-blank col.
         CYCLE
      ELSE IF (CC(c) /= SP) THEN
         c1 = c                          ! 1st nonblank col.
      END IF
   END DO
   IF (c1 == 0) CYCLE                    ! discard blank record

   DO c = RECSIZ,1,-1
      IF (CC(c) /= SP) EXIT
   END DO
   Ceod = c                                ! last nonblank column

   IF (comment_cont) THEN
      IF (Ceod > 1) THEN
         IF (Cline(Ceod-1:Ceod) == '*/') comment_cont = .FALSE.
      ELSE IF (Cline(c1:c1+1) == '*/') THEN
         comment_cont = .FALSE.
      END IF
      CYCLE                              ! discard record
   END IF

   quote_field = .FALSE.
   Cend = 0
   DO c = c1,Ceod                          ! find opfield end
      IF (CC(c) == DQUOTE) quote_field = .NOT.quote_field
      IF (.NOT.quote_field) THEN
         IF (Cline(c:c+1) == '/*'.OR.Cline(c:c+1) == '//') EXIT
      END IF
      IF (CC(c) /= SP) Cend = c            ! track nonblanks to opfield end
   END DO
   IF (Cline(c:c+1) == '//') THEN
      comment_cont = .FALSE.
   ELSE IF (Cline(c:c+1) == '/*') THEN
      comment_cont = .TRUE.
      IF (Cline(Ceod-1:Ceod) == '*/') comment_cont = .FALSE.
   END IF
   IF (Cend == 0) CYCLE                 ! discard record
   Ceod = Cend

   quote_field = .FALSE.
   c = 0
   f = 0
   DO                     ! 1st pass thru record
      c = c+1
      IF (c > Cend) EXIT
      ch = CC(c)
      IF (ch == DQUOTE) quote_field = .NOT.quote_field
      f = f+1
      FF(f) = ch
      IF (f > 2) THEN
         IF (.NOT.quote_field) THEN
            pch = FF(f-1)
            IF (pch == SP) THEN
               IF (ch == SP) THEN
                  f = f-1                  ! remove consecutive sp
               ELSE IF (ch == '(') THEN
                  IF (identifier(ICHAR(FF(f-2)))) THEN
                     f = f-1
                     FF(f) = '('           ! remove sp between fun (
                  END IF
               END IF
            ELSE      ! FF(f-1) /= SP
               IF (pch=='='.OR.pch=='>'.OR.pch=='<') THEN
                  IF (identifier(ICHAR(ch)).AND.FF(f-2) /= '-') THEN
                     FF(f) = SP
                     f = f+1
                     FF(f) = ch                ! insert sp after =><
                  END IF                       ! not sp after ->
               ELSE IF (pch == '!'.AND.ch == SP) THEN
                  f = f-1                      ! remove stored sp after !
                  CYCLE
               ELSE IF (assign_allowed(ICHAR(pch))) THEN
                  IF (ch == '='.OR.ch == '!'.OR. ch == '>'.OR.ch == '<') THEN
                     FF(f) = SP
                     f = f+1
                     FF(f) = ch     ! insert sp before =!><
                  END IF
               END IF
            END IF
         END IF
      ELSE
         IF (FF(1) == SP) f = f-1    ! col.1 not blank
      END IF
   END DO
   IF (FF(f) == SP) f = f-1
   Cline = Fline(1:f)               ! col.1 not blank
   Cend = LEN_TRIM(Cline)

   c = 1
   Key = GetKey()
   IF (Key == C_void) Key = GetKey()  ! interested in setting Program_Sub_Fun

   IF (Program_Sub_Fun) THEN
      IF (CC(Cend) == '}') THEN       ! syn: sub1() {e1,e2}
         c = 1
         CALL MoveToNext('{')              ! break it up into recognized
         WRITE (2,90) Cline(1:c-1)         ! sub()
         WRITE (2,90) '{'
         c = c+1
         CALL BeginString()
         WRITE (2,90) Cline(c:Cend-1)
         WRITE (2,90) '}'
         CYCLE
      END IF

   ELSE IF (Key == C_define) THEN
      c = c+8                  ! past #define
      n = Get()
      CALL BeginString()
      IF (c > Cend.OR.CC(c) == '(') run_pp = .TRUE. ! run cpp next phase

   ELSE IF (Key == C_$if) THEN
      run_pp = .TRUE.    ! detected #if

   ELSE IF (Key == C_include) THEN    ! action taken for  #include "xxx.h"
      c = c+8
      CALL BeginString()
      IF (CC(c) == DQUOTE) THEN
         c = c+1                      ! past "
         c1 = c
         CALL MoveToNext(DQUOTE)
         string = 'C\' // Cline(c1:c-1)
         INQUIRE (FILE=string,EXIST=exists)
         IF (.NOT.exists) THEN
            WRITE (*,*) TRIM(string(3:))//' not found in C2F\C directory'
         ELSE
            input_unit = input_unit+1
            IF (input_unit > 11+20) THEN
               WRITE (*,90) 'INCLUDE files called > 20 at ',TRIM(string)
               STOP
            ELSE
               WRITE (*,90) 'included "user" file ',TRIM(string)
            END IF
            OPEN (input_unit,FILE=string,MODE='READ')
            CYCLE       ! start reading from "nnnn.h" include file
         END IF
      END IF
      CYCLE      ! ignore #include <.....>
   END IF

   ! scan record for syntax cases: cline is modified each case
   ! 0  reorder array
   ! 1  (cast)
   ! 2  % modulo
   ! 3  >>= =<<
   ! 4  >> <<
   ! 5  SP->SP

   c = 0
   quote_field = .FALSE.
   pch = SP
   ch  = SP
   DO
      c = c+1
      IF (c > Cend) EXIT
      pch = ch
      ch = CC(c)

      IF (ch == DQUOTE) quote_field = .NOT.quote_field
      IF (quote_field) CYCLE
      IF (ch == SP) CYCLE

      IF (ch == '[') THEN
         CALL syntax_proc(0)              ! re-order [][] arrays

      ELSE IF (c > 1.AND.Key /= C_function.AND..NOT.identifier(ICHAR(pch)) &
                                          .AND.ch == '(') THEN ! assume (cast)
         CALL syntax_proc(1)              ! (int) x  ->  INT(x)
                                          ! also (*func)(x) -> func(x)
      ELSE IF (ch == '%') THEN
         CALL syntax_proc(2)              ! x%y  -> MOD(x,y)

      ELSE IF (Cline(c:c+1) == '>>'.OR.Cline(c:c+1) == '<<')  THEN
         IF (CC(c+2) == '=') THEN
           CALL syntax_proc(3)
         ELSE
           CALL syntax_proc(4)              !
         END IF

      ELSE IF (Cline(c:c+1) == '->') THEN
         IF (CC(c-1) == SP) THEN
            Cline = Cline(1:c-2) // Cline(c:Cend)
            Cend = Cend-1
            c = c-2         ! back up for 2nd try
         ELSE IF (CC(c+2) == SP) THEN
            Cline = Cline(1:c+1) // Cline(c+3:Cend)
            Cend = Cend-1
         ELSE
            c = c+1
         END IF

      ELSE IF (Cline(c:c+7) == 'typedef ') THEN
         c = c+8
         n = Get()
         IF (string == 'enum') THEN
            Cline = 'typedef_en ' // Cline(c:Cend)  ! change typedef enum syntax
            Cend = LEN_TRIM(Cline)
            c = Cend
         END IF

      ELSE IF (Cline(c:c+5) == 'return') THEN

      END IF
   END DO

   ! multi-statement record separated into individual records;
   c1 = 1
   c = 0
 7 CONTINUE
   openp = 0
   nass = 0
   cass = 0
   ch = SP
   comma = .FALSE.
   quote_field = .FALSE.
   DO
      c = c+1
      IF (c > Cend) EXIT
      pch = ch
      ch = CC(c)
      IF (ch == DQUOTE) quote_field = .NOT.quote_field
      IF (quote_field) CYCLE

      IF (ch == ',') comma = .TRUE.     ! syn: not  x=y=z=1
      IF (c > 1) THEN
         IF (ch == '='.AND.CC(c+1) /= '='.AND.assign_allowed(ICHAR(pch)) &
             .OR.Cline(c-1:c) == '+='.OR.Cline(c-1:c) == '-=' &
             .OR.Cline(c-1:c) == '*='.OR.Cline(c-1:c) == '/=') THEN
            nass = nass+1
            c = c+1                       ! past =
            CALL BeginString()
            IF (CC(c) == '(') c = c+1     ! past  (
            cass(nass) = c                ! save past '=' index
         ELSE IF (ch == ',') THEN
            nass = nass-1                 ! ??
            IF (nass < 0) nass = 0
         END IF
      END IF
      IF (ch == '(') openp = openp+1
      IF (ch == ')') openp = openp-1
      IF (c < Cend) THEN
         IF ((ch == ';'.AND.openp == 0).OR.(ch == ':'.AND. &
             (key == C_case.OR.key == C_default))) THEN   ! assume multi-statement record
            WRITE (2,90) Cline(c1:c)           ! expand into multi-records
            c1 = c+1
            GO TO 7
         END IF
      END IF
   END DO

   IF (Key == C_function.AND.CC(Cend) == ';') THEN  ! breakup multi-prototypes;
      c = 1
      n1 = Get()
      arg1 = string

      DO
         CALL BeginString()
         c1 = c
         CALL MoveToNext('(')
         c = c+1
         CALL BeginString()
         IF (CC(c) == '*') THEN     ! assume syn:  double (*pfunc) (double);
            CALL MoveToNext(')')    ! skip (*pfunc)
         ELSE
            c = c1
         END IF
         CALL MoveToNext('(')    !
         CALL MoveToCloseParens()
         c = c+1                    ! past )
         CALL BeginString()
         IF (CC(c) == ','.OR.CC(c) == ';') THEN  ! syn: int fun1(...), fun2(....);
            WRITE (2,90) arg1(1:n1),SP,Cline(c1:c-1), ';' ! 1 function prototype/record
            IF (CC(c) == ';'.OR.c >= Cend-1) EXIT
            c = c+1                 ! past ,
         END IF
      END DO

   ELSE IF (Key == C_return.AND.nass == 1) THEN     ! syn:  return(x=y...);
      c = 7
      CALL BeginString()
      IF (CC(c) == '(') THEN
        CC(c) = SP
        IF (CC(Cend) == ';'.AND.CC(Cend-1) == ')') THEN
           Cend = Cend-1
           CC(Cend) = ';'
        END IF
      END IF
      CALL BeginString()
      WRITE (2,90) Cline(c:Cend)
      n = Get()            ! x
      WRITE (2,90) 'return ',string(1:n),';'

   ELSE IF (Key == 99.AND.nass > 1.AND..NOT.comma) THEN  ! syn: x=y=z=nnn;
      c = cass(nass)
      CALL BeginString()                     ! nn index
      n = 0
      DO c = c,Cend
         ch = CC(c)
         IF (ch == ';') EXIT
         n = n+1
         arg2(n:n) = ch
      END DO
      narg2 = n
      DO
         c = cass(nass-1)
         IF (c < 1) c = 1
         CALL BeginString()
         n = 0
         DO c = c,Cend
            ch = CC(c)
            IF (ch == SP.OR.ch == '=') EXIT
            n = n+1
            arg1(n:n) = ch
         END DO
         narg1 = n
         CALL BeginString()
         n1 = 1
         IF (arg1(1:1) == '(') THEN
            n1 = 2                 ! remove ( from output
            narg2 = narg2-1        ! remove ) from output
         END IF
         IF (CC(c) /= '=') THEN    ! assume +=  -=  etc.
            WRITE (2,90) arg1(n1:narg1),SP,Cline(c:c+1),SP, arg2(1:narg2), '; '
         ELSE
            WRITE (2,90) arg1(n1:narg1), ' = ', arg2(1:narg2), '; '
         END IF
         nass = nass-1
         IF (nass == 0) EXIT
         narg2 = narg1
         arg2 = arg1
      END DO

   ELSE                       ! not x=y=z=nnn
      ch = CC(Cend)
      IF (ch == '{'.AND.Cend > 1) THEN      ! expand statement{
         CC(Cend) = SP                      ! remove trailing {
         WRITE (2,90) Cline(c1:Cend)
         WRITE (2,90) '{'

      ELSE
         c = c1
         CALL BeginString()                ! sometimes cc(c1) = blank
         WRITE (2,90) Cline(c:Cend)        ! col.1 outputs most records
      END IF
   END IF
END DO
107 CONTINUE

CLOSE (input_unit)
input_unit = input_unit-1
IF (input_unit >= 11) GO TO 1

CLOSE (2)           ! F\NAME.C

! ----------------------------------------------------------------
!  PASS 1 phase 1, Generates F\C2F.1 until EOF F\NAME.C (or NAME.CPP)
! ----------------------------------------------------------------
IF (run_pp) THEN
   arg1 = 'F\' // ProgramName(1:Nsize) // '.C'
   arg2 = 'F\' // ProgramName(1:Nsize) // '.CPP'
   INQUIRE (FILE='CPP.EXE',EXIST=exists)
   IF (exists) THEN
      n = RUNQQ('CPP',' -C -P '//arg1//' '//arg2)   ! use GNU CPP if present
   ELSE   ! assume DVF is installed on system
      n = RUNQQ('FPP',' /ansi /m /C '//arg1//' '//arg2) ! use DVF FPP instead
   END IF
   OPEN (11,FILE='F\' // ProgramName(1:Nsize) // '.CPP',MODE='READ')
   WRITE (*,*) 'pre-processing completed'
ELSE
   OPEN (11,FILE='F\' // ProgramName(1:Nsize) // '.C',MODE='READ')
END IF

OPEN (1,FILE='F\C2F.1',MODE='WRITE')
WRITE (1,90) ProgramName(1:Nsize) // '_1()'   ! psuedo global subroutine create
WRITE (1,90) '{'

Nmod = 1
Key = 0
PrevKey = 0
KeyPrev = 0
add_rightbrace = .FALSE.
delete_leftbrace = .FALSE.

! - - - - - - - - - - - - - - - - - - - - - - -
11 DO     ! process F\NAME.C  -> F\C2F.1

   READ (11,90,END=101) Cline   ! until eof
   IF (Cline(1:80) == SP) GO TO 11         ! because preproc introduces blanks

   IF (CC(1) == SP) THEN                   ! rarely
      c = 2
      CALL BeginString()
      Cline = Cline(c:)
   END IF

   DO c = RECSIZ,1,-1                      ! col.1 is non-blank
      IF (CC(c) /= SP) EXIT
   END DO
   Cend = c                                ! last nonblank column

   FF = SP                                 ! blank FF (Fline)
   f = 1                                   ! reset current Fline/FF index
   c = 1                                   ! reset current Cline/CC index
   c1 = c
   pch = SP                                ! init previous Cline char
   ch = SP
   quote_field = .FALSE.

  ! - - - - - - - - - - - - - - - - - - - - -
  12 DO                       ! cline is not changed below
      IF (c > Cend) EXIT

      pch = ch
      ch = CC(c)
      ch1 = CC(c+1)
      IF (ch == DQUOTE) quote_field = .NOT.quote_field

      IF (quote_field.OR.ch == SP) THEN
         FF(f) = ch                        ! just copy cline -> fline
         f = f+1
         c = c+1

      ELSE IF (ch == '.'.AND.a_z(ICHAR(ch1))) THEN  ! syn: .a
         FF(f) = '%'                                          !  to: %a
         f = f+1
         c = c+1

      ELSE IF (ch == '-'.AND.ch1 == '>') THEN    ! syn: ->
         FF(f) = '%'                        ! is that all there is?
         f = f+1
         c = c+2

      ELSE IF (a_z(ICHAR(ch)).AND..NOT.Identifier(ICHAR(pch))) THEN
         c1 = c                      ! detect start of identifier string
         k = Get()
         argname = UPPER(k)          ! typedefs are entered as UPPER NAME
         DO n = 1,MAXTYPEDEFS        ! search for replacement string
            String = Typedefs(2,n)
            IF (String == SP.OR.String == argname) EXIT
         END DO

         IF (String == argname) THEN  ! recog. typedef
            c = c1+k                  ! advance Cline past NEW_NAME
            argtype = Typedefs(1,n)   ! convert to known type string
            n = LEN_TRIM(argtype)
            Fline(f:f+n-1) = argtype(1:n)
            f = f+n

         ELSE IF (argname == 'ASSERT') THEN ! convert to if(...)printf
            c = c+1                     ! past assert(
            CALL BeginString()
            c1 = c
            Fline(f:f+4) = 'if(!('
            f = f+5
            CALL CopyToFirst(')')
            Fline(f:) = ') exit ' // ' assertion failed: ' // &
                                Cline(c1:c-2) // ';'
            f = LEN_TRIM(Fline)+1
            c = Cend+1

         ELSE IF (CC(c) == '(') THEN   ! check if argname is intrinsic fun.
            changed = .FALSE.
            DO n = 1,NUM_INTRINSICS
               IF (argname == intrinsic_names(1,n)) THEN
                  argname = intrinsic_names(2,n)  ! get replacement
                  k = LEN_TRIM(argname)           ! replace old argname len
                  Fline(f:f+k) = argname(1:k) // '('
                  f = f+k+1
                  ch = '('
                  c = c+1            ! past (
                  changed = .TRUE.
                  EXIT
               END IF
            END DO
            IF (.NOT.changed) THEN   ! check if in C2F_FI table
               changed = .FALSE.
               DO n = 1,Nfs
                  IF (argname == C2F_FI(n)) THEN   ! force UPPER NAME
                     Fline(f:f+k-1) = argname(1:k)
                     Fline(f:f+k) = argname(1:k) // '('
                     f = f+k+1
                     ch = '('
                     c = c+1            ! past (
                     changed = .TRUE.
                     EXIT
                  END IF
               END DO
            END IF
            IF (.NOT.changed) THEN
               FF(f) = ch
               f = f+1
               c = c1+1            ! past x of xyz(
            END IF

         ELSE
            FF(f) = ch
            f = f+1
            c = c1+1               ! past x of xyz
         END IF

      ELSE IF (Cline(c:c+1) == '(*') THEN  ! try to remove () from (*p).x
         c1 = c
         CALL MoveToNext(')')
         c = c+1                 ! past )
         IF (CC(c) == '.') THEN
            Fline(f:) = Cline(c1+1:c-2) // '%'
            f = LEN_TRIM(Fline) +1
            c = c+1              ! past .
         ELSE
            Fline(f:f+1) = '(*'
            f = f+2
            c = c1+2
         END IF

      ELSE IF (ch == '(') THEN
         k = identifier(ICHAR(pch))
         IF (k == 1.AND.FF(f-1) == SP) f = f-1   ! remove blank between fun (..)
         FF(f) = ch
         f = f+1
         c = c+1

      ELSE IF (ch == '#'.AND.ch1 == SP) THEN  ! remove sp between # keywords
         FF(f) = ch
         f = f+1
         c = c+2

      ELSE IF (ch == ';') THEN
         IF (pch == SP) THEN              ! strip blanks ahead of ;
            DO f = f-1,2,-1
               IF (FF(f) /= SP) EXIT
            END DO
            f = f+1
         END IF
         FF(f) = ch
         f = f+1
         c = c+1

      ELSE IF (ch == 'L') THEN
         IF (pch >= '0'.AND.pch <= '9') THEN
            c = c+1                       ! delete L in syn: 365L
         ELSE
            FF(f) = ch
            f = f+1
            c = c+1
         END IF

      ELSE
         FF(f) = ch                        ! copy
         f = f+1
         c = c+1
      END IF
   END DO

  ! - - - - - - - - - - - - - - - - - - - - -
   Cline = Fline                    ! to allow re-processing -> Fline

   DO c = RECSIZ,1,-1               ! get new cline size
      IF (CC(c) /= SP) EXIT
   END DO
   Cend = c                         ! last nonblank column
   chend = CC(Cend)

   IF (PrevKey < 0) THEN            ! accum. continuation records
      f = LEN_TRIM(Bigline)
      IF (f > 1000) THEN
         CALL Error(23)           ! lost info
         PrevKey = 0              ! stop accumulating
         GO TO 11
      ELSE
         DO c = 1,Cend            ! add record to Bigline
            f = f+1
            BB(f) = CC(c)
         END DO
         openp = 0
         openbrace = 0
         quote_field = .FALSE.
         DO c = 1,f
            ch = BB(c)
            IF (ch == DQUOTE) quote_field = .NOT.quote_field
            IF (quote_field) CYCLE
            IF (ch == '(') openp = openp+1
            IF (ch == ')') openp = openp-1
            IF (ch == '{') openbrace = openbrace+1
            IF (ch == '}') openbrace = openbrace-1
         END DO
         IF (openp /= 0.OR.openbrace /= 0.OR. &
             (BB(f) /= ')'.AND.BB(f) /= ';')) GO TO 11
         Cend = f
         IF (Cend > RECSIZ.AND. &
            (PrevKey /= -C_typedef.OR.PrevKey /= -C_struct &
                                  .OR.PrevKey /= -C_union)) THEN
            CALL Error(23)           ! lost info
            PrevKey = 0              ! stop accumulating
            GO TO 11
         END IF
         DO c = 1,Cend             ! copy to CC(1000) max
            CC(c) = Bigline(c:c)
         END DO

         Key = -PrevKey
         Prevkey = 0
         IF (Key < C_program) c = Keysize(Key)+1
         GO TO 121
      END IF
      GO TO 11           ! continue until record; or record)
   END IF

   ! - - - find 1st keyword in record  - - -
   c = 1
   IF (Key >= 0) THEN            ! not data continuation record
      KeyPrev = Key
      Key = GetKey()
      IF (Key == C_static) Key = GetKey()  ! static not relevant below
   END IF

 121 CONTINUE        ! from above accum. record
                     ! or below re-select 2nd key

   IF (add_rightbrace(2)) THEN
      WRITE (1,90) '}'             ! delayed 1 statement after while command
      add_rightbrace(2) = .FALSE.
   ELSE IF (add_rightbrace(1)) THEN
      add_rightbrace(2) = .TRUE.
      add_rightbrace(1) = .FALSE.
   END IF

   SELECT CASE (Key)

   CASE (C_leftbrace)
      IF (delete_leftbrace) THEN    ! while preproc has processed
         delete_leftbrace = .FALSE.
      ELSE
         WRITE (1,90) '{'
      END IF
      c1 = 0
      DO c = 2,Cend
         IF (CC(c) /= SP) THEN
            c1 = c
            EXIT
         END IF
      END DO
      IF (c1 > 0) WRITE (1,90) Cline(c1:Cend)
      CYCLE

   CASE (C_rightbrace)              ! output one } per record
      WRITE (1,90) '}'
      DO c = 2,Cend
         IF (CC(c) == '}') THEN     ! syn:  }   }
            WRITE (1,90) '}'        ! one } per record
            CC(c) = SP              ! remove statement from output
            CYCLE
         END IF
         IF (CC(c) /= SP) EXIT
      END DO
      c1 = c
      n = Get()
      IF (string == 'else'.OR.string == 'elseif') THEN   ! syn:  }else  e1
         Key = C_else
         CALL BeginString()
         IF (Cline(c:c+1) == 'if') THEN           ! syn: } else if
            openp = 0
            DO c = c,Cend
               IF (CC(c) == '(') openp = openp+1
               IF (CC(c) == ')') openp = openp-1
            END DO
            IF (openp == 0) THEN
               WRITE (1,90) Cline(c1:Cend)        ! else if (e2)
               GO TO 11
            END IF               ! else if ( ( e1 )

         ELSE IF (CC(c) == '{') THEN              ! syn: }else{ e1
            WRITE (1,90) string
            WRITE (1,90) '{'
            c = c+1
            CALL BeginString()
            c1 = c
            Key = GetKey()                ! get e1 key
         END IF
         Cline = Cline(c1:)               ! syn:  else  e1
         Cend = LEN_TRIM(Cline)
         GO TO 121           ! backup and re-process else or else if
      END IF
      GO TO 11

   CASE (C_if, C_else, C_enum, C_typedef_en, C_for, C_while)
      PrevKey = 0
      openp = 0
      quote_field = .FALSE.
      DO n = 1,Cend
         ch = CC(n)
         IF (ch == DQUOTE) quote_field = .NOT.quote_field
         IF (quote_field) CYCLE
         IF (ch == '(') openp = openp+1
         IF (ch == ')') openp = openp-1
      END DO
      IF (openp /= 0.OR. &
         ((Key == C_enum.OR.Key == C_typedef_en).AND.chend /= ';')) THEN
         PrevKey = -Key
         Bigline = Cline(1:Cend)
         GO TO 11
      ELSE IF (Key == C_enum.OR.Key == C_typedef_en) THEN
         CALL enum_preproc()
      ELSE IF (Key == C_if.OR.Key == C_else) THEN
         CALL if_else_preproc()
      ELSE IF (Key == C_for) THEN
         CALL for_preproc()
      ELSE IF (Key == C_while) THEN
         CALL while_preproc()
      END IF

   CASE (C_fgets:C_sscanf)
      CALL format_preproc()

   CASE (C_void, C_program, C_subroutine, C_function)
      IF (Key == C_void) THEN
         n = Get()
         IF (String == 'main') THEN
            Key = C_program
         ELSE
            CALL BeginString()
            IF (CC(c) == '('.OR.identifier(ICHAR(CC(c)))) THEN
               Key = C_subroutine
               SubName = string
            ELSE
               Key = 99         ! void temp =
               GO TO 121        ! void *a
            END IF
         END IF
      END IF
      PrevKey = 0
      ch = CC(Cend)
      IF (ch == '('.OR.ch == ','.OR.identifier(ICHAR(ch))) THEN  ! continues
         PrevKey = -Key               ! declaration continue
         Bigline = Cline(1:Cend)      ! start accum. declarations
         GO TO 11                     ! process arglist continuation records

      ELSE IF (Key == C_program.AND.ch == ';') THEN     ! main() {...};
         WRITE (1,90) '}'             ! completes psuedo global sub creation
         WRITE (1,90) 'EOF EOF EOF'
         Nmod = Nmod+1
         ModuleNames(Nmod) = ProgramName     ! already upper case
         DO c = Cend-1,3,-1
            IF (CC(c) == '{') EXIT
         END DO
         WRITE (1,90) Cline(1:c-1)            ! main()
         WRITE (1,90) '{'
         c = c+1
         CALL BeginString()
         WRITE (1,90) Cline(c:Cend)
         GO TO 11

      ELSE IF (CC(Cend) /= ';') THEN          ! new subfun detect
         IF (Nmod == 1) WRITE (1,90) '}' ! completes psuedo global sub creation
         WRITE (1,90) 'EOF EOF EOF'
         WRITE (1,90) Cline(1:Cend)
         Nmod = Nmod+1
         IF (Key == C_program) THEN
            ModuleNames(Nmod) = ProgramName
         ELSE IF (Key == C_subroutine) THEN
            ModuleNames(Nmod) = SubName
         ELSE
            ModuleNames(Nmod) = FunName
         END IF
         GO TO 11                     ! until EOF NAME.C
      END IF                         ! output record, with no action

   CASE (C_typedef, C_struct, C_union)
      PrevKey = 0
      openbrace = 0
      DO n = 1,Cend
         IF (CC(n) == '{') openbrace = openbrace+1
         IF (CC(n) == '}') openbrace = openbrace-1
      END DO
      IF (openbrace.OR.chend /= ';') THEN
         PrevKey = -Key
         Bigline = Cline(1:Cend)
         GO TO 11
      ELSE
         CALL structure_preproc()
      END IF

   CASE (C_define)
      CALL define_preproc()

   CASE (C_const,-C_const, C_short:C_FILE, -C_FILE:-C_short)
      CALL declare_preproc()

   CASE DEFAULT
      CALL statement_preproc()
   END SELECT

   IF (Cline(1:1) /= SP) WRITE (1,90) Cline(1:Cend)

END DO

101 CLOSE (11)

IF (Nmod == 1) WRITE (1,90) '}'    ! complete psuedo global subroutine
WRITE (1,90) 'EOF EOF EOF'
CLOSE (1)
OPEN (1,FILE='F\C2F.1',MODE='READ')     ! stays open until pass 2 reads EOF

Nmod1 = Nmod        ! save pass 1 count
Nmod = 0            ! reset for pass 2
Npass = 2           ! identify output unit for error reporting

! ----------------------------------------------------------------
!  PASS 2 phase 1
! ----------------------------------------------------------------
2 CONTINUE
CLOSE (21)
OPEN (21,FILE='F\C2F.21',MODE='WRITE')              ! prepass 2 output

! - - - initialize for next module - - -
Nmod = Nmod+1

IF (Nmod > MAXMOD) THEN
   CALL Error(7)         ! disasterous
   STOP
END IF
IF (Nmod == 2) g = v            ! save global declarations from module 1
IF (Nmod > 2)  v = g
v.nc = -ABS(v.nc)               ! mark entries from global un-referenced

Labels      = SP
Formats     = SP
caselabel   = SP
List_Suffixes = SP
Nfunc    = 0
Key_Save = 0
Col_Save = 0
Brk_Save = 0
Nkeysave = 0
Indentcol = 0
Localsection = .TRUE.
Labels = SP
Formats = SP
FF = SP
f = 1 + Indentcol
Keys = 0
Nrec = 0

! setup Keys for F\C2F.1 records
! - - - - - - - - - - - - - - -
21 DO
   READ (1,90,END=4) Cline                 ! until C2F translation complete
   IF (Cline(1:10) == SP) THEN      ! blank record unexpected
      CALL Error(34)
      GO TO 21
   END IF
   WRITE (21,90) Cline                     ! copy 1 module to prepass file

   IF (Cline(1:7) == 'EOF EOF') GO TO 201  ! end of module, go to phase 2
   Nrec = Nrec +1

   DO n = RECSIZ,1,-1
      IF (CC(n) /= SP) EXIT
   END DO
   Cend = n
   Cends(Nrec) = Cend
   IF (Cend > 0) Chends(Nrec) = CC(Cend)           ! save last char in opfield

   ! - - - assign 1st keyword in record to Keys - - -
   c = 1
   Key = GetKey()
   Keys(Nrec) = Key

   Insert(Nrec) = .FALSE.
   IF (Cend > 6.AND.Cline(Cend-6:Cend) == '/*C2F*/') THEN
      Cend = LEN_TRIM(Cline(1:Cend-7))
      Cends(Nrec) = Cend
      Insert(Nrec) = .TRUE.
   END IF

END DO       ! 21

201 CLOSE (21)            ! C2F.21  1 module written
OPEN (21,FILE='F\C2F.21',MODE='READ')
CLOSE (2)
OPEN (2,FILE='F\C2F.2',MODE='WRITE')
OPEN (22,FILE='F\C2F.DAT',MODE='WRITE')

! - - - Initialize - - -
Indentcol = 0
Key_Save = 0
Nrec = 0
Key = 0

22 DO     ! process pass 1 output records
   READ (21,90,END=3) Cline         ! input pass 21 record, exit to pass 3

   IF (Cline(1:7) == 'EOF EOF') GO TO 22  ! discard
   Nrec = Nrec +1                   ! this record #
   Cend = Cends(Nrec)
   chend = Chends(Nrec)
   chend2 = Chends(Nrec+1)
   Key2 = Keys(Nrec+1)
   Key3 = Keys(Nrec+2)
   IF (.NOT.casecase) THEN          ! reset Fline if not merging case statements
      FF = SP
      f = 1 + Indentcol             ! reset default start column for Fline
   END IF

   c = 1                            ! reset
   IF (Key >= 0) THEN               ! prev Key processing was completed
      Key = Keys(Nrec)              ! get input record key
      IF (Key <= MAXKEY) c = 1+Keysize(Key)  ! set initial c past keyword
   END IF
   IF (Key /= C_case.AND.Key /= C_default.AND.chend == ':') Key = C_label

   Lconst  = .FALSE.
   Lstatic = .FALSE.   ! reset key = static detect
   Lvoid   = .FALSE.   ! processed by statement_proc

222 Nextkey = 0        ! re-entry from partial record processing
                       ! to process additional keys in record
   SELECT CASE (Key)

   CASE (C_leftbrace)
      CALL leftbrace_proc()

   CASE (C_rightbrace)
      CALL rightbrace_proc()

   CASE (C_auto,C_register)
      Key = GetKey()      ! discard, get next key
      GO TO 222

   CASE (C_signed, C_unsigned)
      c1 = c
      Key = GetKey()         ! get actual type
      IF (Key == 99) THEN    ! no type
         c = c1
         Key = C_int         ! use integer type
      END IF
      GO TO 222

   CASE (C_const)
      Lconst = .TRUE.
      Key = GetKey()      ! get next key
      GO TO 222

   CASE (C_extern)
      CALL extern_proc()

   CASE (C_static)
      Lstatic = .TRUE.
      Key = GetKey()      ! get next key
      GO TO 222

   CASE (C_void)
      CALL BeginString()
      c7 = c
      IF (CC(c) == '*') c = c+1
      Key = GetKey()         ! check next key
      IF (Key /= 99) THEN          ! probable syn: void sub(
         c = c7
         GO TO 222                 ! process next key
      ELSE
         CALL BeginString()
         IF (CC(c) == ','.OR.CC(c) == ';') THEN   ! syn: void x,y;
            c = c7
            Key = C_void
            CALL declare_proc()
         ELSE                            ! syn: void tmp=*x;
            c = c7
            Lvoid = .TRUE.
            CALL statement_proc()
         END IF
      END IF

   CASE (C_short:C_FILE,  -C_FILE:-C_short)
      CALL declare_proc()

   CASE (C_data)
      CALL data_proc()

   CASE (C_struct,C_union)
      CALL struct_union_proc()

   CASE (C_define)
      CALL define_proc()

   CASE (C_enum)
      CALL enum_proc()

   CASE (C_break)
      CALL break_proc()

   CASE (C_case, C_default)
      CALL case_default_proc()
      IF (casecase) CYCLE

   CASE (C_continue)
      CALL continue_proc()

   CASE (C_do, C_for)
      CALL do_for_proc()

   CASE (C_while)
      CALL while_proc()

   CASE (C_goto)
      CALL goto_proc()

   CASE (C_if)
      CALL if_proc()

   CASE (C_else)
      CALL else_proc()

   CASE (C_return)
      CALL return_proc()

   CASE (C_switch)
      CALL switch_proc()

   CASE (C_label)
      CALL label_proc()

   CASE (C_exit)
      CALL exit_proc()

   CASE (C_fprintf)
      CALL fprintf_proc()

   CASE (C_fputs)
      CALL fputs_proc()

   CASE (C_printf)
      CALL printf_proc()

   CASE (C_fscanf, C_scanf, C_sscanf)
      CALL scan_proc()

   CASE (C_fgets)
      CALL fgets_proc()

   CASE (C_getchar)
      CALL getchar_proc()

   CASE (C_putc, C_putch, C_puts)
      CALL put_proc()

   CASE (C_fclose)
      CALL fclose_proc()

   CASE (C_strcat)
      CALL strcat_proc()

   CASE (C_strcpy,C_strncpy)
      CALL strcpy_proc()

   CASE (C_malloc,C_calloc)         ! function detect, entered from nextkey
      CALL malloc_proc()

   CASE (C_memcpy,C_memmove)
      CALL memcpy_memmove_proc()

   CASE (C_program)
      CALL program_proc

   CASE (C_subroutine)
      CALL subroutine_proc()

   CASE (C_function)
      CALL function_proc()

   CASE (99,-99)
      CALL statement_proc()

   CASE (C_$if)
       ! output statement as is

   CASE (C2F_attach)
      Cline = Cline(12:Cend)        ! remove 'C2F_attach '
      Cend = Cend-11
      c = 1
      Key = GetKey()
      IF (Key /= 99) GO TO 222
      c = 1                         ! backup c to statement begin
      f = f+INDENT
      CALL statement_proc()
      f = f-INDENT

   CASE (C2Fpointer)
      CALL pointer_proc()

   CASE DEFAULT                     ! for unrecog. Key until CASE provided
      c = c - Keysize(Key)          ! backup c to keyword begin
      Key = 99                      ! let statement_proc handle this Key
      CALL statement_proc()         ! IF IT CAN!
   END SELECT

   ! - - - - - - - - - - - - - - - - - - - -
   IF (Key == 0) CYCLE    ! fline already output

   IF (Nextkey > 0) THEN
      Key = Nextkey
      GO TO 222                     ! more processing to do for this c record
   END IF

   f = LEN_TRIM(Fline)
   IF (Insert(Nrec)) THEN
      IF (f > 0) WRITE (2,90) Fline(1:f), DOWN ! output down-arrow to pass3
   ELSE
      IF (f > 0) WRITE (2,90) Fline(1:f)
   END IF
   IF (caselabel /= SP) THEN        ! Fline == CASE statement
      WRITE (2,90) caselabel        ! followed by _0 CONTINUE
      caselabel = SP                ! reset, dont do again next rec.
   END IF

!  - - - PASS2  cleanup open -control - - -

   IF (Key == 99) THEN

      DO                            ! until no - controls to cleanup
         indentsave = Indentcol
         brk_expectedsave = Brk_expected
         control = Pull()           ! get current control key

         FF = SP                    ! reset possible additional rec.
         f = 1 + Indentcol

         SELECT CASE (control)

         CASE (-C_for,-C_while)     ! single statement for,while(..)
            IF (Key2 == C2F_attach.OR.Insert(Nrec)) THEN
               CALL Push(control)   ! delay end do to include next record
               EXIT                 ! get out of this loop
            END IF
            Fline(f:f+5) = 'END DO'
            f = f+6
            WRITE (2,90) Fline(1:f)

         CASE (-C_if)               ! single statement after if(..)
            IF (Key2 == C_if.OR.Key2 == C_else) THEN
               CALL Push(-C_if)     ! let if/else_proc cleanup -C_if
               EXIT                 ! get out of this loop
            END IF

         CASE (-C_else)             ! single statement after else
            IF (Key2 == C_else) THEN      ! or else if (..)
               CALL Push(-C_else)   ! let else_proc cleanup -C_else
               EXIT
            ELSE
               Fline(f:f+5) = 'END IF'
               f = f+6
               WRITE (2,90) Fline(1:f)
            END IF

         CASE DEFAULT               ! no Key_Save clean up identified
            CALL Push(control)      ! save
            Indentcol = indentsave
            Brk_expected = brk_expectedsave
            EXIT                    ! get out of this loop
         END SELECT
      END DO
   END IF
END DO          ! until C2F.21 EOF exits to 3

STOP 'err exit pass2'

! ----------------------------------------------------------------
!  PASS 3 confirming declarations used
! ----------------------------------------------------------------
3 CONTINUE
Npass = 3
CLOSE (21,STATUS='DELETE')
CLOSE (2)
OPEN (2,FILE='F\C2F.2',MODE='READ')
CLOSE (22)
OPEN (22,FILE='F\C2F.DAT',MODE='READ')

READ (2,90)      ! dont examine 1st record

31 DO
   READ (2,90,END=301) Cline

   DO c = RECSIZ,1,-1
      IF (CC(c) /= SP) EXIT
   END DO
   Cend = c

   quote_field = .FALSE.
   c = 0
   ch = sp
   pch = sp
   ! - - - - - - - - - - - - - - - - - - - -
   DO
      c = c+1
      IF (c > Cend) EXIT
      pch = ch
      ch = CC(c)
      ch1 = CC(c+1)
      IF (ch == SP) CYCLE
      IF (ch == DQUOTE) quote_field = .NOT.quote_field
      IF (quote_field) CYCLE

      IF (a_z(ICHAR(ch)).AND..NOT.identifier(ICHAR(pch))) THEN
         n = Get()                        ! get string name
         ch = CC(c)                       ! becomes pch
         IF (ch == SP) CYCLE              ! not  xx[ or xx(

         CALL BeginString()
         nid = GetIndex(String)   ! also marks name referenced (-nc -> +nc)
         IF (nid > 0) THEN
            IF (ch == '['.AND.v.declare(nid) == PTR1) THEN
               v.ptr_array(nid)=.TRUE.  ! C2F shud have marked it earlier
               IF (Debug.AND.v.ptr_array(nid) == .FALSE.) CALL Error(31) ! pass 3 fixup
            END IF
         ELSE    ! unknown, may be call to module processed by C2F directly
            IF (string == ModuleNames(Nmod)) THEN    ! module calls itself
               Recurse(Nmod) = .TRUE.              ! direct recursion

            ELSE    ! but most probable is sub() or fun(), check if its known
               IF (Debug.AND.ch == '(') THEN  ! not mandatory to check
                  DO n1 = 2,Nmod1       ! check pass1 > program
                     IF (string == ModuleNames(n1)) EXIT
                  END DO
                  IF (n1 > Nmod1) THEN     ! not found as modulename
                     arg1 = UPPER(n)          ! C2F_FI names are upper
                     DO n = 1,Nfs
                        IF (arg1 ==  C2F_FI(n)) EXIT  ! subfun() name known
                     END DO
                     IF (n > Nfs) CALL Error(30) ! unknown ref?
                  END IF
               END IF
            END IF
         END IF
         c = c-1               ! backup to detect next string
         ch = CC(c)            ! for next pch
      END IF

   END DO       ! until Cend
END DO          ! until EOF

301 REWIND (2)

! - - - - - - - - - - - - - - -
! PASS 3 start final output
! - - - - - - - - - - - - - - -
OPEN (3,FILE='F\' // ProgramName(1:Nsize) // '.F90') ! cant backspace MODE=WRITE
READ (2,90) Cline                       ! module ident record
Cend = LEN_TRIM(Cline)
funtyp = ModuleTypes(Nmod)
IF (Nmod == 1) THEN
   WRITE (3,90) '! --------------------------------------------------'
   WRITE (3,90) Cline(1:Cend),  '         ! global declarations'
   WRITE (3,90) '! --------------------------------------------------'
   WRITE (3,90) "INCLUDE 'C2F.FD' "
   WRITE (3,90) "INCLUDE 'C2F.FI' "
   ! - - - set Nmod=1 identifiers to global, except for FUN - - -
   DO nid = 1,MID
      IF (v.nc(nid) == 0) EXIT
      IF (v.locate(nid) /= EXTERN.AND. &
          v.locate(nid) /= NONE) v.locate(nid) = GLOBAL
   END DO

ELSE IF (funtyp == C_program) THEN
   WRITE (3,*)               ! upchuck a line
   WRITE (3,90) '! --------------------------------------------------'
   WRITE (3,90) Cline(1:Cend)
   WRITE (3,90) '! --------------------------------------------------'
   WRITE (3,90) 'USE ', ProgramName(1:Nsize), '_1'
   WRITE (3,90) 'IMPLICIT NONE'

ELSE
   FF = SP
   f = 1
   c = 1
   CALL CopyToFirst('(')
   DO                            ! check if argname shud be argname_
      n = Get()
      Fline(f:f+n-1) = string(1:n)
      f = f+n
      nid = GetIndex(string)

      IF (nid > 0.AND.v.stored(nid)) THEN
         IF (v.locate(nid) == LOCAL) THEN
            FF(f) = '_'
            f = f+1
         ELSE IF (v.declare(nid) == SCALAR.AND.v.dim(nid) == SP) THEN
            FF(f) = '_'
            f = f+1
         ELSE IF (v.declare(nid) == PTR1.AND.v.dim(nid) == SP &
                                        .AND.v.type(nid) == TSTRUCT) THEN
            FF(f) = '_'
            f = f+1
            n = GetNextIndex()   ! declare arg_ independently
            v.name(n) = TRIM(v.name(nid)) // '_'
            v.nc(n) = v.nc(nid) +1
            v.name2(n) = v.name2(nid)
            v.type(n) = v.type(nid)
            v.declare(n) = v.type(nid)
            v.locate(n) = v.locate(nid)
            v.declare(n) = SCALAR
            v.target(n) = .TRUE.
         END IF
      END IF

      IF (CC(c) == ',') THEN
         FF(f) = ','
         f = f+1
         c = c+1
         CYCLE
      ELSE IF (CC(c) == ')'.OR.c < Cend) THEN
         Fline(f:) = Cline(c:Cend)
         EXIT
      ELSE
         EXIT   ! something wrong with arg list, but dont hangup
      END IF
   END DO
   f = LEN_TRIM(Fline)

   WRITE (3,*)               ! upchuck a line
   WRITE (3,90) '! --------------------------------------------------'
   IF (Recurse(Nmod)) THEN
      WRITE (3,90) 'RECURSIVE '  // Fline(1:f)
   ELSE
      WRITE (3,90) Fline(1:f)
   END IF

   WRITE (3,90) '! --------------------------------------------------'
   WRITE (3,90) 'USE ', ProgramName(1:Nsize), '_1'
   WRITE (3,90) 'IMPLICIT NONE'
   WRITE (3,90) '! - - - arg types - - -'
   IF (funtyp == C_function) WRITE (3,90) output_s  ! from function_proc
END IF

! - - - format/output declarations - - -

DO nlocate = NONE,STATIC
   IF (Nmod == 1.AND.nlocate /= GLOBAL) CYCLE  ! only output global in global
   IF (Nmod > 1.AND.nlocate == LOCAL) WRITE (3,90) '! - - - local declarations - - -'
   FF = SP

DO ntype = TSHORT,TSTRUCT   ! int2,int4,int4,real,real8,char,FILE,TYPE
DO ndeclare = CONST,SUB     ! const,scalar,array1-3,ptr1,ptr2,fun,sub
DO nid = 1,MID

   name = v.name(nid)
   nc = ABS(v.nc(nid))   ! activate all entries for output
   IF (nc == 0) EXIT     ! end entries

   IF (nlocate == NONE) THEN
      IF (name(1:4) == 'I___'.AND.ntype == TINT &
                             .AND.ndeclare == SCALAR) THEN
         WRITE (3,'(A)') '  POINTER (' // name(1:nc) // ', ' // name(5:nc) // ')'
      END IF
      CYCLE
   END IF

   IF (v.locate(nid) == GLOBAL.AND.Nmod > 1) CYCLE ! dont declare globals
   IF (v.declare(nid) == FUN.AND.v.nc(nid) < 0) CYCLE !this FUN not ref'ed

   IF (v.locate(nid) /= nlocate.OR.v.declare(nid) /= ndeclare &
                               .OR.v.type(nid) /= ntype) CYCLE

   IF (v.init(nid) == '{}') CYCLE ! pre-processed -> data file

   CALL fline2_proc(nid)   ! declaration format fline2(1:f2)
   n = LEN_TRIM(Fline2)

   IF (Fline(1:n) == SP) THEN   ! accept new declaration
      f2 = n
      Fline = Fline2(1:n) // ' ::'
      f = n+5
   ELSE IF (Fline2(1:n) /= Fline(1:f2).OR.Nmod == 1) THEN
      WRITE (3,90) Fline(1:f-2)    ! output current record minus ,
      f2 = n
      Fline = Fline2(1:n) // ' ::' ! accept new declaration
      f = n+5
   END IF                    ! else add to existing fline

   Fline(f:f+nc-1) = name(1:nc)
   f = f+nc

   IF (nlocate == EXTERN) GO TO 33    ! simple declare

   ! - - - format ARG output - - -
   dim_s = v.dim(nid)
   init_s = v.init(nid)

   IF (nlocate == ARG) THEN

     IF (v.stored(nid).AND.dim_s == SP.AND.ntype /= TSTRUCT) THEN
        Fline(f:f+nc+1) = '_,' // name(1:nc)  ! double name declare
        f = f+nc+2
     ELSE IF (dim_s == '(C2F_BIGNUM)') THEN
         Fline(f:f+11) = '(C2F_BIGNUM)'
         f = f+12
      ELSE IF ((ndeclare == CONST.OR.ndeclare == SCALAR).AND. &
         dim_s == '[]'.AND.ntype /= TCHAR) THEN
         Fline(f:f+2) = '(*)'
         f = f+3
      ELSE IF (ndeclare == PTR1.AND.v.ptr_array(nid).AND..NOT.v.ptr_deref(nid)) THEN
         Fline(f:f+2) = '(*)'
         f = f+3
      ELSE IF (ndeclare == ARRAY1) THEN
         Fline(f:f+2) = '(*)'
         f = f+3
      ELSE IF (ndeclare == ARRAY2) THEN
         IF (dim_s(1:5) /= '(*,*)') THEN    ! syn:  sub(int x[5][5])
            n = LEN_TRIM(dim_s)
            dim_s(1:1) = '('                ! probably = '['
            Fline(f:f+n-1) = dim_s(1:n)
            f = f+n
         ELSE
            string = v.name(nid+1)          ! $1
            n = LEN_TRIM(string)
            Fline(f:f+n) = '(' // string(1:n)
            f = f+n+1
            string = v.name(nid+2)          ! $2
            n = LEN_TRIM(string)
            Fline(f:f+n+1) = ',' // string(1:n) // ')'
            f = f+n+2
         END IF
      ELSE IF (ndeclare == PTR2.AND.Use_pointer2) THEN
         Fline(f:f+2) = '(*)'
         f = f+3
      END IF

   ELSE     ! format LOCAL section

      IF (name(1:2) == 'T_'.AND.(ndeclare == SCALAR.OR.ndeclare == ARRAY1)) THEN
         n1 = LEN_TRIM(name)
         string = name(3:n1)          ! base name
         n = GetIndex(string)
         IF (n > 0) THEN
            WRITE (3,90) '  POINTER (' // name(3:n1) // ', ' // name(1:n1) // ')'
         END IF
      END IF

      IF (ndeclare == CONST) THEN    ! add PARAMETER :: x=value
         n = LEN_TRIM(init_s)
         IF (init_s(1:1) /= '=') THEN
            Fline(f:f+n) = '='//init_s(1:n)
            f = f+n+1
         ELSE
            Fline(f:f+n-1) = init_s(1:n)
            f = f+n
         END IF

      ELSE IF (ndeclare == SCALAR.AND.init_s /= SP.AND. &
               (nlocate == STATIC.OR.nlocate == GLOBAL)) THEN
         n = LEN_TRIM(init_s)
         i = 1
         IF (init_s(1:1) == '=') i = 2
         IF (init_s(n:n) == ';') n = n-1
         Fline(f:f+n-i+1) = '='//init_s(i:n)
         f = f+n-i+2

      ELSE IF (ndeclare == PTR1.AND.v.ptr_array(nid).AND.dim_s /= SP) THEN
         Fline(f:f+2) = '(:)'
         f = f+3

      ELSE IF (ndeclare == PTR2.AND.Use_pointer2) THEN
         Fline(f:f+2) = '(:)'
         f = f+3

      ELSE IF (ndeclare == PTR2.AND..NOT.Use_pointer2) THEN
         IF (v.dim(nid) == SP) THEN
            Fline(f:f+4) = '(:,:)'
            f = f+5
         ELSE              ! use values gen'ed by malloc
            n = LEN_TRIM(dim_s)
            Fline(f:f+n-1) = dim_s(1:n)
            f = f+n
         END IF

      ELSE IF (ndeclare == ARRAY1.AND.ntype == TCHAR.AND.dim_s /= '[]') THEN
         n1 = LEN_TRIM(dim_s)
         DO n = n1-1,2,-1
            IF (dim_s(n:n) == ',') EXIT
         END DO
         Fline(f:) = '(' // dim_s(n+1:n1)
         f = LEN_TRIM(Fline)+1
         WRITE (3,90) Fline(1:f)
         FF = SP
!            WRITE (3,90) '  CHARACTER :: '//name(1:nc)//'2('//dim_s(2:n1) &
         WRITE (3,90) '  CHARACTER :: '//name(1:nc)//'2(99,' //dim_s(2:n1) &
                  //  '; EQUIVALENCE ('//name(1:nc)//'2,'//name(1:nc)//')'

      ELSE IF (ndeclare >= ARRAY1.AND.ndeclare <= ARRAY3.AND.dim_s /= '[]') THEN
         n = LEN_TRIM(dim_s)
         dim_s(1:1) = '('                ! probably = '['
         Fline(f:f+n-1) = dim_s(1:n)
         f = f+n

    ! caution: nid values for dim_s, init_s are changed by below n = values

      ELSE IF ((ntype == TUNION.OR.ntype == TSTRUCT).AND.Fline(8:8) /= '(') THEN
         WRITE (3,90) Fline(1:f)    ! TYPE NAME
         IF (ntype == TUNION) WRITE (3,90) '  UNION'
         DO n = nid+1,MID
            nc = ABS(v.nc(n))
            IF (nc == 0.OR..NOT.v.member(n)) EXIT ! end of type%members
            IF (ntype == TUNION) WRITE (3,90) '  MAP'
            name = v.name(n)
            k = 0
            DO i = 2,nc
               IF (name(i:i) == '%') k = k+1
            END DO
            IF (k > 1) CYCLE        ! dont output type%type%member
            CALL fline2_proc(n)     ! format member type declaration
            f = LEN_TRIM(Fline2) +1
            DO i = 2,nc             ! get col. for %name
               IF (name(i:i) == '%') EXIT
            END DO
            IF (i < 2) EXIT         ! shud not happen, if type member
            k = nc-i+4
            Fline2(f:f+k-1) = ' :: '//name(i+1:nc)
            f = f+k

            IF (v.declare(n) == CONST) THEN    ! add PARAMETER :: x=value
               init_s = v.init(n)
               k = LEN_TRIM(init_s)
               IF (init_s(1:1) /= '=') THEN
                  Fline(f:f+k) = '='//init_s(1:k)
                  f = f+k+1
               ELSE
                  Fline(f:f+k-1) = init_s(1:k)
                  f = f+k
               END IF
            ELSE IF (v.declare(n) == SCALAR) THEN
               IF (v.init(n) /= SP) THEN
                  init_s = v.init(n)              ! insert INIT value
                  k = LEN_TRIM(init_s)
                  Fline(f:f+k+2) = ' = '//init_s(1:k)
                  f = f+k+3
               END IF
            ELSE IF (v.declare(n) == PTR1.AND.v.type(n) /= TSTRUCT) THEN
               Fline2(f:f+2) = '(:)'
               f = f+3
            ELSE IF (v.declare(n) == PTR2.AND.Use_pointer2) THEN
               Fline2(f:f+2) = '(:)'
               f = f+3
            ELSE IF (v.declare(n) == PTR2.AND..NOT.Use_pointer2) THEN
               IF (v.dim(n) == SP) THEN
                  Fline2(f:f+4) = '(:,:)'
                  f = f+5
               END IF
            ELSE IF (v.declare(n) == ARRAY1.AND.v.type(n) == TCHAR) THEN
               dim_s = v.dim(n)
               n1 = LEN_TRIM(dim_s)
               DO i = n1-1,2,-1
                  IF (dim_s(i:i) == ',') EXIT
               END DO
               Fline(f:) = '(' // dim_s(i+1:n1)
               f = LEN_TRIM(Fline)+1

            ELSE IF (v.declare(n) >= ARRAY1.AND.v.declare(n) <= ARRAY3) THEN
               dim_s = v.dim(n)
               k = LEN_TRIM(dim_s)
               dim_s(1:1) = '('                ! probably = '['
               Fline2(f:f+k-1) = dim_s(1:k)
               f = f+k
            END IF
            WRITE (3,90) '  '//Fline2(1:f)
            IF (ntype == TUNION) WRITE (3,90) '  END MAP'
         END DO
         IF (ntype == TUNION) WRITE (3,90) '  END UNION'
         WRITE (3,90) '  END TYPE'
         Fline = SP
         CYCLE
      END IF
   END IF

   33 CONTINUE
   IF (f > 70) THEN  ! output existing line
      WRITE (3,90) Fline(1:f)
      FF = SP
   ELSE            ! keep formatting until forced to write
      FF(f) = ','
      f = f+1
   END IF

END DO    ! nid
END DO       ! ndeclare
END DO          ! ntype
IF (f > f2.AND.Fline /= SP) WRITE (3,90) Fline(1:f-2)
END DO      ! local declarations output

! output declarations with lengthy data initialization from file
DO
   READ (22,90,END=103) Cline
   IF (CC(1) == HT) CYCLE      ! its executable initialization, handled below
   Cend = LEN_TRIM(Cline)
   DO c = Cend,10,-1           ! find identifier name for possible target attribute
      IF (Cline(c:c+1) == '::') EXIT
   END DO
   c1 = c
   c = c+2
   n = Get()
   nid = GetIndex(string)      ! nid MUST exist
   IF (nid > 0.AND.v.target(nid)) THEN    ! insert ,TARGET ::
      WRITE (3,90) Cline(1:c1-2) // ',TARGET ' // Cline(c1:Cend)
   ELSE
      WRITE (3,90) Cline(1:Cend)
   END IF
END DO
103 REWIND (22)

IF (Nmod > 1) THEN     ! global cannot have any executable statements
   WRITE (3,90)
   WRITE (3,90) '! - - - begin - - -'
END IF

! output lengthy data initialization execution statements from file
DO
   READ (22,90,END=104) Fline
   IF (FF(1) /= HT) CYCLE        ! declaration output above
   f = LEN_TRIM(Fline)
   WRITE (3,90) Fline(2:f)       ! output minus HT
END DO
104 CLOSE (22,STATUS='DELETE')

Localsection = .FALSE.           ! now processing non-declarations

IF (CommandLine) THEN                   ! set true C_program key proc
   WRITE (3,90) '  argc = NARGS()       ! #args from command line'
   WRITE (3,90) '  DO n = 1,20'
   WRITE (3,90) '     CALL GETARG(n-1, argv(n))  ! get command line arguments'
   WRITE (3,90) '     IF (n == argc) EXIT '
   WRITE (3,90) '  END DO '
   CommandLine = .FALSE.   !only for PROGRAM unit
END IF

! output initialization statements
DO nid = 1,MID
   name = v.name(nid)
   nc = ABS(v.nc(nid))          ! output whether referenced or not
   IF (nc == 0) EXIT
   IF (v.dim(nid) == '(C2F_BIGNUM)') THEN
      WRITE (3,90) '  ',name(1:nc-1), ' => ', name(1:nc)
      CYCLE
   END IF
   init_s = v.init(nid)
   IF (init_s == '{}') CYCLE
   IF (init_s /= SP.AND.v.declare(nid) == SCALAR &
                   .AND.v.locate(nid) == LOCAL) THEN
      init_s = v.init(nid) ; k = LEN_TRIM(init_s)
      IF (init_s(k:k) == ';') k = k-1
      IF (init_s(k:k) == ']') THEN
         init_s(k:k+2) = '+1)'
         k = k+2
      END IF
      DO i = 1,k
         IF (init_s(i:i) == '[') init_s(i:i) = '('   ! assume int x=b[n]  call seq.init
      END DO
      WRITE (3,90) '  ',name(1:nc), ' = ', init_s(1:k)

   ELSE IF (v.locate(nid) == ARG.AND.v.stored(nid).AND.v.dim(nid) == SP) THEN
      IF (v.type(nid) == TSTRUCT) THEN
         WRITE (3,90) '  ',name(1:nc), ' => ', name(1:nc),'_'  ! init from arg
      ELSE
         WRITE (3,90) '  ',name(1:nc), ' = ', name(1:nc),'_'   ! init from arg
      END IF
   END IF

END DO

! - - - - - - - - - - - - - - - - - - -
! PASS 3 process F\C2F.2 -> NAME.F90
! - - - - - - - - - - - - - - - - - - -
upchuck = .FALSE.
num_inserts = 0

32 DO
   READ (2,90,END=302) Cline

   IF (Cline == SP) THEN    ! output blank lines from pass 2
      WRITE (3,90)
      GO TO 32
   END IF

   ! examine cline record for final formatting -> fline
   DO c = RECSIZ,1,-1
      IF (CC(c) /= SP) EXIT
   END DO
   Cend = c

   c1 = 0
   DO c = 1,Cend
      IF (CC(c) /= SP) EXIT
   END DO
   c1 = c

   IF (Cline(c1:c1+2) == 'DO ') do_col = c1

   IF (Cline(c1:c1+6) == 'END DO') THEN
      do_col = 0                     ! reset last do_col
      IF (num_inserts > 0) THEN
         DO n = 1,num_inserts
            Fline(1:80) = End_Do_Inserts(n)
            DO f = 1,50
               IF (FF(f) /= SP) EXIT
            END DO
            WRITE (3,90) Cline(1:c1-1),'  ',Fline(f:80)   ! output indented insert
         END DO
         WRITE (3,90) Cline(1:Cend)      ! output END DO
         num_inserts = 0
         GO TO 32
      END IF
   END IF

   IF (CC(Cend) == ';') CC(Cend) = SP    ! remove ONLY the last ;

   ass = .FALSE.
   stringc = .FALSE.
   quote_field = .FALSE.
   c = c1
   f = c
   IF (f == do_col.AND.Cline(c1:c1+2) /= 'DO ') f = f+INDENT
   f1 = f
   pch = SP
   ch = SP
   FF = SP

  ! - - -  - - - - - - - - - - - - -
  ! - - - PASS 3 Cline -> Fline - - -
  ! - - -  - - - - - - - - - - - - -
  DO
      IF (c > Cend) EXIT
      IF (ch /= SP) pch = ch
      ch = CC(c)                          ! below should NOT change ch
      IF (c > Cend) THEN
         FF(f) = ch
         f = f+1
         c = c+1
         CYCLE                            ! just copy chars past opfield
      END IF

      IF (ch == DQUOTE) THEN
         IF (.NOT.quote_field) THEN
            quote_field = .TRUE.
            stringc = .FALSE.             ! reset '\' detect in "string"
            FF(f) = ch                    ! copy "
            f = f+1
            c = c+1
         ELSE                             ! quote_field
            quote_field = .FALSE.         ! end of field"
            FF(f) = ch                    ! copy "
            f = f+1
            c = c+1
            IF (stringc.AND.CC(c+1) /= 'C') THEN  ! there was "123\456" syntax
               FF(f) = 'C'                ! insert once
               f = f+1
            END IF
         END IF
         CYCLE
      END IF

      IF (quote_field) THEN               ! copy chars
         FF(f) = ch
         f = f+1
         c = c+1
         IF (ch == '\') THEN
            stringc = .TRUE.
            IF (CC(c) == DQUOTE.AND.CC(c-2) /= '\') CC(c) = QUOTE ! FPS bug
         ELSE IF (ch == '%'.AND.CC(c) == '%') THEN  ! syn: "123%%456"
            c = c+1                                 ! skip 2nd %
         END IF
         CYCLE
      END IF
                                          ! below processing NOT "quote" field
      IF (ch == SP) THEN
         FF(f) = ch
         f = f+1
         c = c+1
         CYCLE                            ! just copy blanks
      ELSE IF (ch == ENQ) THEN            ! replace comma tmp with comma
         CC(c) = ','
         ch = ','                         ! pass1 gen. ISHFT(..ENQ..)
      END IF
      ch1 = CC(c+1)                       ! next char

      pcheck = identifier(ICHAR(pch))     ! not pcheck and
      check  = identifier(ICHAR(ch))      ! check is start of identifier string

      IF (Cline(c:c+1) == '=='.OR.Cline(c:c+1) == '!=') THEN
         Fline(f:f+1) = Cline(c:c+1)      ! output == or !=
         IF (ch == '!') FF(f) = '/'       ! output /=
         f = f+2
         c = c+2
         equal = .TRUE.
         IF (ch == '!') equal = .FALSE.    ! NOT logic
         associated = .TRUE.               ! assume associated
         DO n = c-3,1,-1                   ! examine
            IF (CC(n) == '*'.OR.CC(n) == '[') associated = .FALSE.
            IF (CC(n) == '(') EXIT         ! detected ( -> (name==
         END DO

         IF (associated) THEN              ! only associated if name=pointer
            n1 = n+1                       ! assumed start of name
            IF (CC(n1) == SP) n1 = n1+1    ! allow a SP -> ( name==
            name = Cline(n1:c-3)           ! x
            nid = GetIndex(name)
            IF (nid > 0.AND.v.declare(nid) == PTR1) THEN  ! assume y pointer
               k = Get()
               IF (equal.OR.string == 'NULL') THEN
                  Fline(n1:n1+10) = 'ASSOCIATED('
                  f = n1+11
               ELSE
                  Fline(n1:n1+15) = '.NOT.ASSOCIATED('
                  f = n1+16
               END IF
               n = LEN_TRIM(name)
               Fline(f:f+n-1) = name(1:n)   ! x
               f = f+n
               IF (string /= 'NULL') THEN  ! y /= NULL, so add it
                  Fline(f:f+k) = ',' // String(1:k)    ! x,y
                  f = f+k+1
               END IF
               FF(f) = ')'
               f = f+1
            END IF
         END IF

      ELSE IF (ch == '*'.AND..NOT.multiply_allowed(ICHAR(pch))) THEN
         c = c+1                      ! past *
         c1 = c
         IF (CC(c) == '(') THEN
            FF(f) = '('               ! copy
            f = f+1
            c = c+1                   ! past (
         END IF
         CALL BeginString()
         IF (a_z(ICHAR(CC(c)))) THEN    ! *x or *(x
            k = Get()
            nid = GetIndex(string)
            CALL BeginString()
            IF (nid > 0.AND.v.ptr_array(nid).AND.CC(c) /= '[') THEN ! scalar use
               Fline(f:f+k+2) = string(1:k) // '(1)'  ! not scalar, use as array
               f = f+k+3
            ELSE IF (nid > 0.AND.CC(c1) == '('.AND.v.declare(nid)==ARRAY1) THEN
               Fline(f:f+k+3) = string(1:k) // '2(1,'  ! conv.to (xx2(1,nn] )
               f = f+k+4
               c = c+1                ! past [
            ELSE
               c = c1                 ! resume past *
            END IF
         ELSE
            FF(f) = '*'
            f = f+1                   ! resume past *
            c = c1
         END IF
         ch = CC(c)                   ! becomes pch

      ELSE IF (ch == '!') THEN
         Fline(f:f+4) = '.NOT.'
         f = f+5
         c = c+1

      ELSE IF (Cline(c:c+1) == '||') THEN
         Fline(f:f+3) = '.OR.'
         f = f+4
         c = c+2

      ELSE IF (ch == '|') THEN
         Fline(f:f+3) = '.OR.'
         f = f+4
         c = c+1

      ELSE IF (ch == '&') THEN               ! elim remaining &
         IF (ch1 == '-') THEN                ! except continuation &
            Fline(f:f+1) = '& '
            f = f+2
            c = c+2
         ELSE IF (a_z(ICHAR(ch1))) THEN
            c = c+1                          ! discard &
         ELSE
            Fline(f:f+4) = '.AND.'           ! must be and operator
            f = f+5
            c = c+1
            IF (ch1 == '&') c = c+1
         END IF

      ELSE IF (Cline(c:c+3) == '+1:]') THEN  ! check ptr_array usage
         c2 = c
         c = 1
         n = Get()
         nid = GetIndex(string)
         IF (nid > 0.AND..NOT.v.ptr_array(nid)) THEN ! remove :
            Fline(f:f+2) = '+1)'
            f = f+3
            c = c2+4
         ELSE
            Fline(f:f+3) = '+1:)'
            f = f+4
            c = c2+4
         END IF

      ELSE IF (Cline(c:c+2) == ']:]') THEN  ! special pointer arithmetic signal
         Fline(f:f+3) = '+1:)'
         f = f+4
         c = c+3

      ELSE IF (ch == '[') THEN
         c2 = c
         DO n = c2-1,1,-1                   ! find start of identifier
            k = identifier(ICHAR(CC(n)))
            IF (k == 0) EXIT
         END DO
         String = Cline(n+1:c2-1)            ! string[  name
         CALL MoveToNext(']')
         c3 = c

         IF (CC(c+1) /= '[') THEN
            nid = GetIndex(string)
            IF (nid > 0.AND.v.type(nid) == TCHAR.AND.v.declare(nid) == SCALAR) THEN
               string = Cline(c2+1:c3-1)     ! [nnn]
               n = LEN_TRIM(string)
               Fline(f:f+n+n+6) = '(' // string(1:n) // '+1:' // string(1:n) // '+1)'
               f = f+n+n+7
               c = c3+1
            ELSE
               FF(f) = '('
               f = f+1
               c = c2+1
            END IF

         ELSE            ! x[][]...  check if **x pointer
            nid = GetIndex(string)
            IF (nid > 0.AND.v.declare(nid) == PTR2) THEN ! **x array[..][..]
               c = c+1                                   !          2  34  5
               c4 = c
               CALL MoveToNext(']')
               c5 = c
               IF (Use_pointer2) THEN
                  n = (c5-c2)+7
                  Fline(f:f+n-1) = '(' // Cline(c4+1:c5-1) // '+1)%p('  &
                                          // Cline(c2+1:c3-1) // '+1)'
               ELSE   ! use arrays(:,:)
                  n = (c5-c2)+4
                  Fline(f:f+n-1) = '(' // Cline(c2+1:c3-1) // '+1,'  &
                                          // Cline(c4+1:c5-1) // '+1)'
               END IF
               f = f+n
               c = c5+1      ! past [][]
               ch = ']'      ! sets pch = ] next cycle allow ]*yyy

            ELSE IF (nid > 0.AND.v.type(nid) == TCHAR &
                            .AND.v.declare(nid) == ARRAY1) THEN
               Fline(f:f+1) = '2('       ! name2(
               f = f+2
               c = c2+1
            ELSE             ! not **x  x[][]
               FF(f) = '('
               f = f+1
               c = c2+1
            END IF
         END IF

      ELSE IF (Cline(c:c+1) == '][') THEN
         Fline(f:f+2) = '+1,'              ! normal translation
         f = f+3
         c = c+2

      ELSE IF (ch == ']') THEN
         IF (FF(f-1) /= ':'.AND.FF(f-1) /= '*') THEN
            Fline(f:f+2) = '+1)'
            f = f+3
         ELSE
            FF(f) = ')'
            f = f+1
         END IF
         c = c+1

      ELSE IF (ch == HT) THEN               ! discard signaling tabs
         c = c+1                            ! from output

      ELSE IF (ch == CHAR(94)) THEN         ! check for ^
         IF (FF(f-1) == SP) f = f-1         ! try to snug x**y together
         Fline(f:f+1) = '**'
         f = f+2
         c = c+1
         IF (CC(c) == SP) c = c+1           ! try to achieve x**y

      ELSE IF (.NOT.pcheck.AND.Cline(c:Cend) == 'NULL') THEN
         Fline(f-1:f+6) = '> NULL()'
         f = f+7
         c = Cend+1

      ELSE IF (.NOT.pcheck.AND.Cline(c:c+3) == 'POW(') THEN  ! to (..)**(..)
         f1 = f
         c = c+4                            ! advance past (
         CALL CopyToFirst(',')
         Fline(f-1:f) = '**'                ! overstore ,
         f = f+1
         CALL BeginString()
         IF (Cline(c:c+3) == 'POW(') THEN
            c = c+4                         ! advance past (
            CALL CopyToFirst(',')
            Fline(f-1:f) = '**'             ! overstore ,
            f = f+1
         END IF
         CALL CopyToFirst(')')
         k = 0
         DO n = f1,f
            IF (FF(n) == '(') k = k+1
            IF (FF(n) == ')') k = k-1
         END DO
         IF (k == -1) FF(f-1) = SP          ! remove surplus )
         IF (CC(c) == ')') c = c+1          ! dont copy ))

      ELSE IF (c > 1.AND.Cline(c-1:c) == ' ='.AND.CC(c+1) /= '=' &
                                             .AND.CC(c+1) /= '>') THEN
         IF (CC(c+1) /= SP) THEN       ! strictly cosmetic blank fixups
            Fline(f:f+1) = '= '        ! insert blank before  x =y
            f = f+2
            c = c+1
         ELSE
            FF(f) = ch                 ! x =
            f = f+1
            c = c+1
            IF (Cline(c:c+1) == '  ') c = c+1   ! delete 1 space  x =  y
            IF (Cline(c:c+1) == '  ') c = c+1   ! delete 1 space  x =   y
         END IF

      ELSE IF (.NOT.pcheck.AND.Cline(c:c+1) == '0x') THEN
         FF(f) = '#'
         f = f+1
         DO c = c+2,Cend
            ch = CC(c)
            IF (.NOT.identifier(ICHAR(ch))) EXIT
            FF(f) = ch
            f = f+1
         END DO

      ELSE IF (Cline(c:c+3) == 'INT(') THEN
         c1 = c
         c = c+4     ! past INT(
         k = Get()
         c = c1
         nid = GetIndex(string)
         IF (nid > 0) THEN
            IF (v.type(nid) == TCHAR) THEN
               Fline(f:f+5) = 'ICHAR('
               f = f+6
               c = c+4
           ELSE IF (v.type(nid) == TINT.OR. &
                    v.type(nid) == TSHORT) THEN  ! remove INT( x )  ->  x
              Fline(f:f+k-1) = string(1:k)
              f = f+k
              CALL MoveToNext(')')
              c = c+1
            END IF
            CYCLE
         END IF
         Fline(f:f+3) = 'INT('
         f = f+4
         c = c+4

      ELSE IF (ch == QUOTE.AND.CC(c+1) == '\') THEN   ! '\
         IF (CC(c+2) == '\') THEN                     ! '\\  = \
            Fline(f:f+2) = "'\'"
            f = f+3
            c = c+3
         ELSE IF (CC(c+2) == 'f'.OR.CC(c+2) == 'n') THEN
            Fline(f:f+7) = 'CHAR(10)'             ! = LF
            f = f+8
            c = c+3
         ELSE IF (CC(c+2) == 'r') THEN
            Fline(f:f+7) = 'CHAR(13)'            ! = CR
            f = f+8
            c = c+3
         ELSE IF (CC(c+2) == 't') THEN
            Fline(f:f+6) = 'CHAR(9)'             ! = TAB
            f = f+7
            c = c+3
         ELSE
            equal = .TRUE.
            DO n = 2,5
               IF (CC(c+n) == QUOTE) EXIT
               IF (CC(c+n) < '0'.OR.CC(c+n) > '7') equal = .FALSE.
            END DO
            IF (n > 5.OR..NOT.equal) THEN
               CALL Error(12)      ! not octal constant
               EXIT
            END IF
            READ (Cline(c+2:c+n-1),'(O)') k
            WRITE (Fline(f:f+8),'(A,I3.3,A)') 'CHAR(',k,')' ! CHAR(255) = 377
            f = f+9
            c = c+2+n
         END IF
         IF (CC(c) == QUOTE) c = c+1      ! past possible end '

      ELSE IF (Cline(c:c+9) == 'CALL FREE(') THEN
         Fline(f:f+10) = 'DEALLOCATE '
         f = f+11
         c = c+9

      ELSE IF (Cline(c:c+6) == 'SIZEOF(') THEN
         c = c+7                          ! past sizeof(
         Fline(f:f+6) = 'SIZEOF('
         f = f+7
         CALL BeginString()
         IF (Cline(c:c+2) == 'int') THEN
            Fline(f:f+1) = '1)'
            f = f+2
            CALL MoveToNext(')')
         ELSE IF (Cline(c:c+4) == 'float') THEN
            Fline(f:f+3) = '1.0)'
            f = f+4
            CALL MoveToNext(')')
         ELSE IF (Cline(c:c+5) == 'double') THEN
            Fline(f:f+4) = '1.d0)'
            f = f+5
            CALL MoveToNext(')')
         ELSE
            CALL CopyToFirst(')')       ! SIZEOF(x)
         END IF
         c = c+1

      ELSE                   ! just copy ch -> Fline
         FF(f) = ch                        ! 1st =
         f = f+1
         c = c+1
      END IF

   END DO                    ! until Cline processed

   DO f = RECSIZ,1,-1
      IF (FF(f) /= SP) EXIT              ! end of just translated record
   END DO

   IF (FF(f) == DOWN) THEN  ! record was extracted originally from
      num_inserts = num_inserts+1     ! for(..e5,e6,, or for(; ..; e3)
      IF (num_inserts > 10) num_inserts = 10  ! shud never be exceeded
      End_Do_Inserts(num_inserts) = Fline(1:f-1)  ! save minus down-arrow
      GO TO 32                        ! dont output until assoc. END DO
   END IF

   IF (f > 0) WRITE (3,90) Fline(1:f)
   upchuck = .TRUE.                      ! next !c rec. upchucks first
END DO
302 CONTINUE     ! END PASS 3 translation for this module

IF (Nfunc > 0) THEN  ! Gen. VOID_TO_XXX explicit ptr conversion functions
   BACKSPACE (3)           ! over END statement
   WRITE (3,*)
   WRITE (3,90) 'CONTAINS'
   DO n = 1,Nfunc
      WRITE (3,96) TRIM(List_Suffixes(n)),TRIM(List_Types(n)), &
               TRIM(List_Suffixes(n)),TRIM(List_Types(n)),TRIM(List_Suffixes(n))
   END DO
   WRITE (3,90) Fline(1:f)  ! re-write END statement
END IF

! PASS 3 output declaration summary - - -
IF (Debug) THEN
   WRITE (3,*)
   WRITE (3,90) '! declarations           type    declare flag locate dimension/init. string'
   WRITE (3,90) '! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'
   DO n = 1,MID
      nc = v.nc(n)
      IF (nc == 0) EXIT         ! end of table
      IF (Nmod == 1) THEN
             ! list all global module entries
      ELSE   ! Nmod > 1
         IF (nc < 0.AND.(v.locate(n) == GLOBAL.OR.v.locate(n) == NONE.OR. &
                         v.locate(n) == EXTERN)) CYCLE  ! skip unref. global/extern
      END IF
      name = v.name(n)
      k = 0
      c = 0
      DO i = 1,MAXSTR
         IF (name(i:i) == '%') THEN
            k = k+1
            c = i
         END IF
      END DO
      IF (k == 1) THEN
         name = ' ' // name(c:)
      ELSE IF (k == 2) THEN
         name = '  ' // name(c:)
      ELSE IF (k == 3) THEN
         name = '   ' // name(c:)
      ELSE IF (k == 4) THEN
         name = '    ' // name(c:)
      END IF

      string = v.name2(n)
      IF (string == SP) string = Types(v.type(n))
      flag_s = SP
      IF (v.ptr_deref(n)) flag_s(1:1) = 'D'
      IF (v.ptr_array(n)) flag_s(2:2) = 'A'
      IF (v.target(n))    flag_s(3:3) = 'T'
      IF (v.ptr_void(n))  flag_s(4:4) = 'V'
      dim_s = v.dim(n)
      init_s = v.init(n)
      DO i = 1,MAXSTR
         IF (dim_s(i:i) == ZERO) dim_s(i:i) = SP
         IF (init_s(i:i) == ZERO) init_s(i:i) = SP
      END DO
      WRITE (3,99) '! ', name(1:22),' ',string(1:10) , &
         declare_s(v.declare(n)),' ',flag_s, locate_s(v.locate(n)), &
         TRIM(dim_s),' ',TRIM(init_s)
      99 FORMAT (8A,T54,5A)
   END DO
   WRITE (3,90)
END IF

GO TO 2            ! continue with next module from F\C2F.1

! - - - - - - - - - - - - - - - - - - - - -
4 CONTINUE        ! translation complete (almost)
! - - - - - - - - - - - - - - - - - - - - -

IF (Debug) THEN                    ! output global typedef info
   DO n = 1,MAXTYPEDEFS
      IF (typedefs(1,n) == SP) EXIT
      name = typedefs(1,n)
      n1 = LEN_TRIM(name)
      string = typedefs(2,n)
      n2 = LEN_TRIM(string)
      WRITE (3,'(A,I2,5A)') '! define',n,' ',string(1:n2),' = ',name(1:n1)
   END DO
   WRITE (9,*) Errcount, ' Errors detected'

END IF

WRITE (3,90) "INCLUDE 'C2F_LIB.F90'  "
WRITE (3,'(A,I2,A)') '! ',Errcount,' Errors detected'
WRITE (*,'(I2,A)')        Errcount,' Errors detected'
CLOSE (3)                ! name.f90

IF (.NOT.debug) THEN     ! delete temp files, new or from previous runs
   CLOSE (1,STATUS='DELETE')   ! F\C2F.1
   CLOSE (2,STATUS='DELETE')   ! F\C2F.2
   OPEN (1,FILE='F\' // ProgramName(1:Nsize) // '.C')
   CLOSE (1,STATUS='DELETE')   ! F\NAME.C
   IF (run_pp) THEN
      OPEN (1,FILE='F\' // ProgramName(1:Nsize) // '.CPP')
      CLOSE (1,STATUS='DELETE')   ! F\NAME.CPP
   END IF
   OPEN (1,FILE='F\' // ProgramName(1:Nsize) // '.ERR')
   CLOSE (1,STATUS='DELETE')
END IF

CLOSE (21,STATUS='DELETE')

IF (Global_Target) THEN   ! FIXUP Global variables with target attribute
   OPEN (3,FILE='F\' // ProgramName(1:Nsize) // '.F90')
   OPEN (4,FILE='F\C2F.4')
   DO                            ! copy name.f90 to temp file
      READ (3,90,END=401) Fline
      f = LEN_TRIM(Fline)
      WRITE (4,90) Fline(1:f)
   END DO
 401 CONTINUE
   CLOSE (3)
   CLOSE (4)
   OPEN (3,FILE='F\C2F.4')
   OPEN (4,FILE='F\' // ProgramName(1:Nsize) // '.F90')
   DO
      READ (3,90) Cline
      Cend = LEN_TRIM(Cline)
      WRITE (4,90) Cline(1:Cend)             ! copy to final name.f90
      IF (Cline(1:10) == 'END MODULE') EXIT

      IF (Cline(1:13) == '  INTEGER :: '.OR. &
          Cline(1:13) == '  REAL(4) :: '.OR. &
          Cline(1:13) == '  REAL(8) :: ')      THEN
         c = 14
         n = Get()
         DO nid = 1,MID
            IF (g.name(nid) == SP.OR.g.name(nid) == string) EXIT
         END DO
         IF (g.name(nid) == string.AND.g.target(nid)) THEN
            BACKSPACE (4)           ! rewrite this record
            WRITE (4,90) Cline(1:9), ',TARGET', Cline(10:Cend)
         END IF
      END IF
   END DO
   DO                            ! copy remaining file
      READ (3,90,END=402) Cline
      Cend = LEN_TRIM(Cline)
      WRITE (4,90) Cline(1:Cend)
   END DO
 402 CLOSE (3,STATUS='DELETE')
END IF

STOP              ! end translation

90 FORMAT (500A)
91 FORMAT (A,3I2)
92 FORMAT (I3,2A,T20,2A,T35,A)
95 FORMAT (A,F6.2,A)

96 FORMAT('! -----------------------------------'/ &
          'FUNCTION VOID_TO_',A,'(addr)'         / &
          '! -----------------------------------'/ &
          '  ',A,',POINTER :: VOID_TO_',A,'(:)'  / &
          '  ' A,',TARGET :: T_addr(*)'          / &
          '  INTEGER :: addr'                    / &
          '  POINTER (addr,T_addr)'             // &
          '  VOID_TO_',A,' => T_addr(:C2F_BIGNUM)' / &
          'END FUNCTION' /)

CONTAINS
! -----------------------------------------------------------
! UTILITY SUPPORT ROUTINES
! -----------------------------------------------------------
FUNCTION UPPER (n)     RESULT (s)   ! input= String
  IMPLICIT NONE
  INTEGER :: n,i
  CHARACTER (LEN=MAXSTR) :: s
  CHARACTER :: ch

  DO i = 1,MAXSTR
     IF (i > n) THEN
        s(i:i) = SP        ! blank output s(n+1:)
     ELSE
        ch = string(i:i)
        IF ( ch >= 'a' .AND. ch <= 'z') THEN
           s(i:i) = CHAR(ICHAR(ch) -32)
        ELSE
           s(i:i) = ch
        END IF
     END IF
  END DO
END FUNCTION UPPER

! -----------------------------------------------------------
FUNCTION TOUPPER (ch) RESULT(outchar)
  IMPLICIT NONE
  CHARACTER (LEN=1), INTENT(IN) :: ch
  CHARACTER (LEN=1)             :: outchar

  IF ( ch >= 'a' .AND. ch <= 'z') THEN
    outchar = CHAR(ICHAR(ch) -32)
  ELSE
    outchar = ch
  END IF
END FUNCTION TOUPPER

! -----------------------------------------------------------
SUBROUTINE BeginString()

DO c = c,Cend
   IF (CC(c) /= SP) EXIT     !note c not advanced if sitting at string
END DO

END SUBROUTINE BeginString
 
! -----------------------------------------------------------
SUBROUTINE MoveToNext(ch)
 
CHARACTER :: ch
DO c = c,Cend
   IF (CC(c) == ch) EXIT
END DO

END SUBROUTINE MoveToNext

! -----------------------------------------------------------
SUBROUTINE MoveToCloseParens()

openp = 0
DO c = c,Cend
   IF (CC(c) == '(') openp = openp+1
   IF (CC(c) == ')') openp = openp-1
   IF (openp == 0) EXIT
END DO

END SUBROUTINE MoveToCloseParens

! -----------------------------------------------------------
SUBROUTINE MoveToStringPast(ch)

CHARACTER :: ch
DO c = c,Cend
   IF (CC(c) == ch) EXIT
END DO
c = c+1
CALL BeginString()

END SUBROUTINE MoveToStringPast

! -----------------------------------------------------------
SUBROUTINE CopyToFirst(ch1)
 
CHARACTER :: ch1
 
DO n = c,RECSIZ
   IF (CC(n) == ch1) EXIT        ! check ch1 exists before copying
END DO
IF (n > RECSIZ) RETURN           ! does not exist
 
DO c = c,n
   FF(f) = CC(c)
   f = f+1
END DO                           ! note c is past ch1

END SUBROUTINE CopyToFirst
 
! -----------------------------------------------------------
SUBROUTINE CopyToLastParen()    !copy string ((...(..)..))

INTEGER :: nop
CHARACTER :: ch

nop = 0

DO c = c,Cend              ! assumes c at (
   ch = CC(c)
   FF(f) = ch
   f = f+1
   IF (ch == '(') nop = nop+1
   IF (ch == ')') nop = nop-1
   IF (nop <= 0) EXIT
END DO

END SUBROUTINE CopyToLastParen
! -----------------------------------------------------------
SUBROUTINE CopyToCend()
 
DO c = c,Cend
   FF(f) = CC(c)
   f = f+1
END DO
END SUBROUTINE CopyToCend
 
! -----------------------------------------------------------
FUNCTION GetIndex(name)  RESULT (nid)

CHARACTER (LEN=*) :: name  ! search v.name for name (lower & upper case)
CHARACTER (LEN=MAXSTR) :: name1, name2
INTEGER :: nid, n1, n      ! use private variables
                           ! returns > 0 for index found, 0 not found

name1 = name
n1 = LEN_TRIM(name1)

DO nid = 1,MID
   nc = Abs(v.nc(nid))     ! nc made public
   IF (nc == 0) EXIT       ! end of entries, therefore not found
   IF (nc /= n1) CYCLE     ! no match with this entry
   name2 = v.name(nid)
   DO n = 1,n1
      IF (TOUPPER(name1(n:n)) /= TOUPPER(name2(n:n))) EXIT  ! not a match
   END DO
   IF (n > n1) THEN        ! do completion implies a match
      v.nc(nid) = nc       ! mark it referenced with a positive char count
      RETURN               ! with nid = entry index found
   END IF
END DO
nid = 0

END FUNCTION GetIndex

! -----------------------------------------------------------
FUNCTION GetNextIndex()  RESULT (nid)

INTEGER :: nid

DO nid = 1,MID
   IF (v.nc(nid) == 0) RETURN  ! found
END DO
IF (nid > MID) THEN            ! table full
   CALL Error(6)
   STOP                        ! cannot proceed
END IF

END FUNCTION GetNextIndex

! -----------------------------------------------------------
INTEGER FUNCTION Get()    ! String identifier from Cline, advancing c
                          ! legal chars are as per identifier array
INTEGER :: n, k
CHARACTER :: ch

CALL BeginString()
String = SP
n = 0
DO c = c,c+MAXSTR-1
   ch = CC(c)
   k = identifier(ICHAR(ch))
   IF (k == 0) EXIT
   n = n+1
   String(n:n) = ch
END DO

Get = n                !#chars stored in string
END FUNCTION Get

! -----------------------------------------------------------
FUNCTION GetKey()  RESULT (key)

INTEGER :: key   ! note: not same variable as program's Key variable
INTEGER :: c1

Program_Sub_Fun = .FALSE. ! set true if key main,sub,fun
SubName = SP
FunName = SP
key = 0                ! below tests for keywords with non-alpha 1st letter

CALL BeginString()

IF (CC(c) == '{') key = C_leftbrace
IF (CC(c) == '}') key = C_rightbrace
IF (CC(c)==DQUOTE.OR.CC(c)==QUOTE.OR.CC(c) >= '0'.AND.CC(c) <= '9') key = C_data
IF (Cline(c:c+1) == '++'.OR.Cline(c:c+1) == '--'.OR.CC(c) == '(') key = 99
IF (Cline(c:c+7) == '#define ')  key = C_define
IF (Cline(c:c+8) == '#include ') key = C_include
IF (Cline(c:c+2) == '#if')       key = C_$if
IF (key > 0) RETURN

n = Get()                    ! get alpha keystring from cline(c:
DO key = C_auto,MAXKEY       ! these keys are linked with follow-on keys
   IF (String == Keywords(key)) EXIT
END DO

IF (key <= C_void) RETURN    ! found key  auto,register,unsigned,void
IF (key == C_main) THEN
   key = C_program           ! C_main not used as program module key
   RETURN
END IF
IF (Cline(c:Cend) == SP) RETURN    ! assume data declaration next record

CALL BeginString()
c1 = c

IF (key <= C_char.OR.key == C_struct) THEN
   IF (key == C_struct) n = Get()     ! past struct name
   CALL BeginString()
   IF (CC(c) == '*') c=c+1   ! past *
   n = Get()
   IF (CC(c) == '('.OR.(c > Cend.AND.key /= C_struct)) THEN
      IF (string == 'main') THEN   ! int main()
         key = C_program
      ELSE
         key = C_function
         FunName = string          ! "name" of fun
      END IF
      Program_Sub_Fun = .TRUE.
   ELSE                            ! int x;
      c = c1                       !     ^
   END IF
   RETURN                    ! with short:char,struct key or C_function key
END IF

IF (key <= MAXKEY) RETURN    ! detected keyword,  (printf, etc.)

IF (CC(c) == '(') THEN       ! sub(
   key = C_subroutine
   SubName = string
   Program_Sub_Fun = .TRUE.
ELSE
   key = 99
   IF (CC(c) == '=') THEN    ! x=
      c = c+1
      CALL BeginString()
      IF (CC(c) == '{') key = C_data    ! x={ data }
   END IF
   c = c1
END IF

END FUNCTION GetKey

! -----------------------------------------------------------
FUNCTION GetName(arg)  RESULT (name)

CHARACTER (LEN=MAXSTR) :: arg, name
INTEGER :: n

DO n = 1,MAXSTR
   IF (.NOT.identifier(ICHAR(arg(n:n)))) EXIT
END DO
name = arg(1:n-1)
END FUNCTION GetName

! -----------------------------------------------------------
SUBROUTINE Error(errnum)

! - - -  Local Declarations - - -
INTEGER, PARAMETER :: WMESS=30, MESS = 34
INTEGER :: errnum, n, n1
CHARACTER (LEN=33) :: errmess, info
CHARACTER (LEN=33) :: errors(0:MESS) = (/    &
   'unknown errorcode <1 or >23      ',  &  ! 0
   'if/else syntax                   ',  &  ! 1
   'C2F has lost track of {} closures',  &  ! 2
   'syntax use of break;             ',  &  ! 3
   'assign non-char to char          ',  &  ! 4
   'undeclared identifier?           ',  &  ! 5
   'identifiers > MID, STOP          ',  &  ! 6
   'modules > MAXMOD, STOP           ',  &  ! 7
   'pointer syntax unrecog.          ',  &  ! 8
   'multiple declaration             ',  &  ! 9
   'unrec.syntax                     ',  &  ! 10
   'unsupported syntax               ',  &  ! 11
   'not octal constant               ',  &  ! 12
   'unknown arg type                 ',  &  ! 13
   'print format specifier           ',  &  ! 14
   'pass 2 modulename /= pass1 name  ',  &  ! 15
   'Malloc for x is unrecog.         ',  &  ! 16
   'Arrays, Functions, typedefs > MAX',  &  ! 17
   'unrec. data init. syntax         ',  &  ! 18
   'Key_Save > MAXKEYSAVE            ',  &  ! 19
   'Labels > MAXLAB                  ',  &  ! 20
   'struct members > MAXMEMBERS      ',  &  ! 21
   'array dimension > 3              ',  &  ! 22
   'arglist records > RECSIZ         ',  &  ! 23
   'CONTAIN functions > 20           ',  &  ! 24
   'print arg extraction > 11        ',  &  ! 25
   '                                 ',  &  ! 26
   '                                 ',  &  ! 27
   '                                 ',  &  ! 28
   '                                 ',  &  ! 29
   'unknown external?                ',  &  ! 30
   'forced to set ptr in pass3       ',  &  ! 31
   'declared type                    ',  &  ! 32
   'syntax not used by debug.c       ',  &  ! 33
   'blank record output by pass 1    '/)    ! 34
! - - - - - - - - - - - - - - - - - - - - - - - -

IF (errnum < 1.OR.errnum > MESS) errnum = 0  ! shud not happen
IF (errnum < WMESS) Errcount = Errcount +1   ! dont count warnings

IF (c > 70) THEN
  DO n = 1,RECSIZ
     IF (ICHAR(CC(n)) == 0) CC(n) = SP
  END DO
  c = LEN_TRIM(Cline)
END IF
errmess = errors(errnum)

IF (Npass == 1) THEN
   info = ' <= Pass1'
ELSE
   IF (c < 30) THEN
      info = Cline(1:c)     ! display line prior to error
   ELSE
      info = Cline(c/2:c)   ! display last half prior to error
   END IF
END IF

IF (Debug) THEN
   IF (errnum < WMESS) THEN
      WRITE (9,91) 'ERROR ',TRIM(errmess),'=> ',TRIM(info)
      WRITE (*,91) 'ERROR ',TRIM(errmess),'=> ',TRIM(info)
   ELSE                    ! dont display warnings to console
      WRITE (9,91) 'WARN: ',TRIM(errmess),'=> ',TRIM(info)
   END IF
END IF

90 FORMAT (9A)
91 FORMAT (7X,9A)
END SUBROUTINE Error

! -----------------------------------------------------------
! PASS 1 routines
! -----------------------------------------------------------
SUBROUTINE define_preproc()

c = c+8                ! past #define
n = Get()
argname = String
CALL BeginString()
c1 = c
n = Get()
DO Key = C_short,C_char
   IF (String == Keywords(Key)) EXIT
END DO

IF ((Key >= C_short.AND.Key<= C_char).OR.CC(c1) == DQUOTE) THEN  ! convert to typedef
   DO n = 1,MAXTYPEDEFS
      String = Typedefs(2,n)
      IF (String == SP.OR.String == argname) EXIT ! enter or replace
   END DO
   IF (n < MAXTYPEDEFS) THEN
      IF (CC(c1) == DQUOTE) THEN
         Typedefs(1,n) = Cline(c1:cend)   ! probably a format "string"
      ELSE
         Typedefs(1,n) = Keywords(Key)   ! char,,,double
      END IF
      Typedefs(2,n) = argname
   ELSE
      CALL Error(17)       ! typedefs > MAX
   END IF
   Cline(1:1) = SP
ELSE
   ! nothing done in pass 1 for normal ex: #define PI 3.14
END IF

90 FORMAT (99A)
END SUBROUTINE define_preproc

! -----------------------------------------------------------
SUBROUTINE enum_preproc()

DO c = 5,Cend
   IF (CC(c) == '}') EXIT
END DO
IF (CC(c) /= '}') RETURN

c1 = c
c = c+1                    ! past }
CALL BeginString()

IF (Key == C_typedef_en.AND.CC(c) /= ';') THEN !syn: typedef enum name {,,} name;
   string = Cline(c:Cend-1)            ! phase 0 removed typedef from above
   k = LEN_TRIM(string)                ! but set flag typedef
   argname = UPPER(k)
   DO n = 1,MAXTYPEDEFS
      String = Typedefs(2,n)
      IF (String == SP.OR.String == argname) EXIT ! enter or replace
   END DO
   IF (n < MAXTYPEDEFS) THEN
      Typedefs(1,n) = 'int'
      Typedefs(2,n) = argname
   ELSE
      CALL Error(17)       ! typedefs > MAX
   END IF
   c = 11                         ! past typedef_en
   CALL BeginString()
   WRITE (1,90) 'enum ',Cline(c:c1), ';'  ! becomes enum  for pass 2 processing
   Cline(1:1) = SP

ELSE IF (Key == C_enum.AND.CC(c) /= ';') THEN  ! syn:  enum {.....} arg;
   WRITE (1,90) Cline(1:c1), ';'
   c = c1+1
   CALL BeginString()
   WRITE (1,90) Cline(c:Cend)
   Cline(1:1) = SP
END IF

90 FORMAT (99A)
END SUBROUTINE enum_preproc

! -----------------------------------------------------------
SUBROUTINE declare_preproc()

LOGICAL :: brace
CHARACTER (LEN=80),SAVE :: prevtype

IF (Key < 0) THEN            ! re-entered from continuation
   n = LEN_TRIM(prevtype) +1
   Cline = prevtype(1:n) // Cline
   Cend = Cend + n
   c = n
   Key = ABS(Key)            ! assume last continuation record
END IF

IF (Key == C_const) THEN
   Key = GetKey()                    ! ignore const in pass1
   IF (Key < C_short.OR.Key > C_char) THEN
      CALL Error(11)         ! unsupported syntax
      RETURN
   END IF
END IF

CALL BeginString()
c1 = c-1                  ! prior to var1
c2 = c                    ! point at var1

prevtype = Cline(1:c1)    ! save or resave data type for possible re-enter

IF (c >= Cend) THEN       ! syn:  char    (blank)
   Key = -Key
   CC(1) = SP
   RETURN
END IF

DO               ! syn: int a,b,c(),d=1,e={..}
   c3 = 0
   c4 = 0
   c5 = 0
   c6 = 0
   DO c = c2,Cend
      IF (CC(c) == '=') c3 = c
      IF (CC(c) == '{') c4 = c
      IF (CC(c) == '}') c5 = c
      IF (CC(c) == '&') c6 = c
      IF (c4 == 0) THEN
         IF (CC(c) == ',') EXIT
      ELSE
         IF (CC(c) == ','.AND.c5 > 0) EXIT
      END IF
   END DO

   IF (c > Cend) THEN
      IF (CC(c2) == '*'.AND.c3 /= 0.AND.c6 /= 0) THEN    ! char *p = &c
         WRITE (1,90) Cline(1:c3-1),';'             ! char *p;
         WRITE (1,90) Cline(c2+1:Cend)              ! p = &c;

      ELSE IF (c4 == 0) THEN
         WRITE (1,90) Cline(1:c1),Cline(c2:Cend)    ! int k; or int k=123;
      ELSE    ! {...  exists
         WRITE (1,90) Cline(1:c1),Cline(c2:c3)      ! int k=
         WRITE (1,90) Cline(c4:Cend)                ! {123...
      END IF
      EXIT

   ELSE   ! process ,
      IF (c4 == 0) THEN
         WRITE (1,90) Cline(1:c1),Cline(c2:c-1),';' ! int k=123,
      ELSE    ! {...  exists
         WRITE (1,90) Cline(1:c1),Cline(c2:c3)      ! int k=
         IF (c5 == 0) THEN
            WRITE (1,90) Cline(c4:c)                ! {123,
         ELSE
            WRITE (1,90) Cline(c4:c5),';'           ! {123};
         END IF
      END IF
      c = c+1              ! past ,
      IF (c > Cend) THEN   ! declaration continues next record
         Key = -Key
         EXIT
      END IF
      CALL BeginString()
      c2 = c               ! point at next var
   END IF

END DO
CC(1) = SP

90 FORMAT (99A)
END SUBROUTINE declare_preproc

! -----------------------------------------------------------
SUBROUTINE format_preproc()

LOGICAL :: brace_out
CHARACTER (LEN=80) :: print_attach(11)

IF (CC(Cend) == ';') GO TO 1    ! examine for extractions

! else  syn:  printf(     with continuation records
c1 = 0
c2 = 0
c3 = 0
DO c = 1,Cend
   ch = CC(c)
   IF (ch == DQUOTE.AND.c1 > 0 .AND.c2 == 0) c2 = c   ! 2nd " exists
   IF (ch == DQUOTE.AND.c1 == 0) c1 = c               ! 1st " exists
   IF (ch == '%') c3 = c
END DO

IF (c1 == 0) THEN
    arg1 = Cline(1:Cend) // DQUOTE   ! printt("
    n = Cend+1
ELSE
    arg1 = Cline(1:c1)               ! printf("
    n = c1
END IF

IF (c3 == 0) THEN        ! no % in 1st record, assume no % in continuations

   IF (ch == '\') Cend = Cend-1               ! remove \ from output
   IF (c2 /= 0) THEN                          ! "...." exists, output line
      WRITE (1,90) Cline(1:Cend),');'
   ELSE IF (c1 /= 0.AND.Cend > c1) THEN       ! ".... exists, output line
      WRITE (1,90) Cline(1:Cend),DQUOTE,');'
   END IF

   DO
      READ (11,90) Cline
      Cend = LEN_TRIM(Cline)
      DO c = 1,Cend
         IF (CC(c) /= SP) EXIT
      END DO
      IF (CC(c) == DQUOTE) c = c+1           ! arg1 already has printf("
      IF (CC(Cend) == ';') THEN
         IF (Cline(1:3) /= '");') WRITE (1,90) arg1(1:n), Cline(c:Cend)
         EXIT
      ELSE
         IF (CC(Cend) == DQUOTE) THEN
            WRITE (1,90) arg1(1:n), Cline(c:Cend),');'
         ELSE
            WRITE (1,90) arg1(1:n), Cline(c:Cend-1),DQUOTE,');'
         END IF
      END IF
   END DO
   CC(1) = SP

ELSE         ! assume continue records can be squeezed into RECSIZ

   IF (CC(Cend) == '\') Cend = Cend-1
   Bigline = Cline(1:Cend)           ! begins col.1, resets rest of line
   f1 = Cend+1

   DO
      READ (11,90) Cline
      Cend = LEN_TRIM(Cline)
      DO c = 1,Cend
         IF (CC(c) /= SP) EXIT
      END DO
      FF = SP
      f = 1
      c = c-1
      quote_field = .FALSE.
      DO
         c = c+1
         IF (c > Cend) EXIT
         ch = CC(c)
         IF (ch == DQUOTE) quote_field = .NOT.quote_field
         IF (quote_field) THEN
            FF(f) = ch
            f = f+1
            CYCLE
         END IF

         ch1 = CC(c+1)
         IF (ch == '.'.AND.a_z(ICHAR(ch1))) THEN
            FF(f) = '%'
            f = f+1
         ELSE IF (ch == '-'.AND.ch1 == '>') THEN
            FF(f) = '%'
            f = f+1
            c = c+1
         ELSE
            FF(f) = ch
            f = f+1
         END IF
      END DO
      f = f-1
      Bigline(f1:f1+f-1) = Fline(1:f)
      f1 = f1+f
      IF (f1 > RECSIZ) THEN
         CALL Error(23)  ! lost info?
         RETURN
      END IF
      IF (CC(Cend) == ';') EXIT
   END DO
   Cline = Bigline(1:f1)
   Cend = f1
END IF

1 CONTINUE
c = 1
CALL MoveToNext(DQUOTE)
c = c+1
CALL MoveToNext(DQUOTE)
c = c+1
CALL BeginString()
c1 = c
n1 = 0              ! #print attachs  from k++,  args
brace_out = .FALSE.

DO            ! extract ++i,--i, and C2Fpointer(...)
   c = c+1
   IF (c >= Cend) EXIT
   IF (CC(c) == ',') c1 = c       ! track last comma col.

   IF (Cline(c:c+1) == '++'.OR.Cline(c:c+1) == '--') THEN
      c2 = c
      IF (identifier(ICHAR(CC(c+2)))) THEN
         c = c+2
         n = Get()
         c = c-1                        ! back to possible ,
         IF (.NOT.brace_out) THEN
            WRITE (1,90) '{'
            brace_out = .TRUE.
         END IF
         WRITE (1,90) string(1:n),' = ',string(1:n), CC(c2), '1;'
      ELSE
         c = c1+1
         CALL BeginString()
         string = Cline(c:c2-1)
         n = LEN_TRIM(string)
         n1 = n1 +1
         IF (n1 > 11) THEN
            CALL Error(25)    ! print arg extract > 11
            n1 = 11
         END IF
         print_attach(n1) = string(1:n)//' = '//string(1:n)// CC(c2)// '1;'
      END IF
      Cline(c2:c2+1) = SP                ! remove ++ --

! debug t80   printf("%d %d\n",*(int *)j,*(int *)k);
! debug t81   printf("%s\n", ((struct S *)sp1)->name);
! jos case    printf("%s\n", (char*)(*temp).entry);
                           ! (char*)*temp.entry);
   ELSE IF (Cline(c:c+1) == '*(') THEN
      Cline(c:c+1) = SP                     ! remove *(
      c = c+2
      c3 = c-1
   ELSE IF (CC(c) == '(') THEN
      c3 = c
   ELSE IF (Cline(c:c+1) == '*)') THEN      !   (int *)   or   (struct S *)
      Cline(c:c+1) = SP                     ! remove *)
      CALL BeginString()
      IF (CC(c) == '*') THEN                !  (char*)*temp%entry);
         CC(c) = SP                         !         ^
         c4 = c+1
         n = Get()
         WRITE (1,90) 'C2Fpointer(' // Cline(c3+1:c-1),');'  ! leave % in
         DO i = 1,n
            IF (string(i:i) == '%') string(i:i) = '_'   ! change % to _
         END DO
         WRITE (1,90) 'int ',string(1:n),';'
         WRITE (1,90) string(1:n),' = ',Cline(c4:c-1),'(1);'
         IF (Cline(c3+1:c3+4) == 'char') THEN
            Cline(c3-1:) = 'T_' // TRIM(string)//'(1:'// Cline(c4:c-1) // '(2)) );'
         ELSE
            Cline(c3-1:) = 'T_' // TRIM(string) // Cline(c:Cend)  ! T_arg
         END IF
      ELSE
         n = Get()
         WRITE (1,90) 'C2Fpointer(' // Cline(c3+1:c-1),');'
         IF (Cline(c:c+1) == ')%') c = c+1    ! past sp1)
         Cline(c3-1:) = 'T_' // TRIM(string) // Cline(c:Cend)  ! T_arg, ...
      END IF
      IF (CC(c) == ')') EXIT
      n = Cend
      Cend = LEN_TRIM(Cline)
      c = c-(n-Cend)
   END IF
END DO

IF (CC(1) /= SP) WRITE (1,90) Cline(1:Cend)       ! output printf record
DO n = 1,n1
   WRITE (1,90) TRIM(print_attach(n))
END DO
IF (brace_out) WRITE (1,90) '}'

CC(1) = SP

90 FORMAT (99A)
END SUBROUTINE format_preproc

! -----------------------------------------------------------
FUNCTION last_col()      RESULT (output_4)
INTEGER :: output_4

IF (CC(c) == '(') THEN           ! (e
   n = 0
   DO c = c,Cend
      IF (CC(c) == '(') n = n+1
      IF (CC(c) == ')') n = n-1
      IF (n == 0) THEN
         output_4 = c
         EXIT
      END IF
   END DO
ELSE                             ! may contain e..(...)
   DO c = c+1,Cend
      IF (CC(c) == '('.OR.CC(c) == ')') EXIT
   END DO
   IF (CC(c) == '(') THEN
      n = 0
      DO c = c,Cend
         IF (CC(c) == '(') n = n+1
         IF (CC(c) == ')') n = n-1
         IF (n == 0) EXIT
      END DO
      output_4 = c
   ELSE
      output_4 = c-1
   END IF
END IF

END FUNCTION last_col

! -----------------------------------------------------------
SUBROUTINE statement_preproc()

INTEGER :: ncase, nat
INTEGER :: s0, s1, s2, s3, s4, s5
INTEGER :: e0, e1, e2, e3, e4, e5
CHARACTER (LEN=80) :: attach(5)

nat = 0

1 CONTINUE          ! reprocess
Fline = SP
quote_field = .FALSE.
c2 = 0
c3 = 0
ncase = 0

DO c = 1,Cend
   pch = ch
   ch = CC(c)
   ch1 = CC(c+1)
   IF (ch == DQUOTE) quote_field = .NOT.quote_field
   IF (quote_field) CYCLE
   IF (ch == '?'.AND.pch /= QUOTE) c2 = c   ! ignore '?

   IF (c2 == 0.AND.(Cline(c:c+1) == '++'.OR.Cline(c:c+1) == '--')) THEN
      ncase = 2                      ! extract pre-(inc)(dec)rement statement
      EXIT

   ELSE IF (c2 == 0.AND.(Cline(c:c+1) == '+='.OR.Cline(c:c+1) == '-='.OR. &
                         Cline(c:c+1) == '*='.OR.Cline(c:c+1) == '/=')) THEN
      ncase = 3                      ! expand x+=n -> x=x+n
      EXIT

   ELSE IF (ch == ':'.AND.c2 /= 0) THEN   ! ? :  detect
      c3 = c
      ncase = 4
      EXIT

   ELSE IF (Cline(c:c+6) == 'malloc('.OR.Cline(c:c+6) == 'calloc') THEN
      ncase = 5
      EXIT
   END IF
END DO

SELECT CASE (ncase)
CASE (0)
   WRITE (1,90) Cline(1:Cend)    ! final statement
   IF (nat > 0) THEN
      DO n = 1,nat
         WRITE (1,90) 'C2F_attach ',attach(n)
      END DO
   END IF
   CC(1) = SP
   RETURN

CASE (2)                        !   ++x  --x  x++  x--
                                !   1    1     1    1
   c1 = c
   ch1 = '+'
   IF (CC(c1) == '-') ch1 = '-'

   IF (a_z(ICHAR(CC(c1+2)))) THEN  ! assume syn:  ++x --x

      c = c1+2                     ! past ++ --
      n = Get()                    ! string = x

      IF (CC(c) == ']') THEN       ! syn: [++x] output x=x+1 now
         WRITE (1,90) String(1:n), ' = ', String(1:n), ch1, '1;'
         nat = nat+1
         IF (nat > 5) nat = 5         ! gen. attach s[x]=1;
         WRITE (attach(nat),90) Cline(1:c1-1), Cline(c1+2:Cend)

      ELSE                            ! syn: ++x
         IF (CC(c) /= '[') THEN       ! syn: ++x
            WRITE (1,90) String(1:n), ' = ', String(1:n), ch1, '1;'
            WRITE (Fline,90) Cline(1:c1-1),Cline(c1+2:Cend)
         ELSE                         ! assume syn: ++x[..];
            WRITE (Fline,90) String(1:n), Cline(c:Cend-1), ' = ', &
                             String(1:n), Cline(c:Cend-1), ch1, '1;'
         END IF
      END IF

   ELSE                               ! syn j++ j--
                                      ! c1   ^
      IF (CC(c1+2) == ']') THEN       ! syn: s[j++]
         DO c = c1-1,2,-1
            IF (CC(c) == '[') EXIT
         END DO
         String = Cline(c+1:c1-1)     ! j
         n = LEN_TRIM(String)
         WRITE (Fline,90) Cline(1:c1-1), Cline(c1+2:Cend) ! elim ++
         nat = nat+1
         IF (nat > 5) nat = 5
         WRITE (attach(nat),90) String(1:n), ' = ', String(1:n), ch,'1;'

      ELSE IF (Key /= 99.AND.Key /= C_for) THEN  ! syn: j++
         c = 1
         CALL BeginString()
         WRITE (Fline,90) Cline(c:c1-1),' = ',Cline(c:c1-1),ch1,'1;'

      ELSE
         DO c = c1-1,1,-1
            ch = CC(c)
            IF (ch == '['.OR.ch == ']'.OR.ch == '*') CYCLE  !include in j string
            IF (.NOT.identifier(ICHAR(ch))) EXIT
         END DO
         String = Cline(c+1:c1-1)              ! j
         n = LEN_TRIM(String)

         IF (c == 0.AND.CC(1) /= '*') THEN     ! syn plain: j++;
            WRITE (1,90) String(1:n),' = ',String(1:n),ch1,'1;'
         ELSE
            WRITE (Fline,90) Cline(1:c1-1), Cline(c1+2:Cend) ! elim ++
            nat = nat+1
            IF (nat > 5) nat = 5
            IF (String(1:1) == '*') THEN        ! syn: *j++
               WRITE (attach(nat),90) String(2:n),' = ',String(2:n),ch1,'1;'
            ELSE
               WRITE (attach(nat),90) String(1:n),' = ',String(1:n),ch1,'1;'
            END IF
         END IF
      END IF
   END IF

CASE (3)                        ! x+=n  x-=n  x*=n  x/=n
   c1 = c                       ! ch = +-*/
   DO n = 1,Cend                ! find start of x
      IF (CC(n) /= SP) EXIT
   END DO
   c = c1+2
   k = Get()                    ! move c to end nnn
   IF (CC(c1+2) == '('.OR.CC(c) == '['.OR.CC(c) == ';') THEN     ! use rightside as is
      WRITE (Fline,90) Cline(1:c1-1) // '=' // Cline(n:c1-1) // ch // &
                                               Cline(c1+2:Cend)
   ELSE       ! wrap rightside in (...)
      WRITE (Fline,90) Cline(1:c1-1) // '=' // Cline(n:c1-1) // ch // '(' // &
                       Cline(c1+2:Cend-1)  // ')' // Cline(Cend:Cend)
   END IF

CASE (4)                           ! c2,c3 points  ?    :
   c = 1                           ! syn:  arg(e1) ? e2 : (e3);
   CALL BeginString()              !       0   1     2    3
   s0 = c                          ! syn:  arg((e1) ? e2 : ((e3) ? e4 : e5));
   e0 = 0                          !       0   1     2      3     4    5
   DO c = c,c2
      ch = CC(c)
      IF (ch == '=') e0 = C
      IF (ch == '('.OR.ch == '!'.OR.ch == '>'.OR.ch == '<'.OR.ch == '(') EXIT
   END DO
   IF (CC(c) == '(') THEN
      c = c-1                      ! arg does not include arg(
      e0 = c
   END IF
   IF (e0 == 0) THEN               ! no arg output
      s0 = 2                       ! and no debugger trap
      e0 = 1
   END IF

   c = e0+1
   CALL BeginString()
   IF (CC(c) == '(') c = c+1
   CALL BeginString()
   IF (CC(c) == '(') c = c+1
   s1 = c
   DO c = c2-1,e0,-1
      IF (CC(c) /= SP) EXIT        ! point at (e1) ?
   END DO                          !             ^
   e1 = c
   IF (CC(c) == ')') e1 = e1-1
   openp = 0
   DO c = s1,e1
      IF (CC(c) == '(') openp = openp+1
      IF (CC(c) == ')') openp = openp-1
   END DO
   IF (openp > 0) e1 = e1+1        ! restore closing )

   c = c2+1
   CALL BeginString()
   s2 = c
   e2 = c3-1
   IF (CC(e2) == SP) e2 = e2-1

   c = c3+1
   CALL BeginString()
   s3 = c
   IF (Cline(c:c+1) == '((' .OR. &
       Cline(c:c+2) == '( (') THEN
      DO c = c+1,c+5
         IF (identifier(ICHAR(CC(c)))) EXIT
      END DO
      s3 = c
      CALL MoveToNext('?')
      c4 = c
      DO c = c4-1,s3,-1
         IF (CC(c) /= SP) EXIT
      END DO
      IF (CC(c) == ')') c = c-1
      e3 = c

      c = c4+1
      CALL BeginString()
      s4 = c
      CALL MoveToNext(':')
      c5 = c
      DO c = c5-1,s4,-1
         IF (CC(c) /= SP) EXIT
      END DO
      IF (CC(c) == ')') c = c-1
      e4 = c

      c = c5+1
      CALL BeginString()
      s5 = c
      e5 = last_col()
      WRITE (1,90) 'if(', Cline(s1:e1), ')'
      WRITE (1,90) Cline(s0:e0), Cline(s2:e2),';'
      WRITE (1,90) 'else if (', Cline(s3:e3), ')'
      WRITE (1,90) Cline(s0:e0), Cline(s4:e4),';'
      WRITE (1,90) 'else'
      WRITE (1,90) Cline(s0:e0), Cline(s5:e5),';'

   ELSE                     ! plain else (e3);
      e3 = last_col()
      IF (CC(e3) == SP.OR.CC(e3) == ';') e3 = e3-1
      WRITE (1,90) 'if(', Cline(s1:e1), ')'
      WRITE (1,90) Cline(s0:e0), Cline(s2:e2),';'
      WRITE (1,90) 'else'
      WRITE (1,90) Cline(s0:e0), Cline(s3:e3),';'
   END IF

   CC(1) = SP
   RETURN        ! no reprocess

CASE (5)         ! contains "malloc" append next record
   READ (11,90) Cline2
   BACKSPACE (11)              ! back to where it was
   n = LEN_TRIM(Cline2)
   WRITE (1,90) Cline(1:Cend), Cline2(1:n)
   CC(1) = SP
   RETURN                      ! no reprocess

END SELECT

IF (Fline /= SP) THEN          ! reprocess, may be further mods
   Cline = Fline
   Cend = LEN_TRIM(Cline)
   GO TO 1                     ! case = 0 will finally output cline
END IF

IF (nat > 0) THEN
   DO n = 1,nat
      WRITE (1,90) 'C2F_attach ',attach(n)
   END DO
END IF

CC(1) = SP          ! do not output Cline in pass 1 exec

90 FORMAT (99A)
END SUBROUTINE statement_preproc

! -----------------------------------------------------------
SUBROUTINE structure_preproc()     ! Key = C_struct,C_typedef,C_union
                                   ! preprocess members within {....}

INTEGER :: count, pass, openleft, nright, nrecords, nrec, nrec1, cend1
INTEGER :: n1, n2, n3
INTEGER :: c1s(MAXMEMBERS), cends(MAXMEMBERS), mark_brace_record(20)
CHARACTER (LEN=MAXSTR) :: brace_names(20)   ! 20 = max nested structs in struct
CHARACTER (LEN=MAXSTR) :: structname1, structname

IF (Key == C_typedef) THEN
   n = Get()
   argtype = String            ! possibly xxx or struct or union
ELSE IF (Key == C_struct) THEN
   argtype = 'struct'          ! struct or union
ELSE
   argtype = 'union'           ! struct or union
END IF
c0 = c

! syn: below is plain typedef <int> XXX;    no {..} involved
IF (Key == C_typedef.AND.argtype /= 'struct'.AND.argtype /= 'union') THEN
   CALL BeginString()
   IF (CC(c) == '*') c = c+1   ! syn:  typedef void *pointer

   k = Get()                          ! xxx;
   argname = UPPER(k)      ! typedefs are entered as uppercase NAMES
   DO n = 1,MAXTYPEDEFS
      String = Typedefs(2,n)
      IF (String == SP.OR.String == argname) EXIT ! enter or replace
   END DO
   IF (n < MAXTYPEDEFS) THEN
      Typedefs(1,n) = argtype
      Typedefs(2,n) = argname
   ELSE
      CALL Error(17)       ! typedefs > MAX
   END IF
   CC(1) = SP
   RETURN
END IF

! syn:   struct or  union   or  typedef struct  or typedef union
DO c = 1,Cend
   IF (CC(c) == '='.OR.CC(c) == ';'.OR.CC(c) == '{') EXIT
END DO

IF (CC(c) == '=') THEN   ! initialization syn: struct name1 name2={....};
   WRITE (1,90) Cline(1:c-1),';'   ! output struct name1 name2;
   c = 1
   n = Get()
   n = Get()
   n = Get()                       ! name2
   WRITE (1,90) string(1:n),Cline(c:Cend)    ! name2={...};
   CC(1) = SP
   RETURN
ELSE IF (CC(c) == ';') THEN
   RETURN   ! let pass1 output,  syn:  struct/union name1 name2;
END IF

structname1 = SP
structname = SP
brace_names = SP           ! {...} name;
mark_brace_record = 0
c1s = 0
cends = 0
nright = 0
nrec = 0
openleft = -1

c = c+1               ! past {
CALL BeginString()
c1 = c
c = c-1

DO     ! accum. brace names and member locations
   c = c+1
   IF (c > Cend) EXIT
   IF (CC(c) == '{') THEN
      IF (openleft == -1) THEN
         openleft = 1
      ELSE
         openleft = openleft+1
      END IF
   ELSE IF (CC(c) == '}') THEN
      openleft = openleft-1
       c = c+1                          ! past }
      CALL BeginString()
      c1 = c
      CALL MoveToNext(';')
      nright = nright+1
      brace_names(nright) = Cline(c1:c-1)      ! } name   minus ;
      mark_brace_record(nright) = nrec    ! mark
      c1 = c+1                            ! past ;
   ELSE
      CALL MoveToNext(';')
      nrec = nrec+1
      c1s(nrec) = c1
      cends(nrec) = c
      c1 = c+1                            ! past ;
   END IF
END DO
nrecords = nrec

! phase 1  extract/output embedded struct/union members from Cline

count = 0             ! index of } being processed
nrec = 0              ! index of member; being processed

DO
   nrec = nrec+1
   IF (nrec > nrecords) EXIT
   c1 = c1s(nrec)
   cend1 = cends(nrec)
   chend = CC(cend1)
   c = c1
   n1 = Get()                         ! get 1st name in member;
   argtype = string
   IF (argtype == 'struct'.OR.argtype == 'union') THEN
      DO c = c,cend1
         IF (CC(c) == '{') EXIT       ! struct {...} must be extracted
      END DO
      IF (CC(c) /= '{') CYCLE         ! syn: struct list name; out in pass 2
      count = count+1     ! # } index
      n2 = Get()
      IF (string == SP) THEN          ! use last } name;
         IF (argtype == 'struct') THEN
            string = TRIM(brace_names(count)) //'_T'  ! anony. struct_T
         ELSE
            string = TRIM(brace_names(count)) //'_U'  ! anony. union_T
         END IF
         n2 = LEN_TRIM(string)
      END IF
      structname = UPPER(n2)
      WRITE (1,90) argtype(1:n1) // ' ' // structname(1:n2)
      nrec1 = nrec    ! save this record# to insert struct name1 name2;
      DO c = c1,cend1
         IF (CC(c) == '{') EXIT
      END DO
      IF (CC(c) == '{') THEN    ! breakup struct statement{...}
         WRITE (1,90) '{'
         c = c+1                     ! past {
         CALL BeginString()
         c1 = c
         IF (nrec == mark_brace_record(count)) THEN
            DO                    ! unpack declaration
               IF (CC(c) == ';') THEN
                  IF (c < cend1) THEN
                     WRITE (1,90) Cline(c1:c)
                  ELSE
                     WRITE (1,90) Cline(c1:cend1)
                  END IF
                  c = c+1           ! past ;
                  CALL BeginString()
                  c1 = c            ! start next output entity
               ELSE
                  c = c+1
               END IF
               IF (c > cend1) EXIT
            END DO
         END IF
         IF (c1 < cend1) WRITE (1,90) Cline(c1:cend1)
      END IF

      ! output remaining {....} records associated with this struct/union
      DO
         nrec = nrec+1
         IF (nrec > nrecords) EXIT
         cend1 = cends(nrec)
         c = c1s(nrec)
         CALL BeginString()
         IF (CC(c) == '{') THEN
            WRITE (1,90) '{'
            c = c+1
            IF (Cline(c:cend1) == SP) THEN
               cends(nrec) = 0              ! remove from pass 2 processing
               CYCLE
            END IF
            CALL BeginString()
         END IF
         c1 = c
         DO
            IF (CC(c) == ';') THEN
               IF (c < cend1) THEN
                  WRITE (1,90) Cline(c1:c)
               ELSE
                  WRITE (1,90) Cline(c1:cend1)
               END IF
               c = c+1           ! past ;
               CALL BeginString()
               c1 = c            ! start next output entity
            ELSE IF (CC(c) == '=') THEN
               WRITE (1,90) Cline(c1:c)
               c = c+1                   ! past =
               CALL BeginString()
               WRITE (1,90) Cline(c:cend1)
               c = cend1+1
               c1 = c
            ELSE
               c = c+1
            END IF
            IF (c > cend1) EXIT
         END DO
         IF (c1 < cend1) WRITE (1,90) Cline(c1:cend1)
         cends(nrec) = 0     ! remove this nrec from pass 2 processing
         IF (nrec == mark_brace_record(count)) EXIT
      END DO

      WRITE (1,90) '}'

      name = brace_names(count)
      n3 = LEN_TRIM(name)
      Fline = 'struct ' // TRIM(structname) // ' ' // name(1:n3) // ';'
      f = LEN_TRIM(Fline)
      c1 = c1s(nrec1)
      Cline(c1:c1+f-1) = Fline(1:f)     ! stuff back into Cline
      cends(nrec1) = c1+f-1
   END IF     ! 1 struct/union{...} processed

END DO        ! nrecords scanned

! phase2  output residual Cline members remaining after extraction

c = 1
n1 = Get()
IF (Key == C_typedef) n1 = Get()
argtype = string
n2 = Get()
IF (string == SP) THEN          ! syn:  struct  {
   structname1 = brace_names(nright)          ! use attached } name;
   n2 = LEN_TRIM(structname1)
   IF (argtype == 'struct') THEN
      structname1 = structname1(1:n2)//'_T'   ! anonymous struct_T
   ELSE
      structname1 = structname1(1:n2)//'_U'   ! anonymous union_T
   END IF
   n2 = n2+2
ELSE                            ! syn:  struct name1 {
   structname1 = string                ! save
END IF

WRITE (1,90) argtype(1:n1),' ',structname1(1:n2)
WRITE (1,90) '{'

nrec = 0              ! index of member; being processed

DO    ! output residual Cline members remaining after pass1
   nrec = nrec+1
   IF (nrec > nrecords) EXIT
   IF (cends(nrec) == 0) CYCLE   ! this member processed in pass 1
   c1 = c1s(nrec)
   cend1 = cends(nrec)
   WRITE (1,90) Cline(c1:cend1)
END DO
WRITE (1,90) '}'

name = brace_names(nright)
n3 = LEN_TRIM(name)
string = name
arg3 = UPPER(n3)
string = structname1
arg2 = UPPER(n2)
IF (arg2 /= arg3) WRITE (1,90) argtype(1:n1),' ',structname1(1:n2),' ',name(1:n3),';'

IF (Key == C_typedef) THEN   ! enter typedef name
   DO n = 1,MAXTYPEDEFS
      string = Typedefs(2,n)
      IF (string == SP.OR.string == name) EXIT ! enter or replace
   END DO
   IF (n < MAXTYPEDEFS) THEN
      Typedefs(1,n) = 'struct ' // structname1(1:n2)
      Typedefs(2,n) = arg3       ! typedef names are upper
   ELSE
      CALL Error(17)       ! typedefs > MAX
   END IF
END IF

CC(1) = SP
RETURN

101 CALL Error(2)       ! lost track of {} closures
RETURN

90 FORMAT (99A)
END SUBROUTINE structure_preproc

! -----------------------------------------------------------
SUBROUTINE for_preproc()     ! syn:  for (e1,e1,arg1;e2,e2,arg2;e3,e3)
                             !        c ^
LOGICAL :: point1, dowhile
INTEGER :: c1,c2,k1
CHARACTER :: ch
CHARACTER (LEN=MAXSTR) :: name1, name2, name3, e(5,0:2)

DO c = 1,Cend
   IF (CC(c) == '(') EXIT
END DO
c = c+1                      ! past for ( e1
CALL BeginString()           !  c         ^

point1 = .FALSE.
dowhile = .FALSE.
e = SP
n = 0
k = 0
k1 = 1
c1 = 1

DO c = c,Cend                ! extract expressions -> e, arg1, arg2
   ch = CC(c)
   IF (ch /= SP.AND.c1 == 1) c1 = c  ! save start of expression

   IF (ch == ','.OR.(ch == ')'.AND.n == 2)) THEN
      k = k+1
      IF (k > 5) THEN
        CALL Error(10)          ! unrec. syntax
        RETURN
     END IF

     IF (c1 > 1) e(k,n) = Cline(c1:c-1)     ! save, excluding ,

     IF (ch == ')') THEN
        arg3 = SP
        IF (c1 > 1) THEN
           IF (Cline(c1:c1+1) /= '++'.AND.Cline(c1:c1+1) /= '--') THEN
              arg3 = Cline(c1:c-1)       ! save, excluding )
              IF  (arg3 == SP) EXIT
              DO n = c-1,c-4,-1
                 IF (CC(n) /= SP) EXIT
              END DO
              IF (Cline(n-1:n) /= '++'.AND.Cline(n-1:n) /= '--') dowhile = .TRUE.
           END IF
        END IF
        EXIT
     END IF
     c1 = 1                 ! reset for next e detect

   ELSE IF (ch == ';') THEN
     n = n+1
     IF (n == 1) THEN
        IF (c == c1) THEN     ! no arg1; detect
           arg1 = ';'
        ELSE
           arg1 = Cline(c1:c)
        END IF
     ELSE IF (n == 2) THEN
        IF (CC(c1) == '*') THEN
           arg2 = Cline(c1+1:c)    ! delete *
           point1 = .TRUE.
        ELSE IF (c == c1) THEN     ! no arg2; detect
           arg2 = ';'
        ELSE
           arg2 = Cline(c1:c)
        END IF
     END IF
     c1 = 1                 ! reset for next e detect
     k = 0                  !   "
   END IF
END DO

c2 = c                      ! save ) index

DO n = 0,1                  ! e ahead of arg2; are output now
   DO k = 1,5
      string = e(k,n)
      IF (string == SP) EXIT
      WRITE (1,90) string(1:LEN_TRIM(string)),';'
      dowhile = .TRUE.       ! any output above activates do while
   END DO
END DO

name1 = getname(arg1)
name2 = getname(arg2)
name3 = getname(arg3)
IF (name1 /= name2.OR. &
    (arg3 /= SP.AND.name1 /= name3)) dowhile=.TRUE. ! syn: for(e1; n-- != 0;

IF ((dowhile.AND.arg1 /= SP)) THEN
   WRITE (1,90) arg1         ! cant use name1 in simple do name1=
   arg1 = SP
END IF

IF (arg1 == ';'.AND.arg2 == ';') THEN         ! syn: for( ;; )
   WRITE (1,90) 'do'

ELSE IF (.NOT.dowhile.AND.arg1 /= ';') THEN   ! syn: for(arg1;arg2;e3)
   string = e(1,2)                     ! 1st e after 2nd ;
   WRITE (1,90) 'for(',  arg1(1:LEN_TRIM(arg1)), arg2(1:LEN_TRIM(arg2)), &
                     string(1:LEN_TRIM(string)), ')', Cline(Cend+1:Cend)
   k1 = 2                         ! skip e(1,2) output below

ELSE                     ! nasty for syntax
   i = LEN_TRIM(arg2) -1               ! minus ;
   n = LEN_TRIM(name2)

   IF (arg2(n+1:n+2) == '++'.OR.arg2(n+1:n+2) == '--') THEN ! for(e1; n-- !=0
      WRITE (1,90) 'while (',name2(1:n), arg2(n+3:i), ')'   ! while( n != 0 )
      WRITE (1,90) name2(1:n), arg2(n+1:n+2), ';  /*C2F*/ ' ! n--;

   ELSE IF (.NOT.point1) THEN
      WRITE (1,90) 'while (', arg2(1:i), ')'    ! while (arg2)

   ELSE                                ! assume syn: for( ; *string >
      c = 1
      CALL MoveToNext(';')           ! 1st ;
      c = c+1
      CALL BeginString()
      c = c+1                        ! past *
      j = Get()                      ! string = do identifier
      WRITE (1,90) 'int ' // string(1:j) // '_;'
      WRITE (1,90) string(1:j) // '_=1;'
      WRITE (1,90) 'while (',  &
                    string(1:j),'(', string(1:j),'_:',string(1:j),'_) ', &
                    arg2(j+1:i),' && ',string(1:j),'_ <= LEN_TRIM(',string(1:j),'))'
      WRITE (1,90) string(1:j),'_ =',string(1:j),'_ +1;   /*C2F*/'
      k1 = 2                         ! skip e(1,2) output below
   END IF
END IF

DO k = k1,5                         ! output e after 2nd ; starting with k1
   string = e(k,2)
   IF (string == SP) EXIT
   i = LEN_TRIM(string)
   IF (string(i-1:i) == '++'.OR.string(i-1:i) == '--') THEN
      WRITE (1,90) string(1:i-2),' = ',string(1:i-2),string(i:i),'1;  /*C2F*/ ' ! signal to pass 3
   ELSE
      WRITE (1,90) string(1:i),';  /*C2F*/ ' ! signal to pass 3
   END IF
END DO

DO k = 1,5                          ! output e after 1st ;  (again)
   string = e(k,1)
   IF (string == SP) EXIT
   i = LEN_TRIM(string)
   WRITE (1,90) string(1:i),';  /*C2F*/ ' ! signal to pass 3
END DO

IF (CC(Cend) == ';') THEN         ! syn:  for(....) e;
   DO c = c2+1,Cend               !            c2 ^
      IF (CC(c) /= SP) EXIT       ! start of e field
   END DO
   c1 = c
   n = Get()
   IF (string == 'if') THEN
      CALL BeginString()
      openp = 0
      DO c = c,Cend
         IF (CC(c) == '(') openp = openp+1
         IF (CC(c) == ')') openp = openp-1
         IF (openp == 0) EXIT
      END DO
      WRITE (1,90) Cline(c1:c)
      c = c+1
      CALL BeginString()
      WRITE (1,90) Cline(c:Cend)
   ELSE
      Cline = Cline(c1:Cend)              ! statement;
      Cend = LEN_TRIM(Cline)
      CALL statement_preproc()           ! output statement;
   END IF

END IF

CC(1) = SP                  ! flag pass1 exec to not output cline

90 FORMAT (99A)
END SUBROUTINE for_preproc

! -----------------------------------------------------------
SUBROUTINE if_else_preproc() ! if( ( x = fun( ) ) == y )
                             !     1 2 3          4    5
INTEGER :: inkey
LOGICAL :: open_brace
CHARACTER :: ch

inkey = Key           ! save
IF (CC(Cend) == ';'.AND.KeyPrev == C_if.AND.inkey == C_if) WRITE (1,90) '{'

CALL BeginString()
IF (Key == C_else.AND.Cline(c:c+1) /= 'if') THEN
   WRITE (1,90) 'else'
   IF (Cline(5:10) /= SP) THEN
      CALL BeginString()
      WRITE (1,90) Cline(c:Cend)
   END IF
   CC(1) = SP
   RETURN
END IF

c1 = 0; c2 = 0 ; c3 = 0 ; c4 = 0 ; c5 = 0

DO c = c,Cend
   IF (CC(c) == '(') EXIT
END DO
c1 = c
openp = 1

DO c = c1+1,Cend                       ! detect c1,c2,c3,c4 indices
   ch = CC(c)
   IF (ch == '(') openp=openp+1
   IF (ch == ')') openp=openp-1
   IF (openp == 0) EXIT             ! terminating )  if(...()..)
   IF (ch == '('.AND.c2 == 0) c1 = c                 ! track ( ahead of x
   IF (identifier(ICHAR(ch)).AND.c2 == 0) c2 = c     ! 1st char x
   IF (ch == '='.AND.c3 == 0) c3 = c                 ! = detect
   IF (c4 == 0) THEN
      IF (cline(c:c+1) == '=='.OR.cline(c:c+1) == '!=' &
                              .OR. ch == '<'.OR.ch == '>') c4 = c
   END IF
END DO
c5 = c

ch = CC(c2-1)                          ! examine char ahead of x

IF (ch == '+'.OR.ch == '-') THEN       !  ++x or --x to extract

   WRITE (1,90) Cline(c2:c4-1),'=',Cline(c2:c4-1),ch,'1;'   ! x=x+-1;
   WRITE (1,90) 'if(',Cline(c2:c5)    ! if(x>0)

ELSE IF (c3 /= 0.AND.c4 > c3) THEN    !  x=statement to extract
   ch = CC(c2-1)
   c = c4
   DO                                 ! scan back from ==
      c = c-1
      ch = CC(c)
      IF (ch == ')') THEN             ! found statement ends with )
         DO n = c-1,c-4,-1
            IF (CC(n) == ')') THEN    ! found statement ends with ) )
               c = c-1                ! discard last )
               EXIT
            END IF
         END DO
         EXIT
      END IF
      IF (identifier(ICHAR(ch))) EXIT    ! no closing ) in statement
   END DO
   WRITE (1,90) Cline(c2:c),';'       ! x=....;
   WRITE (1,90) 'C2F_attach if(',Cline(c2:c3-1),Cline(c4:c5)  ! if(x==y)

ELSE                                  ! no statement to extract
   WRITE (1,90) Cline(1:c5)           ! output if(.....)
END IF

ch = CC(Cend)
IF (ch == ';') THEN                   ! syn: if(...) statement;
   c = c5+1                           ! past )
   CALL Beginstring()
   IF (CC(c) == '{') THEN
      WRITE (1,90) '{'
      c = c+1                         ! past {
   END IF
   Cline = Cline(c:Cend)              ! statement;
   Cend = LEN_TRIM(Cline)
   CALL statement_preproc()           ! output statement;
END IF

IF (CC(Cend) == ';'.AND.KeyPrev == C_if.AND.inkey == C_if) WRITE (1,90) '}'

CC(1) = SP                            ! dont output Cline

90 FORMAT (99A)
END SUBROUTINE if_else_preproc

! -----------------------------------------------------------
SUBROUTINE while_preproc()

! 1. while (e1);        -> do nothing, its attached to  do {...} while(..);
! 2. while (e1) e2;     -> while (e1)
!                           e2;
! 3. while ( ( (x = fun1(..)) == NULL && (.....) )
!          1   2  3         4                    5

DO c = 1,Cend
   IF (CC(c) == '(') EXIT
END DO
c1 = c
c2 = 0
c3 = 0
openp = 0
DO c = c1,Cend
   ch = CC(c)
   IF (ch == '=') THEN
      IF (Cline(c:c+1) == '==') CYCLE
      IF (Cline(c-1:c) == '==') CYCLE
      IF (Cline(c-1:c) == '<=') CYCLE
      IF (Cline(c-1:c) == '>=') CYCLE
      IF (Cline(c-1:c) == '!=') CYCLE
      IF (c3 ==0) c3 = c                      ! save embedded =
   ELSE IF (Cline(c:c+1) == '++'.OR.Cline(c:c+1) == '--') THEN
      c2 = c
   END IF

   IF (ch == '(') openp = openp+1
   IF (ch == ')') openp = openp-1
   IF (openp == 0) THEN
      c5 = c
      EXIT
   END IF
END DO

c = c5+1
CALL BeginString()
c6 = c
IF (c3 == 0) THEN                  ! no embedded  x=fun() to extract
   IF (CC(c6) == ';') RETURN       ! assume do {....} while(....);
                                   !                             ^
   c = c1+1
   CALL BeginString()
   IF (CC(c) == '1') THEN          ! assume while (1)
      WRITE (1,90) 'do'
      IF (CC(Cend) == ';') WRITE (1,90) Cline(c6:Cend)
      CC(1) = SP
      RETURN

   ELSE IF (c2 /= 0) THEN               ! while (--  ++ ) detect

      IF (identifier(ICHAR(CC(c2+2)))) THEN     ! while (--k) or (++k)
         DO c = c5-1,c2+2,-1
            IF (CC(c) /= SP) EXIT
         END DO                                          ! --k -> k--
         WRITE (1,90) Cline(c2+2:c), Cline(c2:c2+1),';'         ! k--;
         WRITE (1,90) Cline(1:c1), Cline(c2+2:c5-1), ' !=0 )'   ! while (k!=0)
         WRITE (1,90) Cline(c2+2:c), Cline(c2:c2+1),'; /*C2F*/' ! k--;
      ELSE                                      ! while (k--)
         WRITE (1,90) Cline(1:c2-1), ' !=0 ', Cline(c2+2:c5)    ! while (k!=0)
         WRITE (1,90) 'C2F_attach ',Cline(c1+1:c2+1), ';'       ! k--;
      END IF
      IF (CC(Cend) == ';') WRITE (1,90) 'C2F_attach ',Cline(c6:Cend) !statement;

   ELSE                             ! plain while
      WRITE (1,90) Cline(1:c5)                        ! while (e1)
      IF (CC(Cend) == ';') WRITE (1,90) Cline(c6:Cend) ! statement;
   END IF
   CC(1) = SP
   RETURN
END IF

! embedded (..=..) statement
DO c = c3,2,-1
   IF (CC(c) == '(') EXIT
END DO
c2 = c
openp = 0
DO c = c2,Cend
   IF (CC(c) == '(') openp = openp+1
   IF (CC(c) == ')') openp = openp-1
   IF (openp == 0) EXIT
END DO
c4 = c
c = c2+1
CALL BeginString()           ! if ( xxx =
c2 = c                       !      ^
WRITE (1,90) 'do'
WRITE (1,90) '{'
WRITE (1,90) Cline(c2:c4-1),';'
WRITE (1,90) 'if(!', Cline(c1+1:c2-1), Cline(c2:c3-1), Cline(c4+1:c5),')'
WRITE (1,90) 'break;'        ! cant be part of above output, strange..

IF (c5+1 < Cend) THEN
   WRITE (1,90) Cline(c5+1:Cend)
   WRITE (1,90) '}'
ELSE
   READ (11,90) Cline    ! thru with current rec. examine 1st char next rec
   BACKSPACE (11)
   IF (CC(1) == '{') THEN
      delete_leftbrace = .TRUE.   ! ask pass1 exec to discard
   ELSE
      add_rightbrace(1) = .TRUE.     ! after next record
   END IF
END IF

CC(1) = SP

90 FORMAT (99A)
END SUBROUTINE while_preproc

! -----------------------------------------------------------
! PASS 2 routines
! -----------------------------------------------------------
SUBROUTINE Append_Record()

Nrec = Nrec+1                    ! advance to next record
n1 = Cends(Nrec)                 ! chars in next record ops field

READ (21,90) (CC(Cend+n),n=1,n1) ! append next record
c = Cend+1
Cend = Cend+n1
Chend = CC(Cend)

90 FORMAT (99A)
END SUBROUTINE Append_Record

! -----------------------------------------------------------
SUBROUTINE ARGLIST_proc()       ! called from ??

INTEGER :: nsub, narg, narg2, argkey
LOGICAL :: void, asterisk
CHARACTER $9*2, $99*3

CALL BeginString()              ! already past SubName(

IF (CC(c) == ')'.OR.Cline(c:c+4) == 'void)') THEN   ! no arg list  sub();
   FF(f) = ')'
   f = f+1
   c = c+1
   RETURN               ! routine name not entered in V
END IF

FunName = Modulenames(Nmod)

narg2 = 1
narg = 0

DO                              ! process ( int arg1, int *arg2, int **arg3,,
   IF (c > Cend) EXIT

   IF (narg < MAXARG) narg = narg+1  ! or ( arg1, arg2,,,,

   point1 = .FALSE.
   point2 = .FALSE.
   point3 = .FALSE.
   CALL BeginString()
   IF (CC(c) == '&') c = c+1    ! discard, &arg1 -> arg1

   IF (CC(c) == '*') THEN       ! error.  (*arg   with no type
      CALL Error(13)            ! unknown arg type
      RETURN
   END IF

   n2 = Get()                   ! get type info

   Lconst = .FALSE.
   IF (String == 'const') THEN
      Lconst = .TRUE.
      n2 = Get()
   END IF

   argname = String             ! becomes argname if no type detect below
   argkey = 0
   void = .FALSE.

   IF (String == 'short')  argkey = TSHORT
   IF (String == 'long')   argkey = TINT
   IF (String == 'int')    argkey = TINT
   IF (String == 'float')  argkey = TFLOAT
   IF (String == 'double') argkey = TDOUBLE
   IF (String == 'char')   argkey = TCHAR
   IF (String == 'struct') argkey = TSTRUCT
   IF (String == 'FILE')   argkey = TINT
   IF (String == 'void') THEN
      argkey = TINT
      void = .TRUE.
   END IF
   CALL BeginString()

   IF (argkey == TSTRUCT) THEN
      n1 = Get()                ! syn: struct bla
      structname = UPPER(n1)
      CALL BeginString()
   END IF

   IF (argkey /= 0) THEN       ! syn:  int x, int *x, int **x
      IF (Cline(c:c+1) == '***') THEN
         CALL Error(11)             ! unsupported syntax
         RETURN
      ELSE IF (Cline(c:c+1) == '**') THEN
         point2 = .TRUE.
         c = c+2
      ELSE IF (CC(c) == '*') THEN
         point1 = .TRUE.
         IF (string == 'FILE'.OR.argkey == TSTRUCT) point1 = .FALSE. ! FILE translated to INT above
         c = c+1
      END IF

      CALL BeginString()
      IF (Cline(c:c+1) == '(*') THEN
         c = c+2
         n2 = Get()
         string = 'I___' // string
         asterisk = .FALSE.
         DO c = c,Cend
            IF (CC(c) == '*') asterisk = .TRUE.  ! func proto in arg list
            IF (CC(c) == ',') EXIT
         END DO
         IF (asterisk.AND.CC(c) == ',') THEN
            c = c+1
            CALL MoveToNext(')')
            c = c+1                  ! past proto )
         END IF
         n2 = n2+4
      ELSE
         n2 = Get()
      END IF
      argname = String
   END IF

   nid = GetIndex(string)            ! check if name already declared in global

   IF (nid > 0) THEN                ! this arg is a global FUN EXTERN
      Fline(f:f+n2-1) = argname(1:n2)
      f = f+n2
      CALL BeginString()
      GO TO 1

   ELSE                              ! enter arg info
      nid = GetNextIndex()
      v.name(nid) = argname
      v.nc(nid) = n2
      v.type(nid) = argkey   ! may be 0  (none), supplied later by declare_proc
      v.declare(nid) = SCALAR          ! else = FUN
      v.locate(nid) = ARG              ! else = EXTERN
      v.ptr_void(nid) = void              ! set T/F as per argkey test above
   END IF

   IF (argname(1:4) == 'I___') THEN    ! above declaration needs fixing
      v.type(nid) = TINT
      v.locate(nid) = NONE

      nid = GetNextIndex()             ! now generate name arg declaration
      v.name(nid) = argname(5:n2)
      v.nc(nid) = n2-4
      v.type(nid) = argkey
      v.declare(nid) = SCALAR
      v.locate(nid) = EXTERN
   END IF

   IF (argkey == TSTRUCT) THEN
      v.name2(nid) = structname
      n = GetIndex(structname)         ! get master struct index
      DO n = n+1,MID                   ! now copy its members
         IF (.NOT.v.member(n)) EXIT    ! end master struct members
         string = v.name(n)
         nc = ABS(v.nc(n))
         DO i = 2,nc
            IF (string(i:i) == '%') EXIT
         END DO
         nid = nid+1                   ! contigous assign
         v.name(nid) = argname(1:n2)//string(i:nc)
         v.name2(nid) = v.name2(n)
         v.nc(nid) = -LEN_TRIM(v.name(nid))
         v.type(nid) = v.type(n)
         v.declare(nid) = v.declare(n)
         v.locate(nid) = NONE
         v.member(nid) = .TRUE.
         v.dim(nid) = v.dim(n)
         v.init(nid) = v.init(n)
      END DO
   END IF

   IF (argkey > 0) THEN
      IF (point1) THEN
         v.declare(nid) = PTR1
         v.dim(nid) = '(*)'
!!         IF (v.type(nid) /= TCHAR) THEN
!!            v.declare(nid) = PTR1
!!            v.dim(nid) = '(*)'
!!         ELSE  ! char *p
!!            v.declare(nid) = SCALAR
!!            v.dim(nid) = '[]'
!!         END IF
      ELSE IF (point2.AND.Use_pointer2) THEN
         v.declare(nid) = PTR2
         v.dim(nid) = '(*)'
      ELSE IF (point2) THEN
         v.declare(nid) = ARRAY2
         v.dim(nid) = '(*,*)'
      ELSE IF (point3) THEN
         CALL Error(11)     ! unsupported syntax
         RETURN
      END IF
   END IF

   Fline(f:f+n2-1) = argname(1:n2)
   f = f+n2
   CALL BeginString()

   IF (Cline(c:c+1) == '[]') THEN
      IF (Lconst) THEN
         v.declare(nid) = CONST
         v.dim(nid) = '[]'
      ELSE
         v.declare(nid) = ARRAY1
         v.dim(nid) = '(*)'
         IF (argkey == TCHAR) THEN
            v.dim(nid) = '[]'
            IF (.NOT.point1) v.declare(nid) = SCALAR
         END IF
      END IF
      c = c+2
      CALL BeginString()

   ELSE IF (CC(c) == '[') THEN
      v.declare(nid) = ARRAY1
      c1 = c+1                        ! past [
      CALL MoveToNext(']')
      dim_s = '(' // Cline(c1:c-1) // ')'
      c = c+1                         ! past ]
      IF (CC(c) == '[') THEN
         v.declare(nid) = ARRAY2
         c1 = c+1                     ! past [
         CALL MoveToNext(']')
         n = LEN_TRIM(dim_s)
         dim_s = dim_s(1:n-1) // ',' // Cline(c1:c-1) // ')'
         c = c+1                      ! past ]
      END IF
      v.dim(nid) = dim_s
      CALL BeginString()

   ELSE IF (Cline(c:c+1) == '()') THEN   ! syn:  sub1( int k(),
      c = c+2                            ! ignore ()
   END IF

   IF ((point2.AND..NOT.Use_pointer2)) THEN

      IF (point1) THEN     ! add 2nd argument for char *arg
         Fline(f:f+n2+1) = ',' // argname(1:n2) // '2'
         f = f+n2+2
         string = argname(1:n2) // '2'
         nid = GetNextIndex()
         v.name(nid) = string
         v.nc(nid) = n2+1
         v.type(nid) = TCHAR
         v.declare(nid) = ARRAY2
         v.locate(nid) = ARG
         v.dim(nid) = '(*,*)'
      END IF

      DO narg2=narg2,narg2+1   ! make entries for 2 INTEGER $n
         nid = GetNextIndex()
         IF (narg2 < 10) THEN
            WRITE ($9,'(A,I1)') '$',narg2
            v.name(nid) = $9
            v.nc(nid) = 2
            Fline(f:f+2) = ','//$9
            f = f+3
         ELSE
            WRITE ($99,'(A,I2)') '$',narg2
            v.name(nid) = $99
            v.nc(nid) = 3
            Fline(f:f+3) = ','//$99
            f = f+4
         END IF
         v.type(nid) = TINT
         v.declare(nid) = SCALAR
         v.locate(nid) = ARG
      END DO

   ELSE IF (v.type(nid) == TCHAR.AND.point1.AND.v.dim(nid) == '[]') THEN   ! add arg2
      Fline(f:f+n2+1) = ',' // argname(1:n2) // '2'
      f = f+n2+2
      string = argname(1:n2) // '2'
      nid = GetNextIndex()
      v.name(nid) = string
      v.nc(nid) = n2+1
      v.type(nid) = TCHAR
      v.declare(nid) = ARRAY2
      v.locate(nid) = ARG
      v.dim(nid) = '(99,*)'
   END IF

1  CONTINUE
   IF (CC(c) == ',') THEN
      FF(f) = ','
      f = f+1
      c = c+1                         ! past ,
      CYCLE                           ! process next arg

   ELSE IF (CC(c) == ')') THEN
      FF(f) = ')'
      f = f+1
      c = Cend+1
      EXIT
   END IF
END DO

90 FORMAT (99A)
END SUBROUTINE arglist_proc

! -----------------------------------------------------------
SUBROUTINE extern_proc ()
INTEGER :: n, n1

DO
   IF (CC(Cend) == ';') EXIT    ! keep appending until ;
   CALL append_record()         ! and then ignore...
END DO
CALL BeginString()

END SUBROUTINE extern_proc

! -----------------------------------------------------------
SUBROUTINE program_proc()

Moduletypes(Nmod) = C_program
Modulenames(Nmod) = ProgramName    ! dont bother checking vs. pass1

IF (Debug) THEN
   WRITE (9,'(I3,1X,A)') Nmod,Modulenames(Nmod)
   WRITE (*,'(I3,1X,A)') Nmod,Modulenames(Nmod)
END IF

Fline = 'PROGRAM ' // ProgramName(1:Nsize)
f = 8+Nsize
CommandLine = .TRUE.
IF (Cline(Cend-1:Cend) == '()'.OR. &
    Cline(Cend-5:Cend) == '(void)') CommandLine = .FALSE.

IF (CommandLine) THEN
   v.name(1)    = 'n'
   v.nc(1)      = 1
   v.type(1)    = TINT
   v.declare(1) = SCALAR
   v.locate(1)  = LOCAL
   v.dim(1)     = SP
   v.init(1)    = SP

   v.name(2)    = 'argc'
   v.nc(2)      = 4
   v.type(2)    = TINT
   v.declare(2) = SCALAR
   v.locate(2)  = LOCAL
   v.dim(2)     = SP
   v.init(2)    = SP

   v.name(3)    = 'argv'
   v.nc(3)      = 4
   v.type(3)    = TCHAR
   v.declare(3) = ARRAY1
   v.locate(3)  = LOCAL
   v.dim(3)     = '[20)'
   v.init(3)    = SP
END IF

CALL Push(-C_program)         ! expecting {
c = 1

END SUBROUTINE program_proc

! -----------------------------------------------------------
SUBROUTINE subroutine_proc()

IF (chend /= ';') THEN       ! syn: sub(...) is a subroutine declare

   Moduletypes(Nmod) = C_subroutine

   c = 1
   n = Get()
   IF (String == 'void') n = Get()
   SubName = string
   IF (Nmod > 1.AND.SubName /= ModuleNames(Nmod)) CALL Error(15)  ! pass 2 /= pass1 name

   IF (Debug) THEN
      WRITE (9,'(I3,1X,A)') Nmod,Modulenames(Nmod)
      WRITE (*,'(I3,1X,A)') Nmod,Modulenames(Nmod)
   END IF

   CALL MoveToNext('(')
   c = c+1                        ! past (
   CALL BeginString()

   IF (Nmod == 1) THEN
      Fline = 'MODULE ' // String(1:n)   ! NAME_1
      CALL Push(-C_subroutine)       ! guaranteed not name() {
   ELSE
      Fline = 'SUBROUTINE ' // String(1:n) // '('
      f = 11+n+2
      CALL arglist_proc()          ! process/copy args to Fline
      c = Cend+1

      CALL Push(-C_subroutine)         ! expecting {
   END IF

ELSE                         ! syn:      sub(...);  is a subroutine call
                             ! syn: void sub(...);  ignore it
   c1 = c
   f1 = f                    ! possible IF (....) already formatted
   c = 1
   n = Getkey()        ! confirm we got here direct or from if(...)
   IF (n /= C_void.AND.n /= C_subroutine) THEN   ! probably from  if(...) sub1();
      DO c = c1,3,-1          ! backup to end of if(..) sub1();
         IF (CC(c) == ')') EXIT
      END DO
      c = c+1                ! past )
   ELSE
      c = 1                  ! get SubName directly
   END IF
   nextkey = 0

   nc = Get()
   IF (string == 'void') RETURN   ! ignore prototype
   SubName = string
   f = 1 + Indentcol
   IF (f1 > f) f = f1
   Fline(f:f+nc+4) = 'CALL ' // SubName(1:nc)
   f = f+nc+5
   CALL BeginString()
   IF (CC(c) == '(') THEN         ! guaranteed ?
      FF(f) = '('
      f = f+1
      c = c+1
   END IF
   IF (Use_pointer2) THEN
      CALL CopyToCend()           ! copy (arg,arg, .. as is
   ELSE
      CALL sublist_proc()         ! format extra args for **args
   END IF

   nid = GetIndex(SubName)
   IF (nid == 0) THEN
      nid = GetNextIndex()
      v.name(nid) = SubName
      v.nc(nid) = -nc             ! mark as un-referenced
      v.declare(nid) = SUB
      v.locate(nid) = EXTERN
   END IF

   Key = 99

END IF

90 FORMAT (99A)
END SUBROUTINE subroutine_proc

! -----------------------------------------------------------
SUBROUTINE sublist_proc ()  ! called by subroutine_proc to gen output
                            ! arglist for call sub(....)
INTEGER :: narg             ! entry with:  SubName
                            ! fline = CALL SubName( arg1
CALL BeginString()          !                       ^
ch = CC(c)
IF (ch == ')') THEN          ! no arg list  sub();
   FF(f) = ')'
   f = f+1
   c = c+1
   RETURN
END IF

narg = 0                               ! #arg in list
DO     ! process arg list

   IF (narg < MAXARG) narg = narg+1

   CALL BeginString()
   IF (CC(c) == '&') THEN
      c = c+1    ! discard, &arg1 -> arg1

   ELSE IF (CC(c) == DQUOTE) THEN    ! just copy ".." args
      FF(f) = DQUOTE
      f = f+1
      c = c+1
      CALL CopyToFirst(DQUOTE)
      GO TO 2

   ELSE IF (Cline(c:c+5) == '(void)') THEN
      c = c+6
      CALL BeginString()
      n2 = Get()
      argname = String
      Fline(f:f+n2+4) = 'LOC(' // argname(1:n2) // ')'
      f = f+n2+5
      GO TO 2
   END IF

   n2 = Get()                    ! get next argname
   argname = String
   Fline(f:f+n2-1) = argname(1:n2)
   f = f+n2
   CALL BeginString()           ! snug up  arg + nnn  syntax
   IF (CC(c) == '(') THEN
      CALL CopyToLastParen()

   ELSE IF (CC(c) /= ','.AND.CC(c) /= ')') THEN
      DO c = c,Cend
         IF (CC(c) == ','.OR.CC(c) == ')') EXIT
         IF (CC(c) == SP) CYCLE
         FF(f) = CC(c)
         f = f+1
      END DO
   END IF

   nid = GetIndex(argname)
   IF (nid > 0) THEN
      IF (v.type(nid) == TCHAR.AND.v.ptr_array(nid) &
                              .AND.v.locate(nid) /= ARG) THEN ! not ARG itself
         Fline(f:f+11+n2) = '(1)(:SIZE(' // argname(1:n2) // '))'
         f = f+12+n2
!!         Fline(f:f+12+n2+1) = '(1:LEN_TRIM(' // argname(1:n2) // '))'
!!         f = f+12+n2+2
      ELSE IF (v.declare(nid) == ARRAY2) THEN ! check adding 2 int args
         dim_s = v.dim(nid)
         IF (dim_s(1:1) /= '[') THEN      ! get **arg's dimension string
            n = LEN_TRIM(dim_s)
            Fline(f:f+n) = ','//dim_s(2:n-1)  ! sub(n,5,5
            f = f+n+1
         END IF

      ELSE IF (v.type(nid) == TCHAR.AND.v.declare(nid) == ARRAY1) THEN  ! ditto
         dim_s = v.dim(nid)
         IF (dim_s(1:1) == '['.AND.dim_s(2:2) /= ']') THEN  ! use dimensions
            n = LEN_TRIM(dim_s)
    !!        Fline(f:) = ',' // argname(1:n2) // '2,' // dim_s(2:n-1)
            Fline(f:) = ',' // argname(1:n2) // '2'
            f = LEN_TRIM(Fline) +1
         END IF

      ELSE
          ! ??
      END IF
   END IF

2  CONTINUE

   IF (CC(c) == ')') THEN
      IF (FF(f-1) == SP) f = f-1   !backup
      IF (FF(f-1) == SP) f = f-1   !backup
      FF(f) = ')'
      f = f+2
      c = c+1    ! past )
      EXIT
   ELSE IF (CC(c) == ',') THEN
      IF (FF(f-1) == SP) f = f-1   !backup
      IF (FF(f-1) == SP) f = f-1   !backup
      Fline(f:f+1) = ', '
      f = f+2
      c = c+1         ! past ,
   ELSE
      DO
         ch = CC(c)
         FF(f) = ch                 ! copy  strange arg field  x + nnn
         f = f+1
         c = c+1
         IF (c > Cend) EXIT
         IF (ch == ','.OR.ch == ')') THEN
            f = f+1
            c = c+1
            RETURN
         END IF
      END DO
   END IF
END DO
END SUBROUTINE sublist_proc

! -----------------------------------------------------------
SUBROUTINE function_proc()    ! int fun1(....)

c = 1
IF (Lstatic) n = Get()        ! discard "static" keyword as meaningless
n = Get()                     ! string = int

DO n = C_short,C_struct       ! these keys are linked with functions(
   IF (string == Keywords(n)) EXIT
END DO
Funkey = (n-C_short)+1
IF (n == C_struct) THEN
   Funkey = (C_struct-C_short)+1
   n = Get()  ! past struct name to fun name
   structname = string
END IF

CALL BeginString()
IF (CC(c) == '(') c = c+1     ! syn: double (*funname)(....)
CALL BeginString()
IF (CC(c) == '*') c = c+1     ! syn: char *Name(
n = Get()
FunName = string                 ! shud agree with GetKey name

IF (Chend == ';') THEN           ! prototype syn:  int fun1(int arg1; ...);
   nid = GetNextIndex()
   v.name(nid) = FunName
   IF (Funkey == (C_struct-C_short)+1) v.name2(nid) = structname
   v.nc(nid) = -n
   v.type(nid) = FunKey
   v.declare(nid) = FUN
   v.locate(nid) = EXTERN
   Key = 0
   RETURN
END IF

Moduletypes(Nmod) = C_function
IF (Nmod > 1.AND.FunName /= ModuleNames(Nmod)) CALL Error(15)  ! pass 2 /= pass1 name

IF (Debug) THEN
   WRITE (9,'(I3,1X,A)') Nmod,Modulenames(Nmod)
   WRITE (*,'(I3,1X,A)') Nmod,Modulenames(Nmod)
END IF

CALL MoveToNext('(')
c = c+1                      ! past (
CALL BeginString()

n = LEN_TRIM(funname)
Fline = 'FUNCTION ' // funname(1:n) // '('
f = 9+n+2

CALL arglist_proc()          ! process arg list items

Fline(f:f+18) = '  RESULT (output' // Kinds(funkey) // ')'
f = f+19

string = Types(funkey)
n = LEN_TRIM(string)

IF (FunKey == (C_char-C_short)+1) THEN
   output_s = '  CHARACTER (LEN=*) :: output_s'
ELSE IF (Funkey == (C_struct-C_short)+1) THEN
   n1 = LEN_TRIM(structname)
   output_s = '  '// string(1:n) // ' (' // structname(1:n1) // ')' &
                                        // ' :: output' // Kinds(funkey)
ELSE
   output_s = '  '// string(1:n) // ' :: output' // Kinds(funkey)
END IF

c = Cend+1

CALL Push(-C_function)         ! expecting {

90 FORMAT (99A)
END SUBROUTINE function_proc

! -----------------------------------------------------------
SUBROUTINE statement_proc()
 
! - - -  Local Declarations - - -
INTEGER :: n, k, tmp, c2
CHARACTER :: chs
LOGICAL :: array_open, array_detect, deref, point1, point2
! - - - - - -
CALL BeginString()

IF (Key < 0) THEN                      ! statement continuation record
   f = 1 + Indentcol +4                ! extra continuation indent
ELSE
   point1 = .FALSE.                    ! 1st record of statement
   point2 = .FALSE.
   deref  = .FALSE.
   IF (Cline(c:c+1) == '**') THEN
      point2 = .TRUE.
      c = c+2
   ELSE IF (CC(c) == '*') THEN
      point1 = .TRUE.
      deref  = .TRUE.    ! *x =
      c = c+1            ! past *
   END IF
END IF

c1 = c                                 ! save
f1 = f
IF (Key > 0) n1 = Get()                ! possible AssName

AssName = SP
CALL BeginString()
array_detect = .FALSE.
ass = .FALSE.
equal = .FALSE.
nid = 0                           ! reset index for assname
ch = CC(c)

IF (ch == '='.OR.ch == '['.OR.ch == '%') THEN ! syn: x=  x[  x%
   AssName = String
   ass = .TRUE.
   IF (ch == '=') equal = .TRUE.

   nid = GetIndex(AssName)        ! update identifier info for assname
!   IF (Lvoid.AND.nid == 0.AND.ch == '=') THEN   ! syn: void tmp = *a
   IF (nid == 0.AND.ch == '=') THEN   ! syn: void tmp = *a
      Lvoid = .FALSE.
      c2 = c
      c = c+1                  ! past =
      CALL BeginString()
      IF (CC(c) == '*') c = c+1
      n = Get()                ! shud be:  *y
      c = c2
      n = GetIndex(string)     ! check  y is declared

      IF (n > 0) THEN
!         IF (v.ptr_void(n)) THEN    ! set by  void *name
         IF (1==1) THEN
            nid = GetNextIndex()
            v.name(nid) = AssName   ! create AssName declaration
            v.nc(nid) = n1
            v.type(nid) = TINT
            v.declare(nid) = SCALAR  ! INTEGER :: tmp
            v.locate(nid) = LOCAL
            IF (v.declare(n) == CONST) THEN
               v.type(nid) = v.type(n)
               v.init(nid) = v.init(n)
               FF = SP                ! discard statement, declare info used
               RETURN  ! OK, declare/init  integer :: x = y
            END IF
            IF (Debug) CALL Error(32) ! WARNING, declared ='
         END IF
         GO TO 1
      END IF
   ELSE IF (nid == 0) THEN
      DO i = 1,n1
         IF (assname(i:i) == '.') GO TO 1   ! ignore for now !!
      END DO
      CALL Error(5)    ! undeclared identifier, SHUD not happen
      RETURN
   END IF

1  CONTINUE

   IF (point1) THEN
      FF(f) = '*'                 ! let pass 3 process *name
      f = f+1
   END IF
   Fline(f:f+n1-1) = AssName(1:n1)
   f = f+n1

   IF (ch == '=') THEN
      v.stored(nid) = .TRUE.          ! mark used in store
      IF (deref) v.ptr_deref(nid) = .TRUE.
      Fline(f:f+1) = ' ='
      f = f+2
      f2 = f                      ! save past =
      c = c+1                     ! past =
      IF (v.locate(nid) == ARG.AND.v.declare(nid) == ARRAY1) THEN
         v.name(nid) = AssName(1:n1) // '_'    ! array= invokes pointer arith
         v.nc(nid) = n1+1                      ! change arg name,declaration
         v.dim(nid) = '(C2F_BIGNUM)'           ! dummy arg size
         v.target(nid) = .TRUE.
         v.stored(nid) = .FALSE.               ! remove not stored status
         nid2 = GetIndex(AssName)     ! not found 1st time array= encountered
         IF (nid2 == 0) nid2 = GetNextIndex()
         v.name(nid2) = AssName(1:n1)
         v.nc(nid2) = n1
         v.type(nid2) = v.type(nid)
         v.declare(nid2) = PTR1
         v.locate(nid2) = LOCAL
         v.dim(nid2) = '(:)'
         v.ptr_array(nid2) = .TRUE.
         v.stored(nid2) = .TRUE.
         nid = nid2                      ! new nid for AssName

      ELSE IF (v.locate(nid) == LOCAL.AND.v.type(nid) == TSTRUCT &
                                     .AND.v.declare(nid) == PTR1) THEN
         c2 = c
         n = Get()
         nid2 = GetIndex(string)
         IF (nid2 > 0) THEN
            IF (v.locate(nid2) == ARG.AND.v.type(nid2) == TSTRUCT &
                                     .AND.v.declare(nid2) == SCALAR) THEN
               name = v.name(nid2)
               v.declare(nid2) = PTR1       ! was scalar?
               v.stored(nid2) = .TRUE.      ! pass3 format name_,name
               IF (FF(f-1) == SP) f = f-1  ! elim space after =
               FF(f) = '> '                ! x => y...
               f = f+2
               Fline(f:f+n1-1) = name(1:n1)
               f = f+n1
               RETURN
            ELSE
               c = c2
            END IF
         ELSE
            c = c2
         END IF
      END IF

   ELSE   ! ch = '['
      array_detect = .TRUE.    ! syn:  x[..
      CALL CopyToFirst('=')
      f = f-2
      IF (FF(f) /= SP) f = f+1
      Fline(f:f+1) = ' ='
      f = f+2
      f2 = f                      ! save past =
      ass = .TRUE.
   END IF
END IF

quote_field = .FALSE.
array_open = .FALSE.
ch = SP
pch = SP
chend = CC(Cend)

! - - - - - - - -
DO                         ! process Cline => Fline
   IF (c > Cend.OR.CC(c) == ';') EXIT
   IF (ch /= SP) pch = ch
   ch = CC(c)

   IF (ch == DQUOTE) THEN
      IF (ass.AND.v.declare(nid) == PTR1) THEN
         v.ptr_array(nid) = .TRUE.
         f = f2
         Fline(f:f+10) = '> .STRPTR.' // DQUOTE
         f = f+11
         c = c+1
         CALL CopyToFirst(DQUOTE)
         EXIT
      ELSE
         quote_field = .NOT.quote_field
      END IF
   END IF
                         ! below processing NOT "quote" field
   IF (ch == SP) THEN
      FF(f) = ch
      f = f+1
      c = c+1
      CYCLE                               ! just copy blanks
   END IF
   ch1 = CC(c+1)                          ! next char

   IF (ch == '[') array_open = .TRUE.
   IF (ch == ']') array_open = .FALSE.

   ! 1 time thru per ass=, so give it your best shot

   IF (ass) THEN          ! syn:  x=  or *x=  or x[..]=
      ass = .FALSE.
      IF (Cline(c:c+1) == '++'.OR.Cline(c:c+1) == '--') THEN
         chs = ch                      !syn: x=++y +....
         c = c+2
         n = Get()
         WRITE (2,90) blank(1:Indentcol),string(1:n),' = ',string(1:n),chs,'1'
         Fline(f:f+n-1) = string(1:n)
         f = f+n
         CALL CopyToCend()
         RETURN
      END IF

      IF (Cline(c:c+6) == 'malloc(' .OR. &
          Cline(c:c+6) == 'calloc(') THEN  ! syn:    x=malloc(...)
         f = f1                           ! discard x= output so far
         CALL malloc_proc()
         RETURN
      END IF

      IF (ch == '(') THEN           ! x = (
         c2 = c
         CALL MoveToNext(')')
         IF (CC(c-1) == '*') THEN   ! x = (int *)
            c = c+1                 ! past )
            CALL BeginString()
            IF (Cline(c:c+6) == 'malloc(' .OR. &
                Cline(c:c+6) == 'calloc(') THEN  ! syn: x=(int *)malloc
               f = f1
               CALL malloc_proc()

            ELSE IF (v.ptr_void(nid).AND.v.locate(nid) /= ARG) THEN
               IF (CC(c) == '&') c = c+1     ! syn: px=(void *)&y;
               n2 = Get()                    ! get y
               n = GetIndex(string)          ! get y identifier index, mark y used
               IF (n > 0) THEN               ! gen. x = LOC(y)
                  IF (v.type(n) == TCHAR) THEN
                     Fline(f-3:) = '(1) = LOC(' // string(1:n2) // ') ; ' // &
                       Assname(1:n1) // '(2) =  LEN_TRIM(' // string(1:n2) // ')'
                     f = LEN_TRIM(Fline)+1
                  ELSE
                     Fline(f:f+n2+4) = 'LOC(' // string(1:n2) // ')'
                     f = f+n2+5
                  END IF
               END IF

            ELSE IF (v.declare(nid) /= PTR1) THEN ! syn: x=(void *)&y;
               IF (CC(c) == '&') c = c+1
               n2 = Get()                    ! get y
               n = GetIndex(string)          ! get y identifier index, mark y used
               IF (n > 0) THEN               ! gen. x = LOC(y)
                  Fline(f:f+n2+4) = 'LOC(' // string(1:n2) // ')'
                  f = f+n2+5
               END IF

            ELSE IF (v.declare(nid) == PTR1) THEN  ! gen. x => VOID_TO_XXX(y)
               v.dim(nid) = '(:)'     ! force x(:) declaration
               n = v.type(nid)
               IF (n == 1) THEN
                  Suffix = 'INT2'
                  arg1 = 'INTEGER(2)'
               ELSE IF (n == 2) THEN
                  Suffix = 'INT'
                  arg1 = 'INTEGER'
               ELSE IF (n == 3) THEN
                  Suffix = 'INT'
                  arg1 = 'INTEGER'
               ELSE IF (n == 4) THEN
                  Suffix = 'FLT'
                  arg1 = 'REAL(4)'
               ELSE IF (n == 5) THEN
                  Suffix = 'DBLE'
                  arg1 = 'REAL(8)'
               ELSE IF (n == 6) THEN
                  Suffix = 'CHAR'
                  arg1 = 'CHARACTER'
               ELSE IF (n == 8) THEN
                  Suffix = v.name2(nid)
                  arg1 = 'TYPE (' // TRIM(Suffix) // ')'
               END IF

               DO n = 1,20
                  IF (List_Suffixes(n) == SP) EXIT   ! new type
                  IF (List_Suffixes(n) == Suffix) EXIT ! found
               END DO
               IF (n > 20) THEN
                  CALL Error(24)    ! contains > 20
                  RETURN
               END IF
               IF (List_Suffixes(n) == SP) THEN   ! enter new type
                  List_Types(n) = arg1
                  List_Suffixes(n) = Suffix
                  Nfunc = Nfunc+1
               END IF
               f = f-1
               Fline(f:) = '> VOID_TO_' // TRIM(Suffix) // HT //  &
                                         '('  // Cline(c:Cend-1) // '(1))'
               f = LEN_TRIM(Fline) +1
            END IF
            RETURN

         ELSE           ! just continue with ch = (
            c = c2
         END IF


      ELSE IF (v.locate(nid) == EXTERN) THEN   ! x=y;
         name = 'I___' // assname(1:n1)
         Fline = blank(1:Indentcol) // TRIM(name) // &
                                    ' = LOC(' // Cline(c:Cend-1) // ')'
         f = LEN_TRIM(Fline) +1

         n = GetIndex(name)
         IF (n == 0) THEN          ! new entry needed for I___name
            n = GetNextIndex()
            v.name(n) = name
            v.nc(n) = n1+4
            v.type(n) = TINT
            v.declare(n) = SCALAR
            v.locate(n) = NONE
         END IF
         RETURN

      ELSE IF (v.declare(nid) == SCALAR.AND.ch == '&') THEN   ! x=&y;
         Fline(f:) = 'LOC(' // Cline(c:Cend-1) // ')'
         f = LEN_TRIM(Fline) +1
         RETURN
      END IF

      IF (v.type(nid) == TCHAR) THEN          ! CHAR assign exec
         IF (ch >= '0'.AND.ch <= '9') THEN
            Fline(f:f+6) = 'CHAR(' // ch // ')'
            f = f+7
            c = c+1
            CYCLE
         ELSE IF (a_z(ICHAR(ch))) THEN
            c1 = c
            k = Get()
            IF (string == 'getchar') THEN
               CALL getchar_proc()
               RETURN
            END IF
            n = GetIndex(string)
            IF (n > 0) THEN
               IF (v.type(n) /= TCHAR) THEN
                  CALL Error(4)          ! cant assign non-char to char
                  RETURN

               ELSE       ! y is char
                  IF (v.name(nid) == v.name(n).AND. &  ! syn: ch = ch+
                      v.declare(nid) == SCALAR.AND.v.declare(n) == SCALAR) THEN
                     Fline(f:f+k+11) = 'CHAR(ICHAR(' // string(1:k) // ')'
                     f = f+k+12
                     CALL CopyToCend()
                     f = f-1
                     FF(f) = ')'
                     EXIT
                  END IF

                  IF (v.declare(n) == PTR1.AND.CC(c)=='[') v.ptr_array(n) =.TRUE.
                  IF (v.declare(nid) == PTR1) THEN
                     IF (v.ptr_array(n)) THEN
                        v.ptr_array(nid) = .TRUE.
                     ELSE IF (v.locate(nid) /= ARG.AND. & ! dont target ptr arg
                              v.declare(n) /= PTR1) THEN
                        v.ptr_array(nid) = .TRUE.
                        v.target(n) = .TRUE.              ! x => y
                        f = f2
                        Fline(f:f+k+11) = '> CV_TO_PTR(' // string(1:k)
                        f = f+k+12
                        CALL CopyToCend()
                        FF(f-1) = ')'
                        EXIT
                     END IF
                  END IF
               END IF
            END IF
            c = c1          ! continue at ch = a-z
         END IF

      END IF

      IF (v.declare(nid) /= PTR1.OR.deref) THEN   ! x not active pointer
         c1 = c
         n1 = Get()
         n = GetIndex(string)
         IF (n > 0) THEN
            IF (v.locate(n) == EXTERN) THEN   ! examine fun(func,arg,
               c = c1
               CALL CopyToFirst('(')          ! past fun(
               DO
                  IF (c >= Cend-1) EXIT
                  CALL BeginString()
                  c2 = c
                  n1 = Get()
                  CALL BeginString()
                  ch = CC(c)                ! , or )
                  n = GetIndex(string)
                  IF (n > 0) THEN
                     IF (v.locate(n) == EXTERN) THEN
                        Fline(f:f+3) = 'I___'
                        f = f+4
                     END IF
                  END IF
                  c = c2
                  CALL CopyToFirst(ch)       ! past , or )
                  IF (ch == ')') EXIT
               END DO

            ELSE            ! copy ch
               c = c1
               FF(f) = ch
               f = f+1
               c = c+1
               CYCLE
            END IF
         ELSE
            c = c1
         END IF
      END IF

      ! x is a non-deref pointer

      IF (ch == '&') THEN              ! syn: x=&...
         c = c+1                       ! past &
         enclosed = .FALSE.
         IF (CC(c) == '(') THEN        ! syn: x=&(
            c = c+1                    ! past (
            enclosed = .TRUE.
         END IF
         n1 = Get()                    ! get y
         n = GetIndex(string)          ! get y identifier index, mark y used
         IF (n > 0) THEN
            v.target(n) = .TRUE.       ! TARGET attribute  y
            IF (FF(f-1) == SP) f = f-1 ! snug up to =
            Fline(f:f+1) = '> '
            f = f+2

            IF (v.declare(n) == SCALAR.AND.v.member(nid)   &
                                      .AND.v.type(n) /= TSTRUCT) THEN
               Fline(f:f+n1+17) = 'CAST_TO_ARRAY_PTR(' // string(1:n1)
               f = f+n1+18
               CALL CopyToCend()
               f = f-1
               FF(f) = ')'

            ELSE IF (enclosed) THEN
               Fline(f:f+n1-1) = string(1:n1)  ! x => y
               f = f+n1
               CALL CopyToFirst(')')
               f = f-1                 ! syn: x=&(y[..]);   discard )
               FF(f) = SP
               c = Cend+1
            ELSE
               Fline(f:f+n1-1) = string(1:n1)  ! x => y
               f = f+n1
               IF (v.declare(n) == SCALAR) v.ptr_deref(nid) = .TRUE.
               CALL CopyToCend()       ! syn: x=&y; or  x=&y[..];
               IF (FF(f-2) == ']') THEN
                  f = f-2
                  Fline(f:f+3) = '+1:]'
                  f = f+4
               END IF
            END IF
         END IF

      ELSE                                ! syn: x = ?
         IF (.NOT.a_z(ICHAR(ch))) THEN    ! syn: x = 1
            f = f+1
            CALL CopyToCend()
            EXIT
         END IF

         n1 = Get()
         name = string                 ! syn: x = y
         n = GetIndex(name)

         IF (.NOT.array_detect.AND.n > 0.AND.(v.declare(n) == PTR1.OR. &
                                              v.declare(n) == ARRAY1)) THEN
            IF (FF(f-1) == SP) f = f-1  ! elim space after =
            FF(f) = '> '                ! x => y...
            f = f+2
            CALL BeginString()

            ! test for pointer arithmetic  syn: x=y+ or x=y-
            IF (CC(c) == '+'.OR.CC(c) == '-') THEN
               Fline(f:f+n1) = name(1:n1) // '['
               f = f+n1+1
               IF (CC(c) == '-') THEN
                  FF(f) = '-'
                  f = f+1
               END IF
               c = c+1                 ! past +-
               CALL BeginString()
               CALL CopyToCend()
               f = f-1
               Fline(f:f+2) = ']:]'    ! ->  +1:) by pass 3
               f = f+3
               IF (v.declare(n) == PTR1) v.ptr_array(nid) = .TRUE.

            ELSE IF (CC(c) == ';') THEN     ! syn: x = y;
               IF (v.ptr_array(n).OR.v.declare(n) == ARRAY1) &
                  v.ptr_array(nid) = .TRUE. ! mark x ptr array use
               IF (v.declare(n) == PTR1) THEN
                  v.ptr_deref(n) = .TRUE.
                  v.ptr_deref(nid) = .TRUE.    ! if y deref, then so is x
               END IF
               v.target(n) = .TRUE.      ! x=ptr1, so y is a target
               IF (v.locate(n) == GLOBAL) THEN
                  g.target(n) = .TRUE.           ! y is a global variable
                  Global_Target = .TRUE.
               END IF
               Fline(f:f+n1-1) = name(1:n1)
               f = f+n1

            ELSE
               Fline(f:f+n1-1) = name(1:n1)
               f = f+n1
            END IF

         ELSE                          ! x = ?
            Fline(f:f+n1-1) = name(1:n1)
            f = f+n1
         END IF
      END IF
      CYCLE

   ELSE IF (Cline(c:c+1) == '++'.OR.Cline(c:c+1) == '--'.OR. &
            Cline(c:c+1) == '+='.OR.Cline(c:c+1) == '-='.OR. &
            Cline(c:c+1) == '*='.OR.Cline(c:c+1) == '/=')      THEN

      IF (equal) THEN           ! syn:  x=y++
         IF (CC(c+2) == ']') THEN
            WRITE (2,90) Fline(1:f),']'
            DO n = c-1,3,-1
               IF (CC(n) == '[') EXIT
            END DO
            chs = CC(c)
            WRITE (2,90) blank(1:Indentcol), Cline(n+1:c-1),' = ',Cline(n+1:c-1),chs,'1'
            FF = SP
            EXIT
         ELSE
            WRITE (2,90) Fline(1:f)
         END IF
         FF = SP
         f = 1+Indentcol
      END IF
      chs = CC(c)        ! save arith. operator
      CALL syntax_proc(11)   ! Fline   x = x+1

      IF (v.locate(nid) == ARG.AND.v.declare(nid) == ARRAY1) THEN
         CALL Error(33)          ! code not exercised by debug.c
         AssName = String                  ! string from syntax_proc
         n1 = LEN_TRIM(String)
         v.name(nid) = String(1:n1) // '_'    ! array= invokes pointer arith
         v.nc(nid) = n1+1                      ! change arg name,declaration
         v.dim(nid) = '(C2F_BIGNUM)'           ! dummy arg size
         v.target(nid) = .TRUE.
         v.stored(nid) = .FALSE.               ! remove not stored status
         nid2 = GetIndex(AssName)     ! not found 1st time array= encountered
         IF (nid2 == 0) nid2 = GetNextIndex()
         v.name(nid2) = AssName(1:n1)
         v.nc(nid2) = n1
         v.type(nid2) = v.type(nid)
         v.declare(nid2) = PTR1
         v.locate(nid2) = LOCAL
         v.dim(nid2) = '(:)'
         v.ptr_array(nid2) = .TRUE.
         v.stored(nid2) = .TRUE.
         nid = nid2                      ! new nid for AssName
         f = f-2
         IF (chs == '+') THEN
            Fline(f:f+5) = '[1]:]'
            f = f+5
         ELSE
            Fline(f:f+5) = '[' // chs // '1]:]'
            f = f+6
         END IF
         f1 = f
         DO f = f1,3,-1
            IF (FF(f) == '=') EXIT
         END DO
         FF(f+1) = '>'
         f = f1
      END IF

   ELSE
      FF(f) = ch
      f = f+1
      c = c+1
   END IF
END DO

Key = 99
IF (CC(Cend) /= ';') THEN
   Fline(f:f+3) = '  &-'        ! statement continued
   f = f+4
   Key = -99           ! get next record
END IF

90 FORMAT (99A)
END SUBROUTINE statement_proc

! -----------------------------------------------------------
SUBROUTINE syntax_proc (ncase)    ! all cases output to Cline, except 11

! - - - arg types - - -
INTEGER :: ncase

! - - -  local declarations - - -
INTEGER :: n
! - - - - - -

SELECT CASE (ncase)
CASE (0)                 ! array[][] reorder
   c1 = c                      ! [
   k = 0                             ! reset dimension count
   openbracket = 0
   DO c = c1,Cend
      IF (CC(c) == '[') THEN
         openbracket = openbracket +1
         IF (openbracket == 1) THEN
            k = k+1                     ! count array dimensions
            IF (k > 3) EXIT
            ii(k) = c                   ! save a dimension's startindex
         END IF
      ELSE IF (CC(c) == ']') THEN
         openbracket = openbracket -1
         IF (openbracket == 0) THEN
            jj(k) = c                   ! save dimension endindex
            IF (CC(c+1) == '['.OR.Cline(c+1:c+2) == ' [') THEN
               CYCLE                    ! array continues
            ELSE
               EXIT                     ! end of array declaration
            END IF
         END IF
      END IF
   END DO
   SELECT CASE (k)
   CASE (1)
      Cline(c1:c) = Cline(ii(1):jj(1))
   CASE (2)
      Cline(c1:c) = Cline(ii(2):jj(2)) // Cline(ii(1):jj(1))
   CASE (3)
      Cline(c1:c) = Cline(ii(3):jj(3)) // Cline(ii(2):jj(2)) // Cline(ii(1):jj(1))
   CASE DEFAULT
      CALL Error(22)                  ! dimension > 3
   END SELECT
   c = c+1                            ! past last close bracket ]
   ch = ']'                           ! pch next scan


CASE (1)                      ! (cast)
   c1 = c                     ! check for function cast (int)
   c = c+1                   ! past (

   IF (CC(c) == '*') THEN     ! possible (*func)(...)
      c = c+1
      n = Get()
      IF (CC(c+1) /= '(') THEN   ! not a cast i.e.  (*v).r
         c = c1
         RETURN
      END IF
      CALL MoveToNext('(')
      c2 = c
      CALL CopyToLastParen()
      Fline = Cline(1:c1-1) // string(1:n) // Cline(c2:c) ! func(....)
      f = LEN_TRIM(Fline)+1
      Cline = Fline(1:f) // Cline(c+1:Cend)
      Cend = LEN_TRIM(Cline)
      c = f               ! resume Cline scan after FUNC(...)
      ch = CC(c)          ! pch next scan
      RETURN
   END IF

   n = GetKey()
   IF (n >= C_short.AND.n <= C_char.AND.CC(c) == ')') THEN  ! (int)
      c = c+1                ! past )
      cast = (n-C_short)+1
      Fline = Cline(1:c1-1) // fcasts(cast)    ! ex: INT(
      f = LEN_TRIM(Fline)+1
      f1 = f                               ! col. to resume Cline scan at exit
      CALL BeginString()
      IF (CC(c) == '(') THEN               ! syn: (cast) (....)
         CALL CopyToLastParen()
         FF(f) = ')'                       ! INT((....))
         c = c+1                           ! past )
      ELSE                                 ! syn: (cast) nnnn
         n = Get()
         Fline(f:f+n-1) = string(1:n)
         f = f+n
         ch = CC(c)
         IF (ch == '(') THEN               ! syn: (cast) nnnn(
            CALL CopyToLastParen()
            c = c+1
         ELSE IF (ch == '[') THEN           ! syn: (cast) nnnn[
            CALL CopyToFirst(']')
         END IF
         FF(f) = ')'
         f = f+1
      END IF
      Cline = Fline(1:f) // Cline(c:Cend)
      Cend = LEN_TRIM(Cline)
      c = f1              ! resume Cline scan after INT(
      ch = CC(c)          ! pch next scan
   ELSE
      c = c1       ! not a cast, continue scan
   END IF

CASE DEFAULT       ! >>= <<= >> <<  %  output changed cline in pass1
                   ! ++ -- += -= *=    output to fline in pass2
   c4 = c
   c5 = c4 +1                           ! past %
   IF (ncase == 3) c5 = c4+3            ! past >>= <<=
   IF (ncase == 4) c5 = c4+2            ! past >> <<
   IF (ncase == 11) c5 = c4+2           ! past ++ -- += -= *=

   c1 = 0; c7 = 0                       ! syn: ( ( xxx ) ??? ( nnn ) )
   DO c = c4-1,1,-1                     !      1 2     3 4   5     6 7
      IF (CC(c) /= SP) EXIT             ! backup until c2:c3 field end
   END DO
   c3 = c
   IF (CC(c3) == ')') THEN              ! syn: ( xxx )
      n = -1
      DO c = c3-1,1,-1
         IF (CC(c) == ')') n = n-1
         IF (CC(c) == '(') n = n+1      ! backup until ( ... ) start
         IF (n == 0) EXIT
      END DO
      c2 = c
      DO c = c2-1,1,-1
         IF (CC(c) == SP.OR.CC(c) == '(') EXIT
      END DO
   ELSE                                 ! syn: xxx
      DO c = c3-1,1,-1                  ! backup until xxx start
         ch = CC(c)
         IF (ch == SP.OR.ch == '('.OR.ch ==')'.OR.ch == '=') EXIT
      END DO
   END IF
   c2 = c+1
   IF (c2 < 1) THEN
      CALL Error(10)                    ! unrecog. syntax
      c = c4+2                          ! restore c past operation
      RETURN                            ! syn error, abandon ship
   END IF

   c = c5                               ! c5 was temp set past ??? field
   CALL BeginString()
   c5 = c                               ! final c5
   IF (CC(c) == '(') THEN               ! syn: (...)
      n = 1                             !      5   6
      DO c = c5+1,Cend
         IF (CC(c) == '(') n = n+1      ! forward until (c5:c6) end
         IF (CC(c) == ')') n = n-1
         IF (n == 0) EXIT
      END DO
      c6 = c
   ELSE                                 ! syn:  nnn
      DO c = c5+1,Cend                  !       5 6
         IF (CC(c) == SP.OR.CC(c) == ','.OR.CC(c) == ')') EXIT
      END DO
      c6 = c-1
   END IF
   IF (CC(c6) == ';') THEN
      c6 = c6-1
   ELSE
      c = c6+1
      IF (c < Cend) CALL BeginString()
   END IF
   IF (CC(c) == ')') THEN               ! syn: ( ( xxx ) >> (nn) )
      c7 = c                            !      1 2     3 4  5  6 7
      n = -1
      DO c = c7-1,2,-1
         IF (CC(c) == ')') n = n-1
         IF (CC(c) == '(') n = n+1      ! backup until ( ( xxx ) start
         IF (n == 0.OR.CC(c) == ',') EXIT  ! possible printf arg, xxx
      END DO
      IF (n /= 0) THEN                  ! closing ( not found, leave c1 = 0
         IF (CC(c) == ',') c = c+1      ! printf arg, xxx
         CALL Beginstring()             ! xxx
      ELSE                              ! found (
         c1 = c                         ! found ( ( .....
      END IF
   END IF

   IF (ncase == 11) THEN     ! ++ -- += -= *=  /= output is to pass 2 Fline

      c = c2
      n = Get()                         ! xxx
      nid = GetIndex(string)
      IF (nid == 0) THEN                ! shud not occur
         CALL Error(5)                  ! undeclared identifier
         c = c4+2                       ! past ++
         RETURN
      ELSE
         v.stored(nid) = .TRUE.
      END IF

      f = f-(c4-c2)+1                  ! backup fline to c2 minimum
      IF (f < Indentcol) f = Indentcol

      ch = CC(c4+1)
      IF (ch == '+'.OR.ch == '-') THEN  ! syn: xxx++  xxx--
         Fline(f:) = Cline(c2:c3) // ' = ' // Cline(c2:c3)
         f = LEN_TRIM(Fline) +1
         IF (ch == '+') Fline(f:f+1) = '+1'
         IF (ch == '-') Fline(f:f+1) = '-1'
         f = f+2
      ELSE                              ! syn: xxx+=nnn
         c = c5
         n = Get()           ! move c to end of nnn
         IF (CC(c5) == '('.OR.CC(c) == '['.OR.CC(c) == ';') THEN   ! use rightside as is...
            Fline(f:) = Cline(c2:c3) // ' = ' // Cline(c2:c3) &
                                 // CC(c4) // Cline(c5:c6)
         ELSE                                ! wrap rightside with  (...)
            Fline(f:) = Cline(c2:c3) // ' = ' // Cline(c2:c3) &
                                 // CC(c4) // ' ('  // Cline(c5:c6) // ')'
         END IF
         f = LEN_TRIM(Fline) +1
      END IF
      c = c6+1                          ! continue past c6
      RETURN                            ! output is to Fline

   ELSE IF (ncase == 2) THEN              ! syn:  xxx % nnn -> MOD(xxx,nnn)

      IF (CC(c6) == ')'.AND.CC(c5) /= '(') c7 = c6         !syn:   nn )
      IF (c7 /= 0.AND.c1 > 0) THEN      ! syn: ( ........)
         Fline(1:) = 'MOD' // Cline(c1:c3) // ',' // Cline(c5:c7)
         c = c7+1                       ! continue past c7
      ELSE
         Fline(1:) = 'MOD(' // Cline(c2:c3) // ',' // Cline(c5:c6) // ')'
         c = c6+1                       ! continue past c6
      END IF

   ELSE IF (ncase == 3) THEN            ! syn:  xxx>>=nnn xxx<<=nnn

      Fline(1:) = Cline(c2:c3) // ' = ISHFT(' // Cline(c2:c3) // ENQ
      f = LEN_TRIM(Fline) +1
      IF (CC(c4) == '>') THEN
         Fline(f:) = '-' // Cline(c5:c6) // ')'
      ELSE
         Fline(f:) = Cline(c5:c6) // ')'
      END IF
      c = c6+1                          ! resume past c6

   ELSE IF (ncase == 4) THEN            ! syn:   xxx>>nn   xxx<<nn

      IF (CC(c6) == ')'.AND.CC(c5) /= '(') c7 = c6         !syn:   nn )
      IF (c7 /= 0.AND.c1 > 0) THEN      ! syn: ( ........)
         IF (CC(c4) == '>'.AND.CC(c5) >= '0'.AND.CC(c5) <= '9') THEN
            Fline(1:) = 'ISHFT' // Cline(c1:c3) // ENQ // '-' // Cline(c5:c7)
         ELSE                           ! ( xxx << yyy )
            Fline(1:) = 'ISHFT' // Cline(c1:c3) // ENQ // Cline(c5:c7)
         END IF
         c = c7+1                       ! continue past ) = c7
      ELSE                              ! xxx >> nnn  xxx << nnn
         IF (CC(c4) == '>'.AND.CC(c5) >= '0'.AND.CC(c5) <= '9') THEN
            Fline(1:) = 'ISHFT(' // Cline(c2:c3) // ENQ // '-' &
                                       // Cline(c5:c6) // ')'
         ELSE
            Fline(1:) = 'ISHFT(' // Cline(c2:c3) // ENQ &
                                       // Cline(c5:c6) // ')'
         END IF
         c = c6+1                       ! continue past c6
      END IF
   END IF                               ! ENQ is char tmp replacement for ,

   ! output is to pass 1 Cline,  Fline not used
   f = LEN_TRIM(Fline)
   IF (c1 == 0) THEN
      Cline = Cline(1:c2-1) // Fline(1:f) // Cline(c:Cend)
   ELSE
      Cline = Cline(1:c1-1) // Fline(1:f) // Cline(c:Cend)
   END IF
   Cend = LEN_TRIM(Cline)
   c = f               ! resume Cline scan at f+1
   ch = CC(c)          ! pch next scan

END SELECT

END SUBROUTINE syntax_proc

! -----------------------------------------------------------
SUBROUTINE leftbrace_proc()

indentsave = Indentcol
brk_expectedsave = Brk_expected
control = Pull()

IF (control < 0) THEN         ! this { attached to -key
   CALL Push(-control)        ! prev -Key_Save now +Key_Save
   control = -control         ! +control
ELSE
   CALL Push(control)         ! reenter +Key_Save typ & col.
   Indentcol = indentsave     ! restore this {'s col.
   CALL Push(Key)             ! enter unattached { & col.
END IF

Indentcol = Indentcol +Indent    ! for all { including switch {
Brk_expected = brk_expectedsave  ! restore value at entry
Key = 99

END SUBROUTINE leftbrace_proc

! -------------------------------------------
SUBROUTINE rightbrace_proc()  !end prev key control, gen termination

INTEGER :: n, n1

control = Pull()

f = 1 + Indentcol             !set col of keyword end action

SELECT CASE (control)      !clean up + Key_Save

   CASE (C_do)
      CALL BeginString()         !check for } while

      IF (Cline(c:c+4) /= 'while'.AND.Key2 == C_while) CALL Append_Record()

      IF (Cline(c:c+4) == 'while') THEN   !final } of while Key_Save
         f = f + INDENT
         Fline(f:f+8) = 'IF (.NOT.'
         f = f+9
         c = c+5                  !past while
         CALL BeginString()
         CALL CopyToFirst(')')
         Fline(f:f+5) = ') EXIT'
         f = f+6
         WRITE (2,90) Fline(1:f)  ! output IF (..) EXIT
         FF = SP                  !clear above statement
         f = 1 + Indentcol        !get next statement col
      END IF
      Fline(f:f+5) = 'END DO'    ! for plain do;  and do; } while (..);
      f = f+6

   CASE (C_for)
      Fline(f:f+5) = 'END DO'
      f = f+6
      WRITE (2,90) Fline(1:f)
      FF = SP

   CASE (C_while)
      Fline(f:f+5) = 'END DO'
      f = f+6
      WRITE (2,90) Fline(1:f)
      FF = SP

   CASE (C_if)
      IF (Key2 /= C_else) THEN   !if Key_Save control ends
         Fline(f:f+5) = 'END IF'
         f = f+6
      ELSE
         CALL Push (control)  !push back +if Key_Save, allow else
      END IF                     !to pull it

   CASE (C_else)
      IF (Key2 /= C_else) THEN   !else Key_Save control ends
         Fline(f:f+5) = 'END IF'
         f = f+6
      ELSE
         CALL Push (control)  !push back +else Key_Save, allow else
      END IF                     !to pull it

   CASE (C_switch)
      Fline(f:f+10) = 'END SELECT '
      f = f+11
      Brk_expected = .FALSE.

   CASE (C_struct,C_union)
!!      Key = -control       ! -C_struct or -C_union
      control = 0          ! release declare_proc from creating name%name

   CASE (C_program,C_subroutine,C_function)
      WRITE (2,90)
      DO n = 1,MAXFORM        !output format stmts if any
         IF (Formats(n) == SP) EXIT
         Fline = Formats(n)
         DO f = RECSIZ,2,-1
            IF (FF(f) /= SP) EXIT   !found end of format
         END DO
         IF (FF(f) == ',') f = f-1
         IF (FF(f) == SP) f = f-1
         WRITE (2,91) n+900,' FORMAT (',Fline(1:f),')'
         FF = SP
      END DO

      IF (Nmod == 1) THEN
         WRITE (2,90) 'END MODULE '
      ELSE IF (control == C_program) THEN
         WRITE (2,90) 'END PROGRAM '
      ELSE IF (control == C_subroutine) THEN
         WRITE (2,90) 'END SUBROUTINE '
      ELSE IF (control == C_function) THEN
         WRITE (2,90) 'END FUNCTION '
      ELSE
         WRITE (2,90) 'END ??????? '
      END IF

      RETURN

   CASE DEFAULT            ! possible unknown Key_Save control for }
      IF (control /= C_leftbrace) CALL Error(2)    ! } with unknown Key_Save
!      CALL Error(2)    ! } with unknown Key_Save
END SELECT

IF (Key2 /= C_rightbrace) Key = 99    ! check for more END DO clean-up

90 FORMAT (99A)
91 FORMAT (I3,9A)
END SUBROUTINE rightbrace_proc

! -------------------------------------------
SUBROUTINE break_proc()
INTEGER :: ac

control = Pull()                  ! note: restores Indentcol of control
CALL Push(control)

ac = ABS(control)

IF (ac == C_do .OR. ac == C_for .OR. ac == C_while .OR.  &
    ac == C_if .OR. ac == C_else) THEN

   Fline(f:f+3) = 'EXIT'
   f = f+4
   Key = 99

ELSE IF (control == C_switch) THEN
   Indentcol = Indentcol +Indent     ! assume next statement = case
   Brk_expected = .FALSE.         !reset for next case

ELSE
   CALL Error(3)              !ERR.. unknown Key_Save to break
END IF

END SUBROUTINE break_proc

! -------------------------------------------
SUBROUTINE case_default_proc()

INTEGER :: n

IF (.NOT.casecase) THEN             ! not consecutive cases
   IF (Brk_expected) THEN           ! prev case received NO break;
      DO n = 1,MAXLAB               ! find empty goto label
         IF (labels(n) == SP) EXIT
      END DO
      IF (n > MAXLAB) THEN
         CALL Error(20)      !labels > MAXLAB
         STOP          !disasterous to continue
      END IF
      labels(n) = 'case label'       !enter dummy label not target of search
      caselabel = SP                 !output as a Fline after next case
      Fline(f:f+5) = 'GO TO '
      IF (n < 10) THEN
         WRITE (Fline(f+6:f+7),92) n*10   !starts at 10
         caselabel(f:f+12) = Fline(f+6:f+7) // ' CONTINUE '
      ELSE
         WRITE (Fline(f+6:f+8),93) n*10   !starts at 100
         caselabel(f:f+12) = Fline(f+6:f+8) // ' CONTINUE'
      END IF
      WRITE (2,90) Fline           !output GO TO 10   etc.

      FF = SP
      Indentcol = Indentcol -INDENT
      IF (Indentcol < 0) Indentcol = 0
      f = 1 + Indentcol
   END IF

   IF (Key == C_case) THEN
      Fline(f:f+5) = 'CASE ('
      f = f+6
   ELSE
      Fline(f:f+12) = 'CASE DEFAULT '
      f = f+13
   END IF
END IF

CALL BeginString()          !find begin of case arg
CALL CopyToFirst(':')       !copy case arg

IF (Key2 == C_case) THEN    !get ready to merge next case
   FF(f-1) = ','            !ready to add next case
   casecase = .TRUE.
   Key = 0
ELSE
   FF(f-1) = SP               ! replace :
   IF (Key /= C_default) FF(f-1) = ')'
   Indentcol = Indentcol+INDENT  !for next statement

   casecase = .FALSE.       !back to normal
END IF
Brk_expected = .TRUE.      !set false by break; or end of switch Key_Save

90 FORMAT (99A)
92 FORMAT (I2)
93 FORMAT (I3)
END SUBROUTINE case_default_proc

! -------------------------------------------
SUBROUTINE struct_union_proc()

   ! this pass2 routine is entered from many caller contexts

CHARACTER (len=MAXSTR) :: prevname, name2

chend = CC(cend)
prevname = Structname      ! for case 2 processing

IF (control == C_struct.OR.control == C_union) THEN  !  nested struct
   CALL BeginString()
   n1 = LEN_TRIM(structname)      ! master structname
   n = Get()
   name = UPPER(n)
   c = c-1
   DO
      c = c+1
      CALL BeginString()
      IF (CC(c) == ','.OR.c >= Cend) THEN
         ! nested structure master entry created, now copy its members
         n = GetIndex(name)           ! ex. name= S1_T
         DO n = n+1,MID                  ! fetch members
            IF (.NOT.v.member(n)) EXIT
            nid = GetNextIndex()         ! same as nid+1
            string = v.name(n)
            nc = ABS(v.nc(n))
            DO i = 2,nc
               IF (string(i:i) == '%') EXIT
            END DO
            IF (string(1:i-1) == structname) CYCLE  ! dont create entry for ptr to master
              !! NOT SURE ABOUT LEAVING A HOLE WITH NO INIT. HERE
            nid = GetNextIndex()
            v.name(nid) = structname(1:n1)//'%'//name2(1:n2)//string(i:nc)
            v.name2(nid) = v.name2(n)
            v.nc(nid) = -LEN_TRIM(v.name(nid))
            v.type(nid) = v.type(n)
            v.declare(nid) = v.declare(n)
            v.locate(nid) = NONE
            v.member(nid) = .TRUE.
            v.dim(nid) = v.dim(n)
            v.init(nid) = v.init(n)
         END DO
         IF (c > Cend) EXIT       ! to return
         c = c+1                     ! past ,
         CALL BeginString()
      END IF

      point1 = .FALSE.
      IF (CC(c) == '*') THEN
         point1 = .TRUE.
         c = c+1
      END IF

      n2 = Get()                     ! name2
      name2 = string
      nid = GetNextIndex()
      v.name(nid) = Structname(1:n1) // '%' // name2(1:n2)
      v.name2(nid) = name            ! nested structname
      v.nc(nid) = -LEN_TRIM(v.name(nid))
      v.type(nid) = TSTRUCT
      IF (Key == C_union) v.type(nid) = TUNION
      v.declare(nid) = SCALAR
      IF (point1) v.declare(nid) = PTR1
      v.locate(nid) = NONE                     !   struct member
      v.member(nid) = .TRUE.
      v.dim(nid) = dim_s
      CALL BeginString()
      IF (CC(c) == '[') THEN        ! process array declaration
         dim_s = '['
         k = 1
         c = c+1                         ! past [
         DO i = 2,MAXSTR
            IF (Cline(c:c+1) == '][') THEN
               dim_s(i:i) = ','
               k = k+1
               c = c+2
            ELSE IF (CC(c) == ']') THEN
               dim_s(i:i) = ')'
               c = c+1
               EXIT
            ELSE
               dim_s(i:i) = CC(c)
               c = c+1
            END IF
         END DO
         IF (k > 2) THEN
            CALL Error(11)           ! unsupported syntax
            RETURN
         END IF
         v.declare(nid) = ARRAY1-1 + k
         v.dim(nid) = dim_s
      END IF
      IF (CC(c) == ',') c = c-1      ! let , be detected next
   END DO

   RETURN
END IF

c = 1
Lstatic = .FALSE.
IF (Cline(1:6) == 'static ') THEN
   c = c+7
   Lstatic = .TRUE.
END IF

IF (Cline(c:c+6) == 'struct ') THEN
   c = c+7
ELSE IF (Cline(c:c+5) == 'union ') THEN
   c = c+6
END IF

n1 = Get()
Structname = UPPER(n1)

! - - - Create derived type template - - -
IF (c >= Cend.AND.chend /= ';') THEN  ! 1:  struct structname
   nid = GetNextIndex()
   v.name(nid) = Structname
   v.nc(nid)   = n1
   v.type(nid) = TSTRUCT
   IF (Key == C_union) v.type(nid) = TUNION
   v.declare(nid) = SCALAR
   v.locate(nid) = LOCAL
   CALL Push(-Key)   ! note: pass1 ensures NO trailing {  ala  struct name{
   RETURN
END IF

2 CONTINUE
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DO
   c = c+1
   CALL BeginString()
   IF (c > Cend.OR.CC(c) == ';') THEN
      IF (Key2 == C_leftbrace) Keys(Nrec+1) = C_data  ! syn: struct n1 n2={..}
      EXIT
   END IF

   point1 = .FALSE.
   IF (CC(c) == '*') THEN
      point1 = .TRUE.
      c = c+1
   END IF

   n = Get()                     ! name
   name = string
   nid = GetNextIndex()
   v.name(nid) = name(1:n)
   v.name2(nid) = Structname(1:n1)
   v.nc(nid) = -n
   v.type(nid) = TSTRUCT
   IF (Key == C_union) v.type(nid) = TUNION
   v.declare(nid) = SCALAR
   IF (point1) v.declare(nid) = PTR1
   v.locate(nid) = NONE                     !   struct member
   IF (v.name2(nid) /= SP) v.locate(nid) = LOCAL
   IF (Lstatic) v.locate(nid) = STATIC
   CALL BeginString()
   IF (CC(c) == '[') THEN        ! process array declaration
      dim_s = '['
      k = 1
      c = c+1                         ! past [
      DO i = 2,MAXSTR
         IF (Cline(c:c+1) == '][') THEN
            dim_s(i:i) = ','
            k = k+1
            c = c+2
         ELSE IF (CC(c) == ']') THEN
            dim_s(i:i) = ')'
            c = c+1
            EXIT
         ELSE
            dim_s(i:i) = CC(c)
            c = c+1
         END IF
      END DO
      IF (k > 3.OR.(k > 2.AND.v.locate(nid) == ARG)) THEN
         CALL Error(11)           ! unsupported syntax
         RETURN
      END IF
      v.declare(nid) = ARRAY1-1 + k
      v.dim(nid) = dim_s
      CALL BeginString()
   END IF

   ! create a instance of structname%members with name%members
   k = LEN_TRIM(name)
   n = GetIndex(structname)        ! get index location for structname used
   DO n = n+1,MID
      nc = ABS(v.nc(n))
      IF (nc == 0) EXIT            ! end of existing index entries
      string = v.name(n)
      DO i = 2,nc
         IF (string(i:i) == SP.OR.string(i:i) == '%') EXIT
      END DO
      IF (string(i:i) == SP) EXIT     ! end of associated type members
      nid = GetNextIndex()
      v.name(nid) = name(1:k)//string(i:nc)   ! name%member
      v.name2(nid) = v.name2(n)
      v.nc(nid) = -LEN_TRIM(v.name(nid))
      v.type(nid) = v.type(n)
      v.declare(nid) = v.declare(n)
      v.locate(nid) = NONE
      v.member(nid) = .TRUE.
      v.dim(nid) = v.dim(n)
      v.init(nid) = v.init(n)
      v.ptr_void(nid) = v.ptr_void(n)
   END DO
END DO            ! possible multi-declares of same structname
Key = 99
Lstatic = .FALSE.

90 FORMAT (9A)
END SUBROUTINE struct_union_proc

! -------------------------------------------
SUBROUTINE declare_proc() !C_short,C_int,C_float,C_double,C_char,C_FILE

INTEGER :: keytype, npoint

chend = CC(Cend)
IF (Key < 0) GO TO 1        ! continuation record for data initialization

IF (Key == C_long.OR.Key == C_short) THEN ! check syn:  long int  short int
   IF (Key == C_long) Key = C_int
   c1 = c
   n = Get()
   IF (string /= 'int') c = c1
END IF

! - - - - - - - - - - - - - - - - - - -  - -
DO                          ! process possible mult. names
   CALL BeginString()
   IF (c > Cend) EXIT
   point1 = .FALSE.
   point2 = .FALSE.
   point3 = .FALSE.
   IF (Cline(c:c+2) == '***') THEN   ! syn:  int ***x;
      point3 = .TRUE.
      c = c+3
   ELSE IF (Cline(c:c+1) == '**') THEN   ! syn:  int **x;
      point2 = .TRUE.
      c = c+2
   ELSE IF (CC(c) == '*') THEN
      point1 = .TRUE.
      IF (Key == C_FILE) point1 = .FALSE.   ! translate to integer
      c = c+1
   END IF
   keytype = 0
   IF (Key == C_short)  keytype = TSHORT
   IF (Key == C_int)    keytype = TINT
   IF (Key == C_float)  keytype = TFLOAT
   IF (Key == C_double) keytype = TDOUBLE
   IF (Key == C_char)   keytype = TCHAR
   IF (Key == C_FILE)   keytype = TINT

   IF (Key == C_void) THEN       ! its a struct member
      keytype = TINT
      point1 = .FALSE.           ! process as int struct member
   END IF

   n1 = Get()                    ! get var name in String

   IF (control /= C_struct.AND.control /= C_union) THEN
      nid = GetIndex(String)    ! use index entered by arglist proc if avail.
      IF (nid == 0) nid = GetNextIndex() ! or new index
      v.name(nid) = String          ! enter normal variable name
      v.nc(nid) = -n1
      v.declare(nid) = SCALAR       ! default
   ELSE                             ! struct/union in control
      nid = GetNextIndex()          ! entering record info in sequence
      k = LEN_TRIM(Structname)      ! enter  struc%var
      v.name(nid) = Structname(1:k) // '%' // String(1:n1)
      v.nc(nid) = -(k+1+n1)
      v.member(nid) = .TRUE.
      IF (Key == C_void) THEN
         v.ptr_void(nid) = .TRUE.
         v.declare(nid) = ARRAY1
         v.dim(nid) = '[2)'         ! integer(2)
      END IF
   END IF

   v.type(nid) = keytype
   IF (Lconst) v.declare(nid) = CONST

   IF (point1) THEN
      IF (v.locate(nid) == ARG) THEN
         IF (v.type(nid) /= TCHAR) THEN
            v.declare(nid) = PTR1
            v.dim(nid) = '(*)'
         ELSE    ! char *p
            v.declare(nid) = SCALAR
            v.dim(nid) = '[]'
         END IF
      ELSE
         v.declare(nid) = PTR1
         v.dim(nid) = '(:)'
      END IF

   ELSE IF (point2) THEN
      IF (Use_pointer2) THEN
         v.declare(nid) = PTR2
         v.dim(nid) = '(:)'
         IF (v.locate(nid) == ARG) v.dim(nid) = '(*)'
      ELSE
         v.declare(nid) = ARRAY2
         v.dim(nid) = '(:,:)'
         IF (v.locate(nid) == ARG) v.dim(nid) = '(*,*)'
      END IF

   ELSE IF (point3) THEN
      v.declare(nid) = ARRAY3
      v.dim(nid) = '(:,:,:)'
      IF (v.locate(nid) == ARG) THEN
         CALL Error(11)             ! unsupported syntax
         RETURN
      END IF
   END IF

   IF (v.locate(nid) /= ARG) THEN
      v.locate(nid) = LOCAL
      IF (Lstatic) v.locate(nid) = STATIC
      IF (control == C_struct.OR.control == C_union) v.locate(nid) = NONE
   END IF

   CALL BeginString()
   IF (CC(c) == '('.AND.chend == ';'.OR.Cline(c:c+1) == '()') THEN
      v.declare(nid) = FUN                ! function ref. used in module
      v.locate(nid) = EXTERN
      c = c+2
      CALL BeginString()
   ELSE IF (Cline(c:c+1) == '[]') THEN
      IF (Lconst) THEN
         v.declare(nid) = CONST
      ELSE
         v.declare(nid) = ARRAY1
         IF (.NOT.point1.AND.keytype == TCHAR) v.declare(nid) = SCALAR
      END IF
      v.dim(nid) = '[]'
      c = c+2
      CALL BeginString()
      IF (CC(c) == '='.AND.c < Cend) THEN   ! syn: char k[]={"12345"};
         c = c+1
         CALL BeginString()
         v.init(nid) = Cline(c:Cend)
         c = Cend+1
      END IF

   ELSE IF (CC(c) == '[') THEN        ! process array declaration
      dim_s = '['
      k = 1
      c = c+1                         ! past [
      DO i = 2,MAXSTR
         IF (Cline(c:c+1) == '][') THEN
            dim_s(i:i) = ','
            k = k+1
            c = c+2
         ELSE IF (CC(c) == ']') THEN
            dim_s(i:i) = ')'
            c = c+1
            EXIT
         ELSE
            dim_s(i:i) = CC(c)
            c = c+1
         END IF
      END DO
      IF (k > 3.OR.(k > 2.AND.v.locate(nid) == ARG)) THEN
         CALL Error(11)           ! unsupported syntax
         RETURN
      END IF

      IF (v.declare(nid) /= CONST) v.declare(nid) = ARRAY1-1 + k
      IF (v.type(nid) == TCHAR) THEN
         IF (v.declare(nid) == ARRAY1.AND..NOT.point1) v.declare(nid) = SCALAR
         IF (v.declare(nid) == ARRAY2) v.declare(nid) = ARRAY1
      END IF
      v.dim(nid) = dim_s
      CALL BeginString()
   END IF

   IF (CC(c) == '=') THEN
      IF (v.declare(nid) == PTR1) THEN  ! syn: char *msg = {"123"};
          v.declare(nid) = SCALAR
          v.dim(nid) = SP
      END IF

      IF (c == Cend) THEN
         v.init(nid) = '{}'                ! signal pass3 output
         Keys(Nrec+1) = C_data             ! signal pass2 to call data_proc
         EXIT                              ! for next record
      END IF
      c = c+1                              ! past =
      CALL BeginString()
      IF (CC(c) == '{') THEN
         c = c+1
         CALL BeginString()
      END IF
      c2 = c
      c3 = Cend
      IF (CC(c3) == ';') c3 = c3-1
      IF (CC(c3) == '}') c3 = c3-1
      DO c = c,Cend
         IF (CC(c) == ','.OR.CC(c) == '}') EXIT
      END DO
      IF (c > c3) THEN
         IF (CC(c3) == ';') c3 = c3-1
         v.init(nid) = Cline(c2:c3)    ! save init. string
         EXIT
      ELSE
         v.init(nid) = Cline(c2:c-1)       ! save init. string "123"
      END IF
   END IF

   IF (CC(c) == ';') EXIT
   c = c+1                            ! go process next var,
END DO

1 CONTINUE

Key = 99

90 FORMAT (9A)
END SUBROUTINE declare_proc

! -------------------------------------------
SUBROUTINE data_proc()              ! assumes nid is valid for data

INTEGER,SAVE :: f, nrecords, length
INTEGER :: delim(99), bend
LOGICAL :: float, float1
LOGICAL,SAVE :: struct = .FALSE.
CHARACTER :: id

c = 1
IF (f < 1) f = 1

CALL BeginString()    ! {
ch = CC(c)
c1 = c

IF (struct) THEN              ! assume continuation for  name={...
   n = LEN_TRIM(Bigline)
   Bigline = Bigline(1:n) // Cline(1:Cend)     ! accum. continuation
   Cend = n + Cend
   IF (Bigline(Cend-1:Cend) /= '};') RETURN ! get continuation
   Cline = Bigline(1:Cend)

ELSE IF (a_z(ICHAR(ch))) THEN
   n = Get()
   nid = GetIndex(string)             ! get nid for name
   Bigline = Cline                       ! initialize reprocessing record
   struct = .TRUE.
   IF (Cline(Cend-1:Cend) /= '};') RETURN
ELSE
   struct = .FALSE.      ! use current nid    syn:  {..
END IF

1 CONTINUE           ! processing name={...} from entry record
IF (struct) THEN
   nid = nid+1                        ! next nid item
   IF (.NOT.v.member(nid)) THEN
      struct = .FALSE.                ! assume {...} init completed
      RETURN
   END IF
   IF (v.type(nid) == TSTRUCT) nid = nid+1  ! struct type nid, no init..
   delim = 0
   i = 0
   bend = LEN_TRIM(Bigline)
   DO n = 1,bend
      IF (Bigline(n:n) == ','.OR.Bigline(n:n) == '{'.OR.Bigline(n:n) == '}') THEN
         i = i+1
         delim(i) = n
      END IF
   END DO

   arg1 = v.name(nid)                 ! m%item
   n = 1
   dim_s = v.dim(nid)
   IF (dim_s /= SP) THEN         ! get #items
      k = LEN_TRIM(dim_s)
      READ (dim_s(2:k-1),'(2I)') i,j
      n = i
      IF (j /= 0) n = i*j
   END IF
   k = LEN_TRIM(arg1)
   Cline = arg1(1:k) // '=' // Bigline(delim(1):delim(n+1)-1) // '};'
   Cend = LEN_TRIM(Cline)
   Bigline = '{' // Bigline(delim(n+1)+1:bend)  ! to be processed next member
   DO c = 1,Cend
      IF (CC(c) == '{') EXIT    ! relocate { in new cline for below testing
   END DO
END IF

IF (CC(c) == '}') THEN
   IF (Fline2 /= SP) THEN
      WRITE (23,90) Fline2(1:f)   ! output last "record"
      nrecords = nrecords+1
   END IF
   GO TO 2
END IF

IF (CC(c) == '{') THEN
   nrecords = 0
   length = 0
   Fline2 = SP
   f = 1
   OPEN (23,FILE='F\C2F.TMP')
   IF (c == Cend) RETURN            ! syn:  {
   c = c+1

   float = .FALSE.
   DO n = c,Cend
      IF (CC(n) == '.') float = .TRUE.     ! all output items must have .
      IF (float.OR.CC(n) == '}') EXIT
   END DO
END IF

DO
   CALL BeginString()
   IF (c >= Cend) EXIT
   IF (CC(c) == DQUOTE) THEN         ! processing character string record
      c = c+1
      Fline2(f:f) = DQUOTE
      f = f+1
      IF (f > 2) THEN                ! dont allow illegal access test below
         IF (Fline2(f-1:f-1) == DQUOTE) THEN
            f = f-1
            Fline2(f:f) = SP         ! dont allow "" in output
         END IF
      END IF
      DO c = c,Cend
         Fline2(f:f) = CC(c)
         f = f+1
         IF (CC(c) == DQUOTE) EXIT
      END DO
      c = c+1                         ! past "
      CALL BeginString()
      IF (CC(c) == ','.OR.CC(c) == '}') THEN
         f = f-1
         WRITE (23,90) Fline2(1:f)
         nrecords = nrecords +1
         IF (f-2 > length) length = f-2  ! "  "  not part of length
         Fline2 = SP
         f = 1
         IF (CC(c) == '}') GO TO 2
      END IF

   ELSE                ! processing numeric string record
      c1 = c
      float1 = .FALSE.
      DO c = c,Cend
         IF (CC(c) == '.') float1 = .TRUE.    ! this item has a .
         IF (CC(c) == ','.OR.CC(c) == '}') EXIT
      END DO
      IF (float.AND..NOT.float1) THEN
         WRITE (23,90) Cline(c1:c-1),'.D0'   ! add .d0 to integer field
      ELSE IF (float.AND.float1) THEN
         WRITE (23,90) Cline(c1:c-1),'D0'    ! add d0 to field with decimal
      ELSE                                   ! add nothing for ALL integers
         WRITE (23,90) Cline(c1:c-1)         ! note this also works for c = Cend+1
      END IF
      nrecords = nrecords +1
   END IF

   IF (CC(c) == '}') EXIT            ! end of data init records
   c = c+1                           ! past ,
END DO

IF (CC(Cend) /= ';') THEN            ! continue until end of data {...};
   Keys(Nrec+1) = C_data
   RETURN
END IF

! end of {...} pre-processing records to temp file 23
! now finalize format of declaration records in file 22
2 CONTINUE

IF (length > 0) THEN                ! assume CHARACTER set largest length
   IF (length < 10) THEN
      WRITE (string,'(I1)') length
   ELSE IF (length < 100) THEN
      WRITE (string,'(I2)') length
   ELSE
      WRITE (string,'(I3)') length
   END IF
   n = LEN_TRIM(string)
   dim_s = v.dim(nid)
   n1 = LEN_TRIM(dim_s)
   IF (nrecords == 1) THEN
      v.dim(nid) = string
   ELSE
      v.dim(nid) = dim_s(1:n1) // string(1:n)
   END IF
END IF

name = v.name(nid)
n1 = LEN_TRIM(name)

CALL fline2_proc(nid)     ! generate declare using length in v.dim(nid)
f = LEN_TRIM(Fline2) +1
Fline2(f:f+n1+3) = ' :: ' // name(1:n1)
f = f+n1+4

dim_s = v.dim(nid)
IF (dim_s /= SP) THEN     ! format array spec
   IF (dim_s(1:1) >= '1'.AND.dim_s(1:1) <= '9') THEN

        ! do nothing, dim_s winds up as char length

   ELSE IF (dim_s(1:2) == '[]') THEN        ! syn: int k[]={1,2,3};
      IF (nrecords < 10) THEN
         WRITE (Fline2(f:f+2),'(A,I1,A)') '(',nrecords,')'
         f = f+3
      ELSE
         WRITE (Fline2(f:f+3),'(A,I2,A)') '(',nrecords,')'
         f = f+4
      END IF
   ELSE
      DO i = 2,7
         IF (dim_s(i:i) == ')'.OR.dim_s(i:i) == ']') EXIT
      END DO
      n = i
      Fline2(f:f+n-1) = '(' // dim_s(2:i-1) // ')'
      f = f+n
   END IF
END IF

IF (v.declare(nid) == CONST.OR.v.locate(nid) == STATIC.OR.nmod == 1) THEN ! output declaration
   IF (v.declare(nid) == ARRAY2.OR.v.declare(nid) == ARRAY3) THEN
      Fline2(f:f+12) = ' = RESHAPE((/'
      f = f+13
   ELSE IF (v.dim(nid) /= SP) THEN
      Fline2(f:f+3)  = '= (/'
      f = f+4
   ELSE
      Fline2(f:f+1)  = '= '
      f = f+2
   END IF
   id = SP
ELSE                          ! output n= data initialization after begin

   IF (v.locate(nid) /= NONE) WRITE (22,90) Fline2(1:f)

   IF (v.declare(nid) == ARRAY2.OR.v.declare(nid) == ARRAY3) THEN
      Fline2 = HT // '  ' // name(1:n1) // ' = RESHAPE((/'
      f = 3+n1+14
   ELSE IF (v.dim(nid) /= SP) THEN
      Fline2 = HT // '  ' // name(1:n1) // ' = (/'
      f = 3+n1+6
   ELSE
      Fline2 = HT // '  ' // name(1:n1) // ' = '
      f = 3+n1+4
   END IF
   id = HT
END IF
REWIND (23)

DO n = 1,nrecords
   READ (23,90,END=823) Fline        ! add identifier's data records to file 22
   k = LEN_TRIM(Fline)
   IF (f > 80.OR.k > 20) THEN        ! output existing formatted record
      WRITE (22,90) Fline2(1:f),' &'
      Fline2 = id
      f = 2
   END IF

   IF (length > 0) THEN       ! assume character records
      Fline(k:k) = SP         ! remove ending" allowing length expansion
      Fline = QUOTE//Fline(2:length+1)//QUOTE
   END IF
   k = LEN_TRIM(Fline)
   Fline2(f:f+k) = Fline(1:k) // ','
   f = f+k+1

   IF (n == nrecords) THEN    ! output last record
      f = f-1
      IF (v.declare(nid) == ARRAY2.OR.v.declare(nid) == ARRAY3) THEN
         k = LEN_TRIM(dim_s)
         n1 = k-2+5+3
         Fline2(f:f+n1-1) = '/),(/' // dim_s(2:k-1) // '/))'
         f = f+n1
      ELSE IF (v.dim(nid) /= SP) THEN
         Fline2(f:f+1) = '/)'
         f = f+2
      ELSE
         IF (Fline2(f:f) == ',') Fline2(f:f) = SP
      END IF
      WRITE (22,90) Fline2(1:f)
      Fline = SP
   END IF
END DO
CLOSE (23,STATUS='DELETE')

IF (struct) GO TO 1     ! until this struct data record processed m={..}
RETURN

823 CONTINUE
WRITE (*,'(2(A,I3))') 'Unexpected EOF F\C2F.TMP reading after rec',n,' #written=',nrecords
RETURN

90 FORMAT (99A)
END SUBROUTINE data_proc

! -------------------------------------------
SUBROUTINE continue_proc()

Fline(f:f+5) = 'CYCLE '
f = f+6

Key = 99

END SUBROUTINE continue_proc

! -------------------------------------------
SUBROUTINE define_proc()      ! syn: #define PI 3.14;
                              ! just enter identifier info
nc = Get()                    ! name
name = String
nid = GetNextIndex()
v.name(nid) = String
v.nc(nid) = -nc
v.declare(nid) = CONST
v.locate(nid) = LOCAL
CALL BeginString()
c1 = Cend
IF (CC(Cend) == ';') c1 = c1-1
v.init(nid) = '=' // Cline(c:c1)
v.type(nid) = TINT
DO n = c,c1
   IF (CC(n) == '.') THEN
      v.init(nid) = '=' // Cline(c:c1) // 'd0'
      v.type(nid) = TDOUBLE
      EXIT
   ELSE IF (CC(n) == 'x') THEN
      v.init(nid) = '=#' // Cline(n+1:c1)
      EXIT
   ELSE IF (CC(n) == DQUOTE) THEN
      v.init(nid) = '=' // Cline(n:c1)
      v.type(nid) = TCHAR
      EXIT
   END IF
END DO
c = Cend+1

END SUBROUTINE define_proc

! -------------------------------------------
SUBROUTINE do_for_proc()    ! C_do, C_for

INTEGER :: n
LOGICAL :: do_char
CHARACTER :: ch, ch2

CALL Push(-Key)                   ! save prev indentcol

Fline(f:f+2) = 'DO '
f = f+3
IF (Key == C_do) GO TO 1

do_char = .FALSE.                ! assume integer do loop
CALL BeginString()
c = c+1                                ! past (
n = Get()
arg1 = string
nid = GetIndex(arg1)
IF (nid == 0) THEN
   CALL Error(5)                  ! undeclared identifier
   RETURN
ELSE
   v.stored(nid) = .TRUE.   ! mark used in store
   IF (v.type(nid) == TCHAR) THEN
      do_char = .TRUE.
      arg1 = 'I_' // arg1
      n = n+2
      nid2 = GetIndex(arg1)
      IF (nid2 == 0) THEN               ! declare do integer
         nid2 = GetNextIndex()
         v.name(nid2) = arg1
         v.nc(nid2) = n
         v.type(nid2) = TINT
         v.declare(nid2) = SCALAR
         v.locate(nid2) = LOCAL
      END IF
   END IF
END IF
Fline(f:f+n-1) = arg1(1:n)
f = f+n
Fline(f:f+2) = ' = '
f = f+3                             ! DO i =
CALL BeginString()


IF (CC(c) /= '=') THEN              ! syn:  e1 contains no =
   Fline(f:f+n-1) = arg1(1:n)
   f = f+n
   IF (Cline(c:c+2) == '--;') THEN
      Fline(f:f+1) = '-1'
      f = f+2
      c = c+3                       ! past ;
   ELSE IF (Cline(c:c+2) == '++;') THEN
      Fline(f:f+1) = '+1'
      f = f+2
      c = c+3                       ! past ;
   ELSE
      c = c+1                       ! past ;
   END IF
ELSE
   c = c+1                          ! past =
   CALL BeginString()
   ch = CC(c)
   IF (do_char.AND.(ch < '0'.OR.ch > '9')) THEN
      Fline(f:f+5) = 'ICHAR('
      f = f+6
      CALL CopyToFirst(';')            ! past ;
      f = f-1                          ! remove ;
      FF(f) = ')'
      f = f+1
   ELSE
      CALL CopyToFirst(';')            ! past ;
      f = f-1                          ! remove ;
   END IF
END IF
FF(f) = ','                         ! DO i=n,
f = f+1

DO c = c+1,Cend                     ! skip repeat of do var.
   ch = CC(c)
   IF (ch == '<'.OR.ch == '>'.OR.ch == '=') EXIT
END DO
c = c+1                             ! past <>
ch2 = CC(c)
IF (ch2 == '=')  c = c+1            ! past =
CALL BeginString()
IF (do_char.AND.(CC(c) < '0'.OR.CC(c) > '9')) THEN
   Fline(f:f+5) = 'ICHAR('
   f = f+6
   CALL CopyToFirst(';')
   f = f-1                          ! remove ;
   FF(f) = ')'
   f = f+1                          ! DO Ic=ICHAR(c1)
ELSE
   CALL CopyToFirst(';')            ! DO i=n1,n2;
   f = f-1                          ! remove ;
END IF

IF (ch == '<'.AND.ch2 /= '=') THEN
   Fline(f:f+1) = '-1'              ! DO i=0,N-1
   f = f+2
ELSE IF (ch == '>'.AND.ch2 /= '=') THEN
   Fline(f:f+1) = '+1'              ! DO i=N,0+1
   f = f+2
END IF

n = Get()                           ! discard repeated var
CALL BeginString()
IF (CC(c) == '+') THEN
   c = c+1                          ! past +
   IF (CC(c) == '=') THEN           ! if ++  adding ,+1 not necessary
      c = c+1                       ! past =
      n = Get()
      Fline(f:f+n) = ',' // String(1:n)  ! do i = n1,n2,n3
      f = f+n+1
   END IF
ELSE IF (CC(c) == '-') THEN
   Fline(f:f+1) = ',-'
   f = f+2
   c = c+1                          ! past -
   IF (CC(c) == '-') THEN           ! --
      FF(f) = '1'                   ! do i = n1,n2,-1
      f = f+1
   ELSE
      c = c+1                       ! past =
      n = Get()
      Fline(f:f+n-1) = String(1:n)  ! do i = n1,n2,-n3
      f = f+n
   END IF
ELSE IF (CC(c) == '=') THEN         ! syn: for(..; ..; count=count+-value
   c = c+1                          ! past =
   n = Get()                        ! past repeat of assumed count string
   FF(f) = ','
   f = f+1
   CALL BeginString()
   IF (CC(c) == '+') c = c+1        ! past +
   CALL CopyToFirst(')')            ! do i = n1,n2,count  or -count
   FF(f-1) = SP                     ! remove )
ELSE
   CALL Error(10)   ! unrec.syntax
END IF

1 CONTINUE

control = -Key
IF (Key2 /= C_leftbrace) Indentcol = Indentcol + INDENT

WRITE (2,90) Fline(1:f)

IF (do_char) THEN            ! output do statement and evaluated do_char
   n = LEN_TRIM(arg1)             ! includes I_string
   WRITE (2,90) blank(1:Indentcol), arg1(3:n),' = CHAR(',arg1(1:n),')'
END IF
Key = 0             ! signal fline has been output

90 FORMAT (9A)
92 FORMAT (I2)
93 FORMAT (I3)
END SUBROUTINE do_for_proc

! -------------------------------------------
SUBROUTINE while_proc()

IF (Key == C_while) THEN
   CALL Push(-C_while)
   Fline(f:f+8) = 'DO WHILE '
   f = f+9
END IF

CALL BeginString()
CALL CopyToCend()

control = -C_while
IF (Key2 /= C_leftbrace) Indentcol = Indentcol +INDENT

END SUBROUTINE while_proc

! -------------------------------------------
SUBROUTINE label_proc()

INTEGER :: n

Label = Cline(1:Cend-1)      !get label minus :
DO n = 1,MAXLAB
   IF (Label == Labels(n).OR.SP == Labels(n)) EXIT
END DO
IF (n > MAXLAB) THEN
   CALL Error(20)
   STOP        !disasterous to continue
END IF
IF (Labels(n) == SP) Labels(n) = Label  !enter new label
IF (n < 10) THEN
   WRITE (Fline(f:f+1),92) n*10     !start with 10, then 20,30,..300
   f = f+2
ELSE
   WRITE (Fline(f:f+2),93) n*10
   f = f+3
END IF
Fline(f:f+9) = ' CONTINUE '
f = f+10

92 FORMAT (I2)
93 FORMAT (I3)
END SUBROUTINE label_proc

! -------------------------------------------
SUBROUTINE exit_proc()

Fline(f:f+4) = 'STOP '
f = f+5
CALL BeginString()       !find begin of 1st variable name
FF(f) = QUOTE
f = f+1
CALL CopyToFirst(';')    !copy stop msg
FF(f-1) = QUOTE          !delete ;

Key = 99

END SUBROUTINE exit_proc


! -------------------------------------------
SUBROUTINE goto_proc()

INTEGER :: k, n   !local

Fline(f:f+5) = 'GO TO '
f = f+6

CALL BeginString()
Label = SP
k = 1
DO c = c,RECSIZ      !get goto label
   IF (CC(c) == ';') EXIT
   Label(k:k) = CC(c)
   k = k+1
   IF (k > 20) EXIT
END DO

DO n = 1,MAXLAB       !find it or if new label
   IF (Labels(n) == Label.OR.Labels(n) == SP) EXIT
END DO

IF (n < MAXLAB) THEN
   IF (labels(n) == SP) labels(n) = Label  !enter new label
   IF (n < 10) THEN
      WRITE (Fline(f:f+1),'(I2)') n*10     ! GO TO 10
      f = f+3
   ELSE
      WRITE (Fline(f:f+2),'(I3)') n*10     ! GO TO 100
      f = f+4
   END IF
ELSE
   CALL Error(20)      !labels > MAXLAB
   STOP          !disasterous to continue
END IF

Key = Pull()              !restore block control Key
CALL Push(Key)            !save

Key = ABS(Key)
IF (Key == C_if .OR. Key == C_else) Key = 99

END SUBROUTINE goto_proc

! -------------------------------------------
SUBROUTINE if_proc()          !Key = C_if

INTEGER :: n, n1

CALL Push(-C_if)            ! push -Key
Fline(f:f+2) = 'IF '
f = f+3

IF (chend == ')'.AND.chend2 == ';'.AND.Key3 /= C_else.AND. &
   ((Key2 >= C_break.AND.Key2 <= 99).OR.Key2 == C_subroutine)) THEN ! append next record
   CALL BeginString()               ! to  if (
   IF (use_C2F_status) THEN
      use_C2F_status = .FALSE.
      Fline(f:f+17) = '(C2F_status /= 0) '   ! if (not allocated) then
      f = f+18
      CALL MoveToNext(')')
   ELSE
      CALL CopyToLastParen()          ! IF (...)
      f = f+1
   END IF
   CALL Append_Record()
   c1 = c
   NextKey = GetKey()               ! appended record key
   IF (Nextkey == 99) c = c1
   RETURN                           ! let nextkey process statement
END IF

CALL BeginString()            ! to  if (

IF (chend == ';') THEN
   IF (CC(c) == '(') THEN
      IF (use_C2F_status) THEN
         use_C2F_status = .FALSE.
         Fline(f:f+17) = '(C2f_status /= 0) '   ! if (not allocated) then
         f = f+18
         CALL MoveToNext(')')
      ELSE
         CALL CopyToLastParen
         f = f+1
      END IF
      c = c+1                 ! past last )
   END IF
   CALL BeginString()
   NextKey = GetKey()         ! let nextkey decipher   ....;

ELSE IF (chend == ')') THEN  ! if (..)

   CALL CopyToCend()

   IF (Key2 == C_leftbrace.OR.Key2 == C_if.OR.Key3 == C_else.OR. &
      (Key2 == C_return.AND.ModuleTypes(Nmod) /= C_program)) THEN
      Fline(f:f+5)   = ' THEN '
      f = f+6

      IF (Key2 /= C_leftbrace) THEN
         Indentcol = Indentcol+INDENT   ! no { to indent next
      END IF
   ELSE
      Fline(f:f+3) = '  &-'
      f = f+4
      IF (Key2 /= 99) control = Pull() ! next not x = y;
   END IF                              ! so release control

ELSE
   CALL Error(1)        ! if/else syntax
END IF

END SUBROUTINE if_proc

! -------------------------------------------
SUBROUTINE else_proc()   ! C_else

INTEGER :: n, c1
LOGICAL :: else_if

else_if = .FALSE.
control = ABS(Pull())
IF (control /= C_if .AND. control /= C_else)  CALL Error(5)

CALL Push(-C_else)           !set -else Key_Save
Fline(f:f+4) = 'ELSE '
f = f+5

IF (Cend == c-1) THEN               !not else if
   chend = SP
ELSE
   CALL BeginString()
   IF (Cline(c:c+1) == 'if') THEN
      else_if = .TRUE.
      Fline(f:f+2) = 'IF '
      f = f+3
      c = c+2
      CALL BeginString()
   END IF
END IF
 
IF (chend == SP) THEN                 ! syn:  else  blank
   Indentcol = Indentcol +INDENT

ELSE IF (chend == ';') THEN           ! syn:  else x=y;
   WRITE (2,90) Fline
   FF = SP
   f = 1 + Indentcol +INDENT
   CALL BeginString()
   c1 = c                   ! save
   n = Get()                ! start decipher of  ....;

   DO NextKey = C_break,MAXKEY   !check for allowed NextKey;
      IF (string(1:n) == Keywords(NextKey)) EXIT
   END DO
   IF (NextKey > MAXKEY) NextKey = 99   ! let statement_proc decode   x=y;
   IF (NextKey /= 99) THEN
      control = Pull()      ! pull -if -else control
   ELSE
      c = c1                ! restore c for statement_proc
   END IF

ELSE IF (chend == ')') THEN
   CALL CopyToLastParen()
   IF (else_if) THEN
      Fline(f:f+5) = ' THEN '
      f = f+6
   END IF
   Indentcol = Indentcol +INDENT
END IF

90 FORMAT (99A)
END SUBROUTINE else_proc

! -------------------------------------------
SUBROUTINE return_proc()

IF (Moduletypes(Nmod) == C_program) THEN  ! bad syn: return in program module
   Fline(f:f+4) = 'STOP '        ! substitute
   f = f+5
   CALL BeginString()
   CALL CopyToFirst(';')      ! STOP xxx
   Key = 99
   RETURN
END IF

Fline(f:f+10) = 'output' // Kinds(FunKey) // ' = '
f = f+11
CALL BeginString()
c1 = c
n = Get()
nid = GetIndex(String)
IF (nid > 0.AND.v.type(nid) == TCHAR) THEN
   Fline(f:f+n-1+2) = String(1:n) // '_s'
   f = f+n+2
ELSE
   c = c1
   CALL CopyToFirst(';')                     ! return expression
END IF

WRITE (2,90) Fline
FF = SP
f = 1 + Indentcol
Fline(f:f+5) = 'RETURN'
WRITE (2,90) Fline
FF = SP

IF (Key2 /= C_else) THEN
   control = Pull()                             ! get current control key
   IF (control == -C_if) THEN                   ! single-statement if control
      f = 1 +Indentcol
      Fline(f:f+8) = 'END IF'
      WRITE (2,90) Fline
      FF = SP
   ELSE
      CALL Push(control)
   END IF
END IF

Key = 99

90 FORMAT (99A)
END SUBROUTINE return_proc

! -------------------------------------------
SUBROUTINE switch_proc()

CALL Push(-Key)                    ! save current indentcol, brk_expected

Fline(f:f+11) = 'SELECT CASE '
f = f+12

CALL BeginString()
CALL CopyToCend()

control = -Key                    ! control = SELECT CASE
Brk_expected = .FALSE.

END SUBROUTINE switch_proc

! -----------------------------------------------------------
SUBROUTINE enum_proc()           !  enum list {a=5,b,c};
                                 !  enum strings {d='a',e,f};
                                 !  enum strings g;
INTEGER :: ivalue                ! pass1 ensures enum terminated;
CHARACTER :: cvalue
LOGICAL :: chartype, first
CHARACTER (LEN=9) :: value

n1 = Get()
nid = GetIndex(string)           ! listname

IF (nid > 0) THEN     ! nid = oldlist exists, assume syn:  enum list name;
   n1 = Get()
   CALL BeginString()
   IF (CC(c) == '{') THEN        ! cant init existing list with {}
      CALL Error(10)             ! unrecog. syntax
      RETURN
   END IF
   n = GetNextIndex()
   v.name(n) = string
   v.nc(n) = -n1
   v.type(n) = v.type(nid)       ! int, or char
   v.declare(n) = SCALAR         ! gen.  INTEGER :: name
   v.locate(n) = LOCAL
   RETURN
END IF

CALL BeginString()
IF (CC(c) /= '{') THEN
   CALL Error(10)             ! unrecog. syntax
   RETURN
END IF
ivalue = 0
cvalue = CHAR(0)
c = c+1                       ! past {
chartype = .FALSE.            ! = integer type
DO n = c,Cend
   IF (CC(n) == ',') EXIT
   IF (CC(n) == QUOTE) chartype = .TRUE.
END DO
nid = GetNextIndex()          ! initial entry for list
IF (string /= SP) THEN        ! syn: enum list {
   v.name(nid) = string
   v.nc(nid) = -n1
   v.type(nid) = TINT
   IF (chartype) v.type(nid) = TCHAR
   v.declare(nid) = CONST
   v.locate(nid) = NONE        ! dont gen. output declaration for enum name
ELSE
   nid = nid-1                 ! will be incremented below before use
END IF
first = .TRUE.

! process parameters in newlist a,b,c,
DO
   CALL BeginString()
   IF (CC(c) == ',') c = c+1
   IF (CC(c) == '}'.OR.c >= Cend) EXIT
   n1 = Get()
   nid = nid+1
   IF (nid >= MID) THEN
      CALL Error(6)
      STOP
   END IF
   v.name(nid) = String
   v.nc(nid)   = -n1
   v.type(nid) = TINT
   IF (chartype) v.type(nid) = TCHAR
   v.declare(nid) = CONST
   v.locate(nid)  = LOCAL
   CALL Beginstring()

   IF (First.AND.CC(c) == ',') THEN   ! use integer = 0 start value
      v.init(nid) = '=0'

   ELSE IF (CC(c) == '=') THEN   ! accept new initialize values
      c = c+1                       ! past =
      CALL BeginString()

      IF (CC(c) == QUOTE) THEN      ! character syn:  enum string a='a'
         cvalue = CC(c+1)           ! get next char value
         c = c+3                    ! a='a'
         v.init(nid) = '=' // QUOTE // cvalue // QUOTE
         cvalue = CHAR(ICHAR(cvalue)+1)         ! next value
      ELSE                          ! integer syn:  enum list {a=nnn,b,c};
         c1 = c
         DO c = c1,Cend
            IF (CC(c) == ','.OR.CC(c) == '}') EXIT
            IF (CC(c) == ENQ) CC(c) = ','
         END DO
         v.init(nid) = '=' // Cline(c1:c-1)
      END IF

   ELSE  ! continue incrementing existing values

      IF (chartype) THEN
         k = Identifier(ICHAR(cvalue))
         IF (k == 1) THEN          ! char is at least acceptable for identifier
            v.init(nid) = '=' // QUOTE // cvalue // QUOTE
         ELSE
            WRITE (value,'(I3)') ICHAR(cvalue)
            IF (value(2:2) == SP) THEN
               v.init(nid) = '=CHAR(' // value(3:3) // ')'
            ELSE IF (value(1:1) == SP) THEN
               v.init(nid) = '=CHAR(' // value(2:3) // ')'
            ELSE
               v.init(nid) = '=CHAR(' // value(1:3) // ')'
            END IF
         END IF
         cvalue = CHAR(ICHAR(cvalue)+1)         ! next value

      ELSE      ! integer
         init_s = v.init(nid-1)
         n = LEN_TRIM(init_s)
         DO i = 1,n
            IF (init_s(i:i) == ENQ) init_s(i:i) = ','
         END DO
         v.init(nid) = init_s(1:n) // '+1'
      END IF
   END IF

   first = .FALSE.
END DO
Key = 99
RETURN

801 CONTINUE
CALL Error(18)      ! unrec. data init.
Key = 0

91 FORMAT (I9)
END SUBROUTINE enum_proc

! -----------------------------------------------------------
SUBROUTINE fprintf_proc()

CALL BeginString()                     ! syn: fprintf ( unit,"...");
c = c+1                                !               ^
n = Get()                              ! unit
Fline(f:f+6) = 'WRITE ('
f = f+7
Fline(f:f+n-1) = String(1:n)           ! WRITE (unit
f = f+n
c = c+1                                ! past ,
CALL BeginString()
c1 = c                                 ! "string  or argname

IF (CC(c) /= DQUOTE) THEN        ! output is binary write ?
   Fline(f:f+6) = ') '
   f = f+2
   n = Get()
   Fline(f:f+n-1) = String(1:n)
   f = f+n

ELSE                       ! syn: fprintf(unit,"string1%dstring2%d",a1,a2);
   Fline(f:f+5) = ',???) '
   f = f+6
   z = f-5                       ! save f index to ??? field
   CALL Format_Proc()
END IF

Key = 99

END SUBROUTINE fprintf_proc

! -----------------------------------------------------------
SUBROUTINE fputs_proc()

CALL BeginString()                     ! syn: fputsf ( "...",  unit);
c1 = c                                 !               ^
c = c+1
CALL MoveToNext(DQUOTE)
c2 = c                                 !  "
CALL MoveToNext(',')
c3 = c                                 !  ,
c = c+1
CALL BeginString()
c4 = c                                 !  unit
CALL MoveToNext(')')
c5 = c                                 !  )
Fline(f:f+6) = 'WRITE ('
f = f+7
n = c5-c4
Fline(f:f+n-1) = Cline(c4:c5-1)        ! WRITE (unit
f = f+n
Fline(f:f+5) = ',???) '
z = f+1                                ! mark format field
f = f+6
Cline(c3:c5) = SP                      ! blank ,unit) field
CC(c3) = ')'                           ! present "string")  ;
CALL Format_Proc()                     ! to format proc

Key = 99

END SUBROUTINE fputs_proc
! -----------------------------------------------------------
SUBROUTINE printf_proc()

CALL BeginString()                     ! printf ( "string"
c = c+1                                !         ^
CALL BeginString()                     !          ^
c1 = c                                 !          ^

IF (CC(c) == DQUOTE) THEN
   Fline(f:f+13) = 'WRITE (*,???) '
   f = f+14
   z = f-5                          ! save f index to ??? field
   CALL Format_Proc()
ELSE
   Fline(f:f+8) = 'WRITE (*,'          ! printf(fmt,arg1,arg2);
   f = f+9
   CALL CopyToFirst(',')
   f = f-1
   Fline(f:f+1) = ') '
   f = f+2
   CALL CopyToCend()
   f = f-2
   Fline(f:f+1) = SP
END IF
Key = 99

END SUBROUTINE printf_proc

! -----------------------------------------------------------
SUBROUTINE scan_proc()                 ! fscanf, scanf, sscanf

CALL BeginString()
c = c+1                                ! past (
CALL BeginString()
c1 = c

IF (Key == C_sscanf) THEN         ! syn: sscanf(&string,"format",&arg1);
   Fline(f:f+5) = 'READ ('
   f = f+6
   IF (CC(c) == '&') c = c+1           ! past & of possible &string
   CALL CopyToFirst(',')               ! READ (string,
   Fline(f:f+2) = '*) '                  ! use list directed input format
   f = f+3
   c = c+1                                ! past "
   CALL MoveToNext(',')                   ! past "format",
   c = c+1

   DO c = c,Cend                          ! copy arg1,arg2,,,)
      ch = CC(c)
      IF (ch == ')') EXIT
      IF (ch == '&') CYCLE
      FF(f) = ch
      f = f+1
   END DO
   Key = 99
   RETURN

ELSE IF (Key == C_fscanf) THEN          ! syn: fscanf(unit,"format",&arg1,&arg2);
   Fline(f:f+5) = 'READ ('
   f = f+6
   CALL CopyToFirst(',')

ELSE IF (Key == C_scanf) THEN          ! syn: scanf("format",&arg1,&arg2);
   Fline(f:f+7) = 'READ (*,'
   f = f+8
END IF

IF (CC(c) == DQUOTE) THEN
   Fline(f:f+4) = '???) '
   f = f+5
   z = f-5
   CALL Format_Proc()
ELSE
   CALL CopyToFirst(',')
   f = f-1
   Fline(f:f+1) = ') '
   f = f+2
   CALL CopyToCend()
   f = f-2
   Fline(f:f+1) = SP
END IF

Key = 99

END SUBROUTINE scan_proc

! -----------------------------------------------------------
SUBROUTINE Format_Proc()    ! called by Fprintf, Printf, Scanf proc()

! - - -  Local Declarations - - -
INTEGER :: k1, k, i, n, f1, f2
LOGICAL :: args, format_a, deref, read_ops, if_select
CHARACTER (LEN=RECSIZ) :: form
CHARACTER (LEN=MAXSTR) :: funname
! - - - - - - - - - - - - - - - -                     ???
        ! z was set by caller to start of Fline output format ^
        ! c1 was set by caller to print string " start index

read_ops = .FALSE.
IF (Key == C_scanf.OR.Key == C_fscanf.OR.Key == C_sscanf) read_ops = .TRUE.

form = SP                              ! reset format
k = 1                                  ! format index
quote_field = .FALSE.
format_a = .TRUE.

c = c1+1                               ! past "
ch = cc(c)
CALL MoveToStringPast(DQUOTE)          ! are there args?
ch = cc(c)
n = 0
args = .FALSE.                         ! reset arg name index
IF (CC(c) == ',') THEN
   c = c+1                             ! move past comma
   CALL BeginString()                  ! to arg index
   n = c                               ! = arg list start index
   args = .TRUE.
END IF

c = c1+1                               ! backup to past "
f1 = f
IF (.NOT.args.AND.(Key == C_fprintf.OR.Key == C_printf)) THEN
   FF(f) = DQUOTE
   f = f+1
END IF

   DO
      ch = CC(c)
      IF (Cline(c:c+1) == '\n') THEN
         IF (CC(c+2) == DQUOTE) THEN   ! delete trailing linefeed, fortran
            c = c+2                    ! defaults provides 1 linefeed
         ELSE
            IF (form(k:k) /= SP) k = k+1
            form(k:k) = '/'               ! insert linefeed format
            k = k+1
            c = c+2                       ! past \n
            IF (k > 2) THEN               ! delimit with , if NOT 1st item
               Fline(f:f+2) = DQUOTE // ',' // DQUOTE
               f = f+3
            END IF
         END IF

      ELSE IF (Cline(c:c+1) == '\t') THEN
         IF (form(k:k) /= SP) k = k+1
         form(k:k+2) = '3X,'           ! 3 blanks output for tab
         k = k+3
         c = c+2                       ! past \t

      ELSE IF (.NOT.args) THEN         ! no arg list
         FF(f) = ch                    ! copy everything inside quotes
         c = c+1
         f = f+1
         IF (f == f1+1) CYCLE          ! dont exit, its the 1st "
         IF (form(k:k) == SP) form(k:k) = 'A'
         IF (ch == DQUOTE) EXIT

      ELSE    ! process  "string%i string%i", arg1,arg2);

         IF (ch == DQUOTE) THEN              ! closing "
            IF (FF(f-1) == ',') THEN
               f = f-1
               FF(f) = SP
            ELSE
               FF(f) = DQUOTE
               f = f+1
            END IF
            IF (Cline(c-2:c-1) /= '\n'.AND.Key /= C_scanf) THEN
               Fline = Fline(1:z+2) // ",ADVANCE='NO'" // Fline(z+3:f)
               f = LEN_TRIM(Fline)+1
            END IF
            EXIT                            ! end of string"
         END IF

         IF (ch /= '%') THEN              ! process "string" arg
            IF (.NOT.quote_field) THEN
               FF(f) = DQUOTE
               f = f+1
               quote_field = .TRUE.
               form(k:k+1) = 'A,'         ! for character arg item
               k = k+2
            END IF
            FF(f) = ch                    ! copy string arg
            f = f+1
            c = c+1

         ELSE  ! ch = %
            IF (quote_field) THEN
               Fline(f:f+1) = DQUOTE // ','  ! term string arg with ",
               f = f+2
               quote_field = .FALSE.
            END IF

            deref = .FALSE.
            IF (Cline(n:n) == '*') THEN
               n = n+1                   ! discard 1st char arg  *x
               deref = .TRUE.
            END IF

            IF (CC(n) == SP) n = n+1
            IF (CC(n) ==  '(') THEN   ! (arg)
               openp=0
               c5=n
               c6=0
               c7=0
               DO n = n,Cend
                  ch = CC(n)
                  IF (ch =='(') openp = openp +1
                  IF (ch ==')') openp = openp -1
                  IF (openp == 0) EXIT
                  IF (ch == '?') c6 = n
                  IF (ch == ':') c7 = n
               END DO
               IF (c6 /= 0.AND.c7 /=0) THEN  ! syn: (a>b ? c : d)
                  Fline(f:) = 'MERGE(' // Cline(c6+1:c7-1) //','// & ! c,
                     Cline(c7+1:n-1) // ',' // Cline(c5+1:c6-1)  // ')'  ! d,a>b)
                  f = LEN_TRIM(Fline)+1
                  GO TO 1
               END IF
            END IF

            ! arg may still be enclosed in (arg) if not ? : case above

            funname = SP                 ! accum possible fun name below
            f2 = f
            f3 = 0
            openp = 0

            DO n = n,Cend                 ! process list arg for this %
               IF (Cline(n:n+1) == ');'.OR. &
                   Cline(n:n+2) == ') ;') EXIT
               ch = CC(n)
               IF (ch == '(') THEN
                  openp = openp+1
                  funname = Fline(f2:f-1)
                  i = LEN_TRIM(funname)
                  Fline(f2:f2+i-1) = funname
                  f = f2+i
               END IF
               IF (ch == '['.AND.f3 == 0) f3 = f
               IF (ch == ')'.AND.openp > 0) openp = openp-1
               IF (ch == ','.AND.openp == 0) EXIT
               FF(f) = ch                 ! add list arg char to Fline
               f = f+1
            END DO

            IF (f3 /= 0) THEN              ! arg[..]
               string = Fline(f2:f3-1)
               nid = GetIndex(string)
               IF (nid > 0) THEN
                  IF (v.declare(nid) == PTR1) v.ptr_array(nid) = .TRUE.
               END IF
            END IF

            IF (FF(f-1) /= ']') THEN
               String = Fline(f2:f)
               n1 = LEN_TRIM(String)
               nid = GetIndex(string)
               IF (nid > 0) THEN
                  IF (v.type(nid) == TCHAR) THEN
                     IF (v.dim(nid) == '(*)'.OR. &
                        (deref.AND.v.dim(nid) == '[]')) THEN
                        Fline(f:f+n1*3+4) = '(' // string(1:n1) // '_:' &
                                                // string(1:n1) // '_)'
                        f = f+n1*3+5
                     ELSE IF (v.ptr_array(nid)) THEN
                        Fline(f:f+11+n1) = '(1)(:SIZE(' // string(1:n1) // '))'
                        f = f+12+n1
                     END IF
                  ELSE IF (deref.AND.v.ptr_array(nid)) THEN  ! conflicting?
                     Fline(f:f+3) = HT//'(1)'  ! oh brave new world
                     f = f+4
                  END IF
               END IF
            END IF

         1  CONTINUE                      ! from proc. arg = ( ... ? ..: ..)
            IF (FF(f-1) == ',') f = f-1   ! dont gen. ,,
            FF(f) = ','
            f = f+1
            IF (CC(n) == ',') n = n+1     ! past ,
            c = c+1                       ! past %
        2   CONTINUE
            ch = CC(c)                    ! get  %_ spec ch
            c = c+1                       ! past %_ spec ch

            SELECT CASE (ch)              ! add spec to format
            CASE ('+')
               form(k:k+2) = 'SP,'        ! output + for positive args
               k = k+3
               GO TO 2                    ! continue with spec field

            CASE ('c','s')
               form(k:k+1) = 'A,'         ! for character arg item
               k = k+2
            CASE ('d','i','l','u')
               IF (read_ops) THEN
                  form(k:k+1) = 'I,'      ! for integer read arg
                  k = k+2
               ELSE
                  form(k:k+2) = 'I0,'     ! for integer write arg
                  k = k+3
               END IF
               IF (ch == 'l') c = c+1     ! skip li or lu spec

            CASE ('e')
               form(k:k+8) = '1P1E12.6,'
               k = k+9

            CASE ('f')
               form(k:k+4) = 'F0.6,'
               k = k+5

            CASE ('g')
               form(k:k+6) = 'G19.12,'
               k = k+7

            CASE ('x')
               form(k:k+4) = 'Z4.2,'      ! output at least 2 hex char in 4 char
               k = k+5

            CASE ('.')                    ! assume %.nf  spec
               i = ICHAR(CC(c)) - 48
               c = c+1                    ! past 2
               IF (CC(c) >= '0'.AND.CC(c) <= '9') THEN  ! %.nnf spec
                  i = i*10 +ICHAR(CC(c))-48
                  c = c+1                 ! past nn
               END IF
               c = c+1                    ! past f
               WRITE (form(k:k+10),'(A,I0)') 'F0.',i  ! F0.nn
               k = LEN_TRIM(form)+1
               form(k:k) = ','
               k = k+1

            CASE ('0':'9')                ! width specified
               c = c-1                    ! backup to ww.p field start
               k1 = k                     ! save data type specifier index
               k = k+1
               DO c = c,c+5
                  ch = CC(c)
                  IF (ch == 'd'.OR.ch == 'i'.OR.ch == 'l'.OR. &
                      ch == 'f'.OR.ch == 'g'.OR.ch == 's') EXIT !ww.p
                  form(k:k) = ch
                  k = k+1
               END DO
               form(k:k) = ','
               k = k+1
               IF (ch == 'd'.OR.ch == 'i'.OR.ch == 'l') THEN
                  form(k1:k1) = 'I'         ! integer field specifier
                  c = c+1
                  IF (ch == 'l'.OR.ch == 'u') c = c+1  ! skip secondary specifiers
               ELSE IF (ch == 'f') THEN
                  form(k1:k1) = 'F'         ! fixed floating field specifier
                  c = c+1
               ELSE IF (ch == 'g') THEN
                  form(k1:k1) = 'G'         ! general floating field specifier
                  c = c+1
               ELSE IF (ch == 's') THEN
                  form(k1:k1) = 'A'      ! for character(len=) arg item
                  c = c+1
               ELSE
                  CALL Error(14)          ! ww.p?  field
                  RETURN
               END IF
            CASE DEFAULT
               CALL Error(14)            !   %?
               RETURN
            END SELECT
         END IF
      END IF

      IF (c > Cend) EXIT

   END DO    ! until processed

format_a = .TRUE.
DO k = 1,RECSIZ
   ch = form(k:k)
   IF (ch == SP) EXIT
   IF (ch == 'A'.OR.ch ==',') CYCLE
   format_a = .FALSE.
   EXIT
END DO
IF (format_a) form = '99A'     ! use common format (99A)

DO n = 1,MAXFORM
   IF (form == Formats(n).OR.Formats(n) == SP) EXIT
END DO
IF (n > MAXFORM) STOP ' FORMATS > MAXFORM'   ! = 99
IF (Formats(n) == SP) Formats(n) = form      ! enter new format
WRITE (Fline(z:z+2),900) n+900               ! 901 = 1st format

900 FORMAT (I3)
901 FORMAT (132A)
END SUBROUTINE Format_Proc

! -----------------------------------------------------------
SUBROUTINE fgets_proc()

INTEGER :: n, n1, n2, n3
CHARACTER (LEN=MAXSTR) :: string1, nchar, unit

CALL BeginString()            ! syn:  fgets ( string1,nchar,unit);
c = c+1                       !        past (
n1 = Get()
string1 = String
c = c+1
n2 = Get()
nchar = String
c = c+1
n3 = Get()
unit = String
n = 6 +n3 +10 +n1 +2
Fline(f:f+n-1) = 'READ (' // unit(1:n3) // ',END=???) ' // string1(1:n1)
f = f+n

Key = 99                ! allow  if (..) fgets   cleanup
END SUBROUTINE fgets_proc

! -----------------------------------------------------------
SUBROUTINE getchar_proc()

f1 = f
DO f = f1,3,-1
   IF (FF(f) /= SP) EXIT
END DO
IF (FF(f) /= '=') THEN             ! rare stand-alone pause?   getchar();
   f = f1
   Fline(f:f+13) = "READ (*,'(A)')"
   f = f +14

ELSE                               ! syn: ch = getchar()
   f2 = f                          ! save col. =
   DO f = f2-1,1,-1
      IF (FF(f) /= SP) EXIT
   END DO
   IF (FF(f) == ']') THEN          ! backup prior to [..]
      DO f = f-1,3,-1
         IF (FF(f) == '[') EXIT
      END DO
   END IF
   DO f = f-1,2,-1
     IF (.NOT.identifier(ICHAR(FF(f-1))).AND.identifier(ICHAR(FF(f)))) EXIT
   END DO
   IF (f < 2) THEN
      CALL Error(10)
      RETURN
   ELSE
      string = Fline(f:f2-1)
      n = LEN_TRIM(string)
      Fline(f:f+15+n-1) = "READ (*,'(A)') " // string(1:n)
      f = f+15+n
   END IF
END IF

Key = 99

END SUBROUTINE getchar_proc

! -----------------------------------------------------------
SUBROUTINE fclose_proc()

Fline(f:f+5) = 'CLOSE '
f = f+6
CALL CopyToCend()

Key = 99

END SUBROUTINE fclose_proc

! -----------------------------------------------------------
SUBROUTINE put_proc()

INTEGER :: c1, c2, nch, n

IF (Key < 0) GO TO 1

CALL BeginString()            ! syn:  putc(ch,unit);  putch(ch);  puts("  ");
c = c+1                       ! past (
CALL BeginString()
c1 = c                        ! save       ^               ^           ^

n = Get()

IF (CC(c) == '(') THEN          ! its a fun(  as putch, putc arg
   nid = GetIndex(string)
   IF (nid > 0) v.declare(nid) = EXTERN   ! meaning it is used in FUN case
END IF

IF (Key == C_putch) THEN
   Fline(f:f+9) = 'WRITE (*) '
   f = f+10
   DO c = Cend,c1,-1
      IF (CC(c) == ')') EXIT        ! find last )
   END DO
   c2 = c
   n = c2-c1
   Fline(f:f+n-1) = Cline(c1:c1+n)    ! string can be expression
   f = f + n

ELSE IF (Key == C_putc) THEN
   Fline(f:f+6) = 'WRITE ('
   f = f+7
   DO c = Cend,c1,-1
      IF (CC(c) == ',') EXIT       ! find last ,
   END DO
   c2 = c
   c = c2+1                        ! past putc(......,
   n = Get()
   Fline(f:f+n-1+2) = String(1:n) // ') '  ! WRITE (unit)
   f = f+n+2
   n = c2-c1
   Fline(f:f+n-1) = Cline(c1:c1+n)    ! string can be expression
   f = f + n

ELSE IF (Key == C_puts) THEN          ! syn: puts("     "); puts( msg );
   Fline(f:f+15) = "WRITE (*,'(A)') "
   f = f+16
   SELECT CASE (CC(Cend))
   CASE (';')
      DO c = Cend,c1,-1
         IF (CC(c) == ')') EXIT        ! find last )
      END DO
      c2 = c
      n = c2-c1
      Fline(f:f+n-1) = Cline(c1:c1+n)    ! string can be expression
      f = f+n
      DO f = f,f-5,-1
         IF (FF(f) /= SP) EXIT
      END DO
      f = f+1

   CASE ('\')
      DO c = Cend,c1,-1
         IF (CC(c) == DQUOTE) EXIT        ! find last "
      END DO
      c2 = c+1
      n = c2-c1
      Fline(f:f+n-1) = Cline(c1:c1+n)    ! string can be expression
      f = f + n
      Fline(f:f+6) = ' //  &-'
      f = f+7
      Key = -C_puts                      ! continue data proc next rec.
      RETURN
   END SELECT
END IF

Key = 99                ! allow  if (..) putx   cleanup
RETURN

1 CONTINUE
Key = ABS(Key)

SELECT CASE (CC(Cend))
CASE (';')
   IF (CC(c) == DQUOTE) THEN
      CALL CopyToFirst(DQUOTE)
      CALL CopyToFirst(DQUOTE)
   ELSE
      CALL CopyToCend()
   END IF

CASE (DQUOTE,'\')
   CALL CopyToCend()
   IF (CC(Cend) == '\') THEN
      f = f-1     ! discard until more info avail.
      FF(f) = SP
   END IF
   IF (Key2 == C_data) THEN
      Fline(f:f+6) = ' //  &-'
      f = f+7
   END IF
   Key = -C_puts                 ! continue data proc next rec.
END SELECT

END SUBROUTINE put_proc

! -----------------------------------------------------------
SUBROUTINE strcat_proc()       ! concatenate

CALL BeginString()
c = c+1                       ! past (
n = Get()
Fline(f:f+n-1) = String(1:n)
f = f+n
Fline(f:f+2) = ' = '              ! x =
f = f+3

k = n +12 +n +6
Fline(f:f+k-1) = String(1:n) // '(1:LEN_TRIM(' // String(1:n) // ')) // '
f = f+k

c = c+1                           ! past ,
CALL BeginString()
IF (CC(c) == DQUOTE) THEN
   CALL CopyToFirst(')')
   FF(f-1) = SP
ELSE
   CALL CopyToCend()              ! expected );
   f = f-1
   Fline(f-1:f) = SP              ! elim     );
   f = f-2                        ! prior to );
   IF (FF(f) == SP) f = f-1
   IF (FF(f) == SP) f = f-1
END IF
Key = 99

END SUBROUTINE strcat_proc

! -----------------------------------------------------------
SUBROUTINE strcpy_proc()       ! strcpy(x,y)  x = y
                               ! or strncpy(x,y,n)  x = y  ignoring n
CALL BeginString()
c = c+1                       ! past (
c1 = c
CALL MoveToNext(',')
n = c-c1
Fline(f:f+n-1) = Cline(c1:c-1)
f = f+n
Fline(f:f+2) = ' = '              ! x =
f = f+3

c = c+1                           ! past ,
CALL BeginString()
IF (CC(c) == DQUOTE) THEN
   FF(f) = DQUOTE
   f = f+1
   c = c+1
   CALL CopyToFirst(DQUOTE)
ELSE
   IF (Key == C_strcpy) THEN
      CALL CopyToFirst(')')          ! expected );
      IF (c == Cend) FF(f-1) = SP    ! remove )  else 2nd arg fun()
   ELSE                         ! strncpy
      CALL CopyToFirst(',')          ! expected ,n);
      FF(f-1) = SP                   ! remove ,
   END IF                            ! ignore n
END IF
Key = 99

END SUBROUTINE strcpy_proc

! -----------------------------------------------------------
SUBROUTINE Push(saveKey)

INTEGER :: saveKey

IF (Nkeysave < MAXKEYSAVE) THEN
   Nkeysave = Nkeysave+1
   Key_Save(Nkeysave) = saveKey
   Col_Save(Nkeysave) = Indentcol
   Brk_Save(Nkeysave) = Brk_expected
ELSE
   CALL ERROR(19)
   STOP                    !disasterous
END IF

END SUBROUTINE Push

! -----------------------------------------------------------
INTEGER FUNCTION Pull()

IF (Nkeysave > 0) THEN
   Pull = Key_Save(Nkeysave)            ! get prev Key
   Indentcol = Col_Save(Nkeysave)       ! get prev col
   Brk_expected = Brk_Save(Nkeysave)    ! get prev Brk_expected
   IF (Indentcol < 0) Indentcol = 0     ! shud not happen
   Key_Save(Nkeysave) = 0               ! release entry
   Col_Save(Nkeysave) = 0
   Brk_Save(Nkeysave) = 0
   Nkeysave = Nkeysave-1                ! index to next non-zero entry
ELSE
   Pull = 0
END IF

END FUNCTION Pull

! -----------------------------------------------------------
SUBROUTINE Malloc_Proc()       ! called by statement_proc() with nid > 0
                               ! point1,2 (or scalar) reflects *x = situation
INTEGER :: cend1, n1           ! syn: *x = malloc(....)
LOGICAL :: point1, point2      !       x = (int *)malloc(....)
LOGICAL :: scalar              !       x[i] = malloc(...)
CHARACTER (LEN=MAXSTR) :: size !      ^
                               !       x = (struct name *)malloc(sizeof(...))
point1 = .FALSE.               ! x = (int *)malloc( (size t)10*sizeof(int))
point2 = .FALSE.               !  void* xptr;   (scalar result)
scalar = .FALSE.               !  xptr = malloc( (unsigned) *name );
                               !  if( xptr == NULL )
IF (v.declare(nid) == PTR1) THEN
   point1 = .TRUE.
ELSE IF (v.declare(nid) == PTR2.OR.v.declare(nid) == ARRAY2) THEN
   point2 = .TRUE.
ELSE
   scalar = .TRUE.             ! assname = err. result
END IF

CALL MoveToNext(';')           ! to end of this malloc record,
cend1 = c                      ! but next record was appended by pass1

c = c1
n = Get()
Argname = String               ! x
AssName = String               ! x

DO c = c,cend1
   IF (CC(c) == '=') EXIT
   n = n+1
   AssName(n:n) = CC(c)       ! x[....]
END DO

IF (point1) THEN
   v.ptr_deref(nid) = .FALSE.     ! set true by statement_proc caller
   v.ptr_array(nid) = .TRUE.
END IF

c = c+1                      ! past =
CALL BeginString()
IF (CC(c) == '(') CALL MoveToStringPast(')') ! discard (int *) syntax
n = Get()                    ! string should = malloc or calloc
Fline(f:f+9) = 'ALLOCATE( '
f = f+10
CALL BeginString()
c = c+1                      ! past malloc (
CALL BeginString()
IF (CC(c) == '(') CALL MoveToStringPast(')')  ! discard (size ?)  syntax
                                              !         (unsigned)
IF (scalar) THEN             ! allocate name in malloc( (unsigned) *name
   IF (CC(c) == '*') THEN
      c = c+1
   ELSE
      FF = SP                ! this malloc is "eaten" by C2F
      RETURN                 ! discard x = malloc(y   if x,y are scalars
   END IF
   n = Get()
   Fline(f:f+n-1) = string(1:n)  ! ALLOCATE( name
   f = f+n
   Fline(f:) = ',STAT=' // TRIM(assname) // ')'
   f = LEN_TRIM(Fline)
   RETURN
END IF

n = LEN_TRIM(AssName)
Fline(f:f+n-1) = AssName(1:n)  ! ALLOCATE( x    or x[..]
f = f+n

IF (Cline(c:c+5) == 'SIZEOF') THEN
   v.dim(nid) = SP           ! no dim set:   malloc(sizeof(..))
   size = SP
ELSE
   n = Get()                    ! size arg.
   size = String
   IF (point2.AND..NOT.Use_pointer2) THEN
      IF (argname == AssName) THEN           ! set dimension 1
         v.dim(nid) = '(' // size
      ELSE                                   ! set dimension 2
         dim_s = v.dim(nid)
         n1 = LEN_TRIM(dim_s)
         n =  LEN_TRIM(size)
         v.dim(nid) = dim_s(1:n1) // ',' // size(1:n) // ')'
      END IF
      FF = SP           ! abort this malloc output, info entered in v.dim
      RETURN
   END IF
END IF

IF (point1) THEN                ! syn:  x = malloc(   where *x declared
   IF (argname /= AssName) THEN
      CALL Error(11)            ! unsupported (illegal) syntax
      RETURN
   END IF
   IF (size /= SP) THEN
      FF(f) = '('                  ! x(
      f = f+1
      Fline(f:f+n-1) = size(1:n)   ! x(size
      f = f+n
   END IF

ELSE  ! point2 and use_pointer2 ! syn: x[i] = malloc(   with **x declared
   IF (argname == AssName) THEN
      FF(f) = '('                  ! x(
      f = f+1
      Fline(f:f+n-1) = size(1:n)   ! x(size
      f = f+n
   ELSE
      Fline(f:f+2) = '%p('
      f = f+3
      Fline(f:f+n-1) = size(1:n)   ! x(i)%p(size
      f = f+n
   END IF
END IF

c = cend1+1            ! to appended record begin
use_C2F_status = .FALSE.
IF (Key2 == C_if.AND.Cline(c:c+1) == 'if') THEN ! check for malloc status test
   CALL MoveToNext('(')
   c = c+1
   CALL BeginString()
   IF (CC(c) == '!') c = c+1
   n = Get()
   IF (string == argname) THEN     ! assume if (condition) can be replaced

      Fline(f:f+16) = ',STAT=C2F_status)'
      f = f+17
      use_C2F_status = .TRUE.    ! next if statement becomes "if (C2f_status"
   END IF
   RETURN
END IF

c = cend1+1            ! to appended record begin
use_C2F_status = .FALSE.
IF (Key2 == C_if.AND.Cline(c:c+1) == 'if') THEN ! check for malloc status test
   CALL MoveToNext('(')
   c = c+1
   CALL BeginString()
   IF (CC(c) == '!') c = c+1
   n = Get()
   IF (string == argname) THEN     ! assume if (condition) can be replaced

      Fline(f:f+16) = ',STAT=C2F_status)'
      f = f+17
      use_C2F_status = .TRUE.    ! next if statement becomes "if (C2f_status"
   END IF
   RETURN
END IF

IF (size == SP) THEN
   Fline(f:f+1) = ' )'
   f = f+2
ELSE
   Fline(f:f+2) = ') )'
   f = f+3
END IF

END SUBROUTINE Malloc_Proc

! -----------------------------------------------------------
SUBROUTINE memcpy_memmove_proc()  ! syn:   memcpy(a[ipvt[i]],b[i],sizeof(double)*n);
                          ! pass2: memcpy(a[ipvt[i]],b[i],SIZEOF(1.d0)*n);
INTEGER :: dimension
LOGICAL :: plus_detect        ! for ptr+offset syntax
CHARACTER (LEN=2) :: type_s

DO n = c,Cend
   IF (Cline(n:n+6) == 'SIZEOF(') THEN
      c2 = n                            ! save index of sizeof
      type_s = '??'
      IF (CC(n+8) == ')') type_s = 'I4'
      IF (CC(n+8) == '.') type_s = 'R4'
      IF (CC(n+9) == 'd') type_s = 'R8'
      EXIT
   END IF
END DO

IF (Key == C_memcpy) THEN
   Fline(f:f+13) = 'CALL COPY_' // type_s // '( '
ELSE       ! memmove
   Fline(f:f+13) = 'CALL MOVE_' // type_s // '( '
END IF
f = f+14
CALL MoveToStringPast('(')

DO k = 1,2                     ! process 2 arg fields identically
   IF (Cline(c:c+1) == '&(') c = c+2   ! past &(
   dimension = 0
   plus_detect = .FALSE.
   DO n = c,Cend                       ! find dimension of field
      IF (CC(n) == ',') EXIT
      IF (CC(n) == '+') plus_detect = .TRUE.
      IF (CC(n) == '['.AND.dimension == 0) dimension = 1
      IF (Cline(n:n+1) == '][') dimension = 2
   END DO

   IF (plus_detect.AND.dimension == 0) THEN    ! convert ptr+n+2 to ptr(n+2)
      n = Get()
      IF (CC(c) /= '+') THEN
         CALL Error(8)          ! pointer syntax unrecog.
         RETURN
      END IF

      Fline(f:f+n) = String(1:n) // '('
      f = f+n+1
      c = c+1                             ! past +
      CALL CopyToFirst(',')
      f = f-1
      Fline(f:f+1) = '),'                 ! ptr(n+j),
      f = f+2

      nid = GetIndex(string)
      IF (nid > 0.AND.v.declare(nid) == PTR1) v.ptr_array(nid) = .TRUE.

   ELSE
      CALL CopyToFirst(',')
      IF (Fline(f-2:f-1) == '),') THEN    ! closing paren associated with &(
         f = f-2
         Fline(f:f+1) = ', '              ! delete )
         f = f+2
      END IF
   END IF
   f = f+1
   IF (dimension == 1) THEN             ! single dimension advanced to 2
      f = f-2
      Fline(f:f+3) = '[0],'           ! supply implied dimension base
      f = f+4
   END IF
   CALL BeginString()
   IF (Fline(f-2:f-1) == SP) f = f-1
   IF (FF(f-1) == ',') f = f+1
END DO
                                    ! process size field
IF (CC(c2-1) == '*')  THEN          ! syn:  ???*sizeof(double)
   c2 = c2-2
   n = c2-c+2
   Fline(f:f+n-1) = Cline(c:c2) // ')'
   f = f+n
ELSE
   CALL MoveToStringPast('*')              ! move past ,sizeof(double)*
   CALL CopyToCend()
   FF(f-1) = SP                          ! remove ;
END IF
c = Cend+1

Key = 99

END SUBROUTINE Memcpy_memmove_proc

! -----------------------------------------------------------
SUBROUTINE fline2_proc(nid)

INTEGER :: nid, f, n, i,k
INTEGER :: ntype, ndeclare, nlocate
LOGICAL :: ltarget, lvoid, lptr
CHARACTER (LEN=MAXSTR) :: type_s

ntype    = v.type(nid)
ndeclare = v.declare(nid)
nlocate  = v.locate(nid)
ltarget  = v.target(nid)
lvoid    = v.ptr_void(nid)
lptr     = v.ptr_array(nid)
init_s   = v.init(nid)
dim_s    = v.dim(nid)

Fline2 = SP               ! reset for next type output
f = 1+INDENT

IF (ndeclare == PTR2.AND.Use_pointer2) THEN
   type_s = TYPE_PTR_1D(ntype)
ELSE
   type_s = Types(ntype)
END IF

k = LEN_TRIM(type_s)
Fline2(f:f+k-1) = type_s(1:k)
f = f+k

IF (ntype == TCHAR) THEN           ! try to define a LEN=xx

   IF (nlocate == ARG) THEN
      IF (dim_s == '[]'.OR. &
         (dim_s == '(*)'.AND..NOT.lptr)) Fline2(f:f+7) = ' (LEN=*)'

   ELSE IF (dim_s(1:2) == '[]'.AND.init_s == SP) THEN
      IF (dim_s(3:3) /= SP) THEN
         DO i = 3,7
            IF (dim_s(i:i) == SP) EXIT
         END DO
         Fline2(f:f+i+3) = ' (LEN=' // dim_s(3:i-1) // ')'
      ELSE
         Fline2(f:f+8) = ' (LEN=99)'   ! default for syn:  char nnn[]
      END IF

   ELSE IF (ndeclare == ARRAY1) THEN
      Fline2(f:f+8) = ' (LEN=99)'   ! default for syn:  char *nnn[..]

   ELSE IF (dim_s(1:1) == '['.AND.dim_s(2:2) /= ']') THEN
      n = LEN_TRIM(dim_s)
      DO i = 3,n
         IF (dim_s(i:i) == ','.OR.dim_s(i:i) == ')') EXIT
      END DO
      IF (dim_s(i:i) == ','.OR.dim_s(i+1:i+1) == SP) THEN   ! [nn)
         Fline2(f:f+i+4) =  ' (LEN=' // dim_s(2:i-1) // ')'
      ELSE                        ! [xx)yy {}
         DO k = i+1,i+3
            IF (dim_s(k:k) == SP) EXIT
         END DO
         Fline2(f:) =  ' (LEN=' // dim_s(i+1:k-1) // ')'
      END IF

   ELSE IF (init_s /= SP.AND.init_s /= '{}') THEN   ! "123"; "123" ='1'
      IF (init_s == 'NULL') THEN
         Fline2(f:f+8) =  ' (LEN=99)'
         f = f+9
      ELSE
         i = LEN_TRIM(init_s)
         IF (init_s(i:i) == ';') i = i-1
         IF (init_s(1:1) == '=') i = i-1
         i = i-2                               ! reduce for ".." or '.'
         IF (i <= 1) THEN
             ! no LEN
         ELSE IF (i < 10) THEN
            WRITE (string,'(I1)') i
            Fline2(f:f+7) = ' (LEN=' // string(1:1) // ')'
         ELSE
            WRITE (string,'(I2)') i
            Fline2(f:f+8) = ' (LEN=' // string(1:2) // ')'
         END IF
      END IF
   END IF
   f = LEN_TRIM(Fline2) +1

ELSE IF (ntype >= TUNION) THEN         ! also TSTRUCT
   IF (v.name2(nid) == SP) THEN
      Fline2(f:f) = SP
      f = f+1
      f1 = f
      f2 = f
      RETURN
   ELSE                                ! TYPE/UNION NAME
      string = v.name2(nid)
      n = LEN_TRIM(string)
      Fline2(f:f+n+2) = ' (' // string(1:n) // ')'
      f = f+n+3
   END IF
END IF

IF (ndeclare == CONST) THEN
   IF (nlocate == ARG) THEN
      Fline2(f:f+10) = ',INTENT(IN)'
      f = f+11
   ELSE
      Fline2(f:f+9) = ',PARAMETER'
      f = f+10
   END IF

! ELSE IF (nlocate == ARG.AND.lvoid) THEN
!    Fline2(f:f+7) = ',POINTER'
!   f = f+8

ELSE IF (nlocate == ARG.AND.ndeclare == PTR1.AND.ntype == TSTRUCT) THEN
   Fline2(f:f+7) = ',POINTER'
   f = f+8

ELSE IF (nlocate /= ARG.AND.(ndeclare == PTR1.OR. &
                            (ndeclare == PTR2.AND.Use_pointer2))) THEN
   Fline2(f:f+7) = ',POINTER'
   f = f+8

ELSE IF (ltarget) THEN
   Fline2(f:f+6) = ',TARGET'
   f = f+7

END IF

IF (nlocate == EXTERN) THEN
   Fline2(f:f+8) = ',EXTERNAL'
   f = f+9

ELSE IF (nlocate == STATIC.AND.init_s /= '{}') THEN
   Fline2(f:f+4) = ',SAVE'      ! note: no save required if data declare
   f = f+5
END IF
END SUBROUTINE fline2_proc

! -----------------------------------------------------------
SUBROUTINE pointer_proc()      ! syn:  C2Fpointer(int   name);
                               !       C2Fpointer(struct structname  name);
INTEGER :: keytype             !       C2Fpointer(char   temp%entry);

c = c+1
n = Get()
structname = SP
keytype = 0
IF (string == 'int')    keytype = TINT
IF (string == 'float')  keytype = TFLOAT
IF (string == 'double') keytype = TDOUBLE
IF (string == 'char')   keytype = TCHAR
IF (string == 'struct') keytype = TSTRUCT
IF (keytype == 0) THEN
   CALL Error(10)       ! unrec. syntax
   RETURN
END IF
IF (keytype == TSTRUCT) THEN
   n = Get()                   ! structname
   structname = string
END IF

n = Get()                   ! name
name = string
nid = GetIndex(name)        ! check if declared
IF (nid == 0) THEN          ! shud not happen
   CALL Error(5)            ! undeclared
   RETURN
END IF

string = 'T_' // name
DO i = 1,n+2
   IF (string(i:i) == '%') string(i:i) = '_'
END DO

nid = GetIndex(string)        ! check T_name already declared
IF (nid == 0) THEN          ! no, declare it
   nid = GetNextIndex()
   v.name(nid) = string
   v.nc(nid) = LEN_TRIM(string)
   v.type(nid) = keytype
   IF (keytype == TSTRUCT) v.name2(nid) = structname
   v.declare(nid) = SCALAR
   v.locate(nid) = LOCAL
   v.dim(nid) = SP
   IF (keytype == TCHAR) THEN
      v.declare(nid) = ARRAY1
      v.dim(nid) = '[]'     ! gen. character (len=99)
   END IF
END IF

90 FORMAT (99A)
END SUBROUTINE pointer_proc

END PROGRAM C2F
