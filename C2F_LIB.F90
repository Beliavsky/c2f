! F90 routines by: David Frank, Alan Miller, Jean Vezina
! must have compatible interfaces supplied in C2F.FI

! -----------------------------
FUNCTION ACCESS(filename, mode) RESULT(OK)
! -----------------------------
! Translation of C function access.
! mode = 0 check that file exists
!      = 1 check if it is executable
!      = 2 can file be written?
!      = 4 can file be read?
!      = 6 both of the above
! access returns a .FALSE. result if mode is not one of above

IMPLICIT NONE
CHARACTER (LEN=*), INTENT(IN) :: filename
INTEGER, INTENT(IN)           :: mode
LOGICAL                       :: OK

! Local variables
INTEGER :: length
CHARACTER (LEN=7) :: r, w, rw

SELECT CASE (mode)
  CASE (0)
    INQUIRE(FILE=filename, EXIST=OK)
  CASE (1)
     length = LEN_TRIM(filename)
     IF(length < 5) THEN
        OK = .FALSE.
     ELSE
        OK = filename(length-3:length) =='.exe' .OR. &
             filename(length-3:length) =='.EXE'
     END IF
  CASE (2)
    INQUIRE(FILE=filename, WRITE=w)
    OK = (w == 'YES')
  CASE (4)
    INQUIRE(FILE=filename, READ=r)
    OK = (r == 'YES')
  CASE (6)
    INQUIRE(FILE=filename, READWRITE=rw)
    OK = (rw == 'YES')
  CASE DEFAULT
    OK = .FALSE.
END SELECT

RETURN
END FUNCTION ACCESS

! ---------------------------
FUNCTION CEIL (value) RESULT(x)
! ---------------------------
  IMPLICIT NONE
  REAL(4) :: value, x
  x = CEILING(value)
END FUNCTION

! ---------------------------
FUNCTION FEOF (unitnum) RESULT(eof)
! ---------------------------
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: unitnum
  LOGICAL             :: eof
  INTEGER             :: status

  INQUIRE(unitnum, IOSTAT=status)
  eof = (status < 0)
  RETURN
END FUNCTION FEOF

! ---------------------------
FUNCTION FOPEN (path, action)  RESULT (unit)
! ---------------------------
  IMPLICIT NONE
  CHARACTER (LEN=*), INTENT(IN) :: path, action
  INTEGER :: unit

  CHARACTER (LEN=10) :: access, form, mode
  INTEGER :: iocheck, num
  LOGICAL :: unitopen

  !set file open defaults
  access = 'SEQUENTIAL'
  form   = 'FORMATTED '
  mode   = 'READWRITE '

  IF (action == 'a'.OR.action == 'a+') THEN
     access = 'APPEND'
  ELSE IF (action == 'r'.OR.action == 'rt') THEN
     mode = 'READ'
  ELSE IF (action == 'rb') THEN
     mode = 'READ'
     form = 'BINARY'
  ELSE IF (action == 'r+'.OR.action == 'w+') THEN
     mode = 'READWRITE'
  ELSE IF (action == 'w'.OR.action == 'wt') THEN
     mode = 'WRITE'
  ELSE IF (action == 'wb') THEN
     mode = 'WRITE'
     form = 'BINARY'
  END IF

  DO num = 11,99                         ! find 1st avail. unit
     INQUIRE (UNIT=num,OPENED=unitopen)
     IF (.NOT.unitopen) EXIT             ! num is avail.
  END DO

  OPEN (UNIT=num,FILE=path,ACCESS=access,FORM=form,ACTION=mode,IOSTAT=iocheck)
  IF (iocheck == 0) THEN      ! open successful
     unit = num               ! return a positive num 11,12,13,,,,
  ELSE
     unit = 0                 ! return 0 if err
  END IF

END FUNCTION FOPEN

! -----------------------------
SUBROUTINE FFLUSH (unitnum)
! -----------------------------
  USE DFLIB, ONLY: COMMITQQ
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: unitnum
  LOGICAL :: result

  result = COMMITQQ (unitnum)
END SUBROUTINE FFLUSH

! ---------------------------
FUNCTION FGETC (unit,ch)  RESULT(status)
! ---------------------------
  IMPLICIT NONE
  INTEGER   :: unit, status
  CHARACTER :: ch

  ! assumes file opened with "rb" read binary
  READ (unit,IOSTAT=status,ERR=1,END=1) ch
1 CONTINUE
  IF (status > 0) WRITE (*,91) 'read unit',unit,' status=',status
  RETURN
91 FORMAT (A,I3,A,I4)
END FUNCTION FGETC

! -----------------------------
FUNCTION TOUPPER (ch) RESULT(outchar)
! -----------------------------
  IMPLICIT NONE
  CHARACTER (LEN=1), INTENT(IN) :: ch
  CHARACTER (LEN=1)             :: outchar

  ! This simple version assumes that the lower case characters occur
  ! 32 positions before upper case characters, as in the ASCII collating
  ! sequence.   For a more general solution, substitute for the '32'
  ! ICHAR('a') - ICHAR('A')

  IF ( ch >= 'a' .AND. ch <= 'z') THEN
    outchar = CHAR(ICHAR(ch) -32)
  ELSE
    outchar = ch
  END IF
END FUNCTION TOUPPER

! -----------------------------
FUNCTION TOLOWER (ch) RESULT(outchar)
! -----------------------------
  IMPLICIT NONE
  CHARACTER (LEN=1), INTENT(IN) :: ch
  CHARACTER (LEN=1)             :: outchar

  ! This simple version assumes that the lower case characters occur
  ! 32 positions before upper case characters, as in the ASCII collating
  ! sequence.   For a more general solution, substitute for the '32'
  ! ICHAR('a') - ICHAR('A')

  IF ( ch >= 'A' .AND. ch <= 'Z') THEN
    outchar = CHAR(ICHAR(ch) +32)
  ELSE
    outchar = ch
  END IF
END FUNCTION TOLOWER

! -----------------------------
FUNCTION ISALPHA (ch)
! -----------------------------
  IMPLICIT NONE
  CHARACTER (LEN=1), INTENT(IN) :: ch
  LOGICAL             :: isalpha

  !  Checks if a character is alphabetic

  isalpha = ch >= 'a' .and. ch <= 'z' .or.  ch >= 'A' .and. ch <= 'Z'
END FUNCTION ISALPHA

! -----------------------------
FUNCTION ISALNUM (ch)
! -----------------------------
  IMPLICIT NONE
  CHARACTER (LEN=1), INTENT(IN) :: ch
  LOGICAL             :: isalnum

  !  Checks if a character is alphanumeric

  isalnum = ch >= 'a' .and. ch <= 'z' .or.  ch >= 'A' .and. ch <= 'Z' .or. &
            ch >= '0' .and. ch <= '9'
END FUNCTION ISALNUM

! -----------------------------
FUNCTION ISDIGIT (ch)
! -----------------------------
  IMPLICIT NONE
  CHARACTER (LEN=1), INTENT(IN) :: ch
  LOGICAL             :: isdigit

  !  Checks if a character is a digit

  isdigit =  ch >= '0' .and. ch <= '9'
END FUNCTION ISDIGIT

! -----------------------------
FUNCTION ISLOWER (ch)
! -----------------------------
  IMPLICIT NONE
  CHARACTER (LEN=1), INTENT(IN) :: ch
  LOGICAL             :: islower

  !  Checks if a character is lower case

  islower = ch >= 'a' .and. ch <= 'z'
END FUNCTION ISLOWER

! -----------------------------
FUNCTION ISUPPER (ch)
! -----------------------------
  IMPLICIT NONE
  CHARACTER (LEN=1), INTENT(IN) :: ch
  LOGICAL             :: isupper

  !  Checks if a character is upper case

  isupper = ch >= 'A' .and. ch <= 'Z'
END FUNCTION ISUPPER

! -----------------------------
! memcpy translations for data array types
! -----------------------------
  SUBROUTINE COPY_I4(a,b,n)
    INTEGER :: n
    INTEGER :: a(n), b(n)
    a = b
  END SUBROUTINE COPY_I4

  SUBROUTINE COPY_R4(a,b,n)
    INTEGER :: n
    REAL(4) :: a(n), b(n)
    a = b
  END SUBROUTINE COPY_R4

  SUBROUTINE COPY_R8(a,b,n)
    INTEGER :: n
    REAL(8) :: a(n), b(n)
    a = b
  END SUBROUTINE COPY_R8

! -----------------------------
! memmove translations for data array types
! -----------------------------
  SUBROUTINE MOVE_I4(a,b,n)
    INTEGER :: n
    INTEGER :: a(n), b(n), temp(8)
    temp = b
    a = temp
  END SUBROUTINE MOVE_I4

  SUBROUTINE MOVE_R4(a,b,n)
    INTEGER :: n
    INTEGER :: a(n), b(n), temp(8)
    temp = b
    a = temp
  END SUBROUTINE MOVE_R4

  SUBROUTINE MOVE_R8(a,b,n)
    INTEGER :: n
    INTEGER :: a(n), b(n), temp(8)
    temp = b
    a = temp
  END SUBROUTINE MOVE_R8

! -----------------------------
FUNCTION INT_CAST_TO_ARRAY_PTR(arg)
! -----------------------------
  INTEGER,POINTER :: INT_CAST_TO_ARRAY_PTR(:)
  INTEGER,TARGET :: arg(1)
  INT_CAST_TO_ARRAY_PTR => arg
END FUNCTION

FUNCTION FLT_CAST_TO_ARRAY_PTR(arg)
  REAL(4),POINTER :: FLT_CAST_TO_ARRAY_PTR(:)
  REAL(4),TARGET :: arg(1)
  FLT_CAST_TO_ARRAY_PTR => arg
END FUNCTION

FUNCTION DBL_CAST_TO_ARRAY_PTR(arg)
  REAL(8),POINTER :: DBL_CAST_TO_ARRAY_PTR(:)
  REAL(8),TARGET :: arg(1)
  DBL_CAST_TO_ARRAY_PTR => arg
END FUNCTION

! -----------------------------
FUNCTION STRING_TO_PTR(string)
! -----------------------------
! Convert a constant string into a pointer to a string
!
CHARACTER(*),INTENT(IN) :: string
CHARACTER,POINTER,DIMENSION(:) :: STRING_TO_PTR

ALLOCATE(STRING_TO_PTR(LEN(string)))

STRING_TO_PTR = TRANSFER(string,(/' '/))
END FUNCTION

! -----------------------------
FUNCTION TO_STRING(strptr)
! -----------------------------
!  Convert a pointer of n characters into a string of length n
CHARACTER,INTENT(IN) :: strptr(:)
CHARACTER*999,PARAMETER :: filler=' '
CHARACTER (LEN=LEN_TRIM(TRANSFER(strptr,filler(:size(strptr))))) TO_STRING
TO_STRING = TRANSFER(strptr,TO_STRING)
END FUNCTION

! -----------------------------
FUNCTION CV_TO_PTR(string)
! -----------------------------
! Convert a character(*) variable to a pointer to an array of n characters
! This function cheats a little the standard by transforming a string into
! an array of n characters while keeping the same starting address
CHARACTER(*),target::string
CHARACTER,POINTER,DIMENSION(:) :: CV_TO_PTR
INTERFACE
   FUNCTION CV_TO_PTR2(string,len)
   CHARACTER,POINTER,DIMENSION(:) :: CV_TO_PTR2
   CHARACTER(*),TARGET :: string
   END FUNCTION
END INTERFACE
CV_TO_PTR => CV_TO_PTR2(string,LEN(string))
END FUNCTION

! -----------------------------
FUNCTION CV_TO_PTR2(string,len)
! -----------------------------
!  Transform a scalar string into a 1 dimensional array of chars
!  while keeping the same starting address
   CHARACTER,POINTER,DIMENSION(:) :: CV_TO_PTR2
   CHARACTER,TARGET :: string(len)
   CV_TO_PTR2 => string
END FUNCTION

SUBROUTINE MEMSET_s(STRING,CHR,N)  ! For character strings with char chr
 CHARACTER(*) STRING,CHR*1
 STRING = repeat(CHR,N)
END SUBROUTINE
SUBROUTINE MEMSET_a(STRING,CHR,N)  ! For character arrays with char chr
 CHARACTER(1) STRING(N),CHR
 STRING = CHR
END SUBROUTINE

SUBROUTINE MEMSET_si(STRING,CHR,N) ! For character strings with int chr
 CHARACTER(*) STRING
 INTEGER CHR
 STRING = repeat(CHAR(CHR),N)
END SUBROUTINE
SUBROUTINE MEMSET_ai(STRING,CHR,N) ! For character arrays with int chr
 CHARACTER(1) STRING(N)
 INTEGER CHR
 STRING = CHAR(CHR)
END SUBROUTINE

FUNCTION STRCMP(s1,s2)     RESULT(output_4)
 CHARACTER (LEN=*) :: s1,s2
 INTEGER :: output_4

 IF (s1 < s2) THEN
    output_4 = -1
 ELSE IF (s1 > s2) THEN
    output_4 = +1
 ELSE
    output_4 = 0
 END IF
 RETURN
END FUNCTION

FUNCTION STRNCMP(s1,s2,n)     RESULT(output_4)
 CHARACTER (LEN=*) :: s1,s2
 INTEGER :: n, output_4

 IF (s1(1:n) < s2(1:n)) THEN
    output_4 = -1
 ELSE IF (s1(1:n) > s2(1:n)) THEN
    output_4 = +1
 ELSE
    output_4 = 0
 END IF
 RETURN
END FUNCTION
