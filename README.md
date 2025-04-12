# c2f
Partial C to Fortran translator by

David Frank<br>
Cocoa Beach, Florida
retired RCA Missile Test Project radar programmer (28 yrs)..
at Cape Canaveral/Patrick AFB Florida, and "downrange"..

See the file README.C2F for instructions. A Python script to translate from C to Fortran is at [PyC2F](https://github.com/Beliavsky/PyC2F).

Note that RUNQQ in c2f.f90 is a Digital Visual Fortran extension that could be replaced by EXECUTE_COMMAND_LINE in modern Fortran. The gfortran mailing list discussed in 2017 https://gcc.gnu.org/legacy-ml/fortran/2017-12/msg00033.html how to make the program compilable with gfortran.
