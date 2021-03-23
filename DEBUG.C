#include <stdio.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <stdlib.h>

FILE *Outfile1;

#define ZERO 0
#define ONE 1
#define TWO 2
#define TRUE 1
#define FALSE 0
#define MAX_EXP 37
#define BOOLEAN int
# define REAL float

REAL xglobal[4];            /* target array */

struct bla {
    short k;
    double z;
    float x[3][2];
  };

struct  record {
        char name[20];
        int id;
        float price;
  };

struct list {
        int value;
        struct list *next;
  };

typedef struct nlist  {
        void  *entry;
        struct nlist *next;
  } llist;

typedef struct FCOMPLEX {double r,i;} fcomplex;
typedef struct dummy {void *pt; } Testpt;
typedef struct dmy { int aa; } DerivedInt;


main( int argc, char *argv[] )
/* Simple validation suite for C2F */
/* checks various aspects of the translation  */
{
  void t87( int argc, char *argv[]);   // provide VC6 a prototype

if(argc==1)
   Outfile1 = fopen("debug.new","w");
else
   Outfile1 = fopen(argv[1],"w");

t1();
t2();
t3();
t4();
t5();
t6();
t7();
t8();
t9();
t10();
t11();
t12();
t13();
t14();
t15();
t16();
t17();
t18();
t19();
t20();
t21();
t22();
t23();
t24();
t25();
t26();
t27();
t28();
t29();
t30();
t31();
t32();
t33();
t34();
t35();
t36();
t37();
t38();
t39();
t40();
t41();
t42();
t43();
t44();
t45();
t46();
t47();
t48();
t49();
t50();
t51();
t52();
t53();
t54();
t55();
t56();
t57();
t58();
t59();
t60();
t61();
t62();
t63();
t64();
t65();
t66();
t67();
t68();
t69();
t70();
t71();
t72();
t73();
t74();
t75();
t76();
t77();
t78();
t79();
t80();
t81();
t82();
t83();
t84();
t85();
t86();
t87( argc, argv);
t88();

fclose(Outfile1);
if(argc==1)
   printf("Output written to DEBUG.NEW\n");
else
   printf("Output written to file: %s\n",argv[1]);

}

t1() /* checks structure definition and references */
{
int j;
struct gg {
        float r;
        int x;
        } a[10];
int i;

fprintf(Outfile1,"\nTest 1\n");
for(i=0;               /* multi-line for */
    i<10;
    i++)
{
a[i].r=12;
a[i].x=7;
fprintf(Outfile1,"%f%d\n",a[i].r,a[i].x);
}
j=16;
fprintf(Outfile1,"%f\n",sqrt((float) j));
}

t2() /* Checks translation of operators */
{
int i,j,k;
i=3;
i+=3;
i*=2;

fprintf(Outfile1,"\nTest 2\n");
fprintf(Outfile1,"%d\n",i);
i=8;
j=i>>1;
k=i<<1;
fprintf(Outfile1,"%d %d\n",j,k);
j=i%2;
k=(i>5) ? 4 : 3;
fprintf(Outfile1,"%d %d\n",j,k);
for(i=0;i<10;i++) fprintf(Outfile1,"%d\n",i);
}

t3() /* checks simple function reference with array */
{
int l [10];

fprintf(Outfile1,"\nTest 3\n");
l[3]=12;
   output(l);
}
output(arg)
int arg[];
{
int i;
float j[3];
i=3;
j[1]=4;
fprintf(Outfile1,"%d\n",arg[3]);
}

t4() /* checks function declaration */
{
int i,j,f();

fprintf(Outfile1,"\nTest 4\n");

i=1;
j=f(i);
fprintf(Outfile1,"%d\n",j);
}
int f(int i)  /* test comment */
{
return (i);
}

t5() /* checks basic string operations */
{
char x[10];

fprintf(Outfile1,"\nTest 5\n");
strcpy(x,"abc");
strcat(x,"def");
fprintf(Outfile1,"%s\n",x);
}

t6() /* checks pointer dereferencing */
{
int i,j,k();

fprintf(Outfile1,"\nTest 6\n");
j=k(&i);
fprintf(Outfile1,"%d\n",i);
}
int k( int *i)
{
int u;
u=0;*i=0;
*i=*i+1;
return(1);
}

t7() /* checks C2F ability to distinguish between pointer references and
     multiplication */
{
int i[1],j;

fprintf(Outfile1,"\nTest 7\n");
i[0]=1;
j=i[0]*i[0]*4;
fprintf(Outfile1,"%d\n",j);
}

t8() /* checks for nesting */
{
int i,j,k;

fprintf(Outfile1,"\nTest 8\n");
for(i=0;i<2;i++)
   for(j=0;j<2;j++)
      {
        fprintf(Outfile1,"%d\n",i); fprintf(Outfile1,"%d\n",j);
      }
fprintf(Outfile1,"%s\n","               end test8");
}

t9() /*  checks while with continuation and bracket on final line */
{
int i=1,j=3,k=5;

fprintf(Outfile1,"\nTest 9\n");
while( i<3 &&
       j==3 &&
       k==5)
{
   fprintf(Outfile1,"%d\n",i);
   i++;
 }
}

t10() /* checks loop nesting and assignments of arrays */
{
int i,j;
int n,m;
float k[10][10],kk[10][10];

fprintf(Outfile1,"\nTest 10\n");
m=10;
n=10;
for(i=0;i<m;i++) {
   for(j=0;j<n;j++) {
      k[j][i]=i+j;
      kk[j][i]=0;
}
}
for(i=0;i<n;i++) {
   for(j=0;j<m;j++) {
       kk[j][i]+=k[j][i];
}
}
}

t11() /* checks generation of multiple assignments */
{
int i,j,k;

fprintf(Outfile1,"\nTest 11\n");
i=j=k=0;
}

t12()  /* other loop nesting tests */
{
int i,j;
float k[10][10];
float s;

fprintf(Outfile1,"\nTest 12\n");
for(i=0;i<10;i++)
   for(j=0;j<10;j++)
      k[j][i]=i+j;
s=0;
for(i=0;i<10;i++)
   for(j=0;j<10;j++)
       if(j>5)s+=k[j][i];

s=0;
for(i=0;i<10;i++)
   for(j=0;j<10;j++)
       if(j>5)
       {
         s+=k[j][i];
       }

fprintf(Outfile1,"%f\n",s);
}

t13() /* Test character inquiry functions */
{
fprintf(Outfile1,"\nTest 13\n");
if(isalpha('a'))fprintf(Outfile1,"alphabetic\n"); /* Test if alphabetic */
if(isalnum('a'))fprintf(Outfile1,"alphanumeric\n"); /* alphanumeric ? */
if(isdigit('1'))fprintf(Outfile1,"numeric\n"); /* Test if numeric */
if(islower('a'))fprintf(Outfile1,"lower case\n"); /* Test if lower case */
if(isupper('A'))fprintf(Outfile1,"upper case\n"); /* Test if upper case */
fprintf(Outfile1,"%c\n",toupper('a'));
fprintf(Outfile1,"%c\n",tolower('B'));
}

t14() /* Test function renaming*/
{
float x=12.7;

fprintf(Outfile1,"\nTest 14\n");
fprintf(Outfile1,"%f\n",fabs(x));
fprintf(Outfile1,"%f\n",ceil(x));
}

t15() /*check simple pointer operations*/
{
   int *x,*y;
   int n,i;

fprintf(Outfile1,"\nTest 15\n");
   /*allocate memory*/
   n=10;
   x=malloc(n*sizeof(int));
   for(i=0;i<n;i++)
      x[i]=i;
   /* pointer assign x to y */
   y=x;
   for(i=0;i<n;i++)
     fprintf(Outfile1,"%d\n",y[i]);
   if(x==y)fprintf(Outfile1,"These pointers are associated\n");
   free(x);
}

t16() /*check pointer operations, Test 2*/
{
int *x,*y,*z;

fprintf(Outfile1,"\nTest 16\n");
x = malloc(2*sizeof(int)); /*allocate 2 elements*/
x[0] =12;/*assign values to the first and second elements*/
x[1] = 8;
y = &x[0]; /*pointer assign first element*/
*y=*y+1;  /*add 1 to the contents of the variable (dereferenced) */
*y = *y+1;  /* same but with spaces */
z = y;   /*pointer assign y to z*/
if(y==z)fprintf(Outfile1,"y and z are pointer associated\n");
if(*y==*z)fprintf(Outfile1,"y and z contains the same values\n");

fprintf(Outfile1,"%d\n",*y);
}

t17() /*simple Test for special C attributes*/
{
static float x=17; /*add save attribute to declaration*/
auto int n=17;/* keep only the type*/
register short i; /*keep only the type*/
static double a,
              b,c;

fprintf(Outfile1,"\nTest 17\n");
i=17;
fprintf(Outfile1,"values =%d %f %d\n",i,x,n);/*note: Test# = output values*/
}

t18() /*Test array reordering*/
{
int kkk=3;
int k[3]={0,1,2};
static short a[11],bb[2][11];
double ccc[3][2][11];

fprintf(Outfile1,"\nTest 18\n");
ccc[3] [k[1]] [kkk] = kkk;
fprintf(Outfile1,"%f\n",ccc[3] [1] [kkk]+15.)   ;/*note: output value = 18. */
}

t19() /*Test accessing same array using array and pointer in subroutine*/
{
int l [10];

fprintf(Outfile1,"\nTest 19\n");
l[3]=19;
   output2(l,l);
}

output2(arg1,arg2)
int arg1[],*arg2;
{
int i;
float j[3];
i=3;
j[1]=4;
fprintf(Outfile1,"%d %d\n",arg1[3],arg2[3]);
}

t20() /*test processing of pointers to pointers*/
{
int **x;
float **y;
double **z;
int i,j,k;
void print_array();

fprintf(Outfile1,"\nTest 20\n");
k=0;
x = malloc(5*sizeof(int *)); /*allocate the array of pointers*/
y = malloc(5*sizeof(float *));
z = malloc(5*sizeof(double *));
for(i=0;i<5;i++)
{
   x[i] = malloc (5*sizeof(int));
   y[i] = malloc (5*sizeof(float));
   z[i] = malloc (5*sizeof(double));
   for(j=0;j<5;j++)
   {
   k++;
   x[i][j] = k;
   y[i][j] = k;
   z[i][j] = k;
   }
}
print_array(x,y,z);
}
void print_array(int **x, float **y, double **z)
{
int i,j;
for(i=0;i<5;i++)
   for(j=0;j<5;j++)
     fprintf(Outfile1,"%3d%6.0f%6.0f\n",x[i][j],y[i][j],z[i][j]);
}

typedef int AN_INTEGER;
t21() /*test processing of pointers to pointers*/
{
AN_INTEGER **x;
AN_INTEGER i,j,k;
void print_array2();

fprintf(Outfile1,"\nTest 21\n");
k=0;
x = malloc(5*sizeof(AN_INTEGER *)); /*allocate the array of pointers*/
for(i=0;i<5;i++)
{
   x[i] = malloc (5 * sizeof(AN_INTEGER));
   for(j=0;j<5;j++)
   {
   k++;
   x[i][j] = k;
   }
}
print_array2(x);
}
void print_array2( AN_INTEGER **x)
{
AN_INTEGER i,j;
for(i=0;i<5;i++)
   for(j=0;j<5;j++)
     fprintf(Outfile1,"%d\n",x[i][j]);
}

t22() /*test initialization in routines*/
{
 int x,y,i;
 void subr();
 fprintf(Outfile1,"\nTest 22\n");
 for(i=1;i<10;i++)
 {
   subr(&x,&y);
   fprintf(Outfile1,"%d %d\n",x,y);
 }
}
void subr(int *a,int *b)
{
 int k=7;
 static int j=1;
 int p,q,r; /*just trying to confuse C2F*/
 k++;
 j++;
 *a = k;
 *b = j;
}

t23()  /* test post-increment included within for loop
          and modulo within if((j%k)==1) statement recognized    */
{
 int i,j,k;
 float s[10];

 fprintf(Outfile1,"\nTest 23\n");
 s[0]=s[1]=s[2]=s[3]=0;
 j=4;
 for(i=1;i<=6;i++)
   s[j++]=i;
 k=0;
 for(i=0;i<10;i++)
 {
   if((i%2) == 0)k++;
   fprintf(Outfile1,"%4d%4d%6.1f\n",i,k,s[i]);
 }
}

t24() /* test simple pointer arithmetic*/
{
 int *x, *y, i, *p;

 fprintf(Outfile1,"\nTest 24\n");
 x = malloc(10*sizeof(int));

 p = x;
 for (i=0;i<10;i++)
 {
   *p=i*i*i;
   p++;
 }
 y = x;
 y = y + 6;
 i = y[3];
 fprintf(Outfile1,"%d %d\n",*y,i);
 /* example 2 */
 for(i=0;i<6;i++)
 {
 p = y - i;
 fprintf(Outfile1,"%d\n",p[1]);
 }
 /*example 3*/
 p =y;
 for(i=0;i<6;i++)
 {
 fprintf(Outfile1,"%d\n",p[1]);
 p--;
 }
}

t25() /* test for pointer target */
{
void test_arg();
int i,j[5];
int *k,*l;

fprintf(Outfile1,"\nTest 25\n");
i=5;
j[1] = 8;
j[3] = 22;

k = &i;

l = &j[2];
l++;
fprintf(Outfile1,"%d %d\n",*k,l[0]);
test_arg(&i,j);
}
void test_arg( int *i,int j[])
{
int *k,*l;
k = i;
l = &j[1];
fprintf(Outfile1,"%d %d\n",*k,*l);
}

t26()
{
int x[5][5];
int i,j,l=1;
void display_array();

fprintf(Outfile1,"\nTest 26\n");
for(i=0;i<5;i++)
   for(j=0;j<5;j++)
     x[i][j]=l++;
display_array(x);
}
void display_array(int x[5][5])
{
int i,j;
for(i=0;i<5;i++)
   for(j=0;j<5;j++)
     fprintf(Outfile1,"%d\n",x[i][j]);
}

t27()
{
int x[3][2];
int i,j,l=1;
void display_array1();
void display_array2();

fprintf(Outfile1,"\nTest 27\n");
for(i=0;i<3;i++)
   for(j=0;j<2;j++)
     x[i][j]=l++;
display_array1(x);
display_array2(x);
}
void display_array1(int x[3][2])
{
int i,j;
for(i=0;i<3;i++)
   for(j=0;j<2;j++)
     fprintf(Outfile1,"%2d\n",x[i][j]);
}
void display_array2(x)
int x[3][2];
{
int i,j;
for(i=0;i<3;i++)
   for(j=0;j<2;j++)
     fprintf(Outfile1,"%3d\n",x[i][j]);
}

t28() /*test the processing of const*/
{
void print_const1();
void print_const2();
const int x[]={1,2,3};
const float b=4;

fprintf(Outfile1,"\nTest 28\n");
fprintf(Outfile1,"%d %d %d\n",x[0],x[1],x[2]);
fprintf(Outfile1,"%f\n",b);
print_const1(x);
print_const2(x);
}
void print_const1(const int x[])
{
fprintf(Outfile1,"%d %d %d\n",x[0],x[1],x[2]);
}
void print_const2(x)
const int x[];
{
fprintf(Outfile1,"%d %d %d\n",x[0],x[1],x[2]);
}

t29() /*test for operations in structures*/
{
 struct yyy {
            int x[3];
            float i;
           };

 struct yyy example;
 int k[]={1,2,3};
 float r=44;

 fprintf(Outfile1,"\nTest 29\n");
 example.x[1] = k[1];
 example.x[1]++;
 example.i = r;
 fprintf(Outfile1,"%d %f\n",example.x[1],example.i);
}

t30() /*test for pointers in 2 instances of same structure*/
      /* with different usages for each instance*/
{
struct yyy { int *x; float *i; };
struct yyy example,example2;
int k[]={1,2,3};
float r=44;
int s=18;
float k2[]={1.44,5.88,12.9};

fprintf(Outfile1,"\nTest 30\n");
example.x = k;
example.x++;
example.i = &r;
fprintf(Outfile1,"%d %f\n",example.x[1],*example.i);
example2.x = &s;
example2.i = k2;
example2.i++;
fprintf(Outfile1,"%d %f\n",*example2.x,example2.i[1]);
}

t31()  /* test for union structure */
{
union test
          {
            int x;
            char y;
          };
union test a;

fprintf(Outfile1,"\nTest 31\n");
a.x=12;
fprintf(Outfile1,"%d\n",a.x);
}

t32()
{
/* test for typedef defining a structure, union, and
nested structure declaration*/

typedef struct
             {
              int a;
              float b;
              union
              {
               int c;
               float d;
              } u1;
              struct
              {
               int e;
               float f;
              } s1;
              double z;
             } Bigtype;
Bigtype example;

fprintf(Outfile1,"\nTest 32\n");
example.u1.c=12;
example.s1.f=144.5;
fprintf(Outfile1,"%d %f\n",example.u1.c,example.s1.f);
}

t33()
{ /*test array initialization*/
int test_array[1][4]={1,2,3,4};
void print_30();

fprintf(Outfile1,"\nTest 33\n");
fprintf(Outfile1,"%d\n",test_array[0][3]);
print_30(test_array);
}
void print_30(int arr [1][4])
{
fprintf(Outfile1,"%d\n",arr[0][3]);
}

t34()
{
int a[3]={1,2,3};
int x[3][2]={1,2,3,4,5,6};
int y[2][2][2]={1,2,
                3,4,5,
                      6,7,8};
int i,j,k;

fprintf(Outfile1,"\nTest 34\n");

/* 1d array */
fprintf(Outfile1," 1 d array \n");
for (i=0;i<3;i++) fprintf(Outfile1,"%d\n",a[i]);

/* 2d array */
fprintf(Outfile1," 2d array \n");
for(i=0;i<3;i++)
   for(j=0;j<2;j++)
      fprintf(Outfile1,"%d\n",x[i][j]);

/* 3d array */
fprintf(Outfile1," 3d array\n");
for(i=0;i<2;i++)
   for(j=0;j<2;j++)
      for(k=0;k<2;k++)
         fprintf(Outfile1,"%d\n",y[i][j][k]);
}

t35()
{
static int a[3]={1,2,3};
static int x[3][2]={1,2,3,4,5,6};
static int y[2][2][2]={1,2,3,4,5,6,7,8};
int i,j,k;

fprintf(Outfile1,"\nTest 35\n");

/* 1d array */
fprintf(Outfile1," 1 d array \n");
for (i=0;i<3;i++) fprintf(Outfile1,"%d\n",a[i]);

/* 2d array */
fprintf(Outfile1," 2d array \n");
for(i=0;i<3;i++)
   for(j=0;j<2;j++)
      fprintf(Outfile1,"%d\n",x[i][j]);

/* 3d array */
fprintf(Outfile1," 3d array\n");
for(i=0;i<2;i++)
   for(j=0;j<2;j++)
      for(k=0;k<2;k++)
         fprintf(Outfile1,"%d\n",y[i][j][k]);
}

t36()
{ /* TEST POINTER TO STRUCTURE */
typedef struct
             {
              int x[3];
              float bbb;
             } Newtype;
Newtype a, *b;

fprintf(Outfile1,"\nTest 36\n");
a.x[1]=22.2;
a.bbb=5.55;
b =&a;
fprintf(Outfile1,"%f %d\n",b->bbb,b->x[1]);
b = (Newtype*)malloc(sizeof(Newtype));
b->x[2] = 99;
b->bbb = 3.1416;
fprintf(Outfile1,"%f %d\n",b->bbb,b->x[2]);
}

t37()
{
/*test direct recursion*/
double fact();

fprintf(Outfile1,"\nTest 37\n");
fprintf(Outfile1,"%e\n",fact(69));
}
double fact(arg)
int arg;
{
if(arg<=1)
 return 1;
else
 return arg*fact(arg-1);
}

t38()
{
/*test indirect recursion*/
double fact1();
fprintf(Outfile1,"\nTest 38  is not translated as yet \n");
/* fprintf(Outfile1,"%e\n",fact(69)); */
}
double fact1(arg)
int arg;
{
double fact2();
if(arg<=1)
 return 1;
else
 return arg*fact2(arg-1);
}
double fact2(int arg)
{
double fact();
return fact(arg);
}

t39()
{
/*test character processing*/
void display_char100();/* to avoid conflicts with other
 test cases*/
 char x[]="abcdef",z[]="12345";

fprintf(Outfile1,"\nTest 39\n");
fprintf(Outfile1,"%s %s\n",x,z);
display_char100(x,z);
}
void display_char100(char a[],char b[])
{
fprintf(Outfile1,"%s\n",a);
fprintf(Outfile1,"%s\n",b);
}

t40()   /*test structure initialization*/
{
struct test1 {int x; float b[3];};
struct test2 { struct test1 a; char b;};

struct test1 x1={1,1.1,2.1,3.1};
struct test2 x2={1,
                 1.1,2.1,3.1,
                 's'};
fprintf(Outfile1,"\nTest 40\n");
fprintf(Outfile1,"%d %f %f %f\n",x1.x,x1.b[0],x1.b[1],x1.b[2]);
fprintf(Outfile1,"%d %f %c\n",x2.a.x,x2.a.b[2],x2.b);

}

t41()
{
/* test procedure parameters*/
double f1();
double f2();
double x=15;

fprintf(Outfile1,"\nTest 41\n");
fprintf(Outfile1,"%f\n",f1(f2,x));
}
double f1(double f(),double arg)
{
return f(arg*2);
}
double f2(double arg)
{
return sqrt(arg);
}

t42()
{
/* test array indexing with nested subscripts */
int x[4][3];
float z[4][3];

fprintf(Outfile1,"\nTest 42\n");
x[3][2]=2;
x[3][1]=1;
z[x[3][2]][x[3][1]]=8;
fprintf(Outfile1,"%f\n",z[x[3][2]][x[3][1]]);
}

t43() /* test pow function translation */
{
 float x,y;
 fprintf(Outfile1,"\nTest 43\n");
 x=pow(2.,2);
 y=pow(2.,pow(2.,2));
 fprintf(Outfile1,"%f%f%f\n",x,y,pow(2.,pow(2.,2)));
}

t44()
/* test pointer to string operations */
{
char *x,*y;
char z[]="12345";
char u[]="t";
void print_strings_xx(); /*to avoid conflicts with other test cases*/

fprintf(Outfile1,"\nTest 44\n");
x="abcdef";
y=z;
fprintf(Outfile1,"%s %s\n",x,y);
print_strings_xx(x,y);
if (x[3]=='d')
   x = u;
else
   x = "long string";
fprintf(Outfile1,"%s\n",x);
}
void print_strings_xx(char x[],
                      char y[])
{
fprintf(Outfile1,"%s %s\n",x,
                           y);
}

t45() /* test multiple statements*/
{
int a,b,c; float g,h; a=1;b=2;
fprintf(Outfile1,"\nTest 45\n");
fprintf(Outfile1,"%d %d\n",a,b);
}

#define FORMAT "%f %f\n"
t46() /* test case statements with syntax included
         & use of define for printf format string */
{
REAL x,y;
int q;

fprintf(Outfile1,"\nTest 46\n");
for(q=2;q<5;q++)
{
 switch (q)
 {
 case 2: x = 1; y = 2;break;
 case 3: x = 4; y= 10;break;
 default: fprintf(Outfile1,"default\n");
 }
 fprintf(Outfile1,FORMAT,x,y);
}
}

t47() /* check for loop with starting i--; */
{
 int i;
fprintf(Outfile1,"\nTest 47\n");
i=9;
for (i--; i>0; i--)
   fprintf(Outfile1,"%d\n",i);
}

t48()
{
REAL x[3]={.1,.2,.3},result;
REAL horner(int n, REAL x[], REAL v);
int n=2;

fprintf(Outfile1,"\nTest 48\n");
result = horner(n,x,.3);
fprintf(Outfile1,"%d %f\n",n,result);
}
REAL horner        /* Horner scheme for polynomial evaluations .......*/
           (
            int n,                         /* Polynomial degree ......*/
            REAL a[],                      /* Polynomial coefficients */
            REAL x                         /* place of evaluation ....*/
           )                               /* Polynomial value at x ..*/
{
  REAL summe;
  for (summe = a[n], n--; n >= 0; n--)
    summe = summe * x + a[n];
  return summe;
}

t49()
{
REAL opolwert(int n,REAL X,REAL B[],
              REAL D[],REAL C[]);
REAL a[4]={.1,.2,.3,.4};
REAL b[4]={.01,.02,.03,.04};
REAL c[4]={1.1,.12,.13,.14};
REAL OP;
int i;

fprintf(Outfile1,"\nTest 49\n");

for(i=0;i<4;i++)
{
  OP = opolwert (i,.4,a,b,c);
  fprintf(Outfile1,"degree %d value = %f\n",i,OP);
}
}

REAL opolwert  /* Evaluate the polynomial from  pol_appr() ...........*/
             (
              int  n,      /* degree of polynomial ...................*/
              REAL x,      /* x-value ................................*/
              REAL b[],    /* coefficients for orthogonal polynomials */
              REAL d[],
              REAL c[]     /* coefficients of optimal polynomial .....*/
             )             /* value of polynomial at x ...............*/

/***********************************************************************
* Evaluate the polynomial P from  pol_appr() at x.                     *

* Due to the two step recursion this code has the same problem as      *
* qwert(). Hence we again do not use recursion here.                   *
*                                                                      *
* Input parameters:                                                    *
* =================                                                    *
* n: degree of P                                                       *
* x: x-value                                                           *
* b: [1..n] aux vectors \  to evaluate the orth. polynomials. for Qk   *
* d: [2..n]-Hilfsvektor /  we need b[1],...,b[k] and  d[2],...,d[k]    *
*                          (see pol_appr()).                           *
* c: [0..n] coefficient vector for the expansion of P wrt. the         *
*    orthogonal system of polynomials                                  *
*                                                                      *
* Return value :                                                       *
* ==============                                                       *
* P(x)                                                                 *
*                                                                      *
* global names used:                                                   *
* ==================                                                   *
* REAL                                                                 *

***********************************************************************/


{
  int  k,i;            /* Loop index                                    */
  REAL sk = ZERO,    /* current value s[k] of the horner like scheme  */
       sk1,          /* s[k+1]                                        */
       sk2;          /* s[k+2]                                        */


  switch (n)
  {
    case 0:  return c[0];

    case 1:  return c[0] + c[1] * (x - b[1]);

    default: sk2 = c[n];
             sk1 = c[n - 1] + c[n] * (x - b[n]);
             for (k = n - 2; k >= 0; k--, sk2 = sk1, sk1 = sk)
               sk = c[k] + sk1 * (x - b[k + 1]) - sk2 * d[k + 2];
             return sk;
  }
}

t50()
/* test  %+ specifier
   test statement embedded {printf(" line 1\n line 2\n line 3\n");}
   test multirecord output using \n in above statement       */
{
fprintf(Outfile1,"\nTest 50\n");
fprintf(Outfile1,"%+d\n",123);
{fprintf(Outfile1," line 1\n line 2\n line 3\n");}
}

t51()    /* checks          for( n = 1; n <= 5; n=n+1 )
             and            for( ; n <= 5; n++ )
             and            for( ; *string != '/0'; string++)      */
{
  int  n,k;
  char string[]="12345";

  fprintf(Outfile1,"\nTest 51\n");
  for( n = 1; n <= 5; n=n+1 ) fprintf(Outfile1,"%d\n", n );

  n = 1;
  k = 1;
  for( ; n <= 5; n++ ) {
      fprintf(Outfile1,"%d%d\n",n,k);
      k++;
   }
   sub51(string);
}
sub51(char s[])
{
  for( ; *s != '\0'; s++)
     fprintf(Outfile1,"%c\n",*s);
}

t52()    /* checks string decode using
           sscanf(string,"%d %c %d",&n1,&ch,n2); */
{
  int  n1,n2;
  char ch,string[]="123 ? 456";

  fprintf(Outfile1,"\nTest 52\n");
  sscanf(string,"%d %c %d",&n1,&ch,&n2);
  fprintf(Outfile1,"%d%c%d\n",n1,ch,n2);
}

t53()  /* test: ++x[][];
                  y[i++] = z[j++];
                  k = ++j        */
{
 int x[5][5], y[5],z[5],i=0,j=0,k;

 fprintf(Outfile1,"\nTest 53\n");

 x[3][3]=5;
 ++x[3][3];
 fprintf(Outfile1,"%d\n",x[3][3]-5);

 for(k=0;k<5;++k)
     z[k]=k;

 while(z[j]<4)
   y[i++] = z[j++];

 for (k=0;k<4;k++)
   fprintf(Outfile1,"%d\n",y[k]);

 i=0;
 k=++i;
 fprintf(Outfile1,"%d\n",k);
}

t54() /* more pointer arithmetic
             *p++=i*2;
             *p++=*q++               */
{
 int *x,*p,i,*q,y[5];
 fprintf(Outfile1,"\nTest 54\n");
 x = malloc(5*sizeof(int));
 p = x;

 for (i=0;i<5;i++)
 {
   *p++=i*2;
   fprintf(Outfile1,"%d\n",x[i]);
 }

 q = y;
 p = x;
 for (i=0;i<5;i++)
   *q++ = *p++;
 for (i=0;i<5;i++)
 {
 fprintf(Outfile1,"%d\n",y[i]);
 }
}

t55()   /* more nasty for syntax */
{
 int i,l=3;
 fprintf(Outfile1,"\nTest 55\n");
 for(;i=l/2,i<9;l=l+1)
   fprintf(Outfile1,"%d %d\n",l,i);
}

t56()   /* test passing struct as function arg */
{
 struct bla m={1,2.,
             3.,4.,5.,6.,7.,8.};
 float y;
 float fun1(struct bla *m);

 fprintf(Outfile1,"\nTest 56\n");
 y=fun1(&m);
 fprintf(Outfile1,"%f\n",y);
}
float fun1(struct bla *p)
{
 p->x[2][1]=p->x[2][0];  // copy next to last value to last value
 return p->x[2][1];     // return last value = 7.
}

t57()
{
 char word[20];

 fprintf(Outfile1,"\nTest 57\n");
 word[0] = 'H';
 word[1] = 'e';
 word[2] = 'l';
 word[3] = 'l';
 word[4] = 'o';
 word[5] = 0;
 fprintf(Outfile1,"The contents of word[] is -->%s\n", word );
}

t58()
{
 char c = 'Q';
 char *char_ptr = &c;

 fprintf(Outfile1,"\nTest 58\n");
 fprintf(Outfile1,"%c %c\n", c, *char_ptr);
 c = '/';
 fprintf(Outfile1,"%c %c\n", c, *char_ptr);
 *char_ptr = '(';  /* assigns ( as contents address specified by ptr  */
 fprintf(Outfile1,"%c %c\n", c, *char_ptr);
}

t59()
{
 struct date { int month, day, year; };
 struct date today, *date_ptr;

 fprintf(Outfile1,"\nTest 59\n");

 date_ptr = &today;
 date_ptr->month = 9;
 date_ptr->day = 25;
 date_ptr->year = 1983;

 fprintf(Outfile1,"Todays date is %d/%d/%d.\n", date_ptr->month,
        date_ptr->day, date_ptr -> year%100);

}

t60()
{
 struct record item;
 void editrecord( struct record * );
 fprintf(Outfile1,"\nTest 60\n");

 editrecord( &item );
 strcpy( item.name, "Red Plum Jam");
 item.price = 2.75;
 fprintf(Outfile1,"Name = %s\n", item.name );
 fprintf(Outfile1,"ID = %d\n", item.id);
 fprintf(Outfile1,"Price = %.2f\n", item.price );
}

void editrecord( struct record *goods )
{
 strcpy( goods->name, "Baked Beans" );
 goods->id = 220;
 goods->price = 2.20;
 fprintf(Outfile1,"Name = %s\n", goods->name );
 fprintf(Outfile1,"ID = %d\n", goods->id);
 fprintf(Outfile1,"Price = %f\n", goods->price );
}

t61()   /* LLIST.C    Program to illustrate linked lists */
{
 struct list n1, n2, n3;
 int i;

 fprintf(Outfile1,"\nTest 61\n");
 n1.value = 100;
 n2.value = 200;
 n3.value = 300;
 n1.next = &n2;
 n2.next = &n3;
 i = n1.next->value;
 fprintf(Outfile1,"%d\n", n2.next->value);
}

t62()
{ /*example for testing enums*/
 enum list1 {a,b,c};
 enum strings {d='a',e,f};
 enum list2 {g=4,h,i};
 fprintf(Outfile1,"\nTest 62\n");
 fprintf(Outfile1,"%d %d %d\n",a,b,c);
 fprintf(Outfile1,"%c %c %c\n",d,e,f);
 fprintf(Outfile1,"%d %d %d\n",g,h,i);
}

t63()   /*  declare a variable using previous declared enum list */
{
 char *pwest = "west",*pnorth = "north", *peast="east", *psouth = "south";
 enum location { east=1, west=2, south=3, north=4};
 enum location direction;

 fprintf(Outfile1,"\nTest 63\n");
 direction = east;
 if( direction == east )
   fprintf(Outfile1,"Cannot go %s\n", peast);
}

t64()
{
 int intervall(int n,REAL A,REAL X[]);
 REAL x[4]={.1,.4,.5,.7};
 fprintf(Outfile1,"\nTest 64\n");
 fprintf(Outfile1,"%d\n",intervall(3,.6,x));
}

int intervall    /* Find the number for a value inside a partition ...*/
             (
              int n,         /* lenght of partition ..................*/
              REAL xwert,    /* number whose interval index is wanted */
              REAL x[]       /* partition ............................*/
             )               /* Index for xwert ......................*/

/***********************************************************************
* For a given interval partition x[i], i = 0,1,...,n with real         *
* monotonically increasing values for the x[i], we compute the index   *
* ix for which x[ix] <= xwert < x[ix+1] holds.                         *
* If xwert < x[0] or xwert >= x[n-1], we set ix = 0 or ix = n-1.       *
* Thus ix has the return value of between 0 and n-1.                   *
* This is a standard function for use with spline evaluations.         *
*                                                                      *
* Input parameters:                                                    *
* =================                                                    *
* n:     Index of final node of partition                              *
* xwert: value whose index is desired                                  *
* x:     [0..n] vector with the partition                              *
*                                                                      *
* Return value :                                                       *
* ==============                                                       *
* desired index ix for xwert                                           *
*                                                                      *
* global names used:                                                   *
* ==================                                                   *
* REAL                                                                 *
***********************************************************************/

{
  int ix,
      m;

  for (ix = 0; m = (ix + n) >> 1, m != ix; )
    if (xwert < x[m])
      n = m;
    else
      ix = m;
  return ix;
}

t65(void)   /* testing the quicksort of Hoare */
{
 int I,N;
 char ch;
 char A[] = "thequickbrownfoxjumpsoverthelazydog";
 void QUICKSORT(char *,int,int);

 fprintf(Outfile1,"\nTest 65\n");
 N = sizeof(A);
 QUICKSORT(A,0,N-1);
 for (I=0; I<N; I++) fprintf(Outfile1,"%c",A[I]);
 fprintf(Outfile1,"\n");
}

void QUICKSORT(char *P, int L, int R)
{
 register int I,J;  char CH,TEMP;

 I = L; J = R;  CH = P[(L+R)/2];

 do
  {
    while (P[I]<CH && I<R) I=I+1;
    while (CH<P[J] && J>L) J--;
    if (I<=J) {TEMP = P[I]; P[I]=P[J]; P[J]=TEMP; I++; J--;}
  }
 while (I<=J);
 if (L<J) QUICKSORT(P,L,J);
 if (I<R) QUICKSORT(P,I,R);
}

t66()    /* perform arithmetic on array arg.  */
{
 REAL x[5]={0.1,0.2,0.3,0.4,0.5};
 REAL norm_max(REAL x[],int n);

 fprintf(Outfile1,"\nTest 66\n");
 fprintf(Outfile1,"%f\n",norm_max(x,5));
}

REAL norm_max      /* Find the maximum norm of a REAL vector .........*/
             (
              REAL vektor[],               /* vector .................*/
              int  n                       /* length of vector .......*/
             )                             /* Maximum norm ...........*/

/***********************************************************************
* Return the maximum norm of a [0..n-1] vector  v.                     *
*                                                                      *
* global names used:                                                   *
* ==================                                                   *
* REAL, FABS, ZERO                                                     *
***********************************************************************/

{
  REAL norm,                                             /* local max */
       betrag;                            /* magnitude of a component */

  for (n--, norm = 0; n >= 0; n--, vektor++)
    if ((betrag = fabs(*vektor)) > norm)
      norm = betrag;

  return norm;
}

t67()
{
 REAL x[4]={1,2,3,4},y[4];
 REAL skalprod(REAL V[],REAL W[], int n);
 void copy_vector(REAL V[],REAL W[], int n);
 copy_vector(y,x,4);

 fprintf(Outfile1,"\nTest 67\n");
 fprintf(Outfile1,"%f\n",skalprod(x,y,4));
}

REAL skalprod           /* standard scalar product of two REAL vectors*/
        (
         REAL v[],                 /* 1st vector .....................*/
         REAL w[],                 /* 2nd vector .....................*/
         int  n                    /* vector length...................*/
        )                          /* scalar product .................*/

/***********************************************************************
* compute the scalar product   v[0] * w[0] + ... + v[n-1] * w[n-1]  of *
* the two [0..n-1] vectors v and w                                     *
*                                                                      *
* Global names used:                                                   *
* ==================                                                   *
* REAL, ZERO                                                           *
***********************************************************************/

{
  REAL skalarprodukt;

  for (skalarprodukt = ZERO; n-- != 0; )
    skalarprodukt += (*v++) * (*w++);

  return skalarprodukt;
}

void copy_vector(        /* copy a REAL vector ........................*/
                 REAL ziel[],            /* copied vector ............*/
                 REAL quelle[],          /* original vector ..........*/
                 int  n                  /* length of vector .........*/
                )

/***********************************************************************
* copy the n elements of the vector quelle into the vector ziel.       *
*                                                                      *
* global names used:                                                   *
* ==================                                                   *
* REAL                                                                 *
***********************************************************************/

{
  for (n--; n >= 0; n--)
    *ziel++ = *quelle++;
}

t68()
{
 REAL x=4,y=6;
 REAL comabs(REAL a, REAL b);
 fprintf(Outfile1,"\nTest 68\n");
 fprintf(Outfile1,"%0.5f %0.5f\n",comabs(x,y),comabs(6.,4.));
}

REAL comabs             /* Complex absolute value ....................*/
              (
               REAL  ar,          /* Real part .......................*/
               REAL  ai           /* Imaginary part ..................*/
              )
/*====================================================================*
 *                                                                    *
 *  Complex absolute value of   a                                     *
 *                                                                    *
 *====================================================================*
 *                                                                    *
 *   Input parameters:                                                *
 *   ================                                                 *
 *      ar,ai    REAL   ar, ai;                                       *
 *               Real, imaginary parts of  a                          *
 *                                                                    *
 *   Return value :                                                   *
 *   =============                                                    *
 *      Absolute value of a                                           *
 *                                                                    *
 *   Macros used :    SQRT, ABS, SWAP                                 *
 *   =============                                                    *
 *                                                                    *
 *====================================================================*/
{
 void SWAP( REAL *X, REAL *Y);
  if (ar == ZERO && ai == ZERO) return (ZERO);

  ar = fabs(ar);
  ai = fabs (ai);

  if (ai > ar)                                  /* Switch  ai and ar */
    SWAP (&ai, &ar);

  return ((ai == ZERO) ? (ar) : (ar * sqrt (ONE + ai / ar * ai / ar)));
}
void SWAP(REAL *A, REAL *B)

{
REAL temp;
temp = *A;
*A = *B;
*B = temp;
}

t69() /*test while(i--)    while(--j)     while(1)  */
{
 int i=3,j=3,k=3;

 fprintf(Outfile1,"\nTest 69\n");
 while(i--) fprintf(Outfile1,"%d\n",i);
 while(--j) fprintf(Outfile1,"%d\n",j);
 while(1) {
            fprintf(Outfile1,"%d\n",k);
            k--;
            if(k==0) break;
          }
}

t70()
{/* test of for with character variable
         and ? operator within printf */

 int a=12,b=3, k;
 float c=14.,d=34.;
 char x;

 fprintf(Outfile1,"\nTest 70\n");
 for(x='a';x<='z';x++)
   fprintf(Outfile1,"%c",x);
 fprintf(Outfile1,"\n");

 for(k=1,x='a';x<='z';x++)     /* force do while translation */
   fprintf(Outfile1,"%c",x);
 fprintf(Outfile1,"\n");

 fprintf(Outfile1,"%d\n",(a>b ? 12 : 23) );
 fprintf(Outfile1,"%f\n",(c>d ? 3.14 : 2.7) );
 fprintf(Outfile1,"%c\n",('e'>'b' ? 'e' : 'b') );

}

t71()   /* test unsigned nnn  <no type>
           (cast)(x<<y) nesting
           0x7FFFFF hex constant */
{
 unsigned j=12;
 long to_int(float in);

 fprintf(Outfile1,"\nTest 71\n");
 fprintf(Outfile1,"%d\n",j);
 fprintf(Outfile1,"%d\n",to_int(3.1416));
}
long to_int(float in)
{
 union { float x; long y; } z;
 long out;

 z.x = in + (float)(1<<23);
 out = (z.y & 0x7FFFFF) >> 1;
 return out;
}

t72()
/*test character constants*/
{
 char x[5];
 fprintf(Outfile1,"\nTest 72\n");
 x[0]='\\';
 x[1]='\67';
 x[2]='\067';
 x[3]='\t'; /* char(9)*/
 x[4]='\n'; /* char(13)*/
 fprintf(Outfile1,"%c %c %c\n",x[0],x[1],x[2]);
 fprintf(Outfile1,"%d %d\n",(int)x[3],(int)x[4]);
}

t73()
{
 REAL maxroot();
 fprintf(Outfile1,"\nTest 73\n");
 fprintf (Outfile1,"%f\n",maxroot());
}

REAL maxroot(void)    /* Root of the largest representable number ....*/

/***********************************************************************
* Compute the square root of the largest machine number 2 ^ (MAX_EXP/2)*
* if not already done                                                  *
*                                                                      *
* global names used:                                                   *
* ==================                                                   *
* REAL, boolean, FALSE, TRUE, SQRT, POSMAX                             *
***********************************************************************/

{
  static REAL       save_maxroot;
  static BOOLEAN    passed = FALSE;
  REAL              faktor;
  unsigned long int n;

  if (! passed)
  {
    save_maxroot = ONE;
    faktor       = TWO;
    for (n = MAX_EXP / 2; n > 1; n/=2, faktor *= faktor)
      if (n % 2 != 0)
        save_maxroot *= faktor;
    save_maxroot    *= faktor;
    passed  = TRUE;
  }

  return save_maxroot;
}

t74() /* check global target generation */
{
 float *b;

 fprintf(Outfile1,"\nTest 74\n");
 b = xglobal;
 b[1] =3;
 fprintf(Outfile1,"%f\n",xglobal[1]);
}

t75()
{
 typedef enum FileType
  {
  FT_IMAGE    = 1,
  FT_POINT    = 1 << 1,
  FT_LINE     = 1 << 2,
  FT_POLYGON  = 1 << 3,
  FT_UNKNOWN  = -1
  } FileType ;

 FileType a,b,c;

 fprintf(Outfile1,"\nTest 75\n");
 a = FT_IMAGE;

 fprintf (Outfile1,"%d %d %d %d %d\n",FT_IMAGE,FT_POINT,FT_LINE,FT_POLYGON,
                                      FT_UNKNOWN);
}

t76()
{
 char s[4][8];
 int i;

 fprintf(Outfile1,"\nTest 76\n");
 strcpy(s[0],"abc");
 strcpy(s[1],"def");
 strcpy(s[2],"ghi");
 strcpy(s[3],"jkl");

 for (i=0;i<4;i++)
   fprintf(Outfile1,"%s\n",s[i]);
 if(strcmp(s[3],s[1]) > 0) fprintf(Outfile1,"%s > %s\n",s[3],s[0]);
}

t77()  /* test memset */
{
 void memset_func();
 char x[4];
 int i;

 fprintf(Outfile1,"\nTest 77\n");
 memset(x,'a',4);
 for(i=0;i<4;i++) fprintf(Outfile1,"%c",x[i]);
 memset(x,67,4);
 for(i=0;i<4;i++) fprintf(Outfile1,"%c",x[i]);
 fprintf(Outfile1,"\n");
 memset_func(x);
}
void memset_func(char *x)
{
 int i;
 memset(x,'a',4);
 for(i=0;i<4;i++) fprintf(Outfile1,"%c",x[i]);
 memset(x,67,4);
 for(i=0;i<4;i++) fprintf(Outfile1,"%c",x[i]);
 fprintf(Outfile1,"\n");
}

t78()   /* test *=  /=  operators */
{
 float x=3,y=1,f=3;

 fprintf(Outfile1,"\nTest 78\n");
 x *= f + 5;
 y /= 2 + f;
 fprintf(Outfile1,"%f %f\n",x,y);
}

t79()
{
 signed a=12;
 signed int q=7;
 fprintf(Outfile1,"\nTest 79\n");
 fprintf(Outfile1,"%d %d\n",a,q);
}

typedef void *Pointer;

t80()    // check use of void pointers in arguments
{
 int x=1, y=2 ;
 void *j,*k;
 void p_swap(Pointer *a, Pointer *b);

 fprintf(Outfile1,"\nTest 80\n");
 j = (void *)&x;
 k = (void *)&y;
 fprintf(Outfile1,"%d %d\n",*(int *)j,*(int *)k);
 p_swap(&j,&k );
 fprintf(Outfile1,"%d %d\n",*(int *)j,*(int *)k);
}

void p_swap(Pointer *a, Pointer *b)
{
  Pointer tmp = *a;
  *a = *b;
  *b = tmp;
}

t81()   //  create DEC pointer to printf args
{
  struct S {char *name; int number;};
  struct S s1, s2;
  Pointer sp1, sp2;

  fprintf(Outfile1,"\nTest 81\n");
  s1.name = "name of 1";
  s2.name = "name of 2";
  s1.number =  200;
  s2.number =  400;
  sp1 = &s1;
  sp2 = &s2;
  p_swap(&sp1, &sp2);

  fprintf(Outfile1,"%s\n", ((struct S *)sp1)->name);
  fprintf(Outfile1,"%d\n", ((struct S *)sp1)->number);
}

t82()  // test function returning a struct result
{
 fcomplex x, y, z;
 fcomplex Cadd(fcomplex a, fcomplex b);

 fprintf(Outfile1,"\nTest 82\n");
 x.r = 10; y.r = 9;
 x.i = 5;  y.i = 6;
 z = Cadd(x,y);
 fprintf(Outfile1,"z.r = %f\n", z.r);
 fprintf(Outfile1,"z.i = %f\n", z.i);
}

fcomplex Cadd(fcomplex a, fcomplex b)
{
 fcomplex c;
 c.r=a.r+b.r;
 c.i=a.i+b.i;
 return c;
}

t83()    // test allocation of struct pointer
{
 fcomplex x, *v;

 fprintf(Outfile1,"\nTest 83\n");
 v = (fcomplex *)malloc((size_t)sizeof(fcomplex));
 if (!v) fprintf(Outfile1,"allocation failure in cvector()");
 x.r = 10;
 x.i = 5;
 (*v).r = 1;
 fprintf(Outfile1,"x.r = %f\n", x.r);
 fprintf(Outfile1,"*v.r = %f\n", (*v).r);
}

t84()  /* test void pointer in struct member
          generating VOID_TO_XXX functions in CONTAINS section  */
{
 int x[7], *k, i;
 Testpt a,aa;
 DerivedInt xx[7], *kk;

 fprintf(Outfile1,"\nTest 84\n");
 a.pt = (void *)&x;
 k = (int *) a.pt;

 aa.pt = (void *)&xx;
 kk = (DerivedInt *) aa.pt;

 for(i=0;i<7;i++)
 {
 x[i] = i;
 xx[i].aa = i*i;
 fprintf(Outfile1,"%d %d\n",k[i],kk[i]);
 }
}

t85()   // more advanced test on ? operator
{
 double x,y,z,ds;
 double *s, *v;
 int ns, n, m;

 fprintf(Outfile1,"\nTest 85\n");
 x = 10;
 y = 20;
 s = (double *)malloc((size_t) 10*sizeof(double));
 v = (double *)malloc((size_t) 10*sizeof(double));

 z = (x > y)?y:y+20;
 fprintf(Outfile1,"z=%f, x = %f, y = %f \n",z, x, y);

 z = x > y?y:y+20;
 fprintf(Outfile1,"z=%f, x = %f, y = %f \n",z, x, y);

 z = (x > y)?y++:++y+20;
 fprintf(Outfile1,"z=%f, x = %f, y = %f \n",z, x, y);

 z = 0;
 z += (x > y)?y:y+20;
 fprintf(Outfile1,"z=%f, x = %f, y = %f \n",z, x, y);

 z = 0;
 z += x = (x > y)?y:y+20;
 fprintf(Outfile1,"z=%f, x = %f, y = %f \n",z, x, y);

 *s = 10;
 *v = 1;
 *s += (x > y)?*v+1:y+20;
 fprintf(Outfile1,"z=%f, x = %f, y = %f *s = %f\n",z, x, y,*s);

 ns = 2;
 n = 1; m = 2;
 s[ns+1] = 2;
 v[ns]   = 3;
 *s += (ds=(2*ns < (n-m) ? s[ns+1] : v[ns--]));
 fprintf(Outfile1,"z=%f, x = %f, y = %f *s = %f ns = %d\n", z, x, y,*s, ns);

}

t86()  // function pointers
{
  double Integrand(double), (*pfunc)(double);   
  double AlterIntegrand(double);
  double midpnt(double(*)(double),double,double);
  double (*choose)(double(*)(double),double,double);
  double qromo(double (*)(double),double,double,double (*)(double(*)(double),double,double));

  double a, b, z, z1;
 
  fprintf(Outfile1,"\nTest 86\n");
  a = 3; b = 5; 

  pfunc = Integrand;
  choose = midpnt;
  z  = midpnt(pfunc, a, b);
  z1 = qromo(pfunc, a, b, choose);
  fprintf(Outfile1,"z=%f\n",z);
  fprintf(Outfile1,"z1=%f\n",z1);
  
  pfunc = AlterIntegrand;
  z  = midpnt(pfunc, a, b);
  z1 = qromo(pfunc, a, b, choose);
  fprintf(Outfile1,"z=%f\n",z);
  fprintf(Outfile1,"z1=%f\n",z1);
}

double Integrand(double y)  /* just a simple test function */
{
  double x;
  x = y*y;
  return x; 
}

double AlterIntegrand(double y)  /* just a simple test function -- Alternative to Integrand*/
{
  double x;
  x = y*y/2;   /* note the difference between two functions !! */
  return x; 
}

// #define FUNC(x) ((*func)(x))    ! no macros desired in debug.c

double midpnt(double (*func)(double), double a, double b) 

/* function which accept function of Integrand type as a parameter --
   not definition of FUNC(x) used further in midpoint */

{
  double z;
  
  return (z = (b-a)*(*func)(0.5*(a+b)));
}

double qromo(double (*func)(double), double a, double b,
	double (*choose)(double(*)(double), double, double))

/* function which accept BOTH functions (Integrand and midpnt) as a parameter --
   not definition of FUNC(x) used futher in midpoint */

{
	double x;
	x = (*choose)(func,a,b);
	return x;
}

void t87( int argc, char *argv[])
{
  int n;
  fprintf(Outfile1,"\nTest 87\n");

  for(n=0;n<argc;n++)
    fprintf(Outfile1,"arg %d  = %s\n",n,argv[n]);
  fprintf(Outfile1,"1st ch = %c\n", argv[0][0]);
}


t88()
{
 void fWriteList(llist *p);
 llist a,b;
 char x[]="item1 12345";
 char y[]="item2 1234567890";

 fprintf(Outfile1,"\nTest 88\n");
 a.entry = (void *) &x;
 b.entry = (void *) &y;
 a.next = &b;
 b.next = NULL;
 fprintf(Outfile1,"Calling fWriteList\n");
 fWriteList(&a);
}

void fWriteList( llist *p)
 {  llist *temp;
  int i = 0;
  for( temp = p; temp != NULL; temp = temp->next ) {
  fprintf(Outfile1, "    %d : %s\n", ++i, (char*)(*temp).entry);
  }
 }
