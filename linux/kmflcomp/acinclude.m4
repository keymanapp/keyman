AC_DEFUN([AM_GNU_GETTEXT_VERSION], [])

AC_DEFUN([AC_YACC_EXPECT],[
AC_MSG_CHECKING([whether $YACC understands %expect])
cat > ac_test.y <<_ACEOF
%{
%}
%union
    { int simple; }
%expect 0
%token <simple> T_SIMP
%type <simple> T_RES
%%
T_RES :
          T_SIMP T_SIMP
        ;
%%
_ACEOF

if ${YACC} ac_test.y 2>/dev/null
then
AC_MSG_RESULT(yes)
rm -f ac_test.y y.tab.c
YACC_PREEXPECT=''
YACC_POSTEXPECT=''
else
AC_MSG_RESULT(no)
rm -f ac_test.y
YACC_PREEXPECT="/*"
YACC_POSTEXPECT="*/"
fi
AC_SUBST(YACC_PREEXPECT)
AC_SUBST(YACC_POSTEXPECT)
])

AC_DEFUN([AC_YACC_ERROR],[
AC_MSG_CHECKING([whether $YACC understands error])
cat > ac_test.y <<_ACEOF
%{
%}
%union
    { int simple; }
%token <simple> T_SIMP
%type <simple> T_RES
%%
T_RES :
          T_SIMP T_SIMP { }
        | error T_SIMP { }
        ;
%%
_ACEOF

if ${YACC} ac_test.y 2>/dev/null
then
AC_MSG_RESULT(yes)
rm -f ac_test.y y.tab.c
YACC_PREERROR=''
YACC_POSTERROR=''
else
AC_MSG_RESULT(no)
rm -f ac_test.y
YACC_PREERROR="/*"
YACC_POSTERROR="*/"
fi
AC_SUBST(YACC_PREERROR)
AC_SUBST(YACC_POSTERROR)
])
