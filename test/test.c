/* test extension for PHP */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "php.h"
#include "ext/standard/info.h"
#include "php_test.h"

/* For compatibility with older PHP versions */
#ifndef ZEND_PARSE_PARAMETERS_NONE
#define ZEND_PARSE_PARAMETERS_NONE() \
	ZEND_PARSE_PARAMETERS_START(0, 0) \
	ZEND_PARSE_PARAMETERS_END()
#endif

/* {{{ void test_test1()
 */
PHP_FUNCTION(test_test1)
{
	ZEND_PARSE_PARAMETERS_NONE();

	php_printf("The extension %s is loaded and working!\r\n", "test");
}
/* }}} */

/* {{{ string test_test2( [ string $var ] )
 */
PHP_FUNCTION(test_test2)
{
	char *var = "World";
	size_t var_len = sizeof("World") - 1;
	zend_string *retval;

	ZEND_PARSE_PARAMETERS_START(0, 1)
		Z_PARAM_OPTIONAL
		Z_PARAM_STRING(var, var_len)
	ZEND_PARSE_PARAMETERS_END();

	retval = strpprintf(0, "Hello %s", var);

	RETURN_STR(retval);
}
/* }}}*/

/* {{{ PHP_RINIT_FUNCTION
 */
PHP_RINIT_FUNCTION(test)
{
#if defined(ZTS) && defined(COMPILE_DL_TEST)
	ZEND_TSRMLS_CACHE_UPDATE();
#endif

	return SUCCESS;
}
/* }}} */

/* {{{ PHP_MINFO_FUNCTION
 */
PHP_MINFO_FUNCTION(test)
{
	php_info_print_table_start();
	php_info_print_table_header(2, "test support", "enabled");
	php_info_print_table_end();
}
/* }}} */

/* {{{ arginfo
 */
ZEND_BEGIN_ARG_INFO(arginfo_test_test1, 0)
ZEND_END_ARG_INFO()

ZEND_BEGIN_ARG_INFO(arginfo_test_test2, 0)
	ZEND_ARG_INFO(0, str)
ZEND_END_ARG_INFO()
/* }}} */

/* {{{ test_functions[]
 */
static const zend_function_entry test_functions[] = {
	PHP_FE(test_test1,		arginfo_test_test1)
	PHP_FE(test_test2,		arginfo_test_test2)
	PHP_FE_END
};
/* }}} */

/* {{{ test_module_entry
 */
zend_module_entry test_module_entry = {
	STANDARD_MODULE_HEADER,
	"test",					/* Extension name */
	test_functions,			/* zend_function_entry */
	NULL,							/* PHP_MINIT - Module initialization */
	NULL,							/* PHP_MSHUTDOWN - Module shutdown */
	PHP_RINIT(test),			/* PHP_RINIT - Request initialization */
	NULL,							/* PHP_RSHUTDOWN - Request shutdown */
	PHP_MINFO(test),			/* PHP_MINFO - Module info */
	PHP_TEST_VERSION,		/* Version */
	STANDARD_MODULE_PROPERTIES
};
/* }}} */

int main() {
    /**
      cc -I. -I/home/olle/kod/escape-c/test -DPHP_ATOM_INC -I/home/olle/kod/escape-c/test/include -I/home/olle/kod/escape-c/test/main -I/home/olle/kod/escape-c/test -I/usr/include/php/20170718 -I/usr/include/php/20170718/main -I/usr/include/php/20170718/TSRM -I/usr/include/php/20170718/Zend -I/usr/include/php/20170718/ext -I/usr/include/php/20170718/ext/date/lib  $(find ../php-src/ | grep '\.o$' | grep -v libs | grep -v php_cli | grep -v cgi_main | grep -v phpdbg) -g -O2 test.c -lm -ldl -lxml2 -lsqlite3 -lresolv -g -O2 -lrt
     */
    return 1;
}

#ifdef COMPILE_DL_TEST
# ifdef ZTS
ZEND_TSRMLS_CACHE_DEFINE()
# endif
ZEND_GET_MODULE(test)
#endif
