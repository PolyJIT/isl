#ifndef ISL_PRINTER_H
#define ISL_PRINTER_H

#include <stdio.h>
#include <isl/ctx.h>

#if defined(__cplusplus)
extern "C" {
#endif

struct __isl_export isl_printer;
typedef struct isl_printer isl_printer;

__isl_give isl_printer *isl_printer_to_file(isl_ctx *ctx, FILE *file);
__isl_constructor
__isl_give isl_printer *isl_printer_to_str(isl_ctx *ctx);
__isl_null isl_printer *isl_printer_free(__isl_take isl_printer *printer);

isl_ctx *isl_printer_get_ctx(__isl_keep isl_printer *printer);
FILE *isl_printer_get_file(__isl_keep isl_printer *printer);

__isl_export
__isl_give char *isl_printer_get_str(__isl_keep isl_printer *printer);

__isl_export
__isl_give isl_printer *isl_printer_set_indent(__isl_take isl_printer *p,
	int indent);
__isl_export
__isl_give isl_printer *isl_printer_indent(__isl_take isl_printer *p,
	int indent);

#define ISL_FORMAT_ISL			0
#define ISL_FORMAT_POLYLIB		1
#define ISL_FORMAT_POLYLIB_CONSTRAINTS	2
#define ISL_FORMAT_OMEGA		3
#define ISL_FORMAT_C			4
#define ISL_FORMAT_LATEX		5
#define ISL_FORMAT_EXT_POLYLIB		6

enum __isl_export isl_format {
	isl_format_isl = ISL_FORMAT_ISL,
	isl_format_polylib = ISL_FORMAT_POLYLIB,
	isl_format_polylib_constraints = ISL_FORMAT_POLYLIB_CONSTRAINTS,
	isl_format_omega = ISL_FORMAT_OMEGA,
	isl_format_c = ISL_FORMAT_C,
	isl_format_latex = ISL_FORMAT_LATEX,
	isl_format_ext_polylib = ISL_FORMAT_EXT_POLYLIB
};

__isl_export
__isl_give isl_printer *isl_printer_set_output_format(__isl_take isl_printer *p,
	enum isl_format output_format);
__isl_export
enum isl_format isl_printer_get_output_format(__isl_keep isl_printer *p);

__isl_give isl_printer *isl_printer_set_indent_prefix(__isl_take isl_printer *p,
	const char *prefix);
__isl_give isl_printer *isl_printer_set_prefix(__isl_take isl_printer *p,
	const char *prefix);
__isl_give isl_printer *isl_printer_set_suffix(__isl_take isl_printer *p,
	const char *suffix);
__isl_give isl_printer *isl_printer_set_isl_int_width(__isl_take isl_printer *p,
	int width);

__isl_export
__isl_give isl_printer *isl_printer_start_line(__isl_take isl_printer *p);
__isl_export
__isl_give isl_printer *isl_printer_end_line(__isl_take isl_printer *p);
__isl_export
__isl_give isl_printer *isl_printer_print_double(__isl_take isl_printer *p,
	double d);
__isl_export
__isl_give isl_printer *isl_printer_print_int(__isl_take isl_printer *p, int i);
__isl_export
__isl_give isl_printer *isl_printer_print_str(__isl_take isl_printer *p,
	const char *s);

__isl_give isl_printer *isl_printer_flush(__isl_take isl_printer *p);

#if defined(__cplusplus)
}
#endif

#endif
