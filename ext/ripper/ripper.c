/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 14 "ripper.y"


#if !YYPURE
# error needs pure parser
#endif
#define YYDEBUG 1
#define YYERROR_VERBOSE 1
#define YYSTACK_USE_ALLOCA 0
#define YYLTYPE rb_code_location_t
#define YYLTYPE_IS_DECLARED 1

#include "ruby/internal/config.h"

#include <ctype.h>
#include <errno.h>
#include <stdio.h>

struct lex_context;

#include "internal.h"
#include "internal/compile.h"
#include "internal/compilers.h"
#include "internal/complex.h"
#include "internal/encoding.h"
#include "internal/error.h"
#include "internal/hash.h"
#include "internal/imemo.h"
#include "internal/io.h"
#include "internal/numeric.h"
#include "internal/parse.h"
#include "internal/rational.h"
#include "internal/re.h"
#include "internal/symbol.h"
#include "internal/thread.h"
#include "internal/variable.h"
#include "node.h"
#include "probes.h"
#include "regenc.h"
#include "ruby/encoding.h"
#include "ruby/regex.h"
#include "ruby/ruby.h"
#include "ruby/st.h"
#include "ruby/util.h"
#include "ruby/ractor.h"
#include "symbol.h"

enum shareability {
    shareable_none,
    shareable_literal,
    shareable_copy,
    shareable_everything,
};

struct lex_context {
    unsigned int in_defined: 1;
    unsigned int in_kwarg: 1;
    unsigned int in_argdef: 1;
    unsigned int in_def: 1;
    unsigned int in_class: 1;
    BITFIELD(enum shareability, shareable_constant_value, 2);
};

#if defined(__GNUC__) && !defined(__clang__)
// Suppress "parameter passing for argument of type 'struct
// lex_context' changed" notes.  `struct lex_context` is file scope,
// and has no ABI compatibility issue.
RBIMPL_WARNING_PUSH()
RBIMPL_WARNING_IGNORED(-Wpsabi)
RBIMPL_WARNING_POP()
// Not sure why effective even after popped.
#endif

#include "parse.h"

#define NO_LEX_CTXT (struct lex_context){0}

#define AREF(ary, i) RARRAY_AREF(ary, i)

#ifndef WARN_PAST_SCOPE
# define WARN_PAST_SCOPE 0
#endif

#define TAB_WIDTH 8

#define yydebug (p->debug)	/* disable the global variable definition */

#define YYMALLOC(size)		rb_parser_malloc(p, (size))
#define YYREALLOC(ptr, size)	rb_parser_realloc(p, (ptr), (size))
#define YYCALLOC(nelem, size)	rb_parser_calloc(p, (nelem), (size))
#define YYFREE(ptr)		rb_parser_free(p, (ptr))
#define YYFPRINTF		rb_parser_printf
#define YY_LOCATION_PRINT(File, loc) \
     rb_parser_printf(p, "%d.%d-%d.%d", \
                      (loc).beg_pos.lineno, (loc).beg_pos.column,\
                      (loc).end_pos.lineno, (loc).end_pos.column)
#define YYLLOC_DEFAULT(Current, Rhs, N)					\
    do									\
      if (N)								\
        {								\
          (Current).beg_pos = YYRHSLOC(Rhs, 1).beg_pos;			\
          (Current).end_pos = YYRHSLOC(Rhs, N).end_pos;			\
        }								\
      else								\
        {                                                               \
          (Current).beg_pos = YYRHSLOC(Rhs, 0).end_pos;                 \
          (Current).end_pos = YYRHSLOC(Rhs, 0).end_pos;                 \
        }                                                               \
    while (0)
#define YY_(Msgid) \
    (((Msgid)[0] == 'm') && (strcmp((Msgid), "memory exhausted") == 0) ? \
     "nesting too deep" : (Msgid))

#define RUBY_SET_YYLLOC_FROM_STRTERM_HEREDOC(Current)			\
    rb_parser_set_location_from_strterm_heredoc(p, &p->lex.strterm->u.heredoc, &(Current))
#define RUBY_SET_YYLLOC_OF_DELAYED_TOKEN(Current)			\
    rb_parser_set_location_of_delayed_token(p, &(Current))
#define RUBY_SET_YYLLOC_OF_HEREDOC_END(Current)				\
    rb_parser_set_location_of_heredoc_end(p, &(Current))
#define RUBY_SET_YYLLOC_OF_DUMMY_END(Current)				\
    rb_parser_set_location_of_dummy_end(p, &(Current))
#define RUBY_SET_YYLLOC_OF_NONE(Current)				\
    rb_parser_set_location_of_none(p, &(Current))
#define RUBY_SET_YYLLOC(Current)					\
    rb_parser_set_location(p, &(Current))
#define RUBY_INIT_YYLLOC() \
    { \
        {p->ruby_sourceline, (int)(p->lex.ptok - p->lex.pbeg)}, \
        {p->ruby_sourceline, (int)(p->lex.pcur - p->lex.pbeg)}, \
    }

enum lex_state_bits {
    EXPR_BEG_bit,		/* ignore newline, +/- is a sign. */
    EXPR_END_bit,		/* newline significant, +/- is an operator. */
    EXPR_ENDARG_bit,		/* ditto, and unbound braces. */
    EXPR_ENDFN_bit,		/* ditto, and unbound braces. */
    EXPR_ARG_bit,		/* newline significant, +/- is an operator. */
    EXPR_CMDARG_bit,		/* newline significant, +/- is an operator. */
    EXPR_MID_bit,		/* newline significant, +/- is an operator. */
    EXPR_FNAME_bit,		/* ignore newline, no reserved words. */
    EXPR_DOT_bit,		/* right after `.', `&.' or `::', no reserved words. */
    EXPR_CLASS_bit,		/* immediate after `class', no here document. */
    EXPR_LABEL_bit,		/* flag bit, label is allowed. */
    EXPR_LABELED_bit,		/* flag bit, just after a label. */
    EXPR_FITEM_bit,		/* symbol literal as FNAME. */
    EXPR_MAX_STATE
};
/* examine combinations */
enum lex_state_e {
#define DEF_EXPR(n) EXPR_##n = (1 << EXPR_##n##_bit)
    DEF_EXPR(BEG),
    DEF_EXPR(END),
    DEF_EXPR(ENDARG),
    DEF_EXPR(ENDFN),
    DEF_EXPR(ARG),
    DEF_EXPR(CMDARG),
    DEF_EXPR(MID),
    DEF_EXPR(FNAME),
    DEF_EXPR(DOT),
    DEF_EXPR(CLASS),
    DEF_EXPR(LABEL),
    DEF_EXPR(LABELED),
    DEF_EXPR(FITEM),
    EXPR_VALUE = EXPR_BEG,
    EXPR_BEG_ANY  =  (EXPR_BEG | EXPR_MID | EXPR_CLASS),
    EXPR_ARG_ANY  =  (EXPR_ARG | EXPR_CMDARG),
    EXPR_END_ANY  =  (EXPR_END | EXPR_ENDARG | EXPR_ENDFN),
    EXPR_NONE = 0
};
#define IS_lex_state_for(x, ls)	((x) & (ls))
#define IS_lex_state_all_for(x, ls) (((x) & (ls)) == (ls))
#define IS_lex_state(ls)	IS_lex_state_for(p->lex.state, (ls))
#define IS_lex_state_all(ls)	IS_lex_state_all_for(p->lex.state, (ls))

# define SET_LEX_STATE(ls) \
    parser_set_lex_state(p, ls, __LINE__)
static inline enum lex_state_e parser_set_lex_state(struct parser_params *p, enum lex_state_e ls, int line);

typedef VALUE stack_type;

static const rb_code_location_t NULL_LOC = { {0, -1}, {0, -1} };

# define SHOW_BITSTACK(stack, name) (p->debug ? rb_parser_show_bitstack(p, stack, name, __LINE__) : (void)0)
# define BITSTACK_PUSH(stack, n) (((p->stack) = ((p->stack)<<1)|((n)&1)), SHOW_BITSTACK(p->stack, #stack"(push)"))
# define BITSTACK_POP(stack)	 (((p->stack) = (p->stack) >> 1), SHOW_BITSTACK(p->stack, #stack"(pop)"))
# define BITSTACK_SET_P(stack)	 (SHOW_BITSTACK(p->stack, #stack), (p->stack)&1)
# define BITSTACK_SET(stack, n)	 ((p->stack)=(n), SHOW_BITSTACK(p->stack, #stack"(set)"))

/* A flag to identify keyword_do_cond, "do" keyword after condition expression.
   Examples: `while ... do`, `until ... do`, and `for ... in ... do` */
#define COND_PUSH(n)	BITSTACK_PUSH(cond_stack, (n))
#define COND_POP()	BITSTACK_POP(cond_stack)
#define COND_P()	BITSTACK_SET_P(cond_stack)
#define COND_SET(n)	BITSTACK_SET(cond_stack, (n))

/* A flag to identify keyword_do_block; "do" keyword after command_call.
   Example: `foo 1, 2 do`. */
#define CMDARG_PUSH(n)	BITSTACK_PUSH(cmdarg_stack, (n))
#define CMDARG_POP()	BITSTACK_POP(cmdarg_stack)
#define CMDARG_P()	BITSTACK_SET_P(cmdarg_stack)
#define CMDARG_SET(n)	BITSTACK_SET(cmdarg_stack, (n))

struct vtable {
    ID *tbl;
    int pos;
    int capa;
    struct vtable *prev;
};

struct local_vars {
    struct vtable *args;
    struct vtable *vars;
    struct vtable *used;
# if WARN_PAST_SCOPE
    struct vtable *past;
# endif
    struct local_vars *prev;
# ifndef RIPPER
    struct {
        NODE *outer, *inner, *current;
    } numparam;
# endif
};

enum {
    ORDINAL_PARAM = -1,
    NO_PARAM = 0,
    NUMPARAM_MAX = 9,
};

#define NUMPARAM_ID_P(id) numparam_id_p(id)
#define NUMPARAM_ID_TO_IDX(id) (unsigned int)(((id) >> ID_SCOPE_SHIFT) - (tNUMPARAM_1 - 1))
#define NUMPARAM_IDX_TO_ID(idx) TOKEN2LOCALID((tNUMPARAM_1 - 1 + (idx)))
static int
numparam_id_p(ID id)
{
    if (!is_local_id(id) || id < (tNUMPARAM_1 << ID_SCOPE_SHIFT)) return 0;
    unsigned int idx = NUMPARAM_ID_TO_IDX(id);
    return idx > 0 && idx <= NUMPARAM_MAX;
}
static void numparam_name(struct parser_params *p, ID id);

#define DVARS_INHERIT ((void*)1)
#define DVARS_TOPSCOPE NULL
#define DVARS_TERMINAL_P(tbl) ((tbl) == DVARS_INHERIT || (tbl) == DVARS_TOPSCOPE)

typedef struct token_info {
    const char *token;
    rb_code_position_t beg;
    int indent;
    int nonspc;
    struct token_info *next;
} token_info;

typedef struct rb_strterm_struct rb_strterm_t;

/*
    Structure of Lexer Buffer:

 lex.pbeg     lex.ptok     lex.pcur     lex.pend
    |            |            |            |
    |------------+------------+------------|
                 |<---------->|
                     token
*/
struct parser_params {
    rb_imemo_tmpbuf_t *heap;

    YYSTYPE *lval;
    YYLTYPE *yylloc;

    struct {
        rb_strterm_t *strterm;
        VALUE (*gets)(struct parser_params*,VALUE);
        VALUE input;
        VALUE lastline;
        VALUE nextline;
        const char *pbeg;
        const char *pcur;
        const char *pend;
        const char *ptok;
        union {
            long ptr;
            VALUE (*call)(VALUE, int);
        } gets_;
        enum lex_state_e state;
        /* track the nest level of any parens "()[]{}" */
        int paren_nest;
        /* keep p->lex.paren_nest at the beginning of lambda "->" to detect tLAMBEG and keyword_do_LAMBDA */
        int lpar_beg;
        /* track the nest level of only braces "{}" */
        int brace_nest;
    } lex;
    stack_type cond_stack;
    stack_type cmdarg_stack;
    int tokidx;
    int toksiz;
    int tokline;
    int heredoc_end;
    int heredoc_indent;
    int heredoc_line_indent;
    char *tokenbuf;
    struct local_vars *lvtbl;
    st_table *pvtbl;
    st_table *pktbl;
    int line_count;
    int ruby_sourceline;	/* current line no. */
    const char *ruby_sourcefile; /* current source file */
    VALUE ruby_sourcefile_string;
    rb_encoding *enc;
    token_info *token_info;
    VALUE case_labels;
    VALUE compile_option;

    VALUE debug_buffer;
    VALUE debug_output;

    struct {
        VALUE token;
        int beg_line;
        int beg_col;
        int end_line;
        int end_col;
    } delayed;

    ID cur_arg;

    rb_ast_t *ast;
    int node_id;

    int max_numparam;

    struct lex_context ctxt;

    unsigned int command_start:1;
    unsigned int eofp: 1;
    unsigned int ruby__end__seen: 1;
    unsigned int debug: 1;
    unsigned int has_shebang: 1;
    unsigned int token_seen: 1;
    unsigned int token_info_enabled: 1;
# if WARN_PAST_SCOPE
    unsigned int past_scope_enabled: 1;
# endif
    unsigned int error_p: 1;
    unsigned int cr_seen: 1;

#ifndef RIPPER
    /* Ruby core only */

    unsigned int do_print: 1;
    unsigned int do_loop: 1;
    unsigned int do_chomp: 1;
    unsigned int do_split: 1;
    unsigned int keep_script_lines: 1;
    unsigned int error_tolerant: 1;
    unsigned int keep_tokens: 1;

    NODE *eval_tree_begin;
    NODE *eval_tree;
    VALUE error_buffer;
    VALUE debug_lines;
    const struct rb_iseq_struct *parent_iseq;
    /* store specific keyword locations to generate dummy end token */
    VALUE end_expect_token_locations;
    /* id for terms */
    int token_id;
    /* Array for term tokens */
    VALUE tokens;
#else
    /* Ripper only */

    VALUE value;
    VALUE result;
    VALUE parsing_thread;
#endif
};

#define intern_cstr(n,l,en) rb_intern3(n,l,en)

#define STR_NEW(ptr,len) rb_enc_str_new((ptr),(len),p->enc)
#define STR_NEW0() rb_enc_str_new(0,0,p->enc)
#define STR_NEW2(ptr) rb_enc_str_new((ptr),strlen(ptr),p->enc)
#define STR_NEW3(ptr,len,e,func) parser_str_new((ptr),(len),(e),(func),p->enc)
#define TOK_INTERN() intern_cstr(tok(p), toklen(p), p->enc)

static st_table *
push_pvtbl(struct parser_params *p)
{
    st_table *tbl = p->pvtbl;
    p->pvtbl = st_init_numtable();
    return tbl;
}

static void
pop_pvtbl(struct parser_params *p, st_table *tbl)
{
    st_free_table(p->pvtbl);
    p->pvtbl = tbl;
}

static st_table *
push_pktbl(struct parser_params *p)
{
    st_table *tbl = p->pktbl;
    p->pktbl = 0;
    return tbl;
}

static void
pop_pktbl(struct parser_params *p, st_table *tbl)
{
    if (p->pktbl) st_free_table(p->pktbl);
    p->pktbl = tbl;
}

#ifndef RIPPER
static void flush_debug_buffer(struct parser_params *p, VALUE out, VALUE str);

static void
debug_end_expect_token_locations(struct parser_params *p, const char *name)
{
    if(p->debug) {
        VALUE mesg = rb_sprintf("%s: ", name);
        rb_str_catf(mesg, " %"PRIsVALUE"\n", p->end_expect_token_locations);
        flush_debug_buffer(p, p->debug_output, mesg);
    }
}

static void
push_end_expect_token_locations(struct parser_params *p, const rb_code_position_t *pos)
{
    if(NIL_P(p->end_expect_token_locations)) return;
    rb_ary_push(p->end_expect_token_locations, rb_ary_new_from_args(2, INT2NUM(pos->lineno), INT2NUM(pos->column)));
    debug_end_expect_token_locations(p, "push_end_expect_token_locations");
}

static void
pop_end_expect_token_locations(struct parser_params *p)
{
    if(NIL_P(p->end_expect_token_locations)) return;
    rb_ary_pop(p->end_expect_token_locations);
    debug_end_expect_token_locations(p, "pop_end_expect_token_locations");
}

static VALUE
peek_end_expect_token_locations(struct parser_params *p)
{
    if(NIL_P(p->end_expect_token_locations)) return Qnil;
    return rb_ary_last(0, 0, p->end_expect_token_locations);
}

static ID
parser_token2id(enum yytokentype tok)
{
    switch ((int) tok) {
#define TOKEN2ID(tok) case tok: return rb_intern(#tok);
#define TOKEN2ID2(tok, name) case tok: return rb_intern(name);
      TOKEN2ID2(' ', "words_sep")
      TOKEN2ID2('!', "!")
      TOKEN2ID2('%', "%");
      TOKEN2ID2('&', "&");
      TOKEN2ID2('*', "*");
      TOKEN2ID2('+', "+");
      TOKEN2ID2('-', "-");
      TOKEN2ID2('/', "/");
      TOKEN2ID2('<', "<");
      TOKEN2ID2('=', "=");
      TOKEN2ID2('>', ">");
      TOKEN2ID2('?', "?");
      TOKEN2ID2('^', "^");
      TOKEN2ID2('|', "|");
      TOKEN2ID2('~', "~");
      TOKEN2ID2(':', ":");
      TOKEN2ID2(',', ",");
      TOKEN2ID2('.', ".");
      TOKEN2ID2(';', ";");
      TOKEN2ID2('`', "`");
      TOKEN2ID2('\n', "nl");
      TOKEN2ID2('{', "{");
      TOKEN2ID2('}', "}");
      TOKEN2ID2('[', "[");
      TOKEN2ID2(']', "]");
      TOKEN2ID2('(', "(");
      TOKEN2ID2(')', ")");
      TOKEN2ID2('\\', "backslash");
      TOKEN2ID(keyword_class);
      TOKEN2ID(keyword_module);
      TOKEN2ID(keyword_def);
      TOKEN2ID(keyword_undef);
      TOKEN2ID(keyword_begin);
      TOKEN2ID(keyword_rescue);
      TOKEN2ID(keyword_ensure);
      TOKEN2ID(keyword_end);
      TOKEN2ID(keyword_if);
      TOKEN2ID(keyword_unless);
      TOKEN2ID(keyword_then);
      TOKEN2ID(keyword_elsif);
      TOKEN2ID(keyword_else);
      TOKEN2ID(keyword_case);
      TOKEN2ID(keyword_when);
      TOKEN2ID(keyword_while);
      TOKEN2ID(keyword_until);
      TOKEN2ID(keyword_for);
      TOKEN2ID(keyword_break);
      TOKEN2ID(keyword_next);
      TOKEN2ID(keyword_redo);
      TOKEN2ID(keyword_retry);
      TOKEN2ID(keyword_in);
      TOKEN2ID(keyword_do);
      TOKEN2ID(keyword_do_cond);
      TOKEN2ID(keyword_do_block);
      TOKEN2ID(keyword_do_LAMBDA);
      TOKEN2ID(keyword_return);
      TOKEN2ID(keyword_yield);
      TOKEN2ID(keyword_super);
      TOKEN2ID(keyword_self);
      TOKEN2ID(keyword_nil);
      TOKEN2ID(keyword_true);
      TOKEN2ID(keyword_false);
      TOKEN2ID(keyword_and);
      TOKEN2ID(keyword_or);
      TOKEN2ID(keyword_not);
      TOKEN2ID(modifier_if);
      TOKEN2ID(modifier_unless);
      TOKEN2ID(modifier_while);
      TOKEN2ID(modifier_until);
      TOKEN2ID(modifier_rescue);
      TOKEN2ID(keyword_alias);
      TOKEN2ID(keyword_defined);
      TOKEN2ID(keyword_BEGIN);
      TOKEN2ID(keyword_END);
      TOKEN2ID(keyword__LINE__);
      TOKEN2ID(keyword__FILE__);
      TOKEN2ID(keyword__ENCODING__);
      TOKEN2ID(tIDENTIFIER);
      TOKEN2ID(tFID);
      TOKEN2ID(tGVAR);
      TOKEN2ID(tIVAR);
      TOKEN2ID(tCONSTANT);
      TOKEN2ID(tCVAR);
      TOKEN2ID(tLABEL);
      TOKEN2ID(tINTEGER);
      TOKEN2ID(tFLOAT);
      TOKEN2ID(tRATIONAL);
      TOKEN2ID(tIMAGINARY);
      TOKEN2ID(tCHAR);
      TOKEN2ID(tNTH_REF);
      TOKEN2ID(tBACK_REF);
      TOKEN2ID(tSTRING_CONTENT);
      TOKEN2ID(tREGEXP_END);
      TOKEN2ID(tDUMNY_END);
      TOKEN2ID(tSP);
      TOKEN2ID(tUPLUS);
      TOKEN2ID(tUMINUS);
      TOKEN2ID(tPOW);
      TOKEN2ID(tCMP);
      TOKEN2ID(tEQ);
      TOKEN2ID(tEQQ);
      TOKEN2ID(tNEQ);
      TOKEN2ID(tGEQ);
      TOKEN2ID(tLEQ);
      TOKEN2ID(tANDOP);
      TOKEN2ID(tOROP);
      TOKEN2ID(tMATCH);
      TOKEN2ID(tNMATCH);
      TOKEN2ID(tDOT2);
      TOKEN2ID(tDOT3);
      TOKEN2ID(tBDOT2);
      TOKEN2ID(tBDOT3);
      TOKEN2ID(tAREF);
      TOKEN2ID(tASET);
      TOKEN2ID(tLSHFT);
      TOKEN2ID(tRSHFT);
      TOKEN2ID(tANDDOT);
      TOKEN2ID(tCOLON2);
      TOKEN2ID(tCOLON3);
      TOKEN2ID(tOP_ASGN);
      TOKEN2ID(tASSOC);
      TOKEN2ID(tLPAREN);
      TOKEN2ID(tLPAREN_ARG);
      TOKEN2ID(tRPAREN);
      TOKEN2ID(tLBRACK);
      TOKEN2ID(tLBRACE);
      TOKEN2ID(tLBRACE_ARG);
      TOKEN2ID(tSTAR);
      TOKEN2ID(tDSTAR);
      TOKEN2ID(tAMPER);
      TOKEN2ID(tLAMBDA);
      TOKEN2ID(tSYMBEG);
      TOKEN2ID(tSTRING_BEG);
      TOKEN2ID(tXSTRING_BEG);
      TOKEN2ID(tREGEXP_BEG);
      TOKEN2ID(tWORDS_BEG);
      TOKEN2ID(tQWORDS_BEG);
      TOKEN2ID(tSYMBOLS_BEG);
      TOKEN2ID(tQSYMBOLS_BEG);
      TOKEN2ID(tSTRING_END);
      TOKEN2ID(tSTRING_DEND);
      TOKEN2ID(tSTRING_DBEG);
      TOKEN2ID(tSTRING_DVAR);
      TOKEN2ID(tLAMBEG);
      TOKEN2ID(tLABEL_END);
      TOKEN2ID(tIGNORED_NL);
      TOKEN2ID(tCOMMENT);
      TOKEN2ID(tEMBDOC_BEG);
      TOKEN2ID(tEMBDOC);
      TOKEN2ID(tEMBDOC_END);
      TOKEN2ID(tHEREDOC_BEG);
      TOKEN2ID(tHEREDOC_END);
      TOKEN2ID(k__END__);
      TOKEN2ID(tLOWEST);
      TOKEN2ID(tUMINUS_NUM);
      TOKEN2ID(tLAST_TOKEN);
#undef TOKEN2ID
#undef TOKEN2ID2
    }

    rb_bug("parser_token2id: unknown token %d", tok);

    UNREACHABLE_RETURN(0);
}

#endif

RBIMPL_ATTR_NONNULL((1, 2, 3))
static int parser_yyerror(struct parser_params*, const YYLTYPE *yylloc, const char*);
RBIMPL_ATTR_NONNULL((1, 2))
static int parser_yyerror0(struct parser_params*, const char*);
#define yyerror0(msg) parser_yyerror0(p, (msg))
#define yyerror1(loc, msg) parser_yyerror(p, (loc), (msg))
#define yyerror(yylloc, p, msg) parser_yyerror(p, yylloc, msg)
#define token_flush(ptr) ((ptr)->lex.ptok = (ptr)->lex.pcur)
#define lex_goto_eol(p) ((p)->lex.pcur = (p)->lex.pend)
#define lex_eol_p(p) ((p)->lex.pcur >= (p)->lex.pend)
#define lex_eol_n_p(p,n) ((p)->lex.pcur+(n) >= (p)->lex.pend)

static void token_info_setup(token_info *ptinfo, const char *ptr, const rb_code_location_t *loc);
static void token_info_push(struct parser_params*, const char *token, const rb_code_location_t *loc);
static void token_info_pop(struct parser_params*, const char *token, const rb_code_location_t *loc);
static void token_info_warn(struct parser_params *p, const char *token, token_info *ptinfo_beg, int same, const rb_code_location_t *loc);
static void token_info_drop(struct parser_params *p, const char *token, rb_code_position_t beg_pos);

#ifdef RIPPER
#define compile_for_eval	(0)
#else
#define compile_for_eval	(p->parent_iseq != 0)
#endif

#define token_column		((int)(p->lex.ptok - p->lex.pbeg))

#define CALL_Q_P(q) ((q) == TOKEN2VAL(tANDDOT))
#define NODE_CALL_Q(q) (CALL_Q_P(q) ? NODE_QCALL : NODE_CALL)
#define NEW_QCALL(q,r,m,a,loc) NEW_NODE(NODE_CALL_Q(q),r,m,a,loc)

#define lambda_beginning_p() (p->lex.lpar_beg == p->lex.paren_nest)

static enum yytokentype yylex(YYSTYPE*, YYLTYPE*, struct parser_params*);

#ifndef RIPPER
static inline void
rb_discard_node(struct parser_params *p, NODE *n)
{
    rb_ast_delete_node(p->ast, n);
}
#endif

#ifdef RIPPER
static inline VALUE
add_mark_object(struct parser_params *p, VALUE obj)
{
    if (!SPECIAL_CONST_P(obj)
        && !RB_TYPE_P(obj, T_NODE) /* Ripper jumbles NODE objects and other objects... */
    ) {
        rb_ast_add_mark_object(p->ast, obj);
    }
    return obj;
}
#else
static NODE* node_newnode_with_locals(struct parser_params *, enum node_type, VALUE, VALUE, const rb_code_location_t*);
#endif

static NODE* node_newnode(struct parser_params *, enum node_type, VALUE, VALUE, VALUE, const rb_code_location_t*);
#define rb_node_newnode(type, a1, a2, a3, loc) node_newnode(p, (type), (a1), (a2), (a3), (loc))

/* Make a new temporal node, which should not be appeared in the
 * result AST and does not have node_id and location. */
static NODE* node_new_temporal(struct parser_params *p, enum node_type type, VALUE a0, VALUE a1, VALUE a2);
#define NODE_NEW_TEMPORAL(t,a0,a1,a2) node_new_temporal(p, (t),(VALUE)(a0),(VALUE)(a1),(VALUE)(a2))

static NODE *nd_set_loc(NODE *nd, const YYLTYPE *loc);

static int
parser_get_node_id(struct parser_params *p)
{
    int node_id = p->node_id;
    p->node_id++;
    return node_id;
}

#ifndef RIPPER
static inline void
set_line_body(NODE *body, int line)
{
    if (!body) return;
    switch (nd_type(body)) {
      case NODE_RESCUE:
      case NODE_ENSURE:
        nd_set_line(body, line);
    }
}

#define yyparse ruby_yyparse

static NODE* cond(struct parser_params *p, NODE *node, const YYLTYPE *loc);
static NODE* method_cond(struct parser_params *p, NODE *node, const YYLTYPE *loc);
#define new_nil(loc) NEW_NIL(loc)
static NODE *new_nil_at(struct parser_params *p, const rb_code_position_t *pos);
static NODE *new_if(struct parser_params*,NODE*,NODE*,NODE*,const YYLTYPE*);
static NODE *new_unless(struct parser_params*,NODE*,NODE*,NODE*,const YYLTYPE*);
static NODE *logop(struct parser_params*,ID,NODE*,NODE*,const YYLTYPE*,const YYLTYPE*);

static NODE *newline_node(NODE*);
static void fixpos(NODE*,NODE*);

static int value_expr_gen(struct parser_params*,NODE*);
static void void_expr(struct parser_params*,NODE*);
static NODE *remove_begin(NODE*);
static NODE *remove_begin_all(NODE*);
#define value_expr(node) value_expr_gen(p, (node))
static NODE *void_stmts(struct parser_params*,NODE*);
static void reduce_nodes(struct parser_params*,NODE**);
static void block_dup_check(struct parser_params*,NODE*,NODE*);

static NODE *block_append(struct parser_params*,NODE*,NODE*);
static NODE *list_append(struct parser_params*,NODE*,NODE*);
static NODE *list_concat(NODE*,NODE*);
static NODE *arg_append(struct parser_params*,NODE*,NODE*,const YYLTYPE*);
static NODE *last_arg_append(struct parser_params *p, NODE *args, NODE *last_arg, const YYLTYPE *loc);
static NODE *rest_arg_append(struct parser_params *p, NODE *args, NODE *rest_arg, const YYLTYPE *loc);
static NODE *literal_concat(struct parser_params*,NODE*,NODE*,const YYLTYPE*);
static NODE *new_evstr(struct parser_params*,NODE*,const YYLTYPE*);
static NODE *new_dstr(struct parser_params*,NODE*,const YYLTYPE*);
static NODE *evstr2dstr(struct parser_params*,NODE*);
static NODE *splat_array(NODE*);
static void mark_lvar_used(struct parser_params *p, NODE *rhs);

static NODE *call_bin_op(struct parser_params*,NODE*,ID,NODE*,const YYLTYPE*,const YYLTYPE*);
static NODE *call_uni_op(struct parser_params*,NODE*,ID,const YYLTYPE*,const YYLTYPE*);
static NODE *new_qcall(struct parser_params* p, ID atype, NODE *recv, ID mid, NODE *args, const YYLTYPE *op_loc, const YYLTYPE *loc);
static NODE *new_command_qcall(struct parser_params* p, ID atype, NODE *recv, ID mid, NODE *args, NODE *block, const YYLTYPE *op_loc, const YYLTYPE *loc);
static NODE *method_add_block(struct parser_params*p, NODE *m, NODE *b, const YYLTYPE *loc) {b->nd_iter = m; b->nd_loc = *loc; return b;}

static bool args_info_empty_p(struct rb_args_info *args);
static NODE *new_args(struct parser_params*,NODE*,NODE*,ID,NODE*,NODE*,const YYLTYPE*);
static NODE *new_args_tail(struct parser_params*,NODE*,ID,ID,const YYLTYPE*);
static NODE *new_array_pattern(struct parser_params *p, NODE *constant, NODE *pre_arg, NODE *aryptn, const YYLTYPE *loc);
static NODE *new_array_pattern_tail(struct parser_params *p, NODE *pre_args, int has_rest, NODE *rest_arg, NODE *post_args, const YYLTYPE *loc);
static NODE *new_find_pattern(struct parser_params *p, NODE *constant, NODE *fndptn, const YYLTYPE *loc);
static NODE *new_find_pattern_tail(struct parser_params *p, NODE *pre_rest_arg, NODE *args, NODE *post_rest_arg, const YYLTYPE *loc);
static NODE *new_hash_pattern(struct parser_params *p, NODE *constant, NODE *hshptn, const YYLTYPE *loc);
static NODE *new_hash_pattern_tail(struct parser_params *p, NODE *kw_args, ID kw_rest_arg, const YYLTYPE *loc);

static NODE *new_kw_arg(struct parser_params *p, NODE *k, const YYLTYPE *loc);
static NODE *args_with_numbered(struct parser_params*,NODE*,int);

static VALUE negate_lit(struct parser_params*, VALUE);
static NODE *ret_args(struct parser_params*,NODE*);
static NODE *arg_blk_pass(NODE*,NODE*);
static NODE *new_yield(struct parser_params*,NODE*,const YYLTYPE*);
static NODE *dsym_node(struct parser_params*,NODE*,const YYLTYPE*);

static NODE *gettable(struct parser_params*,ID,const YYLTYPE*);
static NODE *assignable(struct parser_params*,ID,NODE*,const YYLTYPE*);

static NODE *aryset(struct parser_params*,NODE*,NODE*,const YYLTYPE*);
static NODE *attrset(struct parser_params*,NODE*,ID,ID,const YYLTYPE*);

static void rb_backref_error(struct parser_params*,NODE*);
static NODE *node_assign(struct parser_params*,NODE*,NODE*,struct lex_context,const YYLTYPE*);

static NODE *new_op_assign(struct parser_params *p, NODE *lhs, ID op, NODE *rhs, struct lex_context, const YYLTYPE *loc);
static NODE *new_ary_op_assign(struct parser_params *p, NODE *ary, NODE *args, ID op, NODE *rhs, const YYLTYPE *args_loc, const YYLTYPE *loc);
static NODE *new_attr_op_assign(struct parser_params *p, NODE *lhs, ID atype, ID attr, ID op, NODE *rhs, const YYLTYPE *loc);
static NODE *new_const_op_assign(struct parser_params *p, NODE *lhs, ID op, NODE *rhs, struct lex_context, const YYLTYPE *loc);
static NODE *new_bodystmt(struct parser_params *p, NODE *head, NODE *rescue, NODE *rescue_else, NODE *ensure, const YYLTYPE *loc);

static NODE *const_decl(struct parser_params *p, NODE* path, const YYLTYPE *loc);

static NODE *opt_arg_append(NODE*, NODE*);
static NODE *kwd_append(NODE*, NODE*);

static NODE *new_hash(struct parser_params *p, NODE *hash, const YYLTYPE *loc);
static NODE *new_unique_key_hash(struct parser_params *p, NODE *hash, const YYLTYPE *loc);

static NODE *new_defined(struct parser_params *p, NODE *expr, const YYLTYPE *loc);

static NODE *new_regexp(struct parser_params *, NODE *, int, const YYLTYPE *);

#define make_list(list, loc) ((list) ? (nd_set_loc(list, loc), list) : NEW_ZLIST(loc))

static NODE *new_xstring(struct parser_params *, NODE *, const YYLTYPE *loc);

static NODE *symbol_append(struct parser_params *p, NODE *symbols, NODE *symbol);

static NODE *match_op(struct parser_params*,NODE*,NODE*,const YYLTYPE*,const YYLTYPE*);

static rb_ast_id_table_t *local_tbl(struct parser_params*);

static VALUE reg_compile(struct parser_params*, VALUE, int);
static void reg_fragment_setenc(struct parser_params*, VALUE, int);
static int reg_fragment_check(struct parser_params*, VALUE, int);
static NODE *reg_named_capture_assign(struct parser_params* p, VALUE regexp, const YYLTYPE *loc);

static int literal_concat0(struct parser_params *p, VALUE head, VALUE tail);
static NODE *heredoc_dedent(struct parser_params*,NODE*);

static void check_literal_when(struct parser_params *p, NODE *args, const YYLTYPE *loc);

#define get_id(id) (id)
#define get_value(val) (val)
#define get_num(num) (num)
#else  /* RIPPER */
#define NODE_RIPPER NODE_CDECL
#define NEW_RIPPER(a,b,c,loc) (VALUE)NEW_CDECL(a,b,c,loc)
#define NODE_RIPPER2 NODE_OP_CDECL
#define NEW_RIPPER2(a,b,c,loc) (VALUE)NEW_OP_CDECL(a,c,b,loc)

static inline int ripper_is_node_yylval(VALUE n);

static inline VALUE
ripper_new_yylval(struct parser_params *p, ID a, VALUE b, VALUE c)
{
    if (ripper_is_node_yylval(c)) c = RNODE(c)->nd_cval;
    add_mark_object(p, b);
    add_mark_object(p, c);
    return NEW_RIPPER(a, b, c, &NULL_LOC);
}

static inline VALUE
ripper_new_yylval2(struct parser_params *p, VALUE a, VALUE b, VALUE c)
{
    add_mark_object(p, a);
    add_mark_object(p, b);
    add_mark_object(p, c);
    return NEW_RIPPER2(a, b, c, &NULL_LOC);
}

static inline int
ripper_is_node_yylval(VALUE n)
{
    return RB_TYPE_P(n, T_NODE) && nd_type_p(RNODE(n), NODE_RIPPER);
}

#define value_expr(node) ((void)(node))
#define remove_begin(node) (node)
#define void_stmts(p,x) (x)
#define rb_dvar_defined(id, base) 0
#define rb_local_defined(id, base) 0
static ID ripper_get_id(VALUE);
#define get_id(id) ripper_get_id(id)
static VALUE ripper_get_value(VALUE);
#define get_value(val) ripper_get_value(val)
#define get_num(num) (int)get_id(num)
static VALUE assignable(struct parser_params*,VALUE);
static int id_is_var(struct parser_params *p, ID id);

#define method_cond(p,node,loc) (node)
#define call_bin_op(p, recv,id,arg1,op_loc,loc) dispatch3(binary, (recv), STATIC_ID2SYM(id), (arg1))
#define match_op(p,node1,node2,op_loc,loc) call_bin_op(0, (node1), idEqTilde, (node2), op_loc, loc)
#define call_uni_op(p, recv,id,op_loc,loc) dispatch2(unary, STATIC_ID2SYM(id), (recv))
#define logop(p,id,node1,node2,op_loc,loc) call_bin_op(0, (node1), (id), (node2), op_loc, loc)

#define new_nil(loc) Qnil

static VALUE new_regexp(struct parser_params *, VALUE, VALUE, const YYLTYPE *);

static VALUE const_decl(struct parser_params *p, VALUE path);

static VALUE var_field(struct parser_params *p, VALUE a);
static VALUE assign_error(struct parser_params *p, const char *mesg, VALUE a);

static VALUE parser_reg_compile(struct parser_params*, VALUE, int, VALUE *);

static VALUE backref_error(struct parser_params*, NODE *, VALUE);
#endif /* !RIPPER */

/* forward declaration */
typedef struct rb_strterm_heredoc_struct rb_strterm_heredoc_t;

RUBY_SYMBOL_EXPORT_BEGIN
VALUE rb_parser_reg_compile(struct parser_params* p, VALUE str, int options);
int rb_reg_fragment_setenc(struct parser_params*, VALUE, int);
enum lex_state_e rb_parser_trace_lex_state(struct parser_params *, enum lex_state_e, enum lex_state_e, int);
VALUE rb_parser_lex_state_name(enum lex_state_e state);
void rb_parser_show_bitstack(struct parser_params *, stack_type, const char *, int);
PRINTF_ARGS(void rb_parser_fatal(struct parser_params *p, const char *fmt, ...), 2, 3);
YYLTYPE *rb_parser_set_location_from_strterm_heredoc(struct parser_params *p, rb_strterm_heredoc_t *here, YYLTYPE *yylloc);
YYLTYPE *rb_parser_set_location_of_delayed_token(struct parser_params *p, YYLTYPE *yylloc);
YYLTYPE *rb_parser_set_location_of_heredoc_end(struct parser_params *p, YYLTYPE *yylloc);
YYLTYPE *rb_parser_set_location_of_dummy_end(struct parser_params *p, YYLTYPE *yylloc);
YYLTYPE *rb_parser_set_location_of_none(struct parser_params *p, YYLTYPE *yylloc);
YYLTYPE *rb_parser_set_location(struct parser_params *p, YYLTYPE *yylloc);
RUBY_SYMBOL_EXPORT_END

static void error_duplicate_pattern_variable(struct parser_params *p, ID id, const YYLTYPE *loc);
static void error_duplicate_pattern_key(struct parser_params *p, ID id, const YYLTYPE *loc);
#ifndef RIPPER
static ID formal_argument(struct parser_params*, ID);
#else
static ID formal_argument(struct parser_params*, VALUE);
#endif
static ID shadowing_lvar(struct parser_params*,ID);
static void new_bv(struct parser_params*,ID);

static void local_push(struct parser_params*,int);
static void local_pop(struct parser_params*);
static void local_var(struct parser_params*, ID);
static void arg_var(struct parser_params*, ID);
static int  local_id(struct parser_params *p, ID id);
static int  local_id_ref(struct parser_params*, ID, ID **);
#ifndef RIPPER
static ID   internal_id(struct parser_params*);
static NODE *new_args_forward_call(struct parser_params*, NODE*, const YYLTYPE*, const YYLTYPE*);
#endif
static int check_forwarding_args(struct parser_params*);
static void add_forwarding_args(struct parser_params *p);

static const struct vtable *dyna_push(struct parser_params *);
static void dyna_pop(struct parser_params*, const struct vtable *);
static int dyna_in_block(struct parser_params*);
#define dyna_var(p, id) local_var(p, id)
static int dvar_defined(struct parser_params*, ID);
static int dvar_defined_ref(struct parser_params*, ID, ID**);
static int dvar_curr(struct parser_params*,ID);

static int lvar_defined(struct parser_params*, ID);

static NODE *numparam_push(struct parser_params *p);
static void numparam_pop(struct parser_params *p, NODE *prev_inner);

#ifdef RIPPER
# define METHOD_NOT idNOT
#else
# define METHOD_NOT '!'
#endif

#define idFWD_REST   '*'
#define idFWD_KWREST idPow /* Use simple "**", as tDSTAR is "**arg" */
#define idFWD_BLOCK  '&'
#define idFWD_ALL    idDot3
#define FORWARD_ARGS_WITH_RUBY2_KEYWORDS

#define RE_OPTION_ONCE (1<<16)
#define RE_OPTION_ENCODING_SHIFT 8
#define RE_OPTION_ENCODING(e) (((e)&0xff)<<RE_OPTION_ENCODING_SHIFT)
#define RE_OPTION_ENCODING_IDX(o) (((o)>>RE_OPTION_ENCODING_SHIFT)&0xff)
#define RE_OPTION_ENCODING_NONE(o) ((o)&RE_OPTION_ARG_ENCODING_NONE)
#define RE_OPTION_MASK  0xff
#define RE_OPTION_ARG_ENCODING_NONE 32

/* structs for managing terminator of string literal and heredocment */
typedef struct rb_strterm_literal_struct {
    union {
        VALUE dummy;
        long nest;
    } u0;
    union {
        VALUE dummy;
        long func;	    /* STR_FUNC_* (e.g., STR_FUNC_ESCAPE and STR_FUNC_EXPAND) */
    } u1;
    union {
        VALUE dummy;
        long paren;	    /* '(' of `%q(...)` */
    } u2;
    union {
        VALUE dummy;
        long term;	    /* ')' of `%q(...)` */
    } u3;
} rb_strterm_literal_t;

#define HERETERM_LENGTH_BITS ((SIZEOF_VALUE - 1) * CHAR_BIT - 1)

struct rb_strterm_heredoc_struct {
    VALUE lastline;	/* the string of line that contains `<<"END"` */
    long offset;	/* the column of END in `<<"END"` */
    int sourceline;	/* lineno of the line that contains `<<"END"` */
    unsigned length	/* the length of END in `<<"END"` */
#if HERETERM_LENGTH_BITS < SIZEOF_INT * CHAR_BIT
    : HERETERM_LENGTH_BITS
# define HERETERM_LENGTH_MAX ((1U << HERETERM_LENGTH_BITS) - 1)
#else
# define HERETERM_LENGTH_MAX UINT_MAX
#endif
    ;
#if HERETERM_LENGTH_BITS < SIZEOF_INT * CHAR_BIT
    unsigned quote: 1;
    unsigned func: 8;
#else
    uint8_t quote;
    uint8_t func;
#endif
};
STATIC_ASSERT(rb_strterm_heredoc_t, sizeof(rb_strterm_heredoc_t) <= 4 * SIZEOF_VALUE);

#define STRTERM_HEREDOC IMEMO_FL_USER0

struct rb_strterm_struct {
    VALUE flags;
    union {
        rb_strterm_literal_t literal;
        rb_strterm_heredoc_t heredoc;
    } u;
};

#ifndef RIPPER
void
rb_strterm_mark(VALUE obj)
{
    rb_strterm_t *strterm = (rb_strterm_t*)obj;
    if (RBASIC(obj)->flags & STRTERM_HEREDOC) {
        rb_strterm_heredoc_t *heredoc = &strterm->u.heredoc;
        rb_gc_mark(heredoc->lastline);
    }
}
#endif

#define yytnamerr(yyres, yystr) (YYSIZE_T)rb_yytnamerr(p, yyres, yystr)
size_t rb_yytnamerr(struct parser_params *p, char *yyres, const char *yystr);

#define TOKEN2ID(tok) ( \
    tTOKEN_LOCAL_BEGIN<(tok)&&(tok)<tTOKEN_LOCAL_END ? TOKEN2LOCALID(tok) : \
    tTOKEN_INSTANCE_BEGIN<(tok)&&(tok)<tTOKEN_INSTANCE_END ? TOKEN2INSTANCEID(tok) : \
    tTOKEN_GLOBAL_BEGIN<(tok)&&(tok)<tTOKEN_GLOBAL_END ? TOKEN2GLOBALID(tok) : \
    tTOKEN_CONST_BEGIN<(tok)&&(tok)<tTOKEN_CONST_END ? TOKEN2CONSTID(tok) : \
    tTOKEN_CLASS_BEGIN<(tok)&&(tok)<tTOKEN_CLASS_END ? TOKEN2CLASSID(tok) : \
    tTOKEN_ATTRSET_BEGIN<(tok)&&(tok)<tTOKEN_ATTRSET_END ? TOKEN2ATTRSETID(tok) : \
    ((tok) / ((tok)<tPRESERVED_ID_END && ((tok)>=128 || rb_ispunct(tok)))))

/****** Ripper *******/

#ifdef RIPPER
#define RIPPER_VERSION "0.1.0"

static inline VALUE intern_sym(const char *name);

#include "eventids1.c"
#include "eventids2.c"

static VALUE ripper_dispatch0(struct parser_params*,ID);
static VALUE ripper_dispatch1(struct parser_params*,ID,VALUE);
static VALUE ripper_dispatch2(struct parser_params*,ID,VALUE,VALUE);
static VALUE ripper_dispatch3(struct parser_params*,ID,VALUE,VALUE,VALUE);
static VALUE ripper_dispatch4(struct parser_params*,ID,VALUE,VALUE,VALUE,VALUE);
static VALUE ripper_dispatch5(struct parser_params*,ID,VALUE,VALUE,VALUE,VALUE,VALUE);
static VALUE ripper_dispatch7(struct parser_params*,ID,VALUE,VALUE,VALUE,VALUE,VALUE,VALUE,VALUE);
static void ripper_error(struct parser_params *p);

#define dispatch0(n)            ripper_dispatch0(p, TOKEN_PASTE(ripper_id_, n))
#define dispatch1(n,a)          ripper_dispatch1(p, TOKEN_PASTE(ripper_id_, n), (a))
#define dispatch2(n,a,b)        ripper_dispatch2(p, TOKEN_PASTE(ripper_id_, n), (a), (b))
#define dispatch3(n,a,b,c)      ripper_dispatch3(p, TOKEN_PASTE(ripper_id_, n), (a), (b), (c))
#define dispatch4(n,a,b,c,d)    ripper_dispatch4(p, TOKEN_PASTE(ripper_id_, n), (a), (b), (c), (d))
#define dispatch5(n,a,b,c,d,e)  ripper_dispatch5(p, TOKEN_PASTE(ripper_id_, n), (a), (b), (c), (d), (e))
#define dispatch7(n,a,b,c,d,e,f,g) ripper_dispatch7(p, TOKEN_PASTE(ripper_id_, n), (a), (b), (c), (d), (e), (f), (g))

#define yyparse ripper_yyparse

#define ID2VAL(id) STATIC_ID2SYM(id)
#define TOKEN2VAL(t) ID2VAL(TOKEN2ID(t))
#define KWD2EID(t, v) ripper_new_yylval(p, keyword_##t, get_value(v), 0)

#define params_new(pars, opts, rest, pars2, kws, kwrest, blk) \
        dispatch7(params, (pars), (opts), (rest), (pars2), (kws), (kwrest), (blk))

#define escape_Qundef(x) ((x)==Qundef ? Qnil : (x))

static inline VALUE
new_args(struct parser_params *p, VALUE pre_args, VALUE opt_args, VALUE rest_arg, VALUE post_args, VALUE tail, YYLTYPE *loc)
{
    NODE *t = (NODE *)tail;
    VALUE kw_args = t->u1.value, kw_rest_arg = t->u2.value, block = t->u3.value;
    return params_new(pre_args, opt_args, rest_arg, post_args, kw_args, kw_rest_arg, escape_Qundef(block));
}

static inline VALUE
new_args_tail(struct parser_params *p, VALUE kw_args, VALUE kw_rest_arg, VALUE block, YYLTYPE *loc)
{
    NODE *t = rb_node_newnode(NODE_ARGS_AUX, kw_args, kw_rest_arg, block, &NULL_LOC);
    add_mark_object(p, kw_args);
    add_mark_object(p, kw_rest_arg);
    add_mark_object(p, block);
    return (VALUE)t;
}

static inline VALUE
args_with_numbered(struct parser_params *p, VALUE args, int max_numparam)
{
    return args;
}

static VALUE
new_array_pattern(struct parser_params *p, VALUE constant, VALUE pre_arg, VALUE aryptn, const YYLTYPE *loc)
{
    NODE *t = (NODE *)aryptn;
    VALUE pre_args = t->u1.value, rest_arg = t->u2.value, post_args = t->u3.value;

    if (!NIL_P(pre_arg)) {
        if (!NIL_P(pre_args)) {
            rb_ary_unshift(pre_args, pre_arg);
        }
        else {
            pre_args = rb_ary_new_from_args(1, pre_arg);
        }
    }
    return dispatch4(aryptn, constant, pre_args, rest_arg, post_args);
}

static VALUE
new_array_pattern_tail(struct parser_params *p, VALUE pre_args, VALUE has_rest, VALUE rest_arg, VALUE post_args, const YYLTYPE *loc)
{
    return ripper_new_yylval2(p, pre_args, rest_arg, post_args);
}

static VALUE
new_find_pattern(struct parser_params *p, VALUE constant, VALUE fndptn, const YYLTYPE *loc)
{
    NODE *t = (NODE *)fndptn;
    VALUE pre_rest_arg = t->u1.value, args = t->u2.value, post_rest_arg = t->u3.value;

    return dispatch4(fndptn, constant, pre_rest_arg, args, post_rest_arg);
}

static VALUE
new_find_pattern_tail(struct parser_params *p, VALUE pre_rest_arg, VALUE args, VALUE post_rest_arg, const YYLTYPE *loc)
{
    return ripper_new_yylval2(p, pre_rest_arg, args, post_rest_arg);
}

#define new_hash(p,h,l) rb_ary_new_from_args(0)

static VALUE
new_unique_key_hash(struct parser_params *p, VALUE ary, const YYLTYPE *loc)
{
    return ary;
}

static VALUE
new_hash_pattern(struct parser_params *p, VALUE constant, VALUE hshptn, const YYLTYPE *loc)
{
    NODE *t = (NODE *)hshptn;
    VALUE kw_args = t->u1.value, kw_rest_arg = t->u2.value;
    return dispatch3(hshptn, constant, kw_args, kw_rest_arg);
}

static VALUE
new_hash_pattern_tail(struct parser_params *p, VALUE kw_args, VALUE kw_rest_arg, const YYLTYPE *loc)
{
    if (kw_rest_arg) {
        kw_rest_arg = dispatch1(var_field, kw_rest_arg);
    }
    else {
        kw_rest_arg = Qnil;
    }
    return ripper_new_yylval2(p, kw_args, kw_rest_arg, Qnil);
}

#define new_defined(p,expr,loc) dispatch1(defined, (expr))

static VALUE heredoc_dedent(struct parser_params*,VALUE);

#else
#define ID2VAL(id) (id)
#define TOKEN2VAL(t) ID2VAL(t)
#define KWD2EID(t, v) keyword_##t

static NODE *
set_defun_body(struct parser_params *p, NODE *n, NODE *args, NODE *body, const YYLTYPE *loc)
{
    body = remove_begin(body);
    reduce_nodes(p, &body);
    n->nd_defn = NEW_SCOPE(args, body, loc);
    n->nd_loc = *loc;
    nd_set_line(n->nd_defn, loc->end_pos.lineno);
    set_line_body(body, loc->beg_pos.lineno);
    return n;
}

static NODE *
rescued_expr(struct parser_params *p, NODE *arg, NODE *rescue,
             const YYLTYPE *arg_loc, const YYLTYPE *mod_loc, const YYLTYPE *res_loc)
{
    YYLTYPE loc = code_loc_gen(mod_loc, res_loc);
    rescue = NEW_RESBODY(0, remove_begin(rescue), 0, &loc);
    loc.beg_pos = arg_loc->beg_pos;
    return NEW_RESCUE(arg, rescue, 0, &loc);
}

#endif /* RIPPER */

static void
restore_defun(struct parser_params *p, NODE *name)
{
    NODE *save = name->nd_next;
    YYSTYPE c = {.val = save->nd_cval};
    p->cur_arg = name->nd_vid;
    p->ctxt.in_def = c.ctxt.in_def;
    p->ctxt.shareable_constant_value = c.ctxt.shareable_constant_value;
    p->max_numparam = (int)save->nd_nth;
    numparam_pop(p, save->nd_head);
}

static void
endless_method_name(struct parser_params *p, NODE *defn, const YYLTYPE *loc)
{
#ifdef RIPPER
    defn = defn->nd_defn;
#endif
    ID mid = defn->nd_mid;
    if (is_attrset_id(mid)) {
        yyerror1(loc, "setter method cannot be defined in an endless method definition");
    }
    token_info_drop(p, "def", loc->beg_pos);
}

#define debug_token_line(p, name, line) do { \
        if (p->debug) { \
            const char *const pcur = p->lex.pcur; \
            const char *const ptok = p->lex.ptok; \
            rb_parser_printf(p, name ":%d (%d: %"PRIdPTRDIFF"|%"PRIdPTRDIFF"|%"PRIdPTRDIFF")\n", \
                             line, p->ruby_sourceline, \
                             ptok - p->lex.pbeg, pcur - ptok, p->lex.pend - pcur); \
        } \
    } while (0)

#ifndef RIPPER
# define Qnone 0
# define Qnull 0
# define ifndef_ripper(x) (x)
#else
# define Qnone Qnil
# define Qnull Qundef
# define ifndef_ripper(x)
#endif

# define rb_warn0(fmt)         WARN_CALL(WARN_ARGS(fmt, 1))
# define rb_warn1(fmt,a)       WARN_CALL(WARN_ARGS(fmt, 2), (a))
# define rb_warn2(fmt,a,b)     WARN_CALL(WARN_ARGS(fmt, 3), (a), (b))
# define rb_warn3(fmt,a,b,c)   WARN_CALL(WARN_ARGS(fmt, 4), (a), (b), (c))
# define rb_warn4(fmt,a,b,c,d) WARN_CALL(WARN_ARGS(fmt, 5), (a), (b), (c), (d))
# define rb_warning0(fmt)         WARNING_CALL(WARNING_ARGS(fmt, 1))
# define rb_warning1(fmt,a)       WARNING_CALL(WARNING_ARGS(fmt, 2), (a))
# define rb_warning2(fmt,a,b)     WARNING_CALL(WARNING_ARGS(fmt, 3), (a), (b))
# define rb_warning3(fmt,a,b,c)   WARNING_CALL(WARNING_ARGS(fmt, 4), (a), (b), (c))
# define rb_warning4(fmt,a,b,c,d) WARNING_CALL(WARNING_ARGS(fmt, 5), (a), (b), (c), (d))
# define rb_warn0L(l,fmt)         WARN_CALL(WARN_ARGS_L(l, fmt, 1))
# define rb_warn1L(l,fmt,a)       WARN_CALL(WARN_ARGS_L(l, fmt, 2), (a))
# define rb_warn2L(l,fmt,a,b)     WARN_CALL(WARN_ARGS_L(l, fmt, 3), (a), (b))
# define rb_warn3L(l,fmt,a,b,c)   WARN_CALL(WARN_ARGS_L(l, fmt, 4), (a), (b), (c))
# define rb_warn4L(l,fmt,a,b,c,d) WARN_CALL(WARN_ARGS_L(l, fmt, 5), (a), (b), (c), (d))
# define rb_warning0L(l,fmt)         WARNING_CALL(WARNING_ARGS_L(l, fmt, 1))
# define rb_warning1L(l,fmt,a)       WARNING_CALL(WARNING_ARGS_L(l, fmt, 2), (a))
# define rb_warning2L(l,fmt,a,b)     WARNING_CALL(WARNING_ARGS_L(l, fmt, 3), (a), (b))
# define rb_warning3L(l,fmt,a,b,c)   WARNING_CALL(WARNING_ARGS_L(l, fmt, 4), (a), (b), (c))
# define rb_warning4L(l,fmt,a,b,c,d) WARNING_CALL(WARNING_ARGS_L(l, fmt, 5), (a), (b), (c), (d))
#ifdef RIPPER
static ID id_warn, id_warning, id_gets, id_assoc;
# define ERR_MESG() STR_NEW2(mesg) /* to bypass Ripper DSL */
# define WARN_S_L(s,l) STR_NEW(s,l)
# define WARN_S(s) STR_NEW2(s)
# define WARN_I(i) INT2NUM(i)
# define WARN_ID(i) rb_id2str(i)
# define WARN_IVAL(i) i
# define PRIsWARN "s"
# define rb_warn0L_experimental(l,fmt)         WARN_CALL(WARN_ARGS_L(l, fmt, 1))
# define WARN_ARGS(fmt,n) p->value, id_warn, n, rb_usascii_str_new_lit(fmt)
# define WARN_ARGS_L(l,fmt,n) WARN_ARGS(fmt,n)
# ifdef HAVE_VA_ARGS_MACRO
# define WARN_CALL(...) rb_funcall(__VA_ARGS__)
# else
# define WARN_CALL rb_funcall
# endif
# define WARNING_ARGS(fmt,n) p->value, id_warning, n, rb_usascii_str_new_lit(fmt)
# define WARNING_ARGS_L(l, fmt,n) WARNING_ARGS(fmt,n)
# ifdef HAVE_VA_ARGS_MACRO
# define WARNING_CALL(...) rb_funcall(__VA_ARGS__)
# else
# define WARNING_CALL rb_funcall
# endif
PRINTF_ARGS(static void ripper_compile_error(struct parser_params*, const char *fmt, ...), 2, 3);
# define compile_error ripper_compile_error
#else
# define WARN_S_L(s,l) s
# define WARN_S(s) s
# define WARN_I(i) i
# define WARN_ID(i) rb_id2name(i)
# define WARN_IVAL(i) NUM2INT(i)
# define PRIsWARN PRIsVALUE
# define WARN_ARGS(fmt,n) WARN_ARGS_L(p->ruby_sourceline,fmt,n)
# define WARN_ARGS_L(l,fmt,n) p->ruby_sourcefile, (l), (fmt)
# define WARN_CALL rb_compile_warn
# define rb_warn0L_experimental(l,fmt) rb_category_compile_warn(RB_WARN_CATEGORY_EXPERIMENTAL, WARN_ARGS_L(l, fmt, 1))
# define WARNING_ARGS(fmt,n) WARN_ARGS(fmt,n)
# define WARNING_ARGS_L(l,fmt,n) WARN_ARGS_L(l,fmt,n)
# define WARNING_CALL rb_compile_warning
PRINTF_ARGS(static void parser_compile_error(struct parser_params*, const char *fmt, ...), 2, 3);
# define compile_error parser_compile_error
#endif

#define WARN_EOL(tok) \
    (looking_at_eol_p(p) ? \
     (void)rb_warning0("`" tok "' at the end of line without an expression") : \
     (void)0)
static int looking_at_eol_p(struct parser_params *p);

#line 1386 "ripper.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Use api.header.include to #include this header
   instead of duplicating it here.  */
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
#ifndef yydebug
extern int yydebug;
#endif
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    END_OF_INPUT = 0,              /* "end-of-input"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    keyword_class = 258,           /* "`class'"  */
    keyword_module = 259,          /* "`module'"  */
    keyword_def = 260,             /* "`def'"  */
    keyword_undef = 261,           /* "`undef'"  */
    keyword_begin = 262,           /* "`begin'"  */
    keyword_rescue = 263,          /* "`rescue'"  */
    keyword_ensure = 264,          /* "`ensure'"  */
    keyword_end = 265,             /* "`end'"  */
    keyword_if = 266,              /* "`if'"  */
    keyword_unless = 267,          /* "`unless'"  */
    keyword_then = 268,            /* "`then'"  */
    keyword_elsif = 269,           /* "`elsif'"  */
    keyword_else = 270,            /* "`else'"  */
    keyword_case = 271,            /* "`case'"  */
    keyword_when = 272,            /* "`when'"  */
    keyword_while = 273,           /* "`while'"  */
    keyword_until = 274,           /* "`until'"  */
    keyword_for = 275,             /* "`for'"  */
    keyword_break = 276,           /* "`break'"  */
    keyword_next = 277,            /* "`next'"  */
    keyword_redo = 278,            /* "`redo'"  */
    keyword_retry = 279,           /* "`retry'"  */
    keyword_in = 280,              /* "`in'"  */
    keyword_do = 281,              /* "`do'"  */
    keyword_do_cond = 282,         /* "`do' for condition"  */
    keyword_do_block = 283,        /* "`do' for block"  */
    keyword_do_LAMBDA = 284,       /* "`do' for lambda"  */
    keyword_return = 285,          /* "`return'"  */
    keyword_yield = 286,           /* "`yield'"  */
    keyword_super = 287,           /* "`super'"  */
    keyword_self = 288,            /* "`self'"  */
    keyword_nil = 289,             /* "`nil'"  */
    keyword_true = 290,            /* "`true'"  */
    keyword_false = 291,           /* "`false'"  */
    keyword_and = 292,             /* "`and'"  */
    keyword_or = 293,              /* "`or'"  */
    keyword_not = 294,             /* "`not'"  */
    modifier_if = 295,             /* "`if' modifier"  */
    modifier_unless = 296,         /* "`unless' modifier"  */
    modifier_while = 297,          /* "`while' modifier"  */
    modifier_until = 298,          /* "`until' modifier"  */
    modifier_rescue = 299,         /* "`rescue' modifier"  */
    keyword_alias = 300,           /* "`alias'"  */
    keyword_defined = 301,         /* "`defined?'"  */
    keyword_BEGIN = 302,           /* "`BEGIN'"  */
    keyword_END = 303,             /* "`END'"  */
    keyword__LINE__ = 304,         /* "`__LINE__'"  */
    keyword__FILE__ = 305,         /* "`__FILE__'"  */
    keyword__ENCODING__ = 306,     /* "`__ENCODING__'"  */
    tIDENTIFIER = 307,             /* "local variable or method"  */
    tFID = 308,                    /* "method"  */
    tGVAR = 309,                   /* "global variable"  */
    tIVAR = 310,                   /* "instance variable"  */
    tCONSTANT = 311,               /* "constant"  */
    tCVAR = 312,                   /* "class variable"  */
    tLABEL = 313,                  /* "label"  */
    tINTEGER = 314,                /* "integer literal"  */
    tFLOAT = 315,                  /* "float literal"  */
    tRATIONAL = 316,               /* "rational literal"  */
    tIMAGINARY = 317,              /* "imaginary literal"  */
    tCHAR = 318,                   /* "char literal"  */
    tNTH_REF = 319,                /* "numbered reference"  */
    tBACK_REF = 320,               /* "back reference"  */
    tSTRING_CONTENT = 321,         /* "literal content"  */
    tREGEXP_END = 322,             /* tREGEXP_END  */
    tDUMNY_END = 323,              /* "dummy end"  */
    tSP = 324,                     /* "escaped space"  */
    tUPLUS = 132,                  /* "unary+"  */
    tUMINUS = 133,                 /* "unary-"  */
    tPOW = 134,                    /* "**"  */
    tCMP = 135,                    /* "<=>"  */
    tEQ = 140,                     /* "=="  */
    tEQQ = 141,                    /* "==="  */
    tNEQ = 142,                    /* "!="  */
    tGEQ = 139,                    /* ">="  */
    tLEQ = 138,                    /* "<="  */
    tANDOP = 148,                  /* "&&"  */
    tOROP = 149,                   /* "||"  */
    tMATCH = 143,                  /* "=~"  */
    tNMATCH = 144,                 /* "!~"  */
    tDOT2 = 128,                   /* ".."  */
    tDOT3 = 129,                   /* "..."  */
    tBDOT2 = 130,                  /* "(.."  */
    tBDOT3 = 131,                  /* "(..."  */
    tAREF = 145,                   /* "[]"  */
    tASET = 146,                   /* "[]="  */
    tLSHFT = 136,                  /* "<<"  */
    tRSHFT = 137,                  /* ">>"  */
    tANDDOT = 150,                 /* "&."  */
    tCOLON2 = 147,                 /* "::"  */
    tCOLON3 = 325,                 /* ":: at EXPR_BEG"  */
    tOP_ASGN = 326,                /* "operator-assignment"  */
    tASSOC = 327,                  /* "=>"  */
    tLPAREN = 328,                 /* "("  */
    tLPAREN_ARG = 329,             /* "( arg"  */
    tRPAREN = 330,                 /* ")"  */
    tLBRACK = 331,                 /* "["  */
    tLBRACE = 332,                 /* "{"  */
    tLBRACE_ARG = 333,             /* "{ arg"  */
    tSTAR = 334,                   /* "*"  */
    tDSTAR = 335,                  /* "**arg"  */
    tAMPER = 336,                  /* "&"  */
    tLAMBDA = 337,                 /* "->"  */
    tSYMBEG = 338,                 /* "symbol literal"  */
    tSTRING_BEG = 339,             /* "string literal"  */
    tXSTRING_BEG = 340,            /* "backtick literal"  */
    tREGEXP_BEG = 341,             /* "regexp literal"  */
    tWORDS_BEG = 342,              /* "word list"  */
    tQWORDS_BEG = 343,             /* "verbatim word list"  */
    tSYMBOLS_BEG = 344,            /* "symbol list"  */
    tQSYMBOLS_BEG = 345,           /* "verbatim symbol list"  */
    tSTRING_END = 346,             /* "terminator"  */
    tSTRING_DEND = 347,            /* "'}'"  */
    tSTRING_DBEG = 348,            /* tSTRING_DBEG  */
    tSTRING_DVAR = 349,            /* tSTRING_DVAR  */
    tLAMBEG = 350,                 /* tLAMBEG  */
    tLABEL_END = 351,              /* tLABEL_END  */
    tIGNORED_NL = 352,             /* tIGNORED_NL  */
    tCOMMENT = 353,                /* tCOMMENT  */
    tEMBDOC_BEG = 354,             /* tEMBDOC_BEG  */
    tEMBDOC = 355,                 /* tEMBDOC  */
    tEMBDOC_END = 356,             /* tEMBDOC_END  */
    tHEREDOC_BEG = 357,            /* tHEREDOC_BEG  */
    tHEREDOC_END = 358,            /* tHEREDOC_END  */
    k__END__ = 359,                /* k__END__  */
    tLOWEST = 360,                 /* tLOWEST  */
    tUMINUS_NUM = 361,             /* tUMINUS_NUM  */
    tLAST_TOKEN = 362              /* tLAST_TOKEN  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 1376 "ripper.y"

    VALUE val;
    NODE *node;
    ID id;
    int num;
    st_table *tbl;
    const struct vtable *vars;
    struct rb_strterm_struct *strterm;
    struct lex_context ctxt;

#line 1575 "ripper.c"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif




int yyparse (struct parser_params *p);


/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end-of-input"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_keyword_class = 3,              /* "`class'"  */
  YYSYMBOL_keyword_module = 4,             /* "`module'"  */
  YYSYMBOL_keyword_def = 5,                /* "`def'"  */
  YYSYMBOL_keyword_undef = 6,              /* "`undef'"  */
  YYSYMBOL_keyword_begin = 7,              /* "`begin'"  */
  YYSYMBOL_keyword_rescue = 8,             /* "`rescue'"  */
  YYSYMBOL_keyword_ensure = 9,             /* "`ensure'"  */
  YYSYMBOL_keyword_end = 10,               /* "`end'"  */
  YYSYMBOL_keyword_if = 11,                /* "`if'"  */
  YYSYMBOL_keyword_unless = 12,            /* "`unless'"  */
  YYSYMBOL_keyword_then = 13,              /* "`then'"  */
  YYSYMBOL_keyword_elsif = 14,             /* "`elsif'"  */
  YYSYMBOL_keyword_else = 15,              /* "`else'"  */
  YYSYMBOL_keyword_case = 16,              /* "`case'"  */
  YYSYMBOL_keyword_when = 17,              /* "`when'"  */
  YYSYMBOL_keyword_while = 18,             /* "`while'"  */
  YYSYMBOL_keyword_until = 19,             /* "`until'"  */
  YYSYMBOL_keyword_for = 20,               /* "`for'"  */
  YYSYMBOL_keyword_break = 21,             /* "`break'"  */
  YYSYMBOL_keyword_next = 22,              /* "`next'"  */
  YYSYMBOL_keyword_redo = 23,              /* "`redo'"  */
  YYSYMBOL_keyword_retry = 24,             /* "`retry'"  */
  YYSYMBOL_keyword_in = 25,                /* "`in'"  */
  YYSYMBOL_keyword_do = 26,                /* "`do'"  */
  YYSYMBOL_keyword_do_cond = 27,           /* "`do' for condition"  */
  YYSYMBOL_keyword_do_block = 28,          /* "`do' for block"  */
  YYSYMBOL_keyword_do_LAMBDA = 29,         /* "`do' for lambda"  */
  YYSYMBOL_keyword_return = 30,            /* "`return'"  */
  YYSYMBOL_keyword_yield = 31,             /* "`yield'"  */
  YYSYMBOL_keyword_super = 32,             /* "`super'"  */
  YYSYMBOL_keyword_self = 33,              /* "`self'"  */
  YYSYMBOL_keyword_nil = 34,               /* "`nil'"  */
  YYSYMBOL_keyword_true = 35,              /* "`true'"  */
  YYSYMBOL_keyword_false = 36,             /* "`false'"  */
  YYSYMBOL_keyword_and = 37,               /* "`and'"  */
  YYSYMBOL_keyword_or = 38,                /* "`or'"  */
  YYSYMBOL_keyword_not = 39,               /* "`not'"  */
  YYSYMBOL_modifier_if = 40,               /* "`if' modifier"  */
  YYSYMBOL_modifier_unless = 41,           /* "`unless' modifier"  */
  YYSYMBOL_modifier_while = 42,            /* "`while' modifier"  */
  YYSYMBOL_modifier_until = 43,            /* "`until' modifier"  */
  YYSYMBOL_modifier_rescue = 44,           /* "`rescue' modifier"  */
  YYSYMBOL_keyword_alias = 45,             /* "`alias'"  */
  YYSYMBOL_keyword_defined = 46,           /* "`defined?'"  */
  YYSYMBOL_keyword_BEGIN = 47,             /* "`BEGIN'"  */
  YYSYMBOL_keyword_END = 48,               /* "`END'"  */
  YYSYMBOL_keyword__LINE__ = 49,           /* "`__LINE__'"  */
  YYSYMBOL_keyword__FILE__ = 50,           /* "`__FILE__'"  */
  YYSYMBOL_keyword__ENCODING__ = 51,       /* "`__ENCODING__'"  */
  YYSYMBOL_tIDENTIFIER = 52,               /* "local variable or method"  */
  YYSYMBOL_tFID = 53,                      /* "method"  */
  YYSYMBOL_tGVAR = 54,                     /* "global variable"  */
  YYSYMBOL_tIVAR = 55,                     /* "instance variable"  */
  YYSYMBOL_tCONSTANT = 56,                 /* "constant"  */
  YYSYMBOL_tCVAR = 57,                     /* "class variable"  */
  YYSYMBOL_tLABEL = 58,                    /* "label"  */
  YYSYMBOL_tINTEGER = 59,                  /* "integer literal"  */
  YYSYMBOL_tFLOAT = 60,                    /* "float literal"  */
  YYSYMBOL_tRATIONAL = 61,                 /* "rational literal"  */
  YYSYMBOL_tIMAGINARY = 62,                /* "imaginary literal"  */
  YYSYMBOL_tCHAR = 63,                     /* "char literal"  */
  YYSYMBOL_tNTH_REF = 64,                  /* "numbered reference"  */
  YYSYMBOL_tBACK_REF = 65,                 /* "back reference"  */
  YYSYMBOL_tSTRING_CONTENT = 66,           /* "literal content"  */
  YYSYMBOL_tREGEXP_END = 67,               /* tREGEXP_END  */
  YYSYMBOL_tDUMNY_END = 68,                /* "dummy end"  */
  YYSYMBOL_69_ = 69,                       /* '.'  */
  YYSYMBOL_70_backslash_ = 70,             /* "backslash"  */
  YYSYMBOL_tSP = 71,                       /* "escaped space"  */
  YYSYMBOL_72_escaped_horizontal_tab_ = 72, /* "escaped horizontal tab"  */
  YYSYMBOL_73_escaped_form_feed_ = 73,     /* "escaped form feed"  */
  YYSYMBOL_74_escaped_carriage_return_ = 74, /* "escaped carriage return"  */
  YYSYMBOL_75_escaped_vertical_tab_ = 75,  /* "escaped vertical tab"  */
  YYSYMBOL_tUPLUS = 76,                    /* "unary+"  */
  YYSYMBOL_tUMINUS = 77,                   /* "unary-"  */
  YYSYMBOL_tPOW = 78,                      /* "**"  */
  YYSYMBOL_tCMP = 79,                      /* "<=>"  */
  YYSYMBOL_tEQ = 80,                       /* "=="  */
  YYSYMBOL_tEQQ = 81,                      /* "==="  */
  YYSYMBOL_tNEQ = 82,                      /* "!="  */
  YYSYMBOL_tGEQ = 83,                      /* ">="  */
  YYSYMBOL_tLEQ = 84,                      /* "<="  */
  YYSYMBOL_tANDOP = 85,                    /* "&&"  */
  YYSYMBOL_tOROP = 86,                     /* "||"  */
  YYSYMBOL_tMATCH = 87,                    /* "=~"  */
  YYSYMBOL_tNMATCH = 88,                   /* "!~"  */
  YYSYMBOL_tDOT2 = 89,                     /* ".."  */
  YYSYMBOL_tDOT3 = 90,                     /* "..."  */
  YYSYMBOL_tBDOT2 = 91,                    /* "(.."  */
  YYSYMBOL_tBDOT3 = 92,                    /* "(..."  */
  YYSYMBOL_tAREF = 93,                     /* "[]"  */
  YYSYMBOL_tASET = 94,                     /* "[]="  */
  YYSYMBOL_tLSHFT = 95,                    /* "<<"  */
  YYSYMBOL_tRSHFT = 96,                    /* ">>"  */
  YYSYMBOL_tANDDOT = 97,                   /* "&."  */
  YYSYMBOL_tCOLON2 = 98,                   /* "::"  */
  YYSYMBOL_tCOLON3 = 99,                   /* ":: at EXPR_BEG"  */
  YYSYMBOL_tOP_ASGN = 100,                 /* "operator-assignment"  */
  YYSYMBOL_tASSOC = 101,                   /* "=>"  */
  YYSYMBOL_tLPAREN = 102,                  /* "("  */
  YYSYMBOL_tLPAREN_ARG = 103,              /* "( arg"  */
  YYSYMBOL_tRPAREN = 104,                  /* ")"  */
  YYSYMBOL_tLBRACK = 105,                  /* "["  */
  YYSYMBOL_tLBRACE = 106,                  /* "{"  */
  YYSYMBOL_tLBRACE_ARG = 107,              /* "{ arg"  */
  YYSYMBOL_tSTAR = 108,                    /* "*"  */
  YYSYMBOL_tDSTAR = 109,                   /* "**arg"  */
  YYSYMBOL_tAMPER = 110,                   /* "&"  */
  YYSYMBOL_tLAMBDA = 111,                  /* "->"  */
  YYSYMBOL_tSYMBEG = 112,                  /* "symbol literal"  */
  YYSYMBOL_tSTRING_BEG = 113,              /* "string literal"  */
  YYSYMBOL_tXSTRING_BEG = 114,             /* "backtick literal"  */
  YYSYMBOL_tREGEXP_BEG = 115,              /* "regexp literal"  */
  YYSYMBOL_tWORDS_BEG = 116,               /* "word list"  */
  YYSYMBOL_tQWORDS_BEG = 117,              /* "verbatim word list"  */
  YYSYMBOL_tSYMBOLS_BEG = 118,             /* "symbol list"  */
  YYSYMBOL_tQSYMBOLS_BEG = 119,            /* "verbatim symbol list"  */
  YYSYMBOL_tSTRING_END = 120,              /* "terminator"  */
  YYSYMBOL_tSTRING_DEND = 121,             /* "'}'"  */
  YYSYMBOL_tSTRING_DBEG = 122,             /* tSTRING_DBEG  */
  YYSYMBOL_tSTRING_DVAR = 123,             /* tSTRING_DVAR  */
  YYSYMBOL_tLAMBEG = 124,                  /* tLAMBEG  */
  YYSYMBOL_tLABEL_END = 125,               /* tLABEL_END  */
  YYSYMBOL_tIGNORED_NL = 126,              /* tIGNORED_NL  */
  YYSYMBOL_tCOMMENT = 127,                 /* tCOMMENT  */
  YYSYMBOL_tEMBDOC_BEG = 128,              /* tEMBDOC_BEG  */
  YYSYMBOL_tEMBDOC = 129,                  /* tEMBDOC  */
  YYSYMBOL_tEMBDOC_END = 130,              /* tEMBDOC_END  */
  YYSYMBOL_tHEREDOC_BEG = 131,             /* tHEREDOC_BEG  */
  YYSYMBOL_tHEREDOC_END = 132,             /* tHEREDOC_END  */
  YYSYMBOL_k__END__ = 133,                 /* k__END__  */
  YYSYMBOL_tLOWEST = 134,                  /* tLOWEST  */
  YYSYMBOL_135_ = 135,                     /* '='  */
  YYSYMBOL_136_ = 136,                     /* '?'  */
  YYSYMBOL_137_ = 137,                     /* ':'  */
  YYSYMBOL_138_ = 138,                     /* '>'  */
  YYSYMBOL_139_ = 139,                     /* '<'  */
  YYSYMBOL_140_ = 140,                     /* '|'  */
  YYSYMBOL_141_ = 141,                     /* '^'  */
  YYSYMBOL_142_ = 142,                     /* '&'  */
  YYSYMBOL_143_ = 143,                     /* '+'  */
  YYSYMBOL_144_ = 144,                     /* '-'  */
  YYSYMBOL_145_ = 145,                     /* '*'  */
  YYSYMBOL_146_ = 146,                     /* '/'  */
  YYSYMBOL_147_ = 147,                     /* '%'  */
  YYSYMBOL_tUMINUS_NUM = 148,              /* tUMINUS_NUM  */
  YYSYMBOL_149_ = 149,                     /* '!'  */
  YYSYMBOL_150_ = 150,                     /* '~'  */
  YYSYMBOL_tLAST_TOKEN = 151,              /* tLAST_TOKEN  */
  YYSYMBOL_152_ = 152,                     /* '{'  */
  YYSYMBOL_153_ = 153,                     /* '}'  */
  YYSYMBOL_154_ = 154,                     /* '['  */
  YYSYMBOL_155_ = 155,                     /* ','  */
  YYSYMBOL_156_ = 156,                     /* '`'  */
  YYSYMBOL_157_ = 157,                     /* '('  */
  YYSYMBOL_158_ = 158,                     /* ')'  */
  YYSYMBOL_159_ = 159,                     /* ']'  */
  YYSYMBOL_160_ = 160,                     /* ';'  */
  YYSYMBOL_161_ = 161,                     /* ' '  */
  YYSYMBOL_162_n_ = 162,                   /* '\n'  */
  YYSYMBOL_YYACCEPT = 163,                 /* $accept  */
  YYSYMBOL_program = 164,                  /* program  */
  YYSYMBOL_165_1 = 165,                    /* $@1  */
  YYSYMBOL_top_compstmt = 166,             /* top_compstmt  */
  YYSYMBOL_top_stmts = 167,                /* top_stmts  */
  YYSYMBOL_top_stmt = 168,                 /* top_stmt  */
  YYSYMBOL_begin_block = 169,              /* begin_block  */
  YYSYMBOL_bodystmt = 170,                 /* bodystmt  */
  YYSYMBOL_171_2 = 171,                    /* $@2  */
  YYSYMBOL_compstmt = 172,                 /* compstmt  */
  YYSYMBOL_stmts = 173,                    /* stmts  */
  YYSYMBOL_stmt_or_begin = 174,            /* stmt_or_begin  */
  YYSYMBOL_175_3 = 175,                    /* $@3  */
  YYSYMBOL_stmt = 176,                     /* stmt  */
  YYSYMBOL_177_4 = 177,                    /* $@4  */
  YYSYMBOL_command_asgn = 178,             /* command_asgn  */
  YYSYMBOL_endless_command = 179,          /* endless_command  */
  YYSYMBOL_command_rhs = 180,              /* command_rhs  */
  YYSYMBOL_expr = 181,                     /* expr  */
  YYSYMBOL_182_5 = 182,                    /* @5  */
  YYSYMBOL_183_6 = 183,                    /* @6  */
  YYSYMBOL_184_7 = 184,                    /* @7  */
  YYSYMBOL_185_8 = 185,                    /* @8  */
  YYSYMBOL_def_name = 186,                 /* def_name  */
  YYSYMBOL_defn_head = 187,                /* defn_head  */
  YYSYMBOL_defs_head = 188,                /* defs_head  */
  YYSYMBOL_189_9 = 189,                    /* $@9  */
  YYSYMBOL_expr_value = 190,               /* expr_value  */
  YYSYMBOL_expr_value_do = 191,            /* expr_value_do  */
  YYSYMBOL_192_10 = 192,                   /* $@10  */
  YYSYMBOL_193_11 = 193,                   /* $@11  */
  YYSYMBOL_command_call = 194,             /* command_call  */
  YYSYMBOL_block_command = 195,            /* block_command  */
  YYSYMBOL_cmd_brace_block = 196,          /* cmd_brace_block  */
  YYSYMBOL_fcall = 197,                    /* fcall  */
  YYSYMBOL_command = 198,                  /* command  */
  YYSYMBOL_mlhs = 199,                     /* mlhs  */
  YYSYMBOL_mlhs_inner = 200,               /* mlhs_inner  */
  YYSYMBOL_mlhs_basic = 201,               /* mlhs_basic  */
  YYSYMBOL_mlhs_item = 202,                /* mlhs_item  */
  YYSYMBOL_mlhs_head = 203,                /* mlhs_head  */
  YYSYMBOL_mlhs_post = 204,                /* mlhs_post  */
  YYSYMBOL_mlhs_node = 205,                /* mlhs_node  */
  YYSYMBOL_lhs = 206,                      /* lhs  */
  YYSYMBOL_cname = 207,                    /* cname  */
  YYSYMBOL_cpath = 208,                    /* cpath  */
  YYSYMBOL_fname = 209,                    /* fname  */
  YYSYMBOL_fitem = 210,                    /* fitem  */
  YYSYMBOL_undef_list = 211,               /* undef_list  */
  YYSYMBOL_212_12 = 212,                   /* $@12  */
  YYSYMBOL_op = 213,                       /* op  */
  YYSYMBOL_reswords = 214,                 /* reswords  */
  YYSYMBOL_arg = 215,                      /* arg  */
  YYSYMBOL_216_13 = 216,                   /* $@13  */
  YYSYMBOL_endless_arg = 217,              /* endless_arg  */
  YYSYMBOL_relop = 218,                    /* relop  */
  YYSYMBOL_rel_expr = 219,                 /* rel_expr  */
  YYSYMBOL_lex_ctxt = 220,                 /* lex_ctxt  */
  YYSYMBOL_arg_value = 221,                /* arg_value  */
  YYSYMBOL_aref_args = 222,                /* aref_args  */
  YYSYMBOL_arg_rhs = 223,                  /* arg_rhs  */
  YYSYMBOL_paren_args = 224,               /* paren_args  */
  YYSYMBOL_opt_paren_args = 225,           /* opt_paren_args  */
  YYSYMBOL_opt_call_args = 226,            /* opt_call_args  */
  YYSYMBOL_call_args = 227,                /* call_args  */
  YYSYMBOL_command_args = 228,             /* command_args  */
  YYSYMBOL_229_14 = 229,                   /* $@14  */
  YYSYMBOL_block_arg = 230,                /* block_arg  */
  YYSYMBOL_opt_block_arg = 231,            /* opt_block_arg  */
  YYSYMBOL_args = 232,                     /* args  */
  YYSYMBOL_mrhs_arg = 233,                 /* mrhs_arg  */
  YYSYMBOL_mrhs = 234,                     /* mrhs  */
  YYSYMBOL_primary = 235,                  /* primary  */
  YYSYMBOL_236_15 = 236,                   /* $@15  */
  YYSYMBOL_237_16 = 237,                   /* $@16  */
  YYSYMBOL_238_17 = 238,                   /* $@17  */
  YYSYMBOL_239_18 = 239,                   /* $@18  */
  YYSYMBOL_240_19 = 240,                   /* @19  */
  YYSYMBOL_241_20 = 241,                   /* @20  */
  YYSYMBOL_242_21 = 242,                   /* $@21  */
  YYSYMBOL_243_22 = 243,                   /* $@22  */
  YYSYMBOL_244_23 = 244,                   /* $@23  */
  YYSYMBOL_245_24 = 245,                   /* $@24  */
  YYSYMBOL_246_25 = 246,                   /* $@25  */
  YYSYMBOL_primary_value = 247,            /* primary_value  */
  YYSYMBOL_k_begin = 248,                  /* k_begin  */
  YYSYMBOL_k_if = 249,                     /* k_if  */
  YYSYMBOL_k_unless = 250,                 /* k_unless  */
  YYSYMBOL_k_while = 251,                  /* k_while  */
  YYSYMBOL_k_until = 252,                  /* k_until  */
  YYSYMBOL_k_case = 253,                   /* k_case  */
  YYSYMBOL_k_for = 254,                    /* k_for  */
  YYSYMBOL_k_class = 255,                  /* k_class  */
  YYSYMBOL_k_module = 256,                 /* k_module  */
  YYSYMBOL_k_def = 257,                    /* k_def  */
  YYSYMBOL_k_do = 258,                     /* k_do  */
  YYSYMBOL_k_do_block = 259,               /* k_do_block  */
  YYSYMBOL_k_rescue = 260,                 /* k_rescue  */
  YYSYMBOL_k_ensure = 261,                 /* k_ensure  */
  YYSYMBOL_k_when = 262,                   /* k_when  */
  YYSYMBOL_k_else = 263,                   /* k_else  */
  YYSYMBOL_k_elsif = 264,                  /* k_elsif  */
  YYSYMBOL_k_end = 265,                    /* k_end  */
  YYSYMBOL_k_return = 266,                 /* k_return  */
  YYSYMBOL_then = 267,                     /* then  */
  YYSYMBOL_do = 268,                       /* do  */
  YYSYMBOL_if_tail = 269,                  /* if_tail  */
  YYSYMBOL_opt_else = 270,                 /* opt_else  */
  YYSYMBOL_for_var = 271,                  /* for_var  */
  YYSYMBOL_f_marg = 272,                   /* f_marg  */
  YYSYMBOL_f_marg_list = 273,              /* f_marg_list  */
  YYSYMBOL_f_margs = 274,                  /* f_margs  */
  YYSYMBOL_f_rest_marg = 275,              /* f_rest_marg  */
  YYSYMBOL_f_any_kwrest = 276,             /* f_any_kwrest  */
  YYSYMBOL_f_eq = 277,                     /* f_eq  */
  YYSYMBOL_278_26 = 278,                   /* $@26  */
  YYSYMBOL_block_args_tail = 279,          /* block_args_tail  */
  YYSYMBOL_opt_block_args_tail = 280,      /* opt_block_args_tail  */
  YYSYMBOL_excessed_comma = 281,           /* excessed_comma  */
  YYSYMBOL_block_param = 282,              /* block_param  */
  YYSYMBOL_opt_block_param = 283,          /* opt_block_param  */
  YYSYMBOL_block_param_def = 284,          /* block_param_def  */
  YYSYMBOL_opt_bv_decl = 285,              /* opt_bv_decl  */
  YYSYMBOL_bv_decls = 286,                 /* bv_decls  */
  YYSYMBOL_bvar = 287,                     /* bvar  */
  YYSYMBOL_lambda = 288,                   /* lambda  */
  YYSYMBOL_289_27 = 289,                   /* @27  */
  YYSYMBOL_290_28 = 290,                   /* @28  */
  YYSYMBOL_291_29 = 291,                   /* @29  */
  YYSYMBOL_292_30 = 292,                   /* $@30  */
  YYSYMBOL_f_larglist = 293,               /* f_larglist  */
  YYSYMBOL_lambda_body = 294,              /* lambda_body  */
  YYSYMBOL_295_31 = 295,                   /* $@31  */
  YYSYMBOL_do_block = 296,                 /* do_block  */
  YYSYMBOL_block_call = 297,               /* block_call  */
  YYSYMBOL_method_call = 298,              /* method_call  */
  YYSYMBOL_brace_block = 299,              /* brace_block  */
  YYSYMBOL_brace_body = 300,               /* brace_body  */
  YYSYMBOL_301_32 = 301,                   /* @32  */
  YYSYMBOL_302_33 = 302,                   /* @33  */
  YYSYMBOL_303_34 = 303,                   /* @34  */
  YYSYMBOL_do_body = 304,                  /* do_body  */
  YYSYMBOL_305_35 = 305,                   /* @35  */
  YYSYMBOL_306_36 = 306,                   /* @36  */
  YYSYMBOL_307_37 = 307,                   /* @37  */
  YYSYMBOL_case_args = 308,                /* case_args  */
  YYSYMBOL_case_body = 309,                /* case_body  */
  YYSYMBOL_cases = 310,                    /* cases  */
  YYSYMBOL_p_case_body = 311,              /* p_case_body  */
  YYSYMBOL_312_38 = 312,                   /* @38  */
  YYSYMBOL_313_39 = 313,                   /* @39  */
  YYSYMBOL_314_40 = 314,                   /* $@40  */
  YYSYMBOL_p_cases = 315,                  /* p_cases  */
  YYSYMBOL_p_top_expr = 316,               /* p_top_expr  */
  YYSYMBOL_p_top_expr_body = 317,          /* p_top_expr_body  */
  YYSYMBOL_p_expr = 318,                   /* p_expr  */
  YYSYMBOL_p_as = 319,                     /* p_as  */
  YYSYMBOL_p_alt = 320,                    /* p_alt  */
  YYSYMBOL_p_lparen = 321,                 /* p_lparen  */
  YYSYMBOL_p_lbracket = 322,               /* p_lbracket  */
  YYSYMBOL_p_expr_basic = 323,             /* p_expr_basic  */
  YYSYMBOL_324_41 = 324,                   /* @41  */
  YYSYMBOL_325_42 = 325,                   /* @42  */
  YYSYMBOL_p_args = 326,                   /* p_args  */
  YYSYMBOL_p_args_head = 327,              /* p_args_head  */
  YYSYMBOL_p_args_tail = 328,              /* p_args_tail  */
  YYSYMBOL_p_find = 329,                   /* p_find  */
  YYSYMBOL_p_rest = 330,                   /* p_rest  */
  YYSYMBOL_p_args_post = 331,              /* p_args_post  */
  YYSYMBOL_p_arg = 332,                    /* p_arg  */
  YYSYMBOL_p_kwargs = 333,                 /* p_kwargs  */
  YYSYMBOL_p_kwarg = 334,                  /* p_kwarg  */
  YYSYMBOL_p_kw = 335,                     /* p_kw  */
  YYSYMBOL_p_kw_label = 336,               /* p_kw_label  */
  YYSYMBOL_p_kwrest = 337,                 /* p_kwrest  */
  YYSYMBOL_p_kwnorest = 338,               /* p_kwnorest  */
  YYSYMBOL_p_any_kwrest = 339,             /* p_any_kwrest  */
  YYSYMBOL_p_value = 340,                  /* p_value  */
  YYSYMBOL_p_primitive = 341,              /* p_primitive  */
  YYSYMBOL_p_variable = 342,               /* p_variable  */
  YYSYMBOL_p_var_ref = 343,                /* p_var_ref  */
  YYSYMBOL_p_expr_ref = 344,               /* p_expr_ref  */
  YYSYMBOL_p_const = 345,                  /* p_const  */
  YYSYMBOL_opt_rescue = 346,               /* opt_rescue  */
  YYSYMBOL_exc_list = 347,                 /* exc_list  */
  YYSYMBOL_exc_var = 348,                  /* exc_var  */
  YYSYMBOL_opt_ensure = 349,               /* opt_ensure  */
  YYSYMBOL_literal = 350,                  /* literal  */
  YYSYMBOL_strings = 351,                  /* strings  */
  YYSYMBOL_string = 352,                   /* string  */
  YYSYMBOL_string1 = 353,                  /* string1  */
  YYSYMBOL_xstring = 354,                  /* xstring  */
  YYSYMBOL_regexp = 355,                   /* regexp  */
  YYSYMBOL_words_sep = 356,                /* words_sep  */
  YYSYMBOL_words = 357,                    /* words  */
  YYSYMBOL_word_list = 358,                /* word_list  */
  YYSYMBOL_word = 359,                     /* word  */
  YYSYMBOL_symbols = 360,                  /* symbols  */
  YYSYMBOL_symbol_list = 361,              /* symbol_list  */
  YYSYMBOL_qwords = 362,                   /* qwords  */
  YYSYMBOL_qsymbols = 363,                 /* qsymbols  */
  YYSYMBOL_qword_list = 364,               /* qword_list  */
  YYSYMBOL_qsym_list = 365,                /* qsym_list  */
  YYSYMBOL_string_contents = 366,          /* string_contents  */
  YYSYMBOL_xstring_contents = 367,         /* xstring_contents  */
  YYSYMBOL_regexp_contents = 368,          /* regexp_contents  */
  YYSYMBOL_string_content = 369,           /* string_content  */
  YYSYMBOL_370_43 = 370,                   /* @43  */
  YYSYMBOL_371_44 = 371,                   /* $@44  */
  YYSYMBOL_372_45 = 372,                   /* @45  */
  YYSYMBOL_373_46 = 373,                   /* @46  */
  YYSYMBOL_374_47 = 374,                   /* @47  */
  YYSYMBOL_375_48 = 375,                   /* @48  */
  YYSYMBOL_string_dvar = 376,              /* string_dvar  */
  YYSYMBOL_symbol = 377,                   /* symbol  */
  YYSYMBOL_ssym = 378,                     /* ssym  */
  YYSYMBOL_sym = 379,                      /* sym  */
  YYSYMBOL_dsym = 380,                     /* dsym  */
  YYSYMBOL_numeric = 381,                  /* numeric  */
  YYSYMBOL_simple_numeric = 382,           /* simple_numeric  */
  YYSYMBOL_nonlocal_var = 383,             /* nonlocal_var  */
  YYSYMBOL_user_variable = 384,            /* user_variable  */
  YYSYMBOL_keyword_variable = 385,         /* keyword_variable  */
  YYSYMBOL_var_ref = 386,                  /* var_ref  */
  YYSYMBOL_var_lhs = 387,                  /* var_lhs  */
  YYSYMBOL_backref = 388,                  /* backref  */
  YYSYMBOL_superclass = 389,               /* superclass  */
  YYSYMBOL_390_49 = 390,                   /* $@49  */
  YYSYMBOL_f_opt_paren_args = 391,         /* f_opt_paren_args  */
  YYSYMBOL_f_paren_args = 392,             /* f_paren_args  */
  YYSYMBOL_f_arglist = 393,                /* f_arglist  */
  YYSYMBOL_394_50 = 394,                   /* @50  */
  YYSYMBOL_args_tail = 395,                /* args_tail  */
  YYSYMBOL_opt_args_tail = 396,            /* opt_args_tail  */
  YYSYMBOL_f_args = 397,                   /* f_args  */
  YYSYMBOL_args_forward = 398,             /* args_forward  */
  YYSYMBOL_f_bad_arg = 399,                /* f_bad_arg  */
  YYSYMBOL_f_norm_arg = 400,               /* f_norm_arg  */
  YYSYMBOL_f_arg_asgn = 401,               /* f_arg_asgn  */
  YYSYMBOL_f_arg_item = 402,               /* f_arg_item  */
  YYSYMBOL_f_arg = 403,                    /* f_arg  */
  YYSYMBOL_f_label = 404,                  /* f_label  */
  YYSYMBOL_f_kw = 405,                     /* f_kw  */
  YYSYMBOL_f_block_kw = 406,               /* f_block_kw  */
  YYSYMBOL_f_block_kwarg = 407,            /* f_block_kwarg  */
  YYSYMBOL_f_kwarg = 408,                  /* f_kwarg  */
  YYSYMBOL_kwrest_mark = 409,              /* kwrest_mark  */
  YYSYMBOL_f_no_kwarg = 410,               /* f_no_kwarg  */
  YYSYMBOL_f_kwrest = 411,                 /* f_kwrest  */
  YYSYMBOL_f_opt = 412,                    /* f_opt  */
  YYSYMBOL_f_block_opt = 413,              /* f_block_opt  */
  YYSYMBOL_f_block_optarg = 414,           /* f_block_optarg  */
  YYSYMBOL_f_optarg = 415,                 /* f_optarg  */
  YYSYMBOL_restarg_mark = 416,             /* restarg_mark  */
  YYSYMBOL_f_rest_arg = 417,               /* f_rest_arg  */
  YYSYMBOL_blkarg_mark = 418,              /* blkarg_mark  */
  YYSYMBOL_f_block_arg = 419,              /* f_block_arg  */
  YYSYMBOL_opt_f_block_arg = 420,          /* opt_f_block_arg  */
  YYSYMBOL_singleton = 421,                /* singleton  */
  YYSYMBOL_422_51 = 422,                   /* $@51  */
  YYSYMBOL_assoc_list = 423,               /* assoc_list  */
  YYSYMBOL_assocs = 424,                   /* assocs  */
  YYSYMBOL_assoc = 425,                    /* assoc  */
  YYSYMBOL_operation = 426,                /* operation  */
  YYSYMBOL_operation2 = 427,               /* operation2  */
  YYSYMBOL_operation3 = 428,               /* operation3  */
  YYSYMBOL_dot_or_colon = 429,             /* dot_or_colon  */
  YYSYMBOL_call_op = 430,                  /* call_op  */
  YYSYMBOL_call_op2 = 431,                 /* call_op2  */
  YYSYMBOL_opt_terms = 432,                /* opt_terms  */
  YYSYMBOL_opt_nl = 433,                   /* opt_nl  */
  YYSYMBOL_rparen = 434,                   /* rparen  */
  YYSYMBOL_rbracket = 435,                 /* rbracket  */
  YYSYMBOL_rbrace = 436,                   /* rbrace  */
  YYSYMBOL_trailer = 437,                  /* trailer  */
  YYSYMBOL_term = 438,                     /* term  */
  YYSYMBOL_terms = 439,                    /* terms  */
  YYSYMBOL_none = 440                      /* none  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if 1

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* 1 */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE) \
             + YYSIZEOF (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   15199

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  163
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  278
/* YYNRULES -- Number of rules.  */
#define YYNRULES  786
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1310

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   362


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,    72,
     162,    75,    73,    74,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,   161,   149,     2,     2,     2,   147,   142,     2,
     157,   158,   145,   143,   155,   144,    69,   146,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   137,   160,
     139,   135,   138,   136,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   154,    70,   159,   141,     2,   156,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   152,   140,   153,   150,     2,    89,    90,
      91,    92,    76,    77,    78,    79,    95,    96,    84,    83,
      80,    81,    82,    87,    88,    93,    94,    98,    85,    86,
      97,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    71,    99,   100,   101,   102,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
     134,   148,   151
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,  1581,  1581,  1581,  1607,  1613,  1620,  1627,  1636,  1637,
    1643,  1656,  1654,  1665,  1676,  1682,  1689,  1696,  1705,  1710,
    1709,  1719,  1719,  1726,  1733,  1743,  1752,  1759,  1767,  1775,
    1787,  1799,  1809,  1823,  1824,  1832,  1839,  1847,  1854,  1855,
    1864,  1871,  1878,  1886,  1893,  1900,  1908,  1915,  1925,  1937,
    1947,  1948,  1955,  1961,  1966,  1975,  1978,  1979,  1983,  1987,
    1991,  1996,  2004,  1995,  2018,  2026,  2017,  2039,  2042,  2064,
    2074,  2073,  2092,  2097,  2105,  2105,  2105,  2111,  2112,  2115,
    2116,  2125,  2135,  2145,  2154,  2165,  2172,  2179,  2186,  2193,
    2201,  2209,  2216,  2223,  2232,  2233,  2242,  2243,  2252,  2259,
    2266,  2273,  2280,  2287,  2294,  2301,  2308,  2315,  2324,  2325,
    2334,  2341,  2350,  2357,  2366,  2373,  2380,  2387,  2397,  2404,
    2414,  2421,  2428,  2438,  2445,  2452,  2459,  2466,  2473,  2480,
    2487,  2494,  2504,  2512,  2515,  2522,  2529,  2538,  2539,  2540,
    2541,  2546,  2549,  2556,  2559,  2566,  2566,  2576,  2577,  2578,
    2579,  2580,  2581,  2582,  2583,  2584,  2585,  2586,  2587,  2588,
    2589,  2590,  2591,  2592,  2593,  2594,  2595,  2596,  2597,  2598,
    2599,  2600,  2601,  2602,  2603,  2604,  2605,  2608,  2608,  2608,
    2609,  2609,  2610,  2610,  2610,  2611,  2611,  2611,  2611,  2612,
    2612,  2612,  2612,  2613,  2613,  2613,  2614,  2614,  2614,  2614,
    2615,  2615,  2615,  2615,  2616,  2616,  2616,  2616,  2617,  2617,
    2617,  2617,  2618,  2618,  2618,  2618,  2619,  2619,  2622,  2629,
    2636,  2643,  2650,  2657,  2664,  2672,  2680,  2688,  2697,  2706,
    2714,  2722,  2730,  2738,  2742,  2746,  2750,  2754,  2758,  2762,
    2766,  2770,  2774,  2778,  2782,  2786,  2790,  2791,  2795,  2799,
    2803,  2807,  2811,  2815,  2819,  2823,  2827,  2831,  2835,  2835,
    2840,  2849,  2859,  2871,  2877,  2878,  2885,  2891,  2892,  2893,
    2894,  2897,  2901,  2908,  2914,  2921,  2922,  2926,  2933,  2942,
    2947,  2957,  2964,  2976,  2990,  2991,  2994,  2995,  2996,  3000,
    3007,  3016,  3024,  3031,  3039,  3047,  3051,  3051,  3088,  3095,
    3107,  3111,  3118,  3125,  3132,  3143,  3150,  3157,  3171,  3172,
    3176,  3183,  3190,  3199,  3200,  3201,  3202,  3203,  3204,  3205,
    3206,  3207,  3208,  3209,  3217,  3216,  3231,  3231,  3238,  3238,
    3246,  3254,  3261,  3268,  3275,  3283,  3290,  3297,  3304,  3311,
    3311,  3316,  3320,  3324,  3331,  3332,  3340,  3341,  3352,  3363,
    3373,  3384,  3383,  3400,  3399,  3414,  3423,  3466,  3465,  3489,
    3488,  3511,  3510,  3535,  3533,  3552,  3550,  3569,  3576,  3583,
    3590,  3599,  3606,  3615,  3635,  3644,  3653,  3662,  3671,  3680,
    3690,  3700,  3707,  3717,  3726,  3732,  3738,  3744,  3759,  3766,
    3773,  3779,  3786,  3787,  3788,  3791,  3792,  3795,  3796,  3808,
    3809,  3818,  3819,  3822,  3830,  3839,  3846,  3855,  3862,  3869,
    3876,  3883,  3892,  3900,  3909,  3910,  3913,  3913,  3915,  3919,
    3923,  3927,  3933,  3938,  3943,  3953,  3957,  3961,  3965,  3969,
    3973,  3978,  3982,  3986,  3990,  3994,  3998,  4002,  4006,  4010,
    4016,  4017,  4023,  4033,  4046,  4050,  4059,  4061,  4065,  4070,
    4077,  4083,  4087,  4091,  4076,  4116,  4125,  4136,  4142,  4141,
    4153,  4163,  4177,  4184,  4191,  4200,  4209,  4217,  4225,  4232,
    4240,  4248,  4255,  4262,  4272,  4280,  4290,  4291,  4295,  4290,
    4312,  4313,  4317,  4312,  4336,  4344,  4351,  4359,  4368,  4380,
    4381,  4385,  4392,  4396,  4384,  4411,  4412,  4415,  4416,  4424,
    4434,  4435,  4440,  4448,  4452,  4456,  4462,  4465,  4474,  4477,
    4484,  4487,  4488,  4490,  4491,  4492,  4501,  4510,  4519,  4524,
    4533,  4542,  4551,  4556,  4560,  4564,  4570,  4569,  4581,  4586,
    4586,  4593,  4602,  4606,  4615,  4619,  4623,  4626,  4630,  4639,
    4643,  4649,  4656,  4664,  4673,  4674,  4683,  4692,  4696,  4700,
    4704,  4710,  4712,  4721,  4729,  4743,  4744,  4767,  4771,  4777,
    4783,  4784,  4787,  4788,  4797,  4806,  4814,  4822,  4823,  4824,
    4825,  4833,  4843,  4844,  4845,  4846,  4847,  4848,  4849,  4850,
    4851,  4858,  4861,  4871,  4882,  4891,  4900,  4907,  4914,  4923,
    4944,  4947,  4954,  4961,  4964,  4968,  4971,  4978,  4981,  4982,
    4985,  5002,  5003,  5004,  5013,  5023,  5032,  5038,  5039,  5042,
    5052,  5058,  5067,  5069,  5078,  5088,  5094,  5103,  5112,  5122,
    5128,  5138,  5144,  5154,  5164,  5183,  5189,  5199,  5209,  5250,
    5253,  5252,  5269,  5273,  5278,  5282,  5286,  5268,  5307,  5314,
    5321,  5328,  5331,  5332,  5335,  5345,  5346,  5349,  5359,  5360,
    5370,  5371,  5372,  5373,  5376,  5377,  5378,  5381,  5382,  5383,
    5386,  5387,  5388,  5389,  5390,  5391,  5392,  5395,  5408,  5417,
    5424,  5433,  5434,  5438,  5437,  5447,  5455,  5456,  5464,  5476,
    5477,  5477,  5493,  5497,  5501,  5505,  5509,  5519,  5524,  5529,
    5533,  5537,  5541,  5545,  5549,  5553,  5557,  5561,  5565,  5569,
    5573,  5577,  5581,  5586,  5592,  5605,  5614,  5623,  5632,  5643,
    5644,  5652,  5661,  5669,  5690,  5692,  5705,  5715,  5724,  5735,
    5743,  5753,  5760,  5770,  5777,  5786,  5787,  5790,  5798,  5806,
    5816,  5827,  5838,  5845,  5854,  5861,  5870,  5871,  5874,  5882,
    5892,  5893,  5896,  5904,  5914,  5918,  5924,  5929,  5929,  5953,
    5954,  5963,  5965,  5988,  5999,  6006,  6015,  6023,  6040,  6054,
    6055,  6056,  6059,  6060,  6063,  6064,  6065,  6068,  6069,  6072,
    6073,  6076,  6077,  6080,  6081,  6084,  6085,  6088,  6091,  6094,
    6097,  6098,  6101,  6102,  6109,  6110,  6114
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if 1
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end-of-input\"", "error", "\"invalid token\"", "\"`class'\"",
  "\"`module'\"", "\"`def'\"", "\"`undef'\"", "\"`begin'\"",
  "\"`rescue'\"", "\"`ensure'\"", "\"`end'\"", "\"`if'\"", "\"`unless'\"",
  "\"`then'\"", "\"`elsif'\"", "\"`else'\"", "\"`case'\"", "\"`when'\"",
  "\"`while'\"", "\"`until'\"", "\"`for'\"", "\"`break'\"", "\"`next'\"",
  "\"`redo'\"", "\"`retry'\"", "\"`in'\"", "\"`do'\"",
  "\"`do' for condition\"", "\"`do' for block\"", "\"`do' for lambda\"",
  "\"`return'\"", "\"`yield'\"", "\"`super'\"", "\"`self'\"", "\"`nil'\"",
  "\"`true'\"", "\"`false'\"", "\"`and'\"", "\"`or'\"", "\"`not'\"",
  "\"`if' modifier\"", "\"`unless' modifier\"", "\"`while' modifier\"",
  "\"`until' modifier\"", "\"`rescue' modifier\"", "\"`alias'\"",
  "\"`defined?'\"", "\"`BEGIN'\"", "\"`END'\"", "\"`__LINE__'\"",
  "\"`__FILE__'\"", "\"`__ENCODING__'\"", "\"local variable or method\"",
  "\"method\"", "\"global variable\"", "\"instance variable\"",
  "\"constant\"", "\"class variable\"", "\"label\"", "\"integer literal\"",
  "\"float literal\"", "\"rational literal\"", "\"imaginary literal\"",
  "\"char literal\"", "\"numbered reference\"", "\"back reference\"",
  "\"literal content\"", "tREGEXP_END", "\"dummy end\"", "'.'",
  "\"backslash\"", "\"escaped space\"", "\"escaped horizontal tab\"",
  "\"escaped form feed\"", "\"escaped carriage return\"",
  "\"escaped vertical tab\"", "\"unary+\"", "\"unary-\"", "\"**\"",
  "\"<=>\"", "\"==\"", "\"===\"", "\"!=\"", "\">=\"", "\"<=\"", "\"&&\"",
  "\"||\"", "\"=~\"", "\"!~\"", "\"..\"", "\"...\"", "\"(..\"", "\"(...\"",
  "\"[]\"", "\"[]=\"", "\"<<\"", "\">>\"", "\"&.\"", "\"::\"",
  "\":: at EXPR_BEG\"", "\"operator-assignment\"", "\"=>\"", "\"(\"",
  "\"( arg\"", "\")\"", "\"[\"", "\"{\"", "\"{ arg\"", "\"*\"",
  "\"**arg\"", "\"&\"", "\"->\"", "\"symbol literal\"",
  "\"string literal\"", "\"backtick literal\"", "\"regexp literal\"",
  "\"word list\"", "\"verbatim word list\"", "\"symbol list\"",
  "\"verbatim symbol list\"", "\"terminator\"", "\"'}'\"", "tSTRING_DBEG",
  "tSTRING_DVAR", "tLAMBEG", "tLABEL_END", "tIGNORED_NL", "tCOMMENT",
  "tEMBDOC_BEG", "tEMBDOC", "tEMBDOC_END", "tHEREDOC_BEG", "tHEREDOC_END",
  "k__END__", "tLOWEST", "'='", "'?'", "':'", "'>'", "'<'", "'|'", "'^'",
  "'&'", "'+'", "'-'", "'*'", "'/'", "'%'", "tUMINUS_NUM", "'!'", "'~'",
  "tLAST_TOKEN", "'{'", "'}'", "'['", "','", "'`'", "'('", "')'", "']'",
  "';'", "' '", "'\\n'", "$accept", "program", "$@1", "top_compstmt",
  "top_stmts", "top_stmt", "begin_block", "bodystmt", "$@2", "compstmt",
  "stmts", "stmt_or_begin", "$@3", "stmt", "$@4", "command_asgn",
  "endless_command", "command_rhs", "expr", "@5", "@6", "@7", "@8",
  "def_name", "defn_head", "defs_head", "$@9", "expr_value",
  "expr_value_do", "$@10", "$@11", "command_call", "block_command",
  "cmd_brace_block", "fcall", "command", "mlhs", "mlhs_inner",
  "mlhs_basic", "mlhs_item", "mlhs_head", "mlhs_post", "mlhs_node", "lhs",
  "cname", "cpath", "fname", "fitem", "undef_list", "$@12", "op",
  "reswords", "arg", "$@13", "endless_arg", "relop", "rel_expr",
  "lex_ctxt", "arg_value", "aref_args", "arg_rhs", "paren_args",
  "opt_paren_args", "opt_call_args", "call_args", "command_args", "$@14",
  "block_arg", "opt_block_arg", "args", "mrhs_arg", "mrhs", "primary",
  "$@15", "$@16", "$@17", "$@18", "@19", "@20", "$@21", "$@22", "$@23",
  "$@24", "$@25", "primary_value", "k_begin", "k_if", "k_unless",
  "k_while", "k_until", "k_case", "k_for", "k_class", "k_module", "k_def",
  "k_do", "k_do_block", "k_rescue", "k_ensure", "k_when", "k_else",
  "k_elsif", "k_end", "k_return", "then", "do", "if_tail", "opt_else",
  "for_var", "f_marg", "f_marg_list", "f_margs", "f_rest_marg",
  "f_any_kwrest", "f_eq", "$@26", "block_args_tail", "opt_block_args_tail",
  "excessed_comma", "block_param", "opt_block_param", "block_param_def",
  "opt_bv_decl", "bv_decls", "bvar", "lambda", "@27", "@28", "@29", "$@30",
  "f_larglist", "lambda_body", "$@31", "do_block", "block_call",
  "method_call", "brace_block", "brace_body", "@32", "@33", "@34",
  "do_body", "@35", "@36", "@37", "case_args", "case_body", "cases",
  "p_case_body", "@38", "@39", "$@40", "p_cases", "p_top_expr",
  "p_top_expr_body", "p_expr", "p_as", "p_alt", "p_lparen", "p_lbracket",
  "p_expr_basic", "@41", "@42", "p_args", "p_args_head", "p_args_tail",
  "p_find", "p_rest", "p_args_post", "p_arg", "p_kwargs", "p_kwarg",
  "p_kw", "p_kw_label", "p_kwrest", "p_kwnorest", "p_any_kwrest",
  "p_value", "p_primitive", "p_variable", "p_var_ref", "p_expr_ref",
  "p_const", "opt_rescue", "exc_list", "exc_var", "opt_ensure", "literal",
  "strings", "string", "string1", "xstring", "regexp", "words_sep",
  "words", "word_list", "word", "symbols", "symbol_list", "qwords",
  "qsymbols", "qword_list", "qsym_list", "string_contents",
  "xstring_contents", "regexp_contents", "string_content", "@43", "$@44",
  "@45", "@46", "@47", "@48", "string_dvar", "symbol", "ssym", "sym",
  "dsym", "numeric", "simple_numeric", "nonlocal_var", "user_variable",
  "keyword_variable", "var_ref", "var_lhs", "backref", "superclass",
  "$@49", "f_opt_paren_args", "f_paren_args", "f_arglist", "@50",
  "args_tail", "opt_args_tail", "f_args", "args_forward", "f_bad_arg",
  "f_norm_arg", "f_arg_asgn", "f_arg_item", "f_arg", "f_label", "f_kw",
  "f_block_kw", "f_block_kwarg", "f_kwarg", "kwrest_mark", "f_no_kwarg",
  "f_kwrest", "f_opt", "f_block_opt", "f_block_optarg", "f_optarg",
  "restarg_mark", "f_rest_arg", "blkarg_mark", "f_block_arg",
  "opt_f_block_arg", "singleton", "$@51", "assoc_list", "assocs", "assoc",
  "operation", "operation2", "operation3", "dot_or_colon", "call_op",
  "call_op2", "opt_terms", "opt_nl", "rparen", "rbracket", "rbrace",
  "trailer", "term", "terms", "none", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-1090)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-787)

#define yytable_value_is_error(Yyn) \
  ((Yyn) == YYTABLE_NINF)

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
   -1090,   150,  4426, -1090, -1090, -1090, -1090, -1090,  9802, -1090,
   -1090, -1090, -1090, -1090, -1090, -1090, 10622, 10622, -1090, -1090,
   -1090,  6144,  5676, -1090, -1090, -1090, -1090,    -1,  9648,    51,
      87,   195, -1090, -1090, -1090,  4895,  5832, -1090, -1090,  5052,
   -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090, 12195, 12195,
   12195, 12195,   377,  7782,  8544, 11106, 11469, 10104, -1090,  9494,
   -1090, -1090, -1090,   217,   217,   217,   217,  1351, 12316, 12195,
   -1090,   562, -1090,  1466, -1090,   215,   328,   328, -1090, -1090,
     252,   375,   279, -1090,   280, 12800, -1090,   344,  3485,    81,
     101,   357, -1090, 10501, 10501, -1090, -1090,  8665, 12919, 13038,
   13157,  9339, 10622, -1090,   901,   137, -1090, -1090,   378, -1090,
   -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090,
   -1090, -1090,   322,   487, -1090,   425,   527, -1090, -1090, -1090,
   -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090,
   -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090,
   -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090,
   -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090,
   -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090,
   -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090,
   -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090,
   -1090, -1090, -1090, -1090, -1090,   466, -1090, -1090, -1090,   506,
   12195,   619,  7942, 12195, 12195, 12195, -1090, 12195,   328,   328,
   -1090,   553,  4077,   651, -1090, -1090,   592,   538,    37,    46,
     683,    83,   620, -1090, -1090,  9097, -1090, 10622, 10743, -1090,
   -1090,  9218, -1090, 12437,   439, -1090,   637,  8102, -1090,  8262,
   -1090, -1090,   648,   662,   252, -1090,   639, -1090,   725,  5022,
    5022,   625, -1090,  7782,   670,   562, -1090,  1466,    51,   696,
   -1090,  1466,    51,   677,     0,   135, -1090,   651,   688,   135,
   -1090,    51,   787,  1351, 13276,   689,   689,   694, -1090,   769,
     832,   857,   881, -1090, -1090,   347, -1090, -1090,   447,   551,
     646, -1090,   701,   701,   701,   701,   803, -1090, -1090, -1090,
   -1090, -1090, -1090, -1090,  8787, 10501, 10501, 10501, 10501, 10380,
   12437, 12437,  2753,   755,   761, -1090,  2753, -1090,   765, -1090,
   -1090, -1090, -1090,   800, -1090, -1090, -1090, -1090, -1090, -1090,
   -1090,  7782, 10242,   760, -1090, -1090, 12195, 12195, 12195, 12195,
   12195, -1090, -1090, 12195, 12195, 12195, 12195, 12195, 12195, 12195,
   12195, -1090, 12195, -1090, -1090, 12195, 12195, 12195, 12195, 12195,
   12195, 12195, 12195, 12195, 12195, -1090, -1090, 13558, 10622, 13657,
    6928, -1090,   215,   147,   147,  7660, 10501,  7660,   562, -1090,
     751,   861, -1090, -1090,  1077,   895,    78,    99,   155,   756,
    1031, 12437,   560, -1090,   786,  1150, -1090, -1090, -1090, -1090,
      76,    84,   436,   603,   617,   636,   675,   766,   770, -1090,
   -1090, -1090, -1090,   773, -1090, -1090, -1090, 15043, -1090, -1090,
   -1090, -1090, -1090, -1090,   239, -1090, -1090, -1090,   735,   797,
     805, -1090, 12195, 10864, -1090, -1090, 13756, 10622, 13855, -1090,
   -1090, 11227, -1090,    51,   788, -1090, -1090, 12195,    51, -1090,
     789,    51,   794, -1090,   393, -1090, -1090, -1090, -1090, -1090,
    9802, -1090, 12195,   798,   804, 13756, 13855, -1090,    87,    51,
   -1090, -1090,  8945,   807,    51, -1090, -1090, 11348, -1090, -1090,
   11469, -1090, -1090, -1090,   637,  1204, -1090, -1090,   808, -1090,
   13276, 13954, 10622, 14053, -1090, -1090, -1090, -1090, -1090, -1090,
   -1090, -1090, -1090, -1090, -1090, -1090,  1067,    75,  1074,   112,
   12195, -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090,
   -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090,  1223, -1090,
   -1090, -1090, -1090, -1090,   816, -1090, -1090,    51, -1090, -1090,
   -1090,   845, -1090,   828, 12195, -1090,   830,   431, -1090, -1090,
   -1090,   836,   935,   838,   943, -1090, 12558,  6928,   562, 12558,
    6928,   843, -1090, -1090, -1090,   111, -1090,   111, 11590, -1090,
      51, 13276,   849, -1090, 11590, -1090,   725,  3569,  3569,  3569,
    3569,  3836,  3288,  3569,  3569,  5022,  5022,  1091,  1091, -1090,
    4865,  1726,  1726,  1444,   671,   671,   725,   725,   725,  1760,
    1760,  6300,  5208,  6612,  5364, -1090, -1090,   662, -1090,    51,
     851,   721, -1090,   730, -1090, -1090,  5988,   111,   999, -1090,
    7050,   996,  7416,   111,   141,   111,   985,   995,   168, 14152,
   10622, 14251, -1090,   215, -1090,  1204, -1090, -1090, -1090, 14350,
   10622, 14449,  6928, 12437, -1090, -1090, -1090, -1090, -1090,  2920,
   12316, 12316,  9802, 12195, 12679, 12679, 12195, -1090, 12195,   651,
   -1090,   620,  4584,  5520,    51,   273,   292, 12195, 12195, -1090,
   -1090, -1090, -1090, 10985, -1090, 11227, -1090, -1090, 12437,  4077,
   -1090, -1090,   662,   662, 12195, -1090,   255, -1090, -1090,   135,
   13276,   808,   366,   669,    51,   416,   418,  2489, -1090,  1401,
   -1090,   358, -1090,   217, -1090, -1090,   358,   217, -1090,   725,
    1223,  1369, -1090,   858,    51,   859, -1090,    33, -1090, -1090,
   -1090, 12195,   883,  2753, -1090, -1090,    80, -1090, -1090, -1090,
    2753, -1090, -1090,  1651, -1090, -1090,    -1,   972, -1090,  4077,
     976,   111, -1090,   972,   976,   111, -1090, -1090,   873, -1090,
   -1090, -1090, -1090, -1090, 12195, -1090,   875,   885,   992, -1090,
   -1090,   808, 13276, -1090, -1090,   997,   911,  6267, -1090, -1090,
   -1090,  1224,   317,  3734,  3734,   907, -1090, -1090, -1090,   800,
     888,   833, 10864, -1090, -1090, -1090, -1090,   800, -1090, -1090,
   -1090, 11711,   199, -1090,   618, -1090,  1036, -1090, -1090, -1090,
   -1090, -1090, -1090,   995,   111, -1090, 11832,   111,   177,   241,
      51,   176,   186,  7660,   562, 10501,  6928,  1214,   669, -1090,
      51,   111,   393,  9956,   137,   375, -1090,  6423, -1090, -1090,
   -1090, -1090, -1090,    -1, -1090, -1090, -1090, -1090,   412, -1090,
   -1090,    51,   898,   393, -1090, -1090, -1090,   541,  2753, -1090,
   -1090, -1090, -1090, -1090, -1090, -1090, -1090,   701, -1090,   701,
     701,   701,    51, -1090,  1223, -1090,  1295, -1090, -1090, -1090,
   -1090, -1090,   905,   908, -1090,  1004,   816,   910, -1090,   914,
   -1090,   910, 12558, 12195, 12195, -1090, -1090,   931, -1090,   931,
     917, 11953, 10380,   808, 10380, -1090, 12195, 14548, 10622, 14647,
   -1090, -1090, -1090,  3186,  3186,   638, -1090,  3296,   218,  1025,
   -1090,  1230, -1090, -1090,   352, -1090,   939, -1090, -1090, -1090,
     930, -1090,   938, -1090,  4714, -1090, -1090, -1090, -1090,   762,
   -1090, -1090, -1090,   302, -1090, -1090, -1090, -1090, -1090, -1090,
   -1090, -1090, -1090,   577, -1090, 12195, 12316, 12316, -1090, -1090,
   -1090, 12316, 12316, -1090,   875, -1090,   998, -1090, -1090, -1090,
    7660, -1090, -1090, -1090, -1090,  7660, 10501,   111, -1090, -1090,
     111, -1090, -1090,   111, -1090, 12195, -1090,   105, -1090,   281,
     111,  6928,   562,   111, -1090, -1090, -1090, -1090, -1090, -1090,
   12679, 12195, 12195, -1090, 12195, 12195, -1090, 11227, -1090,    51,
      72, -1090, -1090, -1090,   941,   946,  2753, -1090,  1651, -1090,
   -1090,  1651, -1090,  1651, -1090, -1090,   972,   976,  4077,  4077,
    1857,  8262, -1090, -1090,  6928, 12195,   952, -1090, -1090, 12316,
    4077,  6456,  6768,    51,   462,   468, -1090, -1090, -1090, -1090,
    4714,   385,    51,  4249, -1090,    51,   955, -1090,   542,   959,
   -1090, -1090,   909, -1090, 10501, -1090,  1062,  4249,  4714,  4714,
     542,  1014,  3186,  3186,   638,   665,   549,  3734,  3734, -1090,
    4077, -1090, -1090, -1090, -1090, 12316, -1090, -1090, -1090, -1090,
   13395,   147, -1090, -1090,  7538, -1090,   147, -1090, -1090,  3734,
   -1090, -1090, 12074,  7172, -1090,   111, -1090, -1090, 12195,   958,
     961, -1090,  8262, -1090, -1090,  1295,  1295,   910,   963,   910,
     910,   816, -1090,    51,   990,   845,   970, 13514, -1090,   981,
   -1090,   983,   984, -1090, -1090, -1090,   993,   510,    41, -1090,
    1014,  1000,  1003, -1090, -1090, -1090,    51, -1090, -1090,    51,
   -1090, -1090,  1008, -1090,  1011, -1090, -1090, -1090, -1090, -1090,
   -1090, -1090, -1090,    51,    51,    51,    51,    51,    51, -1090,
   -1090,  1075, -1090,  1229,   158,   184,   191,  6928,  1141,  7050,
     147,   897, 12195, -1090,   742, -1090, -1090,  1390,  6928,  1006,
    8422,   946, -1090,  1651, -1090, -1090, -1090, -1090,  1030, -1090,
   13514,  1436, -1090, -1090,  1117,  1247,    80, -1090,  1436, -1090,
    2029, -1090, -1090,  4714, -1090,   395, -1090, -1090,  4714,  4249,
   -1090, -1090, -1090, -1090, -1090, -1090,   202, 14746, 10622, 14845,
     999, -1090,   618, -1090, 10501, 10501, -1090, -1090, -1090, -1090,
   -1090,   333, -1090, -1090,   111, -1090,  1058,   910, -1090,  1247,
   -1090,  1026,  1044, -1090, 14944, -1090,   816,  1045, -1090,  1048,
    1045,  1050,  1050, -1090, -1090,   134,   178,    51,   210,   227,
   -1090, -1090,  7294, -1090, -1090,  1390, -1090, -1090, -1090, -1090,
    1436, -1090,  2029, -1090,  1053,  1056, -1090,  2029, -1090,  2029,
   -1090, -1090,  4714,   272,   691, -1090,  1045,  1052,  1045,  1045,
   -1090, -1090, -1090, -1090,  2029, -1090, -1090, -1090,  1045, -1090
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,     0,     0,     1,    39,   379,   380,   381,     0,   372,
     373,   374,   377,   375,   376,   378,   367,   368,   369,   370,
     391,   296,   296,   661,   660,   662,   663,   775,     0,   775,
       0,     0,   665,   664,   666,   759,   761,   655,   654,   760,
     656,   650,   651,   652,   653,   601,   671,   672,     0,     0,
       0,     0,     0,     0,     0,   786,   786,   106,   450,   623,
     623,   625,   627,     0,     0,     0,     0,     0,     0,     0,
       3,   773,     6,     8,    33,    38,   680,   680,    56,    78,
     296,    77,     0,    94,     0,    98,   108,     0,    67,   246,
     263,     0,   324,     0,     0,    74,    74,     0,     0,     0,
       0,     0,   335,   346,    79,   344,   313,   314,   600,   602,
     315,   316,   317,   319,   318,   320,   599,   642,   643,   598,
     648,   659,   667,   668,   321,     0,   322,    82,     5,   187,
     198,   188,   211,   184,   204,   194,   193,   214,   215,   209,
     192,   191,   186,   212,   216,   217,   196,   185,   199,   203,
     205,   197,   190,   206,   213,   208,   207,   200,   210,   195,
     183,   202,   201,   182,   189,   180,   181,   177,   178,   179,
     137,   139,   138,   172,   173,   168,   150,   151,   152,   159,
     156,   158,   153,   154,   174,   175,   160,   161,   165,   169,
     155,   157,   147,   148,   149,   162,   163,   164,   166,   167,
     170,   171,   176,   142,   144,    26,   140,   141,   143,     0,
     755,     0,     0,   304,   758,   299,   623,     0,   680,   680,
     291,     0,   274,   302,    92,   295,   786,     0,   667,   668,
       0,   322,   786,   751,    93,   775,    90,     0,   786,   471,
      89,   775,   776,     0,     0,    21,   258,     0,     9,     0,
     367,   368,   338,   472,     0,   240,     0,   335,   241,   231,
     232,   332,    19,     0,     0,   773,    16,    18,   775,    96,
      15,   328,   775,     0,   775,   775,   275,     0,     0,   775,
     749,   775,     0,     0,     0,   680,   680,   104,   371,     0,
     114,   115,   122,   451,   645,     0,   644,   646,     0,     0,
       0,   607,   610,   619,   615,   621,   649,    60,   252,   253,
     782,   783,     4,   784,     0,     0,     0,     0,     0,     0,
       0,     0,   703,     0,   679,   363,   703,   677,     0,   365,
     382,   476,   465,    83,   480,   343,   383,   480,   461,   786,
     110,     0,   102,    99,   786,    64,     0,     0,     0,     0,
       0,   269,   270,     0,     0,     0,     0,   229,   230,     0,
       0,    61,     0,   267,   268,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   769,   770,     0,   786,     0,
       0,    73,    72,     0,     0,     0,     0,     0,   773,   353,
     774,     0,   402,   401,     0,     0,   667,   668,   322,   132,
     133,     0,     0,   135,   675,     0,   667,   668,   322,   361,
     207,   200,   210,   195,   177,   178,   179,   137,   138,   747,
      69,    68,   746,     0,    91,   772,   771,     0,   345,   603,
     786,   786,   145,   754,   332,   303,   757,   298,     0,     0,
       0,   786,     0,     0,   292,   301,     0,   786,     0,   786,
     786,     0,   293,   775,     0,   337,   297,   704,   775,   287,
     786,   775,   786,   286,   775,   342,    59,    23,    25,    24,
       0,   339,     0,     0,     0,     0,     0,   786,     0,   775,
     330,    14,     0,    95,   775,   327,   333,   781,   780,   276,
     781,   278,   334,   750,     0,   121,   649,   112,   107,   679,
       0,     0,   786,     0,   452,   629,   647,   632,   630,   624,
     604,   605,   626,   606,   628,   608,     0,     0,     0,     0,
       0,   785,     7,    27,    28,    29,    30,    31,    57,    58,
     710,   707,   706,   705,   708,   716,   725,   704,     0,   737,
     726,   741,   740,   736,   786,   727,   702,   775,   686,   709,
     711,   712,   714,   688,   718,   723,   786,   729,   415,   414,
     734,   688,   739,   688,   743,   685,     0,     0,     0,     0,
       0,     0,   477,   476,    84,     0,   481,     0,     0,   273,
     775,     0,   100,   111,     0,    65,   238,   245,   247,   248,
     249,   256,   257,   250,   251,   227,   228,   254,   255,    62,
     775,   242,   243,   244,   233,   234,   235,   236,   237,   271,
     272,   759,   761,   760,   763,   470,   762,   296,   468,   775,
     786,   759,   761,   760,   763,   469,   296,     0,   786,   393,
       0,   392,     0,     0,     0,     0,   351,     0,   332,     0,
     786,     0,    74,   359,   132,   133,   134,   673,   357,     0,
     786,     0,     0,     0,   767,   768,    70,   759,   760,   296,
       0,     0,     0,     0,     0,     0,     0,   753,   307,   305,
     300,   786,   759,   760,   775,   759,   760,     0,     0,   752,
     336,   777,   281,   288,   283,   290,   341,    22,     0,   259,
      10,    32,     0,   786,     0,    20,    97,    17,   329,   775,
       0,   105,   764,   120,   775,   759,   760,   703,   633,     0,
     609,     0,   612,     0,   617,   614,     0,     0,   618,   239,
       0,   413,   405,   407,   775,   410,   403,     0,   684,   745,
     678,     0,     0,     0,   695,   717,     0,   683,   559,   728,
       0,   698,   738,     0,   700,   742,   775,    47,    50,   264,
     261,     0,   681,    48,   262,     0,   474,   478,     0,   389,
     390,   475,   482,   460,   304,    34,   309,     0,    37,   308,
     109,   103,     0,    55,    40,    53,     0,   279,   302,   218,
      35,     0,   322,     0,     0,     0,   786,   786,   467,    87,
       0,   473,   288,   786,   786,   285,   466,    85,   284,   325,
     384,   786,   786,   590,   786,   394,   786,   349,   396,    75,
     395,   350,   491,     0,     0,   386,     0,     0,   764,   331,
     775,   759,   760,     0,     0,     0,     0,   132,   133,   136,
     775,     0,   775,     0,   462,    80,    41,   279,   219,    49,
     226,   146,   756,   775,   306,   294,   786,   786,   473,   786,
     786,   775,   786,   775,   225,   277,   113,   473,   703,   453,
     456,   634,   638,   639,   640,   631,   641,   611,   613,   620,
     616,   622,   775,   412,     0,   713,     0,   744,   730,   417,
     687,   715,   688,   688,   724,   729,   786,   688,   735,   688,
     712,   688,     0,     0,     0,   364,   366,   786,    81,   786,
     312,     0,     0,   101,     0,   786,     0,     0,   786,     0,
     582,   588,   555,     0,     0,     0,   529,   775,   526,   543,
     623,     0,   581,    66,   500,   506,   508,   510,   504,   503,
     539,   505,   548,   551,   554,   560,   561,   550,   513,   562,
     514,   567,   568,   569,   572,   573,   574,   575,   576,   578,
     577,   579,   580,   558,    63,     0,     0,     0,    88,   778,
     786,     0,     0,    86,   591,   592,   786,   593,   385,   387,
       0,    11,    13,   597,   388,     0,     0,     0,   397,   399,
       0,    76,   492,     0,   355,     0,   484,     0,   354,   473,
       0,     0,     0,     0,   473,   362,   748,    71,   463,   464,
       0,     0,     0,   786,     0,     0,   282,   289,   340,   775,
       0,   635,   404,   406,   408,   411,     0,   691,     0,   693,
     682,     0,   699,     0,   696,   701,    52,   266,    51,   265,
     775,     0,   441,   440,     0,   307,   310,    36,    54,     0,
     280,   759,   760,   775,   759,   760,   570,   571,   133,   586,
       0,   531,   775,   532,   536,   775,     0,   525,     0,     0,
     528,   542,     0,   583,     0,   584,     0,   501,     0,     0,
     549,   553,   565,   566,     0,   512,   511,     0,     0,   557,
     260,    46,   223,    45,   224,     0,    43,   221,    44,   222,
       0,     0,   595,   596,     0,   400,     0,   347,   348,     0,
     352,   485,     0,     0,   356,     0,   674,   358,     0,     0,
     444,   458,     0,   454,   636,     0,     0,   688,   688,   688,
     688,   786,   439,   775,     0,   712,   423,   720,   721,   786,
     732,   423,   423,   421,   479,   483,   311,   473,   775,   523,
     546,   534,   533,   524,   537,   623,   775,   779,   556,   775,
     507,   502,   539,   509,   540,   544,   552,   547,   563,   564,
     587,   522,   518,   775,   775,   775,   775,   775,   775,    42,
     220,     0,   594,     0,   667,   668,   322,     0,   786,     0,
       0,   497,     0,   486,   786,   360,   455,     0,     0,     0,
       0,   409,   692,     0,   689,   694,   697,   420,     0,   442,
       0,   424,   432,   430,     0,   719,     0,   419,     0,   435,
       0,   437,   530,     0,   538,     0,   527,   585,     0,     0,
     515,   516,   517,   519,   520,   521,   332,     0,   786,     0,
     786,    12,   786,   493,     0,     0,   487,   489,   490,   488,
     448,   775,   446,   449,     0,   457,     0,   688,   443,   731,
     422,   423,   423,   332,     0,   722,   786,   423,   733,   423,
     423,   535,   540,   541,   545,   764,   331,   775,   759,   760,
     589,   398,     0,   498,   499,     0,   445,   459,   637,   690,
       0,   427,     0,   429,   764,   331,   418,     0,   436,     0,
     433,   438,     0,   473,   786,   447,   423,   423,   423,   423,
     495,   496,   494,   428,     0,   425,   431,   434,   423,   426
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1090, -1090, -1090,   969, -1090,   903,   744,  -518, -1090,   -28,
   -1090,   747, -1090,    10, -1090,  -541,  -504,    20,   -74, -1090,
   -1090, -1090, -1090,   391,  2088,  2551, -1090,   -67,   -78, -1090,
   -1090,     4, -1090,  -467,  1118,   -10,  1143,  -147,    52,   -75,
   -1090,  -427,     1,  2899,  -385,  1145,   -44,    -6, -1090, -1090,
      -4, -1090,  3886, -1090,  -513,  1160, -1090,   864,   236, -1090,
      93,    26,   591,  -336,   160,    24, -1090,  -411,  -193,    21,
   -1090,  -499,   -16, -1090, -1090, -1090, -1090, -1090, -1090, -1090,
   -1090, -1090, -1090, -1090,   492, -1090, -1090, -1090, -1090, -1090,
   -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090, -1090,
     451, -1090,   392,  1545,  -348, -1090,    27,  -744, -1090,  -795,
    -799,   540,   389,  -508,   140, -1090,   238,   261, -1090, -1090,
     382, -1090,  -913, -1090,    -3,   189, -1090, -1090, -1090, -1090,
   -1090, -1090, -1090,   441, -1090, -1090,  -103,   716, -1090, -1090,
   -1090,   937, -1090, -1090, -1090, -1090,  -766, -1090,     2, -1090,
   -1090, -1090, -1090, -1090,  -733,   486, -1090, -1090, -1090, -1090,
     222, -1090, -1090,  -357, -1090,  -697,  -778,  -931,  -661,  -943,
    -189, -1090,   229, -1090, -1090,  -693,   237, -1090,  -786,   231,
   -1090, -1090, -1090,    79, -1090, -1090,   142,   759,   884, -1090,
    1202,   940,  1187,    29,  1367, -1090,   806,  1409, -1090,  1460,
    1635, -1090, -1090,   -57, -1090, -1090,  -168, -1090, -1090, -1090,
   -1090, -1090, -1090, -1090,     6, -1090, -1090, -1090, -1090,    42,
     -46,  2773,    14,  1227,  3102,  1794, -1090, -1090,    19,   588,
      74, -1090,  -197,  -356,  -300,  -194, -1089,  -338,  -321,  -704,
     455,   327,   593,   130, -1090, -1090,   373, -1090,  -691,  -680,
   -1068,   136,   606, -1090,  -548, -1090,  -265,  -528, -1090, -1090,
   -1090,   132,  -401,  -368,  -346, -1090, -1090,   -83, -1090,   -34,
     334,   170,   248,   194,  -209,   -36,    17,    -2
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     1,     2,    70,    71,    72,   248,   627,  1094,   628,
     265,   266,   478,   267,   470,    74,   747,   774,    75,   599,
     784,   585,   783,   420,   218,   219,   833,   383,   385,   386,
     981,    78,    79,   574,   254,    81,    82,   268,    83,    84,
      85,   498,    86,   221,   403,   404,   203,   204,   205,   662,
     614,   207,    88,   472,   750,   373,    89,   578,   223,   273,
     779,   615,   796,   458,   459,   236,   237,   225,   444,   620,
     768,   769,    90,   380,   272,   484,   688,   813,   637,   826,
     824,   652,   567,   570,   256,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   334,   337,   801,   970,   816,
     975,   976,   761,   257,   630,   809,   977,   978,   395,   722,
     723,   724,   725,   544,   731,   732,  1250,  1202,  1203,  1123,
    1031,  1032,  1109,  1241,  1242,   103,   293,   504,   707,  1010,
     859,  1113,  1188,   338,   104,   105,   335,   571,   572,   757,
     897,   575,   576,   762,   899,   987,   817,  1239,   814,   982,
    1099,  1272,  1302,  1180,   923,  1140,   925,   926,  1077,  1078,
     927,  1058,  1050,  1052,  1053,  1054,   929,   930,  1154,  1056,
     931,   932,   933,   934,   935,   545,   937,   938,   939,   940,
     941,   942,   943,   802,   966,  1091,   972,   106,   107,   108,
     109,   110,   111,   302,   112,   516,   711,   113,   518,   114,
     115,   517,   519,   295,   299,   300,   509,   709,   708,   861,
    1011,  1114,  1190,   865,   116,   117,   296,   118,   119,   120,
     121,   228,   229,   124,   230,   231,   648,   825,   323,   324,
     325,   326,   880,   734,   547,   548,   549,   550,   890,   552,
     553,   554,   555,  1128,  1129,   556,   557,   558,   559,   560,
    1130,  1131,   561,   562,   563,   564,   565,   728,   423,   653,
     278,   462,   233,   127,   692,   618,   656,   651,   427,   312,
     454,   455,   791,  1060,   489,   631,   390,   270
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     128,   551,   428,   298,   206,   551,   220,   220,   379,   616,
     343,   616,    73,   297,   208,   294,   123,   646,   387,   382,
     382,   426,   245,   382,   206,   264,   568,   384,   737,   881,
     388,   617,   670,   626,   208,   313,   632,   226,   226,   452,
     670,   288,   619,   773,   461,   886,   240,   983,   239,   751,
     679,   954,   755,   276,   280,   206,   754,   421,   287,   616,
     888,   313,   980,   389,   271,   753,   491,   123,   123,   288,
     493,   291,   307,   701,   327,   327,   274,  1015,   616,  1013,
     616,   659,   288,   288,   288,   780,   928,   928,   314,   679,
     936,   936,   220,   303,   304,   305,   328,   206,  1243,   291,
     617,  1111,   626,  -123,   333,   269,   332,   616,   616,   306,
    1142,   674,   397,   407,   407,   407,   479,  1124,   629,   773,
     773,   759,  1141,   226,  -124,   546,  1155,  1046,  1047,   546,
     693,   512,   514,   616,   831,   616,  1152,  -669,   535,  1055,
    1258,   713,  1066,   541,   448,  -661,  -670,  -127,   232,   232,
       3,   329,   754,  -660,   771,   487,   241,   693,   536,   438,
     629,   242,   242,   330,   351,   352,   704,   464,   808,   466,
    -371,  -123,  -123,   476,  -661,   542,   224,   234,   717,   760,
    -131,  -124,  -660,   450,   264,   883,  1243,   275,   279,   540,
     541,  -129,   889,  -130,   580,   714,  1112,  -124,  -371,  -371,
     726,  -126,  -127,   242,  -131,   741,   503,   744,   968,   497,
    1198,  -128,  1258,   242,   969,  -130,   327,   327,  -131,   363,
     364,   474,   542,  -126,   445,   220,   123,   220,   220,   313,
     445,   481,   718,  -114,   232,   264,   463,   439,   440,   247,
    -128,   382,   382,   382,   382,   128,   528,   529,   523,   524,
     525,   526,   320,   321,  -115,  -371,   226,    73,   226,   460,
    1102,   123,   424,   123,   829,   310,  -129,   311,   288,   452,
    1155,   616,   670,   616,   670,  1155,  1264,   123,   330,   239,
     332,   616,   482,   616,   679,  -125,  1158,  1159,  1263,   331,
     490,  -759,   277,   329,  -127,   693,  -127,   242,   291,  1164,
    1167,   310,   965,   311,   820,   693,  -125,   310,   993,   311,
    -122,   641,   382,   264,   830,   269,  1191,   881,  -123,   634,
    -123,  1013,   958,  -121,    73,   496,   288,   643,   123,   527,
     963,  -117,  -118,   123,  -759,  -760,   888,   579,  -129,   477,
    -129,  -119,   579,   582,  -124,   903,  -124,   249,   712,  1264,
     712,  -131,   313,  -131,   636,   123,   291,   633,  1020,   635,
     329,   243,  -130,   246,  -130,   936,  1181,   232,   220,   232,
    -126,  -775,  -126,   849,  -130,   624,   463,   936,   301,  1027,
     242,   670,   726,   873,   936,   936,   551,  -128,  1026,  -128,
     -95,  -118,   850,   269,   123,   453,  -120,   456,  -760,   123,
    1074,   123,   928,   336,   331,   625,   936,   860,  -126,   238,
    -109,   465,   551,   505,   339,   773,   773,   431,  1238,   551,
     773,   773,  -669,   624,   505,   497,   375,  -128,   579,   579,
     320,   321,  -125,   261,  -125,   340,  -116,   220,   483,   579,
    1237,  -117,   485,  -119,   624,   463,   433,   579,   579,   435,
     436,   437,  -131,  1066,   376,   377,  1075,  -123,   445,  1076,
     445,   505,   877,  -786,   687,   738,   206,   506,  1118,   507,
     508,   877,   624,  1105,   625,   579,   208,  -114,   845,   344,
     507,   508,  1132,   739,   288,   322,  1066,  1027,  1275,   851,
     855,    60,   220,   467,    91,   242,   123,   881,   773,   624,
     463,  -118,   625,   468,   469,  -662,   497,  1067,   227,   227,
     546,   378,  1003,   505,   291,  1256,  1135,   507,   508,   301,
    1148,  -118,  1121,  -759,  -118,   430,  1017,  1019,  -118,   625,
    1049,  1022,   752,  1024,  -662,  1025,   726,   551,   726,   616,
    -546,   616,   729,   868,   773,    91,    91,  -125,   868,   289,
    1300,  -117,  1261,  -119,   729,   242,   748,  1262,  1009,   748,
     227,   617,   793,   626,   823,   288,  -116,   510,   794,   507,
     508,  -117,  1043,  -119,  -117,   671,  -119,   289,  -117,   832,
    -119,   123,   765,   881,   123,   227,   227,  -670,   775,   227,
     394,   405,   405,  1197,   227,   291,   670,  -126,   810,   767,
     912,  1207,   804,  -128,   806,   767,   679,   375,   488,   488,
     960,   738,   644,   488,   853,   494,   645,   505,   445,   699,
     536,   432,  -124,   680,   798,   856,   803,   431,   682,  1079,
     220,   684,   974,   969,   686,   376,   446,   624,   463,  1103,
     220,   789,  -115,   788,   123,  -125,   123,   624,   463,   696,
     797,   540,   795,  1252,   698,  1145,   841,   798,   206,   845,
    1259,   546,  -131,   241,   775,   775,   123,   625,   208,   445,
    1244,   511,  -663,   507,   508,   434,  -116,   625,   667,   669,
     836,   839,  -122,   835,   288,   795,  -665,   277,   441,  1160,
     644,   798,   447,  1121,  1048,   551,  -116,   497,   909,  -116,
    1121,  -663,  1121,  -116,    91,  -664,   969,  -775,   375,  1125,
    1151,   242,   505,   513,   291,  -665,   812,   730,   788,   795,
    1163,  1166,   310,   669,   311,   477,   277,   227,  1286,   227,
     227,   998,  1297,   227,  -664,   227,   376,   475,  -331,    91,
     867,    91,   869,  1177,  -666,   870,   871,   443,  1179,   346,
     770,   382,   442,   838,   840,    91,   288,   969,   992,   815,
    -130,  1192,  1194,  1195,  1196,  1133,  -331,  -331,   507,   508,
     838,   840,  1121,  -666,  1121,   451,   289,   726,   726,  1121,
    -121,  1121,  -759,   449,   579,   579,   291,   854,   991,   421,
     735,   579,   579,   447,   471,   990,  1121,   952,   952,   967,
     973,   505,   979,   346,   979,   235,    91,   227,   227,   227,
     227,    91,   227,   227,   766,   852,   370,   371,   372,   238,
     778,   793,   767,  -331,  -775,  -657,  -760,   242,   480,   206,
     794,   -94,  1233,    91,   289,  -657,   486,   123,   375,  -658,
     123,   492,   654,   495,   579,   579,   322,   579,   579,   500,
     445,  1072,  1073,  -657,  -657,   510,  -126,   507,   508,   616,
     663,   616,   515,  1062,  -657,  -128,   376,   501,  -658,  1146,
     227,   655,    91,   499,   499,  1065,  -117,    91,   227,    91,
    1125,   520,   748,   693,   729,  -119,   616,  1125,  1165,  1168,
     566,  1279,  1267,   227,   875,  1033,  -676,  1033,   220,   842,
     569,  -667,   382,   579,   844,   624,   463,   573,  -759,  1096,
    -657,   521,  1037,  -759,  1038,   583,   123,   638,   123,   669,
     642,   277,   848,   502,   852,   647,  -668,   952,   952,  -667,
    -667,   952,   664,   960,   785,   625,  1133,  1234,  1235,   227,
     665,   877,  1093,  1133,   683,  1133,   681,  1095,   952,   685,
    -322,   690,   857,   790,  -668,  -668,  1106,   691,   579,  1125,
     775,   775,  -109,   700,  1092,   775,   775,   878,  -125,   763,
     375,   727,   922,   922,    91,   505,  1081,  1083,  -322,  -322,
    -416,  1086,  1088,   733,   123,   736,  -667,   742,  -116,   123,
     382,   740,   289,   743,   227,   745,   756,  1149,   376,   425,
     900,   579,   996,  1134,   772,   123,   792,   800,   790,   805,
     812,  -668,   815,   874,   876,  1133,   893,  1133,   879,   799,
     894,  1006,  1133,  1008,  1133,   807,   898,   811,   669,   510,
    -302,   507,   508,   488,  1148,  -322,   902,   964,   790,  1133,
     901,   904,  1012,   775,   955,   123,   905,   959,   123,  1082,
    1084,   969,   986,  1007,  1087,  1089,   739,  -760,   227,    91,
    1016,   227,    91,  1018,   952,  1021,  1178,   952,   989,  1023,
     227,  1030,  -303,   289,   288,  1184,   781,  1061,   994,  1068,
     892,   952,   952,   952,  1189,  1069,   952,   952,  1215,   775,
    1229,   952,   952,  1070,  1082,  1084,  1115,  1087,  1089,  1090,
    -658,  1116,   922,   922,  1175,  1169,   922,  -305,   123,   885,
    1144,   288,  1147,   952,   910,  1066,  1186,   123,  1193,   729,
      80,  1187,    91,   922,    91,  1201,   123,   729,  -658,  -658,
    1199,  1226,   227,   505,    80,    80,  1206,  1036,  1208,  1210,
     505,   407,   227,   895,    91,   227,   375,   896,  -306,  1230,
     968,  1232,   781,   781,   790,  1213,   953,   953,  1214,  1245,
     382,   382,  1246,  1218,   790,  1057,  1219,  1273,  1274,   346,
    1248,    80,    80,  1253,   376,   639,   973,  1000,  1170,  1278,
     227,  1280,   979,  -760,   288,  -658,    80,   710,  -760,   507,
     508,   123,   289,   123,   715,   887,   507,   508,   891,  1282,
    1287,  1170,   123,  1289,   123,  1292,   984,  1304,   584,   988,
    -759,    80,    80,  -760,   407,    80,   473,   522,   220,   375,
      80,  1101,   695,   995,   997,   624,   463,   952,   803,   697,
     979,   640,   952,   952,   368,   369,   370,   371,   372,   922,
    -764,   392,   922,   277,  1294,   409,  1162,   376,   649,   374,
     834,   790,  1059,   971,   729,   625,   922,   922,   922,  1271,
     872,   922,   922,  1014,   289,  1200,   922,   922,  1122,   924,
     924,  1136,  1295,  -332,   577,   530,   999,   531,   532,   533,
     534,  1034,  1063,  -764,    37,    38,   123,    40,   922,   758,
    1153,  1137,   979,   375,   660,   661,  1301,  1150,   375,  1156,
    1139,  -332,  -332,  1143,   650,   666,   952,  1157,  1212,  1270,
     429,  -764,  -764,   677,   678,    91,   375,   227,    91,  1217,
    1231,   376,   907,  1161,   716,   720,   376,  1227,   422,   884,
      80,   721,  1064,  1220,  1221,  1222,  1255,  1251,  1183,   882,
    1216,   694,     0,  1110,   376,  1254,     0,   530,     0,   531,
     532,   533,   534,    80,     0,    80,    80,  1127,  -332,    80,
       0,    80,     0,     0,  1110,    80,  -764,    80,  -764,  1097,
       0,  -759,  1098,     0,     0,  1100,     0,   790,   908,     0,
       0,    80,  1104,  1228,   227,  1107,   790,     0,     0,   790,
       0,     0,  1209,  1211,    91,     0,    91,   720,     0,     0,
     227,   650,   922,  1051,     0,     0,     0,   922,   922,   790,
      41,    42,    43,    44,  1223,  1224,  1225,     0,  1236,     0,
    1071,   530,     0,   531,   532,   533,   534,     0,     0,     0,
       0,   953,    80,    80,    80,    80,    80,    80,    80,    80,
       0,     0,  1240,   953,   531,   532,   533,   534,   781,   781,
     953,   953,     0,   781,   781,   862,   863,  1110,   864,    80,
       0,     0,    91,     0,     0,    46,    47,    91,   227,     0,
       0,  1117,   953,  1119,     0,     0,     0,     0,  1120,     0,
    1059,   922,     0,    91,     0,  1126,     0,     0,   530,     0,
     531,   532,   533,   534,   535,     0,    80,  1185,    80,     0,
     790,   790,   790,    80,    80,    80,   315,   316,   317,   318,
     319,     0,  1281,  1283,   536,  1293,     0,     0,  1288,    80,
    1290,  1291,   346,    91,     0,     0,    91,     0,  1127,     0,
       0,   781,     0,  1127,     0,  1127,  1138,  1127,   538,   359,
     360,     0,   944,   944,   539,   540,   541,   102,     0,     0,
       0,     0,     0,  1051,     0,     0,   227,  1303,  1305,  1306,
    1307,   102,   102,  1051,  1051,    80,     0,     0,     0,  1309,
       0,     0,     0,     0,     0,  1276,     0,   781,   542,   885,
       0,   543,  1173,     0,     0,   924,    91,   368,   369,   370,
     371,   372,     0,     0,     0,    91,     0,     0,   102,   102,
      80,   790,     0,     0,    91,     0,     0,  1127,     0,  1127,
       0,     0,     0,   102,  1127,     0,  1127,     0,     0,  1205,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1127,     0,     0,     0,     0,  1277,     0,   102,   102,
       0,     0,   102,     0,     0,     0,     0,   102,  1247,     0,
     956,   957,     0,     0,     0,     0,     0,   961,   962,     0,
       0,     0,     0,  1257,     0,  1260,     0,   945,   945,    91,
       0,    91,   944,   944,     0,     0,   944,     0,     0,     0,
      91,     0,    91,     0,    80,    80,     0,    80,    80,     0,
       0,     0,  1249,   944,     0,     0,    80,     0,     0,     0,
       0,     0,    80,   530,     0,   531,   532,   533,   534,   535,
    1001,  1002,     0,  1004,  1005,     0,     0,     0,     0,     0,
     227,     0,     0,   946,   946,     0,   227,   227,     0,   536,
       0,     0,     0,     0,     0,  1296,     0,  1298,     0,     0,
       0,     0,     0,   537,  1299,     0,     0,     0,    80,     0,
      80,     0,     0,   538,     0,     0,     0,   102,    80,  1308,
     540,   541,     0,     0,    91,     0,     0,     0,    80,  1039,
      80,    80,     0,     0,     0,     0,     0,     0,    80,    80,
     102,     0,   102,   102,     0,     0,   102,     0,   102,     0,
       0,     0,   102,   542,   102,     0,   126,   945,   945,     0,
       0,   945,     0,     0,   346,     0,    80,     0,   102,   944,
       0,     0,   944,     0,     0,     0,     0,     0,   945,     0,
       0,   359,   360,     0,  1085,     0,   944,   944,   944,     0,
       0,   944,   944,     0,     0,     0,   944,   944,   346,     0,
       0,     0,     0,     0,     0,     0,     0,   126,   126,     0,
       0,   292,     0,   946,   946,   359,   360,   946,   944,   102,
     102,   102,   102,   102,   102,   102,   102,  1108,   367,   368,
     369,   370,   371,   372,   946,     0,     0,     0,     0,   292,
       0,     0,     0,     0,     0,     0,   102,     0,     0,     0,
       0,     0,   398,   408,   408,     0,     0,     0,     0,     0,
     365,   366,   367,   368,   369,   370,   371,   372,     0,   530,
       0,   531,   532,   533,   534,   535,     0,     0,     0,     0,
       0,     0,     0,   102,     0,   102,     0,     0,     0,     0,
     102,   102,   102,     0,   945,   536,     0,   945,     0,     0,
       0,    80,     0,    80,    80,     0,   102,     0,     0,     0,
       0,   945,   945,   945,     0,     0,   945,   945,     0,   538,
       0,   945,   945,     0,     0,   539,   540,   541,     0,     0,
     947,   947,   944,     0,     0,     0,     0,   944,   944,     0,
       0,     0,     0,   945,     0,     0,     0,     0,     0,     0,
     946,     0,   102,   946,     0,     0,     0,     0,     0,   542,
       0,     0,   543,     0,     0,     0,   126,   946,   946,   946,
      80,     0,   946,   946,     0,     0,     0,   946,   946,   242,
      80,     0,    80,     0,     0,     0,    80,   102,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   946,
       0,   126,     0,   126,     0,     0,     0,   102,     0,     0,
       0,   944,     0,     0,     0,     0,     0,   126,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,    80,     0,     0,   292,    80,
      80,   530,     0,   531,   532,   533,   534,   535,    80,     0,
      76,     0,     0,    80,    80,     0,     0,   945,     0,     0,
     947,   947,   945,   945,   947,     0,     0,   536,   126,    80,
       0,   102,   102,   126,   102,   102,     0,     0,     0,     0,
       0,   947,     0,   102,     0,     0,     0,     0,     0,   102,
       0,   538,     0,     0,     0,   126,   292,     0,   540,   541,
       0,    76,    76,     0,     0,   285,     0,     0,     0,    80,
     948,   948,    80,   946,     0,     0,     0,    80,   946,   946,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   542,     0,   285,   126,   102,   945,   102,     0,   126,
       0,   126,    80,     0,     0,   102,   285,   285,   285,     0,
       0,     0,   949,   949,     0,   102,     0,   102,   102,     0,
       0,     0,     0,    80,     0,   102,   102,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,   946,   102,     0,     0,     0,   947,     0,     0,
     947,     0,     0,   950,   950,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   947,   947,   947,     0,     0,   947,
     947,     0,     0,     0,   947,   947,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   126,     0,     0,     0,
     948,   948,     0,     0,   948,     0,   947,     0,     0,     0,
       0,     0,     0,     0,   292,    80,     0,    80,     0,     0,
      76,   948,     0,     0,     0,     0,    80,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   949,   949,     0,     0,   949,     0,     0,     0,
       0,     0,     0,     0,     0,    76,     0,    76,     0,     0,
       0,     0,     0,   949,     0,     0,    80,     0,     0,     0,
       0,    76,    80,    80,     0,     0,     0,     0,     0,     0,
       0,   126,     0,     0,   126,     0,     0,     0,   102,     0,
     102,   102,   285,   950,   950,   292,     0,   950,   782,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,   950,     0,     0,     0,     0,     0,
     947,     0,    76,     0,     0,   947,   947,    76,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   948,   951,   951,
     948,     0,     0,     0,   126,     0,   126,     0,     0,    76,
     285,     0,     0,     0,   948,   948,   948,   102,     0,   948,
     948,     0,     0,     0,   948,   948,   126,   102,     0,   102,
       0,     0,     0,   102,   782,   782,     0,     0,     0,   949,
       0,     0,   949,     0,     0,     0,   948,     0,    76,     0,
       0,     0,     0,    76,     0,    76,   949,   949,   949,   947,
       0,   949,   949,     0,     0,     0,   949,   949,     0,     0,
       0,     0,     0,     0,   292,     0,     0,     0,     0,     0,
       0,   102,   102,   866,     0,     0,   102,   102,   949,     0,
     950,     0,     0,   950,     0,   102,     0,     0,     0,     0,
     102,   102,     0,     0,     0,     0,     0,   950,   950,   950,
       0,     0,   950,   950,     0,     0,   102,   950,   950,     0,
       0,   530,     0,   531,   532,   533,   534,   535,   951,   951,
       0,     0,   951,    77,     0,     0,     0,     0,     0,   950,
       0,     0,     0,     0,     0,     0,   292,   536,     0,   951,
      76,     0,     0,     0,     0,     0,   102,     0,     0,   102,
     948,   537,     0,     0,   102,   948,   948,     0,   285,     0,
       0,   538,     0,     0,     0,     0,     0,   539,   540,   541,
       0,     0,     0,     0,    77,    77,     0,     0,   286,   102,
       0,     0,     0,     0,     0,     0,     0,   126,     0,     0,
     126,     0,   949,     0,     0,     0,     0,   949,   949,     0,
     102,   542,     0,     0,   543,     0,   286,     0,     0,   102,
       0,     0,     0,     0,     0,     0,   858,     0,   102,   286,
     286,   286,     0,     0,     0,    76,     0,   102,    76,   948,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   285,
       0,     0,    76,   950,     0,     0,     0,     0,   950,   950,
       0,     0,     0,     0,     0,   951,     0,     0,   951,     0,
       0,     0,     0,     0,     0,     0,   126,     0,   126,     0,
       0,   949,   951,   951,   951,     0,     0,   951,   951,     0,
       0,     0,   951,   951,     0,     0,     0,     0,    76,     0,
      76,     0,   102,     0,   102,     0,     0,     0,     0,     0,
       0,     0,     0,   102,   951,   102,     0,     0,     0,     0,
      76,     0,     0,     0,     0,     0,     0,     0,    76,    76,
     782,   782,   950,     0,     0,   782,   782,     0,     0,     0,
       0,     0,     0,    77,   126,     0,     0,     0,     0,   126,
       0,     0,     0,   102,     0,   122,     0,     0,     0,   102,
     102,     0,     0,     0,     0,   126,     0,     0,   285,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    77,     0,
      77,     0,     0,     0,     0,   530,     0,   531,   532,   533,
     534,   535,     0,     0,    77,     0,     0,   102,     0,     0,
       0,     0,     0,     0,     0,   126,   122,   122,   126,     0,
     290,   536,     0,   782,     0,   286,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   537,     0,     0,   951,     0,
       0,     0,     0,   951,   951,   538,     0,     0,   290,     0,
     285,   539,   540,   541,     0,    77,     0,     0,     0,     0,
      77,   396,   406,   406,   406,     0,     0,     0,     0,   782,
       0,     0,     0,     0,  1176,     0,     0,     0,   126,     0,
       0,     0,    77,   286,     0,   542,     0,   126,   543,     0,
       0,    87,     0,     0,     0,     0,   126,     0,     0,     0,
       0,    76,     0,     0,    76,     0,     0,     0,     0,     0,
    -786,   408,     0,     0,     0,     0,     0,   951,  -786,  -786,
    -786,    77,     0,  -786,  -786,  -786,    77,  -786,    77,     0,
       0,     0,     0,     0,     0,  -786,  -786,  -786,     0,     0,
       0,     0,    87,    87,     0,     0,     0,  -786,  -786,     0,
    -786,  -786,  -786,  -786,  -786,     0,     0,     0,     0,     0,
       0,   126,     0,   126,     0,     0,     0,     0,     0,     0,
       0,     0,   126,     0,   126,   122,     0,     0,  -786,  -786,
      76,     0,    76,     0,   408,     0,     0,   393,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -786,  -786,     0,
     122,     0,   122,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    77,     0,     0,   122,     0,     0,     0,
       0,  -786,     0,     0,    76,    76,     0,     0,     0,    76,
      76,   286,     0,     0,     0,     0,     0,   290,    76,     0,
       0,     0,     0,    76,     0,     0,   126,     0,     0,     0,
       0,     0,  -786,  -786,     0,     0,     0,   238,  -786,    76,
    -786,     0,  -786,     0,     0,     0,     0,   122,     0,     0,
       0,     0,   122,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   125,     0,     0,     0,     0,     0,
       0,    87,     0,     0,   122,   290,     0,     0,    77,    76,
       0,    77,    76,     0,     0,     0,     0,    76,     0,     0,
       0,     0,   286,     0,     0,    77,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    87,     0,    87,     0,
       0,     0,     0,   122,     0,   125,   125,     0,   122,     0,
     122,     0,    87,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,     0,     0,     0,     0,   285,     0,
       0,    77,    76,    77,     0,     0,     0,     0,     0,     0,
       0,    76,     0,     0,     0,     0,     0,     0,     0,     0,
      76,     0,     0,    77,     0,     0,     0,     0,     0,     0,
       0,    77,    77,    87,     0,   285,     0,     0,    87,    23,
      24,    25,    26,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    32,    33,    34,     0,     0,
      87,     0,     0,     0,     0,    41,    42,    43,    44,    45,
       0,   286,     0,     0,     0,   122,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,     0,    76,     0,     0,
       0,     0,     0,   290,     0,     0,    76,     0,    76,    87,
       0,     0,     0,     0,    87,     0,    87,     0,   285,     0,
       0,     0,     0,     0,     0,     0,     0,    58,    59,    60,
      61,    62,    63,    64,    65,    66,     0,     0,     0,     0,
       0,     0,     0,     0,   125,     0,     0,     0,     0,     0,
       0,     0,     0,   286,     0,     0,     0,     0,     0,    23,
      24,    25,    26,     0,   283,     0,     0,     0,     0,     0,
     122,     0,     0,   122,     0,    32,    33,    34,   910,   125,
       0,   125,   911,     0,   290,    41,    42,    43,    44,    45,
      76,     0,     0,     0,     0,   125,   346,   347,   348,   349,
     350,   351,   352,   353,    77,   355,   356,    77,     0,     0,
       0,    87,     0,   359,   360,     0,     0,   913,   914,     0,
       0,     0,     0,     0,     0,   915,     0,     0,   916,     0,
       0,   917,   918,   122,   919,   122,     0,    58,    59,    60,
      61,    62,    63,    64,    65,    66,   125,     0,     0,     0,
       0,   125,     0,     0,     0,   122,   363,   364,   365,   366,
     367,   368,   369,   370,   371,   372,     0,   921,     0,     0,
       0,     0,     0,   125,   283,     0,     0,     0,     0,     0,
       0,     0,     0,    77,     0,    77,     0,     0,   242,     0,
       0,     0,     0,     0,     0,     0,    87,     0,     0,    87,
       0,     0,     0,   290,     0,     0,     0,     0,     0,     0,
       0,     0,   125,   776,     0,     0,     0,   125,     0,   125,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    77,    77,     0,
     345,     0,    77,    77,     0,     0,     0,     0,     0,     0,
       0,    77,     0,     0,     0,     0,    77,     0,     0,    87,
       0,    87,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    77,     0,     0,   290,     0,     0,     0,     0,
       0,    87,     0,     0,     0,     0,     0,     0,     0,   776,
     776,     0,     0,   346,   347,   348,   349,   350,   351,   352,
     353,   354,   355,   356,   357,   358,     0,     0,     0,     0,
     359,   360,    77,     0,   125,    77,   361,     0,     0,     0,
      77,     0,     0,     0,     0,     0,   122,     0,     0,   122,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   362,     0,   363,   364,   365,   366,   367,   368,   369,
     370,   371,   372,     0,     0,     0,    77,     0,     0,     0,
       0,   286,     0,     0,     0,    77,     0,   346,  -787,  -787,
    -787,  -787,   351,   352,    77,     0,  -787,  -787,     0,     0,
       0,     0,     0,    77,   359,   360,     0,     0,     0,   125,
       0,     0,   125,     0,     0,   122,     0,   122,   286,     0,
       0,     0,     0,     0,     0,     0,   125,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   363,   364,   365,
     366,   367,   368,   369,   370,   371,   372,     0,     0,     0,
       0,     0,    87,     0,     0,    87,     0,     0,    77,     0,
      77,     0,   125,     0,   125,     0,     0,     0,     0,    77,
       0,    77,     0,   122,     0,     0,     0,     0,   122,     0,
       0,   286,     0,     0,   125,     0,     0,     0,     0,     0,
       0,     0,   125,   125,   122,     0,     0,    23,    24,    25,
      26,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    32,    33,    34,   910,     0,     0,     0,
     911,     0,   912,    41,    42,    43,    44,    45,     0,     0,
       0,    87,     0,    87,   122,     0,     0,   122,     0,     0,
       0,     0,   536,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    77,     0,   913,   914,     0,     0,     0,
       0,     0,     0,   915,     0,     0,   916,     0,     0,   917,
     918,     0,   919,   540,     0,    58,    59,   920,    61,    62,
      63,    64,    65,    66,     0,   776,   776,     0,     0,     0,
     776,   776,     0,  1174,     0,     0,     0,   122,     0,    87,
       0,     0,     0,     0,    87,   921,   122,     0,     0,     0,
       0,     0,   283,     0,     0,   122,     0,     0,     0,     0,
      87,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     406,     0,   222,   222,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   346,   347,   348,   349,   350,   351,
     352,     0,     0,   355,   356,   125,     0,     0,   125,     0,
      87,   359,   360,    87,   255,   258,   259,   260,   776,     0,
       0,   222,   222,     0,     0,     0,     0,     0,     0,     0,
     122,     0,   122,     0,   308,   309,     0,     0,     0,     0,
       0,   122,     0,   122,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   406,   363,   364,   365,   366,   367,   368,
     369,   370,   371,   372,   776,     0,     0,     0,   222,  1172,
       0,     0,     0,    87,     0,     0,     0,     0,     0,     0,
       0,     0,    87,     0,   125,     0,   125,     0,     0,     0,
       0,    87,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   122,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   125,   125,
       0,     0,     0,   125,   125,     0,     0,     0,     0,     0,
       0,     0,   125,     0,     0,     0,    87,   125,    87,     0,
       0,     0,     0,     0,     0,     0,     0,    87,     0,    87,
       0,     0,     0,   125,     0,     0,   222,     0,     0,   222,
     222,   222,     0,   308,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   222,     0,   222,   222,     0,     0,     0,     0,     0,
       0,     0,     0,   125,     0,     0,   125,     0,     0,     0,
       0,   125,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   346,   347,   348,   349,   350,
     351,   352,   353,   354,   355,   356,   357,   358,     0,     0,
       0,    87,   359,   360,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   125,     0,     0,
       0,     0,     0,     0,     0,     0,   125,     0,     0,     0,
       0,     0,     0,     0,     0,   125,     0,     0,     0,     0,
       0,     0,     0,   362,   125,   363,   364,   365,   366,   367,
     368,   369,   370,   371,   372,     0,     0,     0,     0,     0,
       0,     0,   586,   587,   588,   589,   590,     0,     0,   591,
     592,   593,   594,   595,   596,   597,   598,     0,   600,     0,
       0,   601,   602,   603,   604,   605,   606,   607,   608,   609,
     610,     0,     0,     0,   222,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   125,
       0,   125,    23,    24,    25,    26,     0,     0,     0,     0,
     125,     0,   125,     0,     0,     0,     0,     0,    32,    33,
      34,   910,     0,     0,     0,   911,     0,     0,    41,    42,
      43,    44,    45,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   222,   222,
       0,     0,     0,   222,     0,     0,     0,   222,     0,     0,
     913,   914,     0,   260,     0,     0,     0,     0,   915,     0,
       0,   916,     0,     0,   917,   918,     0,   919,   689,     0,
      58,    59,    60,    61,    62,    63,    64,    65,    66,     0,
       0,     0,     0,   222,   125,     0,   222,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   222,     0,
     921,     0,     0,     0,     0,     0,     0,   283,     0,     0,
       0,     0,     0,     0,     0,     0,   719,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -786,     4,     0,     5,
       6,     7,     8,     9,     0,     0,     0,    10,    11,     0,
     222,     0,    12,     0,    13,    14,    15,    16,    17,    18,
      19,     0,   749,     0,     0,   749,    20,    21,    22,    23,
      24,    25,    26,     0,   222,    27,     0,     0,     0,     0,
     777,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,    41,    42,    43,    44,    45,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    48,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    50,    51,     0,
       0,     0,     0,     0,     0,    52,   222,     0,    53,    54,
       0,    55,    56,     0,    57,     0,   222,    58,    59,    60,
      61,    62,    63,    64,    65,    66,   837,   837,     0,   222,
     749,   749,   837,     0,   222,     0,     0,     0,     0,     0,
       0,     0,     0,   837,   837,     0,     0,     0,     0,   222,
       0,   222,     0,     0,    67,    68,    69,     0,     0,     0,
     837,     0,     0,     0,  -764,     0,  -786,     0,  -786,     0,
       0,     0,  -764,  -764,  -764,     0,     0,  -764,  -764,  -764,
       0,  -764,     0,     0,     0,     0,     0,     0,     0,  -764,
    -764,  -764,  -764,  -764,     0,     0,     0,   222,     0,     0,
       0,  -764,  -764,     0,  -764,  -764,  -764,  -764,  -764,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     222,     0,  -764,  -764,     0,     0,     0,     0,     0,     0,
       0,     0,  -764,  -764,  -764,  -764,  -764,  -764,  -764,  -764,
    -764,  -764,  -764,  -764,  -764,     0,     0,     0,   222,  -764,
    -764,  -764,  -764,     0,   846,  -764,     0,   222,     0,     0,
       0,  -764,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   222,     0,     0,  -764,     0,     0,  -764,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -127,
    -764,  -764,  -764,  -764,  -764,  -764,  -764,  -764,  -764,  -764,
    -764,  -764,     0,     0,     0,     0,  -764,  -764,  -764,  -764,
       0,     0,  -764,  -764,  -764,     0,  -764,    23,    24,    25,
      26,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    32,    33,    34,   910,     0,     0,     0,
     911,     0,     0,    41,    42,    43,    44,    45,   749,  1028,
    1029,     0,     0,     0,     0,     0,     0,   222,     0,     0,
       0,     0,  1040,     0,   222,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   913,   914,     0,     0,     0,
       0,     0,     0,   915,     0,     0,   916,     0,     0,   917,
     918,     0,     0,     0,     0,    58,    59,    60,    61,    62,
      63,    64,    65,    66,     0,     0,     0,     0,     0,     0,
       0,  1080,   837,   837,     0,     0,     0,   837,   837,     0,
       0,     0,     0,     0,     0,   921,     0,     0,     0,     0,
       0,     0,   283,     0,     0,     0,     0,     0,     0,     0,
       0,   222,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   749,   837,   837,     0,
     837,   837,     0,   222,     0,  -657,     0,     0,     0,     0,
       0,     0,     0,  -657,  -657,  -657,     0,     0,  -657,  -657,
    -657,     0,  -657,     0,     0,     0,     0,     0,     0,     0,
    -657,   222,  -657,  -657,  -657,   837,     0,     0,     0,     0,
       0,     0,  -657,  -657,     0,  -657,  -657,  -657,  -657,  -657,
       0,     0,     0,   346,   347,   348,   349,   350,   351,   352,
     353,   354,   355,   356,   357,   358,     0,     0,     0,     0,
     359,   360,     0,  -657,  -657,     0,     0,     0,     0,     0,
       0,   837,     0,  -657,  -657,  -657,  -657,  -657,  -657,  -657,
    -657,  -657,  -657,  -657,  -657,  -657,     0,     0,   222,     0,
    -657,  -657,  -657,  -657,   837,  -657,  -657,     0,     0,     0,
       0,   362,  -657,   363,   364,   365,   366,   367,   368,   369,
     370,   371,   372,     0,     0,     0,  -657,     0,     0,  -657,
       0,     0,     0,     0,     0,     0,     0,   242,     0,     0,
    -657,  -657,  -657,  -657,  -657,  -657,  -657,  -657,  -657,  -657,
    -657,  -657,  -657,     0,     0,     0,     0,     0,  -657,  -657,
    -657,     0,  -658,  -657,  -657,  -657,     0,  -657,     0,     0,
    -658,  -658,  -658,     0,     0,  -658,  -658,  -658,   222,  -658,
       0,     0,     0,     0,     0,     0,     0,  -658,     0,  -658,
    -658,  -658,     0,     0,     0,     0,     0,     0,     0,  -658,
    -658,     0,  -658,  -658,  -658,  -658,  -658,     0,     0,     0,
     346,   347,   348,   349,   350,   351,   352,   353,   354,   355,
     356,  -787,  -787,     0,   222,     0,     0,   359,   360,     0,
    -658,  -658,     0,     0,     0,     0,     0,     0,     0,     0,
    -658,  -658,  -658,  -658,  -658,  -658,  -658,  -658,  -658,  -658,
    -658,  -658,  -658,     0,     0,     0,     0,  -658,  -658,  -658,
    -658,     0,  -658,  -658,     0,     0,     0,     0,     0,  -658,
     363,   364,   365,   366,   367,   368,   369,   370,   371,   372,
       0,     0,     0,  -658,     0,     0,  -658,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -658,  -658,  -658,
    -658,  -658,  -658,  -658,  -658,  -658,  -658,  -658,  -658,  -658,
       0,     0,     0,     0,     0,  -658,  -658,  -658,  -765,     0,
    -658,  -658,  -658,     0,  -658,     0,  -765,  -765,  -765,     0,
       0,  -765,  -765,  -765,     0,  -765,     0,     0,     0,     0,
       0,     0,     0,  -765,  -765,  -765,  -765,  -765,     0,     0,
       0,     0,     0,     0,     0,  -765,  -765,     0,  -765,  -765,
    -765,  -765,  -765,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -765,  -765,     0,     0,
       0,     0,     0,     0,     0,     0,  -765,  -765,  -765,  -765,
    -765,  -765,  -765,  -765,  -765,  -765,  -765,  -765,  -765,     0,
       0,     0,     0,  -765,  -765,  -765,  -765,     0,     0,  -765,
       0,     0,     0,     0,     0,  -765,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -765,
       0,     0,  -765,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -765,  -765,  -765,  -765,  -765,  -765,
    -765,  -765,  -765,  -765,  -765,  -765,     0,     0,     0,     0,
    -765,  -765,  -765,  -765,  -766,     0,  -765,  -765,  -765,     0,
    -765,     0,  -766,  -766,  -766,     0,     0,  -766,  -766,  -766,
       0,  -766,     0,     0,     0,     0,     0,     0,     0,  -766,
    -766,  -766,  -766,  -766,     0,     0,     0,     0,     0,     0,
       0,  -766,  -766,     0,  -766,  -766,  -766,  -766,  -766,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -766,  -766,     0,     0,     0,     0,     0,     0,
       0,     0,  -766,  -766,  -766,  -766,  -766,  -766,  -766,  -766,
    -766,  -766,  -766,  -766,  -766,     0,     0,     0,     0,  -766,
    -766,  -766,  -766,     0,     0,  -766,     0,     0,     0,     0,
       0,  -766,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -766,     0,     0,  -766,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -766,  -766,  -766,  -766,  -766,  -766,  -766,  -766,  -766,  -766,
    -766,  -766,     0,     0,     0,     0,  -766,  -766,  -766,  -766,
    -331,     0,  -766,  -766,  -766,     0,  -766,     0,  -331,  -331,
    -331,     0,     0,  -331,  -331,  -331,     0,  -331,     0,     0,
       0,     0,     0,     0,     0,  -331,     0,  -331,  -331,  -331,
       0,     0,     0,     0,     0,     0,     0,  -331,  -331,     0,
    -331,  -331,  -331,  -331,  -331,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -331,  -331,
       0,     0,     0,     0,     0,     0,     0,     0,  -331,  -331,
    -331,  -331,  -331,  -331,  -331,  -331,  -331,  -331,  -331,  -331,
    -331,     0,     0,     0,     0,  -331,  -331,  -331,  -331,     0,
     847,  -331,     0,     0,     0,     0,     0,  -331,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -331,     0,     0,  -331,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -129,  -331,  -331,  -331,  -331,
    -331,  -331,  -331,  -331,  -331,  -331,  -331,  -331,     0,     0,
       0,     0,     0,  -331,  -331,  -331,  -472,     0,  -331,  -331,
    -331,     0,  -331,     0,  -472,  -472,  -472,     0,     0,  -472,
    -472,  -472,     0,  -472,     0,     0,     0,     0,     0,     0,
       0,  -472,  -472,  -472,  -472,     0,     0,     0,     0,     0,
       0,     0,     0,  -472,  -472,     0,  -472,  -472,  -472,  -472,
    -472,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -472,  -472,     0,     0,     0,     0,
       0,     0,     0,     0,  -472,  -472,  -472,  -472,  -472,  -472,
    -472,  -472,  -472,  -472,  -472,  -472,  -472,     0,     0,     0,
       0,  -472,  -472,  -472,  -472,     0,     0,  -472,     0,     0,
       0,     0,     0,  -472,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -472,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -472,     0,  -472,  -472,  -472,  -472,  -472,  -472,
    -472,  -472,  -472,  -472,     0,     0,     0,     0,  -472,  -472,
    -472,  -472,  -323,   238,  -472,  -472,  -472,     0,  -472,     0,
    -323,  -323,  -323,     0,     0,  -323,  -323,  -323,     0,  -323,
       0,     0,     0,     0,     0,     0,     0,  -323,     0,  -323,
    -323,  -323,     0,     0,     0,     0,     0,     0,     0,  -323,
    -323,     0,  -323,  -323,  -323,  -323,  -323,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -323,  -323,     0,     0,     0,     0,     0,     0,     0,     0,
    -323,  -323,  -323,  -323,  -323,  -323,  -323,  -323,  -323,  -323,
    -323,  -323,  -323,     0,     0,     0,     0,  -323,  -323,  -323,
    -323,     0,     0,  -323,     0,     0,     0,     0,     0,  -323,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -323,     0,     0,  -323,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -323,  -323,
    -323,  -323,  -323,  -323,  -323,  -323,  -323,  -323,  -323,  -323,
       0,     0,     0,     0,     0,  -323,  -323,  -323,  -786,     0,
    -323,  -323,  -323,     0,  -323,     0,  -786,  -786,  -786,     0,
       0,  -786,  -786,  -786,     0,  -786,     0,     0,     0,     0,
       0,     0,     0,  -786,  -786,  -786,  -786,     0,     0,     0,
       0,     0,     0,     0,     0,  -786,  -786,     0,  -786,  -786,
    -786,  -786,  -786,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -786,  -786,     0,     0,
       0,     0,     0,     0,     0,     0,  -786,  -786,  -786,  -786,
    -786,  -786,  -786,  -786,  -786,  -786,  -786,  -786,  -786,     0,
       0,     0,     0,  -786,  -786,  -786,  -786,     0,     0,  -786,
       0,     0,     0,     0,     0,  -786,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -786,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -786,     0,  -786,  -786,  -786,  -786,
    -786,  -786,  -786,  -786,  -786,  -786,     0,     0,     0,     0,
    -786,  -786,  -786,  -786,  -338,   238,  -786,  -786,  -786,     0,
    -786,     0,  -338,  -338,  -338,     0,     0,  -338,  -338,  -338,
       0,  -338,     0,     0,     0,     0,     0,     0,     0,  -338,
       0,  -338,  -338,     0,     0,     0,     0,     0,     0,     0,
       0,  -338,  -338,     0,  -338,  -338,  -338,  -338,  -338,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -338,  -338,     0,     0,     0,     0,     0,     0,
       0,     0,  -338,  -338,  -338,  -338,  -338,  -338,  -338,  -338,
    -338,  -338,  -338,  -338,  -338,     0,     0,     0,     0,  -338,
    -338,  -338,  -338,     0,     0,  -338,     0,     0,     0,     0,
       0,  -338,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -338,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -338,     0,  -338,  -338,  -338,  -338,  -338,  -338,  -338,  -338,
    -338,  -338,     0,     0,     0,     0,     0,  -338,  -338,  -338,
    -764,   235,  -338,  -338,  -338,     0,  -338,     0,  -764,  -764,
    -764,   906,     0,     0,  -764,  -764,     0,  -764,     0,     0,
       0,     0,     0,     0,     0,  -764,  -764,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -764,  -764,     0,
    -764,  -764,  -764,  -764,  -764,   346,   347,   348,   349,   350,
     351,   352,   353,   354,   355,   356,   357,   358,     0,     0,
       0,     0,   359,   360,     0,     0,     0,     0,  -764,  -764,
       0,     0,     0,     0,     0,     0,     0,     0,  -764,  -764,
    -764,  -764,  -764,  -764,  -764,  -764,  -764,  -764,  -764,  -764,
    -764,     0,     0,     0,     0,  -764,  -764,  -764,  -764,     0,
     786,  -764,     0,   362,     0,   363,   364,   365,   366,   367,
     368,   369,   370,   371,   372,     0,     0,     0,     0,     0,
       0,  -764,  -274,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -127,  -764,     0,  -764,  -764,
    -764,  -764,  -764,  -764,  -764,  -764,  -764,  -764,     0,     0,
       0,     0,  -764,  -764,  -764,  -118,  -764,     0,  -764,     0,
    -764,     0,  -764,     0,  -764,  -764,  -764,   906,     0,     0,
    -764,  -764,     0,  -764,     0,     0,     0,     0,     0,     0,
       0,  -764,  -764,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -764,  -764,     0,  -764,  -764,  -764,  -764,
    -764,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,     0,     0,     0,     0,   359,   360,
       0,     0,     0,     0,  -764,  -764,     0,     0,     0,     0,
       0,     0,     0,     0,  -764,  -764,  -764,  -764,  -764,  -764,
    -764,  -764,  -764,  -764,  -764,  -764,  -764,     0,     0,     0,
       0,  -764,  -764,  -764,  -764,     0,   786,  -764,     0,   362,
       0,   363,   364,   365,   366,   367,   368,   369,   370,   371,
     372,     0,     0,     0,     0,     0,     0,  -764,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -127,  -764,     0,  -764,  -764,  -764,  -764,  -764,  -764,
    -764,  -764,  -764,  -764,     0,     0,     0,     0,  -764,  -764,
    -764,  -764,  -331,     0,  -764,     0,  -764,     0,  -764,     0,
    -331,  -331,  -331,     0,     0,     0,  -331,  -331,     0,  -331,
       0,     0,     0,     0,     0,     0,     0,  -331,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -331,
    -331,     0,  -331,  -331,  -331,  -331,  -331,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -331,  -331,     0,     0,     0,     0,     0,     0,     0,     0,
    -331,  -331,  -331,  -331,  -331,  -331,  -331,  -331,  -331,  -331,
    -331,  -331,  -331,     0,     0,     0,     0,  -331,  -331,  -331,
    -331,     0,   787,  -331,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -331,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -129,  -331,     0,
    -331,  -331,  -331,  -331,  -331,  -331,  -331,  -331,  -331,  -331,
       0,     0,     0,     0,     0,  -331,  -331,  -120,  -331,     0,
    -331,     0,  -331,     0,  -331,     0,  -331,  -331,  -331,     0,
       0,     0,  -331,  -331,     0,  -331,     0,     0,     0,     0,
       0,     0,     0,  -331,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -331,  -331,     0,  -331,  -331,
    -331,  -331,  -331,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -331,  -331,     0,     0,
       0,     0,     0,     0,     0,     0,  -331,  -331,  -331,  -331,
    -331,  -331,  -331,  -331,  -331,  -331,  -331,  -331,  -331,     0,
       0,     0,     0,  -331,  -331,  -331,  -331,     0,   787,  -331,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -331,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -129,  -331,     0,  -331,  -331,  -331,  -331,
    -331,  -331,  -331,  -331,  -331,  -331,     0,     0,     0,     0,
       0,  -331,  -331,  -331,     0,     0,  -331,     0,  -331,     4,
    -331,     5,     6,     7,     8,     9,  -786,  -786,  -786,    10,
      11,     0,     0,  -786,    12,     0,    13,    14,    15,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,    28,    29,   262,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,     0,    41,    42,    43,
      44,    45,    46,    47,     0,     0,  -786,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
      51,     0,     0,     0,     0,     0,     0,    52,     0,     0,
      53,    54,     0,    55,    56,     0,    57,     0,     0,    58,
      59,    60,    61,    62,    63,    64,    65,    66,     0,     0,
       0,     4,     0,     5,     6,     7,     8,     9,     0,     0,
    -786,    10,    11,     0,  -786,  -786,    12,     0,    13,    14,
      15,    16,    17,    18,    19,     0,    67,    68,    69,     0,
      20,    21,    22,    23,    24,    25,    26,     0,  -786,    27,
    -786,     0,     0,     0,     0,    28,    29,   262,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,    41,
      42,    43,    44,    45,    46,    47,     0,     0,  -786,     0,
       0,     0,     0,     0,     0,     0,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    50,    51,     0,     0,     0,     0,     0,     0,    52,
       0,     0,    53,    54,     0,    55,    56,     0,    57,     0,
       0,    58,    59,    60,    61,    62,    63,    64,    65,    66,
       0,     0,     0,     4,     0,     5,     6,     7,     8,     9,
       0,     0,  -786,    10,    11,     0,     0,  -786,    12,  -786,
      13,    14,    15,    16,    17,    18,    19,     0,    67,    68,
      69,     0,    20,    21,    22,    23,    24,    25,    26,     0,
    -786,    27,  -786,     0,     0,     0,     0,    28,    29,   262,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,    45,    46,    47,     0,     0,
    -786,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,    51,     0,     0,     0,     0,     0,
       0,    52,     0,     0,    53,    54,     0,    55,    56,     0,
      57,     0,     0,    58,    59,    60,    61,    62,    63,    64,
      65,    66,     0,     0,     0,     4,     0,     5,     6,     7,
       8,     9,     0,     0,  -786,    10,    11,     0,     0,  -786,
      12,     0,    13,    14,    15,    16,    17,    18,    19,  -786,
      67,    68,    69,     0,    20,    21,    22,    23,    24,    25,
      26,     0,  -786,    27,  -786,     0,     0,     0,     0,    28,
      29,   262,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,     0,    41,    42,    43,    44,    45,    46,    47,
       0,     0,  -786,     0,     0,     0,     0,     0,     0,     0,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    50,    51,     0,     0,     0,
       0,     0,     0,    52,     0,     0,    53,    54,     0,    55,
      56,     0,    57,     0,     0,    58,    59,    60,    61,    62,
      63,    64,    65,    66,     0,     0,     0,     4,     0,     5,
       6,     7,     8,     9,     0,     0,  -786,    10,    11,     0,
       0,  -786,    12,     0,    13,    14,    15,    16,    17,    18,
      19,     0,    67,    68,    69,     0,    20,    21,    22,    23,
      24,    25,    26,     0,  -786,    27,  -786,     0,     0,     0,
       0,    28,    29,   262,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,    41,    42,    43,    44,    45,
      46,    47,     0,     0,  -786,     0,     0,     0,     0,     0,
       0,     0,    48,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    50,    51,     0,
       0,     0,     0,     0,     0,    52,     0,     0,    53,    54,
       0,    55,    56,     0,    57,     0,     0,    58,    59,    60,
      61,    62,    63,    64,    65,    66,     0,     0,     0,     4,
       0,     5,     6,     7,     8,     9,     0,  -786,  -786,    10,
      11,     0,     0,     0,    12,     0,    13,    14,    15,    16,
      17,    18,    19,     0,    67,    68,    69,     0,    20,    21,
      22,    23,    24,    25,    26,     0,  -786,    27,  -786,     0,
       0,     0,     0,    28,    29,   262,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,     0,    41,    42,    43,
      44,    45,    46,    47,     0,     0,  -786,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
      51,     0,     0,     0,     0,     0,     0,    52,     0,     0,
      53,    54,     0,    55,    56,     0,    57,     0,     0,    58,
      59,    60,    61,    62,    63,    64,    65,    66,     0,     0,
       0,     4,     0,     5,     6,     7,     8,     9,     0,     0,
    -786,    10,    11,     0,     0,     0,    12,     0,    13,    14,
      15,    16,    17,    18,    19,     0,    67,    68,    69,     0,
      20,    21,    22,    23,    24,    25,    26,     0,  -786,    27,
    -786,     0,     0,     0,     0,    28,    29,   262,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,    41,
      42,    43,    44,    45,    46,    47,     0,     0,  -786,     0,
       0,     0,     0,     0,     0,     0,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    50,    51,     0,     0,     0,     0,     0,     0,    52,
       0,     0,    53,    54,     0,    55,    56,     0,    57,     0,
       0,    58,    59,    60,    61,    62,    63,    64,    65,    66,
       0,     0,     0,     4,     0,     5,     6,     7,     8,     9,
       0,     0,     0,    10,    11,     0,     0,     0,    12,     0,
      13,    14,    15,    16,    17,    18,    19,     0,    67,    68,
      69,     0,    20,    21,    22,    23,    24,    25,    26,     0,
    -786,    27,  -786,     0,     0,     0,     0,    28,    29,   262,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,    45,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,    51,     0,     0,     0,     0,     0,
       0,    52,     0,     0,   263,    54,     0,    55,    56,     0,
      57,     0,     0,    58,    59,    60,    61,    62,    63,    64,
      65,    66,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      67,    68,    69,     0,     0,     0,     0,     0,     0,     0,
    -786,     0,  -786,     4,  -786,     5,     6,     7,     8,     9,
       0,     0,     0,    10,    11,     0,     0,     0,    12,     0,
      13,    14,    15,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,    29,   262,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,    45,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,    51,     0,     0,     0,     0,     0,
       0,    52,     0,     0,    53,    54,     0,    55,    56,     0,
      57,     0,     0,    58,    59,    60,    61,    62,    63,    64,
      65,    66,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      67,    68,    69,     0,     0,     0,     0,     0,     0,     0,
    -786,     0,  -786,     4,  -786,     5,     6,     7,     8,     9,
       0,     0,     0,    10,    11,     0,     0,     0,    12,     0,
      13,    14,    15,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,    45,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,    51,     0,     0,     0,     0,     0,
       0,    52,     0,     0,    53,    54,     0,    55,    56,     0,
      57,     0,     0,    58,    59,    60,    61,    62,    63,    64,
      65,    66,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      67,    68,    69,     0,     0,  -786,     0,     0,     0,     0,
       0,     0,  -786,     4,  -786,     5,     6,     7,     8,     9,
       0,     0,     0,    10,    11,     0,     0,     0,    12,     0,
      13,    14,    15,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,    29,   262,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,    45,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,    51,     0,     0,     0,     0,     0,
       0,    52,     0,     0,    53,    54,     0,    55,    56,     0,
      57,     0,     0,    58,    59,    60,    61,    62,    63,    64,
      65,    66,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      67,    68,    69,     0,     0,  -786,     0,     0,     0,     0,
       0,     0,  -786,     4,  -786,     5,     6,     7,     8,     9,
       0,     0,     0,    10,    11,     0,     0,     0,    12,     0,
      13,    14,    15,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,    29,   262,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,    45,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,    51,     0,     0,     0,     0,     0,
       0,    52,     0,     0,    53,    54,     0,    55,    56,     0,
      57,     0,     0,    58,    59,    60,    61,    62,    63,    64,
      65,    66,     0,  -786,     0,     4,     0,     5,     6,     7,
       8,     9,     0,     0,     0,    10,    11,     0,     0,     0,
      12,     0,    13,    14,    15,    16,    17,    18,    19,     0,
      67,    68,    69,     0,    20,    21,    22,    23,    24,    25,
      26,     0,  -786,    27,  -786,     0,     0,     0,     0,    28,
      29,     0,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,     0,    41,    42,    43,    44,    45,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    50,    51,     0,     0,     0,
       0,     0,     0,    52,     0,     0,    53,    54,     0,    55,
      56,     0,    57,     0,     0,    58,    59,    60,    61,    62,
      63,    64,    65,    66,     0,     0,   381,     0,     5,     6,
       7,     0,     9,     0,     0,     0,    10,    11,     0,     0,
       0,    12,  -773,    13,    14,    15,    16,    17,    18,    19,
       0,     0,    67,    68,    69,    20,    21,    22,    23,    24,
      25,    26,  -326,     0,    27,     0,  -326,     0,     0,     0,
       0,    29,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,    41,    42,    43,    44,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,    49,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    50,    51,     0,     0,
       0,     0,     0,     0,   211,     0,     0,   212,    54,     0,
      55,    56,     0,     0,     0,     0,    58,    59,    60,    61,
      62,    63,    64,    65,    66,     0,     0,  -774,     4,     0,
       5,     6,     7,     8,     9,     0,     0,     0,    10,    11,
       0,     0,     0,    12,     0,    13,    14,    15,    16,    17,
      18,    19,     0,    67,    68,    69,     0,    20,    21,    22,
      23,    24,    25,    26,     0,   310,    27,   311,     0,     0,
       0,     0,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,    41,    42,    43,    44,
      45,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    50,    51,
       0,     0,     0,     0,     0,     0,    52,     0,     0,    53,
      54,     0,    55,    56,     0,    57,     0,     0,    58,    59,
      60,    61,    62,    63,    64,    65,    66,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    67,    68,    69,     0,     0,
    -774,     0,     0,     0,     0,     0,     4,   521,     5,     6,
       7,     8,     9,  -774,  -774,  -774,    10,    11,     0,  -774,
    -774,    12,  -774,    13,    14,    15,    16,    17,    18,    19,
    -774,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
      28,    29,   262,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,    41,    42,    43,    44,    45,    46,
      47,     0,     0,  -774,     0,     0,     0,     0,     0,     0,
       0,    48,    49,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    50,    51,     0,     0,
       0,     0,     0,     0,    52,     0,     0,    53,    54,     0,
      55,    56,     0,    57,     0,     0,    58,    59,    60,    61,
      62,    63,    64,    65,    66,     0,  -774,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    67,    68,    69,     0,     0,  -774,     0,
       5,     6,     7,  -774,     9,   521,     0,     0,    10,    11,
       0,     0,     0,    12,     0,    13,    14,    15,    16,    17,
      18,    19,     0,     0,     0,     0,     0,    20,    21,    22,
      23,    24,    25,    26,     0,     0,   209,     0,     0,     0,
       0,     0,     0,    29,     0,     0,    32,    33,    34,    35,
      36,    37,    38,    39,    40,   210,    41,    42,    43,    44,
      45,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    50,    51,
       0,     0,     0,     0,     0,     0,   211,     0,     0,   212,
      54,     0,    55,    56,     0,   213,   214,   215,    58,    59,
     216,    61,    62,    63,    64,    65,    66,     0,     0,     0,
       0,     5,     6,     7,     0,     9,     0,     0,     0,    10,
      11,     0,     0,     0,    12,     0,    13,    14,    15,    16,
      17,    18,    19,     0,     0,    67,   217,    69,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,   242,
       0,     0,     0,     0,    29,     0,     0,    32,    33,    34,
      35,    36,    37,    38,    39,    40,     0,    41,    42,    43,
      44,    45,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
      51,     0,     0,     0,     0,     0,     0,   211,     0,     0,
     212,    54,     0,    55,    56,     0,     0,     0,     0,    58,
      59,    60,    61,    62,    63,    64,    65,    66,     0,     0,
       0,     0,   129,   130,   131,   132,   133,   134,   135,   136,
     137,   138,   139,   140,   141,   142,   143,   144,   145,   146,
     147,   148,   149,   150,   151,   152,    67,    68,    69,   153,
     154,   155,   410,   411,   412,   413,   160,   161,   162,     0,
     242,     0,     0,     0,   163,   164,   165,   166,   414,   415,
     416,   417,   171,    37,    38,   418,    40,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   173,   174,   175,   176,   177,
     178,   179,   180,   181,     0,     0,   182,   183,     0,     0,
       0,     0,   184,   185,   186,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   190,   191,   192,
     193,   194,   195,   196,   197,   198,   199,     0,   200,   201,
       0,     0,     0,     0,     0,   202,   419,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,   139,   140,   141,
     142,   143,   144,   145,   146,   147,   148,   149,   150,   151,
     152,     0,     0,     0,   153,   154,   155,   156,   157,   158,
     159,   160,   161,   162,     0,     0,     0,     0,     0,   163,
     164,   165,   166,   167,   168,   169,   170,   171,    37,    38,
     172,    40,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     173,   174,   175,   176,   177,   178,   179,   180,   181,     0,
       0,   182,   183,     0,     0,     0,     0,   184,   185,   186,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   190,   191,   192,   193,   194,   195,   196,   197,
     198,   199,     0,   200,   201,     0,     0,     0,     0,     0,
     202,   129,   130,   131,   132,   133,   134,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
     148,   149,   150,   151,   152,     0,     0,     0,   153,   154,
     155,   156,   157,   158,   159,   160,   161,   162,     0,     0,
       0,     0,     0,   163,   164,   165,   166,   167,   168,   169,
     170,   171,   244,     0,   172,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   173,   174,   175,   176,   177,   178,
     179,   180,   181,     0,     0,   182,   183,     0,     0,     0,
       0,   184,   185,   186,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,     0,     0,
      59,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   190,   191,   192,   193,
     194,   195,   196,   197,   198,   199,     0,   200,   201,     0,
       0,     0,     0,     0,   202,   129,   130,   131,   132,   133,
     134,   135,   136,   137,   138,   139,   140,   141,   142,   143,
     144,   145,   146,   147,   148,   149,   150,   151,   152,     0,
       0,     0,   153,   154,   155,   156,   157,   158,   159,   160,
     161,   162,     0,     0,     0,     0,     0,   163,   164,   165,
     166,   167,   168,   169,   170,   171,     0,     0,   172,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   173,   174,
     175,   176,   177,   178,   179,   180,   181,     0,     0,   182,
     183,     0,     0,     0,     0,   184,   185,   186,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,     0,     0,    59,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     190,   191,   192,   193,   194,   195,   196,   197,   198,   199,
       0,   200,   201,     0,     0,     0,     0,     0,   202,   129,
     130,   131,   132,   133,   134,   135,   136,   137,   138,   139,
     140,   141,   142,   143,   144,   145,   146,   147,   148,   149,
     150,   151,   152,     0,     0,     0,   153,   154,   155,   156,
     157,   158,   159,   160,   161,   162,     0,     0,     0,     0,
       0,   163,   164,   165,   166,   167,   168,   169,   170,   171,
       0,     0,   172,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   173,   174,   175,   176,   177,   178,   179,   180,
     181,     0,     0,   182,   183,     0,     0,     0,     0,   184,
     185,   186,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   190,   191,   192,   193,   194,   195,
     196,   197,   198,   199,     0,   200,   201,     5,     6,     7,
       0,     9,   202,     0,     0,    10,    11,     0,     0,     0,
      12,     0,    13,    14,    15,   250,   251,    18,    19,     0,
       0,     0,     0,     0,    20,   252,   253,    23,    24,    25,
      26,     0,     0,   209,     0,     0,     0,     0,     0,     0,
     281,     0,     0,    32,    33,    34,    35,    36,    37,    38,
      39,    40,     0,    41,    42,    43,    44,    45,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   282,     0,     0,   212,    54,     0,    55,
      56,     0,     0,     0,     0,    58,    59,    60,    61,    62,
      63,    64,    65,    66,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     0,     9,
       0,     0,   283,    10,    11,     0,     0,     0,    12,   284,
      13,    14,    15,   250,   251,    18,    19,     0,     0,     0,
       0,     0,    20,   252,   253,    23,    24,    25,    26,     0,
       0,   209,     0,     0,     0,     0,     0,     0,   281,     0,
       0,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,    45,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   282,     0,     0,   212,    54,     0,    55,    56,     0,
       0,     0,     0,    58,    59,    60,    61,    62,    63,    64,
      65,    66,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     4,     0,     5,     6,     7,     8,     9,     0,     0,
     283,    10,    11,     0,     0,     0,    12,   581,    13,    14,
      15,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,    29,     0,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,    41,
      42,    43,    44,    45,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    50,    51,     0,     0,     0,     0,     0,     0,    52,
       0,     0,    53,    54,     0,    55,    56,     0,    57,     0,
       0,    58,    59,    60,    61,    62,    63,    64,    65,    66,
       0,     0,   381,     0,     5,     6,     7,     0,     9,     0,
       0,     0,    10,    11,     0,     0,     0,    12,     0,    13,
      14,    15,    16,    17,    18,    19,     0,     0,    67,    68,
      69,    20,    21,    22,    23,    24,    25,    26,     0,     0,
      27,     0,     0,     0,     0,     0,     0,    29,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
      41,    42,    43,    44,    45,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    48,    49,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    50,    51,     0,     0,     0,     0,     0,     0,
     211,     0,     0,   212,    54,     0,    55,    56,     0,     0,
       0,     0,    58,    59,    60,    61,    62,    63,    64,    65,
      66,     0,     0,     0,     0,     5,     6,     7,     0,     9,
       0,     0,     0,    10,    11,     0,     0,     0,    12,     0,
      13,    14,    15,    16,    17,    18,    19,     0,     0,    67,
      68,    69,    20,    21,    22,    23,    24,    25,    26,     0,
       0,   209,     0,     0,     0,     0,     0,     0,    29,     0,
       0,    32,    33,    34,    35,    36,    37,    38,    39,    40,
     210,    41,    42,    43,    44,    45,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,    51,     0,     0,     0,     0,     0,
       0,   211,     0,     0,   212,    54,     0,    55,    56,     0,
     213,   214,   215,    58,    59,   216,    61,    62,    63,    64,
      65,    66,     0,     0,     0,     0,     5,     6,     7,     0,
       9,     0,     0,     0,    10,    11,     0,     0,     0,    12,
       0,    13,    14,    15,    16,    17,    18,    19,     0,     0,
      67,   217,    69,    20,    21,    22,    23,    24,    25,    26,
       0,     0,   209,     0,     0,     0,     0,     0,     0,    29,
       0,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,   210,    41,    42,    43,    44,    45,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
      49,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    50,   457,     0,     0,     0,     0,
       0,     0,   211,     0,     0,   212,    54,     0,    55,    56,
       0,   213,   214,   215,    58,    59,   216,    61,    62,    63,
      64,    65,    66,     0,     0,     0,     0,     5,     6,     7,
       0,     9,     0,     0,     0,    10,    11,     0,     0,     0,
      12,     0,    13,    14,    15,   250,   251,    18,    19,     0,
       0,    67,   217,    69,    20,   252,   253,    23,    24,    25,
      26,     0,     0,   209,     0,     0,     0,     0,     0,     0,
      29,     0,     0,    32,    33,    34,    35,    36,    37,    38,
      39,    40,   210,    41,    42,    43,    44,    45,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    50,    51,     0,     0,     0,
       0,     0,     0,   211,     0,     0,   212,    54,     0,    55,
      56,     0,   668,   214,   215,    58,    59,   216,    61,    62,
      63,    64,    65,    66,     0,     0,     0,     0,     5,     6,
       7,     0,     9,     0,     0,     0,    10,    11,     0,     0,
       0,    12,     0,    13,    14,    15,   250,   251,    18,    19,
       0,     0,    67,   217,    69,    20,   252,   253,    23,    24,
      25,    26,     0,     0,   209,     0,     0,     0,     0,     0,
       0,    29,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,   210,    41,    42,    43,    44,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,    49,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    50,   457,     0,     0,
       0,     0,     0,     0,   211,     0,     0,   212,    54,     0,
      55,    56,     0,   668,   214,   215,    58,    59,   216,    61,
      62,    63,    64,    65,    66,     0,     0,     0,     0,     5,
       6,     7,     0,     9,     0,     0,     0,    10,    11,     0,
       0,     0,    12,     0,    13,    14,    15,   250,   251,    18,
      19,     0,     0,    67,   217,    69,    20,   252,   253,    23,
      24,    25,    26,     0,     0,   209,     0,     0,     0,     0,
       0,     0,    29,     0,     0,    32,    33,    34,    35,    36,
      37,    38,    39,    40,   210,    41,    42,    43,    44,    45,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    48,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    50,    51,     0,
       0,     0,     0,     0,     0,   211,     0,     0,   212,    54,
       0,    55,    56,     0,   213,   214,     0,    58,    59,   216,
      61,    62,    63,    64,    65,    66,     0,     0,     0,     0,
       5,     6,     7,     0,     9,     0,     0,     0,    10,    11,
       0,     0,     0,    12,     0,    13,    14,    15,   250,   251,
      18,    19,     0,     0,    67,   217,    69,    20,   252,   253,
      23,    24,    25,    26,     0,     0,   209,     0,     0,     0,
       0,     0,     0,    29,     0,     0,    32,    33,    34,    35,
      36,    37,    38,    39,    40,   210,    41,    42,    43,    44,
      45,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    50,    51,
       0,     0,     0,     0,     0,     0,   211,     0,     0,   212,
      54,     0,    55,    56,     0,     0,   214,   215,    58,    59,
     216,    61,    62,    63,    64,    65,    66,     0,     0,     0,
       0,     5,     6,     7,     0,     9,     0,     0,     0,    10,
      11,     0,     0,     0,    12,     0,    13,    14,    15,   250,
     251,    18,    19,     0,     0,    67,   217,    69,    20,   252,
     253,    23,    24,    25,    26,     0,     0,   209,     0,     0,
       0,     0,     0,     0,    29,     0,     0,    32,    33,    34,
      35,    36,    37,    38,    39,    40,   210,    41,    42,    43,
      44,    45,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
      51,     0,     0,     0,     0,     0,     0,   211,     0,     0,
     212,    54,     0,    55,    56,     0,   668,   214,     0,    58,
      59,   216,    61,    62,    63,    64,    65,    66,     0,     0,
       0,     0,     5,     6,     7,     0,     9,     0,     0,     0,
      10,    11,     0,     0,     0,    12,     0,    13,    14,    15,
     250,   251,    18,    19,     0,     0,    67,   217,    69,    20,
     252,   253,    23,    24,    25,    26,     0,     0,   209,     0,
       0,     0,     0,     0,     0,    29,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,   210,    41,    42,
      43,    44,    45,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      50,    51,     0,     0,     0,     0,     0,     0,   211,     0,
       0,   212,    54,     0,    55,    56,     0,     0,   214,     0,
      58,    59,   216,    61,    62,    63,    64,    65,    66,     0,
       0,     0,     0,     5,     6,     7,     0,     9,     0,     0,
       0,    10,    11,     0,     0,     0,    12,     0,    13,    14,
      15,    16,    17,    18,    19,     0,     0,    67,   217,    69,
      20,    21,    22,    23,    24,    25,    26,     0,     0,   209,
       0,     0,     0,     0,     0,     0,    29,     0,     0,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,    41,
      42,    43,    44,    45,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    50,    51,     0,     0,     0,     0,     0,     0,   211,
       0,     0,   212,    54,     0,    55,    56,     0,   764,     0,
       0,    58,    59,    60,    61,    62,    63,    64,    65,    66,
       0,     0,     0,     0,     5,     6,     7,     0,     9,     0,
       0,     0,    10,    11,     0,     0,     0,    12,     0,    13,
      14,    15,   250,   251,    18,    19,     0,     0,    67,   217,
      69,    20,   252,   253,    23,    24,    25,    26,     0,     0,
     209,     0,     0,     0,     0,     0,     0,    29,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
      41,    42,    43,    44,    45,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    48,    49,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    50,    51,     0,     0,     0,     0,     0,     0,
     211,     0,     0,   212,    54,     0,    55,    56,     0,   764,
       0,     0,    58,    59,    60,    61,    62,    63,    64,    65,
      66,     0,     0,     0,     0,     5,     6,     7,     0,     9,
       0,     0,     0,    10,    11,     0,     0,     0,    12,     0,
      13,    14,    15,   250,   251,    18,    19,     0,     0,    67,
     217,    69,    20,   252,   253,    23,    24,    25,    26,     0,
       0,   209,     0,     0,     0,     0,     0,     0,    29,     0,
       0,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,    45,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,    51,     0,     0,     0,     0,     0,
       0,   211,     0,     0,   212,    54,     0,    55,    56,     0,
     985,     0,     0,    58,    59,    60,    61,    62,    63,    64,
      65,    66,     0,     0,     0,     0,     5,     6,     7,     0,
       9,     0,     0,     0,    10,    11,     0,     0,     0,    12,
       0,    13,    14,    15,   250,   251,    18,    19,     0,     0,
      67,   217,    69,    20,   252,   253,    23,    24,    25,    26,
       0,     0,   209,     0,     0,     0,     0,     0,     0,    29,
       0,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,    41,    42,    43,    44,    45,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
      49,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    50,    51,     0,     0,     0,     0,
       0,     0,   211,     0,     0,   212,    54,     0,    55,    56,
       0,  1035,     0,     0,    58,    59,    60,    61,    62,    63,
      64,    65,    66,     0,     0,     0,     0,     5,     6,     7,
       0,     9,     0,     0,     0,    10,    11,     0,     0,     0,
      12,     0,    13,    14,    15,   250,   251,    18,    19,     0,
       0,    67,   217,    69,    20,   252,   253,    23,    24,    25,
      26,     0,     0,   209,     0,     0,     0,     0,     0,     0,
      29,     0,     0,    32,    33,    34,    35,    36,    37,    38,
      39,    40,     0,    41,    42,    43,    44,    45,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    50,    51,     0,     0,     0,
       0,     0,     0,   211,     0,     0,   212,    54,     0,    55,
      56,     0,  1182,     0,     0,    58,    59,    60,    61,    62,
      63,    64,    65,    66,     0,     0,     0,     0,     5,     6,
       7,     0,     9,     0,     0,     0,    10,    11,     0,     0,
       0,    12,     0,    13,    14,    15,   250,   251,    18,    19,
       0,     0,    67,   217,    69,    20,   252,   253,    23,    24,
      25,    26,     0,     0,   209,     0,     0,     0,     0,     0,
       0,    29,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,    41,    42,    43,    44,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,    49,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    50,    51,     0,     0,
       0,     0,     0,     0,   211,     0,     0,   212,    54,     0,
      55,    56,     0,     0,     0,     0,    58,    59,    60,    61,
      62,    63,    64,    65,    66,     0,     0,     0,     0,     5,
       6,     7,     0,     9,     0,     0,     0,    10,    11,     0,
       0,     0,    12,     0,    13,    14,    15,    16,    17,    18,
      19,     0,     0,    67,   217,    69,    20,    21,    22,    23,
      24,    25,    26,     0,     0,   209,     0,     0,     0,     0,
       0,     0,    29,     0,     0,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,    41,    42,    43,    44,    45,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    48,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    50,    51,     0,
       0,     0,     0,     0,     0,   211,     0,     0,   212,    54,
       0,    55,    56,     0,     0,     0,     0,    58,    59,    60,
      61,    62,    63,    64,    65,    66,     0,     0,     0,     0,
       5,     6,     7,     0,     9,     0,     0,     0,    10,    11,
       0,     0,     0,    12,     0,    13,    14,    15,    16,    17,
      18,    19,     0,     0,    67,   217,    69,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,     0,    29,     0,     0,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,    41,    42,    43,    44,
      45,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    50,    51,
       0,     0,     0,     0,     0,     0,   211,     0,     0,   212,
      54,     0,    55,    56,     0,     0,     0,     0,    58,    59,
      60,    61,    62,    63,    64,    65,    66,     0,     0,     0,
       0,     5,     6,     7,     0,     9,     0,     0,     0,    10,
      11,     0,     0,     0,    12,     0,    13,    14,    15,    16,
      17,    18,    19,     0,     0,    67,    68,    69,    20,    21,
      22,    23,    24,    25,    26,     0,     0,   746,     0,     0,
       0,     0,     0,     0,    29,     0,     0,    32,    33,    34,
      35,    36,    37,    38,    39,    40,     0,    41,    42,    43,
      44,    45,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
      51,     0,     0,     0,     0,     0,     0,   211,     0,     0,
     212,    54,     0,    55,    56,     0,     0,     0,     0,    58,
      59,    60,    61,    62,    63,    64,    65,    66,     0,     0,
       0,     0,     5,     6,     7,     0,     9,     0,     0,     0,
      10,    11,     0,     0,     0,    12,     0,    13,    14,    15,
     250,   251,    18,    19,     0,     0,    67,   217,    69,    20,
     252,   253,    23,    24,    25,    26,     0,     0,   843,     0,
       0,     0,     0,     0,     0,    29,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,    41,    42,
      43,    44,    45,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      50,    51,     0,     0,     0,     0,     0,     0,   211,     0,
       0,   212,    54,     0,    55,    56,     0,     0,     0,     0,
      58,    59,    60,    61,    62,    63,    64,    65,    66,     0,
       0,     0,     0,     5,     6,     7,     0,     9,     0,     0,
       0,    10,    11,     0,     0,     0,    12,     0,    13,    14,
      15,   250,   251,    18,    19,     0,     0,    67,   217,    69,
      20,   252,   253,    23,    24,    25,    26,     0,     0,   209,
       0,     0,     0,     0,     0,     0,   281,     0,     0,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,    41,
      42,    43,    44,    45,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   282,
       0,     0,   341,    54,     0,    55,    56,     0,   342,     0,
       0,    58,    59,    60,    61,    62,    63,    64,    65,    66,
       0,     0,     5,     6,     7,     0,     9,     0,     0,     0,
      10,    11,     0,     0,     0,    12,     0,    13,    14,    15,
     250,   251,    18,    19,     0,     0,     0,     0,   283,    20,
     252,   253,    23,    24,    25,    26,     0,     0,   209,     0,
       0,     0,     0,     0,     0,   281,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,    41,    42,
      43,    44,    45,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   391,     0,
       0,    53,    54,     0,    55,    56,     0,    57,     0,     0,
      58,    59,    60,    61,    62,    63,    64,    65,    66,     0,
       0,     5,     6,     7,     0,     9,     0,     0,     0,    10,
      11,     0,     0,     0,    12,     0,    13,    14,    15,   250,
     251,    18,    19,     0,     0,     0,     0,   283,    20,   252,
     253,    23,    24,    25,    26,     0,     0,   209,     0,     0,
       0,     0,     0,     0,   281,     0,     0,    32,    33,    34,
     399,    36,    37,    38,   400,    40,     0,    41,    42,    43,
      44,    45,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   401,     0,     0,     0,   402,     0,     0,
     212,    54,     0,    55,    56,     0,     0,     0,     0,    58,
      59,    60,    61,    62,    63,    64,    65,    66,     0,     0,
       5,     6,     7,     0,     9,     0,     0,     0,    10,    11,
       0,     0,     0,    12,     0,    13,    14,    15,   250,   251,
      18,    19,     0,     0,     0,     0,   283,    20,   252,   253,
      23,    24,    25,    26,     0,     0,   209,     0,     0,     0,
       0,     0,     0,   281,     0,     0,    32,    33,    34,   399,
      36,    37,    38,   400,    40,     0,    41,    42,    43,    44,
      45,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   402,     0,     0,   212,
      54,     0,    55,    56,     0,     0,     0,     0,    58,    59,
      60,    61,    62,    63,    64,    65,    66,     0,     0,     5,
       6,     7,     0,     9,     0,     0,     0,    10,    11,     0,
       0,     0,    12,     0,    13,    14,    15,   250,   251,    18,
      19,     0,     0,     0,     0,   283,    20,   252,   253,    23,
      24,    25,    26,     0,     0,   209,     0,     0,     0,     0,
       0,     0,   281,     0,     0,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,    41,    42,    43,    44,    45,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   282,     0,     0,   341,    54,
       0,    55,    56,     0,     0,     0,     0,    58,    59,    60,
      61,    62,    63,    64,    65,    66,     0,     0,     5,     6,
       7,     0,     9,     0,     0,     0,    10,    11,     0,     0,
       0,    12,     0,    13,    14,    15,   250,   251,    18,    19,
       0,     0,     0,     0,   283,    20,   252,   253,    23,    24,
      25,    26,     0,     0,   209,     0,     0,     0,     0,     0,
       0,   281,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,    41,    42,    43,    44,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1171,     0,     0,   212,    54,     0,
      55,    56,     0,     0,     0,     0,    58,    59,    60,    61,
      62,    63,    64,    65,    66,     0,     0,     5,     6,     7,
       0,     9,     0,     0,     0,    10,    11,     0,     0,     0,
      12,     0,    13,    14,    15,   250,   251,    18,    19,     0,
       0,     0,     0,   283,    20,   252,   253,    23,    24,    25,
      26,     0,     0,   209,     0,     0,     0,     0,     0,     0,
     281,     0,     0,    32,    33,    34,    35,    36,    37,    38,
      39,    40,     0,    41,    42,    43,    44,    45,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     611,   612,     0,  1204,   613,     0,   212,    54,     0,    55,
      56,     0,     0,     0,     0,    58,    59,    60,    61,    62,
      63,    64,    65,    66,   173,   174,   175,   176,   177,   178,
     179,   180,   181,     0,     0,   182,   183,     0,     0,     0,
       0,   184,   185,   186,   187,     0,     0,     0,     0,     0,
       0,     0,   283,     0,     0,     0,   188,   189,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   190,   191,   192,   193,
     194,   195,   196,   197,   198,   199,     0,   200,   201,   621,
     622,     0,     0,   623,   202,   238,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   173,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   190,   191,   192,   193,   194,
     195,   196,   197,   198,   199,     0,   200,   201,   672,   612,
       0,     0,   673,   202,   238,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   173,   174,   175,   176,   177,   178,   179,   180,
     181,     0,     0,   182,   183,     0,     0,     0,     0,   184,
     185,   186,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   190,   191,   192,   193,   194,   195,
     196,   197,   198,   199,     0,   200,   201,   675,   622,     0,
       0,   676,   202,   238,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   173,   174,   175,   176,   177,   178,   179,   180,   181,
       0,     0,   182,   183,     0,     0,     0,     0,   184,   185,
     186,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   190,   191,   192,   193,   194,   195,   196,
     197,   198,   199,     0,   200,   201,   702,   612,     0,     0,
     703,   202,   238,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     173,   174,   175,   176,   177,   178,   179,   180,   181,     0,
       0,   182,   183,     0,     0,     0,     0,   184,   185,   186,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   190,   191,   192,   193,   194,   195,   196,   197,
     198,   199,     0,   200,   201,   705,   622,     0,     0,   706,
     202,   238,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   173,
     174,   175,   176,   177,   178,   179,   180,   181,     0,     0,
     182,   183,     0,     0,     0,     0,   184,   185,   186,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   190,   191,   192,   193,   194,   195,   196,   197,   198,
     199,     0,   200,   201,   818,   612,     0,     0,   819,   202,
     238,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   173,   174,
     175,   176,   177,   178,   179,   180,   181,     0,     0,   182,
     183,     0,     0,     0,     0,   184,   185,   186,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     190,   191,   192,   193,   194,   195,   196,   197,   198,   199,
       0,   200,   201,   821,   622,     0,     0,   822,   202,   238,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   173,   174,   175,
     176,   177,   178,   179,   180,   181,     0,     0,   182,   183,
       0,     0,     0,     0,   184,   185,   186,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     189,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   190,
     191,   192,   193,   194,   195,   196,   197,   198,   199,     0,
     200,   201,   827,   612,     0,     0,   828,   202,   238,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   173,   174,   175,   176,
     177,   178,   179,   180,   181,     0,     0,   182,   183,     0,
       0,     0,     0,   184,   185,   186,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   190,   191,
     192,   193,   194,   195,   196,   197,   198,   199,     0,   200,
     201,   657,   622,     0,     0,   658,   202,   238,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   173,   174,   175,   176,   177,
     178,   179,   180,   181,     0,     0,   182,   183,     0,     0,
       0,     0,   184,   185,   186,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   190,   191,   192,
     193,   194,   195,   196,   197,   198,   199,     0,   200,   201,
    1041,   612,     0,     0,  1042,   202,   238,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   173,   174,   175,   176,   177,   178,
     179,   180,   181,     0,     0,   182,   183,     0,     0,     0,
       0,   184,   185,   186,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   190,   191,   192,   193,
     194,   195,   196,   197,   198,   199,     0,   200,   201,  1044,
     622,     0,     0,  1045,   202,   238,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   173,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   190,   191,   192,   193,   194,
     195,   196,   197,   198,   199,     0,   200,   201,  1265,   612,
       0,     0,  1266,   202,   238,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   173,   174,   175,   176,   177,   178,   179,   180,
     181,     0,     0,   182,   183,     0,     0,     0,     0,   184,
     185,   186,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   190,   191,   192,   193,   194,   195,
     196,   197,   198,   199,     0,   200,   201,  1268,   622,     0,
       0,  1269,   202,   238,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   173,   174,   175,   176,   177,   178,   179,   180,   181,
       0,     0,   182,   183,     0,     0,     0,     0,   184,   185,
     186,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   190,   191,   192,   193,   194,   195,   196,
     197,   198,   199,     0,   200,   201,  1284,   612,     0,     0,
    1285,   202,   238,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     173,   174,   175,   176,   177,   178,   179,   180,   181,     0,
       0,   182,   183,     0,     0,     0,     0,   184,   185,   186,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   190,   191,   192,   193,   194,   195,   196,   197,
     198,   199,     0,   200,   201,   657,   622,     0,     0,   658,
     202,   238,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   173,
     174,   175,   176,   177,   178,   179,   180,   181,     0,     0,
     182,   183,     0,     0,     0,     0,   184,   185,   186,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   190,   191,   192,   193,   194,   195,   196,   197,   198,
     199,     0,   200,   201,     0,     0,     0,     0,     0,   202
};

static const yytype_int16 yycheck[] =
{
       2,   322,   105,    60,     8,   326,    16,    17,    91,   377,
      85,   379,     2,    59,     8,    59,     2,   402,    96,    93,
      94,   104,    28,    97,    28,    53,   326,    94,   556,   733,
      97,   377,   443,   379,    28,    71,   384,    16,    17,   232,
     451,    57,   378,   584,   238,   736,    22,   813,    22,   567,
     451,   784,   570,    55,    56,    59,   569,   101,    57,   427,
     740,    97,   806,    97,    54,   569,   275,    53,    54,    85,
     279,    57,    68,   500,    76,    77,    55,   876,   446,   874,
     448,   427,    98,    99,   100,   584,   783,   784,    71,   490,
     783,   784,   102,    64,    65,    66,    77,   101,  1187,    85,
     446,    29,   448,    25,    80,    53,    80,   475,   476,    67,
    1053,   447,    98,    99,   100,   101,   263,  1030,    13,   660,
     661,    10,  1053,   102,    25,   322,  1069,   913,   914,   326,
     476,   299,   300,   501,   652,   503,  1067,   100,    58,   917,
    1208,    66,   101,   110,   227,    69,   100,    13,    16,    17,
       0,    77,   665,    69,   581,   155,   157,   503,    78,   216,
      13,   162,   162,    26,    83,    84,   502,   241,    27,   243,
      69,    13,   135,   256,    98,   142,    16,    17,    66,    68,
      25,   135,    98,   100,   212,   733,  1275,    55,    56,   109,
     110,    13,   740,    25,   341,   120,   124,    13,    97,    98,
     538,    25,    25,   162,    13,   561,   289,   563,     9,   284,
    1123,    25,  1280,   162,    15,    13,   218,   219,   135,   138,
     139,   249,   142,    13,   226,   235,   212,   237,   238,   265,
     232,   265,   120,   155,   102,   263,   238,   218,   219,   152,
      13,   315,   316,   317,   318,   247,   320,   321,   315,   316,
     317,   318,    37,    38,   155,   154,   235,   247,   237,   238,
     155,   247,   102,   249,   649,   160,    25,   162,   284,   462,
    1213,   639,   683,   641,   685,  1218,  1219,   263,    26,   253,
     254,   649,   265,   651,   685,    13,  1072,  1073,  1219,   152,
     155,   157,    56,   219,   160,   641,   162,   162,   284,  1077,
    1078,   160,   801,   162,   640,   651,    25,   160,   826,   162,
     155,   394,   386,   341,   650,   263,  1115,  1021,   160,   386,
     162,  1116,   789,   155,   314,   283,   342,   401,   314,   319,
     797,   155,   155,   319,   157,   157,  1016,   339,   160,   100,
     162,   155,   344,   342,   160,   772,   162,   152,   516,  1292,
     518,   160,   388,   162,   388,   341,   342,   385,   886,   387,
     286,    27,   160,    29,   162,  1058,  1099,   235,   378,   237,
     160,   153,   162,   100,   135,   379,   378,  1070,   161,   892,
     162,   792,   720,   721,  1077,  1078,   707,   160,   892,   162,
     135,    25,   100,   341,   380,   235,   155,   237,   157,   385,
      98,   387,  1099,    28,   152,   379,  1099,   707,   135,   157,
     155,   241,   733,    66,   135,   956,   957,   100,  1184,   740,
     961,   962,   100,   427,    66,   500,    69,   135,   430,   431,
      37,    38,   160,    56,   162,   155,   155,   447,   268,   441,
    1184,    25,   272,    25,   448,   447,   210,   449,   450,   213,
     214,   215,   135,   101,    97,    98,   154,   135,   460,   157,
     462,    66,   727,   135,   470,    34,   470,   120,  1016,   122,
     123,   736,   476,   991,   448,   477,   470,   155,   671,   135,
     122,   123,  1030,    52,   500,   157,   101,  1000,   155,   683,
     699,   113,   502,    54,     2,   162,   482,  1201,  1039,   503,
     502,   135,   476,    64,    65,    69,   581,   155,    16,    17,
     707,   154,   100,    66,   500,  1206,  1034,   122,   123,   161,
     125,   155,  1030,   157,   158,   100,   882,   883,   162,   503,
     915,   887,   568,   889,    98,   891,   874,   858,   876,   907,
     155,   909,   544,   711,  1085,    53,    54,   135,   716,    57,
    1294,   135,  1213,   135,   556,   162,   566,  1218,   858,   569,
      68,   907,   100,   909,   642,   581,    25,   120,   100,   122,
     123,   155,   908,   155,   158,   443,   158,    85,   162,   653,
     162,   567,   578,  1287,   570,    93,    94,   100,   584,    97,
      98,    99,   100,  1121,   102,   581,  1007,   135,   634,   578,
      58,  1129,   630,   135,   632,   584,  1007,    69,   274,   275,
     100,    34,    52,   279,   688,   281,    56,    66,   620,   487,
      78,   155,   135,   453,   626,   700,   628,   100,   458,    52,
     640,   461,    14,    15,   464,    97,    98,   641,   640,   987,
     650,   617,   155,   617,   630,   135,   632,   651,   650,   479,
     626,   109,   626,  1201,   484,   113,   662,   659,   662,   852,
    1208,   858,   135,   157,   660,   661,   652,   641,   662,   671,
    1188,   120,    69,   122,   123,    56,   135,   651,   442,   443,
     660,   661,   155,   659,   700,   659,    69,   451,   135,  1074,
      52,   693,   154,  1201,    56,  1016,   155,   772,   781,   158,
    1208,    98,  1210,   162,   212,    69,    15,   158,    69,  1030,
    1067,   162,    66,    67,   700,    98,    25,   547,   692,   693,
    1077,  1078,   160,   487,   162,   100,   490,   235,  1256,   237,
     238,   834,  1280,   241,    98,   243,    97,    98,    69,   247,
     711,   249,   713,  1091,    69,   716,   717,   155,  1096,    78,
     580,   825,   101,   660,   661,   263,   772,    15,   825,    17,
     135,  1117,  1118,  1119,  1120,  1030,    97,    98,   122,   123,
     677,   678,  1280,    98,  1282,   155,   284,  1115,  1116,  1287,
     155,  1289,    26,   100,   786,   787,   772,   694,   824,   833,
     554,   793,   794,   154,   157,   823,  1304,   783,   784,   801,
     802,    66,   804,    78,   806,   157,   314,   315,   316,   317,
     318,   319,   320,   321,   578,   683,   145,   146,   147,   157,
     584,   100,   801,   154,   159,    69,   157,   162,   158,   833,
     100,   135,  1180,   341,   342,    69,   159,   823,    69,    69,
     826,   153,    69,    56,   846,   847,   157,   849,   850,   155,
     852,    89,    90,    97,    98,   120,   135,   122,   123,  1227,
     125,  1229,   161,   920,    98,   135,    97,    98,    98,  1058,
     378,    98,   380,   285,   286,   921,   155,   385,   386,   387,
    1201,    78,   892,  1229,   886,   155,  1254,  1208,  1077,  1078,
     135,  1247,  1228,   401,   724,   897,   135,   899,   908,   663,
     135,    69,   976,   905,   668,   909,   908,   107,   152,   976,
     154,   160,   902,   157,   904,   155,   902,    56,   904,   683,
      25,   685,   674,   154,   792,   139,    69,   913,   914,    97,
      98,   917,   135,   100,   600,   909,  1201,    40,    41,   447,
     135,  1206,   970,  1208,   155,  1210,   158,   975,   934,   155,
      69,   153,   704,   619,    97,    98,   992,   153,   960,  1280,
     956,   957,   155,   155,   966,   961,   962,   731,   135,   577,
      69,   155,   783,   784,   482,    66,   956,   957,    97,    98,
     135,   961,   962,   155,   970,   155,   154,    52,   155,   975,
    1064,   155,   500,   155,   502,    52,   153,  1064,    97,    98,
     764,  1003,   832,  1031,   155,   991,   155,     8,   674,    13,
      25,   154,    17,   155,   155,  1280,    44,  1282,   135,   627,
      44,   851,  1287,   853,  1289,   633,   153,   635,   792,   120,
     155,   122,   123,   699,   125,   154,    44,   801,   704,  1304,
     155,    44,   872,  1039,   137,  1031,   135,   159,  1034,   956,
     957,    15,   816,   155,   961,   962,    52,    26,   566,   567,
     155,   569,   570,   155,  1050,   155,  1094,  1053,   820,   155,
     578,   140,   155,   581,  1090,  1103,   584,    52,   830,   140,
     746,  1067,  1068,  1069,  1112,   155,  1072,  1073,  1145,  1085,
    1173,  1077,  1078,   155,  1001,  1002,   155,  1004,  1005,   101,
      69,   155,   913,   914,  1090,  1085,   917,   155,  1094,   736,
     155,  1127,   153,  1099,    52,   101,   158,  1103,   155,  1121,
       2,   160,   630,   934,   632,   155,  1112,  1129,    97,    98,
     140,    56,   640,    66,    16,    17,   155,   901,   155,   155,
      66,  1127,   650,   751,   652,   653,    69,   755,   155,  1177,
       9,  1179,   660,   661,   820,   155,   783,   784,   155,   153,
    1234,  1235,  1190,   155,   830,   917,   155,  1234,  1235,    78,
     140,    53,    54,    56,    97,    98,  1178,   843,  1085,   121,
     688,   155,  1184,   152,  1200,   154,    68,   120,   157,   122,
     123,  1177,   700,  1179,   120,   740,   122,   123,   743,   155,
     155,  1108,  1188,   155,  1190,   155,   814,   155,   344,   817,
     157,    93,    94,   157,  1200,    97,   247,   314,  1228,    69,
     102,   985,   478,   831,   833,  1229,  1228,  1213,  1230,   482,
    1232,   154,  1218,  1219,   143,   144,   145,   146,   147,  1050,
      26,    98,  1053,  1007,  1272,   100,  1076,    97,    98,    89,
     659,   917,   918,   802,  1256,  1229,  1067,  1068,  1069,  1232,
     720,  1072,  1073,   874,   772,  1125,  1077,  1078,  1030,   783,
     784,  1035,  1275,    69,   337,    52,   835,    54,    55,    56,
      57,   899,    52,    69,    54,    55,  1272,    57,  1099,   573,
    1068,  1043,  1294,    69,   430,   431,  1294,  1066,    69,  1070,
    1052,    97,    98,  1055,   154,   441,  1292,  1070,  1138,  1230,
     108,    97,    98,   449,   450,   823,    69,   825,   826,  1149,
    1178,    97,    98,  1075,   518,   102,    97,    98,   101,   736,
     212,   108,   102,  1163,  1164,  1165,  1206,  1201,  1102,   733,
    1146,   477,    -1,  1009,    97,    98,    -1,    52,    -1,    54,
      55,    56,    57,   235,    -1,   237,   238,  1030,   154,   241,
      -1,   243,    -1,    -1,  1030,   247,   152,   249,   154,   977,
      -1,   157,   980,    -1,    -1,   983,    -1,  1043,   154,    -1,
      -1,   263,   990,   154,   892,   993,  1052,    -1,    -1,  1055,
      -1,    -1,  1131,  1132,   902,    -1,   904,   102,    -1,    -1,
     908,   154,  1213,   917,    -1,    -1,    -1,  1218,  1219,  1075,
      59,    60,    61,    62,  1166,  1167,  1168,    -1,  1182,    -1,
     934,    52,    -1,    54,    55,    56,    57,    -1,    -1,    -1,
      -1,  1058,   314,   315,   316,   317,   318,   319,   320,   321,
      -1,    -1,    52,  1070,    54,    55,    56,    57,   956,   957,
    1077,  1078,    -1,   961,   962,    54,    55,  1123,    57,   341,
      -1,    -1,   970,    -1,    -1,    64,    65,   975,   976,    -1,
      -1,  1016,  1099,  1018,    -1,    -1,    -1,    -1,  1023,    -1,
    1146,  1292,    -1,   991,    -1,  1030,    -1,    -1,    52,    -1,
      54,    55,    56,    57,    58,    -1,   378,  1105,   380,    -1,
    1166,  1167,  1168,   385,   386,   387,    40,    41,    42,    43,
      44,    -1,  1251,  1252,    78,  1267,    -1,    -1,  1257,   401,
    1259,  1260,    78,  1031,    -1,    -1,  1034,    -1,  1201,    -1,
      -1,  1039,    -1,  1206,    -1,  1208,  1050,  1210,   102,    95,
      96,    -1,   783,   784,   108,   109,   110,     2,    -1,    -1,
      -1,    -1,    -1,  1067,    -1,    -1,  1064,  1296,  1297,  1298,
    1299,    16,    17,  1077,  1078,   447,    -1,    -1,    -1,  1308,
      -1,    -1,    -1,    -1,    -1,  1241,    -1,  1085,   142,  1206,
      -1,   145,  1090,    -1,    -1,  1099,  1094,   143,   144,   145,
     146,   147,    -1,    -1,    -1,  1103,    -1,    -1,    53,    54,
     482,  1267,    -1,    -1,  1112,    -1,    -1,  1280,    -1,  1282,
      -1,    -1,    -1,    68,  1287,    -1,  1289,    -1,    -1,  1127,
     502,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1304,    -1,    -1,    -1,    -1,  1244,    -1,    93,    94,
      -1,    -1,    97,    -1,    -1,    -1,    -1,   102,  1193,    -1,
     786,   787,    -1,    -1,    -1,    -1,    -1,   793,   794,    -1,
      -1,    -1,    -1,  1208,    -1,  1210,    -1,   783,   784,  1177,
      -1,  1179,   913,   914,    -1,    -1,   917,    -1,    -1,    -1,
    1188,    -1,  1190,    -1,   566,   567,    -1,   569,   570,    -1,
      -1,    -1,  1200,   934,    -1,    -1,   578,    -1,    -1,    -1,
      -1,    -1,   584,    52,    -1,    54,    55,    56,    57,    58,
     846,   847,    -1,   849,   850,    -1,    -1,    -1,    -1,    -1,
    1228,    -1,    -1,   783,   784,    -1,  1234,  1235,    -1,    78,
      -1,    -1,    -1,    -1,    -1,  1280,    -1,  1282,    -1,    -1,
      -1,    -1,    -1,    92,  1289,    -1,    -1,    -1,   630,    -1,
     632,    -1,    -1,   102,    -1,    -1,    -1,   212,   640,  1304,
     109,   110,    -1,    -1,  1272,    -1,    -1,    -1,   650,   905,
     652,   653,    -1,    -1,    -1,    -1,    -1,    -1,   660,   661,
     235,    -1,   237,   238,    -1,    -1,   241,    -1,   243,    -1,
      -1,    -1,   247,   142,   249,    -1,     2,   913,   914,    -1,
      -1,   917,    -1,    -1,    78,    -1,   688,    -1,   263,  1050,
      -1,    -1,  1053,    -1,    -1,    -1,    -1,    -1,   934,    -1,
      -1,    95,    96,    -1,   960,    -1,  1067,  1068,  1069,    -1,
      -1,  1072,  1073,    -1,    -1,    -1,  1077,  1078,    78,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    53,    54,    -1,
      -1,    57,    -1,   913,   914,    95,    96,   917,  1099,   314,
     315,   316,   317,   318,   319,   320,   321,  1003,   142,   143,
     144,   145,   146,   147,   934,    -1,    -1,    -1,    -1,    85,
      -1,    -1,    -1,    -1,    -1,    -1,   341,    -1,    -1,    -1,
      -1,    -1,    98,    99,   100,    -1,    -1,    -1,    -1,    -1,
     140,   141,   142,   143,   144,   145,   146,   147,    -1,    52,
      -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   378,    -1,   380,    -1,    -1,    -1,    -1,
     385,   386,   387,    -1,  1050,    78,    -1,  1053,    -1,    -1,
      -1,   823,    -1,   825,   826,    -1,   401,    -1,    -1,    -1,
      -1,  1067,  1068,  1069,    -1,    -1,  1072,  1073,    -1,   102,
      -1,  1077,  1078,    -1,    -1,   108,   109,   110,    -1,    -1,
     783,   784,  1213,    -1,    -1,    -1,    -1,  1218,  1219,    -1,
      -1,    -1,    -1,  1099,    -1,    -1,    -1,    -1,    -1,    -1,
    1050,    -1,   447,  1053,    -1,    -1,    -1,    -1,    -1,   142,
      -1,    -1,   145,    -1,    -1,    -1,   212,  1067,  1068,  1069,
     892,    -1,  1072,  1073,    -1,    -1,    -1,  1077,  1078,   162,
     902,    -1,   904,    -1,    -1,    -1,   908,   482,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1099,
      -1,   247,    -1,   249,    -1,    -1,    -1,   502,    -1,    -1,
      -1,  1292,    -1,    -1,    -1,    -1,    -1,   263,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   956,   957,    -1,    -1,   284,   961,
     962,    52,    -1,    54,    55,    56,    57,    58,   970,    -1,
       2,    -1,    -1,   975,   976,    -1,    -1,  1213,    -1,    -1,
     913,   914,  1218,  1219,   917,    -1,    -1,    78,   314,   991,
      -1,   566,   567,   319,   569,   570,    -1,    -1,    -1,    -1,
      -1,   934,    -1,   578,    -1,    -1,    -1,    -1,    -1,   584,
      -1,   102,    -1,    -1,    -1,   341,   342,    -1,   109,   110,
      -1,    53,    54,    -1,    -1,    57,    -1,    -1,    -1,  1031,
     783,   784,  1034,  1213,    -1,    -1,    -1,  1039,  1218,  1219,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,    85,   380,   630,  1292,   632,    -1,   385,
      -1,   387,  1064,    -1,    -1,   640,    98,    99,   100,    -1,
      -1,    -1,   783,   784,    -1,   650,    -1,   652,   653,    -1,
      -1,    -1,    -1,  1085,    -1,   660,   661,    -1,    -1,    -1,
      -1,    -1,  1094,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1103,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1112,    -1,  1292,   688,    -1,    -1,    -1,  1050,    -1,    -1,
    1053,    -1,    -1,   783,   784,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1067,  1068,  1069,    -1,    -1,  1072,
    1073,    -1,    -1,    -1,  1077,  1078,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   482,    -1,    -1,    -1,
     913,   914,    -1,    -1,   917,    -1,  1099,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   500,  1177,    -1,  1179,    -1,    -1,
     212,   934,    -1,    -1,    -1,    -1,  1188,    -1,  1190,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   913,   914,    -1,    -1,   917,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   247,    -1,   249,    -1,    -1,
      -1,    -1,    -1,   934,    -1,    -1,  1228,    -1,    -1,    -1,
      -1,   263,  1234,  1235,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   567,    -1,    -1,   570,    -1,    -1,    -1,   823,    -1,
     825,   826,   284,   913,   914,   581,    -1,   917,   584,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1272,    -1,    -1,    -1,   934,    -1,    -1,    -1,    -1,    -1,
    1213,    -1,   314,    -1,    -1,  1218,  1219,   319,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1050,   783,   784,
    1053,    -1,    -1,    -1,   630,    -1,   632,    -1,    -1,   341,
     342,    -1,    -1,    -1,  1067,  1068,  1069,   892,    -1,  1072,
    1073,    -1,    -1,    -1,  1077,  1078,   652,   902,    -1,   904,
      -1,    -1,    -1,   908,   660,   661,    -1,    -1,    -1,  1050,
      -1,    -1,  1053,    -1,    -1,    -1,  1099,    -1,   380,    -1,
      -1,    -1,    -1,   385,    -1,   387,  1067,  1068,  1069,  1292,
      -1,  1072,  1073,    -1,    -1,    -1,  1077,  1078,    -1,    -1,
      -1,    -1,    -1,    -1,   700,    -1,    -1,    -1,    -1,    -1,
      -1,   956,   957,   709,    -1,    -1,   961,   962,  1099,    -1,
    1050,    -1,    -1,  1053,    -1,   970,    -1,    -1,    -1,    -1,
     975,   976,    -1,    -1,    -1,    -1,    -1,  1067,  1068,  1069,
      -1,    -1,  1072,  1073,    -1,    -1,   991,  1077,  1078,    -1,
      -1,    52,    -1,    54,    55,    56,    57,    58,   913,   914,
      -1,    -1,   917,     2,    -1,    -1,    -1,    -1,    -1,  1099,
      -1,    -1,    -1,    -1,    -1,    -1,   772,    78,    -1,   934,
     482,    -1,    -1,    -1,    -1,    -1,  1031,    -1,    -1,  1034,
    1213,    92,    -1,    -1,  1039,  1218,  1219,    -1,   500,    -1,
      -1,   102,    -1,    -1,    -1,    -1,    -1,   108,   109,   110,
      -1,    -1,    -1,    -1,    53,    54,    -1,    -1,    57,  1064,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   823,    -1,    -1,
     826,    -1,  1213,    -1,    -1,    -1,    -1,  1218,  1219,    -1,
    1085,   142,    -1,    -1,   145,    -1,    85,    -1,    -1,  1094,
      -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,  1103,    98,
      99,   100,    -1,    -1,    -1,   567,    -1,  1112,   570,  1292,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   581,
      -1,    -1,   584,  1213,    -1,    -1,    -1,    -1,  1218,  1219,
      -1,    -1,    -1,    -1,    -1,  1050,    -1,    -1,  1053,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   902,    -1,   904,    -1,
      -1,  1292,  1067,  1068,  1069,    -1,    -1,  1072,  1073,    -1,
      -1,    -1,  1077,  1078,    -1,    -1,    -1,    -1,   630,    -1,
     632,    -1,  1177,    -1,  1179,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1188,  1099,  1190,    -1,    -1,    -1,    -1,
     652,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   660,   661,
     956,   957,  1292,    -1,    -1,   961,   962,    -1,    -1,    -1,
      -1,    -1,    -1,   212,   970,    -1,    -1,    -1,    -1,   975,
      -1,    -1,    -1,  1228,    -1,     2,    -1,    -1,    -1,  1234,
    1235,    -1,    -1,    -1,    -1,   991,    -1,    -1,   700,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   247,    -1,
     249,    -1,    -1,    -1,    -1,    52,    -1,    54,    55,    56,
      57,    58,    -1,    -1,   263,    -1,    -1,  1272,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1031,    53,    54,  1034,    -1,
      57,    78,    -1,  1039,    -1,   284,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,  1213,    -1,
      -1,    -1,    -1,  1218,  1219,   102,    -1,    -1,    85,    -1,
     772,   108,   109,   110,    -1,   314,    -1,    -1,    -1,    -1,
     319,    98,    99,   100,   101,    -1,    -1,    -1,    -1,  1085,
      -1,    -1,    -1,    -1,  1090,    -1,    -1,    -1,  1094,    -1,
      -1,    -1,   341,   342,    -1,   142,    -1,  1103,   145,    -1,
      -1,     2,    -1,    -1,    -1,    -1,  1112,    -1,    -1,    -1,
      -1,   823,    -1,    -1,   826,    -1,    -1,    -1,    -1,    -1,
       0,  1127,    -1,    -1,    -1,    -1,    -1,  1292,     8,     9,
      10,   380,    -1,    13,    14,    15,   385,    17,   387,    -1,
      -1,    -1,    -1,    -1,    -1,    25,    26,    27,    -1,    -1,
      -1,    -1,    53,    54,    -1,    -1,    -1,    37,    38,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,
      -1,  1177,    -1,  1179,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1188,    -1,  1190,   212,    -1,    -1,    68,    69,
     902,    -1,   904,    -1,  1200,    -1,    -1,    98,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    97,    98,    -1,
     247,    -1,   249,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   482,    -1,    -1,   263,    -1,    -1,    -1,
      -1,   121,    -1,    -1,   956,   957,    -1,    -1,    -1,   961,
     962,   500,    -1,    -1,    -1,    -1,    -1,   284,   970,    -1,
      -1,    -1,    -1,   975,    -1,    -1,  1272,    -1,    -1,    -1,
      -1,    -1,   152,   153,    -1,    -1,    -1,   157,   158,   991,
     160,    -1,   162,    -1,    -1,    -1,    -1,   314,    -1,    -1,
      -1,    -1,   319,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     2,    -1,    -1,    -1,    -1,    -1,
      -1,   212,    -1,    -1,   341,   342,    -1,    -1,   567,  1031,
      -1,   570,  1034,    -1,    -1,    -1,    -1,  1039,    -1,    -1,
      -1,    -1,   581,    -1,    -1,   584,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   247,    -1,   249,    -1,
      -1,    -1,    -1,   380,    -1,    53,    54,    -1,   385,    -1,
     387,    -1,   263,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1085,    -1,    -1,    -1,    -1,  1090,    -1,
      -1,   630,  1094,   632,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1103,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1112,    -1,    -1,   652,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   660,   661,   314,    -1,  1127,    -1,    -1,   319,    33,
      34,    35,    36,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    49,    50,    51,    -1,    -1,
     341,    -1,    -1,    -1,    -1,    59,    60,    61,    62,    63,
      -1,   700,    -1,    -1,    -1,   482,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1177,    -1,  1179,    -1,    -1,
      -1,    -1,    -1,   500,    -1,    -1,  1188,    -1,  1190,   380,
      -1,    -1,    -1,    -1,   385,    -1,   387,    -1,  1200,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   111,   112,   113,
     114,   115,   116,   117,   118,   119,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   212,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   772,    -1,    -1,    -1,    -1,    -1,    33,
      34,    35,    36,    -1,   148,    -1,    -1,    -1,    -1,    -1,
     567,    -1,    -1,   570,    -1,    49,    50,    51,    52,   247,
      -1,   249,    56,    -1,   581,    59,    60,    61,    62,    63,
    1272,    -1,    -1,    -1,    -1,   263,    78,    79,    80,    81,
      82,    83,    84,    85,   823,    87,    88,   826,    -1,    -1,
      -1,   482,    -1,    95,    96,    -1,    -1,    91,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,   102,    -1,
      -1,   105,   106,   630,   108,   632,    -1,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   314,    -1,    -1,    -1,
      -1,   319,    -1,    -1,    -1,   652,   138,   139,   140,   141,
     142,   143,   144,   145,   146,   147,    -1,   141,    -1,    -1,
      -1,    -1,    -1,   341,   148,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   902,    -1,   904,    -1,    -1,   162,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   567,    -1,    -1,   570,
      -1,    -1,    -1,   700,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   380,   584,    -1,    -1,    -1,   385,    -1,   387,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   956,   957,    -1,
      25,    -1,   961,   962,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   970,    -1,    -1,    -1,    -1,   975,    -1,    -1,   630,
      -1,   632,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   991,    -1,    -1,   772,    -1,    -1,    -1,    -1,
      -1,   652,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   660,
     661,    -1,    -1,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    -1,    -1,    -1,    -1,
      95,    96,  1031,    -1,   482,  1034,   101,    -1,    -1,    -1,
    1039,    -1,    -1,    -1,    -1,    -1,   823,    -1,    -1,   826,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   136,    -1,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,    -1,    -1,    -1,  1085,    -1,    -1,    -1,
      -1,  1090,    -1,    -1,    -1,  1094,    -1,    78,    79,    80,
      81,    82,    83,    84,  1103,    -1,    87,    88,    -1,    -1,
      -1,    -1,    -1,  1112,    95,    96,    -1,    -1,    -1,   567,
      -1,    -1,   570,    -1,    -1,   902,    -1,   904,  1127,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   584,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   138,   139,   140,
     141,   142,   143,   144,   145,   146,   147,    -1,    -1,    -1,
      -1,    -1,   823,    -1,    -1,   826,    -1,    -1,  1177,    -1,
    1179,    -1,   630,    -1,   632,    -1,    -1,    -1,    -1,  1188,
      -1,  1190,    -1,   970,    -1,    -1,    -1,    -1,   975,    -1,
      -1,  1200,    -1,    -1,   652,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   660,   661,   991,    -1,    -1,    33,    34,    35,
      36,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    50,    51,    52,    -1,    -1,    -1,
      56,    -1,    58,    59,    60,    61,    62,    63,    -1,    -1,
      -1,   902,    -1,   904,  1031,    -1,    -1,  1034,    -1,    -1,
      -1,    -1,    78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1272,    -1,    91,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,   102,    -1,    -1,   105,
     106,    -1,   108,   109,    -1,   111,   112,   113,   114,   115,
     116,   117,   118,   119,    -1,   956,   957,    -1,    -1,    -1,
     961,   962,    -1,  1090,    -1,    -1,    -1,  1094,    -1,   970,
      -1,    -1,    -1,    -1,   975,   141,  1103,    -1,    -1,    -1,
      -1,    -1,   148,    -1,    -1,  1112,    -1,    -1,    -1,    -1,
     991,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1127,    -1,    16,    17,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    78,    79,    80,    81,    82,    83,
      84,    -1,    -1,    87,    88,   823,    -1,    -1,   826,    -1,
    1031,    95,    96,  1034,    48,    49,    50,    51,  1039,    -1,
      -1,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1177,    -1,  1179,    -1,    68,    69,    -1,    -1,    -1,    -1,
      -1,  1188,    -1,  1190,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1200,   138,   139,   140,   141,   142,   143,
     144,   145,   146,   147,  1085,    -1,    -1,    -1,   102,  1090,
      -1,    -1,    -1,  1094,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1103,    -1,   902,    -1,   904,    -1,    -1,    -1,
      -1,  1112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1272,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   956,   957,
      -1,    -1,    -1,   961,   962,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   970,    -1,    -1,    -1,  1177,   975,  1179,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1188,    -1,  1190,
      -1,    -1,    -1,   991,    -1,    -1,   210,    -1,    -1,   213,
     214,   215,    -1,   217,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   235,    -1,   237,   238,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1031,    -1,    -1,  1034,    -1,    -1,    -1,
      -1,  1039,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    -1,    -1,
      -1,  1272,    95,    96,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1085,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1094,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1103,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   136,  1112,   138,   139,   140,   141,   142,
     143,   144,   145,   146,   147,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   346,   347,   348,   349,   350,    -1,    -1,   353,
     354,   355,   356,   357,   358,   359,   360,    -1,   362,    -1,
      -1,   365,   366,   367,   368,   369,   370,   371,   372,   373,
     374,    -1,    -1,    -1,   378,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1177,
      -1,  1179,    33,    34,    35,    36,    -1,    -1,    -1,    -1,
    1188,    -1,  1190,    -1,    -1,    -1,    -1,    -1,    49,    50,
      51,    52,    -1,    -1,    -1,    56,    -1,    -1,    59,    60,
      61,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   442,   443,
      -1,    -1,    -1,   447,    -1,    -1,    -1,   451,    -1,    -1,
      91,    92,    -1,   457,    -1,    -1,    -1,    -1,    99,    -1,
      -1,   102,    -1,    -1,   105,   106,    -1,   108,   472,    -1,
     111,   112,   113,   114,   115,   116,   117,   118,   119,    -1,
      -1,    -1,    -1,   487,  1272,    -1,   490,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   502,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,   148,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   520,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     0,     1,    -1,     3,
       4,     5,     6,     7,    -1,    -1,    -1,    11,    12,    -1,
     554,    -1,    16,    -1,    18,    19,    20,    21,    22,    23,
      24,    -1,   566,    -1,    -1,   569,    30,    31,    32,    33,
      34,    35,    36,    -1,   578,    39,    -1,    -1,    -1,    -1,
     584,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    99,   640,    -1,   102,   103,
      -1,   105,   106,    -1,   108,    -1,   650,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   660,   661,    -1,   663,
     664,   665,   666,    -1,   668,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   677,   678,    -1,    -1,    -1,    -1,   683,
      -1,   685,    -1,    -1,   148,   149,   150,    -1,    -1,    -1,
     694,    -1,    -1,    -1,     0,    -1,   160,    -1,   162,    -1,
      -1,    -1,     8,     9,    10,    -1,    -1,    13,    14,    15,
      -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,
      26,    27,    28,    29,    -1,    -1,    -1,   731,    -1,    -1,
      -1,    37,    38,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     764,    -1,    68,    69,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    -1,    -1,    -1,   792,    95,
      96,    97,    98,    -1,   100,   101,    -1,   801,    -1,    -1,
      -1,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   816,    -1,    -1,   121,    -1,    -1,   124,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
     136,   137,   138,   139,   140,   141,   142,   143,   144,   145,
     146,   147,    -1,    -1,    -1,    -1,   152,   153,   154,   155,
      -1,    -1,   158,   159,   160,    -1,   162,    33,    34,    35,
      36,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    50,    51,    52,    -1,    -1,    -1,
      56,    -1,    -1,    59,    60,    61,    62,    63,   892,   893,
     894,    -1,    -1,    -1,    -1,    -1,    -1,   901,    -1,    -1,
      -1,    -1,   906,    -1,   908,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    91,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,   102,    -1,    -1,   105,
     106,    -1,    -1,    -1,    -1,   111,   112,   113,   114,   115,
     116,   117,   118,   119,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   955,   956,   957,    -1,    -1,    -1,   961,   962,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,
      -1,    -1,   148,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   985,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1000,  1001,  1002,    -1,
    1004,  1005,    -1,  1007,    -1,     0,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     8,     9,    10,    -1,    -1,    13,    14,
      15,    -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      25,  1035,    27,    28,    29,  1039,    -1,    -1,    -1,    -1,
      -1,    -1,    37,    38,    -1,    40,    41,    42,    43,    44,
      -1,    -1,    -1,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    -1,    -1,    -1,    -1,
      95,    96,    -1,    68,    69,    -1,    -1,    -1,    -1,    -1,
      -1,  1085,    -1,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    -1,    -1,  1102,    -1,
      95,    96,    97,    98,  1108,   100,   101,    -1,    -1,    -1,
      -1,   136,   107,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,    -1,    -1,    -1,   121,    -1,    -1,   124,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   162,    -1,    -1,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,    -1,    -1,    -1,    -1,    -1,   153,   154,
     155,    -1,     0,   158,   159,   160,    -1,   162,    -1,    -1,
       8,     9,    10,    -1,    -1,    13,    14,    15,  1182,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,    -1,    27,
      28,    29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,
      38,    -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    -1,  1228,    -1,    -1,    95,    96,    -1,
      68,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    -1,    -1,    -1,    -1,    95,    96,    97,
      98,    -1,   100,   101,    -1,    -1,    -1,    -1,    -1,   107,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
      -1,    -1,    -1,   121,    -1,    -1,   124,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
      -1,    -1,    -1,    -1,    -1,   153,   154,   155,     0,    -1,
     158,   159,   160,    -1,   162,    -1,     8,     9,    10,    -1,
      -1,    13,    14,    15,    -1,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    25,    26,    27,    28,    29,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    68,    69,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    -1,
      -1,    -1,    -1,    95,    96,    97,    98,    -1,    -1,   101,
      -1,    -1,    -1,    -1,    -1,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   121,
      -1,    -1,   124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   136,   137,   138,   139,   140,   141,
     142,   143,   144,   145,   146,   147,    -1,    -1,    -1,    -1,
     152,   153,   154,   155,     0,    -1,   158,   159,   160,    -1,
     162,    -1,     8,     9,    10,    -1,    -1,    13,    14,    15,
      -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,
      26,    27,    28,    29,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    37,    38,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    68,    69,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    -1,    -1,    -1,    -1,    95,
      96,    97,    98,    -1,    -1,   101,    -1,    -1,    -1,    -1,
      -1,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   121,    -1,    -1,   124,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     136,   137,   138,   139,   140,   141,   142,   143,   144,   145,
     146,   147,    -1,    -1,    -1,    -1,   152,   153,   154,   155,
       0,    -1,   158,   159,   160,    -1,   162,    -1,     8,     9,
      10,    -1,    -1,    13,    14,    15,    -1,    17,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    25,    -1,    27,    28,    29,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,    69,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    -1,    -1,    -1,    -1,    95,    96,    97,    98,    -1,
     100,   101,    -1,    -1,    -1,    -1,    -1,   107,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   121,    -1,    -1,   124,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   135,   136,   137,   138,   139,
     140,   141,   142,   143,   144,   145,   146,   147,    -1,    -1,
      -1,    -1,    -1,   153,   154,   155,     0,    -1,   158,   159,
     160,    -1,   162,    -1,     8,     9,    10,    -1,    -1,    13,
      14,    15,    -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    25,    26,    27,    28,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    68,    69,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    -1,    -1,    -1,
      -1,    95,    96,    97,    98,    -1,    -1,   101,    -1,    -1,
      -1,    -1,    -1,   107,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   121,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   136,    -1,   138,   139,   140,   141,   142,   143,
     144,   145,   146,   147,    -1,    -1,    -1,    -1,   152,   153,
     154,   155,     0,   157,   158,   159,   160,    -1,   162,    -1,
       8,     9,    10,    -1,    -1,    13,    14,    15,    -1,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,    -1,    27,
      28,    29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,
      38,    -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      68,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    -1,    -1,    -1,    -1,    95,    96,    97,
      98,    -1,    -1,   101,    -1,    -1,    -1,    -1,    -1,   107,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   121,    -1,    -1,   124,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   136,   137,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
      -1,    -1,    -1,    -1,    -1,   153,   154,   155,     0,    -1,
     158,   159,   160,    -1,   162,    -1,     8,     9,    10,    -1,
      -1,    13,    14,    15,    -1,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    25,    26,    27,    28,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    68,    69,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    -1,
      -1,    -1,    -1,    95,    96,    97,    98,    -1,    -1,   101,
      -1,    -1,    -1,    -1,    -1,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   121,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   136,    -1,   138,   139,   140,   141,
     142,   143,   144,   145,   146,   147,    -1,    -1,    -1,    -1,
     152,   153,   154,   155,     0,   157,   158,   159,   160,    -1,
     162,    -1,     8,     9,    10,    -1,    -1,    13,    14,    15,
      -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,
      -1,    27,    28,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    37,    38,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    68,    69,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    -1,    -1,    -1,    -1,    95,
      96,    97,    98,    -1,    -1,   101,    -1,    -1,    -1,    -1,
      -1,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   121,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     136,    -1,   138,   139,   140,   141,   142,   143,   144,   145,
     146,   147,    -1,    -1,    -1,    -1,    -1,   153,   154,   155,
       0,   157,   158,   159,   160,    -1,   162,    -1,     8,     9,
      10,    44,    -1,    -1,    14,    15,    -1,    17,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    25,    26,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,
      40,    41,    42,    43,    44,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    -1,    -1,
      -1,    -1,    95,    96,    -1,    -1,    -1,    -1,    68,    69,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    -1,    -1,    -1,    -1,    95,    96,    97,    98,    -1,
     100,   101,    -1,   136,    -1,   138,   139,   140,   141,   142,
     143,   144,   145,   146,   147,    -1,    -1,    -1,    -1,    -1,
      -1,   121,   155,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   135,   136,    -1,   138,   139,
     140,   141,   142,   143,   144,   145,   146,   147,    -1,    -1,
      -1,    -1,   152,   153,   154,   155,     0,    -1,   158,    -1,
     160,    -1,   162,    -1,     8,     9,    10,    44,    -1,    -1,
      14,    15,    -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    25,    26,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,    43,
      44,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    -1,    -1,    -1,    -1,    95,    96,
      -1,    -1,    -1,    -1,    68,    69,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    -1,    -1,    -1,
      -1,    95,    96,    97,    98,    -1,   100,   101,    -1,   136,
      -1,   138,   139,   140,   141,   142,   143,   144,   145,   146,
     147,    -1,    -1,    -1,    -1,    -1,    -1,   121,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,   136,    -1,   138,   139,   140,   141,   142,   143,
     144,   145,   146,   147,    -1,    -1,    -1,    -1,   152,   153,
     154,   155,     0,    -1,   158,    -1,   160,    -1,   162,    -1,
       8,     9,    10,    -1,    -1,    -1,    14,    15,    -1,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,
      38,    -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      68,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    -1,    -1,    -1,    -1,    95,    96,    97,
      98,    -1,   100,   101,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   121,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,   136,    -1,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
      -1,    -1,    -1,    -1,    -1,   153,   154,   155,     0,    -1,
     158,    -1,   160,    -1,   162,    -1,     8,     9,    10,    -1,
      -1,    -1,    14,    15,    -1,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    25,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    68,    69,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    -1,
      -1,    -1,    -1,    95,    96,    97,    98,    -1,   100,   101,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   121,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   135,   136,    -1,   138,   139,   140,   141,
     142,   143,   144,   145,   146,   147,    -1,    -1,    -1,    -1,
      -1,   153,   154,   155,    -1,    -1,   158,    -1,   160,     1,
     162,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,    -1,    15,    16,    -1,    18,    19,    20,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    68,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    76,    77,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,
     102,   103,    -1,   105,   106,    -1,   108,    -1,    -1,   111,
     112,   113,   114,   115,   116,   117,   118,   119,    -1,    -1,
      -1,     1,    -1,     3,     4,     5,     6,     7,    -1,    -1,
      10,    11,    12,    -1,    14,    15,    16,    -1,    18,    19,
      20,    21,    22,    23,    24,    -1,   148,   149,   150,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,   160,    39,
     162,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    68,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    76,    77,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    99,
      -1,    -1,   102,   103,    -1,   105,   106,    -1,   108,    -1,
      -1,   111,   112,   113,   114,   115,   116,   117,   118,   119,
      -1,    -1,    -1,     1,    -1,     3,     4,     5,     6,     7,
      -1,    -1,    10,    11,    12,    -1,    -1,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    -1,   148,   149,
     150,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
     160,    39,   162,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    77,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,   102,   103,    -1,   105,   106,    -1,
     108,    -1,    -1,   111,   112,   113,   114,   115,   116,   117,
     118,   119,    -1,    -1,    -1,     1,    -1,     3,     4,     5,
       6,     7,    -1,    -1,    10,    11,    12,    -1,    -1,    15,
      16,    -1,    18,    19,    20,    21,    22,    23,    24,    25,
     148,   149,   150,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,   160,    39,   162,    -1,    -1,    -1,    -1,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    91,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,   102,   103,    -1,   105,
     106,    -1,   108,    -1,    -1,   111,   112,   113,   114,   115,
     116,   117,   118,   119,    -1,    -1,    -1,     1,    -1,     3,
       4,     5,     6,     7,    -1,    -1,    10,    11,    12,    -1,
      -1,    15,    16,    -1,    18,    19,    20,    21,    22,    23,
      24,    -1,   148,   149,   150,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,   160,    39,   162,    -1,    -1,    -1,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    68,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,   102,   103,
      -1,   105,   106,    -1,   108,    -1,    -1,   111,   112,   113,
     114,   115,   116,   117,   118,   119,    -1,    -1,    -1,     1,
      -1,     3,     4,     5,     6,     7,    -1,     9,    10,    11,
      12,    -1,    -1,    -1,    16,    -1,    18,    19,    20,    21,
      22,    23,    24,    -1,   148,   149,   150,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,   160,    39,   162,    -1,
      -1,    -1,    -1,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    68,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    76,    77,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,
     102,   103,    -1,   105,   106,    -1,   108,    -1,    -1,   111,
     112,   113,   114,   115,   116,   117,   118,   119,    -1,    -1,
      -1,     1,    -1,     3,     4,     5,     6,     7,    -1,    -1,
      10,    11,    12,    -1,    -1,    -1,    16,    -1,    18,    19,
      20,    21,    22,    23,    24,    -1,   148,   149,   150,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,   160,    39,
     162,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    68,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    76,    77,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    99,
      -1,    -1,   102,   103,    -1,   105,   106,    -1,   108,    -1,
      -1,   111,   112,   113,   114,   115,   116,   117,   118,   119,
      -1,    -1,    -1,     1,    -1,     3,     4,     5,     6,     7,
      -1,    -1,    -1,    11,    12,    -1,    -1,    -1,    16,    -1,
      18,    19,    20,    21,    22,    23,    24,    -1,   148,   149,
     150,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
     160,    39,   162,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    77,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,   102,   103,    -1,   105,   106,    -1,
     108,    -1,    -1,   111,   112,   113,   114,   115,   116,   117,
     118,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     148,   149,   150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     158,    -1,   160,     1,   162,     3,     4,     5,     6,     7,
      -1,    -1,    -1,    11,    12,    -1,    -1,    -1,    16,    -1,
      18,    19,    20,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    77,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,   102,   103,    -1,   105,   106,    -1,
     108,    -1,    -1,   111,   112,   113,   114,   115,   116,   117,
     118,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     148,   149,   150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     158,    -1,   160,     1,   162,     3,     4,     5,     6,     7,
      -1,    -1,    -1,    11,    12,    -1,    -1,    -1,    16,    -1,
      18,    19,    20,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    77,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,   102,   103,    -1,   105,   106,    -1,
     108,    -1,    -1,   111,   112,   113,   114,   115,   116,   117,
     118,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     148,   149,   150,    -1,    -1,   153,    -1,    -1,    -1,    -1,
      -1,    -1,   160,     1,   162,     3,     4,     5,     6,     7,
      -1,    -1,    -1,    11,    12,    -1,    -1,    -1,    16,    -1,
      18,    19,    20,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    77,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,   102,   103,    -1,   105,   106,    -1,
     108,    -1,    -1,   111,   112,   113,   114,   115,   116,   117,
     118,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     148,   149,   150,    -1,    -1,   153,    -1,    -1,    -1,    -1,
      -1,    -1,   160,     1,   162,     3,     4,     5,     6,     7,
      -1,    -1,    -1,    11,    12,    -1,    -1,    -1,    16,    -1,
      18,    19,    20,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    77,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,   102,   103,    -1,   105,   106,    -1,
     108,    -1,    -1,   111,   112,   113,   114,   115,   116,   117,
     118,   119,    -1,   121,    -1,     1,    -1,     3,     4,     5,
       6,     7,    -1,    -1,    -1,    11,    12,    -1,    -1,    -1,
      16,    -1,    18,    19,    20,    21,    22,    23,    24,    -1,
     148,   149,   150,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,   160,    39,   162,    -1,    -1,    -1,    -1,    45,
      46,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    91,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,   102,   103,    -1,   105,
     106,    -1,   108,    -1,    -1,   111,   112,   113,   114,   115,
     116,   117,   118,   119,    -1,    -1,     1,    -1,     3,     4,
       5,    -1,     7,    -1,    -1,    -1,    11,    12,    -1,    -1,
      -1,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,   148,   149,   150,    30,    31,    32,    33,    34,
      35,    36,   158,    -1,    39,    -1,   162,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    99,    -1,    -1,   102,   103,    -1,
     105,   106,    -1,    -1,    -1,    -1,   111,   112,   113,   114,
     115,   116,   117,   118,   119,    -1,    -1,     0,     1,    -1,
       3,     4,     5,     6,     7,    -1,    -1,    -1,    11,    12,
      -1,    -1,    -1,    16,    -1,    18,    19,    20,    21,    22,
      23,    24,    -1,   148,   149,   150,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,   160,    39,   162,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    76,    77,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,   102,
     103,    -1,   105,   106,    -1,   108,    -1,    -1,   111,   112,
     113,   114,   115,   116,   117,   118,   119,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   148,   149,   150,    -1,    -1,
     153,    -1,    -1,    -1,    -1,    -1,     1,   160,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    -1,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    68,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    99,    -1,    -1,   102,   103,    -1,
     105,   106,    -1,   108,    -1,    -1,   111,   112,   113,   114,
     115,   116,   117,   118,   119,    -1,   121,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   148,   149,   150,    -1,    -1,   153,    -1,
       3,     4,     5,   158,     7,   160,    -1,    -1,    11,    12,
      -1,    -1,    -1,    16,    -1,    18,    19,    20,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    76,    77,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,   102,
     103,    -1,   105,   106,    -1,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,    -1,    -1,    -1,
      -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,
      12,    -1,    -1,    -1,    16,    -1,    18,    19,    20,    21,
      22,    23,    24,    -1,    -1,   148,   149,   150,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,   162,
      -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    76,    77,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,
     102,   103,    -1,   105,   106,    -1,    -1,    -1,    -1,   111,
     112,   113,   114,   115,   116,   117,   118,   119,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,   148,   149,   150,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    -1,
     162,    -1,    -1,    -1,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    -1,    -1,    87,    88,    -1,    -1,
      -1,    -1,    93,    94,    95,    96,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   138,   139,   140,
     141,   142,   143,   144,   145,   146,   147,    -1,   149,   150,
      -1,    -1,    -1,    -1,    -1,   156,   157,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    -1,
      -1,    87,    88,    -1,    -1,    -1,    -1,    93,    94,    95,
      96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   138,   139,   140,   141,   142,   143,   144,   145,
     146,   147,    -1,   149,   150,    -1,    -1,    -1,    -1,    -1,
     156,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    -1,    -1,
      -1,    -1,    -1,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    -1,    -1,    87,    88,    -1,    -1,    -1,
      -1,    93,    94,    95,    96,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,
     112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   138,   139,   140,   141,
     142,   143,   144,   145,   146,   147,    -1,   149,   150,    -1,
      -1,    -1,    -1,    -1,   156,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    -1,    -1,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    -1,    -1,    87,
      88,    -1,    -1,    -1,    -1,    93,    94,    95,    96,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     108,   109,    -1,    -1,   112,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
      -1,   149,   150,    -1,    -1,    -1,    -1,    -1,   156,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    -1,    -1,    87,    88,    -1,    -1,    -1,    -1,    93,
      94,    95,    96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   138,   139,   140,   141,   142,   143,
     144,   145,   146,   147,    -1,   149,   150,     3,     4,     5,
      -1,     7,   156,    -1,    -1,    11,    12,    -1,    -1,    -1,
      16,    -1,    18,    19,    20,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      46,    -1,    -1,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,   102,   103,    -1,   105,
     106,    -1,    -1,    -1,    -1,   111,   112,   113,   114,   115,
     116,   117,   118,   119,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,
      -1,    -1,   148,    11,    12,    -1,    -1,    -1,    16,   155,
      18,    19,    20,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,
      -1,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,   102,   103,    -1,   105,   106,    -1,
      -1,    -1,    -1,   111,   112,   113,   114,   115,   116,   117,
     118,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    -1,     3,     4,     5,     6,     7,    -1,    -1,
     148,    11,    12,    -1,    -1,    -1,    16,   155,    18,    19,
      20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    76,    77,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    99,
      -1,    -1,   102,   103,    -1,   105,   106,    -1,   108,    -1,
      -1,   111,   112,   113,   114,   115,   116,   117,   118,   119,
      -1,    -1,     1,    -1,     3,     4,     5,    -1,     7,    -1,
      -1,    -1,    11,    12,    -1,    -1,    -1,    16,    -1,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,   148,   149,
     150,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    77,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,
      99,    -1,    -1,   102,   103,    -1,   105,   106,    -1,    -1,
      -1,    -1,   111,   112,   113,   114,   115,   116,   117,   118,
     119,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,
      -1,    -1,    -1,    11,    12,    -1,    -1,    -1,    16,    -1,
      18,    19,    20,    21,    22,    23,    24,    -1,    -1,   148,
     149,   150,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,
      -1,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    77,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,   102,   103,    -1,   105,   106,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,
       7,    -1,    -1,    -1,    11,    12,    -1,    -1,    -1,    16,
      -1,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
     148,   149,   150,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,
      -1,    -1,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,
      77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    91,    92,    -1,    -1,    -1,    -1,
      -1,    -1,    99,    -1,    -1,   102,   103,    -1,   105,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,    -1,    -1,    -1,    -1,     3,     4,     5,
      -1,     7,    -1,    -1,    -1,    11,    12,    -1,    -1,    -1,
      16,    -1,    18,    19,    20,    21,    22,    23,    24,    -1,
      -1,   148,   149,   150,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      46,    -1,    -1,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    91,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,   102,   103,    -1,   105,
     106,    -1,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,    -1,    -1,    -1,    -1,     3,     4,
       5,    -1,     7,    -1,    -1,    -1,    11,    12,    -1,    -1,
      -1,    16,    -1,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,   148,   149,   150,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    99,    -1,    -1,   102,   103,    -1,
     105,   106,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,    -1,    -1,    -1,    -1,     3,
       4,     5,    -1,     7,    -1,    -1,    -1,    11,    12,    -1,
      -1,    -1,    16,    -1,    18,    19,    20,    21,    22,    23,
      24,    -1,    -1,   148,   149,   150,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,   102,   103,
      -1,   105,   106,    -1,   108,   109,    -1,   111,   112,   113,
     114,   115,   116,   117,   118,   119,    -1,    -1,    -1,    -1,
       3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,    12,
      -1,    -1,    -1,    16,    -1,    18,    19,    20,    21,    22,
      23,    24,    -1,    -1,   148,   149,   150,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    76,    77,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,   102,
     103,    -1,   105,   106,    -1,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,    -1,    -1,    -1,
      -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,
      12,    -1,    -1,    -1,    16,    -1,    18,    19,    20,    21,
      22,    23,    24,    -1,    -1,   148,   149,   150,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    76,    77,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,
     102,   103,    -1,   105,   106,    -1,   108,   109,    -1,   111,
     112,   113,   114,   115,   116,   117,   118,   119,    -1,    -1,
      -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,
      11,    12,    -1,    -1,    -1,    16,    -1,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,   148,   149,   150,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    76,    77,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,
      -1,   102,   103,    -1,   105,   106,    -1,    -1,   109,    -1,
     111,   112,   113,   114,   115,   116,   117,   118,   119,    -1,
      -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,
      -1,    11,    12,    -1,    -1,    -1,    16,    -1,    18,    19,
      20,    21,    22,    23,    24,    -1,    -1,   148,   149,   150,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    76,    77,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    99,
      -1,    -1,   102,   103,    -1,   105,   106,    -1,   108,    -1,
      -1,   111,   112,   113,   114,   115,   116,   117,   118,   119,
      -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,
      -1,    -1,    11,    12,    -1,    -1,    -1,    16,    -1,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,   148,   149,
     150,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    77,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,
      99,    -1,    -1,   102,   103,    -1,   105,   106,    -1,   108,
      -1,    -1,   111,   112,   113,   114,   115,   116,   117,   118,
     119,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,
      -1,    -1,    -1,    11,    12,    -1,    -1,    -1,    16,    -1,
      18,    19,    20,    21,    22,    23,    24,    -1,    -1,   148,
     149,   150,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,
      -1,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    77,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,   102,   103,    -1,   105,   106,    -1,
     108,    -1,    -1,   111,   112,   113,   114,   115,   116,   117,
     118,   119,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,
       7,    -1,    -1,    -1,    11,    12,    -1,    -1,    -1,    16,
      -1,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
     148,   149,   150,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,
      -1,    -1,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,
      77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    91,    92,    -1,    -1,    -1,    -1,
      -1,    -1,    99,    -1,    -1,   102,   103,    -1,   105,   106,
      -1,   108,    -1,    -1,   111,   112,   113,   114,   115,   116,
     117,   118,   119,    -1,    -1,    -1,    -1,     3,     4,     5,
      -1,     7,    -1,    -1,    -1,    11,    12,    -1,    -1,    -1,
      16,    -1,    18,    19,    20,    21,    22,    23,    24,    -1,
      -1,   148,   149,   150,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      46,    -1,    -1,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    91,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,   102,   103,    -1,   105,
     106,    -1,   108,    -1,    -1,   111,   112,   113,   114,   115,
     116,   117,   118,   119,    -1,    -1,    -1,    -1,     3,     4,
       5,    -1,     7,    -1,    -1,    -1,    11,    12,    -1,    -1,
      -1,    16,    -1,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,   148,   149,   150,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    99,    -1,    -1,   102,   103,    -1,
     105,   106,    -1,    -1,    -1,    -1,   111,   112,   113,   114,
     115,   116,   117,   118,   119,    -1,    -1,    -1,    -1,     3,
       4,     5,    -1,     7,    -1,    -1,    -1,    11,    12,    -1,
      -1,    -1,    16,    -1,    18,    19,    20,    21,    22,    23,
      24,    -1,    -1,   148,   149,   150,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,   102,   103,
      -1,   105,   106,    -1,    -1,    -1,    -1,   111,   112,   113,
     114,   115,   116,   117,   118,   119,    -1,    -1,    -1,    -1,
       3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,    12,
      -1,    -1,    -1,    16,    -1,    18,    19,    20,    21,    22,
      23,    24,    -1,    -1,   148,   149,   150,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    76,    77,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,   102,
     103,    -1,   105,   106,    -1,    -1,    -1,    -1,   111,   112,
     113,   114,   115,   116,   117,   118,   119,    -1,    -1,    -1,
      -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,
      12,    -1,    -1,    -1,    16,    -1,    18,    19,    20,    21,
      22,    23,    24,    -1,    -1,   148,   149,   150,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    76,    77,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,
     102,   103,    -1,   105,   106,    -1,    -1,    -1,    -1,   111,
     112,   113,   114,   115,   116,   117,   118,   119,    -1,    -1,
      -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,
      11,    12,    -1,    -1,    -1,    16,    -1,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,   148,   149,   150,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    76,    77,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,
      -1,   102,   103,    -1,   105,   106,    -1,    -1,    -1,    -1,
     111,   112,   113,   114,   115,   116,   117,   118,   119,    -1,
      -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,
      -1,    11,    12,    -1,    -1,    -1,    16,    -1,    18,    19,
      20,    21,    22,    23,    24,    -1,    -1,   148,   149,   150,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,
      -1,    -1,   102,   103,    -1,   105,   106,    -1,   108,    -1,
      -1,   111,   112,   113,   114,   115,   116,   117,   118,   119,
      -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,
      11,    12,    -1,    -1,    -1,    16,    -1,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,   148,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,
      -1,   102,   103,    -1,   105,   106,    -1,   108,    -1,    -1,
     111,   112,   113,   114,   115,   116,   117,   118,   119,    -1,
      -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,
      12,    -1,    -1,    -1,    16,    -1,    18,    19,    20,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,   148,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    95,    -1,    -1,    -1,    99,    -1,    -1,
     102,   103,    -1,   105,   106,    -1,    -1,    -1,    -1,   111,
     112,   113,   114,   115,   116,   117,   118,   119,    -1,    -1,
       3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,    12,
      -1,    -1,    -1,    16,    -1,    18,    19,    20,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,   148,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,   102,
     103,    -1,   105,   106,    -1,    -1,    -1,    -1,   111,   112,
     113,   114,   115,   116,   117,   118,   119,    -1,    -1,     3,
       4,     5,    -1,     7,    -1,    -1,    -1,    11,    12,    -1,
      -1,    -1,    16,    -1,    18,    19,    20,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,   148,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,   102,   103,
      -1,   105,   106,    -1,    -1,    -1,    -1,   111,   112,   113,
     114,   115,   116,   117,   118,   119,    -1,    -1,     3,     4,
       5,    -1,     7,    -1,    -1,    -1,    11,    12,    -1,    -1,
      -1,    16,    -1,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,   148,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    99,    -1,    -1,   102,   103,    -1,
     105,   106,    -1,    -1,    -1,    -1,   111,   112,   113,   114,
     115,   116,   117,   118,   119,    -1,    -1,     3,     4,     5,
      -1,     7,    -1,    -1,    -1,    11,    12,    -1,    -1,    -1,
      16,    -1,    18,    19,    20,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,   148,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      46,    -1,    -1,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      52,    53,    -1,    99,    56,    -1,   102,   103,    -1,   105,
     106,    -1,    -1,    -1,    -1,   111,   112,   113,   114,   115,
     116,   117,   118,   119,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    -1,    -1,    87,    88,    -1,    -1,    -1,
      -1,    93,    94,    95,    96,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   148,    -1,    -1,    -1,   108,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   138,   139,   140,   141,
     142,   143,   144,   145,   146,   147,    -1,   149,   150,    52,
      53,    -1,    -1,    56,   156,   157,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    -1,    -1,    87,    88,    -1,    -1,    -1,    -1,
      93,    94,    95,    96,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   138,   139,   140,   141,   142,
     143,   144,   145,   146,   147,    -1,   149,   150,    52,    53,
      -1,    -1,    56,   156,   157,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    -1,    -1,    87,    88,    -1,    -1,    -1,    -1,    93,
      94,    95,    96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   138,   139,   140,   141,   142,   143,
     144,   145,   146,   147,    -1,   149,   150,    52,    53,    -1,
      -1,    56,   156,   157,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      -1,    -1,    87,    88,    -1,    -1,    -1,    -1,    93,    94,
      95,    96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,    -1,   149,   150,    52,    53,    -1,    -1,
      56,   156,   157,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    -1,
      -1,    87,    88,    -1,    -1,    -1,    -1,    93,    94,    95,
      96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   138,   139,   140,   141,   142,   143,   144,   145,
     146,   147,    -1,   149,   150,    52,    53,    -1,    -1,    56,
     156,   157,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    -1,    -1,
      87,    88,    -1,    -1,    -1,    -1,    93,    94,    95,    96,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   138,   139,   140,   141,   142,   143,   144,   145,   146,
     147,    -1,   149,   150,    52,    53,    -1,    -1,    56,   156,
     157,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    -1,    -1,    87,
      88,    -1,    -1,    -1,    -1,    93,    94,    95,    96,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
      -1,   149,   150,    52,    53,    -1,    -1,    56,   156,   157,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    -1,    -1,    87,    88,
      -1,    -1,    -1,    -1,    93,    94,    95,    96,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   138,
     139,   140,   141,   142,   143,   144,   145,   146,   147,    -1,
     149,   150,    52,    53,    -1,    -1,    56,   156,   157,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    -1,    -1,    87,    88,    -1,
      -1,    -1,    -1,    93,    94,    95,    96,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   138,   139,
     140,   141,   142,   143,   144,   145,   146,   147,    -1,   149,
     150,    52,    53,    -1,    -1,    56,   156,   157,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    -1,    -1,    87,    88,    -1,    -1,
      -1,    -1,    93,    94,    95,    96,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   138,   139,   140,
     141,   142,   143,   144,   145,   146,   147,    -1,   149,   150,
      52,    53,    -1,    -1,    56,   156,   157,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    -1,    -1,    87,    88,    -1,    -1,    -1,
      -1,    93,    94,    95,    96,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   138,   139,   140,   141,
     142,   143,   144,   145,   146,   147,    -1,   149,   150,    52,
      53,    -1,    -1,    56,   156,   157,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    -1,    -1,    87,    88,    -1,    -1,    -1,    -1,
      93,    94,    95,    96,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   138,   139,   140,   141,   142,
     143,   144,   145,   146,   147,    -1,   149,   150,    52,    53,
      -1,    -1,    56,   156,   157,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    -1,    -1,    87,    88,    -1,    -1,    -1,    -1,    93,
      94,    95,    96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   138,   139,   140,   141,   142,   143,
     144,   145,   146,   147,    -1,   149,   150,    52,    53,    -1,
      -1,    56,   156,   157,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      -1,    -1,    87,    88,    -1,    -1,    -1,    -1,    93,    94,
      95,    96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,    -1,   149,   150,    52,    53,    -1,    -1,
      56,   156,   157,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    -1,
      -1,    87,    88,    -1,    -1,    -1,    -1,    93,    94,    95,
      96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   138,   139,   140,   141,   142,   143,   144,   145,
     146,   147,    -1,   149,   150,    52,    53,    -1,    -1,    56,
     156,   157,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    -1,    -1,
      87,    88,    -1,    -1,    -1,    -1,    93,    94,    95,    96,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   138,   139,   140,   141,   142,   143,   144,   145,   146,
     147,    -1,   149,   150,    -1,    -1,    -1,    -1,    -1,   156
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   164,   165,     0,     1,     3,     4,     5,     6,     7,
      11,    12,    16,    18,    19,    20,    21,    22,    23,    24,
      30,    31,    32,    33,    34,    35,    36,    39,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    59,    60,    61,    62,    63,    64,    65,    76,    77,
      91,    92,    99,   102,   103,   105,   106,   108,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   148,   149,   150,
     166,   167,   168,   176,   178,   181,   187,   188,   194,   195,
     197,   198,   199,   201,   202,   203,   205,   206,   215,   219,
     235,   247,   248,   249,   250,   251,   252,   253,   254,   255,
     256,   257,   266,   288,   297,   298,   350,   351,   352,   353,
     354,   355,   357,   360,   362,   363,   377,   378,   380,   381,
     382,   383,   384,   385,   386,   387,   388,   426,   440,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    56,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    87,    88,    93,    94,    95,    96,   108,   109,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
     149,   150,   156,   209,   210,   211,   213,   214,   377,    39,
      58,    99,   102,   108,   109,   110,   113,   149,   187,   188,
     198,   206,   215,   221,   227,   230,   232,   247,   384,   385,
     387,   388,   424,   425,   227,   157,   228,   229,   157,   224,
     228,   157,   162,   433,    54,   210,   433,   152,   169,   152,
      21,    22,    31,    32,   197,   215,   247,   266,   215,   215,
     215,    56,    47,   102,   172,   173,   174,   176,   200,   201,
     440,   176,   237,   222,   232,   424,   440,   221,   423,   424,
     440,    46,    99,   148,   155,   187,   188,   205,   235,   247,
     384,   385,   388,   289,   209,   366,   379,   383,   366,   367,
     368,   161,   356,   356,   356,   356,   382,   194,   215,   215,
     160,   162,   432,   438,   439,    40,    41,    42,    43,    44,
      37,    38,   157,   391,   392,   393,   394,   440,   391,   393,
      26,   152,   224,   228,   258,   299,    28,   259,   296,   135,
     155,   102,   108,   202,   135,    25,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    95,
      96,   101,   136,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   218,   218,    69,    97,    98,   154,   430,
     236,     1,   181,   190,   190,   191,   192,   191,   190,   432,
     439,    99,   199,   206,   247,   271,   384,   385,   388,    52,
      56,    95,    99,   207,   208,   247,   384,   385,   388,   208,
      33,    34,    35,    36,    49,    50,    51,    52,    56,   157,
     186,   209,   386,   421,   227,    98,   430,   431,   299,   353,
     100,   100,   155,   221,    56,   221,   221,   221,   366,   391,
     391,   135,   101,   155,   231,   440,    98,   154,   430,   100,
     100,   155,   231,   227,   433,   434,   227,    92,   226,   227,
     232,   398,   424,   440,   181,   434,   181,    54,    64,    65,
     177,   157,   216,   166,   172,    98,   430,   100,   175,   200,
     158,   432,   439,   434,   238,   434,   159,   155,   433,   437,
     155,   437,   153,   437,   433,    56,   382,   202,   204,   392,
     155,    98,   154,   430,   290,    66,   120,   122,   123,   369,
     120,   120,   369,    67,   369,   161,   358,   364,   361,   365,
      78,   160,   168,   190,   190,   190,   190,   176,   181,   181,
      52,    54,    55,    56,    57,    58,    78,    92,   102,   108,
     109,   110,   142,   145,   276,   338,   395,   397,   398,   399,
     400,   401,   402,   403,   404,   405,   408,   409,   410,   411,
     412,   415,   416,   417,   418,   419,   135,   245,   397,   135,
     246,   300,   301,   107,   196,   304,   305,   304,   220,   440,
     200,   155,   205,   155,   220,   184,   215,   215,   215,   215,
     215,   215,   215,   215,   215,   215,   215,   215,   215,   182,
     215,   215,   215,   215,   215,   215,   215,   215,   215,   215,
     215,    52,    53,    56,   213,   224,   426,   427,   428,   226,
     232,    52,    53,    56,   213,   224,   427,   170,   172,    13,
     267,   438,   267,   172,   190,   172,   432,   241,    56,    98,
     154,   430,    25,   181,    52,    56,   207,   139,   389,    98,
     154,   430,   244,   422,    69,    98,   429,    52,    56,   427,
     220,   220,   212,   125,   135,   135,   220,   221,   108,   221,
     230,   424,    52,    56,   226,    52,    56,   220,   220,   425,
     434,   158,   434,   155,   434,   155,   434,   210,   239,   215,
     153,   153,   427,   427,   220,   169,   434,   174,   434,   424,
     155,   204,    52,    56,   226,    52,    56,   291,   371,   370,
     120,   359,   369,    66,   120,   120,   359,    66,   120,   215,
     102,   108,   272,   273,   274,   275,   400,   155,   420,   440,
     434,   277,   278,   155,   396,   221,   155,   420,    34,    52,
     155,   396,    52,   155,   396,    52,    39,   179,   198,   215,
     217,   170,   438,   179,   217,   170,   153,   302,   300,    10,
      68,   265,   306,   265,   108,   194,   221,   232,   233,   234,
     434,   204,   155,   178,   180,   194,   206,   215,   221,   223,
     234,   247,   388,   185,   183,   433,   100,   100,   224,   228,
     433,   435,   155,   100,   100,   224,   225,   228,   440,   265,
       8,   260,   346,   440,   172,    13,   172,   265,    27,   268,
     438,   265,    25,   240,   311,    17,   262,   309,    52,    56,
     226,    52,    56,   191,   243,   390,   242,    52,    56,   207,
     226,   170,   181,   189,   225,   228,   180,   215,   223,   180,
     223,   210,   221,    39,   221,   231,   100,   100,   435,   100,
     100,   398,   424,   181,   223,   437,   202,   435,   157,   293,
     397,   372,    54,    55,    57,   376,   388,   356,   369,   356,
     356,   356,   274,   400,   155,   434,   155,   419,   221,   135,
     395,   402,   415,   417,   405,   409,   411,   403,   412,   417,
     401,   403,   433,    44,    44,   265,   265,   303,   153,   307,
     221,   155,    44,   204,    44,   135,    44,    98,   154,   430,
      52,    56,    58,    91,    92,    99,   102,   105,   106,   108,
     113,   141,   288,   317,   318,   319,   320,   323,   328,   329,
     330,   333,   334,   335,   336,   337,   338,   339,   340,   341,
     342,   343,   344,   345,   350,   351,   354,   355,   357,   360,
     362,   363,   385,   409,   317,   137,   220,   220,   196,   159,
     100,   220,   220,   196,   221,   234,   347,   440,     9,    15,
     261,   263,   349,   440,    14,   263,   264,   269,   270,   440,
     270,   193,   312,   309,   265,   108,   221,   308,   265,   435,
     172,   438,   190,   170,   435,   265,   434,   186,   299,   296,
     433,   220,   220,   100,   220,   220,   434,   155,   434,   397,
     292,   373,   434,   272,   275,   273,   155,   396,   155,   396,
     420,   155,   396,   155,   396,   396,   179,   217,   215,   215,
     140,   283,   284,   440,   283,   108,   221,   176,   176,   220,
     215,    52,    56,   226,    52,    56,   341,   341,    56,   207,
     325,   318,   326,   327,   328,   329,   332,   435,   324,   433,
     436,    52,   366,    52,   102,   383,   101,   155,   140,   155,
     155,   318,    89,    90,    98,   154,   157,   321,   322,    52,
     215,   180,   223,   180,   223,   220,   180,   223,   180,   223,
     101,   348,   440,   172,   171,   172,   190,   265,   265,   313,
     265,   221,   155,   267,   265,   170,   438,   265,   220,   285,
     433,    29,   124,   294,   374,   155,   155,   403,   417,   403,
     403,   276,   279,   282,   285,   401,   403,   404,   406,   407,
     413,   414,   417,   419,   172,   170,   221,   435,   318,   435,
     318,   330,   332,   435,   155,   113,   333,   153,   125,   190,
     342,   326,   330,   323,   331,   332,   335,   339,   341,   341,
     207,   435,   434,   326,   329,   333,   326,   329,   333,   180,
     223,    99,   206,   247,   384,   385,   388,   267,   172,   267,
     316,   317,   108,   221,   172,   265,   158,   160,   295,   172,
     375,   273,   396,   155,   396,   396,   396,   420,   285,   140,
     277,   155,   280,   281,    99,   247,   155,   420,   155,   280,
     155,   280,   434,   155,   155,   366,   436,   434,   155,   155,
     434,   434,   434,   435,   435,   435,    56,    98,   154,   430,
     172,   349,   172,   267,    40,    41,   221,   270,   309,   310,
      52,   286,   287,   399,   170,   153,   172,   403,   140,   247,
     279,   414,   417,    56,    98,   406,   411,   403,   413,   417,
     403,   331,   331,   330,   332,    52,    56,   226,    52,    56,
     346,   269,   314,   190,   190,   155,   433,   265,   121,   396,
     155,   280,   155,   280,    52,    56,   420,   155,   280,   155,
     280,   280,   155,   435,   172,   287,   403,   417,   403,   403,
     270,   311,   315,   280,   155,   280,   280,   280,   403,   280
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int16 yyr1[] =
{
       0,   163,   165,   164,   166,   167,   167,   167,   168,   168,
     169,   171,   170,   170,   172,   173,   173,   173,   174,   175,
     174,   177,   176,   176,   176,   176,   176,   176,   176,   176,
     176,   176,   176,   176,   176,   176,   176,   176,   176,   176,
     178,   178,   178,   178,   178,   178,   178,   178,   178,   178,
     179,   179,   179,   180,   180,   180,   181,   181,   181,   181,
     181,   182,   183,   181,   184,   185,   181,   181,   186,   187,
     189,   188,   190,   190,   192,   193,   191,   194,   194,   195,
     195,   196,   197,   198,   198,   198,   198,   198,   198,   198,
     198,   198,   198,   198,   199,   199,   200,   200,   201,   201,
     201,   201,   201,   201,   201,   201,   201,   201,   202,   202,
     203,   203,   204,   204,   205,   205,   205,   205,   205,   205,
     205,   205,   205,   206,   206,   206,   206,   206,   206,   206,
     206,   206,   207,   207,   208,   208,   208,   209,   209,   209,
     209,   209,   210,   210,   211,   212,   211,   213,   213,   213,
     213,   213,   213,   213,   213,   213,   213,   213,   213,   213,
     213,   213,   213,   213,   213,   213,   213,   213,   213,   213,
     213,   213,   213,   213,   213,   213,   213,   214,   214,   214,
     214,   214,   214,   214,   214,   214,   214,   214,   214,   214,
     214,   214,   214,   214,   214,   214,   214,   214,   214,   214,
     214,   214,   214,   214,   214,   214,   214,   214,   214,   214,
     214,   214,   214,   214,   214,   214,   214,   214,   215,   215,
     215,   215,   215,   215,   215,   215,   215,   215,   215,   215,
     215,   215,   215,   215,   215,   215,   215,   215,   215,   215,
     215,   215,   215,   215,   215,   215,   215,   215,   215,   215,
     215,   215,   215,   215,   215,   215,   215,   215,   216,   215,
     215,   215,   215,   215,   217,   217,   217,   218,   218,   218,
     218,   219,   219,   220,   221,   222,   222,   222,   222,   223,
     223,   224,   224,   224,   225,   225,   226,   226,   226,   226,
     226,   227,   227,   227,   227,   227,   229,   228,   230,   230,
     231,   231,   232,   232,   232,   232,   232,   232,   233,   233,
     234,   234,   234,   235,   235,   235,   235,   235,   235,   235,
     235,   235,   235,   235,   236,   235,   237,   235,   238,   235,
     235,   235,   235,   235,   235,   235,   235,   235,   235,   239,
     235,   235,   235,   235,   235,   235,   235,   235,   235,   235,
     235,   240,   235,   241,   235,   235,   235,   242,   235,   243,
     235,   244,   235,   245,   235,   246,   235,   235,   235,   235,
     235,   247,   248,   249,   250,   251,   252,   253,   254,   255,
     256,   257,   258,   259,   260,   261,   262,   263,   264,   265,
     265,   266,   267,   267,   267,   268,   268,   269,   269,   270,
     270,   271,   271,   272,   272,   273,   273,   274,   274,   274,
     274,   274,   275,   275,   276,   276,   278,   277,   279,   279,
     279,   279,   280,   280,   281,   282,   282,   282,   282,   282,
     282,   282,   282,   282,   282,   282,   282,   282,   282,   282,
     283,   283,   284,   284,   285,   285,   286,   286,   287,   287,
     289,   290,   291,   292,   288,   293,   293,   294,   295,   294,
     296,   297,   297,   297,   297,   298,   298,   298,   298,   298,
     298,   298,   298,   298,   299,   299,   301,   302,   303,   300,
     305,   306,   307,   304,   308,   308,   308,   308,   309,   310,
     310,   312,   313,   314,   311,   315,   315,   316,   316,   316,
     317,   317,   317,   317,   317,   317,   318,   319,   319,   320,
     320,   321,   322,   323,   323,   323,   323,   323,   323,   323,
     323,   323,   323,   323,   323,   323,   324,   323,   323,   325,
     323,   326,   326,   326,   326,   326,   326,   327,   327,   328,
     328,   329,   330,   330,   331,   331,   332,   333,   333,   333,
     333,   334,   334,   335,   335,   336,   336,   337,   337,   338,
     339,   339,   340,   340,   340,   340,   340,   340,   340,   340,
     340,   340,   341,   341,   341,   341,   341,   341,   341,   341,
     341,   341,   342,   343,   343,   344,   345,   345,   345,   346,
     346,   347,   347,   347,   348,   348,   349,   349,   350,   350,
     351,   352,   352,   352,   353,   354,   355,   356,   356,   357,
     358,   358,   359,   359,   360,   361,   361,   362,   363,   364,
     364,   365,   365,   366,   366,   367,   367,   368,   368,   369,
     370,   369,   371,   372,   373,   374,   375,   369,   376,   376,
     376,   376,   377,   377,   378,   379,   379,   380,   381,   381,
     382,   382,   382,   382,   383,   383,   383,   384,   384,   384,
     385,   385,   385,   385,   385,   385,   385,   386,   386,   387,
     387,   388,   388,   390,   389,   389,   391,   391,   392,   393,
     394,   393,   395,   395,   395,   395,   395,   396,   396,   397,
     397,   397,   397,   397,   397,   397,   397,   397,   397,   397,
     397,   397,   397,   397,   398,   399,   399,   399,   399,   400,
     400,   401,   402,   402,   403,   403,   404,   405,   405,   406,
     406,   407,   407,   408,   408,   409,   409,   410,   411,   411,
     412,   413,   414,   414,   415,   415,   416,   416,   417,   417,
     418,   418,   419,   419,   420,   420,   421,   422,   421,   423,
     423,   424,   424,   425,   425,   425,   425,   425,   425,   426,
     426,   426,   427,   427,   428,   428,   428,   429,   429,   430,
     430,   431,   431,   432,   432,   433,   433,   434,   435,   436,
     437,   437,   438,   438,   439,   439,   440
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     2,     2,     1,     1,     3,     1,     2,
       3,     0,     6,     3,     2,     1,     1,     3,     1,     0,
       3,     0,     4,     3,     3,     3,     2,     3,     3,     3,
       3,     3,     4,     1,     4,     4,     6,     4,     1,     1,
       4,     4,     7,     6,     6,     6,     6,     4,     4,     4,
       1,     3,     3,     1,     3,     1,     1,     3,     3,     3,
       2,     0,     0,     5,     0,     0,     5,     1,     1,     2,
       0,     5,     1,     1,     0,     0,     4,     1,     1,     1,
       4,     3,     1,     2,     3,     4,     5,     4,     5,     2,
       2,     2,     2,     2,     1,     3,     1,     3,     1,     2,
       3,     5,     2,     4,     2,     4,     1,     3,     1,     3,
       2,     3,     1,     3,     1,     1,     4,     3,     3,     3,
       3,     2,     1,     1,     1,     4,     3,     3,     3,     3,
       2,     1,     1,     1,     2,     1,     3,     1,     1,     1,
       1,     1,     1,     1,     1,     0,     4,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     4,     4,
       7,     6,     6,     6,     6,     5,     4,     3,     3,     2,
       2,     2,     2,     3,     3,     3,     3,     3,     3,     4,
       2,     2,     3,     3,     3,     3,     1,     3,     3,     3,
       3,     3,     2,     2,     3,     3,     3,     3,     0,     4,
       6,     4,     4,     1,     1,     3,     3,     1,     1,     1,
       1,     3,     3,     1,     1,     1,     2,     4,     2,     1,
       3,     3,     5,     3,     1,     1,     1,     1,     2,     4,
       2,     1,     2,     2,     4,     1,     0,     2,     2,     1,
       2,     1,     1,     2,     1,     3,     4,     3,     1,     1,
       3,     4,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     0,     4,     0,     3,     0,     4,
       3,     3,     2,     3,     3,     1,     4,     3,     1,     0,
       6,     4,     3,     2,     1,     2,     1,     6,     6,     4,
       4,     0,     6,     0,     5,     5,     6,     0,     6,     0,
       7,     0,     5,     0,     5,     0,     5,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     2,     1,     1,     1,     5,     1,
       2,     1,     1,     1,     3,     1,     3,     1,     3,     5,
       1,     3,     2,     1,     1,     1,     0,     2,     4,     2,
       2,     1,     2,     0,     1,     6,     8,     4,     6,     4,
       2,     6,     2,     4,     6,     2,     4,     2,     4,     1,
       1,     1,     3,     4,     1,     4,     1,     3,     1,     1,
       0,     0,     0,     0,     7,     4,     1,     3,     0,     4,
       3,     2,     4,     5,     5,     2,     4,     4,     3,     3,
       3,     2,     1,     4,     3,     3,     0,     0,     0,     5,
       0,     0,     0,     5,     1,     2,     3,     4,     5,     1,
       1,     0,     0,     0,     8,     1,     1,     1,     3,     3,
       1,     2,     3,     1,     1,     1,     1,     3,     1,     3,
       1,     1,     1,     1,     1,     4,     4,     4,     3,     4,
       4,     4,     3,     3,     3,     2,     0,     4,     2,     0,
       4,     1,     1,     2,     2,     4,     1,     2,     3,     1,
       3,     5,     2,     1,     1,     3,     1,     3,     1,     2,
       1,     1,     3,     2,     1,     1,     3,     2,     1,     2,
       1,     1,     1,     3,     3,     2,     2,     1,     1,     1,
       2,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     2,     4,     2,     3,     1,     6,
       1,     1,     1,     1,     2,     1,     2,     1,     1,     1,
       1,     1,     1,     2,     3,     3,     3,     1,     2,     4,
       0,     3,     1,     2,     4,     0,     3,     4,     4,     0,
       3,     0,     3,     0,     2,     0,     2,     0,     2,     1,
       0,     3,     0,     0,     0,     0,     0,     8,     1,     1,
       1,     1,     1,     1,     2,     1,     1,     3,     1,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     0,     4,     0,     1,     1,     3,     1,
       0,     3,     4,     2,     2,     1,     1,     2,     0,     6,
       8,     4,     6,     4,     6,     2,     4,     6,     2,     4,
       2,     4,     1,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     3,     1,     3,     1,     2,     1,     2,
       1,     1,     3,     1,     3,     1,     1,     1,     2,     1,
       3,     3,     1,     3,     1,     3,     1,     1,     2,     1,
       1,     1,     2,     1,     2,     1,     1,     0,     4,     1,
       2,     1,     3,     3,     2,     1,     4,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     0,     1,     0,     1,     2,     2,     2,
       1,     1,     1,     1,     1,     2,     0
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (&yylloc, p, YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF

/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)


/* YYLOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

# ifndef YYLOCATION_PRINT

#  if defined YY_LOCATION_PRINT

   /* Temporary convenience wrapper in case some people defined the
      undocumented and private YY_LOCATION_PRINT macros.  */
#   define YYLOCATION_PRINT(File, Loc)  YY_LOCATION_PRINT(File, *(Loc))

#  elif defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static int
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  int res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (p, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (p, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (p, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (p, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (p, "-%d", end_col);
    }
  return res;
}

#   define YYLOCATION_PRINT  yy_location_print_

    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT(File, Loc)  YYLOCATION_PRINT(File, &(Loc))

#  else

#   define YYLOCATION_PRINT(File, Loc) ((void) 0)
    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT  YYLOCATION_PRINT

#  endif
# endif /* !defined YYLOCATION_PRINT */


# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (p, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value, Location, p); \
      YYFPRINTF (p, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, struct parser_params *p)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  YY_USE (yylocationp);
  YY_USE (p);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
switch (yykind)
    {
    case YYSYMBOL_keyword_class: /* "`class'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6516 "ripper.c"
        break;

    case YYSYMBOL_keyword_module: /* "`module'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6529 "ripper.c"
        break;

    case YYSYMBOL_keyword_def: /* "`def'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6542 "ripper.c"
        break;

    case YYSYMBOL_keyword_undef: /* "`undef'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6555 "ripper.c"
        break;

    case YYSYMBOL_keyword_begin: /* "`begin'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6568 "ripper.c"
        break;

    case YYSYMBOL_keyword_rescue: /* "`rescue'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6581 "ripper.c"
        break;

    case YYSYMBOL_keyword_ensure: /* "`ensure'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6594 "ripper.c"
        break;

    case YYSYMBOL_keyword_end: /* "`end'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6607 "ripper.c"
        break;

    case YYSYMBOL_keyword_if: /* "`if'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6620 "ripper.c"
        break;

    case YYSYMBOL_keyword_unless: /* "`unless'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6633 "ripper.c"
        break;

    case YYSYMBOL_keyword_then: /* "`then'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6646 "ripper.c"
        break;

    case YYSYMBOL_keyword_elsif: /* "`elsif'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6659 "ripper.c"
        break;

    case YYSYMBOL_keyword_else: /* "`else'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6672 "ripper.c"
        break;

    case YYSYMBOL_keyword_case: /* "`case'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6685 "ripper.c"
        break;

    case YYSYMBOL_keyword_when: /* "`when'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6698 "ripper.c"
        break;

    case YYSYMBOL_keyword_while: /* "`while'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6711 "ripper.c"
        break;

    case YYSYMBOL_keyword_until: /* "`until'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6724 "ripper.c"
        break;

    case YYSYMBOL_keyword_for: /* "`for'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6737 "ripper.c"
        break;

    case YYSYMBOL_keyword_break: /* "`break'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6750 "ripper.c"
        break;

    case YYSYMBOL_keyword_next: /* "`next'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6763 "ripper.c"
        break;

    case YYSYMBOL_keyword_redo: /* "`redo'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6776 "ripper.c"
        break;

    case YYSYMBOL_keyword_retry: /* "`retry'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6789 "ripper.c"
        break;

    case YYSYMBOL_keyword_in: /* "`in'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6802 "ripper.c"
        break;

    case YYSYMBOL_keyword_do: /* "`do'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6815 "ripper.c"
        break;

    case YYSYMBOL_keyword_do_cond: /* "`do' for condition"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6828 "ripper.c"
        break;

    case YYSYMBOL_keyword_do_block: /* "`do' for block"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6841 "ripper.c"
        break;

    case YYSYMBOL_keyword_do_LAMBDA: /* "`do' for lambda"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6854 "ripper.c"
        break;

    case YYSYMBOL_keyword_return: /* "`return'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6867 "ripper.c"
        break;

    case YYSYMBOL_keyword_yield: /* "`yield'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6880 "ripper.c"
        break;

    case YYSYMBOL_keyword_super: /* "`super'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6893 "ripper.c"
        break;

    case YYSYMBOL_keyword_self: /* "`self'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6906 "ripper.c"
        break;

    case YYSYMBOL_keyword_nil: /* "`nil'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6919 "ripper.c"
        break;

    case YYSYMBOL_keyword_true: /* "`true'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6932 "ripper.c"
        break;

    case YYSYMBOL_keyword_false: /* "`false'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6945 "ripper.c"
        break;

    case YYSYMBOL_keyword_and: /* "`and'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6958 "ripper.c"
        break;

    case YYSYMBOL_keyword_or: /* "`or'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6971 "ripper.c"
        break;

    case YYSYMBOL_keyword_not: /* "`not'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6984 "ripper.c"
        break;

    case YYSYMBOL_modifier_if: /* "`if' modifier"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 6997 "ripper.c"
        break;

    case YYSYMBOL_modifier_unless: /* "`unless' modifier"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7010 "ripper.c"
        break;

    case YYSYMBOL_modifier_while: /* "`while' modifier"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7023 "ripper.c"
        break;

    case YYSYMBOL_modifier_until: /* "`until' modifier"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7036 "ripper.c"
        break;

    case YYSYMBOL_modifier_rescue: /* "`rescue' modifier"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7049 "ripper.c"
        break;

    case YYSYMBOL_keyword_alias: /* "`alias'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7062 "ripper.c"
        break;

    case YYSYMBOL_keyword_defined: /* "`defined?'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7075 "ripper.c"
        break;

    case YYSYMBOL_keyword_BEGIN: /* "`BEGIN'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7088 "ripper.c"
        break;

    case YYSYMBOL_keyword_END: /* "`END'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7101 "ripper.c"
        break;

    case YYSYMBOL_keyword__LINE__: /* "`__LINE__'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7114 "ripper.c"
        break;

    case YYSYMBOL_keyword__FILE__: /* "`__FILE__'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7127 "ripper.c"
        break;

    case YYSYMBOL_keyword__ENCODING__: /* "`__ENCODING__'"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7140 "ripper.c"
        break;

    case YYSYMBOL_tIDENTIFIER: /* "local variable or method"  */
#line 1340 "ripper.y"
         {
#ifndef RIPPER
    rb_parser_printf(p, "%"PRIsVALUE, rb_id2str(((*yyvaluep).val)));
#else
    rb_parser_printf(p, "%"PRIsVALUE, RNODE(((*yyvaluep).val))->nd_rval);
#endif
}
#line 7152 "ripper.c"
        break;

    case YYSYMBOL_tFID: /* "method"  */
#line 1340 "ripper.y"
         {
#ifndef RIPPER
    rb_parser_printf(p, "%"PRIsVALUE, rb_id2str(((*yyvaluep).val)));
#else
    rb_parser_printf(p, "%"PRIsVALUE, RNODE(((*yyvaluep).val))->nd_rval);
#endif
}
#line 7164 "ripper.c"
        break;

    case YYSYMBOL_tGVAR: /* "global variable"  */
#line 1340 "ripper.y"
         {
#ifndef RIPPER
    rb_parser_printf(p, "%"PRIsVALUE, rb_id2str(((*yyvaluep).val)));
#else
    rb_parser_printf(p, "%"PRIsVALUE, RNODE(((*yyvaluep).val))->nd_rval);
#endif
}
#line 7176 "ripper.c"
        break;

    case YYSYMBOL_tIVAR: /* "instance variable"  */
#line 1340 "ripper.y"
         {
#ifndef RIPPER
    rb_parser_printf(p, "%"PRIsVALUE, rb_id2str(((*yyvaluep).val)));
#else
    rb_parser_printf(p, "%"PRIsVALUE, RNODE(((*yyvaluep).val))->nd_rval);
#endif
}
#line 7188 "ripper.c"
        break;

    case YYSYMBOL_tCONSTANT: /* "constant"  */
#line 1340 "ripper.y"
         {
#ifndef RIPPER
    rb_parser_printf(p, "%"PRIsVALUE, rb_id2str(((*yyvaluep).val)));
#else
    rb_parser_printf(p, "%"PRIsVALUE, RNODE(((*yyvaluep).val))->nd_rval);
#endif
}
#line 7200 "ripper.c"
        break;

    case YYSYMBOL_tCVAR: /* "class variable"  */
#line 1340 "ripper.y"
         {
#ifndef RIPPER
    rb_parser_printf(p, "%"PRIsVALUE, rb_id2str(((*yyvaluep).val)));
#else
    rb_parser_printf(p, "%"PRIsVALUE, RNODE(((*yyvaluep).val))->nd_rval);
#endif
}
#line 7212 "ripper.c"
        break;

    case YYSYMBOL_tLABEL: /* "label"  */
#line 1340 "ripper.y"
         {
#ifndef RIPPER
    rb_parser_printf(p, "%"PRIsVALUE, rb_id2str(((*yyvaluep).val)));
#else
    rb_parser_printf(p, "%"PRIsVALUE, RNODE(((*yyvaluep).val))->nd_rval);
#endif
}
#line 7224 "ripper.c"
        break;

    case YYSYMBOL_tINTEGER: /* "integer literal"  */
#line 1347 "ripper.y"
         {
#ifndef RIPPER
    rb_parser_printf(p, "%+"PRIsVALUE, ((*yyvaluep).val)->nd_lit);
#else
    rb_parser_printf(p, "%+"PRIsVALUE, get_value(((*yyvaluep).val)));
#endif
}
#line 7236 "ripper.c"
        break;

    case YYSYMBOL_tFLOAT: /* "float literal"  */
#line 1347 "ripper.y"
         {
#ifndef RIPPER
    rb_parser_printf(p, "%+"PRIsVALUE, ((*yyvaluep).val)->nd_lit);
#else
    rb_parser_printf(p, "%+"PRIsVALUE, get_value(((*yyvaluep).val)));
#endif
}
#line 7248 "ripper.c"
        break;

    case YYSYMBOL_tRATIONAL: /* "rational literal"  */
#line 1347 "ripper.y"
         {
#ifndef RIPPER
    rb_parser_printf(p, "%+"PRIsVALUE, ((*yyvaluep).val)->nd_lit);
#else
    rb_parser_printf(p, "%+"PRIsVALUE, get_value(((*yyvaluep).val)));
#endif
}
#line 7260 "ripper.c"
        break;

    case YYSYMBOL_tIMAGINARY: /* "imaginary literal"  */
#line 1347 "ripper.y"
         {
#ifndef RIPPER
    rb_parser_printf(p, "%+"PRIsVALUE, ((*yyvaluep).val)->nd_lit);
#else
    rb_parser_printf(p, "%+"PRIsVALUE, get_value(((*yyvaluep).val)));
#endif
}
#line 7272 "ripper.c"
        break;

    case YYSYMBOL_tCHAR: /* "char literal"  */
#line 1347 "ripper.y"
         {
#ifndef RIPPER
    rb_parser_printf(p, "%+"PRIsVALUE, ((*yyvaluep).val)->nd_lit);
#else
    rb_parser_printf(p, "%+"PRIsVALUE, get_value(((*yyvaluep).val)));
#endif
}
#line 7284 "ripper.c"
        break;

    case YYSYMBOL_tNTH_REF: /* "numbered reference"  */
#line 1354 "ripper.y"
         {
#ifndef RIPPER
    rb_parser_printf(p, "$%ld", ((*yyvaluep).val)->nd_nth);
#else
    rb_parser_printf(p, "%"PRIsVALUE, ((*yyvaluep).val));
#endif
}
#line 7296 "ripper.c"
        break;

    case YYSYMBOL_tBACK_REF: /* "back reference"  */
#line 1361 "ripper.y"
         {
#ifndef RIPPER
    rb_parser_printf(p, "$%c", (int)((*yyvaluep).val)->nd_nth);
#else
    rb_parser_printf(p, "%"PRIsVALUE, ((*yyvaluep).val));
#endif
}
#line 7308 "ripper.c"
        break;

    case YYSYMBOL_tSTRING_CONTENT: /* "literal content"  */
#line 1347 "ripper.y"
         {
#ifndef RIPPER
    rb_parser_printf(p, "%+"PRIsVALUE, ((*yyvaluep).val)->nd_lit);
#else
    rb_parser_printf(p, "%+"PRIsVALUE, get_value(((*yyvaluep).val)));
#endif
}
#line 7320 "ripper.c"
        break;

    case YYSYMBOL_tREGEXP_END: /* tREGEXP_END  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7333 "ripper.c"
        break;

    case YYSYMBOL_tDUMNY_END: /* "dummy end"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7346 "ripper.c"
        break;

    case YYSYMBOL_69_: /* '.'  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7359 "ripper.c"
        break;

    case YYSYMBOL_70_backslash_: /* "backslash"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7372 "ripper.c"
        break;

    case YYSYMBOL_72_escaped_horizontal_tab_: /* "escaped horizontal tab"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7385 "ripper.c"
        break;

    case YYSYMBOL_73_escaped_form_feed_: /* "escaped form feed"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7398 "ripper.c"
        break;

    case YYSYMBOL_74_escaped_carriage_return_: /* "escaped carriage return"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7411 "ripper.c"
        break;

    case YYSYMBOL_75_escaped_vertical_tab_: /* "escaped vertical tab"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7424 "ripper.c"
        break;

    case YYSYMBOL_tANDDOT: /* "&."  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7437 "ripper.c"
        break;

    case YYSYMBOL_tCOLON2: /* "::"  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7450 "ripper.c"
        break;

    case YYSYMBOL_tOP_ASGN: /* "operator-assignment"  */
#line 1340 "ripper.y"
         {
#ifndef RIPPER
    rb_parser_printf(p, "%"PRIsVALUE, rb_id2str(((*yyvaluep).val)));
#else
    rb_parser_printf(p, "%"PRIsVALUE, RNODE(((*yyvaluep).val))->nd_rval);
#endif
}
#line 7462 "ripper.c"
        break;

    case YYSYMBOL_top_compstmt: /* top_compstmt  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7475 "ripper.c"
        break;

    case YYSYMBOL_top_stmts: /* top_stmts  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7488 "ripper.c"
        break;

    case YYSYMBOL_top_stmt: /* top_stmt  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7501 "ripper.c"
        break;

    case YYSYMBOL_begin_block: /* begin_block  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7514 "ripper.c"
        break;

    case YYSYMBOL_bodystmt: /* bodystmt  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7527 "ripper.c"
        break;

    case YYSYMBOL_compstmt: /* compstmt  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7540 "ripper.c"
        break;

    case YYSYMBOL_stmts: /* stmts  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7553 "ripper.c"
        break;

    case YYSYMBOL_stmt_or_begin: /* stmt_or_begin  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7566 "ripper.c"
        break;

    case YYSYMBOL_stmt: /* stmt  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7579 "ripper.c"
        break;

    case YYSYMBOL_command_asgn: /* command_asgn  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7592 "ripper.c"
        break;

    case YYSYMBOL_endless_command: /* endless_command  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7605 "ripper.c"
        break;

    case YYSYMBOL_command_rhs: /* command_rhs  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7618 "ripper.c"
        break;

    case YYSYMBOL_expr: /* expr  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7631 "ripper.c"
        break;

    case YYSYMBOL_def_name: /* def_name  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7644 "ripper.c"
        break;

    case YYSYMBOL_defn_head: /* defn_head  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7657 "ripper.c"
        break;

    case YYSYMBOL_defs_head: /* defs_head  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7670 "ripper.c"
        break;

    case YYSYMBOL_expr_value: /* expr_value  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7683 "ripper.c"
        break;

    case YYSYMBOL_expr_value_do: /* expr_value_do  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7696 "ripper.c"
        break;

    case YYSYMBOL_command_call: /* command_call  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7709 "ripper.c"
        break;

    case YYSYMBOL_block_command: /* block_command  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7722 "ripper.c"
        break;

    case YYSYMBOL_cmd_brace_block: /* cmd_brace_block  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7735 "ripper.c"
        break;

    case YYSYMBOL_fcall: /* fcall  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7748 "ripper.c"
        break;

    case YYSYMBOL_command: /* command  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7761 "ripper.c"
        break;

    case YYSYMBOL_mlhs: /* mlhs  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7774 "ripper.c"
        break;

    case YYSYMBOL_mlhs_inner: /* mlhs_inner  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7787 "ripper.c"
        break;

    case YYSYMBOL_mlhs_basic: /* mlhs_basic  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7800 "ripper.c"
        break;

    case YYSYMBOL_mlhs_item: /* mlhs_item  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7813 "ripper.c"
        break;

    case YYSYMBOL_mlhs_head: /* mlhs_head  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7826 "ripper.c"
        break;

    case YYSYMBOL_mlhs_post: /* mlhs_post  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7839 "ripper.c"
        break;

    case YYSYMBOL_mlhs_node: /* mlhs_node  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7852 "ripper.c"
        break;

    case YYSYMBOL_lhs: /* lhs  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7865 "ripper.c"
        break;

    case YYSYMBOL_cname: /* cname  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7878 "ripper.c"
        break;

    case YYSYMBOL_cpath: /* cpath  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7891 "ripper.c"
        break;

    case YYSYMBOL_fname: /* fname  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7904 "ripper.c"
        break;

    case YYSYMBOL_fitem: /* fitem  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7917 "ripper.c"
        break;

    case YYSYMBOL_undef_list: /* undef_list  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7930 "ripper.c"
        break;

    case YYSYMBOL_op: /* op  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7943 "ripper.c"
        break;

    case YYSYMBOL_reswords: /* reswords  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7956 "ripper.c"
        break;

    case YYSYMBOL_arg: /* arg  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7969 "ripper.c"
        break;

    case YYSYMBOL_endless_arg: /* endless_arg  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7982 "ripper.c"
        break;

    case YYSYMBOL_relop: /* relop  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 7995 "ripper.c"
        break;

    case YYSYMBOL_rel_expr: /* rel_expr  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8008 "ripper.c"
        break;

    case YYSYMBOL_arg_value: /* arg_value  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8021 "ripper.c"
        break;

    case YYSYMBOL_aref_args: /* aref_args  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8034 "ripper.c"
        break;

    case YYSYMBOL_arg_rhs: /* arg_rhs  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8047 "ripper.c"
        break;

    case YYSYMBOL_paren_args: /* paren_args  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8060 "ripper.c"
        break;

    case YYSYMBOL_opt_paren_args: /* opt_paren_args  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8073 "ripper.c"
        break;

    case YYSYMBOL_opt_call_args: /* opt_call_args  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8086 "ripper.c"
        break;

    case YYSYMBOL_call_args: /* call_args  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8099 "ripper.c"
        break;

    case YYSYMBOL_command_args: /* command_args  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8112 "ripper.c"
        break;

    case YYSYMBOL_block_arg: /* block_arg  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8125 "ripper.c"
        break;

    case YYSYMBOL_opt_block_arg: /* opt_block_arg  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8138 "ripper.c"
        break;

    case YYSYMBOL_args: /* args  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8151 "ripper.c"
        break;

    case YYSYMBOL_mrhs_arg: /* mrhs_arg  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8164 "ripper.c"
        break;

    case YYSYMBOL_mrhs: /* mrhs  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8177 "ripper.c"
        break;

    case YYSYMBOL_primary: /* primary  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8190 "ripper.c"
        break;

    case YYSYMBOL_primary_value: /* primary_value  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8203 "ripper.c"
        break;

    case YYSYMBOL_if_tail: /* if_tail  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8216 "ripper.c"
        break;

    case YYSYMBOL_opt_else: /* opt_else  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8229 "ripper.c"
        break;

    case YYSYMBOL_for_var: /* for_var  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8242 "ripper.c"
        break;

    case YYSYMBOL_f_marg: /* f_marg  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8255 "ripper.c"
        break;

    case YYSYMBOL_f_marg_list: /* f_marg_list  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8268 "ripper.c"
        break;

    case YYSYMBOL_f_margs: /* f_margs  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8281 "ripper.c"
        break;

    case YYSYMBOL_f_rest_marg: /* f_rest_marg  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8294 "ripper.c"
        break;

    case YYSYMBOL_f_any_kwrest: /* f_any_kwrest  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8307 "ripper.c"
        break;

    case YYSYMBOL_block_args_tail: /* block_args_tail  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8320 "ripper.c"
        break;

    case YYSYMBOL_opt_block_args_tail: /* opt_block_args_tail  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8333 "ripper.c"
        break;

    case YYSYMBOL_excessed_comma: /* excessed_comma  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8346 "ripper.c"
        break;

    case YYSYMBOL_block_param: /* block_param  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8359 "ripper.c"
        break;

    case YYSYMBOL_opt_block_param: /* opt_block_param  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8372 "ripper.c"
        break;

    case YYSYMBOL_block_param_def: /* block_param_def  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8385 "ripper.c"
        break;

    case YYSYMBOL_opt_bv_decl: /* opt_bv_decl  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8398 "ripper.c"
        break;

    case YYSYMBOL_bv_decls: /* bv_decls  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8411 "ripper.c"
        break;

    case YYSYMBOL_bvar: /* bvar  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8424 "ripper.c"
        break;

    case YYSYMBOL_lambda: /* lambda  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8437 "ripper.c"
        break;

    case YYSYMBOL_f_larglist: /* f_larglist  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8450 "ripper.c"
        break;

    case YYSYMBOL_lambda_body: /* lambda_body  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8463 "ripper.c"
        break;

    case YYSYMBOL_do_block: /* do_block  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8476 "ripper.c"
        break;

    case YYSYMBOL_block_call: /* block_call  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8489 "ripper.c"
        break;

    case YYSYMBOL_method_call: /* method_call  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8502 "ripper.c"
        break;

    case YYSYMBOL_brace_block: /* brace_block  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8515 "ripper.c"
        break;

    case YYSYMBOL_brace_body: /* brace_body  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8528 "ripper.c"
        break;

    case YYSYMBOL_do_body: /* do_body  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8541 "ripper.c"
        break;

    case YYSYMBOL_case_args: /* case_args  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8554 "ripper.c"
        break;

    case YYSYMBOL_case_body: /* case_body  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8567 "ripper.c"
        break;

    case YYSYMBOL_cases: /* cases  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8580 "ripper.c"
        break;

    case YYSYMBOL_p_case_body: /* p_case_body  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8593 "ripper.c"
        break;

    case YYSYMBOL_p_cases: /* p_cases  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8606 "ripper.c"
        break;

    case YYSYMBOL_p_top_expr: /* p_top_expr  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8619 "ripper.c"
        break;

    case YYSYMBOL_p_top_expr_body: /* p_top_expr_body  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8632 "ripper.c"
        break;

    case YYSYMBOL_p_expr: /* p_expr  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8645 "ripper.c"
        break;

    case YYSYMBOL_p_as: /* p_as  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8658 "ripper.c"
        break;

    case YYSYMBOL_p_alt: /* p_alt  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8671 "ripper.c"
        break;

    case YYSYMBOL_p_expr_basic: /* p_expr_basic  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8684 "ripper.c"
        break;

    case YYSYMBOL_p_args: /* p_args  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8697 "ripper.c"
        break;

    case YYSYMBOL_p_args_head: /* p_args_head  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8710 "ripper.c"
        break;

    case YYSYMBOL_p_args_tail: /* p_args_tail  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8723 "ripper.c"
        break;

    case YYSYMBOL_p_find: /* p_find  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8736 "ripper.c"
        break;

    case YYSYMBOL_p_rest: /* p_rest  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8749 "ripper.c"
        break;

    case YYSYMBOL_p_args_post: /* p_args_post  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8762 "ripper.c"
        break;

    case YYSYMBOL_p_arg: /* p_arg  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8775 "ripper.c"
        break;

    case YYSYMBOL_p_kwargs: /* p_kwargs  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8788 "ripper.c"
        break;

    case YYSYMBOL_p_kwarg: /* p_kwarg  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8801 "ripper.c"
        break;

    case YYSYMBOL_p_kw: /* p_kw  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8814 "ripper.c"
        break;

    case YYSYMBOL_p_kw_label: /* p_kw_label  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8827 "ripper.c"
        break;

    case YYSYMBOL_p_kwrest: /* p_kwrest  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8840 "ripper.c"
        break;

    case YYSYMBOL_p_kwnorest: /* p_kwnorest  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8853 "ripper.c"
        break;

    case YYSYMBOL_p_any_kwrest: /* p_any_kwrest  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8866 "ripper.c"
        break;

    case YYSYMBOL_p_value: /* p_value  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8879 "ripper.c"
        break;

    case YYSYMBOL_p_primitive: /* p_primitive  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8892 "ripper.c"
        break;

    case YYSYMBOL_p_variable: /* p_variable  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8905 "ripper.c"
        break;

    case YYSYMBOL_p_var_ref: /* p_var_ref  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8918 "ripper.c"
        break;

    case YYSYMBOL_p_expr_ref: /* p_expr_ref  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8931 "ripper.c"
        break;

    case YYSYMBOL_p_const: /* p_const  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8944 "ripper.c"
        break;

    case YYSYMBOL_opt_rescue: /* opt_rescue  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8957 "ripper.c"
        break;

    case YYSYMBOL_exc_list: /* exc_list  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8970 "ripper.c"
        break;

    case YYSYMBOL_exc_var: /* exc_var  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8983 "ripper.c"
        break;

    case YYSYMBOL_opt_ensure: /* opt_ensure  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 8996 "ripper.c"
        break;

    case YYSYMBOL_literal: /* literal  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9009 "ripper.c"
        break;

    case YYSYMBOL_strings: /* strings  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9022 "ripper.c"
        break;

    case YYSYMBOL_string: /* string  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9035 "ripper.c"
        break;

    case YYSYMBOL_string1: /* string1  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9048 "ripper.c"
        break;

    case YYSYMBOL_xstring: /* xstring  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9061 "ripper.c"
        break;

    case YYSYMBOL_regexp: /* regexp  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9074 "ripper.c"
        break;

    case YYSYMBOL_words: /* words  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9087 "ripper.c"
        break;

    case YYSYMBOL_word_list: /* word_list  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9100 "ripper.c"
        break;

    case YYSYMBOL_word: /* word  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9113 "ripper.c"
        break;

    case YYSYMBOL_symbols: /* symbols  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9126 "ripper.c"
        break;

    case YYSYMBOL_symbol_list: /* symbol_list  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9139 "ripper.c"
        break;

    case YYSYMBOL_qwords: /* qwords  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9152 "ripper.c"
        break;

    case YYSYMBOL_qsymbols: /* qsymbols  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9165 "ripper.c"
        break;

    case YYSYMBOL_qword_list: /* qword_list  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9178 "ripper.c"
        break;

    case YYSYMBOL_qsym_list: /* qsym_list  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9191 "ripper.c"
        break;

    case YYSYMBOL_string_contents: /* string_contents  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9204 "ripper.c"
        break;

    case YYSYMBOL_xstring_contents: /* xstring_contents  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9217 "ripper.c"
        break;

    case YYSYMBOL_regexp_contents: /* regexp_contents  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9230 "ripper.c"
        break;

    case YYSYMBOL_string_content: /* string_content  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9243 "ripper.c"
        break;

    case YYSYMBOL_string_dvar: /* string_dvar  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9256 "ripper.c"
        break;

    case YYSYMBOL_symbol: /* symbol  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9269 "ripper.c"
        break;

    case YYSYMBOL_ssym: /* ssym  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9282 "ripper.c"
        break;

    case YYSYMBOL_sym: /* sym  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9295 "ripper.c"
        break;

    case YYSYMBOL_dsym: /* dsym  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9308 "ripper.c"
        break;

    case YYSYMBOL_numeric: /* numeric  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9321 "ripper.c"
        break;

    case YYSYMBOL_simple_numeric: /* simple_numeric  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9334 "ripper.c"
        break;

    case YYSYMBOL_nonlocal_var: /* nonlocal_var  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9347 "ripper.c"
        break;

    case YYSYMBOL_user_variable: /* user_variable  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9360 "ripper.c"
        break;

    case YYSYMBOL_keyword_variable: /* keyword_variable  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9373 "ripper.c"
        break;

    case YYSYMBOL_var_ref: /* var_ref  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9386 "ripper.c"
        break;

    case YYSYMBOL_var_lhs: /* var_lhs  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9399 "ripper.c"
        break;

    case YYSYMBOL_backref: /* backref  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9412 "ripper.c"
        break;

    case YYSYMBOL_superclass: /* superclass  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9425 "ripper.c"
        break;

    case YYSYMBOL_f_opt_paren_args: /* f_opt_paren_args  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9438 "ripper.c"
        break;

    case YYSYMBOL_f_paren_args: /* f_paren_args  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9451 "ripper.c"
        break;

    case YYSYMBOL_f_arglist: /* f_arglist  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9464 "ripper.c"
        break;

    case YYSYMBOL_args_tail: /* args_tail  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9477 "ripper.c"
        break;

    case YYSYMBOL_opt_args_tail: /* opt_args_tail  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9490 "ripper.c"
        break;

    case YYSYMBOL_f_args: /* f_args  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9503 "ripper.c"
        break;

    case YYSYMBOL_args_forward: /* args_forward  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9516 "ripper.c"
        break;

    case YYSYMBOL_f_bad_arg: /* f_bad_arg  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9529 "ripper.c"
        break;

    case YYSYMBOL_f_norm_arg: /* f_norm_arg  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9542 "ripper.c"
        break;

    case YYSYMBOL_f_arg_asgn: /* f_arg_asgn  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9555 "ripper.c"
        break;

    case YYSYMBOL_f_arg_item: /* f_arg_item  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9568 "ripper.c"
        break;

    case YYSYMBOL_f_arg: /* f_arg  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9581 "ripper.c"
        break;

    case YYSYMBOL_f_label: /* f_label  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9594 "ripper.c"
        break;

    case YYSYMBOL_f_kw: /* f_kw  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9607 "ripper.c"
        break;

    case YYSYMBOL_f_block_kw: /* f_block_kw  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9620 "ripper.c"
        break;

    case YYSYMBOL_f_block_kwarg: /* f_block_kwarg  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9633 "ripper.c"
        break;

    case YYSYMBOL_f_kwarg: /* f_kwarg  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9646 "ripper.c"
        break;

    case YYSYMBOL_f_no_kwarg: /* f_no_kwarg  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9659 "ripper.c"
        break;

    case YYSYMBOL_f_kwrest: /* f_kwrest  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9672 "ripper.c"
        break;

    case YYSYMBOL_f_opt: /* f_opt  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9685 "ripper.c"
        break;

    case YYSYMBOL_f_block_opt: /* f_block_opt  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9698 "ripper.c"
        break;

    case YYSYMBOL_f_block_optarg: /* f_block_optarg  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9711 "ripper.c"
        break;

    case YYSYMBOL_f_optarg: /* f_optarg  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9724 "ripper.c"
        break;

    case YYSYMBOL_f_rest_arg: /* f_rest_arg  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9737 "ripper.c"
        break;

    case YYSYMBOL_f_block_arg: /* f_block_arg  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9750 "ripper.c"
        break;

    case YYSYMBOL_opt_f_block_arg: /* opt_f_block_arg  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9763 "ripper.c"
        break;

    case YYSYMBOL_singleton: /* singleton  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9776 "ripper.c"
        break;

    case YYSYMBOL_assoc_list: /* assoc_list  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9789 "ripper.c"
        break;

    case YYSYMBOL_assocs: /* assocs  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9802 "ripper.c"
        break;

    case YYSYMBOL_assoc: /* assoc  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9815 "ripper.c"
        break;

    case YYSYMBOL_operation: /* operation  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9828 "ripper.c"
        break;

    case YYSYMBOL_operation2: /* operation2  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9841 "ripper.c"
        break;

    case YYSYMBOL_operation3: /* operation3  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9854 "ripper.c"
        break;

    case YYSYMBOL_dot_or_colon: /* dot_or_colon  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9867 "ripper.c"
        break;

    case YYSYMBOL_call_op: /* call_op  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9880 "ripper.c"
        break;

    case YYSYMBOL_call_op2: /* call_op2  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9893 "ripper.c"
        break;

    case YYSYMBOL_none: /* none  */
#line 1332 "ripper.y"
         {
#ifndef RIPPER
    if (((*yyvaluep).val)) {
        rb_parser_printf(p, "%s", ruby_node_name(nd_type(((*yyvaluep).val))));
    }
#else
#endif
}
#line 9906 "ripper.c"
        break;

      default:
        break;
    }
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, struct parser_params *p)
{
  YYFPRINTF (p, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  YYLOCATION_PRINT (yyo, yylocationp);
  YYFPRINTF (p, ": ");
  yy_symbol_value_print (yyo, yykind, yyvaluep, yylocationp, p);
  YYFPRINTF (p, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
ruby_parser_yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop, struct parser_params *p)
#define yy_stack_print(b, t) ruby_parser_yy_stack_print(b, t, p)
{
  YYFPRINTF (p, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (p, " %d", yybot);
    }
  YYFPRINTF (p, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp,
                 int yyrule, struct parser_params *p)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (p, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (p, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)],
                       &(yylsp[(yyi + 1) - (yynrhs)]), p);
      YYFPRINTF (p, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule, p); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
#ifndef yydebug
int yydebug;
#endif
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


/* Context of a parse error.  */
typedef struct
{
  yy_state_t *yyssp;
  yysymbol_kind_t yytoken;
  YYLTYPE *yylloc;
} yypcontext_t;

/* Put in YYARG at most YYARGN of the expected tokens given the
   current YYCTX, and return the number of tokens stored in YYARG.  If
   YYARG is null, return the number of expected tokens (guaranteed to
   be less than YYNTOKENS).  Return YYENOMEM on memory exhaustion.
   Return 0 if there are more than YYARGN expected tokens, yet fill
   YYARG up to YYARGN. */
static int
yypcontext_expected_tokens (const yypcontext_t *yyctx,
                            yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  int yyn = yypact[+*yyctx->yyssp];
  if (!yypact_value_is_default (yyn))
    {
      /* Start YYX at -YYN if negative to avoid negative indexes in
         YYCHECK.  In other words, skip the first -YYN actions for
         this state because they are default actions.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;
      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yyx;
      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
        if (yycheck[yyx + yyn] == yyx && yyx != YYSYMBOL_YYerror
            && !yytable_value_is_error (yytable[yyx + yyn]))
          {
            if (!yyarg)
              ++yycount;
            else if (yycount == yyargn)
              return 0;
            else
              yyarg[yycount++] = YY_CAST (yysymbol_kind_t, yyx);
          }
    }
  if (yyarg && yycount == 0 && 0 < yyargn)
    yyarg[0] = YYSYMBOL_YYEMPTY;
  return yycount;
}




#ifndef yystrlen
# if defined __GLIBC__ && defined _STRING_H
#  define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
# else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
# endif
#endif

#ifndef yystpcpy
# if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#  define yystpcpy stpcpy
# else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
# endif
#endif

#ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
      char const *yyp = yystr;
      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            else
              goto append;

          append:
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
#endif


static int
yy_syntax_error_arguments (const yypcontext_t *yyctx,
                           yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yyctx->yytoken != YYSYMBOL_YYEMPTY)
    {
      int yyn;
      if (yyarg)
        yyarg[yycount] = yyctx->yytoken;
      ++yycount;
      yyn = yypcontext_expected_tokens (yyctx,
                                        yyarg ? yyarg + 1 : yyarg, yyargn - 1);
      if (yyn == YYENOMEM)
        return YYENOMEM;
      else
        yycount += yyn;
    }
  return yycount;
}

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return -1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return YYENOMEM if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (struct parser_params *p, YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                const yypcontext_t *yyctx)
{
  enum { YYARGS_MAX = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  yysymbol_kind_t yyarg[YYARGS_MAX];
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* Actual size of YYARG. */
  int yycount = yy_syntax_error_arguments (yyctx, yyarg, YYARGS_MAX);
  if (yycount == YYENOMEM)
    return YYENOMEM;

  switch (yycount)
    {
#define YYCASE_(N, S)                       \
      case N:                               \
        yyformat = S;                       \
        break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
    }

  /* Compute error message size.  Don't count the "%s"s, but reserve
     room for the terminator.  */
  yysize = yystrlen (yyformat) - 2 * yycount + 1;
  {
    int yyi;
    for (yyi = 0; yyi < yycount; ++yyi)
      {
        YYPTRDIFF_T yysize1
          = yysize + yytnamerr (YY_NULLPTR, yytname[yyarg[yyi]]);
        if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
          yysize = yysize1;
        else
          return YYENOMEM;
      }
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return -1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yytname[yyarg[yyi++]]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep, YYLTYPE *yylocationp, struct parser_params *p)
{
  YY_USE (yyvaluep);
  YY_USE (yylocationp);
  YY_USE (p);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}






/*----------.
| yyparse.  |
`----------*/

int
yyparse (struct parser_params *p)
{
/* Lookahead token kind.  */
int yychar;


/* The semantic value of the lookahead symbol.  */
/* Default value used for initialization, for pacifying older GCCs
   or non-GCC compilers.  */
YY_INITIAL_VALUE (static const YYSTYPE yyval_default;)
YYSTYPE yylval YY_INITIAL_VALUE (= yyval_default);

/* Location data for the lookahead symbol.  */
static const YYLTYPE yyloc_default
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
YYLTYPE yylloc = yyloc_default;

    /* Number of syntax errors so far.  */
    int yynerrs = 0;

    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

    /* The location stack: array, bottom, top.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls = yylsa;
    YYLTYPE *yylsp = yyls;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[3];

  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((p, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */


        /* User initialization code.  */
#line 1372 "ripper.y"
        {
    RUBY_SET_YYLLOC_OF_NONE(yylloc);
}

#line 10401 "ripper.c"

  yylsp[0] = yylloc;
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((p, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yyls1, yysize * YYSIZEOF (*yylsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
        yyls = yyls1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
        YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((p, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((p, "Reading a token\n"));
      yychar = yylex (&yylval, &yylloc, p);
    }

  if (yychar <= END_OF_INPUT)
    {
      yychar = END_OF_INPUT;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((p, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      yyerror_range[1] = yylloc;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location. */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  yyerror_range[1] = yyloc;
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* $@1: %empty  */
#line 1581 "ripper.y"
            {
                        SET_LEX_STATE(EXPR_BEG);
                        local_push(p, ifndef_ripper(1)+0);
                    }
#line 10617 "ripper.c"
    break;

  case 3: /* program: $@1 top_compstmt  */
#line 1586 "ripper.y"
                    {
#if 0
                        if ((yyvsp[0].val) && !compile_for_eval) {
                            NODE *node = (yyvsp[0].val);
                            /* last expression should not be void */
                            if (nd_type_p(node, NODE_BLOCK)) {
                                while (node->nd_next) {
                                    node = node->nd_next;
                                }
                                node = node->nd_head;
                            }
                            node = remove_begin(node);
                            void_expr(p, node);
                        }
                        p->eval_tree = NEW_SCOPE(0, block_append(p, p->eval_tree, (yyvsp[0].val)), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(program,v1);p->result=v2;}
                        local_pop(p);
                    }
#line 10641 "ripper.c"
    break;

  case 4: /* top_compstmt: top_stmts opt_terms  */
#line 1608 "ripper.y"
                    {
                        (yyval.val) = void_stmts(p, (yyvsp[-1].val));
                    }
#line 10649 "ripper.c"
    break;

  case 5: /* top_stmts: none  */
#line 1614 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_BEGIN(0, &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5;v1=dispatch0(stmts_new);v2=dispatch0(void_stmt);v3=v1;v4=v2;v5=dispatch2(stmts_add,v3,v4);(yyval.val)=v5;}
                    }
#line 10660 "ripper.c"
    break;

  case 6: /* top_stmts: top_stmt  */
#line 1621 "ripper.y"
                    {
#if 0
                        (yyval.val) = newline_node((yyvsp[0].val));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(stmts_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(stmts_add,v2,v3);(yyval.val)=v4;}
                    }
#line 10671 "ripper.c"
    break;

  case 7: /* top_stmts: top_stmts terms top_stmt  */
#line 1628 "ripper.y"
                    {
#if 0
                        (yyval.val) = block_append(p, (yyvsp[-2].val), newline_node((yyvsp[0].val)));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(stmts_add,v1,v2);(yyval.val)=v3;}
                    }
#line 10682 "ripper.c"
    break;

  case 9: /* top_stmt: "`BEGIN'" begin_block  */
#line 1638 "ripper.y"
                    {
                        (yyval.val) = (yyvsp[0].val);
                    }
#line 10690 "ripper.c"
    break;

  case 10: /* begin_block: '{' top_compstmt '}'  */
#line 1644 "ripper.y"
                    {
#if 0
                        p->eval_tree_begin = block_append(p, p->eval_tree_begin,
                                                          NEW_BEGIN((yyvsp[-1].val), &(yyloc)));
                        (yyval.val) = NEW_BEGIN(0, &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(BEGIN,v1);(yyval.val)=v2;}
                    }
#line 10703 "ripper.c"
    break;

  case 11: /* $@2: %empty  */
#line 1656 "ripper.y"
                         {if (!(yyvsp[-1].val)) {yyerror1(&(yylsp[0]), "else without rescue is useless");}}
#line 10709 "ripper.c"
    break;

  case 12: /* bodystmt: compstmt opt_rescue k_else $@2 compstmt opt_ensure  */
#line 1659 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_bodystmt(p, (yyvsp[-5].val), (yyvsp[-4].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5;v1=escape_Qundef((yyvsp[-5].val));v2=escape_Qundef((yyvsp[-4].val));v3=escape_Qundef((yyvsp[-1].val));v4=escape_Qundef((yyvsp[0].val));v5=dispatch4(bodystmt,v1,v2,v3,v4);(yyval.val)=v5;}
                    }
#line 10720 "ripper.c"
    break;

  case 13: /* bodystmt: compstmt opt_rescue opt_ensure  */
#line 1668 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_bodystmt(p, (yyvsp[-2].val), (yyvsp[-1].val), 0, (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5;v1=escape_Qundef((yyvsp[-2].val));v2=escape_Qundef((yyvsp[-1].val));v3=Qnil;v4=escape_Qundef((yyvsp[0].val));v5=dispatch4(bodystmt,v1,v2,v3,v4);(yyval.val)=v5;}
                    }
#line 10731 "ripper.c"
    break;

  case 14: /* compstmt: stmts opt_terms  */
#line 1677 "ripper.y"
                    {
                        (yyval.val) = void_stmts(p, (yyvsp[-1].val));
                    }
#line 10739 "ripper.c"
    break;

  case 15: /* stmts: none  */
#line 1683 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_BEGIN(0, &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5;v1=dispatch0(stmts_new);v2=dispatch0(void_stmt);v3=v1;v4=v2;v5=dispatch2(stmts_add,v3,v4);(yyval.val)=v5;}
                    }
#line 10750 "ripper.c"
    break;

  case 16: /* stmts: stmt_or_begin  */
#line 1690 "ripper.y"
                    {
#if 0
                        (yyval.val) = newline_node((yyvsp[0].val));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(stmts_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(stmts_add,v2,v3);(yyval.val)=v4;}
                    }
#line 10761 "ripper.c"
    break;

  case 17: /* stmts: stmts terms stmt_or_begin  */
#line 1697 "ripper.y"
                    {
#if 0
                        (yyval.val) = block_append(p, (yyvsp[-2].val), newline_node((yyvsp[0].val)));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(stmts_add,v1,v2);(yyval.val)=v3;}
                    }
#line 10772 "ripper.c"
    break;

  case 18: /* stmt_or_begin: stmt  */
#line 1706 "ripper.y"
                    {
                        (yyval.val) = (yyvsp[0].val);
                    }
#line 10780 "ripper.c"
    break;

  case 19: /* $@3: %empty  */
#line 1710 "ripper.y"
                    {
                        yyerror1(&(yylsp[0]), "BEGIN is permitted only at toplevel");
                    }
#line 10788 "ripper.c"
    break;

  case 20: /* stmt_or_begin: "`BEGIN'" $@3 begin_block  */
#line 1714 "ripper.y"
                    {
                        (yyval.val) = (yyvsp[0].val);
                    }
#line 10796 "ripper.c"
    break;

  case 21: /* $@4: %empty  */
#line 1719 "ripper.y"
                            {SET_LEX_STATE(EXPR_FNAME|EXPR_FITEM);}
#line 10802 "ripper.c"
    break;

  case 22: /* stmt: "`alias'" fitem $@4 fitem  */
#line 1720 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_ALIAS((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(alias,v1,v2);(yyval.val)=v3;}
                    }
#line 10813 "ripper.c"
    break;

  case 23: /* stmt: "`alias'" "global variable" "global variable"  */
#line 1727 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_VALIAS((yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=(yyvsp[0].val);v3=dispatch2(var_alias,v1,v2);(yyval.val)=v3;}
                    }
#line 10824 "ripper.c"
    break;

  case 24: /* stmt: "`alias'" "global variable" "back reference"  */
#line 1734 "ripper.y"
                    {
#if 0
                        char buf[2];
                        buf[0] = '$';
                        buf[1] = (char)(yyvsp[0].val)->nd_nth;
                        (yyval.val) = NEW_VALIAS((yyvsp[-1].val), rb_intern2(buf, 2), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=(yyvsp[0].val);v3=dispatch2(var_alias,v1,v2);(yyval.val)=v3;}
                    }
#line 10838 "ripper.c"
    break;

  case 25: /* stmt: "`alias'" "global variable" "numbered reference"  */
#line 1744 "ripper.y"
                    {
                        static const char mesg[] = "can't make alias for the number variables";
#if 0
                        yyerror1(&(yylsp[0]), mesg);
                        (yyval.val) = NEW_BEGIN(0, &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=ERR_MESG();v2=(yyvsp[0].val);v3=dispatch2(alias_error,v1,v2);(yyval.val)=v3;}ripper_error(p);
                    }
#line 10851 "ripper.c"
    break;

  case 26: /* stmt: "`undef'" undef_list  */
#line 1753 "ripper.y"
                    {
#if 0
                        (yyval.val) = (yyvsp[0].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(undef,v1);(yyval.val)=v2;}
                    }
#line 10862 "ripper.c"
    break;

  case 27: /* stmt: stmt "`if' modifier" expr_value  */
#line 1760 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_if(p, (yyvsp[0].val), remove_begin((yyvsp[-2].val)), 0, &(yyloc));
                        fixpos((yyval.val), (yyvsp[0].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[0].val);v2=(yyvsp[-2].val);v3=dispatch2(if_mod,v1,v2);(yyval.val)=v3;}
                    }
#line 10874 "ripper.c"
    break;

  case 28: /* stmt: stmt "`unless' modifier" expr_value  */
#line 1768 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_unless(p, (yyvsp[0].val), remove_begin((yyvsp[-2].val)), 0, &(yyloc));
                        fixpos((yyval.val), (yyvsp[0].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[0].val);v2=(yyvsp[-2].val);v3=dispatch2(unless_mod,v1,v2);(yyval.val)=v3;}
                    }
#line 10886 "ripper.c"
    break;

  case 29: /* stmt: stmt "`while' modifier" expr_value  */
#line 1776 "ripper.y"
                    {
#if 0
                        if ((yyvsp[-2].val) && nd_type_p((yyvsp[-2].val), NODE_BEGIN)) {
                            (yyval.val) = NEW_WHILE(cond(p, (yyvsp[0].val), &(yylsp[0])), (yyvsp[-2].val)->nd_body, 0, &(yyloc));
                        }
                        else {
                            (yyval.val) = NEW_WHILE(cond(p, (yyvsp[0].val), &(yylsp[0])), (yyvsp[-2].val), 1, &(yyloc));
                        }
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[0].val);v2=(yyvsp[-2].val);v3=dispatch2(while_mod,v1,v2);(yyval.val)=v3;}
                    }
#line 10902 "ripper.c"
    break;

  case 30: /* stmt: stmt "`until' modifier" expr_value  */
#line 1788 "ripper.y"
                    {
#if 0
                        if ((yyvsp[-2].val) && nd_type_p((yyvsp[-2].val), NODE_BEGIN)) {
                            (yyval.val) = NEW_UNTIL(cond(p, (yyvsp[0].val), &(yylsp[0])), (yyvsp[-2].val)->nd_body, 0, &(yyloc));
                        }
                        else {
                            (yyval.val) = NEW_UNTIL(cond(p, (yyvsp[0].val), &(yylsp[0])), (yyvsp[-2].val), 1, &(yyloc));
                        }
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[0].val);v2=(yyvsp[-2].val);v3=dispatch2(until_mod,v1,v2);(yyval.val)=v3;}
                    }
#line 10918 "ripper.c"
    break;

  case 31: /* stmt: stmt "`rescue' modifier" stmt  */
#line 1800 "ripper.y"
                    {
#if 0
                        NODE *resq;
                        YYLTYPE loc = code_loc_gen(&(yylsp[-1]), &(yylsp[0]));
                        resq = NEW_RESBODY(0, remove_begin((yyvsp[0].val)), 0, &loc);
                        (yyval.val) = NEW_RESCUE(remove_begin((yyvsp[-2].val)), resq, 0, &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(rescue_mod,v1,v2);(yyval.val)=v3;}
                    }
#line 10932 "ripper.c"
    break;

  case 32: /* stmt: "`END'" '{' compstmt '}'  */
#line 1810 "ripper.y"
                    {
                        if (p->ctxt.in_def) {
                            rb_warn0("END in method; use at_exit");
                        }
#if 0
                        {
                            NODE *scope = NEW_NODE(
                                NODE_SCOPE, 0 /* tbl */, (yyvsp[-1].val) /* body */, 0 /* args */, &(yyloc));
                            (yyval.val) = NEW_POSTEXE(scope, &(yyloc));
                        }
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(END,v1);(yyval.val)=v2;}
                    }
#line 10950 "ripper.c"
    break;

  case 34: /* stmt: mlhs '=' lex_ctxt command_call  */
#line 1825 "ripper.y"
                    {
#if 0
                        value_expr((yyvsp[0].val));
                        (yyval.val) = node_assign(p, (yyvsp[-3].val), (yyvsp[0].val), (yyvsp[-1].ctxt), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-3].val);v2=(yyvsp[0].val);v3=dispatch2(massign,v1,v2);(yyval.val)=v3;}
                    }
#line 10962 "ripper.c"
    break;

  case 35: /* stmt: lhs '=' lex_ctxt mrhs  */
#line 1833 "ripper.y"
                    {
#if 0
                        (yyval.val) = node_assign(p, (yyvsp[-3].val), (yyvsp[0].val), (yyvsp[-1].ctxt), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-3].val);v2=(yyvsp[0].val);v3=dispatch2(assign,v1,v2);(yyval.val)=v3;}
                    }
#line 10973 "ripper.c"
    break;

  case 36: /* stmt: mlhs '=' lex_ctxt mrhs_arg "`rescue' modifier" stmt  */
#line 1840 "ripper.y"
                    {
#if 0
                        YYLTYPE loc = code_loc_gen(&(yylsp[-1]), &(yylsp[0]));
                        (yyval.val) = node_assign(p, (yyvsp[-5].val), NEW_RESCUE((yyvsp[-2].val), NEW_RESBODY(0, remove_begin((yyvsp[0].val)), 0, &loc), 0, &(yyloc)), (yyvsp[-3].ctxt), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(rescue_mod,v1,v2);v4=(yyvsp[-5].val);v5=v3;v6=dispatch2(massign,v4,v5);(yyval.val)=v6;}
                    }
#line 10985 "ripper.c"
    break;

  case 37: /* stmt: mlhs '=' lex_ctxt mrhs_arg  */
#line 1848 "ripper.y"
                    {
#if 0
                        (yyval.val) = node_assign(p, (yyvsp[-3].val), (yyvsp[0].val), (yyvsp[-1].ctxt), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-3].val);v2=(yyvsp[0].val);v3=dispatch2(massign,v1,v2);(yyval.val)=v3;}
                    }
#line 10996 "ripper.c"
    break;

  case 39: /* stmt: error  */
#line 1856 "ripper.y"
                    {
                        (void)yynerrs;
#if 0
                        (yyval.val) = NEW_ERROR(&(yyloc));
#endif
                    }
#line 11007 "ripper.c"
    break;

  case 40: /* command_asgn: lhs '=' lex_ctxt command_rhs  */
#line 1865 "ripper.y"
                    {
#if 0
                        (yyval.val) = node_assign(p, (yyvsp[-3].val), (yyvsp[0].val), (yyvsp[-1].ctxt), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-3].val);v2=(yyvsp[0].val);v3=dispatch2(assign,v1,v2);(yyval.val)=v3;}
                    }
#line 11018 "ripper.c"
    break;

  case 41: /* command_asgn: var_lhs "operator-assignment" lex_ctxt command_rhs  */
#line 1872 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_op_assign(p, (yyvsp[-3].val), (yyvsp[-2].val), (yyvsp[0].val), (yyvsp[-1].ctxt), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-3].val);v2=(yyvsp[-2].val);v3=(yyvsp[0].val);v4=dispatch3(opassign,v1,v2,v3);(yyval.val)=v4;}
                    }
#line 11029 "ripper.c"
    break;

  case 42: /* command_asgn: primary_value '[' opt_call_args rbracket "operator-assignment" lex_ctxt command_rhs  */
#line 1879 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_ary_op_assign(p, (yyvsp[-6].val), (yyvsp[-4].val), (yyvsp[-2].val), (yyvsp[0].val), &(yylsp[-4]), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=(yyvsp[-6].val);v2=escape_Qundef((yyvsp[-4].val));v3=dispatch2(aref_field,v1,v2);v4=v3;v5=(yyvsp[-2].val);v6=(yyvsp[0].val);v7=dispatch3(opassign,v4,v5,v6);(yyval.val)=v7;}

                    }
#line 11041 "ripper.c"
    break;

  case 43: /* command_asgn: primary_value call_op "local variable or method" "operator-assignment" lex_ctxt command_rhs  */
#line 1887 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_attr_op_assign(p, (yyvsp[-5].val), (yyvsp[-4].val), (yyvsp[-3].val), (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8;v1=(yyvsp[-5].val);v2=(yyvsp[-4].val);v3=(yyvsp[-3].val);v4=dispatch3(field,v1,v2,v3);v5=v4;v6=(yyvsp[-2].val);v7=(yyvsp[0].val);v8=dispatch3(opassign,v5,v6,v7);(yyval.val)=v8;}
                    }
#line 11052 "ripper.c"
    break;

  case 44: /* command_asgn: primary_value call_op "constant" "operator-assignment" lex_ctxt command_rhs  */
#line 1894 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_attr_op_assign(p, (yyvsp[-5].val), (yyvsp[-4].val), (yyvsp[-3].val), (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8;v1=(yyvsp[-5].val);v2=(yyvsp[-4].val);v3=(yyvsp[-3].val);v4=dispatch3(field,v1,v2,v3);v5=v4;v6=(yyvsp[-2].val);v7=(yyvsp[0].val);v8=dispatch3(opassign,v5,v6,v7);(yyval.val)=v8;}
                    }
#line 11063 "ripper.c"
    break;

  case 45: /* command_asgn: primary_value "::" "constant" "operator-assignment" lex_ctxt command_rhs  */
#line 1901 "ripper.y"
                    {
#if 0
                        YYLTYPE loc = code_loc_gen(&(yylsp[-5]), &(yylsp[-3]));
                        (yyval.val) = new_const_op_assign(p, NEW_COLON2((yyvsp[-5].val), (yyvsp[-3].val), &loc), (yyvsp[-2].val), (yyvsp[0].val), (yyvsp[-1].ctxt), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=(yyvsp[-5].val);v2=(yyvsp[-3].val);v3=dispatch2(const_path_field,v1,v2);v4=v3;v5=(yyvsp[-2].val);v6=(yyvsp[0].val);v7=dispatch3(opassign,v4,v5,v6);(yyval.val)=v7;}
                    }
#line 11075 "ripper.c"
    break;

  case 46: /* command_asgn: primary_value "::" "local variable or method" "operator-assignment" lex_ctxt command_rhs  */
#line 1909 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_attr_op_assign(p, (yyvsp[-5].val), ID2VAL(idCOLON2), (yyvsp[-3].val), (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8;v1=(yyvsp[-5].val);v2=ID2VAL(idCOLON2);v3=(yyvsp[-3].val);v4=dispatch3(field,v1,v2,v3);v5=v4;v6=(yyvsp[-2].val);v7=(yyvsp[0].val);v8=dispatch3(opassign,v5,v6,v7);(yyval.val)=v8;}
                    }
#line 11086 "ripper.c"
    break;

  case 47: /* command_asgn: defn_head f_opt_paren_args '=' endless_command  */
#line 1916 "ripper.y"
                    {
                        endless_method_name(p, (yyvsp[-3].node), &(yylsp[-3]));
                        restore_defun(p, (yyvsp[-3].node)->nd_defn);
#if 0
                        (yyval.val) = set_defun_body(p, (yyvsp[-3].val), (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8,v9;v1=(yyvsp[0].val);v2=Qnil;v3=Qnil;v4=Qnil;v5=dispatch4(bodystmt,v1,v2,v3,v4);v6=get_value((yyvsp[-3].val));v7=(yyvsp[-2].val);v8=v5;v9=dispatch3(def,v6,v7,v8);(yyval.val)=v9;}
                        local_pop(p);
                    }
#line 11100 "ripper.c"
    break;

  case 48: /* command_asgn: defs_head f_opt_paren_args '=' endless_command  */
#line 1926 "ripper.y"
                    {
                        endless_method_name(p, (yyvsp[-3].node), &(yylsp[-3]));
                        restore_defun(p, (yyvsp[-3].node)->nd_defn);
#if 0
                        (yyval.val) = set_defun_body(p, (yyvsp[-3].val), (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
                        (yyvsp[-3].val) = get_value((yyvsp[-3].val));

			{VALUE v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11;v1=(yyvsp[0].val);v2=Qnil;v3=Qnil;v4=Qnil;v5=dispatch4(bodystmt,v1,v2,v3,v4);v6=AREF((yyvsp[-3].val), 0);v7=AREF((yyvsp[-3].val), 1);v8=AREF((yyvsp[-3].val), 2);v9=(yyvsp[-2].val);v10=v5;v11=dispatch5(defs,v6,v7,v8,v9,v10);(yyval.val)=v11;}
                        local_pop(p);
                    }
#line 11116 "ripper.c"
    break;

  case 49: /* command_asgn: backref "operator-assignment" lex_ctxt command_rhs  */
#line 1938 "ripper.y"
                    {
#if 0
                        rb_backref_error(p, (yyvsp[-3].val));
                        (yyval.val) = NEW_BEGIN(0, &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=var_field(p, (yyvsp[-3].val));v2=(yyvsp[0].val);v3=dispatch2(assign,v1,v2);(yyval.val)=backref_error(p, RNODE((yyvsp[-3].val)), v3);}ripper_error(p);
                    }
#line 11128 "ripper.c"
    break;

  case 51: /* endless_command: endless_command "`rescue' modifier" arg  */
#line 1949 "ripper.y"
                    {
#if 0
                        (yyval.val) = rescued_expr(p, (yyvsp[-2].val), (yyvsp[0].val), &(yylsp[-2]), &(yylsp[-1]), &(yylsp[0]));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(rescue_mod,v1,v2);(yyval.val)=v3;}
                    }
#line 11139 "ripper.c"
    break;

  case 52: /* endless_command: "`not'" opt_nl endless_command  */
#line 1956 "ripper.y"
                    {
                        (yyval.val) = call_uni_op(p, method_cond(p, (yyvsp[0].val), &(yylsp[0])), METHOD_NOT, &(yylsp[-2]), &(yyloc));
                    }
#line 11147 "ripper.c"
    break;

  case 53: /* command_rhs: command_call  */
#line 1962 "ripper.y"
                    {
                        value_expr((yyvsp[0].val));
                        (yyval.val) = (yyvsp[0].val);
                    }
#line 11156 "ripper.c"
    break;

  case 54: /* command_rhs: command_call "`rescue' modifier" stmt  */
#line 1967 "ripper.y"
                    {
#if 0
                        YYLTYPE loc = code_loc_gen(&(yylsp[-1]), &(yylsp[0]));
                        value_expr((yyvsp[-2].val));
                        (yyval.val) = NEW_RESCUE((yyvsp[-2].val), NEW_RESBODY(0, remove_begin((yyvsp[0].val)), 0, &loc), 0, &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(rescue_mod,v1,v2);(yyval.val)=v3;}
                    }
#line 11169 "ripper.c"
    break;

  case 57: /* expr: expr "`and'" expr  */
#line 1980 "ripper.y"
                    {
                        (yyval.val) = logop(p, idAND, (yyvsp[-2].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                    }
#line 11177 "ripper.c"
    break;

  case 58: /* expr: expr "`or'" expr  */
#line 1984 "ripper.y"
                    {
                        (yyval.val) = logop(p, idOR, (yyvsp[-2].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                    }
#line 11185 "ripper.c"
    break;

  case 59: /* expr: "`not'" opt_nl expr  */
#line 1988 "ripper.y"
                    {
                        (yyval.val) = call_uni_op(p, method_cond(p, (yyvsp[0].val), &(yylsp[0])), METHOD_NOT, &(yylsp[-2]), &(yyloc));
                    }
#line 11193 "ripper.c"
    break;

  case 60: /* expr: '!' command_call  */
#line 1992 "ripper.y"
                    {
                        (yyval.val) = call_uni_op(p, method_cond(p, (yyvsp[0].val), &(yylsp[0])), '!', &(yylsp[-1]), &(yyloc));
                    }
#line 11201 "ripper.c"
    break;

  case 61: /* @5: %empty  */
#line 1996 "ripper.y"
                    {
                        value_expr((yyvsp[-1].val));
                        SET_LEX_STATE(EXPR_BEG|EXPR_LABEL);
                        p->command_start = FALSE;
                        (yyvsp[0].ctxt) = p->ctxt;
                        p->ctxt.in_kwarg = 1;
                        (yyval.tbl) = push_pvtbl(p);
                    }
#line 11214 "ripper.c"
    break;

  case 62: /* @6: %empty  */
#line 2004 "ripper.y"
                    {
                        (yyval.tbl) = push_pktbl(p);
                    }
#line 11222 "ripper.c"
    break;

  case 63: /* expr: arg "=>" @5 @6 p_top_expr_body  */
#line 2008 "ripper.y"
                    {
                        pop_pktbl(p, (yyvsp[-1].tbl));
                        pop_pvtbl(p, (yyvsp[-2].tbl));
                        p->ctxt.in_kwarg = (yyvsp[-3].ctxt).in_kwarg;
#if 0
                        (yyval.val) = NEW_CASE3((yyvsp[-4].val), NEW_IN((yyvsp[0].val), 0, 0, &(yylsp[0])), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=(yyvsp[0].val);v2=Qnil;v3=Qnil;v4=dispatch3(in,v1,v2,v3);v5=(yyvsp[-4].val);v6=v4;v7=dispatch2(case,v5,v6);(yyval.val)=v7;}
                    }
#line 11236 "ripper.c"
    break;

  case 64: /* @7: %empty  */
#line 2018 "ripper.y"
                    {
                        value_expr((yyvsp[-1].val));
                        SET_LEX_STATE(EXPR_BEG|EXPR_LABEL);
                        p->command_start = FALSE;
                        (yyvsp[0].ctxt) = p->ctxt;
                        p->ctxt.in_kwarg = 1;
                        (yyval.tbl) = push_pvtbl(p);
                    }
#line 11249 "ripper.c"
    break;

  case 65: /* @8: %empty  */
#line 2026 "ripper.y"
                    {
                        (yyval.tbl) = push_pktbl(p);
                    }
#line 11257 "ripper.c"
    break;

  case 66: /* expr: arg "`in'" @7 @8 p_top_expr_body  */
#line 2030 "ripper.y"
                    {
                        pop_pktbl(p, (yyvsp[-1].tbl));
                        pop_pvtbl(p, (yyvsp[-2].tbl));
                        p->ctxt.in_kwarg = (yyvsp[-3].ctxt).in_kwarg;
#if 0
                        (yyval.val) = NEW_CASE3((yyvsp[-4].val), NEW_IN((yyvsp[0].val), NEW_TRUE(&(yylsp[0])), NEW_FALSE(&(yylsp[0])), &(yylsp[0])), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=(yyvsp[0].val);v2=Qnil;v3=Qnil;v4=dispatch3(in,v1,v2,v3);v5=(yyvsp[-4].val);v6=v4;v7=dispatch2(case,v5,v6);(yyval.val)=v7;}
                    }
#line 11271 "ripper.c"
    break;

  case 68: /* def_name: fname  */
#line 2043 "ripper.y"
                    {
                        ID fname = get_id((yyvsp[0].val));
                        ID cur_arg = p->cur_arg;
                        YYSTYPE c = {.ctxt = p->ctxt};
                        numparam_name(p, fname);
                        NODE *save =
                            NODE_NEW_TEMPORAL(NODE_SELF,
                                              /*head*/numparam_push(p),
                                              /*nth*/p->max_numparam,
                                              /*cval*/c.val);
                        local_push(p, 0);
                        p->cur_arg = 0;
                        p->ctxt.in_def = 1;
                        (yyval.node) = NEW_NODE(NODE_SELF, /*vid*/cur_arg, /*mid*/fname, /*args*/save, &(yyloc));
#if 0
#endif
                        (yyval.val) = NEW_RIPPER(fname, get_value((yyvsp[0].val)), (yyval.val), &NULL_LOC);

                    }
#line 11295 "ripper.c"
    break;

  case 69: /* defn_head: k_def def_name  */
#line 2065 "ripper.y"
                    {
                        (yyval.val) = (yyvsp[0].val);
#if 0
                        (yyval.val) = NEW_NODE(NODE_DEFN, 0, (yyval.val)->nd_mid, (yyval.val), &(yyloc));
#endif
                    }
#line 11306 "ripper.c"
    break;

  case 70: /* $@9: %empty  */
#line 2074 "ripper.y"
                    {
                        SET_LEX_STATE(EXPR_FNAME);
                        p->ctxt.in_argdef = 1;
                    }
#line 11315 "ripper.c"
    break;

  case 71: /* defs_head: k_def singleton dot_or_colon $@9 def_name  */
#line 2079 "ripper.y"
                    {
                        SET_LEX_STATE(EXPR_ENDFN|EXPR_LABEL); /* force for args */
                        (yyval.val) = (yyvsp[0].val);
#if 0
                        (yyval.val) = NEW_NODE(NODE_DEFS, (yyvsp[-3].val), (yyval.val)->nd_mid, (yyval.val), &(yyloc));
#endif
                        VALUE ary = rb_ary_new_from_args(3, (yyvsp[-3].val), (yyvsp[-2].val), get_value((yyval.val)));
                        add_mark_object(p, ary);
                        (yyval.node)->nd_rval = ary;

                    }
#line 11331 "ripper.c"
    break;

  case 72: /* expr_value: expr  */
#line 2093 "ripper.y"
                    {
                        value_expr((yyvsp[0].val));
                        (yyval.val) = (yyvsp[0].val);
                    }
#line 11340 "ripper.c"
    break;

  case 73: /* expr_value: error  */
#line 2098 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_ERROR(&(yyloc));
#endif
                    }
#line 11350 "ripper.c"
    break;

  case 74: /* $@10: %empty  */
#line 2105 "ripper.y"
                {COND_PUSH(1);}
#line 11356 "ripper.c"
    break;

  case 75: /* $@11: %empty  */
#line 2105 "ripper.y"
                                              {COND_POP();}
#line 11362 "ripper.c"
    break;

  case 76: /* expr_value_do: $@10 expr_value do $@11  */
#line 2106 "ripper.y"
                    {
                        (yyval.val) = (yyvsp[-2].val);
                    }
#line 11370 "ripper.c"
    break;

  case 80: /* block_command: block_call call_op2 operation2 command_args  */
#line 2117 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_qcall(p, (yyvsp[-2].val), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=(yyvsp[-3].val);v2=(yyvsp[-2].val);v3=(yyvsp[-1].val);v4=dispatch3(call,v1,v2,v3);v5=v4;v6=(yyvsp[0].val);v7=dispatch2(method_add_arg,v5,v6);(yyval.val)=v7;}
                    }
#line 11381 "ripper.c"
    break;

  case 81: /* cmd_brace_block: "{ arg" brace_body '}'  */
#line 2126 "ripper.y"
                    {
                        (yyval.val) = (yyvsp[-1].val);
#if 0
                        (yyval.val)->nd_body->nd_loc = code_loc_gen(&(yylsp[-2]), &(yylsp[0]));
                        nd_set_line((yyval.val), (yylsp[-2]).end_pos.lineno);
#endif
                    }
#line 11393 "ripper.c"
    break;

  case 82: /* fcall: operation  */
#line 2136 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_FCALL((yyvsp[0].val), 0, &(yyloc));
                        nd_set_line((yyval.val), p->tokline);
#endif
			(yyval.val)=(yyvsp[0].val);
                    }
#line 11405 "ripper.c"
    break;

  case 83: /* command: fcall command_args  */
#line 2146 "ripper.y"
                    {
#if 0
                        (yyvsp[-1].val)->nd_args = (yyvsp[0].val);
                        nd_set_last_loc((yyvsp[-1].val), (yylsp[0]).end_pos);
                        (yyval.val) = (yyvsp[-1].val);
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=(yyvsp[0].val);v3=dispatch2(command,v1,v2);(yyval.val)=v3;}
                    }
#line 11418 "ripper.c"
    break;

  case 84: /* command: fcall command_args cmd_brace_block  */
#line 2155 "ripper.y"
                    {
#if 0
                        block_dup_check(p, (yyvsp[-1].val), (yyvsp[0].val));
                        (yyvsp[-2].val)->nd_args = (yyvsp[-1].val);
                        (yyval.val) = method_add_block(p, (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
                        fixpos((yyval.val), (yyvsp[-2].val));
                        nd_set_last_loc((yyvsp[-2].val), (yylsp[-1]).end_pos);
#endif
			{VALUE v1,v2,v3,v4,v5,v6;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=dispatch2(command,v1,v2);v4=v3;v5=(yyvsp[0].val);v6=dispatch2(method_add_block,v4,v5);(yyval.val)=v6;}
                    }
#line 11433 "ripper.c"
    break;

  case 85: /* command: primary_value call_op operation2 command_args  */
#line 2166 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_command_qcall(p, (yyvsp[-2].val), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), Qnull, &(yylsp[-1]), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5;v1=(yyvsp[-3].val);v2=(yyvsp[-2].val);v3=(yyvsp[-1].val);v4=(yyvsp[0].val);v5=dispatch4(command_call,v1,v2,v3,v4);(yyval.val)=v5;}
                    }
#line 11444 "ripper.c"
    break;

  case 86: /* command: primary_value call_op operation2 command_args cmd_brace_block  */
#line 2173 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_command_qcall(p, (yyvsp[-3].val), (yyvsp[-4].val), (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-2]), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8;v1=(yyvsp[-4].val);v2=(yyvsp[-3].val);v3=(yyvsp[-2].val);v4=(yyvsp[-1].val);v5=dispatch4(command_call,v1,v2,v3,v4);v6=v5;v7=(yyvsp[0].val);v8=dispatch2(method_add_block,v6,v7);(yyval.val)=v8;}
                    }
#line 11455 "ripper.c"
    break;

  case 87: /* command: primary_value "::" operation2 command_args  */
#line 2180 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_command_qcall(p, ID2VAL(idCOLON2), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), Qnull, &(yylsp[-1]), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5;v1=(yyvsp[-3].val);v2=ID2VAL(idCOLON2);v3=(yyvsp[-1].val);v4=(yyvsp[0].val);v5=dispatch4(command_call,v1,v2,v3,v4);(yyval.val)=v5;}
                    }
#line 11466 "ripper.c"
    break;

  case 88: /* command: primary_value "::" operation2 command_args cmd_brace_block  */
#line 2187 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_command_qcall(p, ID2VAL(idCOLON2), (yyvsp[-4].val), (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-2]), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8;v1=(yyvsp[-4].val);v2=ID2VAL(idCOLON2);v3=(yyvsp[-2].val);v4=(yyvsp[-1].val);v5=dispatch4(command_call,v1,v2,v3,v4);v6=v5;v7=(yyvsp[0].val);v8=dispatch2(method_add_block,v6,v7);(yyval.val)=v8;}
                   }
#line 11477 "ripper.c"
    break;

  case 89: /* command: "`super'" command_args  */
#line 2194 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_SUPER((yyvsp[0].val), &(yyloc));
                        fixpos((yyval.val), (yyvsp[0].val));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(super,v1);(yyval.val)=v2;}
                    }
#line 11489 "ripper.c"
    break;

  case 90: /* command: "`yield'" command_args  */
#line 2202 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_yield(p, (yyvsp[0].val), &(yyloc));
                        fixpos((yyval.val), (yyvsp[0].val));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(yield,v1);(yyval.val)=v2;}
                    }
#line 11501 "ripper.c"
    break;

  case 91: /* command: k_return call_args  */
#line 2210 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_RETURN(ret_args(p, (yyvsp[0].val)), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(return,v1);(yyval.val)=v2;}
                    }
#line 11512 "ripper.c"
    break;

  case 92: /* command: "`break'" call_args  */
#line 2217 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_BREAK(ret_args(p, (yyvsp[0].val)), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(break,v1);(yyval.val)=v2;}
                    }
#line 11523 "ripper.c"
    break;

  case 93: /* command: "`next'" call_args  */
#line 2224 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_NEXT(ret_args(p, (yyvsp[0].val)), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(next,v1);(yyval.val)=v2;}
                    }
#line 11534 "ripper.c"
    break;

  case 95: /* mlhs: "(" mlhs_inner rparen  */
#line 2234 "ripper.y"
                    {
#if 0
                        (yyval.val) = (yyvsp[-1].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(mlhs_paren,v1);(yyval.val)=v2;}
                    }
#line 11545 "ripper.c"
    break;

  case 97: /* mlhs_inner: "(" mlhs_inner rparen  */
#line 2244 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_MASGN(NEW_LIST((yyvsp[-1].val), &(yyloc)), 0, &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(mlhs_paren,v1);(yyval.val)=v2;}
                    }
#line 11556 "ripper.c"
    break;

  case 98: /* mlhs_basic: mlhs_head  */
#line 2253 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_MASGN((yyvsp[0].val), 0, &(yyloc));
#endif
			(yyval.val)=(yyvsp[0].val);
                    }
#line 11567 "ripper.c"
    break;

  case 99: /* mlhs_basic: mlhs_head mlhs_item  */
#line 2260 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_MASGN(list_append(p, (yyvsp[-1].val),(yyvsp[0].val)), 0, &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=(yyvsp[0].val);v3=dispatch2(mlhs_add,v1,v2);(yyval.val)=v3;}
                    }
#line 11578 "ripper.c"
    break;

  case 100: /* mlhs_basic: mlhs_head "*" mlhs_node  */
#line 2267 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_MASGN((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(mlhs_add_star,v1,v2);(yyval.val)=v3;}
                    }
#line 11589 "ripper.c"
    break;

  case 101: /* mlhs_basic: mlhs_head "*" mlhs_node ',' mlhs_post  */
#line 2274 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_MASGN((yyvsp[-4].val), NEW_POSTARG((yyvsp[-2].val),(yyvsp[0].val),&(yyloc)), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6;v1=(yyvsp[-4].val);v2=(yyvsp[-2].val);v3=dispatch2(mlhs_add_star,v1,v2);v4=v3;v5=(yyvsp[0].val);v6=dispatch2(mlhs_add_post,v4,v5);(yyval.val)=v6;}
                    }
#line 11600 "ripper.c"
    break;

  case 102: /* mlhs_basic: mlhs_head "*"  */
#line 2281 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_MASGN((yyvsp[-1].val), NODE_SPECIAL_NO_NAME_REST, &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=Qnil;v3=dispatch2(mlhs_add_star,v1,v2);(yyval.val)=v3;}
                    }
#line 11611 "ripper.c"
    break;

  case 103: /* mlhs_basic: mlhs_head "*" ',' mlhs_post  */
#line 2288 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_MASGN((yyvsp[-3].val), NEW_POSTARG(NODE_SPECIAL_NO_NAME_REST, (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6;v1=(yyvsp[-3].val);v2=Qnil;v3=dispatch2(mlhs_add_star,v1,v2);v4=v3;v5=(yyvsp[0].val);v6=dispatch2(mlhs_add_post,v4,v5);(yyval.val)=v6;}
                    }
#line 11622 "ripper.c"
    break;

  case 104: /* mlhs_basic: "*" mlhs_node  */
#line 2295 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_MASGN(0, (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(mlhs_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(mlhs_add_star,v2,v3);(yyval.val)=v4;}
                    }
#line 11633 "ripper.c"
    break;

  case 105: /* mlhs_basic: "*" mlhs_node ',' mlhs_post  */
#line 2302 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_MASGN(0, NEW_POSTARG((yyvsp[-2].val),(yyvsp[0].val),&(yyloc)), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=dispatch0(mlhs_new);v2=v1;v3=(yyvsp[-2].val);v4=dispatch2(mlhs_add_star,v2,v3);v5=v4;v6=(yyvsp[0].val);v7=dispatch2(mlhs_add_post,v5,v6);(yyval.val)=v7;}
                    }
#line 11644 "ripper.c"
    break;

  case 106: /* mlhs_basic: "*"  */
#line 2309 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_MASGN(0, NODE_SPECIAL_NO_NAME_REST, &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(mlhs_new);v2=v1;v3=Qnil;v4=dispatch2(mlhs_add_star,v2,v3);(yyval.val)=v4;}
                    }
#line 11655 "ripper.c"
    break;

  case 107: /* mlhs_basic: "*" ',' mlhs_post  */
#line 2316 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_MASGN(0, NEW_POSTARG(NODE_SPECIAL_NO_NAME_REST, (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=dispatch0(mlhs_new);v2=v1;v3=Qnil;v4=dispatch2(mlhs_add_star,v2,v3);v5=v4;v6=(yyvsp[0].val);v7=dispatch2(mlhs_add_post,v5,v6);(yyval.val)=v7;}
                    }
#line 11666 "ripper.c"
    break;

  case 109: /* mlhs_item: "(" mlhs_inner rparen  */
#line 2326 "ripper.y"
                    {
#if 0
                        (yyval.val) = (yyvsp[-1].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(mlhs_paren,v1);(yyval.val)=v2;}
                    }
#line 11677 "ripper.c"
    break;

  case 110: /* mlhs_head: mlhs_item ','  */
#line 2335 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_LIST((yyvsp[-1].val), &(yylsp[-1]));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(mlhs_new);v2=v1;v3=(yyvsp[-1].val);v4=dispatch2(mlhs_add,v2,v3);(yyval.val)=v4;}
                    }
#line 11688 "ripper.c"
    break;

  case 111: /* mlhs_head: mlhs_head mlhs_item ','  */
#line 2342 "ripper.y"
                    {
#if 0
                        (yyval.val) = list_append(p, (yyvsp[-2].val), (yyvsp[-1].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=dispatch2(mlhs_add,v1,v2);(yyval.val)=v3;}
                    }
#line 11699 "ripper.c"
    break;

  case 112: /* mlhs_post: mlhs_item  */
#line 2351 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_LIST((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(mlhs_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(mlhs_add,v2,v3);(yyval.val)=v4;}
                    }
#line 11710 "ripper.c"
    break;

  case 113: /* mlhs_post: mlhs_post ',' mlhs_item  */
#line 2358 "ripper.y"
                    {
#if 0
                        (yyval.val) = list_append(p, (yyvsp[-2].val), (yyvsp[0].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(mlhs_add,v1,v2);(yyval.val)=v3;}
                    }
#line 11721 "ripper.c"
    break;

  case 114: /* mlhs_node: user_variable  */
#line 2367 "ripper.y"
                    {
#if 0
                        (yyval.val) = assignable(p, (yyvsp[0].val), 0, &(yyloc));
#endif
			(yyval.val)=assignable(p, var_field(p, (yyvsp[0].val)));
                    }
#line 11732 "ripper.c"
    break;

  case 115: /* mlhs_node: keyword_variable  */
#line 2374 "ripper.y"
                    {
#if 0
                        (yyval.val) = assignable(p, (yyvsp[0].val), 0, &(yyloc));
#endif
			(yyval.val)=assignable(p, var_field(p, (yyvsp[0].val)));
                    }
#line 11743 "ripper.c"
    break;

  case 116: /* mlhs_node: primary_value '[' opt_call_args rbracket  */
#line 2381 "ripper.y"
                    {
#if 0
                        (yyval.val) = aryset(p, (yyvsp[-3].val), (yyvsp[-1].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-3].val);v2=escape_Qundef((yyvsp[-1].val));v3=dispatch2(aref_field,v1,v2);(yyval.val)=v3;}
                    }
#line 11754 "ripper.c"
    break;

  case 117: /* mlhs_node: primary_value call_op "local variable or method"  */
#line 2388 "ripper.y"
                    {
                        if ((yyvsp[-1].val) == tANDDOT) {
                            yyerror1(&(yylsp[-1]), "&. inside multiple assignment destination");
                        }
#if 0
                        (yyval.val) = attrset(p, (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=(yyvsp[0].val);v4=dispatch3(field,v1,v2,v3);(yyval.val)=v4;}
                    }
#line 11768 "ripper.c"
    break;

  case 118: /* mlhs_node: primary_value "::" "local variable or method"  */
#line 2398 "ripper.y"
                    {
#if 0
                        (yyval.val) = attrset(p, (yyvsp[-2].val), idCOLON2, (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(const_path_field,v1,v2);(yyval.val)=v3;}
                    }
#line 11779 "ripper.c"
    break;

  case 119: /* mlhs_node: primary_value call_op "constant"  */
#line 2405 "ripper.y"
                    {
                        if ((yyvsp[-1].val) == tANDDOT) {
                            yyerror1(&(yylsp[-1]), "&. inside multiple assignment destination");
                        }
#if 0
                        (yyval.val) = attrset(p, (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=(yyvsp[0].val);v4=dispatch3(field,v1,v2,v3);(yyval.val)=v4;}
                    }
#line 11793 "ripper.c"
    break;

  case 120: /* mlhs_node: primary_value "::" "constant"  */
#line 2415 "ripper.y"
                    {
#if 0
                        (yyval.val) = const_decl(p, NEW_COLON2((yyvsp[-2].val), (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(const_path_field,v1,v2);(yyval.val)=const_decl(p, v3);}
                    }
#line 11804 "ripper.c"
    break;

  case 121: /* mlhs_node: ":: at EXPR_BEG" "constant"  */
#line 2422 "ripper.y"
                    {
#if 0
                        (yyval.val) = const_decl(p, NEW_COLON3((yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(top_const_field,v1);(yyval.val)=const_decl(p, v2);}
                    }
#line 11815 "ripper.c"
    break;

  case 122: /* mlhs_node: backref  */
#line 2429 "ripper.y"
                    {
#if 0
                        rb_backref_error(p, (yyvsp[0].val));
                        (yyval.val) = NEW_BEGIN(0, &(yyloc));
#endif
			(yyval.val)=backref_error(p, RNODE((yyvsp[0].val)), var_field(p, (yyvsp[0].val)));ripper_error(p);
                    }
#line 11827 "ripper.c"
    break;

  case 123: /* lhs: user_variable  */
#line 2439 "ripper.y"
                    {
#if 0
                        (yyval.val) = assignable(p, (yyvsp[0].val), 0, &(yyloc));
#endif
			(yyval.val)=assignable(p, var_field(p, (yyvsp[0].val)));
                    }
#line 11838 "ripper.c"
    break;

  case 124: /* lhs: keyword_variable  */
#line 2446 "ripper.y"
                    {
#if 0
                        (yyval.val) = assignable(p, (yyvsp[0].val), 0, &(yyloc));
#endif
			(yyval.val)=assignable(p, var_field(p, (yyvsp[0].val)));
                    }
#line 11849 "ripper.c"
    break;

  case 125: /* lhs: primary_value '[' opt_call_args rbracket  */
#line 2453 "ripper.y"
                    {
#if 0
                        (yyval.val) = aryset(p, (yyvsp[-3].val), (yyvsp[-1].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-3].val);v2=escape_Qundef((yyvsp[-1].val));v3=dispatch2(aref_field,v1,v2);(yyval.val)=v3;}
                    }
#line 11860 "ripper.c"
    break;

  case 126: /* lhs: primary_value call_op "local variable or method"  */
#line 2460 "ripper.y"
                    {
#if 0
                        (yyval.val) = attrset(p, (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=(yyvsp[0].val);v4=dispatch3(field,v1,v2,v3);(yyval.val)=v4;}
                    }
#line 11871 "ripper.c"
    break;

  case 127: /* lhs: primary_value "::" "local variable or method"  */
#line 2467 "ripper.y"
                    {
#if 0
                        (yyval.val) = attrset(p, (yyvsp[-2].val), idCOLON2, (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-2].val);v2=ID2VAL(idCOLON2);v3=(yyvsp[0].val);v4=dispatch3(field,v1,v2,v3);(yyval.val)=v4;}
                    }
#line 11882 "ripper.c"
    break;

  case 128: /* lhs: primary_value call_op "constant"  */
#line 2474 "ripper.y"
                    {
#if 0
                        (yyval.val) = attrset(p, (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=(yyvsp[0].val);v4=dispatch3(field,v1,v2,v3);(yyval.val)=v4;}
                    }
#line 11893 "ripper.c"
    break;

  case 129: /* lhs: primary_value "::" "constant"  */
#line 2481 "ripper.y"
                    {
#if 0
                        (yyval.val) = const_decl(p, NEW_COLON2((yyvsp[-2].val), (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(const_path_field,v1,v2);(yyval.val)=const_decl(p, v3);}
                    }
#line 11904 "ripper.c"
    break;

  case 130: /* lhs: ":: at EXPR_BEG" "constant"  */
#line 2488 "ripper.y"
                    {
#if 0
                        (yyval.val) = const_decl(p, NEW_COLON3((yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(top_const_field,v1);(yyval.val)=const_decl(p, v2);}
                    }
#line 11915 "ripper.c"
    break;

  case 131: /* lhs: backref  */
#line 2495 "ripper.y"
                    {
#if 0
                        rb_backref_error(p, (yyvsp[0].val));
                        (yyval.val) = NEW_BEGIN(0, &(yyloc));
#endif
			(yyval.val)=backref_error(p, RNODE((yyvsp[0].val)), var_field(p, (yyvsp[0].val)));ripper_error(p);
                    }
#line 11927 "ripper.c"
    break;

  case 132: /* cname: "local variable or method"  */
#line 2505 "ripper.y"
                    {
                        static const char mesg[] = "class/module name must be CONSTANT";
#if 0
                        yyerror1(&(yylsp[0]), mesg);
#endif
			{VALUE v1,v2,v3;v1=ERR_MESG();v2=(yyvsp[0].val);v3=dispatch2(class_name_error,v1,v2);(yyval.val)=v3;}ripper_error(p);
                    }
#line 11939 "ripper.c"
    break;

  case 134: /* cpath: ":: at EXPR_BEG" cname  */
#line 2516 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_COLON3((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(top_const_ref,v1);(yyval.val)=v2;}
                    }
#line 11950 "ripper.c"
    break;

  case 135: /* cpath: cname  */
#line 2523 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_COLON2(0, (yyval.val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(const_ref,v1);(yyval.val)=v2;}
                    }
#line 11961 "ripper.c"
    break;

  case 136: /* cpath: primary_value "::" cname  */
#line 2530 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_COLON2((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(const_path_ref,v1,v2);(yyval.val)=v3;}
                    }
#line 11972 "ripper.c"
    break;

  case 140: /* fname: op  */
#line 2542 "ripper.y"
                    {
                        SET_LEX_STATE(EXPR_ENDFN);
                        (yyval.val) = (yyvsp[0].val);
                    }
#line 11981 "ripper.c"
    break;

  case 142: /* fitem: fname  */
#line 2550 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_LIT(ID2SYM((yyvsp[0].val)), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(symbol_literal,v1);(yyval.val)=v2;}
                    }
#line 11992 "ripper.c"
    break;

  case 144: /* undef_list: fitem  */
#line 2560 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_UNDEF((yyvsp[0].val), &(yyloc));
#endif
			(yyval.val)=rb_ary_new3(1, get_value((yyvsp[0].val)));
                    }
#line 12003 "ripper.c"
    break;

  case 145: /* $@12: %empty  */
#line 2566 "ripper.y"
                                 {SET_LEX_STATE(EXPR_FNAME|EXPR_FITEM);}
#line 12009 "ripper.c"
    break;

  case 146: /* undef_list: undef_list ',' $@12 fitem  */
#line 2567 "ripper.y"
                    {
#if 0
                        NODE *undef = NEW_UNDEF((yyvsp[0].val), &(yylsp[0]));
                        (yyval.val) = block_append(p, (yyvsp[-3].val), undef);
#endif
			(yyval.val)=rb_ary_push((yyvsp[-3].val), get_value((yyvsp[0].val)));
                    }
#line 12021 "ripper.c"
    break;

  case 147: /* op: '|'  */
#line 2576 "ripper.y"
           { ifndef_ripper((yyval.val) = '|'); }
#line 12027 "ripper.c"
    break;

  case 148: /* op: '^'  */
#line 2577 "ripper.y"
                       { ifndef_ripper((yyval.val) = '^'); }
#line 12033 "ripper.c"
    break;

  case 149: /* op: '&'  */
#line 2578 "ripper.y"
                       { ifndef_ripper((yyval.val) = '&'); }
#line 12039 "ripper.c"
    break;

  case 150: /* op: "<=>"  */
#line 2579 "ripper.y"
                        { ifndef_ripper((yyval.val) = tCMP); }
#line 12045 "ripper.c"
    break;

  case 151: /* op: "=="  */
#line 2580 "ripper.y"
                       { ifndef_ripper((yyval.val) = tEQ); }
#line 12051 "ripper.c"
    break;

  case 152: /* op: "==="  */
#line 2581 "ripper.y"
                        { ifndef_ripper((yyval.val) = tEQQ); }
#line 12057 "ripper.c"
    break;

  case 153: /* op: "=~"  */
#line 2582 "ripper.y"
                         { ifndef_ripper((yyval.val) = tMATCH); }
#line 12063 "ripper.c"
    break;

  case 154: /* op: "!~"  */
#line 2583 "ripper.y"
                          { ifndef_ripper((yyval.val) = tNMATCH); }
#line 12069 "ripper.c"
    break;

  case 155: /* op: '>'  */
#line 2584 "ripper.y"
                       { ifndef_ripper((yyval.val) = '>'); }
#line 12075 "ripper.c"
    break;

  case 156: /* op: ">="  */
#line 2585 "ripper.y"
                        { ifndef_ripper((yyval.val) = tGEQ); }
#line 12081 "ripper.c"
    break;

  case 157: /* op: '<'  */
#line 2586 "ripper.y"
                       { ifndef_ripper((yyval.val) = '<'); }
#line 12087 "ripper.c"
    break;

  case 158: /* op: "<="  */
#line 2587 "ripper.y"
                        { ifndef_ripper((yyval.val) = tLEQ); }
#line 12093 "ripper.c"
    break;

  case 159: /* op: "!="  */
#line 2588 "ripper.y"
                        { ifndef_ripper((yyval.val) = tNEQ); }
#line 12099 "ripper.c"
    break;

  case 160: /* op: "<<"  */
#line 2589 "ripper.y"
                         { ifndef_ripper((yyval.val) = tLSHFT); }
#line 12105 "ripper.c"
    break;

  case 161: /* op: ">>"  */
#line 2590 "ripper.y"
                         { ifndef_ripper((yyval.val) = tRSHFT); }
#line 12111 "ripper.c"
    break;

  case 162: /* op: '+'  */
#line 2591 "ripper.y"
                       { ifndef_ripper((yyval.val) = '+'); }
#line 12117 "ripper.c"
    break;

  case 163: /* op: '-'  */
#line 2592 "ripper.y"
                       { ifndef_ripper((yyval.val) = '-'); }
#line 12123 "ripper.c"
    break;

  case 164: /* op: '*'  */
#line 2593 "ripper.y"
                       { ifndef_ripper((yyval.val) = '*'); }
#line 12129 "ripper.c"
    break;

  case 165: /* op: "*"  */
#line 2594 "ripper.y"
                         { ifndef_ripper((yyval.val) = '*'); }
#line 12135 "ripper.c"
    break;

  case 166: /* op: '/'  */
#line 2595 "ripper.y"
                       { ifndef_ripper((yyval.val) = '/'); }
#line 12141 "ripper.c"
    break;

  case 167: /* op: '%'  */
#line 2596 "ripper.y"
                       { ifndef_ripper((yyval.val) = '%'); }
#line 12147 "ripper.c"
    break;

  case 168: /* op: "**"  */
#line 2597 "ripper.y"
                        { ifndef_ripper((yyval.val) = tPOW); }
#line 12153 "ripper.c"
    break;

  case 169: /* op: "**arg"  */
#line 2598 "ripper.y"
                         { ifndef_ripper((yyval.val) = tDSTAR); }
#line 12159 "ripper.c"
    break;

  case 170: /* op: '!'  */
#line 2599 "ripper.y"
                       { ifndef_ripper((yyval.val) = '!'); }
#line 12165 "ripper.c"
    break;

  case 171: /* op: '~'  */
#line 2600 "ripper.y"
                       { ifndef_ripper((yyval.val) = '~'); }
#line 12171 "ripper.c"
    break;

  case 172: /* op: "unary+"  */
#line 2601 "ripper.y"
                         { ifndef_ripper((yyval.val) = tUPLUS); }
#line 12177 "ripper.c"
    break;

  case 173: /* op: "unary-"  */
#line 2602 "ripper.y"
                          { ifndef_ripper((yyval.val) = tUMINUS); }
#line 12183 "ripper.c"
    break;

  case 174: /* op: "[]"  */
#line 2603 "ripper.y"
                         { ifndef_ripper((yyval.val) = tAREF); }
#line 12189 "ripper.c"
    break;

  case 175: /* op: "[]="  */
#line 2604 "ripper.y"
                         { ifndef_ripper((yyval.val) = tASET); }
#line 12195 "ripper.c"
    break;

  case 176: /* op: '`'  */
#line 2605 "ripper.y"
                       { ifndef_ripper((yyval.val) = '`'); }
#line 12201 "ripper.c"
    break;

  case 218: /* arg: lhs '=' lex_ctxt arg_rhs  */
#line 2623 "ripper.y"
                    {
#if 0
                        (yyval.val) = node_assign(p, (yyvsp[-3].val), (yyvsp[0].val), (yyvsp[-1].ctxt), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-3].val);v2=(yyvsp[0].val);v3=dispatch2(assign,v1,v2);(yyval.val)=v3;}
                    }
#line 12212 "ripper.c"
    break;

  case 219: /* arg: var_lhs "operator-assignment" lex_ctxt arg_rhs  */
#line 2630 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_op_assign(p, (yyvsp[-3].val), (yyvsp[-2].val), (yyvsp[0].val), (yyvsp[-1].ctxt), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-3].val);v2=(yyvsp[-2].val);v3=(yyvsp[0].val);v4=dispatch3(opassign,v1,v2,v3);(yyval.val)=v4;}
                    }
#line 12223 "ripper.c"
    break;

  case 220: /* arg: primary_value '[' opt_call_args rbracket "operator-assignment" lex_ctxt arg_rhs  */
#line 2637 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_ary_op_assign(p, (yyvsp[-6].val), (yyvsp[-4].val), (yyvsp[-2].val), (yyvsp[0].val), &(yylsp[-4]), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=(yyvsp[-6].val);v2=escape_Qundef((yyvsp[-4].val));v3=dispatch2(aref_field,v1,v2);v4=v3;v5=(yyvsp[-2].val);v6=(yyvsp[0].val);v7=dispatch3(opassign,v4,v5,v6);(yyval.val)=v7;}
                    }
#line 12234 "ripper.c"
    break;

  case 221: /* arg: primary_value call_op "local variable or method" "operator-assignment" lex_ctxt arg_rhs  */
#line 2644 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_attr_op_assign(p, (yyvsp[-5].val), (yyvsp[-4].val), (yyvsp[-3].val), (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8;v1=(yyvsp[-5].val);v2=(yyvsp[-4].val);v3=(yyvsp[-3].val);v4=dispatch3(field,v1,v2,v3);v5=v4;v6=(yyvsp[-2].val);v7=(yyvsp[0].val);v8=dispatch3(opassign,v5,v6,v7);(yyval.val)=v8;}
                    }
#line 12245 "ripper.c"
    break;

  case 222: /* arg: primary_value call_op "constant" "operator-assignment" lex_ctxt arg_rhs  */
#line 2651 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_attr_op_assign(p, (yyvsp[-5].val), (yyvsp[-4].val), (yyvsp[-3].val), (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8;v1=(yyvsp[-5].val);v2=(yyvsp[-4].val);v3=(yyvsp[-3].val);v4=dispatch3(field,v1,v2,v3);v5=v4;v6=(yyvsp[-2].val);v7=(yyvsp[0].val);v8=dispatch3(opassign,v5,v6,v7);(yyval.val)=v8;}
                    }
#line 12256 "ripper.c"
    break;

  case 223: /* arg: primary_value "::" "local variable or method" "operator-assignment" lex_ctxt arg_rhs  */
#line 2658 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_attr_op_assign(p, (yyvsp[-5].val), ID2VAL(idCOLON2), (yyvsp[-3].val), (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8;v1=(yyvsp[-5].val);v2=ID2VAL(idCOLON2);v3=(yyvsp[-3].val);v4=dispatch3(field,v1,v2,v3);v5=v4;v6=(yyvsp[-2].val);v7=(yyvsp[0].val);v8=dispatch3(opassign,v5,v6,v7);(yyval.val)=v8;}
                    }
#line 12267 "ripper.c"
    break;

  case 224: /* arg: primary_value "::" "constant" "operator-assignment" lex_ctxt arg_rhs  */
#line 2665 "ripper.y"
                    {
#if 0
                        YYLTYPE loc = code_loc_gen(&(yylsp[-5]), &(yylsp[-3]));
                        (yyval.val) = new_const_op_assign(p, NEW_COLON2((yyvsp[-5].val), (yyvsp[-3].val), &loc), (yyvsp[-2].val), (yyvsp[0].val), (yyvsp[-1].ctxt), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=(yyvsp[-5].val);v2=(yyvsp[-3].val);v3=dispatch2(const_path_field,v1,v2);v4=v3;v5=(yyvsp[-2].val);v6=(yyvsp[0].val);v7=dispatch3(opassign,v4,v5,v6);(yyval.val)=v7;}
                    }
#line 12279 "ripper.c"
    break;

  case 225: /* arg: ":: at EXPR_BEG" "constant" "operator-assignment" lex_ctxt arg_rhs  */
#line 2673 "ripper.y"
                    {
#if 0
                        YYLTYPE loc = code_loc_gen(&(yylsp[-4]), &(yylsp[-3]));
                        (yyval.val) = new_const_op_assign(p, NEW_COLON3((yyvsp[-3].val), &loc), (yyvsp[-2].val), (yyvsp[0].val), (yyvsp[-1].ctxt), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6;v1=(yyvsp[-3].val);v2=dispatch1(top_const_field,v1);v3=v2;v4=(yyvsp[-2].val);v5=(yyvsp[0].val);v6=dispatch3(opassign,v3,v4,v5);(yyval.val)=v6;}
                    }
#line 12291 "ripper.c"
    break;

  case 226: /* arg: backref "operator-assignment" lex_ctxt arg_rhs  */
#line 2681 "ripper.y"
                    {
#if 0
                        rb_backref_error(p, (yyvsp[-3].val));
                        (yyval.val) = NEW_BEGIN(0, &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=var_field(p, (yyvsp[-3].val));v2=(yyvsp[-2].val);v3=(yyvsp[0].val);v4=dispatch3(opassign,v1,v2,v3);(yyval.val)=backref_error(p, RNODE((yyvsp[-3].val)), v4);}ripper_error(p);
                    }
#line 12303 "ripper.c"
    break;

  case 227: /* arg: arg ".." arg  */
#line 2689 "ripper.y"
                    {
#if 0
                        value_expr((yyvsp[-2].val));
                        value_expr((yyvsp[0].val));
                        (yyval.val) = NEW_DOT2((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(dot2,v1,v2);(yyval.val)=v3;}
                    }
#line 12316 "ripper.c"
    break;

  case 228: /* arg: arg "..." arg  */
#line 2698 "ripper.y"
                    {
#if 0
                        value_expr((yyvsp[-2].val));
                        value_expr((yyvsp[0].val));
                        (yyval.val) = NEW_DOT3((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(dot3,v1,v2);(yyval.val)=v3;}
                    }
#line 12329 "ripper.c"
    break;

  case 229: /* arg: arg ".."  */
#line 2707 "ripper.y"
                    {
#if 0
                        value_expr((yyvsp[-1].val));
                        (yyval.val) = NEW_DOT2((yyvsp[-1].val), new_nil_at(p, &(yylsp[0]).end_pos), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=Qnil;v3=dispatch2(dot2,v1,v2);(yyval.val)=v3;}
                    }
#line 12341 "ripper.c"
    break;

  case 230: /* arg: arg "..."  */
#line 2715 "ripper.y"
                    {
#if 0
                        value_expr((yyvsp[-1].val));
                        (yyval.val) = NEW_DOT3((yyvsp[-1].val), new_nil_at(p, &(yylsp[0]).end_pos), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=Qnil;v3=dispatch2(dot3,v1,v2);(yyval.val)=v3;}
                    }
#line 12353 "ripper.c"
    break;

  case 231: /* arg: "(.." arg  */
#line 2723 "ripper.y"
                    {
#if 0
                        value_expr((yyvsp[0].val));
                        (yyval.val) = NEW_DOT2(new_nil_at(p, &(yylsp[-1]).beg_pos), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=Qnil;v2=(yyvsp[0].val);v3=dispatch2(dot2,v1,v2);(yyval.val)=v3;}
                    }
#line 12365 "ripper.c"
    break;

  case 232: /* arg: "(..." arg  */
#line 2731 "ripper.y"
                    {
#if 0
                        value_expr((yyvsp[0].val));
                        (yyval.val) = NEW_DOT3(new_nil_at(p, &(yylsp[-1]).beg_pos), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=Qnil;v2=(yyvsp[0].val);v3=dispatch2(dot3,v1,v2);(yyval.val)=v3;}
                    }
#line 12377 "ripper.c"
    break;

  case 233: /* arg: arg '+' arg  */
#line 2739 "ripper.y"
                    {
                        (yyval.val) = call_bin_op(p, (yyvsp[-2].val), '+', (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                    }
#line 12385 "ripper.c"
    break;

  case 234: /* arg: arg '-' arg  */
#line 2743 "ripper.y"
                    {
                        (yyval.val) = call_bin_op(p, (yyvsp[-2].val), '-', (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                    }
#line 12393 "ripper.c"
    break;

  case 235: /* arg: arg '*' arg  */
#line 2747 "ripper.y"
                    {
                        (yyval.val) = call_bin_op(p, (yyvsp[-2].val), '*', (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                    }
#line 12401 "ripper.c"
    break;

  case 236: /* arg: arg '/' arg  */
#line 2751 "ripper.y"
                    {
                        (yyval.val) = call_bin_op(p, (yyvsp[-2].val), '/', (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                    }
#line 12409 "ripper.c"
    break;

  case 237: /* arg: arg '%' arg  */
#line 2755 "ripper.y"
                    {
                        (yyval.val) = call_bin_op(p, (yyvsp[-2].val), '%', (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                    }
#line 12417 "ripper.c"
    break;

  case 238: /* arg: arg "**" arg  */
#line 2759 "ripper.y"
                    {
                        (yyval.val) = call_bin_op(p, (yyvsp[-2].val), idPow, (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                    }
#line 12425 "ripper.c"
    break;

  case 239: /* arg: tUMINUS_NUM simple_numeric "**" arg  */
#line 2763 "ripper.y"
                    {
                        (yyval.val) = call_uni_op(p, call_bin_op(p, (yyvsp[-2].val), idPow, (yyvsp[0].val), &(yylsp[-2]), &(yyloc)), idUMinus, &(yylsp[-3]), &(yyloc));
                    }
#line 12433 "ripper.c"
    break;

  case 240: /* arg: "unary+" arg  */
#line 2767 "ripper.y"
                    {
                        (yyval.val) = call_uni_op(p, (yyvsp[0].val), idUPlus, &(yylsp[-1]), &(yyloc));
                    }
#line 12441 "ripper.c"
    break;

  case 241: /* arg: "unary-" arg  */
#line 2771 "ripper.y"
                    {
                        (yyval.val) = call_uni_op(p, (yyvsp[0].val), idUMinus, &(yylsp[-1]), &(yyloc));
                    }
#line 12449 "ripper.c"
    break;

  case 242: /* arg: arg '|' arg  */
#line 2775 "ripper.y"
                    {
                        (yyval.val) = call_bin_op(p, (yyvsp[-2].val), '|', (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                    }
#line 12457 "ripper.c"
    break;

  case 243: /* arg: arg '^' arg  */
#line 2779 "ripper.y"
                    {
                        (yyval.val) = call_bin_op(p, (yyvsp[-2].val), '^', (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                    }
#line 12465 "ripper.c"
    break;

  case 244: /* arg: arg '&' arg  */
#line 2783 "ripper.y"
                    {
                        (yyval.val) = call_bin_op(p, (yyvsp[-2].val), '&', (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                    }
#line 12473 "ripper.c"
    break;

  case 245: /* arg: arg "<=>" arg  */
#line 2787 "ripper.y"
                    {
                        (yyval.val) = call_bin_op(p, (yyvsp[-2].val), idCmp, (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                    }
#line 12481 "ripper.c"
    break;

  case 247: /* arg: arg "==" arg  */
#line 2792 "ripper.y"
                    {
                        (yyval.val) = call_bin_op(p, (yyvsp[-2].val), idEq, (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                    }
#line 12489 "ripper.c"
    break;

  case 248: /* arg: arg "===" arg  */
#line 2796 "ripper.y"
                    {
                        (yyval.val) = call_bin_op(p, (yyvsp[-2].val), idEqq, (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                    }
#line 12497 "ripper.c"
    break;

  case 249: /* arg: arg "!=" arg  */
#line 2800 "ripper.y"
                    {
                        (yyval.val) = call_bin_op(p, (yyvsp[-2].val), idNeq, (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                    }
#line 12505 "ripper.c"
    break;

  case 250: /* arg: arg "=~" arg  */
#line 2804 "ripper.y"
                    {
                        (yyval.val) = match_op(p, (yyvsp[-2].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                    }
#line 12513 "ripper.c"
    break;

  case 251: /* arg: arg "!~" arg  */
#line 2808 "ripper.y"
                    {
                        (yyval.val) = call_bin_op(p, (yyvsp[-2].val), idNeqTilde, (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                    }
#line 12521 "ripper.c"
    break;

  case 252: /* arg: '!' arg  */
#line 2812 "ripper.y"
                    {
                        (yyval.val) = call_uni_op(p, method_cond(p, (yyvsp[0].val), &(yylsp[0])), '!', &(yylsp[-1]), &(yyloc));
                    }
#line 12529 "ripper.c"
    break;

  case 253: /* arg: '~' arg  */
#line 2816 "ripper.y"
                    {
                        (yyval.val) = call_uni_op(p, (yyvsp[0].val), '~', &(yylsp[-1]), &(yyloc));
                    }
#line 12537 "ripper.c"
    break;

  case 254: /* arg: arg "<<" arg  */
#line 2820 "ripper.y"
                    {
                        (yyval.val) = call_bin_op(p, (yyvsp[-2].val), idLTLT, (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                    }
#line 12545 "ripper.c"
    break;

  case 255: /* arg: arg ">>" arg  */
#line 2824 "ripper.y"
                    {
                        (yyval.val) = call_bin_op(p, (yyvsp[-2].val), idGTGT, (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                    }
#line 12553 "ripper.c"
    break;

  case 256: /* arg: arg "&&" arg  */
#line 2828 "ripper.y"
                    {
                        (yyval.val) = logop(p, idANDOP, (yyvsp[-2].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                    }
#line 12561 "ripper.c"
    break;

  case 257: /* arg: arg "||" arg  */
#line 2832 "ripper.y"
                    {
                        (yyval.val) = logop(p, idOROP, (yyvsp[-2].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                    }
#line 12569 "ripper.c"
    break;

  case 258: /* $@13: %empty  */
#line 2835 "ripper.y"
                                         {p->ctxt.in_defined = 1;}
#line 12575 "ripper.c"
    break;

  case 259: /* arg: "`defined?'" opt_nl $@13 arg  */
#line 2836 "ripper.y"
                    {
                        p->ctxt.in_defined = 0;
                        (yyval.val) = new_defined(p, (yyvsp[0].val), &(yyloc));
                    }
#line 12584 "ripper.c"
    break;

  case 260: /* arg: arg '?' arg opt_nl ':' arg  */
#line 2841 "ripper.y"
                    {
#if 0
                        value_expr((yyvsp[-5].val));
                        (yyval.val) = new_if(p, (yyvsp[-5].val), (yyvsp[-3].val), (yyvsp[0].val), &(yyloc));
                        fixpos((yyval.val), (yyvsp[-5].val));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-5].val);v2=(yyvsp[-3].val);v3=(yyvsp[0].val);v4=dispatch3(ifop,v1,v2,v3);(yyval.val)=v4;}
                    }
#line 12597 "ripper.c"
    break;

  case 261: /* arg: defn_head f_opt_paren_args '=' endless_arg  */
#line 2850 "ripper.y"
                    {
                        endless_method_name(p, (yyvsp[-3].node), &(yylsp[-3]));
                        restore_defun(p, (yyvsp[-3].node)->nd_defn);
#if 0
                        (yyval.val) = set_defun_body(p, (yyvsp[-3].val), (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8,v9;v1=(yyvsp[0].val);v2=Qnil;v3=Qnil;v4=Qnil;v5=dispatch4(bodystmt,v1,v2,v3,v4);v6=get_value((yyvsp[-3].val));v7=(yyvsp[-2].val);v8=v5;v9=dispatch3(def,v6,v7,v8);(yyval.val)=v9;}
                        local_pop(p);
                    }
#line 12611 "ripper.c"
    break;

  case 262: /* arg: defs_head f_opt_paren_args '=' endless_arg  */
#line 2860 "ripper.y"
                    {
                        endless_method_name(p, (yyvsp[-3].node), &(yylsp[-3]));
                        restore_defun(p, (yyvsp[-3].node)->nd_defn);
#if 0
                        (yyval.val) = set_defun_body(p, (yyvsp[-3].val), (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
                        (yyvsp[-3].val) = get_value((yyvsp[-3].val));

			{VALUE v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11;v1=(yyvsp[0].val);v2=Qnil;v3=Qnil;v4=Qnil;v5=dispatch4(bodystmt,v1,v2,v3,v4);v6=AREF((yyvsp[-3].val), 0);v7=AREF((yyvsp[-3].val), 1);v8=AREF((yyvsp[-3].val), 2);v9=(yyvsp[-2].val);v10=v5;v11=dispatch5(defs,v6,v7,v8,v9,v10);(yyval.val)=v11;}
                        local_pop(p);
                    }
#line 12627 "ripper.c"
    break;

  case 263: /* arg: primary  */
#line 2872 "ripper.y"
                    {
                        (yyval.val) = (yyvsp[0].val);
                    }
#line 12635 "ripper.c"
    break;

  case 265: /* endless_arg: endless_arg "`rescue' modifier" arg  */
#line 2879 "ripper.y"
                    {
#if 0
                        (yyval.val) = rescued_expr(p, (yyvsp[-2].val), (yyvsp[0].val), &(yylsp[-2]), &(yylsp[-1]), &(yylsp[0]));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(rescue_mod,v1,v2);(yyval.val)=v3;}
                    }
#line 12646 "ripper.c"
    break;

  case 266: /* endless_arg: "`not'" opt_nl endless_arg  */
#line 2886 "ripper.y"
                    {
                        (yyval.val) = call_uni_op(p, method_cond(p, (yyvsp[0].val), &(yylsp[0])), METHOD_NOT, &(yylsp[-2]), &(yyloc));
                    }
#line 12654 "ripper.c"
    break;

  case 267: /* relop: '>'  */
#line 2891 "ripper.y"
              {(yyval.val) = '>';}
#line 12660 "ripper.c"
    break;

  case 268: /* relop: '<'  */
#line 2892 "ripper.y"
                       {(yyval.val) = '<';}
#line 12666 "ripper.c"
    break;

  case 269: /* relop: ">="  */
#line 2893 "ripper.y"
                       {(yyval.val) = idGE;}
#line 12672 "ripper.c"
    break;

  case 270: /* relop: "<="  */
#line 2894 "ripper.y"
                       {(yyval.val) = idLE;}
#line 12678 "ripper.c"
    break;

  case 271: /* rel_expr: arg relop arg  */
#line 2898 "ripper.y"
                    {
                        (yyval.val) = call_bin_op(p, (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                    }
#line 12686 "ripper.c"
    break;

  case 272: /* rel_expr: rel_expr relop arg  */
#line 2902 "ripper.y"
                    {
                        rb_warning1("comparison '%s' after comparison", WARN_ID((yyvsp[-1].val)));
                        (yyval.val) = call_bin_op(p, (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                    }
#line 12695 "ripper.c"
    break;

  case 273: /* lex_ctxt: none  */
#line 2909 "ripper.y"
                    {
                        (yyval.ctxt) = p->ctxt;
                    }
#line 12703 "ripper.c"
    break;

  case 274: /* arg_value: arg  */
#line 2915 "ripper.y"
                    {
                        value_expr((yyvsp[0].val));
                        (yyval.val) = (yyvsp[0].val);
                    }
#line 12712 "ripper.c"
    break;

  case 276: /* aref_args: args trailer  */
#line 2923 "ripper.y"
                    {
                        (yyval.val) = (yyvsp[-1].val);
                    }
#line 12720 "ripper.c"
    break;

  case 277: /* aref_args: args ',' assocs trailer  */
#line 2927 "ripper.y"
                    {
#if 0
                        (yyval.val) = (yyvsp[-1].val) ? arg_append(p, (yyvsp[-3].val), new_hash(p, (yyvsp[-1].val), &(yylsp[-1])), &(yyloc)) : (yyvsp[-3].val);
#endif
			{VALUE v1,v2,v3,v4,v5;v1=(yyvsp[-1].val);v2=dispatch1(bare_assoc_hash,v1);v3=(yyvsp[-3].val);v4=v2;v5=dispatch2(args_add,v3,v4);(yyval.val)=v5;}
                    }
#line 12731 "ripper.c"
    break;

  case 278: /* aref_args: assocs trailer  */
#line 2934 "ripper.y"
                    {
#if 0
                        (yyval.val) = (yyvsp[-1].val) ? NEW_LIST(new_hash(p, (yyvsp[-1].val), &(yylsp[-1])), &(yyloc)) : 0;
#endif
			{VALUE v1,v2,v3,v4,v5,v6;v1=dispatch0(args_new);v2=(yyvsp[-1].val);v3=dispatch1(bare_assoc_hash,v2);v4=v1;v5=v3;v6=dispatch2(args_add,v4,v5);(yyval.val)=v6;}
                    }
#line 12742 "ripper.c"
    break;

  case 279: /* arg_rhs: arg  */
#line 2943 "ripper.y"
                    {
                        value_expr((yyvsp[0].val));
                        (yyval.val) = (yyvsp[0].val);
                    }
#line 12751 "ripper.c"
    break;

  case 280: /* arg_rhs: arg "`rescue' modifier" arg  */
#line 2948 "ripper.y"
                    {
#if 0
                        value_expr((yyvsp[-2].val));
                        (yyval.val) = rescued_expr(p, (yyvsp[-2].val), (yyvsp[0].val), &(yylsp[-2]), &(yylsp[-1]), &(yylsp[0]));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(rescue_mod,v1,v2);(yyval.val)=v3;}
                    }
#line 12763 "ripper.c"
    break;

  case 281: /* paren_args: '(' opt_call_args rparen  */
#line 2958 "ripper.y"
                    {
#if 0
                        (yyval.val) = (yyvsp[-1].val);
#endif
			{VALUE v1,v2;v1=escape_Qundef((yyvsp[-1].val));v2=dispatch1(arg_paren,v1);(yyval.val)=v2;}
                    }
#line 12774 "ripper.c"
    break;

  case 282: /* paren_args: '(' args ',' args_forward rparen  */
#line 2965 "ripper.y"
                    {
                        if (!check_forwarding_args(p)) {
                            (yyval.val) = Qnone;
                        }
                        else {
#if 0
                            (yyval.val) = new_args_forward_call(p, (yyvsp[-3].val), &(yylsp[-1]), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5;v1=(yyvsp[-3].val);v2=(yyvsp[-1].val);v3=dispatch2(args_add,v1,v2);v4=v3;v5=dispatch1(arg_paren,v4);(yyval.val)=v5;}
                        }
                    }
#line 12790 "ripper.c"
    break;

  case 283: /* paren_args: '(' args_forward rparen  */
#line 2977 "ripper.y"
                    {
                        if (!check_forwarding_args(p)) {
                            (yyval.val) = Qnone;
                        }
                        else {
#if 0
                            (yyval.val) = new_args_forward_call(p, 0, &(yylsp[-1]), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(arg_paren,v1);(yyval.val)=v2;}
                        }
                    }
#line 12806 "ripper.c"
    break;

  case 288: /* opt_call_args: args ','  */
#line 2997 "ripper.y"
                    {
                      (yyval.val) = (yyvsp[-1].val);
                    }
#line 12814 "ripper.c"
    break;

  case 289: /* opt_call_args: args ',' assocs ','  */
#line 3001 "ripper.y"
                    {
#if 0
                        (yyval.val) = (yyvsp[-1].val) ? arg_append(p, (yyvsp[-3].val), new_hash(p, (yyvsp[-1].val), &(yylsp[-1])), &(yyloc)) : (yyvsp[-3].val);
#endif
			{VALUE v1,v2,v3,v4,v5;v1=(yyvsp[-1].val);v2=dispatch1(bare_assoc_hash,v1);v3=(yyvsp[-3].val);v4=v2;v5=dispatch2(args_add,v3,v4);(yyval.val)=v5;}
                    }
#line 12825 "ripper.c"
    break;

  case 290: /* opt_call_args: assocs ','  */
#line 3008 "ripper.y"
                    {
#if 0
                        (yyval.val) = (yyvsp[-1].val) ? NEW_LIST(new_hash(p, (yyvsp[-1].val), &(yylsp[-1])), &(yylsp[-1])) : 0;
#endif
			{VALUE v1,v2,v3,v4,v5,v6;v1=dispatch0(args_new);v2=(yyvsp[-1].val);v3=dispatch1(bare_assoc_hash,v2);v4=v1;v5=v3;v6=dispatch2(args_add,v4,v5);(yyval.val)=v6;}
                    }
#line 12836 "ripper.c"
    break;

  case 291: /* call_args: command  */
#line 3017 "ripper.y"
                    {
#if 0
                        value_expr((yyvsp[0].val));
                        (yyval.val) = NEW_LIST((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(args_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(args_add,v2,v3);(yyval.val)=v4;}
                    }
#line 12848 "ripper.c"
    break;

  case 292: /* call_args: args opt_block_arg  */
#line 3025 "ripper.y"
                    {
#if 0
                        (yyval.val) = arg_blk_pass((yyvsp[-1].val), (yyvsp[0].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=(yyvsp[0].val);v3=dispatch2(args_add_block,v1,v2);(yyval.val)=v3;}
                    }
#line 12859 "ripper.c"
    break;

  case 293: /* call_args: assocs opt_block_arg  */
#line 3032 "ripper.y"
                    {
#if 0
                        (yyval.val) = (yyvsp[-1].val) ? NEW_LIST(new_hash(p, (yyvsp[-1].val), &(yylsp[-1])), &(yylsp[-1])) : 0;
                        (yyval.val) = arg_blk_pass((yyval.val), (yyvsp[0].val));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8,v9;v1=dispatch0(args_new);v2=(yyvsp[-1].val);v3=dispatch1(bare_assoc_hash,v2);v4=v1;v5=v3;v6=dispatch2(args_add,v4,v5);v7=v6;v8=(yyvsp[0].val);v9=dispatch2(args_add_block,v7,v8);(yyval.val)=v9;}
                    }
#line 12871 "ripper.c"
    break;

  case 294: /* call_args: args ',' assocs opt_block_arg  */
#line 3040 "ripper.y"
                    {
#if 0
                        (yyval.val) = (yyvsp[-1].val) ? arg_append(p, (yyvsp[-3].val), new_hash(p, (yyvsp[-1].val), &(yylsp[-1])), &(yyloc)) : (yyvsp[-3].val);
                        (yyval.val) = arg_blk_pass((yyval.val), (yyvsp[0].val));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8;v1=(yyvsp[-1].val);v2=dispatch1(bare_assoc_hash,v1);v3=(yyvsp[-3].val);v4=v2;v5=dispatch2(args_add,v3,v4);v6=v5;v7=(yyvsp[0].val);v8=dispatch2(args_add_block,v6,v7);(yyval.val)=v8;}
                    }
#line 12883 "ripper.c"
    break;

  case 295: /* call_args: block_arg  */
#line 3048 "ripper.y"
   {{VALUE v1,v2,v3,v4;v1=dispatch0(args_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(args_add_block,v2,v3);(yyval.val)=v4;}}
#line 12889 "ripper.c"
    break;

  case 296: /* $@14: %empty  */
#line 3051 "ripper.y"
                 {
                        /* If call_args starts with a open paren '(' or '[',
                         * look-ahead reading of the letters calls CMDARG_PUSH(0),
                         * but the push must be done after CMDARG_PUSH(1).
                         * So this code makes them consistent by first cancelling
                         * the premature CMDARG_PUSH(0), doing CMDARG_PUSH(1),
                         * and finally redoing CMDARG_PUSH(0).
                         */
                        int lookahead = 0;
                        switch (yychar) {
                          case '(': case tLPAREN: case tLPAREN_ARG: case '[': case tLBRACK:
                            lookahead = 1;
                        }
                        if (lookahead) CMDARG_POP();
                        CMDARG_PUSH(1);
                        if (lookahead) CMDARG_PUSH(0);
                    }
#line 12911 "ripper.c"
    break;

  case 297: /* command_args: $@14 call_args  */
#line 3069 "ripper.y"
                    {
                        /* call_args can be followed by tLBRACE_ARG (that does CMDARG_PUSH(0) in the lexer)
                         * but the push must be done after CMDARG_POP() in the parser.
                         * So this code does CMDARG_POP() to pop 0 pushed by tLBRACE_ARG,
                         * CMDARG_POP() to pop 1 pushed by command_args,
                         * and CMDARG_PUSH(0) to restore back the flag set by tLBRACE_ARG.
                         */
                        int lookahead = 0;
                        switch (yychar) {
                          case tLBRACE_ARG:
                            lookahead = 1;
                        }
                        if (lookahead) CMDARG_POP();
                        CMDARG_POP();
                        if (lookahead) CMDARG_PUSH(0);
                        (yyval.val) = (yyvsp[0].val);
                    }
#line 12933 "ripper.c"
    break;

  case 298: /* block_arg: "&" arg_value  */
#line 3089 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_BLOCK_PASS((yyvsp[0].val), &(yyloc));
#endif
			(yyval.val)=(yyvsp[0].val);
                    }
#line 12944 "ripper.c"
    break;

  case 299: /* block_arg: "&"  */
#line 3096 "ripper.y"
                    {
                        if (!local_id(p, idFWD_BLOCK)) {
                            compile_error(p, "no anonymous block parameter");
                        }
#if 0
                        (yyval.val) = NEW_BLOCK_PASS(NEW_LVAR(idFWD_BLOCK, &(yylsp[0])), &(yyloc));
#endif
			(yyval.val)=Qnil;
                    }
#line 12958 "ripper.c"
    break;

  case 300: /* opt_block_arg: ',' block_arg  */
#line 3108 "ripper.y"
                    {
                        (yyval.val) = (yyvsp[0].val);
                    }
#line 12966 "ripper.c"
    break;

  case 301: /* opt_block_arg: none  */
#line 3112 "ripper.y"
                    {
                        (yyval.val) = 0;
                    }
#line 12974 "ripper.c"
    break;

  case 302: /* args: arg_value  */
#line 3119 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_LIST((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(args_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(args_add,v2,v3);(yyval.val)=v4;}
                    }
#line 12985 "ripper.c"
    break;

  case 303: /* args: "*" arg_value  */
#line 3126 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_SPLAT((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(args_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(args_add_star,v2,v3);(yyval.val)=v4;}
                    }
#line 12996 "ripper.c"
    break;

  case 304: /* args: "*"  */
#line 3133 "ripper.y"
                    {
                        if (!local_id(p, idFWD_REST) ||
                            local_id(p, idFWD_ALL)) {
                            compile_error(p, "no anonymous rest parameter");
                        }
#if 0
                        (yyval.val) = NEW_SPLAT(NEW_LVAR(idFWD_REST, &(yylsp[0])), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(args_new);v2=v1;v3=Qnil;v4=dispatch2(args_add_star,v2,v3);(yyval.val)=v4;}
                    }
#line 13011 "ripper.c"
    break;

  case 305: /* args: args ',' arg_value  */
#line 3144 "ripper.y"
                    {
#if 0
                        (yyval.val) = last_arg_append(p, (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(args_add,v1,v2);(yyval.val)=v3;}
                    }
#line 13022 "ripper.c"
    break;

  case 306: /* args: args ',' "*" arg_value  */
#line 3151 "ripper.y"
                    {
#if 0
                        (yyval.val) = rest_arg_append(p, (yyvsp[-3].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-3].val);v2=(yyvsp[0].val);v3=dispatch2(args_add_star,v1,v2);(yyval.val)=v3;}
                    }
#line 13033 "ripper.c"
    break;

  case 307: /* args: args ',' "*"  */
#line 3158 "ripper.y"
                    {
                        if (!local_id(p, idFWD_REST) ||
                            local_id(p, idFWD_ALL)) {
                            compile_error(p, "no anonymous rest parameter");
                        }
#if 0
                        (yyval.val) = rest_arg_append(p, (yyvsp[-2].val), NEW_LVAR(idFWD_REST, &(yylsp[0])), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=Qnil;v3=dispatch2(args_add_star,v1,v2);(yyval.val)=v3;}
                    }
#line 13048 "ripper.c"
    break;

  case 310: /* mrhs: args ',' arg_value  */
#line 3177 "ripper.y"
                    {
#if 0
                        (yyval.val) = last_arg_append(p, (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5;v1=(yyvsp[-2].val);v2=dispatch1(mrhs_new_from_args,v1);v3=v2;v4=(yyvsp[0].val);v5=dispatch2(mrhs_add,v3,v4);(yyval.val)=v5;}
                    }
#line 13059 "ripper.c"
    break;

  case 311: /* mrhs: args ',' "*" arg_value  */
#line 3184 "ripper.y"
                    {
#if 0
                        (yyval.val) = rest_arg_append(p, (yyvsp[-3].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5;v1=(yyvsp[-3].val);v2=dispatch1(mrhs_new_from_args,v1);v3=v2;v4=(yyvsp[0].val);v5=dispatch2(mrhs_add_star,v3,v4);(yyval.val)=v5;}
                    }
#line 13070 "ripper.c"
    break;

  case 312: /* mrhs: "*" arg_value  */
#line 3191 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_SPLAT((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(mrhs_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(mrhs_add_star,v2,v3);(yyval.val)=v4;}
                    }
#line 13081 "ripper.c"
    break;

  case 323: /* primary: "method"  */
#line 3210 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_FCALL((yyvsp[0].val), 0, &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6;v1=(yyvsp[0].val);v2=dispatch1(fcall,v1);v3=dispatch0(args_new);v4=v2;v5=v3;v6=dispatch2(method_add_arg,v4,v5);(yyval.val)=v6;}
                    }
#line 13092 "ripper.c"
    break;

  case 324: /* $@15: %empty  */
#line 3217 "ripper.y"
                    {
                        CMDARG_PUSH(0);
                    }
#line 13100 "ripper.c"
    break;

  case 325: /* primary: k_begin $@15 bodystmt k_end  */
#line 3222 "ripper.y"
                    {
                        CMDARG_POP();
#if 0
                        set_line_body((yyvsp[-1].val), (yylsp[-3]).end_pos.lineno);
                        (yyval.val) = NEW_BEGIN((yyvsp[-1].val), &(yyloc));
                        nd_set_line((yyval.val), (yylsp[-3]).end_pos.lineno);
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(begin,v1);(yyval.val)=v2;}
                    }
#line 13114 "ripper.c"
    break;

  case 326: /* $@16: %empty  */
#line 3231 "ripper.y"
                              {SET_LEX_STATE(EXPR_ENDARG);}
#line 13120 "ripper.c"
    break;

  case 327: /* primary: "( arg" $@16 rparen  */
#line 3232 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_BEGIN(0, &(yyloc));
#endif
			{VALUE v1,v2;v1=0;v2=dispatch1(paren,v1);(yyval.val)=v2;}
                    }
#line 13131 "ripper.c"
    break;

  case 328: /* $@17: %empty  */
#line 3238 "ripper.y"
                                   {SET_LEX_STATE(EXPR_ENDARG);}
#line 13137 "ripper.c"
    break;

  case 329: /* primary: "( arg" stmt $@17 rparen  */
#line 3239 "ripper.y"
                    {
#if 0
                        if (nd_type_p((yyvsp[-2].val), NODE_SELF)) (yyvsp[-2].val)->nd_state = 0;
                        (yyval.val) = (yyvsp[-2].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[-2].val);v2=dispatch1(paren,v1);(yyval.val)=v2;}
                    }
#line 13149 "ripper.c"
    break;

  case 330: /* primary: "(" compstmt ')'  */
#line 3247 "ripper.y"
                    {
#if 0
                        if (nd_type_p((yyvsp[-1].val), NODE_SELF)) (yyvsp[-1].val)->nd_state = 0;
                        (yyval.val) = (yyvsp[-1].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(paren,v1);(yyval.val)=v2;}
                    }
#line 13161 "ripper.c"
    break;

  case 331: /* primary: primary_value "::" "constant"  */
#line 3255 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_COLON2((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(const_path_ref,v1,v2);(yyval.val)=v3;}
                    }
#line 13172 "ripper.c"
    break;

  case 332: /* primary: ":: at EXPR_BEG" "constant"  */
#line 3262 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_COLON3((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(top_const_ref,v1);(yyval.val)=v2;}
                    }
#line 13183 "ripper.c"
    break;

  case 333: /* primary: "[" aref_args ']'  */
#line 3269 "ripper.y"
                    {
#if 0
                        (yyval.val) = make_list((yyvsp[-1].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=escape_Qundef((yyvsp[-1].val));v2=dispatch1(array,v1);(yyval.val)=v2;}
                    }
#line 13194 "ripper.c"
    break;

  case 334: /* primary: "{" assoc_list '}'  */
#line 3276 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_hash(p, (yyvsp[-1].val), &(yyloc));
                        (yyval.val)->nd_brace = TRUE;
#endif
			{VALUE v1,v2;v1=escape_Qundef((yyvsp[-1].val));v2=dispatch1(hash,v1);(yyval.val)=v2;}
                    }
#line 13206 "ripper.c"
    break;

  case 335: /* primary: k_return  */
#line 3284 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_RETURN(0, &(yyloc));
#endif
			{VALUE v1;v1=dispatch0(return0);(yyval.val)=v1;}
                    }
#line 13217 "ripper.c"
    break;

  case 336: /* primary: "`yield'" '(' call_args rparen  */
#line 3291 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_yield(p, (yyvsp[-1].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-1].val);v2=dispatch1(paren,v1);v3=v2;v4=dispatch1(yield,v3);(yyval.val)=v4;}
                    }
#line 13228 "ripper.c"
    break;

  case 337: /* primary: "`yield'" '(' rparen  */
#line 3298 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_YIELD(0, &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5;v1=dispatch0(args_new);v2=v1;v3=dispatch1(paren,v2);v4=v3;v5=dispatch1(yield,v4);(yyval.val)=v5;}
                    }
#line 13239 "ripper.c"
    break;

  case 338: /* primary: "`yield'"  */
#line 3305 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_YIELD(0, &(yyloc));
#endif
			{VALUE v1;v1=dispatch0(yield0);(yyval.val)=v1;}
                    }
#line 13250 "ripper.c"
    break;

  case 339: /* $@18: %empty  */
#line 3311 "ripper.y"
                                             {p->ctxt.in_defined = 1;}
#line 13256 "ripper.c"
    break;

  case 340: /* primary: "`defined?'" opt_nl '(' $@18 expr rparen  */
#line 3312 "ripper.y"
                    {
                        p->ctxt.in_defined = 0;
                        (yyval.val) = new_defined(p, (yyvsp[-1].val), &(yyloc));
                    }
#line 13265 "ripper.c"
    break;

  case 341: /* primary: "`not'" '(' expr rparen  */
#line 3317 "ripper.y"
                    {
                        (yyval.val) = call_uni_op(p, method_cond(p, (yyvsp[-1].val), &(yylsp[-1])), METHOD_NOT, &(yylsp[-3]), &(yyloc));
                    }
#line 13273 "ripper.c"
    break;

  case 342: /* primary: "`not'" '(' rparen  */
#line 3321 "ripper.y"
                    {
                        (yyval.val) = call_uni_op(p, method_cond(p, new_nil(&(yylsp[-1])), &(yylsp[-1])), METHOD_NOT, &(yylsp[-2]), &(yyloc));
                    }
#line 13281 "ripper.c"
    break;

  case 343: /* primary: fcall brace_block  */
#line 3325 "ripper.y"
                    {
#if 0
                        (yyval.val) = method_add_block(p, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8,v9;v1=(yyvsp[-1].val);v2=dispatch1(fcall,v1);v3=dispatch0(args_new);v4=v2;v5=v3;v6=dispatch2(method_add_arg,v4,v5);v7=v6;v8=(yyvsp[0].val);v9=dispatch2(method_add_block,v7,v8);(yyval.val)=v9;}
                    }
#line 13292 "ripper.c"
    break;

  case 345: /* primary: method_call brace_block  */
#line 3333 "ripper.y"
                    {
#if 0
                        block_dup_check(p, (yyvsp[-1].val)->nd_args, (yyvsp[0].val));
                        (yyval.val) = method_add_block(p, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=(yyvsp[0].val);v3=dispatch2(method_add_block,v1,v2);(yyval.val)=v3;}
                    }
#line 13304 "ripper.c"
    break;

  case 347: /* primary: k_if expr_value then compstmt if_tail k_end  */
#line 3345 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_if(p, (yyvsp[-4].val), (yyvsp[-2].val), (yyvsp[-1].val), &(yyloc));
                        fixpos((yyval.val), (yyvsp[-4].val));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-4].val);v2=(yyvsp[-2].val);v3=escape_Qundef((yyvsp[-1].val));v4=dispatch3(if,v1,v2,v3);(yyval.val)=v4;}
                    }
#line 13316 "ripper.c"
    break;

  case 348: /* primary: k_unless expr_value then compstmt opt_else k_end  */
#line 3356 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_unless(p, (yyvsp[-4].val), (yyvsp[-2].val), (yyvsp[-1].val), &(yyloc));
                        fixpos((yyval.val), (yyvsp[-4].val));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-4].val);v2=(yyvsp[-2].val);v3=escape_Qundef((yyvsp[-1].val));v4=dispatch3(unless,v1,v2,v3);(yyval.val)=v4;}
                    }
#line 13328 "ripper.c"
    break;

  case 349: /* primary: k_while expr_value_do compstmt k_end  */
#line 3366 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_WHILE(cond(p, (yyvsp[-2].val), &(yylsp[-2])), (yyvsp[-1].val), 1, &(yyloc));
                        fixpos((yyval.val), (yyvsp[-2].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=dispatch2(while,v1,v2);(yyval.val)=v3;}
                    }
#line 13340 "ripper.c"
    break;

  case 350: /* primary: k_until expr_value_do compstmt k_end  */
#line 3376 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_UNTIL(cond(p, (yyvsp[-2].val), &(yylsp[-2])), (yyvsp[-1].val), 1, &(yyloc));
                        fixpos((yyval.val), (yyvsp[-2].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=dispatch2(until,v1,v2);(yyval.val)=v3;}
                    }
#line 13352 "ripper.c"
    break;

  case 351: /* @19: %empty  */
#line 3384 "ripper.y"
                    {
                        (yyval.val) = p->case_labels;
                        p->case_labels = Qnil;
                    }
#line 13361 "ripper.c"
    break;

  case 352: /* primary: k_case expr_value opt_terms @19 case_body k_end  */
#line 3390 "ripper.y"
                    {
                        if (RTEST(p->case_labels)) rb_hash_clear(p->case_labels);
                        p->case_labels = (yyvsp[-2].val);
#if 0
                        (yyval.val) = NEW_CASE((yyvsp[-4].val), (yyvsp[-1].val), &(yyloc));
                        fixpos((yyval.val), (yyvsp[-4].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-4].val);v2=(yyvsp[-1].val);v3=dispatch2(case,v1,v2);(yyval.val)=v3;}
                    }
#line 13375 "ripper.c"
    break;

  case 353: /* @20: %empty  */
#line 3400 "ripper.y"
                    {
                        (yyval.val) = p->case_labels;
                        p->case_labels = 0;
                    }
#line 13384 "ripper.c"
    break;

  case 354: /* primary: k_case opt_terms @20 case_body k_end  */
#line 3406 "ripper.y"
                    {
                        if (RTEST(p->case_labels)) rb_hash_clear(p->case_labels);
                        p->case_labels = (yyvsp[-2].val);
#if 0
                        (yyval.val) = NEW_CASE2((yyvsp[-1].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=Qnil;v2=(yyvsp[-1].val);v3=dispatch2(case,v1,v2);(yyval.val)=v3;}
                    }
#line 13397 "ripper.c"
    break;

  case 355: /* primary: k_case expr_value opt_terms p_case_body k_end  */
#line 3417 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_CASE3((yyvsp[-3].val), (yyvsp[-1].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-3].val);v2=(yyvsp[-1].val);v3=dispatch2(case,v1,v2);(yyval.val)=v3;}
                    }
#line 13408 "ripper.c"
    break;

  case 356: /* primary: k_for for_var "`in'" expr_value_do compstmt k_end  */
#line 3426 "ripper.y"
                    {
#if 0
                        /*
                         *  for a, b, c in e
                         *  #=>
                         *  e.each{|*x| a, b, c = x}
                         *
                         *  for a in e
                         *  #=>
                         *  e.each{|x| a, = x}
                         */
                        ID id = internal_id(p);
                        NODE *m = NEW_ARGS_AUX(0, 0, &NULL_LOC);
                        NODE *args, *scope, *internal_var = NEW_DVAR(id, &(yylsp[-4]));
                        rb_ast_id_table_t *tbl = rb_ast_new_local_table(p->ast, 1);
                        tbl->ids[0] = id; /* internal id */

                        switch (nd_type((yyvsp[-4].val))) {
                          case NODE_LASGN:
                          case NODE_DASGN: /* e.each {|internal_var| a = internal_var; ... } */
                            (yyvsp[-4].val)->nd_value = internal_var;
                            id = 0;
                            m->nd_plen = 1;
                            m->nd_next = (yyvsp[-4].val);
                            break;
                          case NODE_MASGN: /* e.each {|*internal_var| a, b, c = (internal_var.length == 1 && Array === (tmp = internal_var[0]) ? tmp : internal_var); ... } */
                            m->nd_next = node_assign(p, (yyvsp[-4].val), NEW_FOR_MASGN(internal_var, &(yylsp[-4])), NO_LEX_CTXT, &(yylsp[-4]));
                            break;
                          default: /* e.each {|*internal_var| @a, B, c[1], d.attr = internal_val; ... } */
                            m->nd_next = node_assign(p, NEW_MASGN(NEW_LIST((yyvsp[-4].val), &(yylsp[-4])), 0, &(yylsp[-4])), internal_var, NO_LEX_CTXT, &(yylsp[-4]));
                        }
                        /* {|*internal_id| <m> = internal_id; ... } */
                        args = new_args(p, m, 0, id, 0, new_args_tail(p, 0, 0, 0, &(yylsp[-4])), &(yylsp[-4]));
                        scope = NEW_NODE(NODE_SCOPE, tbl, (yyvsp[-1].val), args, &(yyloc));
                        (yyval.val) = NEW_FOR((yyvsp[-2].val), scope, &(yyloc));
                        fixpos((yyval.val), (yyvsp[-4].val));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-4].val);v2=(yyvsp[-2].val);v3=(yyvsp[-1].val);v4=dispatch3(for,v1,v2,v3);(yyval.val)=v4;}
                    }
#line 13452 "ripper.c"
    break;

  case 357: /* $@21: %empty  */
#line 3466 "ripper.y"
                    {
                        if (p->ctxt.in_def) {
                            YYLTYPE loc = code_loc_gen(&(yylsp[-2]), &(yylsp[-1]));
                            yyerror1(&loc, "class definition in method body");
                        }
                        p->ctxt.in_class = 1;
                        local_push(p, 0);
                    }
#line 13465 "ripper.c"
    break;

  case 358: /* primary: k_class cpath superclass $@21 bodystmt k_end  */
#line 3476 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_CLASS((yyvsp[-4].val), (yyvsp[-1].val), (yyvsp[-3].val), &(yyloc));
                        nd_set_line((yyval.val)->nd_body, (yylsp[0]).end_pos.lineno);
                        set_line_body((yyvsp[-1].val), (yylsp[-3]).end_pos.lineno);
                        nd_set_line((yyval.val), (yylsp[-3]).end_pos.lineno);
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-4].val);v2=(yyvsp[-3].val);v3=(yyvsp[-1].val);v4=dispatch3(class,v1,v2,v3);(yyval.val)=v4;}
                        local_pop(p);
                        p->ctxt.in_class = (yyvsp[-5].ctxt).in_class;
                        p->ctxt.shareable_constant_value = (yyvsp[-5].ctxt).shareable_constant_value;
                    }
#line 13482 "ripper.c"
    break;

  case 359: /* $@22: %empty  */
#line 3489 "ripper.y"
                    {
                        p->ctxt.in_def = 0;
                        p->ctxt.in_class = 0;
                        local_push(p, 0);
                    }
#line 13492 "ripper.c"
    break;

  case 360: /* primary: k_class "<<" expr $@22 term bodystmt k_end  */
#line 3497 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_SCLASS((yyvsp[-4].val), (yyvsp[-1].val), &(yyloc));
                        nd_set_line((yyval.val)->nd_body, (yylsp[0]).end_pos.lineno);
                        set_line_body((yyvsp[-1].val), nd_line((yyvsp[-4].val)));
                        fixpos((yyval.val), (yyvsp[-4].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-4].val);v2=(yyvsp[-1].val);v3=dispatch2(sclass,v1,v2);(yyval.val)=v3;}
                        local_pop(p);
                        p->ctxt.in_def = (yyvsp[-6].ctxt).in_def;
                        p->ctxt.in_class = (yyvsp[-6].ctxt).in_class;
                        p->ctxt.shareable_constant_value = (yyvsp[-6].ctxt).shareable_constant_value;
                    }
#line 13510 "ripper.c"
    break;

  case 361: /* $@23: %empty  */
#line 3511 "ripper.y"
                    {
                        if (p->ctxt.in_def) {
                            YYLTYPE loc = code_loc_gen(&(yylsp[-1]), &(yylsp[0]));
                            yyerror1(&loc, "module definition in method body");
                        }
                        p->ctxt.in_class = 1;
                        local_push(p, 0);
                    }
#line 13523 "ripper.c"
    break;

  case 362: /* primary: k_module cpath $@23 bodystmt k_end  */
#line 3521 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_MODULE((yyvsp[-3].val), (yyvsp[-1].val), &(yyloc));
                        nd_set_line((yyval.val)->nd_body, (yylsp[0]).end_pos.lineno);
                        set_line_body((yyvsp[-1].val), (yylsp[-3]).end_pos.lineno);
                        nd_set_line((yyval.val), (yylsp[-3]).end_pos.lineno);
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-3].val);v2=(yyvsp[-1].val);v3=dispatch2(module,v1,v2);(yyval.val)=v3;}
                        local_pop(p);
                        p->ctxt.in_class = (yyvsp[-4].ctxt).in_class;
                        p->ctxt.shareable_constant_value = (yyvsp[-4].ctxt).shareable_constant_value;
                    }
#line 13540 "ripper.c"
    break;

  case 363: /* $@24: %empty  */
#line 3535 "ripper.y"
                    {
#if 0
                        push_end_expect_token_locations(p, &(yylsp[-1]).beg_pos);
#endif
                    }
#line 13550 "ripper.c"
    break;

  case 364: /* primary: defn_head f_arglist $@24 bodystmt k_end  */
#line 3542 "ripper.y"
                    {
                        restore_defun(p, (yyvsp[-4].node)->nd_defn);
#if 0
                        (yyval.val) = set_defun_body(p, (yyvsp[-4].val), (yyvsp[-3].val), (yyvsp[-1].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=get_value((yyvsp[-4].val));v2=(yyvsp[-3].val);v3=(yyvsp[-1].val);v4=dispatch3(def,v1,v2,v3);(yyval.val)=v4;}
                        local_pop(p);
                    }
#line 13563 "ripper.c"
    break;

  case 365: /* $@25: %empty  */
#line 3552 "ripper.y"
                    {
#if 0
                        push_end_expect_token_locations(p, &(yylsp[-1]).beg_pos);
#endif
                    }
#line 13573 "ripper.c"
    break;

  case 366: /* primary: defs_head f_arglist $@25 bodystmt k_end  */
#line 3559 "ripper.y"
                    {
                        restore_defun(p, (yyvsp[-4].node)->nd_defn);
#if 0
                        (yyval.val) = set_defun_body(p, (yyvsp[-4].val), (yyvsp[-3].val), (yyvsp[-1].val), &(yyloc));
#endif
                        (yyvsp[-4].val) = get_value((yyvsp[-4].val));

			{VALUE v1,v2,v3,v4,v5,v6;v1=AREF((yyvsp[-4].val), 0);v2=AREF((yyvsp[-4].val), 1);v3=AREF((yyvsp[-4].val), 2);v4=(yyvsp[-3].val);v5=(yyvsp[-1].val);v6=dispatch5(defs,v1,v2,v3,v4,v5);(yyval.val)=v6;}
                        local_pop(p);
                    }
#line 13588 "ripper.c"
    break;

  case 367: /* primary: "`break'"  */
#line 3570 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_BREAK(0, &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=dispatch0(args_new);v2=v1;v3=dispatch1(break,v2);(yyval.val)=v3;}
                    }
#line 13599 "ripper.c"
    break;

  case 368: /* primary: "`next'"  */
#line 3577 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_NEXT(0, &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=dispatch0(args_new);v2=v1;v3=dispatch1(next,v2);(yyval.val)=v3;}
                    }
#line 13610 "ripper.c"
    break;

  case 369: /* primary: "`redo'"  */
#line 3584 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_REDO(&(yyloc));
#endif
			{VALUE v1;v1=dispatch0(redo);(yyval.val)=v1;}
                    }
#line 13621 "ripper.c"
    break;

  case 370: /* primary: "`retry'"  */
#line 3591 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_RETRY(&(yyloc));
#endif
			{VALUE v1;v1=dispatch0(retry);(yyval.val)=v1;}
                    }
#line 13632 "ripper.c"
    break;

  case 371: /* primary_value: primary  */
#line 3600 "ripper.y"
                    {
                        value_expr((yyvsp[0].val));
                        (yyval.val) = (yyvsp[0].val);
                    }
#line 13641 "ripper.c"
    break;

  case 372: /* k_begin: "`begin'"  */
#line 3607 "ripper.y"
                    {
                        token_info_push(p, "begin", &(yyloc));
#if 0
                        push_end_expect_token_locations(p, &(yylsp[0]).beg_pos);
#endif
                    }
#line 13652 "ripper.c"
    break;

  case 373: /* k_if: "`if'"  */
#line 3616 "ripper.y"
                    {
                        WARN_EOL("if");
                        token_info_push(p, "if", &(yyloc));
                        if (p->token_info && p->token_info->nonspc &&
                            p->token_info->next && !strcmp(p->token_info->next->token, "else")) {
                            const char *tok = p->lex.ptok - rb_strlen_lit("if");
                            const char *beg = p->lex.pbeg + p->token_info->next->beg.column;
                            beg += rb_strlen_lit("else");
                            while (beg < tok && ISSPACE(*beg)) beg++;
                            if (beg == tok) {
                                p->token_info->nonspc = 0;
                            }
                        }
#if 0
                        push_end_expect_token_locations(p, &(yylsp[0]).beg_pos);
#endif
                    }
#line 13674 "ripper.c"
    break;

  case 374: /* k_unless: "`unless'"  */
#line 3636 "ripper.y"
                    {
                        token_info_push(p, "unless", &(yyloc));
#if 0
                        push_end_expect_token_locations(p, &(yylsp[0]).beg_pos);
#endif
                    }
#line 13685 "ripper.c"
    break;

  case 375: /* k_while: "`while'"  */
#line 3645 "ripper.y"
                    {
                        token_info_push(p, "while", &(yyloc));
#if 0
                        push_end_expect_token_locations(p, &(yylsp[0]).beg_pos);
#endif
                    }
#line 13696 "ripper.c"
    break;

  case 376: /* k_until: "`until'"  */
#line 3654 "ripper.y"
                    {
                        token_info_push(p, "until", &(yyloc));
#if 0
                        push_end_expect_token_locations(p, &(yylsp[0]).beg_pos);
#endif
                    }
#line 13707 "ripper.c"
    break;

  case 377: /* k_case: "`case'"  */
#line 3663 "ripper.y"
                    {
                        token_info_push(p, "case", &(yyloc));
#if 0
                        push_end_expect_token_locations(p, &(yylsp[0]).beg_pos);
#endif
                    }
#line 13718 "ripper.c"
    break;

  case 378: /* k_for: "`for'"  */
#line 3672 "ripper.y"
                    {
                        token_info_push(p, "for", &(yyloc));
#if 0
                        push_end_expect_token_locations(p, &(yylsp[0]).beg_pos);
#endif
                    }
#line 13729 "ripper.c"
    break;

  case 379: /* k_class: "`class'"  */
#line 3681 "ripper.y"
                    {
                        token_info_push(p, "class", &(yyloc));
                        (yyval.ctxt) = p->ctxt;
#if 0
                        push_end_expect_token_locations(p, &(yylsp[0]).beg_pos);
#endif
                    }
#line 13741 "ripper.c"
    break;

  case 380: /* k_module: "`module'"  */
#line 3691 "ripper.y"
                    {
                        token_info_push(p, "module", &(yyloc));
                        (yyval.ctxt) = p->ctxt;
#if 0
                        push_end_expect_token_locations(p, &(yylsp[0]).beg_pos);
#endif
                    }
#line 13753 "ripper.c"
    break;

  case 381: /* k_def: "`def'"  */
#line 3701 "ripper.y"
                    {
                        token_info_push(p, "def", &(yyloc));
                        p->ctxt.in_argdef = 1;
                    }
#line 13762 "ripper.c"
    break;

  case 382: /* k_do: "`do'"  */
#line 3708 "ripper.y"
                    {
                        token_info_push(p, "do", &(yyloc));
#if 0
                        push_end_expect_token_locations(p, &(yylsp[0]).beg_pos);
#endif

                    }
#line 13774 "ripper.c"
    break;

  case 383: /* k_do_block: "`do' for block"  */
#line 3718 "ripper.y"
                    {
                        token_info_push(p, "do", &(yyloc));
#if 0
                        push_end_expect_token_locations(p, &(yylsp[0]).beg_pos);
#endif
                    }
#line 13785 "ripper.c"
    break;

  case 384: /* k_rescue: "`rescue'"  */
#line 3727 "ripper.y"
                    {
                        token_info_warn(p, "rescue", p->token_info, 1, &(yyloc));
                    }
#line 13793 "ripper.c"
    break;

  case 385: /* k_ensure: "`ensure'"  */
#line 3733 "ripper.y"
                    {
                        token_info_warn(p, "ensure", p->token_info, 1, &(yyloc));
                    }
#line 13801 "ripper.c"
    break;

  case 386: /* k_when: "`when'"  */
#line 3739 "ripper.y"
                    {
                        token_info_warn(p, "when", p->token_info, 0, &(yyloc));
                    }
#line 13809 "ripper.c"
    break;

  case 387: /* k_else: "`else'"  */
#line 3745 "ripper.y"
                    {
                        token_info *ptinfo_beg = p->token_info;
                        int same = ptinfo_beg && strcmp(ptinfo_beg->token, "case") != 0;
                        token_info_warn(p, "else", p->token_info, same, &(yyloc));
                        if (same) {
                            token_info e;
                            e.next = ptinfo_beg->next;
                            e.token = "else";
                            token_info_setup(&e, p->lex.pbeg, &(yyloc));
                            if (!e.nonspc) *ptinfo_beg = e;
                        }
                    }
#line 13826 "ripper.c"
    break;

  case 388: /* k_elsif: "`elsif'"  */
#line 3760 "ripper.y"
                    {
                        WARN_EOL("elsif");
                        token_info_warn(p, "elsif", p->token_info, 1, &(yyloc));
                    }
#line 13835 "ripper.c"
    break;

  case 389: /* k_end: "`end'"  */
#line 3767 "ripper.y"
                    {
                        token_info_pop(p, "end", &(yyloc));
#if 0
                        pop_end_expect_token_locations(p);
#endif
                    }
#line 13846 "ripper.c"
    break;

  case 390: /* k_end: "dummy end"  */
#line 3774 "ripper.y"
                    {
                        compile_error(p, "syntax error, unexpected end-of-input");
                    }
#line 13854 "ripper.c"
    break;

  case 391: /* k_return: "`return'"  */
#line 3780 "ripper.y"
                    {
                        if (p->ctxt.in_class && !p->ctxt.in_def && !dyna_in_block(p))
                            yyerror1(&(yylsp[0]), "Invalid return in class/module body");
                    }
#line 13863 "ripper.c"
    break;

  case 398: /* if_tail: k_elsif expr_value then compstmt if_tail  */
#line 3799 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_if(p, (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
                        fixpos((yyval.val), (yyvsp[-3].val));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-3].val);v2=(yyvsp[-1].val);v3=escape_Qundef((yyvsp[0].val));v4=dispatch3(elsif,v1,v2,v3);(yyval.val)=v4;}
                    }
#line 13875 "ripper.c"
    break;

  case 400: /* opt_else: k_else compstmt  */
#line 3810 "ripper.y"
                    {
#if 0
                        (yyval.val) = (yyvsp[0].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(else,v1);(yyval.val)=v2;}
                    }
#line 13886 "ripper.c"
    break;

  case 403: /* f_marg: f_norm_arg  */
#line 3823 "ripper.y"
                    {
#if 0
                        (yyval.val) = assignable(p, (yyvsp[0].val), 0, &(yyloc));
                        mark_lvar_used(p, (yyval.val));
#endif
			(yyval.val)=assignable(p, (yyvsp[0].val));
                    }
#line 13898 "ripper.c"
    break;

  case 404: /* f_marg: "(" f_margs rparen  */
#line 3831 "ripper.y"
                    {
#if 0
                        (yyval.val) = (yyvsp[-1].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(mlhs_paren,v1);(yyval.val)=v2;}
                    }
#line 13909 "ripper.c"
    break;

  case 405: /* f_marg_list: f_marg  */
#line 3840 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_LIST((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(mlhs_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(mlhs_add,v2,v3);(yyval.val)=v4;}
                    }
#line 13920 "ripper.c"
    break;

  case 406: /* f_marg_list: f_marg_list ',' f_marg  */
#line 3847 "ripper.y"
                    {
#if 0
                        (yyval.val) = list_append(p, (yyvsp[-2].val), (yyvsp[0].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(mlhs_add,v1,v2);(yyval.val)=v3;}
                    }
#line 13931 "ripper.c"
    break;

  case 407: /* f_margs: f_marg_list  */
#line 3856 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_MASGN((yyvsp[0].val), 0, &(yyloc));
#endif
			(yyval.val)=(yyvsp[0].val);
                    }
#line 13942 "ripper.c"
    break;

  case 408: /* f_margs: f_marg_list ',' f_rest_marg  */
#line 3863 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_MASGN((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(mlhs_add_star,v1,v2);(yyval.val)=v3;}
                    }
#line 13953 "ripper.c"
    break;

  case 409: /* f_margs: f_marg_list ',' f_rest_marg ',' f_marg_list  */
#line 3870 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_MASGN((yyvsp[-4].val), NEW_POSTARG((yyvsp[-2].val), (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6;v1=(yyvsp[-4].val);v2=(yyvsp[-2].val);v3=dispatch2(mlhs_add_star,v1,v2);v4=v3;v5=(yyvsp[0].val);v6=dispatch2(mlhs_add_post,v4,v5);(yyval.val)=v6;}
                    }
#line 13964 "ripper.c"
    break;

  case 410: /* f_margs: f_rest_marg  */
#line 3877 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_MASGN(0, (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(mlhs_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(mlhs_add_star,v2,v3);(yyval.val)=v4;}
                    }
#line 13975 "ripper.c"
    break;

  case 411: /* f_margs: f_rest_marg ',' f_marg_list  */
#line 3884 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_MASGN(0, NEW_POSTARG((yyvsp[-2].val), (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=dispatch0(mlhs_new);v2=v1;v3=(yyvsp[-2].val);v4=dispatch2(mlhs_add_star,v2,v3);v5=v4;v6=(yyvsp[0].val);v7=dispatch2(mlhs_add_post,v5,v6);(yyval.val)=v7;}
                    }
#line 13986 "ripper.c"
    break;

  case 412: /* f_rest_marg: "*" f_norm_arg  */
#line 3893 "ripper.y"
                    {
#if 0
                        (yyval.val) = assignable(p, (yyvsp[0].val), 0, &(yyloc));
                        mark_lvar_used(p, (yyval.val));
#endif
			(yyval.val)=assignable(p, (yyvsp[0].val));
                    }
#line 13998 "ripper.c"
    break;

  case 413: /* f_rest_marg: "*"  */
#line 3901 "ripper.y"
                    {
#if 0
                        (yyval.val) = NODE_SPECIAL_NO_NAME_REST;
#endif
			(yyval.val)=Qnil;
                    }
#line 14009 "ripper.c"
    break;

  case 415: /* f_any_kwrest: f_no_kwarg  */
#line 3910 "ripper.y"
                             {(yyval.val) = ID2VAL(idNil);}
#line 14015 "ripper.c"
    break;

  case 416: /* $@26: %empty  */
#line 3913 "ripper.y"
        {p->ctxt.in_argdef = 0;}
#line 14021 "ripper.c"
    break;

  case 418: /* block_args_tail: f_block_kwarg ',' f_kwrest opt_f_block_arg  */
#line 3916 "ripper.y"
                    {
                        (yyval.val) = new_args_tail(p, (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]));
                    }
#line 14029 "ripper.c"
    break;

  case 419: /* block_args_tail: f_block_kwarg opt_f_block_arg  */
#line 3920 "ripper.y"
                    {
                        (yyval.val) = new_args_tail(p, (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yylsp[-1]));
                    }
#line 14037 "ripper.c"
    break;

  case 420: /* block_args_tail: f_any_kwrest opt_f_block_arg  */
#line 3924 "ripper.y"
                    {
                        (yyval.val) = new_args_tail(p, Qnone, (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]));
                    }
#line 14045 "ripper.c"
    break;

  case 421: /* block_args_tail: f_block_arg  */
#line 3928 "ripper.y"
                    {
                        (yyval.val) = new_args_tail(p, Qnone, Qnone, (yyvsp[0].val), &(yylsp[0]));
                    }
#line 14053 "ripper.c"
    break;

  case 422: /* opt_block_args_tail: ',' block_args_tail  */
#line 3934 "ripper.y"
                    {
                        (yyval.val) = (yyvsp[0].val);
                    }
#line 14061 "ripper.c"
    break;

  case 423: /* opt_block_args_tail: %empty  */
#line 3938 "ripper.y"
                    {
                        (yyval.val) = new_args_tail(p, Qnone, Qnone, Qnone, &(yylsp[0]));
                    }
#line 14069 "ripper.c"
    break;

  case 424: /* excessed_comma: ','  */
#line 3944 "ripper.y"
                    {
                        /* magic number for rest_id in iseq_set_arguments() */
#if 0
                        (yyval.val) = NODE_SPECIAL_EXCESSIVE_COMMA;
#endif
			{VALUE v1;v1=dispatch0(excessed_comma);(yyval.val)=v1;}
                    }
#line 14081 "ripper.c"
    break;

  case 425: /* block_param: f_arg ',' f_block_optarg ',' f_rest_arg opt_block_args_tail  */
#line 3954 "ripper.y"
                    {
                        (yyval.val) = new_args(p, (yyvsp[-5].val), (yyvsp[-3].val), (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yyloc));
                    }
#line 14089 "ripper.c"
    break;

  case 426: /* block_param: f_arg ',' f_block_optarg ',' f_rest_arg ',' f_arg opt_block_args_tail  */
#line 3958 "ripper.y"
                    {
                        (yyval.val) = new_args(p, (yyvsp[-7].val), (yyvsp[-5].val), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
                    }
#line 14097 "ripper.c"
    break;

  case 427: /* block_param: f_arg ',' f_block_optarg opt_block_args_tail  */
#line 3962 "ripper.y"
                    {
                        (yyval.val) = new_args(p, (yyvsp[-3].val), (yyvsp[-1].val), Qnone, Qnone, (yyvsp[0].val), &(yyloc));
                    }
#line 14105 "ripper.c"
    break;

  case 428: /* block_param: f_arg ',' f_block_optarg ',' f_arg opt_block_args_tail  */
#line 3966 "ripper.y"
                    {
                        (yyval.val) = new_args(p, (yyvsp[-5].val), (yyvsp[-3].val), Qnone, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
                    }
#line 14113 "ripper.c"
    break;

  case 429: /* block_param: f_arg ',' f_rest_arg opt_block_args_tail  */
#line 3970 "ripper.y"
                    {
                        (yyval.val) = new_args(p, (yyvsp[-3].val), Qnone, (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yyloc));
                    }
#line 14121 "ripper.c"
    break;

  case 430: /* block_param: f_arg excessed_comma  */
#line 3974 "ripper.y"
                    {
                        (yyval.val) = new_args_tail(p, Qnone, Qnone, Qnone, &(yylsp[0]));
                        (yyval.val) = new_args(p, (yyvsp[-1].val), Qnone, (yyvsp[0].val), Qnone, (yyval.val), &(yyloc));
                    }
#line 14130 "ripper.c"
    break;

  case 431: /* block_param: f_arg ',' f_rest_arg ',' f_arg opt_block_args_tail  */
#line 3979 "ripper.y"
                    {
                        (yyval.val) = new_args(p, (yyvsp[-5].val), Qnone, (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
                    }
#line 14138 "ripper.c"
    break;

  case 432: /* block_param: f_arg opt_block_args_tail  */
#line 3983 "ripper.y"
                    {
                        (yyval.val) = new_args(p, (yyvsp[-1].val), Qnone, Qnone, Qnone, (yyvsp[0].val), &(yyloc));
                    }
#line 14146 "ripper.c"
    break;

  case 433: /* block_param: f_block_optarg ',' f_rest_arg opt_block_args_tail  */
#line 3987 "ripper.y"
                    {
                        (yyval.val) = new_args(p, Qnone, (yyvsp[-3].val), (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yyloc));
                    }
#line 14154 "ripper.c"
    break;

  case 434: /* block_param: f_block_optarg ',' f_rest_arg ',' f_arg opt_block_args_tail  */
#line 3991 "ripper.y"
                    {
                        (yyval.val) = new_args(p, Qnone, (yyvsp[-5].val), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
                    }
#line 14162 "ripper.c"
    break;

  case 435: /* block_param: f_block_optarg opt_block_args_tail  */
#line 3995 "ripper.y"
                    {
                        (yyval.val) = new_args(p, Qnone, (yyvsp[-1].val), Qnone, Qnone, (yyvsp[0].val), &(yyloc));
                    }
#line 14170 "ripper.c"
    break;

  case 436: /* block_param: f_block_optarg ',' f_arg opt_block_args_tail  */
#line 3999 "ripper.y"
                    {
                        (yyval.val) = new_args(p, Qnone, (yyvsp[-3].val), Qnone, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
                    }
#line 14178 "ripper.c"
    break;

  case 437: /* block_param: f_rest_arg opt_block_args_tail  */
#line 4003 "ripper.y"
                    {
                        (yyval.val) = new_args(p, Qnone, Qnone, (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yyloc));
                    }
#line 14186 "ripper.c"
    break;

  case 438: /* block_param: f_rest_arg ',' f_arg opt_block_args_tail  */
#line 4007 "ripper.y"
                    {
                        (yyval.val) = new_args(p, Qnone, Qnone, (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
                    }
#line 14194 "ripper.c"
    break;

  case 439: /* block_param: block_args_tail  */
#line 4011 "ripper.y"
                    {
                        (yyval.val) = new_args(p, Qnone, Qnone, Qnone, Qnone, (yyvsp[0].val), &(yyloc));
                    }
#line 14202 "ripper.c"
    break;

  case 441: /* opt_block_param: block_param_def  */
#line 4018 "ripper.y"
                    {
                        p->command_start = TRUE;
                    }
#line 14210 "ripper.c"
    break;

  case 442: /* block_param_def: '|' opt_bv_decl '|'  */
#line 4024 "ripper.y"
                    {
                        p->cur_arg = 0;
                        p->max_numparam = ORDINAL_PARAM;
                        p->ctxt.in_argdef = 0;
#if 0
                        (yyval.val) = 0;
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11;v1=Qnil;v2=Qnil;v3=Qnil;v4=Qnil;v5=Qnil;v6=Qnil;v7=Qnil;v8=dispatch7(params,v1,v2,v3,v4,v5,v6,v7);v9=v8;v10=escape_Qundef((yyvsp[-1].val));v11=dispatch2(block_var,v9,v10);(yyval.val)=v11;}
                    }
#line 14224 "ripper.c"
    break;

  case 443: /* block_param_def: '|' block_param opt_bv_decl '|'  */
#line 4034 "ripper.y"
                    {
                        p->cur_arg = 0;
                        p->max_numparam = ORDINAL_PARAM;
                        p->ctxt.in_argdef = 0;
#if 0
                        (yyval.val) = (yyvsp[-2].val);
#endif
			{VALUE v1,v2,v3;v1=escape_Qundef((yyvsp[-2].val));v2=escape_Qundef((yyvsp[-1].val));v3=dispatch2(block_var,v1,v2);(yyval.val)=v3;}
                    }
#line 14238 "ripper.c"
    break;

  case 444: /* opt_bv_decl: opt_nl  */
#line 4047 "ripper.y"
                    {
                        (yyval.val) = 0;
                    }
#line 14246 "ripper.c"
    break;

  case 445: /* opt_bv_decl: opt_nl ';' bv_decls opt_nl  */
#line 4051 "ripper.y"
                    {
#if 0
                        (yyval.val) = 0;
#endif
			(yyval.val)=(yyvsp[-1].val);
                    }
#line 14257 "ripper.c"
    break;

  case 446: /* bv_decls: bvar  */
#line 4060 "ripper.y"
   {(yyval.val)=rb_ary_new3(1, get_value((yyvsp[0].val)));}
#line 14263 "ripper.c"
    break;

  case 447: /* bv_decls: bv_decls ',' bvar  */
#line 4062 "ripper.y"
   {(yyval.val)=rb_ary_push((yyvsp[-2].val), get_value((yyvsp[0].val)));}
#line 14269 "ripper.c"
    break;

  case 448: /* bvar: "local variable or method"  */
#line 4066 "ripper.y"
                    {
                        new_bv(p, get_id((yyvsp[0].val)));
			(yyval.val)=get_value((yyvsp[0].val));
                    }
#line 14278 "ripper.c"
    break;

  case 449: /* bvar: f_bad_arg  */
#line 4071 "ripper.y"
                    {
                        (yyval.val) = 0;
                    }
#line 14286 "ripper.c"
    break;

  case 450: /* @27: %empty  */
#line 4077 "ripper.y"
                    {
                        token_info_push(p, "->", &(yylsp[0]));
                        (yyvsp[0].vars) = dyna_push(p);
                        (yyval.num) = p->lex.lpar_beg;
                        p->lex.lpar_beg = p->lex.paren_nest;
                    }
#line 14297 "ripper.c"
    break;

  case 451: /* @28: %empty  */
#line 4083 "ripper.y"
                    {
                        (yyval.num) = p->max_numparam;
                        p->max_numparam = 0;
                    }
#line 14306 "ripper.c"
    break;

  case 452: /* @29: %empty  */
#line 4087 "ripper.y"
                    {
                        (yyval.node) = numparam_push(p);
                    }
#line 14314 "ripper.c"
    break;

  case 453: /* $@30: %empty  */
#line 4091 "ripper.y"
                    {
                        CMDARG_PUSH(0);
                    }
#line 14322 "ripper.c"
    break;

  case 454: /* lambda: "->" @27 @28 @29 f_larglist $@30 lambda_body  */
#line 4095 "ripper.y"
                    {
                        int max_numparam = p->max_numparam;
                        p->lex.lpar_beg = (yyvsp[-5].num);
                        p->max_numparam = (yyvsp[-4].num);
                        CMDARG_POP();
                        (yyvsp[-2].val) = args_with_numbered(p, (yyvsp[-2].val), max_numparam);
#if 0
                        {
                            YYLTYPE loc = code_loc_gen(&(yylsp[-2]), &(yylsp[0]));
                            (yyval.val) = NEW_LAMBDA((yyvsp[-2].val), (yyvsp[0].val), &loc);
                            nd_set_line((yyval.val)->nd_body, (yylsp[0]).end_pos.lineno);
                            nd_set_line((yyval.val), (yylsp[-2]).end_pos.lineno);
                            nd_set_first_loc((yyval.val), (yylsp[-6]).beg_pos);
                        }
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(lambda,v1,v2);(yyval.val)=v3;}
                        numparam_pop(p, (yyvsp[-3].node));
                        dyna_pop(p, (yyvsp[-6].vars));
                    }
#line 14346 "ripper.c"
    break;

  case 455: /* f_larglist: '(' f_args opt_bv_decl ')'  */
#line 4117 "ripper.y"
                    {
                        p->ctxt.in_argdef = 0;
#if 0
                        (yyval.val) = (yyvsp[-2].val);
                        p->max_numparam = ORDINAL_PARAM;
#endif
			{VALUE v1,v2;v1=(yyvsp[-2].val);v2=dispatch1(paren,v1);(yyval.val)=v2;}
                    }
#line 14359 "ripper.c"
    break;

  case 456: /* f_larglist: f_args  */
#line 4126 "ripper.y"
                    {
                        p->ctxt.in_argdef = 0;
#if 0
                        if (!args_info_empty_p((yyvsp[0].val)->nd_ainfo))
                            p->max_numparam = ORDINAL_PARAM;
#endif
                        (yyval.val) = (yyvsp[0].val);
                    }
#line 14372 "ripper.c"
    break;

  case 457: /* lambda_body: tLAMBEG compstmt '}'  */
#line 4137 "ripper.y"
                    {
                        token_info_pop(p, "}", &(yylsp[0]));
                        (yyval.val) = (yyvsp[-1].val);
                    }
#line 14381 "ripper.c"
    break;

  case 458: /* $@31: %empty  */
#line 4142 "ripper.y"
                    {
#if 0
                        push_end_expect_token_locations(p, &(yylsp[0]).beg_pos);
#endif
                    }
#line 14391 "ripper.c"
    break;

  case 459: /* lambda_body: "`do' for lambda" $@31 bodystmt k_end  */
#line 4148 "ripper.y"
                    {
                        (yyval.val) = (yyvsp[-1].val);
                    }
#line 14399 "ripper.c"
    break;

  case 460: /* do_block: k_do_block do_body k_end  */
#line 4154 "ripper.y"
                    {
                        (yyval.val) = (yyvsp[-1].val);
#if 0
                        (yyval.val)->nd_body->nd_loc = code_loc_gen(&(yylsp[-2]), &(yylsp[0]));
                        nd_set_line((yyval.val), (yylsp[-2]).end_pos.lineno);
#endif
                    }
#line 14411 "ripper.c"
    break;

  case 461: /* block_call: command do_block  */
#line 4164 "ripper.y"
                    {
#if 0
                        if (nd_type_p((yyvsp[-1].val), NODE_YIELD)) {
                            compile_error(p, "block given to yield");
                        }
                        else {
                            block_dup_check(p, (yyvsp[-1].val)->nd_args, (yyvsp[0].val));
                        }
                        (yyval.val) = method_add_block(p, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
                        fixpos((yyval.val), (yyvsp[-1].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=(yyvsp[0].val);v3=dispatch2(method_add_block,v1,v2);(yyval.val)=v3;}
                    }
#line 14429 "ripper.c"
    break;

  case 462: /* block_call: block_call call_op2 operation2 opt_paren_args  */
#line 4178 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_qcall(p, (yyvsp[-2].val), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=(yyvsp[-3].val);v2=(yyvsp[-2].val);v3=(yyvsp[-1].val);v4=dispatch3(call,v1,v2,v3);v5=v4;v6=(yyvsp[0].val);v7=v6==Qundef ? v5 : dispatch2(method_add_arg,v5,v6);(yyval.val)=v7;}
                    }
#line 14440 "ripper.c"
    break;

  case 463: /* block_call: block_call call_op2 operation2 opt_paren_args brace_block  */
#line 4185 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_command_qcall(p, (yyvsp[-3].val), (yyvsp[-4].val), (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-2]), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8;v1=(yyvsp[-4].val);v2=(yyvsp[-3].val);v3=(yyvsp[-2].val);v4=(yyvsp[-1].val);v5=dispatch4(command_call,v1,v2,v3,v4);v6=v5;v7=(yyvsp[0].val);v8=v7==Qundef ? v6 : dispatch2(method_add_block,v6,v7);(yyval.val)=v8;}
                    }
#line 14451 "ripper.c"
    break;

  case 464: /* block_call: block_call call_op2 operation2 command_args do_block  */
#line 4192 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_command_qcall(p, (yyvsp[-3].val), (yyvsp[-4].val), (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-2]), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8;v1=(yyvsp[-4].val);v2=(yyvsp[-3].val);v3=(yyvsp[-2].val);v4=(yyvsp[-1].val);v5=dispatch4(command_call,v1,v2,v3,v4);v6=v5;v7=(yyvsp[0].val);v8=dispatch2(method_add_block,v6,v7);(yyval.val)=v8;}
                    }
#line 14462 "ripper.c"
    break;

  case 465: /* method_call: fcall paren_args  */
#line 4201 "ripper.y"
                    {
#if 0
                        (yyval.val) = (yyvsp[-1].val);
                        (yyval.val)->nd_args = (yyvsp[0].val);
                        nd_set_last_loc((yyvsp[-1].val), (yylsp[0]).end_pos);
#endif
			{VALUE v1,v2,v3,v4,v5;v1=(yyvsp[-1].val);v2=dispatch1(fcall,v1);v3=v2;v4=(yyvsp[0].val);v5=dispatch2(method_add_arg,v3,v4);(yyval.val)=v5;}
                    }
#line 14475 "ripper.c"
    break;

  case 466: /* method_call: primary_value call_op operation2 opt_paren_args  */
#line 4210 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_qcall(p, (yyvsp[-2].val), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                        nd_set_line((yyval.val), (yylsp[-1]).end_pos.lineno);
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=(yyvsp[-3].val);v2=(yyvsp[-2].val);v3=(yyvsp[-1].val);v4=dispatch3(call,v1,v2,v3);v5=v4;v6=(yyvsp[0].val);v7=v6==Qundef ? v5 : dispatch2(method_add_arg,v5,v6);(yyval.val)=v7;}
                    }
#line 14487 "ripper.c"
    break;

  case 467: /* method_call: primary_value "::" operation2 paren_args  */
#line 4218 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_qcall(p, ID2VAL(idCOLON2), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                        nd_set_line((yyval.val), (yylsp[-1]).end_pos.lineno);
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=(yyvsp[-3].val);v2=ID2VAL(idCOLON2);v3=(yyvsp[-1].val);v4=dispatch3(call,v1,v2,v3);v5=v4;v6=(yyvsp[0].val);v7=dispatch2(method_add_arg,v5,v6);(yyval.val)=v7;}
                    }
#line 14499 "ripper.c"
    break;

  case 468: /* method_call: primary_value "::" operation3  */
#line 4226 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_qcall(p, ID2VAL(idCOLON2), (yyvsp[-2].val), (yyvsp[0].val), Qnull, &(yylsp[0]), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-2].val);v2=ID2VAL(idCOLON2);v3=(yyvsp[0].val);v4=dispatch3(call,v1,v2,v3);(yyval.val)=v4;}
                    }
#line 14510 "ripper.c"
    break;

  case 469: /* method_call: primary_value call_op paren_args  */
#line 4233 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_qcall(p, (yyvsp[-1].val), (yyvsp[-2].val), ID2VAL(idCall), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                        nd_set_line((yyval.val), (yylsp[-1]).end_pos.lineno);
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=ID2VAL(idCall);v4=dispatch3(call,v1,v2,v3);v5=v4;v6=(yyvsp[0].val);v7=dispatch2(method_add_arg,v5,v6);(yyval.val)=v7;}
                    }
#line 14522 "ripper.c"
    break;

  case 470: /* method_call: primary_value "::" paren_args  */
#line 4241 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_qcall(p, ID2VAL(idCOLON2), (yyvsp[-2].val), ID2VAL(idCall), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
                        nd_set_line((yyval.val), (yylsp[-1]).end_pos.lineno);
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=(yyvsp[-2].val);v2=ID2VAL(idCOLON2);v3=ID2VAL(idCall);v4=dispatch3(call,v1,v2,v3);v5=v4;v6=(yyvsp[0].val);v7=dispatch2(method_add_arg,v5,v6);(yyval.val)=v7;}
                    }
#line 14534 "ripper.c"
    break;

  case 471: /* method_call: "`super'" paren_args  */
#line 4249 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_SUPER((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(super,v1);(yyval.val)=v2;}
                    }
#line 14545 "ripper.c"
    break;

  case 472: /* method_call: "`super'"  */
#line 4256 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_ZSUPER(&(yyloc));
#endif
			{VALUE v1;v1=dispatch0(zsuper);(yyval.val)=v1;}
                    }
#line 14556 "ripper.c"
    break;

  case 473: /* method_call: primary_value '[' opt_call_args rbracket  */
#line 4263 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_CALL((yyvsp[-3].val), tAREF, (yyvsp[-1].val), &(yyloc));
                        fixpos((yyval.val), (yyvsp[-3].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-3].val);v2=escape_Qundef((yyvsp[-1].val));v3=dispatch2(aref,v1,v2);(yyval.val)=v3;}
                    }
#line 14568 "ripper.c"
    break;

  case 474: /* brace_block: '{' brace_body '}'  */
#line 4273 "ripper.y"
                    {
                        (yyval.val) = (yyvsp[-1].val);
#if 0
                        (yyval.val)->nd_body->nd_loc = code_loc_gen(&(yylsp[-2]), &(yylsp[0]));
                        nd_set_line((yyval.val), (yylsp[-2]).end_pos.lineno);
#endif
                    }
#line 14580 "ripper.c"
    break;

  case 475: /* brace_block: k_do do_body k_end  */
#line 4281 "ripper.y"
                    {
                        (yyval.val) = (yyvsp[-1].val);
#if 0
                        (yyval.val)->nd_body->nd_loc = code_loc_gen(&(yylsp[-2]), &(yylsp[0]));
                        nd_set_line((yyval.val), (yylsp[-2]).end_pos.lineno);
#endif
                    }
#line 14592 "ripper.c"
    break;

  case 476: /* @32: %empty  */
#line 4290 "ripper.y"
             {(yyval.vars) = dyna_push(p);}
#line 14598 "ripper.c"
    break;

  case 477: /* @33: %empty  */
#line 4291 "ripper.y"
                    {
                        (yyval.num) = p->max_numparam;
                        p->max_numparam = 0;
                    }
#line 14607 "ripper.c"
    break;

  case 478: /* @34: %empty  */
#line 4295 "ripper.y"
                    {
                        (yyval.node) = numparam_push(p);
                    }
#line 14615 "ripper.c"
    break;

  case 479: /* brace_body: @32 @33 @34 opt_block_param compstmt  */
#line 4299 "ripper.y"
                    {
                        int max_numparam = p->max_numparam;
                        p->max_numparam = (yyvsp[-3].num);
                        (yyvsp[-1].val) = args_with_numbered(p, (yyvsp[-1].val), max_numparam);
#if 0
                        (yyval.val) = NEW_ITER((yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=escape_Qundef((yyvsp[-1].val));v2=(yyvsp[0].val);v3=dispatch2(brace_block,v1,v2);(yyval.val)=v3;}
                        numparam_pop(p, (yyvsp[-2].node));
                        dyna_pop(p, (yyvsp[-4].vars));
                    }
#line 14631 "ripper.c"
    break;

  case 480: /* @35: %empty  */
#line 4312 "ripper.y"
           {(yyval.vars) = dyna_push(p);}
#line 14637 "ripper.c"
    break;

  case 481: /* @36: %empty  */
#line 4313 "ripper.y"
                    {
                        (yyval.num) = p->max_numparam;
                        p->max_numparam = 0;
                    }
#line 14646 "ripper.c"
    break;

  case 482: /* @37: %empty  */
#line 4317 "ripper.y"
                    {
                        (yyval.node) = numparam_push(p);
                        CMDARG_PUSH(0);
                    }
#line 14655 "ripper.c"
    break;

  case 483: /* do_body: @35 @36 @37 opt_block_param bodystmt  */
#line 4322 "ripper.y"
                    {
                        int max_numparam = p->max_numparam;
                        p->max_numparam = (yyvsp[-3].num);
                        (yyvsp[-1].val) = args_with_numbered(p, (yyvsp[-1].val), max_numparam);
#if 0
                        (yyval.val) = NEW_ITER((yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=escape_Qundef((yyvsp[-1].val));v2=(yyvsp[0].val);v3=dispatch2(do_block,v1,v2);(yyval.val)=v3;}
                        CMDARG_POP();
                        numparam_pop(p, (yyvsp[-2].node));
                        dyna_pop(p, (yyvsp[-4].vars));
                    }
#line 14672 "ripper.c"
    break;

  case 484: /* case_args: arg_value  */
#line 4337 "ripper.y"
                    {
#if 0
                        check_literal_when(p, (yyvsp[0].val), &(yylsp[0]));
                        (yyval.val) = NEW_LIST((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(args_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(args_add,v2,v3);(yyval.val)=v4;}
                    }
#line 14684 "ripper.c"
    break;

  case 485: /* case_args: "*" arg_value  */
#line 4345 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_SPLAT((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(args_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(args_add_star,v2,v3);(yyval.val)=v4;}
                    }
#line 14695 "ripper.c"
    break;

  case 486: /* case_args: case_args ',' arg_value  */
#line 4352 "ripper.y"
                    {
#if 0
                        check_literal_when(p, (yyvsp[0].val), &(yylsp[0]));
                        (yyval.val) = last_arg_append(p, (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(args_add,v1,v2);(yyval.val)=v3;}
                    }
#line 14707 "ripper.c"
    break;

  case 487: /* case_args: case_args ',' "*" arg_value  */
#line 4360 "ripper.y"
                    {
#if 0
                        (yyval.val) = rest_arg_append(p, (yyvsp[-3].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-3].val);v2=(yyvsp[0].val);v3=dispatch2(args_add_star,v1,v2);(yyval.val)=v3;}
                    }
#line 14718 "ripper.c"
    break;

  case 488: /* case_body: k_when case_args then compstmt cases  */
#line 4371 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_WHEN((yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
                        fixpos((yyval.val), (yyvsp[-3].val));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-3].val);v2=(yyvsp[-1].val);v3=escape_Qundef((yyvsp[0].val));v4=dispatch3(when,v1,v2,v3);(yyval.val)=v4;}
                    }
#line 14730 "ripper.c"
    break;

  case 491: /* @38: %empty  */
#line 4385 "ripper.y"
                    {
                        SET_LEX_STATE(EXPR_BEG|EXPR_LABEL);
                        p->command_start = FALSE;
                        (yyvsp[0].ctxt) = p->ctxt;
                        p->ctxt.in_kwarg = 1;
                        (yyval.tbl) = push_pvtbl(p);
                    }
#line 14742 "ripper.c"
    break;

  case 492: /* @39: %empty  */
#line 4392 "ripper.y"
                    {
                        (yyval.tbl) = push_pktbl(p);
                    }
#line 14750 "ripper.c"
    break;

  case 493: /* $@40: %empty  */
#line 4396 "ripper.y"
                    {
                        pop_pktbl(p, (yyvsp[-2].tbl));
                        pop_pvtbl(p, (yyvsp[-3].tbl));
                        p->ctxt.in_kwarg = (yyvsp[-4].ctxt).in_kwarg;
                    }
#line 14760 "ripper.c"
    break;

  case 494: /* p_case_body: "`in'" @38 @39 p_top_expr then $@40 compstmt p_cases  */
#line 4403 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_IN((yyvsp[-4].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-4].val);v2=(yyvsp[-1].val);v3=escape_Qundef((yyvsp[0].val));v4=dispatch3(in,v1,v2,v3);(yyval.val)=v4;}
                    }
#line 14771 "ripper.c"
    break;

  case 498: /* p_top_expr: p_top_expr_body "`if' modifier" expr_value  */
#line 4417 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_if(p, (yyvsp[0].val), (yyvsp[-2].val), 0, &(yyloc));
                        fixpos((yyval.val), (yyvsp[0].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[0].val);v2=(yyvsp[-2].val);v3=dispatch2(if_mod,v1,v2);(yyval.val)=v3;}
                    }
#line 14783 "ripper.c"
    break;

  case 499: /* p_top_expr: p_top_expr_body "`unless' modifier" expr_value  */
#line 4425 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_unless(p, (yyvsp[0].val), (yyvsp[-2].val), 0, &(yyloc));
                        fixpos((yyval.val), (yyvsp[0].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[0].val);v2=(yyvsp[-2].val);v3=dispatch2(unless_mod,v1,v2);(yyval.val)=v3;}
                    }
#line 14795 "ripper.c"
    break;

  case 501: /* p_top_expr_body: p_expr ','  */
#line 4436 "ripper.y"
                    {
                        (yyval.val) = new_array_pattern_tail(p, Qnone, 1, Qnone, Qnone, &(yyloc));
                        (yyval.val) = new_array_pattern(p, Qnone, get_value((yyvsp[-1].val)), (yyval.val), &(yyloc));
                    }
#line 14804 "ripper.c"
    break;

  case 502: /* p_top_expr_body: p_expr ',' p_args  */
#line 4441 "ripper.y"
                    {
                        (yyval.val) = new_array_pattern(p, Qnone, get_value((yyvsp[-2].val)), (yyvsp[0].val), &(yyloc));
#if 0
                        nd_set_first_loc((yyval.val), (yylsp[-2]).beg_pos);
#endif

                    }
#line 14816 "ripper.c"
    break;

  case 503: /* p_top_expr_body: p_find  */
#line 4449 "ripper.y"
                    {
                        (yyval.val) = new_find_pattern(p, Qnone, (yyvsp[0].val), &(yyloc));
                    }
#line 14824 "ripper.c"
    break;

  case 504: /* p_top_expr_body: p_args_tail  */
#line 4453 "ripper.y"
                    {
                        (yyval.val) = new_array_pattern(p, Qnone, Qnone, (yyvsp[0].val), &(yyloc));
                    }
#line 14832 "ripper.c"
    break;

  case 505: /* p_top_expr_body: p_kwargs  */
#line 4457 "ripper.y"
                    {
                        (yyval.val) = new_hash_pattern(p, Qnone, (yyvsp[0].val), &(yyloc));
                    }
#line 14840 "ripper.c"
    break;

  case 507: /* p_as: p_expr "=>" p_variable  */
#line 4466 "ripper.y"
                    {
#if 0
                        NODE *n = NEW_LIST((yyvsp[-2].val), &(yyloc));
                        n = list_append(p, n, (yyvsp[0].val));
                        (yyval.val) = new_hash(p, n, &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-2].val);v2=STATIC_ID2SYM(id_assoc);v3=(yyvsp[0].val);v4=dispatch3(binary,v1,v2,v3);(yyval.val)=v4;}
                    }
#line 14853 "ripper.c"
    break;

  case 509: /* p_alt: p_alt '|' p_expr_basic  */
#line 4478 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_NODE(NODE_OR, (yyvsp[-2].val), (yyvsp[0].val), 0, &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-2].val);v2=STATIC_ID2SYM(idOr);v3=(yyvsp[0].val);v4=dispatch3(binary,v1,v2,v3);(yyval.val)=v4;}
                    }
#line 14864 "ripper.c"
    break;

  case 511: /* p_lparen: '('  */
#line 4487 "ripper.y"
               {(yyval.tbl) = push_pktbl(p);}
#line 14870 "ripper.c"
    break;

  case 512: /* p_lbracket: '['  */
#line 4488 "ripper.y"
                 {(yyval.tbl) = push_pktbl(p);}
#line 14876 "ripper.c"
    break;

  case 515: /* p_expr_basic: p_const p_lparen p_args rparen  */
#line 4493 "ripper.y"
                    {
                        pop_pktbl(p, (yyvsp[-2].tbl));
                        (yyval.val) = new_array_pattern(p, (yyvsp[-3].val), Qnone, (yyvsp[-1].val), &(yyloc));
#if 0
                        nd_set_first_loc((yyval.val), (yylsp[-3]).beg_pos);
#endif

                    }
#line 14889 "ripper.c"
    break;

  case 516: /* p_expr_basic: p_const p_lparen p_find rparen  */
#line 4502 "ripper.y"
                    {
                        pop_pktbl(p, (yyvsp[-2].tbl));
                        (yyval.val) = new_find_pattern(p, (yyvsp[-3].val), (yyvsp[-1].val), &(yyloc));
#if 0
                        nd_set_first_loc((yyval.val), (yylsp[-3]).beg_pos);
#endif

                    }
#line 14902 "ripper.c"
    break;

  case 517: /* p_expr_basic: p_const p_lparen p_kwargs rparen  */
#line 4511 "ripper.y"
                    {
                        pop_pktbl(p, (yyvsp[-2].tbl));
                        (yyval.val) = new_hash_pattern(p, (yyvsp[-3].val), (yyvsp[-1].val), &(yyloc));
#if 0
                        nd_set_first_loc((yyval.val), (yylsp[-3]).beg_pos);
#endif

                    }
#line 14915 "ripper.c"
    break;

  case 518: /* p_expr_basic: p_const '(' rparen  */
#line 4520 "ripper.y"
                    {
                        (yyval.val) = new_array_pattern_tail(p, Qnone, 0, Qnone, Qnone, &(yyloc));
                        (yyval.val) = new_array_pattern(p, (yyvsp[-2].val), Qnone, (yyval.val), &(yyloc));
                    }
#line 14924 "ripper.c"
    break;

  case 519: /* p_expr_basic: p_const p_lbracket p_args rbracket  */
#line 4525 "ripper.y"
                    {
                        pop_pktbl(p, (yyvsp[-2].tbl));
                        (yyval.val) = new_array_pattern(p, (yyvsp[-3].val), Qnone, (yyvsp[-1].val), &(yyloc));
#if 0
                        nd_set_first_loc((yyval.val), (yylsp[-3]).beg_pos);
#endif

                    }
#line 14937 "ripper.c"
    break;

  case 520: /* p_expr_basic: p_const p_lbracket p_find rbracket  */
#line 4534 "ripper.y"
                    {
                        pop_pktbl(p, (yyvsp[-2].tbl));
                        (yyval.val) = new_find_pattern(p, (yyvsp[-3].val), (yyvsp[-1].val), &(yyloc));
#if 0
                        nd_set_first_loc((yyval.val), (yylsp[-3]).beg_pos);
#endif

                    }
#line 14950 "ripper.c"
    break;

  case 521: /* p_expr_basic: p_const p_lbracket p_kwargs rbracket  */
#line 4543 "ripper.y"
                    {
                        pop_pktbl(p, (yyvsp[-2].tbl));
                        (yyval.val) = new_hash_pattern(p, (yyvsp[-3].val), (yyvsp[-1].val), &(yyloc));
#if 0
                        nd_set_first_loc((yyval.val), (yylsp[-3]).beg_pos);
#endif

                    }
#line 14963 "ripper.c"
    break;

  case 522: /* p_expr_basic: p_const '[' rbracket  */
#line 4552 "ripper.y"
                    {
                        (yyval.val) = new_array_pattern_tail(p, Qnone, 0, Qnone, Qnone, &(yyloc));
                        (yyval.val) = new_array_pattern(p, (yyvsp[-2].val), Qnone, (yyval.val), &(yyloc));
                    }
#line 14972 "ripper.c"
    break;

  case 523: /* p_expr_basic: "[" p_args rbracket  */
#line 4557 "ripper.y"
                    {
                        (yyval.val) = new_array_pattern(p, Qnone, Qnone, (yyvsp[-1].val), &(yyloc));
                    }
#line 14980 "ripper.c"
    break;

  case 524: /* p_expr_basic: "[" p_find rbracket  */
#line 4561 "ripper.y"
                    {
                        (yyval.val) = new_find_pattern(p, Qnone, (yyvsp[-1].val), &(yyloc));
                    }
#line 14988 "ripper.c"
    break;

  case 525: /* p_expr_basic: "[" rbracket  */
#line 4565 "ripper.y"
                    {
                        (yyval.val) = new_array_pattern_tail(p, Qnone, 0, Qnone, Qnone, &(yyloc));
                        (yyval.val) = new_array_pattern(p, Qnone, Qnone, (yyval.val), &(yyloc));
                    }
#line 14997 "ripper.c"
    break;

  case 526: /* @41: %empty  */
#line 4570 "ripper.y"
                    {
                        (yyval.tbl) = push_pktbl(p);
                        (yyvsp[0].ctxt) = p->ctxt;
                        p->ctxt.in_kwarg = 0;
                    }
#line 15007 "ripper.c"
    break;

  case 527: /* p_expr_basic: "{" @41 p_kwargs rbrace  */
#line 4576 "ripper.y"
                    {
                        pop_pktbl(p, (yyvsp[-2].tbl));
                        p->ctxt.in_kwarg = (yyvsp[-3].ctxt).in_kwarg;
                        (yyval.val) = new_hash_pattern(p, Qnone, (yyvsp[-1].val), &(yyloc));
                    }
#line 15017 "ripper.c"
    break;

  case 528: /* p_expr_basic: "{" rbrace  */
#line 4582 "ripper.y"
                    {
                        (yyval.val) = new_hash_pattern_tail(p, Qnone, 0, &(yyloc));
                        (yyval.val) = new_hash_pattern(p, Qnone, (yyval.val), &(yyloc));
                    }
#line 15026 "ripper.c"
    break;

  case 529: /* @42: %empty  */
#line 4586 "ripper.y"
                          {(yyval.tbl) = push_pktbl(p);}
#line 15032 "ripper.c"
    break;

  case 530: /* p_expr_basic: "(" @42 p_expr rparen  */
#line 4587 "ripper.y"
                    {
                        pop_pktbl(p, (yyvsp[-2].tbl));
                        (yyval.val) = (yyvsp[-1].val);
                    }
#line 15041 "ripper.c"
    break;

  case 531: /* p_args: p_expr  */
#line 4594 "ripper.y"
                    {
#if 0
                        NODE *pre_args = NEW_LIST((yyvsp[0].val), &(yyloc));
                        (yyval.val) = new_array_pattern_tail(p, pre_args, 0, Qnone, Qnone, &(yyloc));
#endif
                        (yyval.val) = new_array_pattern_tail(p, rb_ary_new_from_args(1, get_value((yyvsp[0].val))), 0, Qnone, Qnone, &(yyloc));

                    }
#line 15054 "ripper.c"
    break;

  case 532: /* p_args: p_args_head  */
#line 4603 "ripper.y"
                    {
                        (yyval.val) = new_array_pattern_tail(p, (yyvsp[0].val), 1, Qnone, Qnone, &(yyloc));
                    }
#line 15062 "ripper.c"
    break;

  case 533: /* p_args: p_args_head p_arg  */
#line 4607 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_array_pattern_tail(p, list_concat((yyvsp[-1].val), (yyvsp[0].val)), 0, Qnone, Qnone, &(yyloc));
#endif
                        VALUE pre_args = rb_ary_concat((yyvsp[-1].val), get_value((yyvsp[0].val)));
                        (yyval.val) = new_array_pattern_tail(p, pre_args, 0, Qnone, Qnone, &(yyloc));

                    }
#line 15075 "ripper.c"
    break;

  case 534: /* p_args: p_args_head p_rest  */
#line 4616 "ripper.y"
                    {
                        (yyval.val) = new_array_pattern_tail(p, (yyvsp[-1].val), 1, (yyvsp[0].val), Qnone, &(yyloc));
                    }
#line 15083 "ripper.c"
    break;

  case 535: /* p_args: p_args_head p_rest ',' p_args_post  */
#line 4620 "ripper.y"
                    {
                        (yyval.val) = new_array_pattern_tail(p, (yyvsp[-3].val), 1, (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
                    }
#line 15091 "ripper.c"
    break;

  case 537: /* p_args_head: p_arg ','  */
#line 4627 "ripper.y"
                    {
                        (yyval.val) = (yyvsp[-1].val);
                    }
#line 15099 "ripper.c"
    break;

  case 538: /* p_args_head: p_args_head p_arg ','  */
#line 4631 "ripper.y"
                    {
#if 0
                        (yyval.val) = list_concat((yyvsp[-2].val), (yyvsp[-1].val));
#endif
			(yyval.val)=rb_ary_concat((yyvsp[-2].val), get_value((yyvsp[-1].val)));
                    }
#line 15110 "ripper.c"
    break;

  case 539: /* p_args_tail: p_rest  */
#line 4640 "ripper.y"
                    {
                        (yyval.val) = new_array_pattern_tail(p, Qnone, 1, (yyvsp[0].val), Qnone, &(yyloc));
                    }
#line 15118 "ripper.c"
    break;

  case 540: /* p_args_tail: p_rest ',' p_args_post  */
#line 4644 "ripper.y"
                    {
                        (yyval.val) = new_array_pattern_tail(p, Qnone, 1, (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
                    }
#line 15126 "ripper.c"
    break;

  case 541: /* p_find: p_rest ',' p_args_post ',' p_rest  */
#line 4650 "ripper.y"
                    {
                        (yyval.val) = new_find_pattern_tail(p, (yyvsp[-4].val), (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
                    }
#line 15134 "ripper.c"
    break;

  case 542: /* p_rest: "*" "local variable or method"  */
#line 4657 "ripper.y"
                    {
#if 0
                        error_duplicate_pattern_variable(p, (yyvsp[0].val), &(yylsp[0]));
                        (yyval.val) = assignable(p, (yyvsp[0].val), 0, &(yyloc));
#endif
			(yyval.val)=assignable(p, var_field(p, (yyvsp[0].val)));
                    }
#line 15146 "ripper.c"
    break;

  case 543: /* p_rest: "*"  */
#line 4665 "ripper.y"
                    {
#if 0
                        (yyval.val) = 0;
#endif
			(yyval.val)=var_field(p, Qnil);
                    }
#line 15157 "ripper.c"
    break;

  case 545: /* p_args_post: p_args_post ',' p_arg  */
#line 4675 "ripper.y"
                    {
#if 0
                        (yyval.val) = list_concat((yyvsp[-2].val), (yyvsp[0].val));
#endif
			(yyval.val)=rb_ary_concat((yyvsp[-2].val), get_value((yyvsp[0].val)));
                    }
#line 15168 "ripper.c"
    break;

  case 546: /* p_arg: p_expr  */
#line 4684 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_LIST((yyvsp[0].val), &(yyloc));
#endif
			(yyval.val)=rb_ary_new_from_args(1, get_value((yyvsp[0].val)));
                    }
#line 15179 "ripper.c"
    break;

  case 547: /* p_kwargs: p_kwarg ',' p_any_kwrest  */
#line 4693 "ripper.y"
                    {
                        (yyval.val) =  new_hash_pattern_tail(p, new_unique_key_hash(p, (yyvsp[-2].val), &(yyloc)), (yyvsp[0].val), &(yyloc));
                    }
#line 15187 "ripper.c"
    break;

  case 548: /* p_kwargs: p_kwarg  */
#line 4697 "ripper.y"
                    {
                        (yyval.val) =  new_hash_pattern_tail(p, new_unique_key_hash(p, (yyvsp[0].val), &(yyloc)), 0, &(yyloc));
                    }
#line 15195 "ripper.c"
    break;

  case 549: /* p_kwargs: p_kwarg ','  */
#line 4701 "ripper.y"
                    {
                        (yyval.val) =  new_hash_pattern_tail(p, new_unique_key_hash(p, (yyvsp[-1].val), &(yyloc)), 0, &(yyloc));
                    }
#line 15203 "ripper.c"
    break;

  case 550: /* p_kwargs: p_any_kwrest  */
#line 4705 "ripper.y"
                    {
                        (yyval.val) =  new_hash_pattern_tail(p, new_hash(p, Qnone, &(yyloc)), (yyvsp[0].val), &(yyloc));
                    }
#line 15211 "ripper.c"
    break;

  case 551: /* p_kwarg: p_kw  */
#line 4711 "ripper.y"
   {(yyval.val)=rb_ary_new_from_args(1, (yyvsp[0].val));}
#line 15217 "ripper.c"
    break;

  case 552: /* p_kwarg: p_kwarg ',' p_kw  */
#line 4713 "ripper.y"
                    {
#if 0
                        (yyval.val) = list_concat((yyvsp[-2].val), (yyvsp[0].val));
#endif
			(yyval.val)=rb_ary_push((yyvsp[-2].val), (yyvsp[0].val));
                    }
#line 15228 "ripper.c"
    break;

  case 553: /* p_kw: p_kw_label p_expr  */
#line 4722 "ripper.y"
                    {
                        error_duplicate_pattern_key(p, get_id((yyvsp[-1].val)), &(yylsp[-1]));
#if 0
                        (yyval.val) = list_append(p, NEW_LIST(NEW_LIT(ID2SYM((yyvsp[-1].val)), &(yylsp[-1])), &(yyloc)), (yyvsp[0].val));
#endif
			(yyval.val)=rb_ary_new_from_args(2, get_value((yyvsp[-1].val)), get_value((yyvsp[0].val)));
                    }
#line 15240 "ripper.c"
    break;

  case 554: /* p_kw: p_kw_label  */
#line 4730 "ripper.y"
                    {
                        error_duplicate_pattern_key(p, get_id((yyvsp[0].val)), &(yylsp[0]));
                        if ((yyvsp[0].val) && !is_local_id(get_id((yyvsp[0].val)))) {
                            yyerror1(&(yylsp[0]), "key must be valid as local variables");
                        }
                        error_duplicate_pattern_variable(p, get_id((yyvsp[0].val)), &(yylsp[0]));
#if 0
                        (yyval.val) = list_append(p, NEW_LIST(NEW_LIT(ID2SYM((yyvsp[0].val)), &(yyloc)), &(yyloc)), assignable(p, (yyvsp[0].val), 0, &(yyloc)));
#endif
			(yyval.val)=rb_ary_new_from_args(2, get_value(assignable(p, (yyvsp[0].val))), Qnil);
                    }
#line 15256 "ripper.c"
    break;

  case 556: /* p_kw_label: "string literal" string_contents tLABEL_END  */
#line 4745 "ripper.y"
                    {
                        YYLTYPE loc = code_loc_gen(&(yylsp[-2]), &(yylsp[0]));
#if 0
                        if (!(yyvsp[-1].val) || nd_type_p((yyvsp[-1].val), NODE_STR)) {
                            NODE *node = dsym_node(p, (yyvsp[-1].val), &loc);
                            (yyval.val) = SYM2ID(node->nd_lit);
                        }
#endif
                        if (ripper_is_node_yylval((yyvsp[-1].val)) && RNODE((yyvsp[-1].val))->nd_cval) {
                            VALUE label = RNODE((yyvsp[-1].val))->nd_cval;
                            VALUE rval = RNODE((yyvsp[-1].val))->nd_rval;
                            (yyval.val) = ripper_new_yylval(p, rb_intern_str(label), rval, label);
                            RNODE((yyval.val))->nd_loc = loc;
                        }

                        else {
                            yyerror1(&loc, "symbol literal with interpolation is not allowed");
                            (yyval.val) = 0;
                        }
                    }
#line 15281 "ripper.c"
    break;

  case 557: /* p_kwrest: kwrest_mark "local variable or method"  */
#line 4768 "ripper.y"
                    {
                        (yyval.val) = (yyvsp[0].val);
                    }
#line 15289 "ripper.c"
    break;

  case 558: /* p_kwrest: kwrest_mark  */
#line 4772 "ripper.y"
                    {
                        (yyval.val) = 0;
                    }
#line 15297 "ripper.c"
    break;

  case 559: /* p_kwnorest: kwrest_mark "`nil'"  */
#line 4778 "ripper.y"
                    {
                        (yyval.val) = 0;
                    }
#line 15305 "ripper.c"
    break;

  case 561: /* p_any_kwrest: p_kwnorest  */
#line 4784 "ripper.y"
                             {(yyval.val) = ID2VAL(idNil);}
#line 15311 "ripper.c"
    break;

  case 563: /* p_value: p_primitive ".." p_primitive  */
#line 4789 "ripper.y"
                    {
#if 0
                        value_expr((yyvsp[-2].val));
                        value_expr((yyvsp[0].val));
                        (yyval.val) = NEW_DOT2((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(dot2,v1,v2);(yyval.val)=v3;}
                    }
#line 15324 "ripper.c"
    break;

  case 564: /* p_value: p_primitive "..." p_primitive  */
#line 4798 "ripper.y"
                    {
#if 0
                        value_expr((yyvsp[-2].val));
                        value_expr((yyvsp[0].val));
                        (yyval.val) = NEW_DOT3((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(dot3,v1,v2);(yyval.val)=v3;}
                    }
#line 15337 "ripper.c"
    break;

  case 565: /* p_value: p_primitive ".."  */
#line 4807 "ripper.y"
                    {
#if 0
                        value_expr((yyvsp[-1].val));
                        (yyval.val) = NEW_DOT2((yyvsp[-1].val), new_nil_at(p, &(yylsp[0]).end_pos), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=Qnil;v3=dispatch2(dot2,v1,v2);(yyval.val)=v3;}
                    }
#line 15349 "ripper.c"
    break;

  case 566: /* p_value: p_primitive "..."  */
#line 4815 "ripper.y"
                    {
#if 0
                        value_expr((yyvsp[-1].val));
                        (yyval.val) = NEW_DOT3((yyvsp[-1].val), new_nil_at(p, &(yylsp[0]).end_pos), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=Qnil;v3=dispatch2(dot3,v1,v2);(yyval.val)=v3;}
                    }
#line 15361 "ripper.c"
    break;

  case 570: /* p_value: "(.." p_primitive  */
#line 4826 "ripper.y"
                    {
#if 0
                        value_expr((yyvsp[0].val));
                        (yyval.val) = NEW_DOT2(new_nil_at(p, &(yylsp[-1]).beg_pos), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=Qnil;v2=(yyvsp[0].val);v3=dispatch2(dot2,v1,v2);(yyval.val)=v3;}
                    }
#line 15373 "ripper.c"
    break;

  case 571: /* p_value: "(..." p_primitive  */
#line 4834 "ripper.y"
                    {
#if 0
                        value_expr((yyvsp[0].val));
                        (yyval.val) = NEW_DOT3(new_nil_at(p, &(yylsp[-1]).beg_pos), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=Qnil;v2=(yyvsp[0].val);v3=dispatch2(dot3,v1,v2);(yyval.val)=v3;}
                    }
#line 15385 "ripper.c"
    break;

  case 580: /* p_primitive: keyword_variable  */
#line 4852 "ripper.y"
                    {
#if 0
                        if (!((yyval.val) = gettable(p, (yyvsp[0].val), &(yyloc)))) (yyval.val) = NEW_BEGIN(0, &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(var_ref,v1);(yyval.val)=v2;}
                    }
#line 15396 "ripper.c"
    break;

  case 582: /* p_variable: "local variable or method"  */
#line 4862 "ripper.y"
                    {
#if 0
                        error_duplicate_pattern_variable(p, (yyvsp[0].val), &(yylsp[0]));
                        (yyval.val) = assignable(p, (yyvsp[0].val), 0, &(yyloc));
#endif
			(yyval.val)=assignable(p, var_field(p, (yyvsp[0].val)));
                    }
#line 15408 "ripper.c"
    break;

  case 583: /* p_var_ref: '^' "local variable or method"  */
#line 4872 "ripper.y"
                    {
#if 0
                        NODE *n = gettable(p, (yyvsp[0].val), &(yyloc));
                        if (!(nd_type_p(n, NODE_LVAR) || nd_type_p(n, NODE_DVAR))) {
                            compile_error(p, "%"PRIsVALUE": no such local variable", rb_id2str((yyvsp[0].val)));
                        }
                        (yyval.val) = n;
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(var_ref,v1);(yyval.val)=v2;}
                    }
#line 15423 "ripper.c"
    break;

  case 584: /* p_var_ref: '^' nonlocal_var  */
#line 4883 "ripper.y"
                    {
#if 0
                        if (!((yyval.val) = gettable(p, (yyvsp[0].val), &(yyloc)))) (yyval.val) = NEW_BEGIN(0, &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(var_ref,v1);(yyval.val)=v2;}
                    }
#line 15434 "ripper.c"
    break;

  case 585: /* p_expr_ref: '^' "(" expr_value rparen  */
#line 4892 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_BEGIN((yyvsp[-1].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(begin,v1);(yyval.val)=v2;}
                    }
#line 15445 "ripper.c"
    break;

  case 586: /* p_const: ":: at EXPR_BEG" cname  */
#line 4901 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_COLON3((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(top_const_ref,v1);(yyval.val)=v2;}
                    }
#line 15456 "ripper.c"
    break;

  case 587: /* p_const: p_const "::" cname  */
#line 4908 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_COLON2((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(const_path_ref,v1,v2);(yyval.val)=v3;}
                    }
#line 15467 "ripper.c"
    break;

  case 588: /* p_const: "constant"  */
#line 4915 "ripper.y"
                   {
#if 0
                        (yyval.val) = gettable(p, (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(var_ref,v1);(yyval.val)=v2;}
                   }
#line 15478 "ripper.c"
    break;

  case 589: /* opt_rescue: k_rescue exc_list exc_var then compstmt opt_rescue  */
#line 4926 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_RESBODY((yyvsp[-4].val),
                                         (yyvsp[-3].val) ? block_append(p, node_assign(p, (yyvsp[-3].val), NEW_ERRINFO(&(yylsp[-3])), NO_LEX_CTXT, &(yylsp[-3])), (yyvsp[-1].val)) : (yyvsp[-1].val),
                                         (yyvsp[0].val), &(yyloc));

                        if ((yyvsp[-4].val)) {
                            fixpos((yyval.val), (yyvsp[-4].val));
                        }
                        else if ((yyvsp[-3].val)) {
                            fixpos((yyval.val), (yyvsp[-3].val));
                        }
                        else {
                            fixpos((yyval.val), (yyvsp[-1].val));
                        }
#endif
			{VALUE v1,v2,v3,v4,v5;v1=escape_Qundef((yyvsp[-4].val));v2=escape_Qundef((yyvsp[-3].val));v3=escape_Qundef((yyvsp[-1].val));v4=escape_Qundef((yyvsp[0].val));v5=dispatch4(rescue,v1,v2,v3,v4);(yyval.val)=v5;}
                    }
#line 15501 "ripper.c"
    break;

  case 591: /* exc_list: arg_value  */
#line 4948 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_LIST((yyvsp[0].val), &(yyloc));
#endif
			(yyval.val)=rb_ary_new3(1, get_value((yyvsp[0].val)));
                    }
#line 15512 "ripper.c"
    break;

  case 592: /* exc_list: mrhs  */
#line 4955 "ripper.y"
                    {
#if 0
                        if (!((yyval.val) = splat_array((yyvsp[0].val)))) (yyval.val) = (yyvsp[0].val);
#endif
			(yyval.val)=(yyvsp[0].val);
                    }
#line 15523 "ripper.c"
    break;

  case 594: /* exc_var: "=>" lhs  */
#line 4965 "ripper.y"
                    {
                        (yyval.val) = (yyvsp[0].val);
                    }
#line 15531 "ripper.c"
    break;

  case 596: /* opt_ensure: k_ensure compstmt  */
#line 4972 "ripper.y"
                    {
#if 0
                        (yyval.val) = (yyvsp[0].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(ensure,v1);(yyval.val)=v2;}
                    }
#line 15542 "ripper.c"
    break;

  case 600: /* strings: string  */
#line 4986 "ripper.y"
                    {
#if 0
                        NODE *node = (yyvsp[0].val);
                        if (!node) {
                            node = NEW_STR(STR_NEW0(), &(yyloc));
                            RB_OBJ_WRITTEN(p->ast, Qnil, node->nd_lit);
                        }
                        else {
                            node = evstr2dstr(p, node);
                        }
                        (yyval.val) = node;
#endif
			(yyval.val)=(yyvsp[0].val);
                    }
#line 15561 "ripper.c"
    break;

  case 603: /* string: string string1  */
#line 5005 "ripper.y"
                    {
#if 0
                        (yyval.val) = literal_concat(p, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=(yyvsp[0].val);v3=dispatch2(string_concat,v1,v2);(yyval.val)=v3;}
                    }
#line 15572 "ripper.c"
    break;

  case 604: /* string1: "string literal" string_contents "terminator"  */
#line 5014 "ripper.y"
                    {
#if 0
                        (yyval.val) = heredoc_dedent(p, (yyvsp[-1].val));
                        if ((yyval.val)) nd_set_loc((yyval.val), &(yyloc));
#endif
			{VALUE v1,v2;v1=heredoc_dedent(p, (yyvsp[-1].val));v2=dispatch1(string_literal,v1);(yyval.val)=v2;}
                    }
#line 15584 "ripper.c"
    break;

  case 605: /* xstring: "backtick literal" xstring_contents "terminator"  */
#line 5024 "ripper.y"
                    {
#if 0
                        (yyval.val) = new_xstring(p, heredoc_dedent(p, (yyvsp[-1].val)), &(yyloc));
#endif
			{VALUE v1,v2;v1=heredoc_dedent(p, (yyvsp[-1].val));v2=dispatch1(xstring_literal,v1);(yyval.val)=v2;}
                    }
#line 15595 "ripper.c"
    break;

  case 606: /* regexp: "regexp literal" regexp_contents tREGEXP_END  */
#line 5033 "ripper.y"
                    {
                        (yyval.val) = new_regexp(p, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
                    }
#line 15603 "ripper.c"
    break;

  case 607: /* words_sep: ' '  */
#line 5038 "ripper.y"
                {}
#line 15609 "ripper.c"
    break;

  case 609: /* words: "word list" words_sep word_list "terminator"  */
#line 5043 "ripper.y"
                    {
#if 0
                        (yyval.val) = make_list((yyvsp[-1].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(array,v1);(yyval.val)=v2;}
                    }
#line 15620 "ripper.c"
    break;

  case 610: /* word_list: %empty  */
#line 5052 "ripper.y"
                    {
#if 0
                        (yyval.val) = 0;
#endif
			{VALUE v1;v1=dispatch0(words_new);(yyval.val)=v1;}
                    }
#line 15631 "ripper.c"
    break;

  case 611: /* word_list: word_list word words_sep  */
#line 5059 "ripper.y"
                    {
#if 0
                        (yyval.val) = list_append(p, (yyvsp[-2].val), evstr2dstr(p, (yyvsp[-1].val)));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=dispatch2(words_add,v1,v2);(yyval.val)=v3;}
                    }
#line 15642 "ripper.c"
    break;

  case 612: /* word: string_content  */
#line 5068 "ripper.y"
   {{VALUE v1,v2,v3,v4;v1=dispatch0(word_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(word_add,v2,v3);(yyval.val)=v4;}}
#line 15648 "ripper.c"
    break;

  case 613: /* word: word string_content  */
#line 5070 "ripper.y"
                    {
#if 0
                        (yyval.val) = literal_concat(p, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=(yyvsp[0].val);v3=dispatch2(word_add,v1,v2);(yyval.val)=v3;}
                    }
#line 15659 "ripper.c"
    break;

  case 614: /* symbols: "symbol list" words_sep symbol_list "terminator"  */
#line 5079 "ripper.y"
                    {
#if 0
                        (yyval.val) = make_list((yyvsp[-1].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(array,v1);(yyval.val)=v2;}
                    }
#line 15670 "ripper.c"
    break;

  case 615: /* symbol_list: %empty  */
#line 5088 "ripper.y"
                    {
#if 0
                        (yyval.val) = 0;
#endif
			{VALUE v1;v1=dispatch0(symbols_new);(yyval.val)=v1;}
                    }
#line 15681 "ripper.c"
    break;

  case 616: /* symbol_list: symbol_list word words_sep  */
#line 5095 "ripper.y"
                    {
#if 0
                        (yyval.val) = symbol_append(p, (yyvsp[-2].val), evstr2dstr(p, (yyvsp[-1].val)));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=dispatch2(symbols_add,v1,v2);(yyval.val)=v3;}
                    }
#line 15692 "ripper.c"
    break;

  case 617: /* qwords: "verbatim word list" words_sep qword_list "terminator"  */
#line 5104 "ripper.y"
                    {
#if 0
                        (yyval.val) = make_list((yyvsp[-1].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(array,v1);(yyval.val)=v2;}
                    }
#line 15703 "ripper.c"
    break;

  case 618: /* qsymbols: "verbatim symbol list" words_sep qsym_list "terminator"  */
#line 5113 "ripper.y"
                    {
#if 0
                        (yyval.val) = make_list((yyvsp[-1].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(array,v1);(yyval.val)=v2;}
                    }
#line 15714 "ripper.c"
    break;

  case 619: /* qword_list: %empty  */
#line 5122 "ripper.y"
                    {
#if 0
                        (yyval.val) = 0;
#endif
			{VALUE v1;v1=dispatch0(qwords_new);(yyval.val)=v1;}
                    }
#line 15725 "ripper.c"
    break;

  case 620: /* qword_list: qword_list "literal content" words_sep  */
#line 5129 "ripper.y"
                    {
#if 0
                        (yyval.val) = list_append(p, (yyvsp[-2].val), (yyvsp[-1].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=dispatch2(qwords_add,v1,v2);(yyval.val)=v3;}
                    }
#line 15736 "ripper.c"
    break;

  case 621: /* qsym_list: %empty  */
#line 5138 "ripper.y"
                    {
#if 0
                        (yyval.val) = 0;
#endif
			{VALUE v1;v1=dispatch0(qsymbols_new);(yyval.val)=v1;}
                    }
#line 15747 "ripper.c"
    break;

  case 622: /* qsym_list: qsym_list "literal content" words_sep  */
#line 5145 "ripper.y"
                    {
#if 0
                        (yyval.val) = symbol_append(p, (yyvsp[-2].val), (yyvsp[-1].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=dispatch2(qsymbols_add,v1,v2);(yyval.val)=v3;}
                    }
#line 15758 "ripper.c"
    break;

  case 623: /* string_contents: %empty  */
#line 5154 "ripper.y"
                    {
#if 0
                        (yyval.val) = 0;
#endif
			{VALUE v1;v1=dispatch0(string_content);(yyval.val)=v1;}
#if 0
#endif
                        (yyval.val) = ripper_new_yylval(p, 0, (yyval.val), 0);

                    }
#line 15773 "ripper.c"
    break;

  case 624: /* string_contents: string_contents string_content  */
#line 5165 "ripper.y"
                    {
#if 0
                        (yyval.val) = literal_concat(p, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=(yyvsp[0].val);v3=dispatch2(string_add,v1,v2);(yyval.val)=v3;}
#if 0
#endif
                        if (ripper_is_node_yylval((yyvsp[-1].val)) && ripper_is_node_yylval((yyvsp[0].val)) &&
                            !RNODE((yyvsp[-1].val))->nd_cval) {
                            RNODE((yyvsp[-1].val))->nd_cval = RNODE((yyvsp[0].val))->nd_cval;
                            RNODE((yyvsp[-1].val))->nd_rval = add_mark_object(p, (yyval.val));
                            (yyval.val) = (yyvsp[-1].val);
                        }

                    }
#line 15793 "ripper.c"
    break;

  case 625: /* xstring_contents: %empty  */
#line 5183 "ripper.y"
                    {
#if 0
                        (yyval.val) = 0;
#endif
			{VALUE v1;v1=dispatch0(xstring_new);(yyval.val)=v1;}
                    }
#line 15804 "ripper.c"
    break;

  case 626: /* xstring_contents: xstring_contents string_content  */
#line 5190 "ripper.y"
                    {
#if 0
                        (yyval.val) = literal_concat(p, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=(yyvsp[0].val);v3=dispatch2(xstring_add,v1,v2);(yyval.val)=v3;}
                    }
#line 15815 "ripper.c"
    break;

  case 627: /* regexp_contents: %empty  */
#line 5199 "ripper.y"
                    {
#if 0
                        (yyval.val) = 0;
#endif
			{VALUE v1;v1=dispatch0(regexp_new);(yyval.val)=v1;}
#if 0
#endif
                        (yyval.val) = ripper_new_yylval(p, 0, (yyval.val), 0);

                    }
#line 15830 "ripper.c"
    break;

  case 628: /* regexp_contents: regexp_contents string_content  */
#line 5210 "ripper.y"
                    {
#if 0
                        NODE *head = (yyvsp[-1].val), *tail = (yyvsp[0].val);
                        if (!head) {
                            (yyval.val) = tail;
                        }
                        else if (!tail) {
                            (yyval.val) = head;
                        }
                        else {
                            switch (nd_type(head)) {
                              case NODE_STR:
                                nd_set_type(head, NODE_DSTR);
                                break;
                              case NODE_DSTR:
                                break;
                              default:
                                head = list_append(p, NEW_DSTR(Qnil, &(yyloc)), head);
                                break;
                            }
                            (yyval.val) = list_append(p, head, tail);
                        }
#endif
                        VALUE s1 = 1, s2 = 0, n1 = (yyvsp[-1].val), n2 = (yyvsp[0].val);
                        if (ripper_is_node_yylval(n1)) {
                            s1 = RNODE(n1)->nd_cval;
                            n1 = RNODE(n1)->nd_rval;
                        }
                        if (ripper_is_node_yylval(n2)) {
                            s2 = RNODE(n2)->nd_cval;
                            n2 = RNODE(n2)->nd_rval;
                        }
                        (yyval.val) = dispatch2(regexp_add, n1, n2);
                        if (!s1 && s2) {
                            (yyval.val) = ripper_new_yylval(p, 0, (yyval.val), s2);
                        }

                    }
#line 15873 "ripper.c"
    break;

  case 629: /* string_content: "literal content"  */
#line 5251 "ripper.y"
   {(yyval.val)=ripper_new_yylval(p, 0, get_value((yyvsp[0].val)), (yyvsp[0].val));}
#line 15879 "ripper.c"
    break;

  case 630: /* @43: %empty  */
#line 5253 "ripper.y"
                    {
                        /* need to backup p->lex.strterm so that a string literal `%&foo,#$&,bar&` can be parsed */
                        (yyval.strterm) = p->lex.strterm;
                        p->lex.strterm = 0;
                        SET_LEX_STATE(EXPR_BEG);
                    }
#line 15890 "ripper.c"
    break;

  case 631: /* string_content: tSTRING_DVAR @43 string_dvar  */
#line 5260 "ripper.y"
                    {
                        p->lex.strterm = (yyvsp[-1].strterm);
#if 0
                        (yyval.val) = NEW_EVSTR((yyvsp[0].val), &(yyloc));
                        nd_set_line((yyval.val), (yylsp[0]).end_pos.lineno);
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(string_dvar,v1);(yyval.val)=v2;}
                    }
#line 15903 "ripper.c"
    break;

  case 632: /* $@44: %empty  */
#line 5269 "ripper.y"
                    {
                        CMDARG_PUSH(0);
                        COND_PUSH(0);
                    }
#line 15912 "ripper.c"
    break;

  case 633: /* @45: %empty  */
#line 5273 "ripper.y"
                    {
                        /* need to backup p->lex.strterm so that a string literal `%!foo,#{ !0 },bar!` can be parsed */
                        (yyval.strterm) = p->lex.strterm;
                        p->lex.strterm = 0;
                    }
#line 15922 "ripper.c"
    break;

  case 634: /* @46: %empty  */
#line 5278 "ripper.y"
                    {
                        (yyval.num) = p->lex.state;
                        SET_LEX_STATE(EXPR_BEG);
                    }
#line 15931 "ripper.c"
    break;

  case 635: /* @47: %empty  */
#line 5282 "ripper.y"
                    {
                        (yyval.num) = p->lex.brace_nest;
                        p->lex.brace_nest = 0;
                    }
#line 15940 "ripper.c"
    break;

  case 636: /* @48: %empty  */
#line 5286 "ripper.y"
                    {
                        (yyval.num) = p->heredoc_indent;
                        p->heredoc_indent = 0;
                    }
#line 15949 "ripper.c"
    break;

  case 637: /* string_content: tSTRING_DBEG $@44 @45 @46 @47 @48 compstmt "'}'"  */
#line 5291 "ripper.y"
                    {
                        COND_POP();
                        CMDARG_POP();
                        p->lex.strterm = (yyvsp[-5].strterm);
                        SET_LEX_STATE((yyvsp[-4].num));
                        p->lex.brace_nest = (yyvsp[-3].num);
                        p->heredoc_indent = (yyvsp[-2].num);
                        p->heredoc_line_indent = -1;
#if 0
                        if ((yyvsp[-1].val)) (yyvsp[-1].val)->flags &= ~NODE_FL_NEWLINE;
                        (yyval.val) = new_evstr(p, (yyvsp[-1].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(string_embexpr,v1);(yyval.val)=v2;}
                    }
#line 15968 "ripper.c"
    break;

  case 638: /* string_dvar: "global variable"  */
#line 5308 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_GVAR((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(var_ref,v1);(yyval.val)=v2;}
                    }
#line 15979 "ripper.c"
    break;

  case 639: /* string_dvar: "instance variable"  */
#line 5315 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_IVAR((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(var_ref,v1);(yyval.val)=v2;}
                    }
#line 15990 "ripper.c"
    break;

  case 640: /* string_dvar: "class variable"  */
#line 5322 "ripper.y"
                    {
#if 0
                        (yyval.val) = NEW_CVAR((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(var_ref,v1);(yyval.val)=v2;}
                    }
#line 16001 "ripper.c"
    break;

  case 644: /* ssym: "symbol literal" sym  */
#line 5336 "ripper.y"
                    {
                        SET_LEX_STATE(EXPR_END);
#if 0
                        (yyval.val) = NEW_LIT(ID2SYM((yyvsp[0].val)), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[0].val);v2=dispatch1(symbol,v1);v3=v2;v4=dispatch1(symbol_literal,v3);(yyval.val)=v4;}
                    }
#line 16013 "ripper.c"
    break;

  case 647: /* dsym: "symbol literal" string_contents "terminator"  */
#line 5350 "ripper.y"
                    {
                        SET_LEX_STATE(EXPR_END);
#if 0
                        (yyval.val) = dsym_node(p, (yyvsp[-1].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(dyna_symbol,v1);(yyval.val)=v2;}
                    }
#line 16025 "ripper.c"
    break;

  case 649: /* numeric: tUMINUS_NUM simple_numeric  */
#line 5361 "ripper.y"
                    {
#if 0
                        (yyval.val) = (yyvsp[0].val);
                        RB_OBJ_WRITE(p->ast, &(yyval.val)->nd_lit, negate_lit(p, (yyval.val)->nd_lit));
#endif
			{VALUE v1,v2,v3;v1=ID2VAL(idUMinus);v2=(yyvsp[0].val);v3=dispatch2(unary,v1,v2);(yyval.val)=v3;}
                    }
#line 16037 "ripper.c"
    break;

  case 660: /* keyword_variable: "`nil'"  */
#line 5386 "ripper.y"
                              {(yyval.val) = KWD2EID(nil, (yyvsp[0].val));}
#line 16043 "ripper.c"
    break;

  case 661: /* keyword_variable: "`self'"  */
#line 5387 "ripper.y"
                               {(yyval.val) = KWD2EID(self, (yyvsp[0].val));}
#line 16049 "ripper.c"
    break;

  case 662: /* keyword_variable: "`true'"  */
#line 5388 "ripper.y"
                               {(yyval.val) = KWD2EID(true, (yyvsp[0].val));}
#line 16055 "ripper.c"
    break;

  case 663: /* keyword_variable: "`false'"  */
#line 5389 "ripper.y"
                                {(yyval.val) = KWD2EID(false, (yyvsp[0].val));}
#line 16061 "ripper.c"
    break;

  case 664: /* keyword_variable: "`__FILE__'"  */
#line 5390 "ripper.y"
                                  {(yyval.val) = KWD2EID(_FILE__, (yyvsp[0].val));}
#line 16067 "ripper.c"
    break;

  case 665: /* keyword_variable: "`__LINE__'"  */
#line 5391 "ripper.y"
                                  {(yyval.val) = KWD2EID(_LINE__, (yyvsp[0].val));}
#line 16073 "ripper.c"
    break;

  case 666: /* keyword_variable: "`__ENCODING__'"  */
#line 5392 "ripper.y"
                                      {(yyval.val) = KWD2EID(_ENCODING__, (yyvsp[0].val));}
#line 16079 "ripper.c"
    break;

  case 667: /* var_ref: user_variable  */
#line 5396 "ripper.y"
                    {
#if 0
                        if (!((yyval.val) = gettable(p, (yyvsp[0].val), &(yyloc)))) (yyval.val) = NEW_BEGIN(0, &(yyloc));
#endif
                        if (id_is_var(p, get_id((yyvsp[0].val)))) {
                            (yyval.val) = dispatch1(var_ref, (yyvsp[0].val));
                        }
                        else {
                            (yyval.val) = dispatch1(vcall, (yyvsp[0].val));
                        }

                    }
#line 16096 "ripper.c"
    break;

  case 668: /* var_ref: keyword_variable  */
#line 5409 "ripper.y"
                    {
#if 0
                        if (!((yyval.val) = gettable(p, (yyvsp[0].val), &(yyloc)))) (yyval.val) = NEW_BEGIN(0, &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(var_ref,v1);(yyval.val)=v2;}
                    }
#line 16107 "ripper.c"
    break;

  case 669: /* var_lhs: user_variable  */
#line 5418 "ripper.y"
                    {
#if 0
                        (yyval.val) = assignable(p, (yyvsp[0].val), 0, &(yyloc));
#endif
			(yyval.val)=assignable(p, var_field(p, (yyvsp[0].val)));
                    }
#line 16118 "ripper.c"
    break;

  case 670: /* var_lhs: keyword_variable  */
#line 5425 "ripper.y"
                    {
#if 0
                        (yyval.val) = assignable(p, (yyvsp[0].val), 0, &(yyloc));
#endif
			(yyval.val)=assignable(p, var_field(p, (yyvsp[0].val)));
                    }
#line 16129 "ripper.c"
    break;

  case 673: /* $@49: %empty  */
#line 5438 "ripper.y"
                    {
                        SET_LEX_STATE(EXPR_BEG);
                        p->command_start = TRUE;
                    }
#line 16138 "ripper.c"
    break;

  case 674: /* superclass: '<' $@49 expr_value term  */
#line 5443 "ripper.y"
                    {
                        (yyval.val) = (yyvsp[-1].val);
                    }
#line 16146 "ripper.c"
    break;

  case 675: /* superclass: %empty  */
#line 5447 "ripper.y"
                    {
#if 0
                        (yyval.val) = 0;
#endif
			(yyval.val)=Qnil;
                    }
#line 16157 "ripper.c"
    break;

  case 677: /* f_opt_paren_args: none  */
#line 5457 "ripper.y"
                    {
                        p->ctxt.in_argdef = 0;
                        (yyval.val) = new_args_tail(p, Qnone, Qnone, Qnone, &(yylsp[-1]));
                        (yyval.val) = new_args(p, Qnone, Qnone, Qnone, Qnone, (yyval.val), &(yylsp[-1]));
                    }
#line 16167 "ripper.c"
    break;

  case 678: /* f_paren_args: '(' f_args rparen  */
#line 5465 "ripper.y"
                    {
#if 0
                        (yyval.val) = (yyvsp[-1].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(paren,v1);(yyval.val)=v2;}
                        SET_LEX_STATE(EXPR_BEG);
                        p->command_start = TRUE;
                        p->ctxt.in_argdef = 0;
                    }
#line 16181 "ripper.c"
    break;

  case 680: /* @50: %empty  */
#line 5477 "ripper.y"
                    {
                        (yyval.ctxt) = p->ctxt;
                        p->ctxt.in_kwarg = 1;
                        p->ctxt.in_argdef = 1;
                        SET_LEX_STATE(p->lex.state|EXPR_LABEL); /* force for args */
                    }
#line 16192 "ripper.c"
    break;

  case 681: /* f_arglist: @50 f_args term  */
#line 5484 "ripper.y"
                    {
                        p->ctxt.in_kwarg = (yyvsp[-2].ctxt).in_kwarg;
                        p->ctxt.in_argdef = 0;
                        (yyval.val) = (yyvsp[-1].val);
                        SET_LEX_STATE(EXPR_BEG);
                        p->command_start = TRUE;
                    }
#line 16204 "ripper.c"
    break;

  case 682: /* args_tail: f_kwarg ',' f_kwrest opt_f_block_arg  */
#line 5494 "ripper.y"
                    {
                        (yyval.val) = new_args_tail(p, (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]));
                    }
#line 16212 "ripper.c"
    break;

  case 683: /* args_tail: f_kwarg opt_f_block_arg  */
#line 5498 "ripper.y"
                    {
                        (yyval.val) = new_args_tail(p, (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yylsp[-1]));
                    }
#line 16220 "ripper.c"
    break;

  case 684: /* args_tail: f_any_kwrest opt_f_block_arg  */
#line 5502 "ripper.y"
                    {
                        (yyval.val) = new_args_tail(p, Qnone, (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]));
                    }
#line 16228 "ripper.c"
    break;

  case 685: /* args_tail: f_block_arg  */
#line 5506 "ripper.y"
                    {
                        (yyval.val) = new_args_tail(p, Qnone, Qnone, (yyvsp[0].val), &(yylsp[0]));
                    }
#line 16236 "ripper.c"
    break;

  case 686: /* args_tail: args_forward  */
#line 5510 "ripper.y"
                    {
                        add_forwarding_args(p);
                        (yyval.val) = new_args_tail(p, Qnone, (yyvsp[0].val), ID2VAL(idFWD_BLOCK), &(yylsp[0]));
#if 0
                        ((yyval.val)->nd_ainfo)->forwarding = 1;
#endif
                    }
#line 16248 "ripper.c"
    break;

  case 687: /* opt_args_tail: ',' args_tail  */
#line 5520 "ripper.y"
                    {
                        (yyval.val) = (yyvsp[0].val);
                    }
#line 16256 "ripper.c"
    break;

  case 688: /* opt_args_tail: %empty  */
#line 5524 "ripper.y"
                    {
                        (yyval.val) = new_args_tail(p, Qnone, Qnone, Qnone, &(yylsp[0]));
                    }
#line 16264 "ripper.c"
    break;

  case 689: /* f_args: f_arg ',' f_optarg ',' f_rest_arg opt_args_tail  */
#line 5530 "ripper.y"
                    {
                        (yyval.val) = new_args(p, (yyvsp[-5].val), (yyvsp[-3].val), (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yyloc));
                    }
#line 16272 "ripper.c"
    break;

  case 690: /* f_args: f_arg ',' f_optarg ',' f_rest_arg ',' f_arg opt_args_tail  */
#line 5534 "ripper.y"
                    {
                        (yyval.val) = new_args(p, (yyvsp[-7].val), (yyvsp[-5].val), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
                    }
#line 16280 "ripper.c"
    break;

  case 691: /* f_args: f_arg ',' f_optarg opt_args_tail  */
#line 5538 "ripper.y"
                    {
                        (yyval.val) = new_args(p, (yyvsp[-3].val), (yyvsp[-1].val), Qnone, Qnone, (yyvsp[0].val), &(yyloc));
                    }
#line 16288 "ripper.c"
    break;

  case 692: /* f_args: f_arg ',' f_optarg ',' f_arg opt_args_tail  */
#line 5542 "ripper.y"
                    {
                        (yyval.val) = new_args(p, (yyvsp[-5].val), (yyvsp[-3].val), Qnone, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
                    }
#line 16296 "ripper.c"
    break;

  case 693: /* f_args: f_arg ',' f_rest_arg opt_args_tail  */
#line 5546 "ripper.y"
                    {
                        (yyval.val) = new_args(p, (yyvsp[-3].val), Qnone, (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yyloc));
                    }
#line 16304 "ripper.c"
    break;

  case 694: /* f_args: f_arg ',' f_rest_arg ',' f_arg opt_args_tail  */
#line 5550 "ripper.y"
                    {
                        (yyval.val) = new_args(p, (yyvsp[-5].val), Qnone, (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
                    }
#line 16312 "ripper.c"
    break;

  case 695: /* f_args: f_arg opt_args_tail  */
#line 5554 "ripper.y"
                    {
                        (yyval.val) = new_args(p, (yyvsp[-1].val), Qnone, Qnone, Qnone, (yyvsp[0].val), &(yyloc));
                    }
#line 16320 "ripper.c"
    break;

  case 696: /* f_args: f_optarg ',' f_rest_arg opt_args_tail  */
#line 5558 "ripper.y"
                    {
                        (yyval.val) = new_args(p, Qnone, (yyvsp[-3].val), (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yyloc));
                    }
#line 16328 "ripper.c"
    break;

  case 697: /* f_args: f_optarg ',' f_rest_arg ',' f_arg opt_args_tail  */
#line 5562 "ripper.y"
                    {
                        (yyval.val) = new_args(p, Qnone, (yyvsp[-5].val), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
                    }
#line 16336 "ripper.c"
    break;

  case 698: /* f_args: f_optarg opt_args_tail  */
#line 5566 "ripper.y"
                    {
                        (yyval.val) = new_args(p, Qnone, (yyvsp[-1].val), Qnone, Qnone, (yyvsp[0].val), &(yyloc));
                    }
#line 16344 "ripper.c"
    break;

  case 699: /* f_args: f_optarg ',' f_arg opt_args_tail  */
#line 5570 "ripper.y"
                    {
                        (yyval.val) = new_args(p, Qnone, (yyvsp[-3].val), Qnone, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
                    }
#line 16352 "ripper.c"
    break;

  case 700: /* f_args: f_rest_arg opt_args_tail  */
#line 5574 "ripper.y"
                    {
                        (yyval.val) = new_args(p, Qnone, Qnone, (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yyloc));
                    }
#line 16360 "ripper.c"
    break;

  case 701: /* f_args: f_rest_arg ',' f_arg opt_args_tail  */
#line 5578 "ripper.y"
                    {
                        (yyval.val) = new_args(p, Qnone, Qnone, (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
                    }
#line 16368 "ripper.c"
    break;

  case 702: /* f_args: args_tail  */
#line 5582 "ripper.y"
                    {
                        (yyval.val) = new_args(p, Qnone, Qnone, Qnone, Qnone, (yyvsp[0].val), &(yyloc));
                    }
#line 16376 "ripper.c"
    break;

  case 703: /* f_args: %empty  */
#line 5586 "ripper.y"
                    {
                        (yyval.val) = new_args_tail(p, Qnone, Qnone, Qnone, &(yylsp[0]));
                        (yyval.val) = new_args(p, Qnone, Qnone, Qnone, Qnone, (yyval.val), &(yylsp[0]));
                    }
#line 16385 "ripper.c"
    break;

  case 704: /* args_forward: "(..."  */
#line 5593 "ripper.y"
                    {
#if 0
#ifdef FORWARD_ARGS_WITH_RUBY2_KEYWORDS
                        (yyval.val) = 0;
#else
                        (yyval.val) = idFWD_KWREST;
#endif
#endif
			{VALUE v1;v1=dispatch0(args_forward);(yyval.val)=v1;}
                    }
#line 16400 "ripper.c"
    break;

  case 705: /* f_bad_arg: "constant"  */
#line 5606 "ripper.y"
                    {
                        static const char mesg[] = "formal argument cannot be a constant";
#if 0
                        yyerror1(&(yylsp[0]), mesg);
                        (yyval.val) = 0;
#endif
			{VALUE v1,v2,v3;v1=ERR_MESG();v2=(yyvsp[0].val);v3=dispatch2(param_error,v1,v2);(yyval.val)=v3;}ripper_error(p);
                    }
#line 16413 "ripper.c"
    break;

  case 706: /* f_bad_arg: "instance variable"  */
#line 5615 "ripper.y"
                    {
                        static const char mesg[] = "formal argument cannot be an instance variable";
#if 0
                        yyerror1(&(yylsp[0]), mesg);
                        (yyval.val) = 0;
#endif
			{VALUE v1,v2,v3;v1=ERR_MESG();v2=(yyvsp[0].val);v3=dispatch2(param_error,v1,v2);(yyval.val)=v3;}ripper_error(p);
                    }
#line 16426 "ripper.c"
    break;

  case 707: /* f_bad_arg: "global variable"  */
#line 5624 "ripper.y"
                    {
                        static const char mesg[] = "formal argument cannot be a global variable";
#if 0
                        yyerror1(&(yylsp[0]), mesg);
                        (yyval.val) = 0;
#endif
			{VALUE v1,v2,v3;v1=ERR_MESG();v2=(yyvsp[0].val);v3=dispatch2(param_error,v1,v2);(yyval.val)=v3;}ripper_error(p);
                    }
#line 16439 "ripper.c"
    break;

  case 708: /* f_bad_arg: "class variable"  */
#line 5633 "ripper.y"
                    {
                        static const char mesg[] = "formal argument cannot be a class variable";
#if 0
                        yyerror1(&(yylsp[0]), mesg);
                        (yyval.val) = 0;
#endif
			{VALUE v1,v2,v3;v1=ERR_MESG();v2=(yyvsp[0].val);v3=dispatch2(param_error,v1,v2);(yyval.val)=v3;}ripper_error(p);
                    }
#line 16452 "ripper.c"
    break;

  case 710: /* f_norm_arg: "local variable or method"  */
#line 5645 "ripper.y"
                    {
                        formal_argument(p, (yyvsp[0].val));
                        p->max_numparam = ORDINAL_PARAM;
                        (yyval.val) = (yyvsp[0].val);
                    }
#line 16462 "ripper.c"
    break;

  case 711: /* f_arg_asgn: f_norm_arg  */
#line 5653 "ripper.y"
                    {
                        ID id = get_id((yyvsp[0].val));
                        arg_var(p, id);
                        p->cur_arg = id;
                        (yyval.val) = (yyvsp[0].val);
                    }
#line 16473 "ripper.c"
    break;

  case 712: /* f_arg_item: f_arg_asgn  */
#line 5662 "ripper.y"
                    {
                        p->cur_arg = 0;
#if 0
                        (yyval.val) = NEW_ARGS_AUX((yyvsp[0].val), 1, &NULL_LOC);
#endif
			(yyval.val)=get_value((yyvsp[0].val));
                    }
#line 16485 "ripper.c"
    break;

  case 713: /* f_arg_item: "(" f_margs rparen  */
#line 5670 "ripper.y"
                    {
#if 0
                        ID tid = internal_id(p);
                        YYLTYPE loc;
                        loc.beg_pos = (yylsp[-1]).beg_pos;
                        loc.end_pos = (yylsp[-1]).beg_pos;
                        arg_var(p, tid);
                        if (dyna_in_block(p)) {
                            (yyvsp[-1].val)->nd_value = NEW_DVAR(tid, &loc);
                        }
                        else {
                            (yyvsp[-1].val)->nd_value = NEW_LVAR(tid, &loc);
                        }
                        (yyval.val) = NEW_ARGS_AUX(tid, 1, &NULL_LOC);
                        (yyval.val)->nd_next = (yyvsp[-1].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(mlhs_paren,v1);(yyval.val)=v2;}
                    }
#line 16508 "ripper.c"
    break;

  case 714: /* f_arg: f_arg_item  */
#line 5691 "ripper.y"
   {(yyval.val)=rb_ary_new3(1, get_value((yyvsp[0].val)));}
#line 16514 "ripper.c"
    break;

  case 715: /* f_arg: f_arg ',' f_arg_item  */
#line 5693 "ripper.y"
                    {
#if 0
                        (yyval.val) = (yyvsp[-2].val);
                        (yyval.val)->nd_plen++;
                        (yyval.val)->nd_next = block_append(p, (yyval.val)->nd_next, (yyvsp[0].val)->nd_next);
                        rb_discard_node(p, (yyvsp[0].val));
#endif
			(yyval.val)=rb_ary_push((yyvsp[-2].val), get_value((yyvsp[0].val)));
                    }
#line 16528 "ripper.c"
    break;

  case 716: /* f_label: "label"  */
#line 5706 "ripper.y"
                    {
                        arg_var(p, formal_argument(p, (yyvsp[0].val)));
                        p->cur_arg = get_id((yyvsp[0].val));
                        p->max_numparam = ORDINAL_PARAM;
                        p->ctxt.in_argdef = 0;
                        (yyval.val) = (yyvsp[0].val);
                    }
#line 16540 "ripper.c"
    break;

  case 717: /* f_kw: f_label arg_value  */
#line 5716 "ripper.y"
                    {
                        p->cur_arg = 0;
                        p->ctxt.in_argdef = 1;
#if 0
                        (yyval.val) = new_kw_arg(p, assignable(p, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			(yyval.val)=rb_assoc_new(get_value(assignable(p, (yyvsp[-1].val))), get_value((yyvsp[0].val)));
                    }
#line 16553 "ripper.c"
    break;

  case 718: /* f_kw: f_label  */
#line 5725 "ripper.y"
                    {
                        p->cur_arg = 0;
                        p->ctxt.in_argdef = 1;
#if 0
                        (yyval.val) = new_kw_arg(p, assignable(p, (yyvsp[0].val), NODE_SPECIAL_REQUIRED_KEYWORD, &(yyloc)), &(yyloc));
#endif
			(yyval.val)=rb_assoc_new(get_value(assignable(p, (yyvsp[0].val))), 0);
                    }
#line 16566 "ripper.c"
    break;

  case 719: /* f_block_kw: f_label primary_value  */
#line 5736 "ripper.y"
                    {
                        p->ctxt.in_argdef = 1;
#if 0
                        (yyval.val) = new_kw_arg(p, assignable(p, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			(yyval.val)=rb_assoc_new(get_value(assignable(p, (yyvsp[-1].val))), get_value((yyvsp[0].val)));
                    }
#line 16578 "ripper.c"
    break;

  case 720: /* f_block_kw: f_label  */
#line 5744 "ripper.y"
                    {
                        p->ctxt.in_argdef = 1;
#if 0
                        (yyval.val) = new_kw_arg(p, assignable(p, (yyvsp[0].val), NODE_SPECIAL_REQUIRED_KEYWORD, &(yyloc)), &(yyloc));
#endif
			(yyval.val)=rb_assoc_new(get_value(assignable(p, (yyvsp[0].val))), 0);
                    }
#line 16590 "ripper.c"
    break;

  case 721: /* f_block_kwarg: f_block_kw  */
#line 5754 "ripper.y"
                    {
#if 0
                        (yyval.val) = (yyvsp[0].val);
#endif
			(yyval.val)=rb_ary_new3(1, get_value((yyvsp[0].val)));
                    }
#line 16601 "ripper.c"
    break;

  case 722: /* f_block_kwarg: f_block_kwarg ',' f_block_kw  */
#line 5761 "ripper.y"
                    {
#if 0
                        (yyval.val) = kwd_append((yyvsp[-2].val), (yyvsp[0].val));
#endif
			(yyval.val)=rb_ary_push((yyvsp[-2].val), get_value((yyvsp[0].val)));
                    }
#line 16612 "ripper.c"
    break;

  case 723: /* f_kwarg: f_kw  */
#line 5771 "ripper.y"
                    {
#if 0
                        (yyval.val) = (yyvsp[0].val);
#endif
			(yyval.val)=rb_ary_new3(1, get_value((yyvsp[0].val)));
                    }
#line 16623 "ripper.c"
    break;

  case 724: /* f_kwarg: f_kwarg ',' f_kw  */
#line 5778 "ripper.y"
                    {
#if 0
                        (yyval.val) = kwd_append((yyvsp[-2].val), (yyvsp[0].val));
#endif
			(yyval.val)=rb_ary_push((yyvsp[-2].val), get_value((yyvsp[0].val)));
                    }
#line 16634 "ripper.c"
    break;

  case 727: /* f_no_kwarg: p_kwnorest  */
#line 5791 "ripper.y"
                    {
#if 0
#endif
			{VALUE v1,v2;v1=Qnil;v2=dispatch1(nokw_param,v1);(yyval.val)=v2;}
                    }
#line 16644 "ripper.c"
    break;

  case 728: /* f_kwrest: kwrest_mark "local variable or method"  */
#line 5799 "ripper.y"
                    {
                        arg_var(p, shadowing_lvar(p, get_id((yyvsp[0].val))));
#if 0
                        (yyval.val) = (yyvsp[0].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(kwrest_param,v1);(yyval.val)=v2;}
                    }
#line 16656 "ripper.c"
    break;

  case 729: /* f_kwrest: kwrest_mark  */
#line 5807 "ripper.y"
                    {
                        arg_var(p, idFWD_KWREST);
#if 0
                        (yyval.val) = idFWD_KWREST;
#endif
			{VALUE v1,v2;v1=Qnil;v2=dispatch1(kwrest_param,v1);(yyval.val)=v2;}
                    }
#line 16668 "ripper.c"
    break;

  case 730: /* f_opt: f_arg_asgn f_eq arg_value  */
#line 5817 "ripper.y"
                    {
                        p->cur_arg = 0;
                        p->ctxt.in_argdef = 1;
#if 0
                        (yyval.val) = NEW_OPT_ARG(0, assignable(p, (yyvsp[-2].val), (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			(yyval.val)=rb_assoc_new(get_value(assignable(p, (yyvsp[-2].val))), get_value((yyvsp[0].val)));
                    }
#line 16681 "ripper.c"
    break;

  case 731: /* f_block_opt: f_arg_asgn f_eq primary_value  */
#line 5828 "ripper.y"
                    {
                        p->cur_arg = 0;
                        p->ctxt.in_argdef = 1;
#if 0
                        (yyval.val) = NEW_OPT_ARG(0, assignable(p, (yyvsp[-2].val), (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			(yyval.val)=rb_assoc_new(get_value(assignable(p, (yyvsp[-2].val))), get_value((yyvsp[0].val)));
                    }
#line 16694 "ripper.c"
    break;

  case 732: /* f_block_optarg: f_block_opt  */
#line 5839 "ripper.y"
                    {
#if 0
                        (yyval.val) = (yyvsp[0].val);
#endif
			(yyval.val)=rb_ary_new3(1, get_value((yyvsp[0].val)));
                    }
#line 16705 "ripper.c"
    break;

  case 733: /* f_block_optarg: f_block_optarg ',' f_block_opt  */
#line 5846 "ripper.y"
                    {
#if 0
                        (yyval.val) = opt_arg_append((yyvsp[-2].val), (yyvsp[0].val));
#endif
			(yyval.val)=rb_ary_push((yyvsp[-2].val), get_value((yyvsp[0].val)));
                    }
#line 16716 "ripper.c"
    break;

  case 734: /* f_optarg: f_opt  */
#line 5855 "ripper.y"
                    {
#if 0
                        (yyval.val) = (yyvsp[0].val);
#endif
			(yyval.val)=rb_ary_new3(1, get_value((yyvsp[0].val)));
                    }
#line 16727 "ripper.c"
    break;

  case 735: /* f_optarg: f_optarg ',' f_opt  */
#line 5862 "ripper.y"
                    {
#if 0
                        (yyval.val) = opt_arg_append((yyvsp[-2].val), (yyvsp[0].val));
#endif
			(yyval.val)=rb_ary_push((yyvsp[-2].val), get_value((yyvsp[0].val)));
                    }
#line 16738 "ripper.c"
    break;

  case 738: /* f_rest_arg: restarg_mark "local variable or method"  */
#line 5875 "ripper.y"
                    {
                        arg_var(p, shadowing_lvar(p, get_id((yyvsp[0].val))));
#if 0
                        (yyval.val) = (yyvsp[0].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(rest_param,v1);(yyval.val)=v2;}
                    }
#line 16750 "ripper.c"
    break;

  case 739: /* f_rest_arg: restarg_mark  */
#line 5883 "ripper.y"
                    {
                        arg_var(p, idFWD_REST);
#if 0
                        (yyval.val) = idFWD_REST;
#endif
			{VALUE v1,v2;v1=Qnil;v2=dispatch1(rest_param,v1);(yyval.val)=v2;}
                    }
#line 16762 "ripper.c"
    break;

  case 742: /* f_block_arg: blkarg_mark "local variable or method"  */
#line 5897 "ripper.y"
                    {
                        arg_var(p, shadowing_lvar(p, get_id((yyvsp[0].val))));
#if 0
                        (yyval.val) = (yyvsp[0].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(blockarg,v1);(yyval.val)=v2;}
                    }
#line 16774 "ripper.c"
    break;

  case 743: /* f_block_arg: blkarg_mark  */
#line 5905 "ripper.y"
                    {
                        arg_var(p, idFWD_BLOCK);
#if 0
                        (yyval.val) = idFWD_BLOCK;
#endif
			{VALUE v1,v2;v1=Qnil;v2=dispatch1(blockarg,v1);(yyval.val)=v2;}
                    }
#line 16786 "ripper.c"
    break;

  case 744: /* opt_f_block_arg: ',' f_block_arg  */
#line 5915 "ripper.y"
                    {
                        (yyval.val) = (yyvsp[0].val);
                    }
#line 16794 "ripper.c"
    break;

  case 745: /* opt_f_block_arg: none  */
#line 5919 "ripper.y"
                    {
                        (yyval.val) = Qnull;
                    }
#line 16802 "ripper.c"
    break;

  case 746: /* singleton: var_ref  */
#line 5925 "ripper.y"
                    {
                        value_expr((yyvsp[0].val));
                        (yyval.val) = (yyvsp[0].val);
                    }
#line 16811 "ripper.c"
    break;

  case 747: /* $@51: %empty  */
#line 5929 "ripper.y"
                      {SET_LEX_STATE(EXPR_BEG);}
#line 16817 "ripper.c"
    break;

  case 748: /* singleton: '(' $@51 expr rparen  */
#line 5930 "ripper.y"
                    {
#if 0
                        switch (nd_type((yyvsp[-1].val))) {
                          case NODE_STR:
                          case NODE_DSTR:
                          case NODE_XSTR:
                          case NODE_DXSTR:
                          case NODE_DREGX:
                          case NODE_LIT:
                          case NODE_LIST:
                          case NODE_ZLIST:
                            yyerror1(&(yylsp[-1]), "can't define singleton method for literals");
                            break;
                          default:
                            value_expr((yyvsp[-1].val));
                            break;
                        }
                        (yyval.val) = (yyvsp[-1].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(paren,v1);(yyval.val)=v2;}
                    }
#line 16843 "ripper.c"
    break;

  case 750: /* assoc_list: assocs trailer  */
#line 5955 "ripper.y"
                    {
#if 0
                        (yyval.val) = (yyvsp[-1].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(assoclist_from_args,v1);(yyval.val)=v2;}
                    }
#line 16854 "ripper.c"
    break;

  case 751: /* assocs: assoc  */
#line 5964 "ripper.y"
   {(yyval.val)=rb_ary_new3(1, get_value((yyvsp[0].val)));}
#line 16860 "ripper.c"
    break;

  case 752: /* assocs: assocs ',' assoc  */
#line 5966 "ripper.y"
                    {
#if 0
                        NODE *assocs = (yyvsp[-2].val);
                        NODE *tail = (yyvsp[0].val);
                        if (!assocs) {
                            assocs = tail;
                        }
                        else if (tail) {
                            if (assocs->nd_head &&
                                !tail->nd_head && nd_type_p(tail->nd_next, NODE_LIST) &&
                                nd_type_p(tail->nd_next->nd_head, NODE_HASH)) {
                                /* DSTAR */
                                tail = tail->nd_next->nd_head->nd_head;
                            }
                            assocs = list_concat(assocs, tail);
                        }
                        (yyval.val) = assocs;
#endif
			(yyval.val)=rb_ary_push((yyvsp[-2].val), get_value((yyvsp[0].val)));
                    }
#line 16885 "ripper.c"
    break;

  case 753: /* assoc: arg_value "=>" arg_value  */
#line 5989 "ripper.y"
                    {
#if 0
                        if (nd_type_p((yyvsp[-2].val), NODE_STR)) {
                            nd_set_type((yyvsp[-2].val), NODE_LIT);
                            RB_OBJ_WRITE(p->ast, &(yyvsp[-2].val)->nd_lit, rb_fstring((yyvsp[-2].val)->nd_lit));
                        }
                        (yyval.val) = list_append(p, NEW_LIST((yyvsp[-2].val), &(yyloc)), (yyvsp[0].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(assoc_new,v1,v2);(yyval.val)=v3;}
                    }
#line 16900 "ripper.c"
    break;

  case 754: /* assoc: "label" arg_value  */
#line 6000 "ripper.y"
                    {
#if 0
                        (yyval.val) = list_append(p, NEW_LIST(NEW_LIT(ID2SYM((yyvsp[-1].val)), &(yylsp[-1])), &(yyloc)), (yyvsp[0].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=(yyvsp[0].val);v3=dispatch2(assoc_new,v1,v2);(yyval.val)=v3;}
                    }
#line 16911 "ripper.c"
    break;

  case 755: /* assoc: "label"  */
#line 6007 "ripper.y"
                    {
#if 0
                        NODE *val = gettable(p, (yyvsp[0].val), &(yyloc));
                        if (!val) val = NEW_BEGIN(0, &(yyloc));
                        (yyval.val) = list_append(p, NEW_LIST(NEW_LIT(ID2SYM((yyvsp[0].val)), &(yylsp[0])), &(yyloc)), val);
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[0].val);v2=Qnil;v3=dispatch2(assoc_new,v1,v2);(yyval.val)=v3;}
                    }
#line 16924 "ripper.c"
    break;

  case 756: /* assoc: "string literal" string_contents tLABEL_END arg_value  */
#line 6016 "ripper.y"
                    {
#if 0
                        YYLTYPE loc = code_loc_gen(&(yylsp[-3]), &(yylsp[-1]));
                        (yyval.val) = list_append(p, NEW_LIST(dsym_node(p, (yyvsp[-2].val), &loc), &loc), (yyvsp[0].val));
#endif
			{VALUE v1,v2,v3,v4,v5;v1=(yyvsp[-2].val);v2=dispatch1(dyna_symbol,v1);v3=v2;v4=(yyvsp[0].val);v5=dispatch2(assoc_new,v3,v4);(yyval.val)=v5;}
                    }
#line 16936 "ripper.c"
    break;

  case 757: /* assoc: "**arg" arg_value  */
#line 6024 "ripper.y"
                    {
#if 0
                        if (nd_type_p((yyvsp[0].val), NODE_HASH) &&
                            !((yyvsp[0].val)->nd_head && (yyvsp[0].val)->nd_head->nd_alen)) {
                            static VALUE empty_hash;
                            if (!empty_hash) {
                                empty_hash = rb_obj_freeze(rb_hash_new());
                                rb_gc_register_mark_object(empty_hash);
                            }
                            (yyval.val) = list_append(p, NEW_LIST(0, &(yyloc)), NEW_LIT(empty_hash, &(yyloc)));
                        }
                        else
                            (yyval.val) = list_append(p, NEW_LIST(0, &(yyloc)), (yyvsp[0].val));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(assoc_splat,v1);(yyval.val)=v2;}
                    }
#line 16957 "ripper.c"
    break;

  case 758: /* assoc: "**arg"  */
#line 6041 "ripper.y"
                    {
                        if (!local_id(p, idFWD_KWREST) ||
                            local_id(p, idFWD_ALL)) {
                            compile_error(p, "no anonymous keyword rest parameter");
                        }
#if 0
                        (yyval.val) = list_append(p, NEW_LIST(0, &(yyloc)),
                                         NEW_LVAR(idFWD_KWREST, &(yyloc)));
#endif
			{VALUE v1,v2;v1=Qnil;v2=dispatch1(assoc_splat,v1);(yyval.val)=v2;}
                    }
#line 16973 "ripper.c"
    break;

  case 782: /* term: ';'  */
#line 6101 "ripper.y"
            {yyerrok;token_flush(p);}
#line 16979 "ripper.c"
    break;

  case 783: /* term: '\n'  */
#line 6103 "ripper.y"
                    {
                        (yyloc).end_pos = (yyloc).beg_pos;
                        token_flush(p);
                    }
#line 16988 "ripper.c"
    break;

  case 785: /* terms: terms ';'  */
#line 6110 "ripper.y"
                            {yyerrok;}
#line 16994 "ripper.c"
    break;

  case 786: /* none: %empty  */
#line 6114 "ripper.y"
                    {
                        (yyval.val) = Qnull;
                    }
#line 17002 "ripper.c"
    break;


#line 17006 "ripper.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      {
        yypcontext_t yyctx
          = {yyssp, yytoken, &yylloc};
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = yysyntax_error (p, &yymsg_alloc, &yymsg, &yyctx);
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == -1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *,
                             YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (yymsg)
              {
                yysyntax_error_status
                  = yysyntax_error (p, &yymsg_alloc, &yymsg, &yyctx);
                yymsgp = yymsg;
              }
            else
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = YYENOMEM;
              }
          }
        yyerror (&yylloc, p, yymsgp);
        if (yysyntax_error_status == YYENOMEM)
          YYNOMEM;
      }
    }

  yyerror_range[1] = yylloc;
  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= END_OF_INPUT)
        {
          /* Return failure if at end of input.  */
          if (yychar == END_OF_INPUT)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, &yylloc, p);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp, yylsp, p);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  ++yylsp;
  YYLLOC_DEFAULT (*yylsp, yyerror_range, 2);

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (&yylloc, p, YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc, p);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp, yylsp, p);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
  return yyresult;
}

#line 6118 "ripper.y"

# undef p
# undef yylex
# undef yylval
# define yylval  (*p->lval)

static int regx_options(struct parser_params*);
static int tokadd_string(struct parser_params*,int,int,int,long*,rb_encoding**,rb_encoding**);
static void tokaddmbc(struct parser_params *p, int c, rb_encoding *enc);
static enum yytokentype parse_string(struct parser_params*,rb_strterm_literal_t*);
static enum yytokentype here_document(struct parser_params*,rb_strterm_heredoc_t*);

#ifndef RIPPER
# define set_yylval_node(x) {				\
  YYLTYPE _cur_loc;					\
  rb_parser_set_location(p, &_cur_loc);			\
  yylval.node = (x);					\
}
# define set_yylval_str(x) \
do { \
  set_yylval_node(NEW_STR(x, &_cur_loc)); \
  RB_OBJ_WRITTEN(p->ast, Qnil, x); \
} while(0)
# define set_yylval_literal(x) \
do { \
  set_yylval_node(NEW_LIT(x, &_cur_loc)); \
  RB_OBJ_WRITTEN(p->ast, Qnil, x); \
} while(0)
# define set_yylval_num(x) (yylval.num = (x))
# define set_yylval_id(x)  (yylval.id = (x))
# define set_yylval_name(x)  (yylval.id = (x))
# define yylval_id() (yylval.id)
#else
static inline VALUE
ripper_yylval_id(struct parser_params *p, ID x)
{
    return ripper_new_yylval(p, x, ID2SYM(x), 0);
}
# define set_yylval_str(x) (yylval.val = add_mark_object(p, (x)))
# define set_yylval_num(x) (yylval.val = ripper_new_yylval(p, (x), 0, 0))
# define set_yylval_id(x)  (void)(x)
# define set_yylval_name(x) (void)(yylval.val = ripper_yylval_id(p, x))
# define set_yylval_literal(x) add_mark_object(p, (x))
# define set_yylval_node(x) (yylval.val = ripper_new_yylval(p, 0, 0, STR_NEW(p->lex.ptok, p->lex.pcur-p->lex.ptok)))
# define yylval_id() yylval.id
# define _cur_loc NULL_LOC /* dummy */
#endif

#define set_yylval_noname() set_yylval_id(keyword_nil)
#define has_delayed_token(p) (!NIL_P(p->delayed.token))

#ifndef RIPPER
#define literal_flush(p, ptr) ((p)->lex.ptok = (ptr))
#define dispatch_scan_event(p, t) parser_dispatch_scan_event(p, t, __LINE__)

static bool
parser_has_token(struct parser_params *p)
{
    const char *const pcur = p->lex.pcur;
    const char *const ptok = p->lex.ptok;
    if (p->keep_tokens && (pcur < ptok)) {
        rb_bug("lex.pcur < lex.ptok. (line: %d) %"PRIdPTRDIFF"|%"PRIdPTRDIFF"|%"PRIdPTRDIFF"",
               p->ruby_sourceline, ptok - p->lex.pbeg, pcur - ptok, p->lex.pend - pcur);
    }
    return pcur > ptok;
}

static VALUE
code_loc_to_ary(const rb_code_location_t *loc)
{
    VALUE ary = rb_ary_new_from_args(4,
        INT2NUM(loc->beg_pos.lineno), INT2NUM(loc->beg_pos.column),
        INT2NUM(loc->end_pos.lineno), INT2NUM(loc->end_pos.column));
    rb_obj_freeze(ary);

    return ary;
}

static void
parser_append_tokens(struct parser_params *p, VALUE str, enum yytokentype t, int line)
{
    VALUE ary;
    int token_id;

    ary = rb_ary_new2(4);
    token_id = p->token_id;
    rb_ary_push(ary, INT2FIX(token_id));
    rb_ary_push(ary, ID2SYM(parser_token2id(t)));
    rb_ary_push(ary, str);
    rb_ary_push(ary, code_loc_to_ary(p->yylloc));
    rb_obj_freeze(ary);
    rb_ary_push(p->tokens, ary);
    p->token_id++;

    if (p->debug) {
        rb_parser_printf(p, "Append tokens (line: %d) %"PRIsVALUE"\n", line, ary);
    }
}

static void
parser_dispatch_scan_event(struct parser_params *p, enum yytokentype t, int line)
{
    debug_token_line(p, "parser_dispatch_scan_event", line);

    if (!parser_has_token(p)) return;

    RUBY_SET_YYLLOC(*p->yylloc);

    if (p->keep_tokens) {
        VALUE str = STR_NEW(p->lex.ptok, p->lex.pcur - p->lex.ptok);
        parser_append_tokens(p, str, t, line);
    }

    token_flush(p);
}

#define dispatch_delayed_token(p, t) parser_dispatch_delayed_token(p, t, __LINE__)
static void
parser_dispatch_delayed_token(struct parser_params *p, enum yytokentype t, int line)
{
    int saved_line = p->ruby_sourceline;
    const char *saved_tokp = p->lex.ptok;

    debug_token_line(p, "parser_dispatch_delayed_token", line);

    if (!has_delayed_token(p)) return;

    RUBY_SET_YYLLOC_OF_DELAYED_TOKEN(*p->yylloc);

    if (p->keep_tokens) {
        p->ruby_sourceline = p->delayed.beg_line;
        p->lex.ptok = p->lex.pbeg + p->delayed.beg_col;
        parser_append_tokens(p, p->delayed.token, t, line);
        p->ruby_sourceline = saved_line;
        p->lex.ptok = saved_tokp;
    }

    p->delayed.token = Qnil;
}
#else
#define literal_flush(p, ptr) ((void)(ptr))

#define yylval_rval (*(RB_TYPE_P(yylval.val, T_NODE) ? &yylval.node->nd_rval : &yylval.val))

static inline VALUE
intern_sym(const char *name)
{
    ID id = rb_intern_const(name);
    return ID2SYM(id);
}

static int
ripper_has_scan_event(struct parser_params *p)
{
    if (p->lex.pcur < p->lex.ptok) rb_raise(rb_eRuntimeError, "lex.pcur < lex.ptok");
    return p->lex.pcur > p->lex.ptok;
}

static VALUE
ripper_scan_event_val(struct parser_params *p, enum yytokentype t)
{
    VALUE str = STR_NEW(p->lex.ptok, p->lex.pcur - p->lex.ptok);
    VALUE rval = ripper_dispatch1(p, ripper_token2eventid(t), str);
    RUBY_SET_YYLLOC(*p->yylloc);
    token_flush(p);
    return rval;
}

static void
ripper_dispatch_scan_event(struct parser_params *p, enum yytokentype t)
{
    if (!ripper_has_scan_event(p)) return;
    add_mark_object(p, yylval_rval = ripper_scan_event_val(p, t));
}
#define dispatch_scan_event(p, t) ripper_dispatch_scan_event(p, t)

static void
ripper_dispatch_delayed_token(struct parser_params *p, enum yytokentype t)
{
    int saved_line = p->ruby_sourceline;
    const char *saved_tokp = p->lex.ptok;

    if (!has_delayed_token(p)) return;
    p->ruby_sourceline = p->delayed.beg_line;
    p->lex.ptok = p->lex.pbeg + p->delayed.beg_col;
    add_mark_object(p, yylval_rval = ripper_dispatch1(p, ripper_token2eventid(t), p->delayed.token));
    p->delayed.token = Qnil;
    p->ruby_sourceline = saved_line;
    p->lex.ptok = saved_tokp;
}
#define dispatch_delayed_token(p, t) ripper_dispatch_delayed_token(p, t)
#endif /* RIPPER */

static inline int
is_identchar(const char *ptr, const char *MAYBE_UNUSED(ptr_end), rb_encoding *enc)
{
    return rb_enc_isalnum((unsigned char)*ptr, enc) || *ptr == '_' || !ISASCII(*ptr);
}

static inline int
parser_is_identchar(struct parser_params *p)
{
    return !(p)->eofp && is_identchar(p->lex.pcur-1, p->lex.pend, p->enc);
}

static inline int
parser_isascii(struct parser_params *p)
{
    return ISASCII(*(p->lex.pcur-1));
}

static void
token_info_setup(token_info *ptinfo, const char *ptr, const rb_code_location_t *loc)
{
    int column = 1, nonspc = 0, i;
    for (i = 0; i < loc->beg_pos.column; i++, ptr++) {
        if (*ptr == '\t') {
            column = (((column - 1) / TAB_WIDTH) + 1) * TAB_WIDTH;
        }
        column++;
        if (*ptr != ' ' && *ptr != '\t') {
            nonspc = 1;
        }
    }

    ptinfo->beg = loc->beg_pos;
    ptinfo->indent = column;
    ptinfo->nonspc = nonspc;
}

static void
token_info_push(struct parser_params *p, const char *token, const rb_code_location_t *loc)
{
    token_info *ptinfo;

    if (!p->token_info_enabled) return;
    ptinfo = ALLOC(token_info);
    ptinfo->token = token;
    ptinfo->next = p->token_info;
    token_info_setup(ptinfo, p->lex.pbeg, loc);

    p->token_info = ptinfo;
}

static void
token_info_pop(struct parser_params *p, const char *token, const rb_code_location_t *loc)
{
    token_info *ptinfo_beg = p->token_info;

    if (!ptinfo_beg) return;
    p->token_info = ptinfo_beg->next;

    /* indentation check of matched keywords (begin..end, if..end, etc.) */
    token_info_warn(p, token, ptinfo_beg, 1, loc);
    ruby_sized_xfree(ptinfo_beg, sizeof(*ptinfo_beg));
}

static void
token_info_drop(struct parser_params *p, const char *token, rb_code_position_t beg_pos)
{
    token_info *ptinfo_beg = p->token_info;

    if (!ptinfo_beg) return;
    p->token_info = ptinfo_beg->next;

    if (ptinfo_beg->beg.lineno != beg_pos.lineno ||
        ptinfo_beg->beg.column != beg_pos.column ||
        strcmp(ptinfo_beg->token, token)) {
        compile_error(p, "token position mismatch: %d:%d:%s expected but %d:%d:%s",
                      beg_pos.lineno, beg_pos.column, token,
                      ptinfo_beg->beg.lineno, ptinfo_beg->beg.column,
                      ptinfo_beg->token);
    }

    ruby_sized_xfree(ptinfo_beg, sizeof(*ptinfo_beg));
}

static void
token_info_warn(struct parser_params *p, const char *token, token_info *ptinfo_beg, int same, const rb_code_location_t *loc)
{
    token_info ptinfo_end_body, *ptinfo_end = &ptinfo_end_body;
    if (!p->token_info_enabled) return;
    if (!ptinfo_beg) return;
    token_info_setup(ptinfo_end, p->lex.pbeg, loc);
    if (ptinfo_beg->beg.lineno == ptinfo_end->beg.lineno) return; /* ignore one-line block */
    if (ptinfo_beg->nonspc || ptinfo_end->nonspc) return; /* ignore keyword in the middle of a line */
    if (ptinfo_beg->indent == ptinfo_end->indent) return; /* the indents are matched */
    if (!same && ptinfo_beg->indent < ptinfo_end->indent) return;
    rb_warn3L(ptinfo_end->beg.lineno,
              "mismatched indentations at '%s' with '%s' at %d",
              WARN_S(token), WARN_S(ptinfo_beg->token), WARN_I(ptinfo_beg->beg.lineno));
}

static int
parser_precise_mbclen(struct parser_params *p, const char *ptr)
{
    int len = rb_enc_precise_mbclen(ptr, p->lex.pend, p->enc);
    if (!MBCLEN_CHARFOUND_P(len)) {
        compile_error(p, "invalid multibyte char (%s)", rb_enc_name(p->enc));
        return -1;
    }
    return len;
}

#ifndef RIPPER
static void ruby_show_error_line(VALUE errbuf, const YYLTYPE *yylloc, int lineno, VALUE str);

static inline void
parser_show_error_line(struct parser_params *p, const YYLTYPE *yylloc)
{
    VALUE str;
    int lineno = p->ruby_sourceline;
    if (!yylloc) {
        return;
    }
    else if (yylloc->beg_pos.lineno == lineno) {
        str = p->lex.lastline;
    }
    else {
        return;
    }
    ruby_show_error_line(p->error_buffer, yylloc, lineno, str);
}

static int
parser_yyerror(struct parser_params *p, const YYLTYPE *yylloc, const char *msg)
{
#if 0
    YYLTYPE current;

    if (!yylloc) {
        yylloc = RUBY_SET_YYLLOC(current);
    }
    else if ((p->ruby_sourceline != yylloc->beg_pos.lineno &&
              p->ruby_sourceline != yylloc->end_pos.lineno)) {
        yylloc = 0;
    }
#endif
    compile_error(p, "%s", msg);
    parser_show_error_line(p, yylloc);
    return 0;
}

static int
parser_yyerror0(struct parser_params *p, const char *msg)
{
    YYLTYPE current;
    return parser_yyerror(p, RUBY_SET_YYLLOC(current), msg);
}

static void
ruby_show_error_line(VALUE errbuf, const YYLTYPE *yylloc, int lineno, VALUE str)
{
    VALUE mesg;
    const int max_line_margin = 30;
    const char *ptr, *ptr_end, *pt, *pb;
    const char *pre = "", *post = "", *pend;
    const char *code = "", *caret = "";
    const char *lim;
    const char *const pbeg = RSTRING_PTR(str);
    char *buf;
    long len;
    int i;

    if (!yylloc) return;
    pend = RSTRING_END(str);
    if (pend > pbeg && pend[-1] == '\n') {
        if (--pend > pbeg && pend[-1] == '\r') --pend;
    }

    pt = pend;
    if (lineno == yylloc->end_pos.lineno &&
        (pend - pbeg) > yylloc->end_pos.column) {
        pt = pbeg + yylloc->end_pos.column;
    }

    ptr = ptr_end = pt;
    lim = ptr - pbeg > max_line_margin ? ptr - max_line_margin : pbeg;
    while ((lim < ptr) && (*(ptr-1) != '\n')) ptr--;

    lim = pend - ptr_end > max_line_margin ? ptr_end + max_line_margin : pend;
    while ((ptr_end < lim) && (*ptr_end != '\n') && (*ptr_end != '\r')) ptr_end++;

    len = ptr_end - ptr;
    if (len > 4) {
        if (ptr > pbeg) {
            ptr = rb_enc_prev_char(pbeg, ptr, pt, rb_enc_get(str));
            if (ptr > pbeg) pre = "...";
        }
        if (ptr_end < pend) {
            ptr_end = rb_enc_prev_char(pt, ptr_end, pend, rb_enc_get(str));
            if (ptr_end < pend) post = "...";
        }
    }
    pb = pbeg;
    if (lineno == yylloc->beg_pos.lineno) {
        pb += yylloc->beg_pos.column;
        if (pb > pt) pb = pt;
    }
    if (pb < ptr) pb = ptr;
    if (len <= 4 && yylloc->beg_pos.lineno == yylloc->end_pos.lineno) {
        return;
    }
    if (RTEST(errbuf)) {
        mesg = rb_attr_get(errbuf, idMesg);
        if (RSTRING_LEN(mesg) > 0 && *(RSTRING_END(mesg)-1) != '\n')
            rb_str_cat_cstr(mesg, "\n");
    }
    else {
        mesg = rb_enc_str_new(0, 0, rb_enc_get(str));
    }
    if (!errbuf && rb_stderr_tty_p()) {
#define CSI_BEGIN "\033["
#define CSI_SGR "m"
        rb_str_catf(mesg,
                    CSI_BEGIN""CSI_SGR"%s" /* pre */
                    CSI_BEGIN"1"CSI_SGR"%.*s"
                    CSI_BEGIN"1;4"CSI_SGR"%.*s"
                    CSI_BEGIN";1"CSI_SGR"%.*s"
                    CSI_BEGIN""CSI_SGR"%s" /* post */
                    "\n",
                    pre,
                    (int)(pb - ptr), ptr,
                    (int)(pt - pb), pb,
                    (int)(ptr_end - pt), pt,
                    post);
    }
    else {
        char *p2;

        len = ptr_end - ptr;
        lim = pt < pend ? pt : pend;
        i = (int)(lim - ptr);
        buf = ALLOCA_N(char, i+2);
        code = ptr;
        caret = p2 = buf;
        if (ptr <= pb) {
            while (ptr < pb) {
                *p2++ = *ptr++ == '\t' ? '\t' : ' ';
            }
            *p2++ = '^';
            ptr++;
        }
        if (lim > ptr) {
            memset(p2, '~', (lim - ptr));
            p2 += (lim - ptr);
        }
        *p2 = '\0';
        rb_str_catf(mesg, "%s%.*s%s\n""%s%s\n",
                    pre, (int)len, code, post,
                    pre, caret);
    }
    if (!errbuf) rb_write_error_str(mesg);
}
#else
static int
parser_yyerror(struct parser_params *p, const YYLTYPE *yylloc, const char *msg)
{
    const char *pcur = 0, *ptok = 0;
    if (p->ruby_sourceline == yylloc->beg_pos.lineno &&
        p->ruby_sourceline == yylloc->end_pos.lineno) {
        pcur = p->lex.pcur;
        ptok = p->lex.ptok;
        p->lex.ptok = p->lex.pbeg + yylloc->beg_pos.column;
        p->lex.pcur = p->lex.pbeg + yylloc->end_pos.column;
    }
    parser_yyerror0(p, msg);
    if (pcur) {
        p->lex.ptok = ptok;
        p->lex.pcur = pcur;
    }
    return 0;
}

static int
parser_yyerror0(struct parser_params *p, const char *msg)
{
    dispatch1(parse_error, STR_NEW2(msg));
    ripper_error(p);
    return 0;
}

static inline void
parser_show_error_line(struct parser_params *p, const YYLTYPE *yylloc)
{
}
#endif /* !RIPPER */

#ifndef RIPPER
static int
vtable_size(const struct vtable *tbl)
{
    if (!DVARS_TERMINAL_P(tbl)) {
        return tbl->pos;
    }
    else {
        return 0;
    }
}
#endif

static struct vtable *
vtable_alloc_gen(struct parser_params *p, int line, struct vtable *prev)
{
    struct vtable *tbl = ALLOC(struct vtable);
    tbl->pos = 0;
    tbl->capa = 8;
    tbl->tbl = ALLOC_N(ID, tbl->capa);
    tbl->prev = prev;
#ifndef RIPPER
    if (p->debug) {
        rb_parser_printf(p, "vtable_alloc:%d: %p\n", line, (void *)tbl);
    }
#endif
    return tbl;
}
#define vtable_alloc(prev) vtable_alloc_gen(p, __LINE__, prev)

static void
vtable_free_gen(struct parser_params *p, int line, const char *name,
                struct vtable *tbl)
{
#ifndef RIPPER
    if (p->debug) {
        rb_parser_printf(p, "vtable_free:%d: %s(%p)\n", line, name, (void *)tbl);
    }
#endif
    if (!DVARS_TERMINAL_P(tbl)) {
        if (tbl->tbl) {
            ruby_sized_xfree(tbl->tbl, tbl->capa * sizeof(ID));
        }
        ruby_sized_xfree(tbl, sizeof(*tbl));
    }
}
#define vtable_free(tbl) vtable_free_gen(p, __LINE__, #tbl, tbl)

static void
vtable_add_gen(struct parser_params *p, int line, const char *name,
               struct vtable *tbl, ID id)
{
#ifndef RIPPER
    if (p->debug) {
        rb_parser_printf(p, "vtable_add:%d: %s(%p), %s\n",
                         line, name, (void *)tbl, rb_id2name(id));
    }
#endif
    if (DVARS_TERMINAL_P(tbl)) {
        rb_parser_fatal(p, "vtable_add: vtable is not allocated (%p)", (void *)tbl);
        return;
    }
    if (tbl->pos == tbl->capa) {
        tbl->capa = tbl->capa * 2;
        SIZED_REALLOC_N(tbl->tbl, ID, tbl->capa, tbl->pos);
    }
    tbl->tbl[tbl->pos++] = id;
}
#define vtable_add(tbl, id) vtable_add_gen(p, __LINE__, #tbl, tbl, id)

#ifndef RIPPER
static void
vtable_pop_gen(struct parser_params *p, int line, const char *name,
               struct vtable *tbl, int n)
{
    if (p->debug) {
        rb_parser_printf(p, "vtable_pop:%d: %s(%p), %d\n",
                         line, name, (void *)tbl, n);
    }
    if (tbl->pos < n) {
        rb_parser_fatal(p, "vtable_pop: unreachable (%d < %d)", tbl->pos, n);
        return;
    }
    tbl->pos -= n;
}
#define vtable_pop(tbl, n) vtable_pop_gen(p, __LINE__, #tbl, tbl, n)
#endif

static int
vtable_included(const struct vtable * tbl, ID id)
{
    int i;

    if (!DVARS_TERMINAL_P(tbl)) {
        for (i = 0; i < tbl->pos; i++) {
            if (tbl->tbl[i] == id) {
                return i+1;
            }
        }
    }
    return 0;
}

static void parser_prepare(struct parser_params *p);

#ifndef RIPPER
static NODE *parser_append_options(struct parser_params *p, NODE *node);

static VALUE
debug_lines(VALUE fname)
{
    ID script_lines;
    CONST_ID(script_lines, "SCRIPT_LINES__");
    if (rb_const_defined_at(rb_cObject, script_lines)) {
        VALUE hash = rb_const_get_at(rb_cObject, script_lines);
        if (RB_TYPE_P(hash, T_HASH)) {
            VALUE lines = rb_ary_new();
            rb_hash_aset(hash, fname, lines);
            return lines;
        }
    }
    return 0;
}

static int
e_option_supplied(struct parser_params *p)
{
    return strcmp(p->ruby_sourcefile, "-e") == 0;
}

static VALUE
yycompile0(VALUE arg)
{
    int n;
    NODE *tree;
    struct parser_params *p = (struct parser_params *)arg;
    VALUE cov = Qfalse;

    if (!compile_for_eval && !NIL_P(p->ruby_sourcefile_string)) {
        p->debug_lines = debug_lines(p->ruby_sourcefile_string);
        if (p->debug_lines && p->ruby_sourceline > 0) {
            VALUE str = rb_default_rs;
            n = p->ruby_sourceline;
            do {
                rb_ary_push(p->debug_lines, str);
            } while (--n);
        }

        if (!e_option_supplied(p)) {
            cov = Qtrue;
        }
    }

    if (p->keep_script_lines || ruby_vm_keep_script_lines) {
        if (!p->debug_lines) {
            p->debug_lines = rb_ary_new();
        }

        RB_OBJ_WRITE(p->ast, &p->ast->body.script_lines, p->debug_lines);
    }

    parser_prepare(p);
#define RUBY_DTRACE_PARSE_HOOK(name) \
    if (RUBY_DTRACE_PARSE_##name##_ENABLED()) { \
        RUBY_DTRACE_PARSE_##name(p->ruby_sourcefile, p->ruby_sourceline); \
    }
    RUBY_DTRACE_PARSE_HOOK(BEGIN);
    n = yyparse(p);
    RUBY_DTRACE_PARSE_HOOK(END);
    p->debug_lines = 0;

    p->lex.strterm = 0;
    p->lex.pcur = p->lex.pbeg = p->lex.pend = 0;
    if (n || p->error_p) {
        VALUE mesg = p->error_buffer;
        if (!mesg) {
            mesg = rb_class_new_instance(0, 0, rb_eSyntaxError);
        }
        if (!p->error_tolerant) {
            rb_set_errinfo(mesg);
            return FALSE;
        }
    }
    tree = p->eval_tree;
    if (!tree) {
        tree = NEW_NIL(&NULL_LOC);
    }
    else {
        VALUE opt = p->compile_option;
        VALUE tokens = p->tokens;
        NODE *prelude;
        NODE *body = parser_append_options(p, tree->nd_body);
        if (!opt) opt = rb_obj_hide(rb_ident_hash_new());
        rb_hash_aset(opt, rb_sym_intern_ascii_cstr("coverage_enabled"), cov);
        prelude = block_append(p, p->eval_tree_begin, body);
        tree->nd_body = prelude;
        RB_OBJ_WRITE(p->ast, &p->ast->body.compile_option, opt);
        if (p->keep_tokens) {
            rb_obj_freeze(tokens);
            rb_ast_set_tokens(p->ast, tokens);
        }
    }
    p->ast->body.root = tree;
    if (!p->ast->body.script_lines) p->ast->body.script_lines = INT2FIX(p->line_count);
    return TRUE;
}

static rb_ast_t *
yycompile(VALUE vparser, struct parser_params *p, VALUE fname, int line)
{
    rb_ast_t *ast;
    if (NIL_P(fname)) {
        p->ruby_sourcefile_string = Qnil;
        p->ruby_sourcefile = "(none)";
    }
    else {
        p->ruby_sourcefile_string = rb_fstring(fname);
        p->ruby_sourcefile = StringValueCStr(fname);
    }
    p->ruby_sourceline = line - 1;

    p->lvtbl = NULL;

    p->ast = ast = rb_ast_new();
    rb_suppress_tracing(yycompile0, (VALUE)p);
    p->ast = 0;
    RB_GC_GUARD(vparser); /* prohibit tail call optimization */

    while (p->lvtbl) {
        local_pop(p);
    }

    return ast;
}
#endif /* !RIPPER */

static rb_encoding *
must_be_ascii_compatible(VALUE s)
{
    rb_encoding *enc = rb_enc_get(s);
    if (!rb_enc_asciicompat(enc)) {
        rb_raise(rb_eArgError, "invalid source encoding");
    }
    return enc;
}

static VALUE
lex_get_str(struct parser_params *p, VALUE s)
{
    char *beg, *end, *start;
    long len;

    beg = RSTRING_PTR(s);
    len = RSTRING_LEN(s);
    start = beg;
    if (p->lex.gets_.ptr) {
        if (len == p->lex.gets_.ptr) return Qnil;
        beg += p->lex.gets_.ptr;
        len -= p->lex.gets_.ptr;
    }
    end = memchr(beg, '\n', len);
    if (end) len = ++end - beg;
    p->lex.gets_.ptr += len;
    return rb_str_subseq(s, beg - start, len);
}

static VALUE
lex_getline(struct parser_params *p)
{
    VALUE line = (*p->lex.gets)(p, p->lex.input);
    if (NIL_P(line)) return line;
    must_be_ascii_compatible(line);
    if (RB_OBJ_FROZEN(line)) line = rb_str_dup(line); // needed for RubyVM::AST.of because script_lines in iseq is deep-frozen
    p->line_count++;
    return line;
}

static const rb_data_type_t parser_data_type;

#ifndef RIPPER
static rb_ast_t*
parser_compile_string(VALUE vparser, VALUE fname, VALUE s, int line)
{
    struct parser_params *p;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, p);

    p->lex.gets = lex_get_str;
    p->lex.gets_.ptr = 0;
    p->lex.input = rb_str_new_frozen(s);
    p->lex.pbeg = p->lex.pcur = p->lex.pend = 0;

    return yycompile(vparser, p, fname, line);
}

rb_ast_t*
rb_parser_compile_string(VALUE vparser, const char *f, VALUE s, int line)
{
    return rb_parser_compile_string_path(vparser, rb_filesystem_str_new_cstr(f), s, line);
}

rb_ast_t*
rb_parser_compile_string_path(VALUE vparser, VALUE f, VALUE s, int line)
{
    must_be_ascii_compatible(s);
    return parser_compile_string(vparser, f, s, line);
}

VALUE rb_io_gets_internal(VALUE io);

static VALUE
lex_io_gets(struct parser_params *p, VALUE io)
{
    return rb_io_gets_internal(io);
}

rb_ast_t*
rb_parser_compile_file_path(VALUE vparser, VALUE fname, VALUE file, int start)
{
    struct parser_params *p;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, p);

    p->lex.gets = lex_io_gets;
    p->lex.input = file;
    p->lex.pbeg = p->lex.pcur = p->lex.pend = 0;

    return yycompile(vparser, p, fname, start);
}

static VALUE
lex_generic_gets(struct parser_params *p, VALUE input)
{
    return (*p->lex.gets_.call)(input, p->line_count);
}

rb_ast_t*
rb_parser_compile_generic(VALUE vparser, VALUE (*lex_gets)(VALUE, int), VALUE fname, VALUE input, int start)
{
    struct parser_params *p;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, p);

    p->lex.gets = lex_generic_gets;
    p->lex.gets_.call = lex_gets;
    p->lex.input = input;
    p->lex.pbeg = p->lex.pcur = p->lex.pend = 0;

    return yycompile(vparser, p, fname, start);
}
#endif  /* !RIPPER */

#define STR_FUNC_ESCAPE 0x01
#define STR_FUNC_EXPAND 0x02
#define STR_FUNC_REGEXP 0x04
#define STR_FUNC_QWORDS 0x08
#define STR_FUNC_SYMBOL 0x10
#define STR_FUNC_INDENT 0x20
#define STR_FUNC_LABEL  0x40
#define STR_FUNC_LIST   0x4000
#define STR_FUNC_TERM   0x8000

enum string_type {
    str_label  = STR_FUNC_LABEL,
    str_squote = (0),
    str_dquote = (STR_FUNC_EXPAND),
    str_xquote = (STR_FUNC_EXPAND),
    str_regexp = (STR_FUNC_REGEXP|STR_FUNC_ESCAPE|STR_FUNC_EXPAND),
    str_sword  = (STR_FUNC_QWORDS|STR_FUNC_LIST),
    str_dword  = (STR_FUNC_QWORDS|STR_FUNC_EXPAND|STR_FUNC_LIST),
    str_ssym   = (STR_FUNC_SYMBOL),
    str_dsym   = (STR_FUNC_SYMBOL|STR_FUNC_EXPAND)
};

static VALUE
parser_str_new(const char *ptr, long len, rb_encoding *enc, int func, rb_encoding *enc0)
{
    VALUE str;

    str = rb_enc_str_new(ptr, len, enc);
    if (!(func & STR_FUNC_REGEXP) && rb_enc_asciicompat(enc)) {
        if (is_ascii_string(str)) {
        }
        else if (rb_is_usascii_enc(enc0) && enc != rb_utf8_encoding()) {
            rb_enc_associate(str, rb_ascii8bit_encoding());
        }
    }

    return str;
}

#define peek(p,c) peek_n(p, (c), 0)
#define peek_n(p,c,n) (!lex_eol_n_p(p, n) && (c) == (unsigned char)(p)->lex.pcur[n])
#define peekc(p) peekc_n(p, 0)
#define peekc_n(p,n) (lex_eol_n_p(p, n) ? -1 : (unsigned char)(p)->lex.pcur[n])

static void
add_delayed_token(struct parser_params *p, const char *tok, const char *end, int line)
{
#ifndef RIPPER
    debug_token_line(p, "add_delayed_token", line);
#endif

    if (tok < end) {
        if (!has_delayed_token(p)) {
            p->delayed.token = rb_str_buf_new(end - tok);
            rb_enc_associate(p->delayed.token, p->enc);
            p->delayed.beg_line = p->ruby_sourceline;
            p->delayed.beg_col = rb_long2int(tok - p->lex.pbeg);
        }
        rb_str_buf_cat(p->delayed.token, tok, end - tok);
        p->delayed.end_line = p->ruby_sourceline;
        p->delayed.end_col = rb_long2int(end - p->lex.pbeg);
        p->lex.ptok = end;
    }
}

static void
set_lastline(struct parser_params *p, VALUE v)
{
    p->lex.pbeg = p->lex.pcur = RSTRING_PTR(v);
    p->lex.pend = p->lex.pcur + RSTRING_LEN(v);
    p->lex.lastline = v;
}

static int
nextline(struct parser_params *p, int set_encoding)
{
    VALUE v = p->lex.nextline;
    p->lex.nextline = 0;
    if (!v) {
        if (p->eofp)
            return -1;

        if (p->lex.pend > p->lex.pbeg && *(p->lex.pend-1) != '\n') {
            goto end_of_input;
        }

        if (!p->lex.input || NIL_P(v = lex_getline(p))) {
          end_of_input:
            p->eofp = 1;
            lex_goto_eol(p);
            return -1;
        }
#ifndef RIPPER
        if (p->debug_lines) {
            if (set_encoding) rb_enc_associate(v, p->enc);
            rb_ary_push(p->debug_lines, v);
        }
#endif
        p->cr_seen = FALSE;
    }
    else if (NIL_P(v)) {
        /* after here-document without terminator */
        goto end_of_input;
    }
    add_delayed_token(p, p->lex.ptok, p->lex.pend, __LINE__);
    if (p->heredoc_end > 0) {
        p->ruby_sourceline = p->heredoc_end;
        p->heredoc_end = 0;
    }
    p->ruby_sourceline++;
    set_lastline(p, v);
    token_flush(p);
    return 0;
}

static int
parser_cr(struct parser_params *p, int c)
{
    if (peek(p, '\n')) {
        p->lex.pcur++;
        c = '\n';
    }
    return c;
}

static inline int
nextc0(struct parser_params *p, int set_encoding)
{
    int c;

    if (UNLIKELY((p->lex.pcur == p->lex.pend) || p->eofp || RTEST(p->lex.nextline))) {
        if (nextline(p, set_encoding)) return -1;
    }
    c = (unsigned char)*p->lex.pcur++;
    if (UNLIKELY(c == '\r')) {
        c = parser_cr(p, c);
    }

    return c;
}
#define nextc(p) nextc0(p, TRUE)

static void
pushback(struct parser_params *p, int c)
{
    if (c == -1) return;
    p->lex.pcur--;
    if (p->lex.pcur > p->lex.pbeg && p->lex.pcur[0] == '\n' && p->lex.pcur[-1] == '\r') {
        p->lex.pcur--;
    }
}

#define was_bol(p) ((p)->lex.pcur == (p)->lex.pbeg + 1)

#define tokfix(p) ((p)->tokenbuf[(p)->tokidx]='\0')
#define tok(p) (p)->tokenbuf
#define toklen(p) (p)->tokidx

static int
looking_at_eol_p(struct parser_params *p)
{
    const char *ptr = p->lex.pcur;
    while (ptr < p->lex.pend) {
        int c = (unsigned char)*ptr++;
        int eol = (c == '\n' || c == '#');
        if (eol || !ISSPACE(c)) {
            return eol;
        }
    }
    return TRUE;
}

static char*
newtok(struct parser_params *p)
{
    p->tokidx = 0;
    p->tokline = p->ruby_sourceline;
    if (!p->tokenbuf) {
        p->toksiz = 60;
        p->tokenbuf = ALLOC_N(char, 60);
    }
    if (p->toksiz > 4096) {
        p->toksiz = 60;
        REALLOC_N(p->tokenbuf, char, 60);
    }
    return p->tokenbuf;
}

static char *
tokspace(struct parser_params *p, int n)
{
    p->tokidx += n;

    if (p->tokidx >= p->toksiz) {
        do {p->toksiz *= 2;} while (p->toksiz < p->tokidx);
        REALLOC_N(p->tokenbuf, char, p->toksiz);
    }
    return &p->tokenbuf[p->tokidx-n];
}

static void
tokadd(struct parser_params *p, int c)
{
    p->tokenbuf[p->tokidx++] = (char)c;
    if (p->tokidx >= p->toksiz) {
        p->toksiz *= 2;
        REALLOC_N(p->tokenbuf, char, p->toksiz);
    }
}

static int
tok_hex(struct parser_params *p, size_t *numlen)
{
    int c;

    c = scan_hex(p->lex.pcur, 2, numlen);
    if (!*numlen) {
        yyerror0("invalid hex escape");
        token_flush(p);
        return 0;
    }
    p->lex.pcur += *numlen;
    return c;
}

#define tokcopy(p, n) memcpy(tokspace(p, n), (p)->lex.pcur - (n), (n))

static int
escaped_control_code(int c)
{
    int c2 = 0;
    switch (c) {
      case ' ':
        c2 = 's';
        break;
      case '\n':
        c2 = 'n';
        break;
      case '\t':
        c2 = 't';
        break;
      case '\v':
        c2 = 'v';
        break;
      case '\r':
        c2 = 'r';
        break;
      case '\f':
        c2 = 'f';
        break;
    }
    return c2;
}

#define WARN_SPACE_CHAR(c, prefix) \
    rb_warn1("invalid character syntax; use "prefix"\\%c", WARN_I(c2))

static int
tokadd_codepoint(struct parser_params *p, rb_encoding **encp,
                 int regexp_literal, int wide)
{
    size_t numlen;
    int codepoint = scan_hex(p->lex.pcur, wide ? p->lex.pend - p->lex.pcur : 4, &numlen);
    p->lex.pcur += numlen;
    if (p->lex.strterm == NULL ||
        (p->lex.strterm->flags & STRTERM_HEREDOC) ||
        (p->lex.strterm->u.literal.u1.func != str_regexp)) {
        if (wide ? (numlen == 0 || numlen > 6) : (numlen < 4))  {
            literal_flush(p, p->lex.pcur);
            yyerror0("invalid Unicode escape");
            return wide && numlen > 0;
        }
        if (codepoint > 0x10ffff) {
            literal_flush(p, p->lex.pcur);
            yyerror0("invalid Unicode codepoint (too large)");
            return wide;
        }
        if ((codepoint & 0xfffff800) == 0xd800) {
            literal_flush(p, p->lex.pcur);
            yyerror0("invalid Unicode codepoint");
            return wide;
        }
    }
    if (regexp_literal) {
        tokcopy(p, (int)numlen);
    }
    else if (codepoint >= 0x80) {
        rb_encoding *utf8 = rb_utf8_encoding();
        if (*encp && utf8 != *encp) {
            YYLTYPE loc = RUBY_INIT_YYLLOC();
            compile_error(p, "UTF-8 mixed within %s source", rb_enc_name(*encp));
            parser_show_error_line(p, &loc);
            return wide;
        }
        *encp = utf8;
        tokaddmbc(p, codepoint, *encp);
    }
    else {
        tokadd(p, codepoint);
    }
    return TRUE;
}

/* return value is for ?\u3042 */
static void
tokadd_utf8(struct parser_params *p, rb_encoding **encp,
            int term, int symbol_literal, int regexp_literal)
{
    /*
     * If `term` is not -1, then we allow multiple codepoints in \u{}
     * upto `term` byte, otherwise we're parsing a character literal.
     * And then add the codepoints to the current token.
     */
    static const char multiple_codepoints[] = "Multiple codepoints at single character literal";

    const int open_brace = '{', close_brace = '}';

    if (regexp_literal) { tokadd(p, '\\'); tokadd(p, 'u'); }

    if (peek(p, open_brace)) {  /* handle \u{...} form */
        const char *second = NULL;
        int c, last = nextc(p);
        if (p->lex.pcur >= p->lex.pend) goto unterminated;
        while (ISSPACE(c = *p->lex.pcur) && ++p->lex.pcur < p->lex.pend);
        while (c != close_brace) {
            if (c == term) goto unterminated;
            if (second == multiple_codepoints)
                second = p->lex.pcur;
            if (regexp_literal) tokadd(p, last);
            if (!tokadd_codepoint(p, encp, regexp_literal, TRUE)) {
                break;
            }
            while (ISSPACE(c = *p->lex.pcur)) {
                if (++p->lex.pcur >= p->lex.pend) goto unterminated;
                last = c;
            }
            if (term == -1 && !second)
                second = multiple_codepoints;
        }

        if (c != close_brace) {
          unterminated:
            token_flush(p);
            yyerror0("unterminated Unicode escape");
            return;
        }
        if (second && second != multiple_codepoints) {
            const char *pcur = p->lex.pcur;
            p->lex.pcur = second;
            dispatch_scan_event(p, tSTRING_CONTENT);
            token_flush(p);
            p->lex.pcur = pcur;
            yyerror0(multiple_codepoints);
            token_flush(p);
        }

        if (regexp_literal) tokadd(p, close_brace);
        nextc(p);
    }
    else {			/* handle \uxxxx form */
        if (!tokadd_codepoint(p, encp, regexp_literal, FALSE)) {
            token_flush(p);
            return;
        }
    }
}

#define ESCAPE_CONTROL 1
#define ESCAPE_META    2

static int
read_escape(struct parser_params *p, int flags, rb_encoding **encp)
{
    int c;
    size_t numlen;

    switch (c = nextc(p)) {
      case '\\':	/* Backslash */
        return c;

      case 'n':	/* newline */
        return '\n';

      case 't':	/* horizontal tab */
        return '\t';

      case 'r':	/* carriage-return */
        return '\r';

      case 'f':	/* form-feed */
        return '\f';

      case 'v':	/* vertical tab */
        return '\13';

      case 'a':	/* alarm(bell) */
        return '\007';

      case 'e':	/* escape */
        return 033;

      case '0': case '1': case '2': case '3': /* octal constant */
      case '4': case '5': case '6': case '7':
        pushback(p, c);
        c = scan_oct(p->lex.pcur, 3, &numlen);
        p->lex.pcur += numlen;
        return c;

      case 'x':	/* hex constant */
        c = tok_hex(p, &numlen);
        if (numlen == 0) return 0;
        return c;

      case 'b':	/* backspace */
        return '\010';

      case 's':	/* space */
        return ' ';

      case 'M':
        if (flags & ESCAPE_META) goto eof;
        if ((c = nextc(p)) != '-') {
            goto eof;
        }
        if ((c = nextc(p)) == '\\') {
            switch (peekc(p)) {
              case 'u': case 'U':
                nextc(p);
                goto eof;
            }
            return read_escape(p, flags|ESCAPE_META, encp) | 0x80;
        }
        else if (c == -1 || !ISASCII(c)) goto eof;
        else {
            int c2 = escaped_control_code(c);
            if (c2) {
                if (ISCNTRL(c) || !(flags & ESCAPE_CONTROL)) {
                    WARN_SPACE_CHAR(c2, "\\M-");
                }
                else {
                    WARN_SPACE_CHAR(c2, "\\C-\\M-");
                }
            }
            else if (ISCNTRL(c)) goto eof;
            return ((c & 0xff) | 0x80);
        }

      case 'C':
        if ((c = nextc(p)) != '-') {
            goto eof;
        }
      case 'c':
        if (flags & ESCAPE_CONTROL) goto eof;
        if ((c = nextc(p))== '\\') {
            switch (peekc(p)) {
              case 'u': case 'U':
                nextc(p);
                goto eof;
            }
            c = read_escape(p, flags|ESCAPE_CONTROL, encp);
        }
        else if (c == '?')
            return 0177;
        else if (c == -1 || !ISASCII(c)) goto eof;
        else {
            int c2 = escaped_control_code(c);
            if (c2) {
                if (ISCNTRL(c)) {
                    if (flags & ESCAPE_META) {
                        WARN_SPACE_CHAR(c2, "\\M-");
                    }
                    else {
                        WARN_SPACE_CHAR(c2, "");
                    }
                }
                else {
                    if (flags & ESCAPE_META) {
                        WARN_SPACE_CHAR(c2, "\\M-\\C-");
                    }
                    else {
                        WARN_SPACE_CHAR(c2, "\\C-");
                    }
                }
            }
            else if (ISCNTRL(c)) goto eof;
        }
        return c & 0x9f;

      eof:
      case -1:
        yyerror0("Invalid escape character syntax");
        token_flush(p);
        return '\0';

      default:
        return c;
    }
}

static void
tokaddmbc(struct parser_params *p, int c, rb_encoding *enc)
{
    int len = rb_enc_codelen(c, enc);
    rb_enc_mbcput(c, tokspace(p, len), enc);
}

static int
tokadd_escape(struct parser_params *p, rb_encoding **encp)
{
    int c;
    size_t numlen;

    switch (c = nextc(p)) {
      case '\n':
        return 0;		/* just ignore */

      case '0': case '1': case '2': case '3': /* octal constant */
      case '4': case '5': case '6': case '7':
        {
            ruby_scan_oct(--p->lex.pcur, 3, &numlen);
            if (numlen == 0) goto eof;
            p->lex.pcur += numlen;
            tokcopy(p, (int)numlen + 1);
        }
        return 0;

      case 'x':	/* hex constant */
        {
            tok_hex(p, &numlen);
            if (numlen == 0) return -1;
            tokcopy(p, (int)numlen + 2);
        }
        return 0;

      eof:
      case -1:
        yyerror0("Invalid escape character syntax");
        token_flush(p);
        return -1;

      default:
        tokadd(p, '\\');
        tokadd(p, c);
    }
    return 0;
}

static int
regx_options(struct parser_params *p)
{
    int kcode = 0;
    int kopt = 0;
    int options = 0;
    int c, opt, kc;

    newtok(p);
    while (c = nextc(p), ISALPHA(c)) {
        if (c == 'o') {
            options |= RE_OPTION_ONCE;
        }
        else if (rb_char_to_option_kcode(c, &opt, &kc)) {
            if (kc >= 0) {
                if (kc != rb_ascii8bit_encindex()) kcode = c;
                kopt = opt;
            }
            else {
                options |= opt;
            }
        }
        else {
            tokadd(p, c);
        }
    }
    options |= kopt;
    pushback(p, c);
    if (toklen(p)) {
        YYLTYPE loc = RUBY_INIT_YYLLOC();
        tokfix(p);
        compile_error(p, "unknown regexp option%s - %*s",
                      toklen(p) > 1 ? "s" : "", toklen(p), tok(p));
        parser_show_error_line(p, &loc);
    }
    return options | RE_OPTION_ENCODING(kcode);
}

static int
tokadd_mbchar(struct parser_params *p, int c)
{
    int len = parser_precise_mbclen(p, p->lex.pcur-1);
    if (len < 0) return -1;
    tokadd(p, c);
    p->lex.pcur += --len;
    if (len > 0) tokcopy(p, len);
    return c;
}

static inline int
simple_re_meta(int c)
{
    switch (c) {
      case '$': case '*': case '+': case '.':
      case '?': case '^': case '|':
      case ')': case ']': case '}': case '>':
        return TRUE;
      default:
        return FALSE;
    }
}

static int
parser_update_heredoc_indent(struct parser_params *p, int c)
{
    if (p->heredoc_line_indent == -1) {
        if (c == '\n') p->heredoc_line_indent = 0;
    }
    else {
        if (c == ' ') {
            p->heredoc_line_indent++;
            return TRUE;
        }
        else if (c == '\t') {
            int w = (p->heredoc_line_indent / TAB_WIDTH) + 1;
            p->heredoc_line_indent = w * TAB_WIDTH;
            return TRUE;
        }
        else if (c != '\n') {
            if (p->heredoc_indent > p->heredoc_line_indent) {
                p->heredoc_indent = p->heredoc_line_indent;
            }
            p->heredoc_line_indent = -1;
        }
    }
    return FALSE;
}

static void
parser_mixed_error(struct parser_params *p, rb_encoding *enc1, rb_encoding *enc2)
{
    YYLTYPE loc = RUBY_INIT_YYLLOC();
    const char *n1 = rb_enc_name(enc1), *n2 = rb_enc_name(enc2);
    compile_error(p, "%s mixed within %s source", n1, n2);
    parser_show_error_line(p, &loc);
}

static void
parser_mixed_escape(struct parser_params *p, const char *beg, rb_encoding *enc1, rb_encoding *enc2)
{
    const char *pos = p->lex.pcur;
    p->lex.pcur = beg;
    parser_mixed_error(p, enc1, enc2);
    p->lex.pcur = pos;
}

static int
tokadd_string(struct parser_params *p,
              int func, int term, int paren, long *nest,
              rb_encoding **encp, rb_encoding **enc)
{
    int c;
    bool erred = false;
#ifdef RIPPER
    const int heredoc_end = (p->heredoc_end ? p->heredoc_end + 1 : 0);
    int top_of_line = FALSE;
#endif

#define mixed_error(enc1, enc2) \
    (void)(erred || (parser_mixed_error(p, enc1, enc2), erred = true))
#define mixed_escape(beg, enc1, enc2) \
    (void)(erred || (parser_mixed_escape(p, beg, enc1, enc2), erred = true))

    while ((c = nextc(p)) != -1) {
        if (p->heredoc_indent > 0) {
            parser_update_heredoc_indent(p, c);
        }
#ifdef RIPPER
        if (top_of_line && heredoc_end == p->ruby_sourceline) {
            pushback(p, c);
            break;
        }
#endif

        if (paren && c == paren) {
            ++*nest;
        }
        else if (c == term) {
            if (!nest || !*nest) {
                pushback(p, c);
                break;
            }
            --*nest;
        }
        else if ((func & STR_FUNC_EXPAND) && c == '#' && p->lex.pcur < p->lex.pend) {
            int c2 = *p->lex.pcur;
            if (c2 == '$' || c2 == '@' || c2 == '{') {
                pushback(p, c);
                break;
            }
        }
        else if (c == '\\') {
            c = nextc(p);
            switch (c) {
              case '\n':
                if (func & STR_FUNC_QWORDS) break;
                if (func & STR_FUNC_EXPAND) {
                    if (!(func & STR_FUNC_INDENT) || (p->heredoc_indent < 0))
                        continue;
                    if (c == term) {
                        c = '\\';
                        goto terminate;
                    }
                }
                tokadd(p, '\\');
                break;

              case '\\':
                if (func & STR_FUNC_ESCAPE) tokadd(p, c);
                break;

              case 'u':
                if ((func & STR_FUNC_EXPAND) == 0) {
                    tokadd(p, '\\');
                    break;
                }
                tokadd_utf8(p, enc, term,
                            func & STR_FUNC_SYMBOL,
                            func & STR_FUNC_REGEXP);
                continue;

              default:
                if (c == -1) return -1;
                if (!ISASCII(c)) {
                    if ((func & STR_FUNC_EXPAND) == 0) tokadd(p, '\\');
                    goto non_ascii;
                }
                if (func & STR_FUNC_REGEXP) {
                    switch (c) {
                      case 'c':
                      case 'C':
                      case 'M': {
                        pushback(p, c);
                        c = read_escape(p, 0, enc);

                        int i;
                        char escbuf[5];
                        snprintf(escbuf, sizeof(escbuf), "\\x%02X", c);
                        for (i = 0; i < 4; i++) {
                            tokadd(p, escbuf[i]);
                        }
                        continue;
                      }
                    }

                    if (c == term && !simple_re_meta(c)) {
                        tokadd(p, c);
                        continue;
                    }
                    pushback(p, c);
                    if ((c = tokadd_escape(p, enc)) < 0)
                        return -1;
                    if (*enc && *enc != *encp) {
                        mixed_escape(p->lex.ptok+2, *enc, *encp);
                    }
                    continue;
                }
                else if (func & STR_FUNC_EXPAND) {
                    pushback(p, c);
                    if (func & STR_FUNC_ESCAPE) tokadd(p, '\\');
                    c = read_escape(p, 0, enc);
                }
                else if ((func & STR_FUNC_QWORDS) && ISSPACE(c)) {
                    /* ignore backslashed spaces in %w */
                }
                else if (c != term && !(paren && c == paren)) {
                    tokadd(p, '\\');
                    pushback(p, c);
                    continue;
                }
            }
        }
        else if (!parser_isascii(p)) {
          non_ascii:
            if (!*enc) {
                *enc = *encp;
            }
            else if (*enc != *encp) {
                mixed_error(*enc, *encp);
                continue;
            }
            if (tokadd_mbchar(p, c) == -1) return -1;
            continue;
        }
        else if ((func & STR_FUNC_QWORDS) && ISSPACE(c)) {
            pushback(p, c);
            break;
        }
        if (c & 0x80) {
            if (!*enc) {
                *enc = *encp;
            }
            else if (*enc != *encp) {
                mixed_error(*enc, *encp);
                continue;
            }
        }
        tokadd(p, c);
#ifdef RIPPER
        top_of_line = (c == '\n');
#endif
    }
  terminate:
    if (*enc) *encp = *enc;
    return c;
}

static inline rb_strterm_t *
new_strterm(VALUE v1, VALUE v2, VALUE v3, VALUE v0)
{
    return (rb_strterm_t*)rb_imemo_new(imemo_parser_strterm, v1, v2, v3, v0);
}

/* imemo_parser_strterm for literal */
#define NEW_STRTERM(func, term, paren) \
    new_strterm((VALUE)(func), (VALUE)(paren), (VALUE)(term), 0)

#ifdef RIPPER
static void
flush_string_content(struct parser_params *p, rb_encoding *enc)
{
    VALUE content = yylval.val;
    if (!ripper_is_node_yylval(content))
        content = ripper_new_yylval(p, 0, 0, content);
    if (has_delayed_token(p)) {
        ptrdiff_t len = p->lex.pcur - p->lex.ptok;
        if (len > 0) {
            rb_enc_str_buf_cat(p->delayed.token, p->lex.ptok, len, enc);
        }
        dispatch_delayed_token(p, tSTRING_CONTENT);
        p->lex.ptok = p->lex.pcur;
        RNODE(content)->nd_rval = yylval.val;
    }
    dispatch_scan_event(p, tSTRING_CONTENT);
    if (yylval.val != content)
        RNODE(content)->nd_rval = yylval.val;
    yylval.val = content;
}
#else
static void
flush_string_content(struct parser_params *p, rb_encoding *enc)
{
    if (has_delayed_token(p)) {
        ptrdiff_t len = p->lex.pcur - p->lex.ptok;
        if (len > 0) {
            rb_enc_str_buf_cat(p->delayed.token, p->lex.ptok, len, enc);
            p->delayed.end_line = p->ruby_sourceline;
            p->delayed.end_col = rb_long2int(p->lex.pcur - p->lex.pbeg);
        }
        dispatch_delayed_token(p, tSTRING_CONTENT);
        p->lex.ptok = p->lex.pcur;
    }
    dispatch_scan_event(p, tSTRING_CONTENT);
}
#endif

RUBY_FUNC_EXPORTED const unsigned int ruby_global_name_punct_bits[(0x7e - 0x20 + 31) / 32];
/* this can be shared with ripper, since it's independent from struct
 * parser_params. */
#ifndef RIPPER
#define BIT(c, idx) (((c) / 32 - 1 == idx) ? (1U << ((c) % 32)) : 0)
#define SPECIAL_PUNCT(idx) ( \
        BIT('~', idx) | BIT('*', idx) | BIT('$', idx) | BIT('?', idx) | \
        BIT('!', idx) | BIT('@', idx) | BIT('/', idx) | BIT('\\', idx) | \
        BIT(';', idx) | BIT(',', idx) | BIT('.', idx) | BIT('=', idx) | \
        BIT(':', idx) | BIT('<', idx) | BIT('>', idx) | BIT('\"', idx) | \
        BIT('&', idx) | BIT('`', idx) | BIT('\'', idx) | BIT('+', idx) | \
        BIT('0', idx))
const unsigned int ruby_global_name_punct_bits[] = {
    SPECIAL_PUNCT(0),
    SPECIAL_PUNCT(1),
    SPECIAL_PUNCT(2),
};
#undef BIT
#undef SPECIAL_PUNCT
#endif

static enum yytokentype
parser_peek_variable_name(struct parser_params *p)
{
    int c;
    const char *ptr = p->lex.pcur;

    if (ptr + 1 >= p->lex.pend) return 0;
    c = *ptr++;
    switch (c) {
      case '$':
        if ((c = *ptr) == '-') {
            if (++ptr >= p->lex.pend) return 0;
            c = *ptr;
        }
        else if (is_global_name_punct(c) || ISDIGIT(c)) {
            return tSTRING_DVAR;
        }
        break;
      case '@':
        if ((c = *ptr) == '@') {
            if (++ptr >= p->lex.pend) return 0;
            c = *ptr;
        }
        break;
      case '{':
        p->lex.pcur = ptr;
        p->command_start = TRUE;
        return tSTRING_DBEG;
      default:
        return 0;
    }
    if (!ISASCII(c) || c == '_' || ISALPHA(c))
        return tSTRING_DVAR;
    return 0;
}

#define IS_ARG() IS_lex_state(EXPR_ARG_ANY)
#define IS_END() IS_lex_state(EXPR_END_ANY)
#define IS_BEG() (IS_lex_state(EXPR_BEG_ANY) || IS_lex_state_all(EXPR_ARG|EXPR_LABELED))
#define IS_SPCARG(c) (IS_ARG() && space_seen && !ISSPACE(c))
#define IS_LABEL_POSSIBLE() (\
        (IS_lex_state(EXPR_LABEL|EXPR_ENDFN) && !cmd_state) || \
        IS_ARG())
#define IS_LABEL_SUFFIX(n) (peek_n(p, ':',(n)) && !peek_n(p, ':', (n)+1))
#define IS_AFTER_OPERATOR() IS_lex_state(EXPR_FNAME | EXPR_DOT)

static inline enum yytokentype
parser_string_term(struct parser_params *p, int func)
{
    p->lex.strterm = 0;
    if (func & STR_FUNC_REGEXP) {
        set_yylval_num(regx_options(p));
        dispatch_scan_event(p, tREGEXP_END);
        SET_LEX_STATE(EXPR_END);
        return tREGEXP_END;
    }
    if ((func & STR_FUNC_LABEL) && IS_LABEL_SUFFIX(0)) {
        nextc(p);
        SET_LEX_STATE(EXPR_ARG|EXPR_LABELED);
        return tLABEL_END;
    }
    SET_LEX_STATE(EXPR_END);
    return tSTRING_END;
}

static enum yytokentype
parse_string(struct parser_params *p, rb_strterm_literal_t *quote)
{
    int func = (int)quote->u1.func;
    int term = (int)quote->u3.term;
    int paren = (int)quote->u2.paren;
    int c, space = 0;
    rb_encoding *enc = p->enc;
    rb_encoding *base_enc = 0;
    VALUE lit;

    if (func & STR_FUNC_TERM) {
        if (func & STR_FUNC_QWORDS) nextc(p); /* delayed term */
        SET_LEX_STATE(EXPR_END);
        p->lex.strterm = 0;
        return func & STR_FUNC_REGEXP ? tREGEXP_END : tSTRING_END;
    }
    c = nextc(p);
    if ((func & STR_FUNC_QWORDS) && ISSPACE(c)) {
        while (c != '\n' && ISSPACE(c = nextc(p)));
        space = 1;
    }
    if (func & STR_FUNC_LIST) {
        quote->u1.func &= ~STR_FUNC_LIST;
        space = 1;
    }
    if (c == term && !quote->u0.nest) {
        if (func & STR_FUNC_QWORDS) {
            quote->u1.func |= STR_FUNC_TERM;
            pushback(p, c); /* dispatch the term at tSTRING_END */
            add_delayed_token(p, p->lex.ptok, p->lex.pcur, __LINE__);
            return ' ';
        }
        return parser_string_term(p, func);
    }
    if (space) {
        if (!ISSPACE(c)) pushback(p, c);
        add_delayed_token(p, p->lex.ptok, p->lex.pcur, __LINE__);
        return ' ';
    }
    newtok(p);
    if ((func & STR_FUNC_EXPAND) && c == '#') {
        int t = parser_peek_variable_name(p);
        if (t) return t;
        tokadd(p, '#');
        c = nextc(p);
    }
    pushback(p, c);
    if (tokadd_string(p, func, term, paren, &quote->u0.nest,
                      &enc, &base_enc) == -1) {
        if (p->eofp) {
#ifndef RIPPER
# define unterminated_literal(mesg) yyerror0(mesg)
#else
# define unterminated_literal(mesg) compile_error(p,  mesg)
#endif
            literal_flush(p, p->lex.pcur);
            if (func & STR_FUNC_QWORDS) {
                /* no content to add, bailing out here */
                unterminated_literal("unterminated list meets end of file");
                p->lex.strterm = 0;
                return tSTRING_END;
            }
            if (func & STR_FUNC_REGEXP) {
                unterminated_literal("unterminated regexp meets end of file");
            }
            else {
                unterminated_literal("unterminated string meets end of file");
            }
            quote->u1.func |= STR_FUNC_TERM;
        }
    }

    tokfix(p);
    lit = STR_NEW3(tok(p), toklen(p), enc, func);
    set_yylval_str(lit);
    flush_string_content(p, enc);

    return tSTRING_CONTENT;
}

static enum yytokentype
heredoc_identifier(struct parser_params *p)
{
    /*
     * term_len is length of `<<"END"` except `END`,
     * in this case term_len is 4 (<, <, " and ").
     */
    long len, offset = p->lex.pcur - p->lex.pbeg;
    int c = nextc(p), term, func = 0, quote = 0;
    enum yytokentype token = tSTRING_BEG;
    int indent = 0;

    if (c == '-') {
        c = nextc(p);
        func = STR_FUNC_INDENT;
        offset++;
    }
    else if (c == '~') {
        c = nextc(p);
        func = STR_FUNC_INDENT;
        offset++;
        indent = INT_MAX;
    }
    switch (c) {
      case '\'':
        func |= str_squote; goto quoted;
      case '"':
        func |= str_dquote; goto quoted;
      case '`':
        token = tXSTRING_BEG;
        func |= str_xquote; goto quoted;

      quoted:
        quote++;
        offset++;
        term = c;
        len = 0;
        while ((c = nextc(p)) != term) {
            if (c == -1 || c == '\r' || c == '\n') {
                yyerror0("unterminated here document identifier");
                return -1;
            }
        }
        break;

      default:
        if (!parser_is_identchar(p)) {
            pushback(p, c);
            if (func & STR_FUNC_INDENT) {
                pushback(p, indent > 0 ? '~' : '-');
            }
            return 0;
        }
        func |= str_dquote;
        do {
            int n = parser_precise_mbclen(p, p->lex.pcur-1);
            if (n < 0) return 0;
            p->lex.pcur += --n;
        } while ((c = nextc(p)) != -1 && parser_is_identchar(p));
        pushback(p, c);
        break;
    }

    len = p->lex.pcur - (p->lex.pbeg + offset) - quote;
    if ((unsigned long)len >= HERETERM_LENGTH_MAX)
        yyerror0("too long here document identifier");
    dispatch_scan_event(p, tHEREDOC_BEG);
    lex_goto_eol(p);

    p->lex.strterm = new_strterm(0, 0, 0, p->lex.lastline);
    p->lex.strterm->flags |= STRTERM_HEREDOC;
    rb_strterm_heredoc_t *here = &p->lex.strterm->u.heredoc;
    here->offset = offset;
    here->sourceline = p->ruby_sourceline;
    here->length = (int)len;
    here->quote = quote;
    here->func = func;

    token_flush(p);
    p->heredoc_indent = indent;
    p->heredoc_line_indent = 0;
    return token;
}

static void
heredoc_restore(struct parser_params *p, rb_strterm_heredoc_t *here)
{
    VALUE line;

    p->lex.strterm = 0;
    line = here->lastline;
    p->lex.lastline = line;
    p->lex.pbeg = RSTRING_PTR(line);
    p->lex.pend = p->lex.pbeg + RSTRING_LEN(line);
    p->lex.pcur = p->lex.pbeg + here->offset + here->length + here->quote;
    p->lex.ptok = p->lex.pbeg + here->offset - here->quote;
    p->heredoc_end = p->ruby_sourceline;
    p->ruby_sourceline = (int)here->sourceline;
    if (p->eofp) p->lex.nextline = Qnil;
    p->eofp = 0;
}

static int
dedent_string(VALUE string, int width)
{
    char *str;
    long len;
    int i, col = 0;

    RSTRING_GETMEM(string, str, len);
    for (i = 0; i < len && col < width; i++) {
        if (str[i] == ' ') {
            col++;
        }
        else if (str[i] == '\t') {
            int n = TAB_WIDTH * (col / TAB_WIDTH + 1);
            if (n > width) break;
            col = n;
        }
        else {
            break;
        }
    }
    if (!i) return 0;
    rb_str_modify(string);
    str = RSTRING_PTR(string);
    if (RSTRING_LEN(string) != len)
        rb_fatal("literal string changed: %+"PRIsVALUE, string);
    MEMMOVE(str, str + i, char, len - i);
    rb_str_set_len(string, len - i);
    return i;
}

#ifndef RIPPER
static NODE *
heredoc_dedent(struct parser_params *p, NODE *root)
{
    NODE *node, *str_node, *prev_node;
    int indent = p->heredoc_indent;
    VALUE prev_lit = 0;

    if (indent <= 0) return root;
    p->heredoc_indent = 0;
    if (!root) return root;

    prev_node = node = str_node = root;
    if (nd_type_p(root, NODE_LIST)) str_node = root->nd_head;

    while (str_node) {
        VALUE lit = str_node->nd_lit;
        if (str_node->flags & NODE_FL_NEWLINE) {
            dedent_string(lit, indent);
        }
        if (!prev_lit) {
            prev_lit = lit;
        }
        else if (!literal_concat0(p, prev_lit, lit)) {
            return 0;
        }
        else {
            NODE *end = node->nd_end;
            node = prev_node->nd_next = node->nd_next;
            if (!node) {
                if (nd_type_p(prev_node, NODE_DSTR))
                    nd_set_type(prev_node, NODE_STR);
                break;
            }
            node->nd_end = end;
            goto next_str;
        }

        str_node = 0;
        while ((node = (prev_node = node)->nd_next) != 0) {
          next_str:
            if (!nd_type_p(node, NODE_LIST)) break;
            if ((str_node = node->nd_head) != 0) {
                enum node_type type = nd_type(str_node);
                if (type == NODE_STR || type == NODE_DSTR) break;
                prev_lit = 0;
                str_node = 0;
            }
        }
    }
    return root;
}
#else /* RIPPER */
static VALUE
heredoc_dedent(struct parser_params *p, VALUE array)
{
    int indent = p->heredoc_indent;

    if (indent <= 0) return array;
    p->heredoc_indent = 0;
    dispatch2(heredoc_dedent, array, INT2NUM(indent));
    return array;
}

/*
 *  call-seq:
 *    Ripper.dedent_string(input, width)   -> Integer
 *
 *  USE OF RIPPER LIBRARY ONLY.
 *
 *  Strips up to +width+ leading whitespaces from +input+,
 *  and returns the stripped column width.
 */
static VALUE
parser_dedent_string(VALUE self, VALUE input, VALUE width)
{
    int wid, col;

    StringValue(input);
    wid = NUM2UINT(width);
    col = dedent_string(input, wid);
    return INT2NUM(col);
}
#endif

static int
whole_match_p(struct parser_params *p, const char *eos, long len, int indent)
{
    const char *beg = p->lex.pbeg;
    const char *ptr = p->lex.pend;

    if (ptr - beg < len) return FALSE;
    if (ptr > beg && ptr[-1] == '\n') {
        if (--ptr > beg && ptr[-1] == '\r') --ptr;
        if (ptr - beg < len) return FALSE;
    }
    if (strncmp(eos, ptr -= len, len)) return FALSE;
    if (indent) {
        while (beg < ptr && ISSPACE(*beg)) beg++;
    }
    return beg == ptr;
}

static int
word_match_p(struct parser_params *p, const char *word, long len)
{
    if (strncmp(p->lex.pcur, word, len)) return 0;
    if (p->lex.pcur + len == p->lex.pend) return 1;
    int c = (unsigned char)p->lex.pcur[len];
    if (ISSPACE(c)) return 1;
    switch (c) {
      case '\0': case '\004': case '\032': return 1;
    }
    return 0;
}

#define NUM_SUFFIX_R   (1<<0)
#define NUM_SUFFIX_I   (1<<1)
#define NUM_SUFFIX_ALL 3

static int
number_literal_suffix(struct parser_params *p, int mask)
{
    int c, result = 0;
    const char *lastp = p->lex.pcur;

    while ((c = nextc(p)) != -1) {
        if ((mask & NUM_SUFFIX_I) && c == 'i') {
            result |= (mask & NUM_SUFFIX_I);
            mask &= ~NUM_SUFFIX_I;
            /* r after i, rational of complex is disallowed */
            mask &= ~NUM_SUFFIX_R;
            continue;
        }
        if ((mask & NUM_SUFFIX_R) && c == 'r') {
            result |= (mask & NUM_SUFFIX_R);
            mask &= ~NUM_SUFFIX_R;
            continue;
        }
        if (!ISASCII(c) || ISALPHA(c) || c == '_') {
            p->lex.pcur = lastp;
            literal_flush(p, p->lex.pcur);
            return 0;
        }
        pushback(p, c);
        break;
    }
    return result;
}

static enum yytokentype
set_number_literal(struct parser_params *p, VALUE v,
                   enum yytokentype type, int suffix)
{
    if (suffix & NUM_SUFFIX_I) {
        v = rb_complex_raw(INT2FIX(0), v);
        type = tIMAGINARY;
    }
    set_yylval_literal(v);
    SET_LEX_STATE(EXPR_END);
    return type;
}

static enum yytokentype
set_integer_literal(struct parser_params *p, VALUE v, int suffix)
{
    enum yytokentype type = tINTEGER;
    if (suffix & NUM_SUFFIX_R) {
        v = rb_rational_raw1(v);
        type = tRATIONAL;
    }
    return set_number_literal(p, v, type, suffix);
}

#ifdef RIPPER
static void
dispatch_heredoc_end(struct parser_params *p)
{
    VALUE str;
    if (has_delayed_token(p))
        dispatch_delayed_token(p, tSTRING_CONTENT);
    str = STR_NEW(p->lex.ptok, p->lex.pend - p->lex.ptok);
    ripper_dispatch1(p, ripper_token2eventid(tHEREDOC_END), str);
    RUBY_SET_YYLLOC_FROM_STRTERM_HEREDOC(*p->yylloc);
    lex_goto_eol(p);
    token_flush(p);
}

#else
#define dispatch_heredoc_end(p) parser_dispatch_heredoc_end(p, __LINE__)
static void
parser_dispatch_heredoc_end(struct parser_params *p, int line)
{
    if (has_delayed_token(p))
        dispatch_delayed_token(p, tSTRING_CONTENT);

    if (p->keep_tokens) {
        VALUE str = STR_NEW(p->lex.ptok, p->lex.pend - p->lex.ptok);
        RUBY_SET_YYLLOC_OF_HEREDOC_END(*p->yylloc);
        parser_append_tokens(p, str, tHEREDOC_END, line);
    }

    RUBY_SET_YYLLOC_FROM_STRTERM_HEREDOC(*p->yylloc);
    lex_goto_eol(p);
    token_flush(p);
}
#endif

static enum yytokentype
here_document(struct parser_params *p, rb_strterm_heredoc_t *here)
{
    int c, func, indent = 0;
    const char *eos, *ptr, *ptr_end;
    long len;
    VALUE str = 0;
    rb_encoding *enc = p->enc;
    rb_encoding *base_enc = 0;
    int bol;

    eos = RSTRING_PTR(here->lastline) + here->offset;
    len = here->length;
    indent = (func = here->func) & STR_FUNC_INDENT;

    if ((c = nextc(p)) == -1) {
      error:
#ifdef RIPPER
        if (!has_delayed_token(p)) {
            dispatch_scan_event(p, tSTRING_CONTENT);
        }
        else {
            if ((len = p->lex.pcur - p->lex.ptok) > 0) {
                if (!(func & STR_FUNC_REGEXP) && rb_enc_asciicompat(enc)) {
                    int cr = ENC_CODERANGE_UNKNOWN;
                    rb_str_coderange_scan_restartable(p->lex.ptok, p->lex.pcur, enc, &cr);
                    if (cr != ENC_CODERANGE_7BIT &&
                        rb_is_usascii_enc(p->enc) &&
                        enc != rb_utf8_encoding()) {
                        enc = rb_ascii8bit_encoding();
                    }
                }
                rb_enc_str_buf_cat(p->delayed.token, p->lex.ptok, len, enc);
            }
            dispatch_delayed_token(p, tSTRING_CONTENT);
        }
        lex_goto_eol(p);
#endif
        heredoc_restore(p, &p->lex.strterm->u.heredoc);
        compile_error(p, "can't find string \"%.*s\" anywhere before EOF",
                      (int)len, eos);
        token_flush(p);
        p->lex.strterm = 0;
        SET_LEX_STATE(EXPR_END);
        return tSTRING_END;
    }
    bol = was_bol(p);
    if (!bol) {
        /* not beginning of line, cannot be the terminator */
    }
    else if (p->heredoc_line_indent == -1) {
        /* `heredoc_line_indent == -1` means
         * - "after an interpolation in the same line", or
         * - "in a continuing line"
         */
        p->heredoc_line_indent = 0;
    }
    else if (whole_match_p(p, eos, len, indent)) {
        dispatch_heredoc_end(p);
      restore:
        heredoc_restore(p, &p->lex.strterm->u.heredoc);
        token_flush(p);
        p->lex.strterm = 0;
        SET_LEX_STATE(EXPR_END);
        return tSTRING_END;
    }

    if (!(func & STR_FUNC_EXPAND)) {
        do {
            ptr = RSTRING_PTR(p->lex.lastline);
            ptr_end = p->lex.pend;
            if (ptr_end > ptr) {
                switch (ptr_end[-1]) {
                  case '\n':
                    if (--ptr_end == ptr || ptr_end[-1] != '\r') {
                        ptr_end++;
                        break;
                    }
                  case '\r':
                    --ptr_end;
                }
            }

            if (p->heredoc_indent > 0) {
                long i = 0;
                while (ptr + i < ptr_end && parser_update_heredoc_indent(p, ptr[i]))
                    i++;
                p->heredoc_line_indent = 0;
            }

            if (str)
                rb_str_cat(str, ptr, ptr_end - ptr);
            else
                str = STR_NEW(ptr, ptr_end - ptr);
            if (ptr_end < p->lex.pend) rb_str_cat(str, "\n", 1);
            lex_goto_eol(p);
            if (p->heredoc_indent > 0) {
                goto flush_str;
            }
            if (nextc(p) == -1) {
                if (str) {
                    str = 0;
                }
                goto error;
            }
        } while (!whole_match_p(p, eos, len, indent));
    }
    else {
        /*	int mb = ENC_CODERANGE_7BIT, *mbp = &mb;*/
        newtok(p);
        if (c == '#') {
            int t = parser_peek_variable_name(p);
            if (p->heredoc_line_indent != -1) {
                if (p->heredoc_indent > p->heredoc_line_indent) {
                    p->heredoc_indent = p->heredoc_line_indent;
                }
                p->heredoc_line_indent = -1;
            }
            if (t) return t;
            tokadd(p, '#');
            c = nextc(p);
        }
        do {
            pushback(p, c);
            enc = p->enc;
            if ((c = tokadd_string(p, func, '\n', 0, NULL, &enc, &base_enc)) == -1) {
                if (p->eofp) goto error;
                goto restore;
            }
            if (c != '\n') {
                if (c == '\\') p->heredoc_line_indent = -1;
              flush:
                str = STR_NEW3(tok(p), toklen(p), enc, func);
              flush_str:
                set_yylval_str(str);
#ifndef RIPPER
                if (bol) yylval.node->flags |= NODE_FL_NEWLINE;
#endif
                flush_string_content(p, enc);
                return tSTRING_CONTENT;
            }
            tokadd(p, nextc(p));
            if (p->heredoc_indent > 0) {
                lex_goto_eol(p);
                goto flush;
            }
            /*	    if (mbp && mb == ENC_CODERANGE_UNKNOWN) mbp = 0;*/
            if ((c = nextc(p)) == -1) goto error;
        } while (!whole_match_p(p, eos, len, indent));
        str = STR_NEW3(tok(p), toklen(p), enc, func);
    }
    dispatch_heredoc_end(p);
#ifdef RIPPER
    str = ripper_new_yylval(p, ripper_token2eventid(tSTRING_CONTENT),
                            yylval.val, str);
#endif
    heredoc_restore(p, &p->lex.strterm->u.heredoc);
    token_flush(p);
    p->lex.strterm = NEW_STRTERM(func | STR_FUNC_TERM, 0, 0);
    set_yylval_str(str);
#ifndef RIPPER
    if (bol) yylval.node->flags |= NODE_FL_NEWLINE;
#endif
    return tSTRING_CONTENT;
}

#include "lex.c"

static int
arg_ambiguous(struct parser_params *p, char c)
{
#ifndef RIPPER
    if (c == '/') {
        rb_warning1("ambiguity between regexp and two divisions: wrap regexp in parentheses or add a space after `%c' operator", WARN_I(c));
    }
    else {
        rb_warning1("ambiguous first argument; put parentheses or a space even after `%c' operator", WARN_I(c));
    }
#else
    dispatch1(arg_ambiguous, rb_usascii_str_new(&c, 1));
#endif
    return TRUE;
}

static ID
#ifndef RIPPER
formal_argument(struct parser_params *p, ID lhs)
#else
formal_argument(struct parser_params *p, VALUE lhs)
#endif
{
    ID id = get_id(lhs);

    switch (id_type(id)) {
      case ID_LOCAL:
        break;
#ifndef RIPPER
# define ERR(mesg) yyerror0(mesg)
#else
# define ERR(mesg) (dispatch2(param_error, WARN_S(mesg), lhs), ripper_error(p))
#endif
      case ID_CONST:
        ERR("formal argument cannot be a constant");
        return 0;
      case ID_INSTANCE:
        ERR("formal argument cannot be an instance variable");
        return 0;
      case ID_GLOBAL:
        ERR("formal argument cannot be a global variable");
        return 0;
      case ID_CLASS:
        ERR("formal argument cannot be a class variable");
        return 0;
      default:
        ERR("formal argument must be local variable");
        return 0;
#undef ERR
    }
    shadowing_lvar(p, id);
    return lhs;
}

static int
lvar_defined(struct parser_params *p, ID id)
{
    return (dyna_in_block(p) && dvar_defined(p, id)) || local_id(p, id);
}

/* emacsen -*- hack */
static long
parser_encode_length(struct parser_params *p, const char *name, long len)
{
    long nlen;

    if (len > 5 && name[nlen = len - 5] == '-') {
        if (rb_memcicmp(name + nlen + 1, "unix", 4) == 0)
            return nlen;
    }
    if (len > 4 && name[nlen = len - 4] == '-') {
        if (rb_memcicmp(name + nlen + 1, "dos", 3) == 0)
            return nlen;
        if (rb_memcicmp(name + nlen + 1, "mac", 3) == 0 &&
            !(len == 8 && rb_memcicmp(name, "utf8-mac", len) == 0))
            /* exclude UTF8-MAC because the encoding named "UTF8" doesn't exist in Ruby */
            return nlen;
    }
    return len;
}

static void
parser_set_encode(struct parser_params *p, const char *name)
{
    int idx = rb_enc_find_index(name);
    rb_encoding *enc;
    VALUE excargs[3];

    if (idx < 0) {
        excargs[1] = rb_sprintf("unknown encoding name: %s", name);
      error:
        excargs[0] = rb_eArgError;
        excargs[2] = rb_make_backtrace();
        rb_ary_unshift(excargs[2], rb_sprintf("%"PRIsVALUE":%d", p->ruby_sourcefile_string, p->ruby_sourceline));
        rb_exc_raise(rb_make_exception(3, excargs));
    }
    enc = rb_enc_from_index(idx);
    if (!rb_enc_asciicompat(enc)) {
        excargs[1] = rb_sprintf("%s is not ASCII compatible", rb_enc_name(enc));
        goto error;
    }
    p->enc = enc;
#ifndef RIPPER
    if (p->debug_lines) {
        VALUE lines = p->debug_lines;
        long i, n = RARRAY_LEN(lines);
        for (i = 0; i < n; ++i) {
            rb_enc_associate_index(RARRAY_AREF(lines, i), idx);
        }
    }
#endif
}

static int
comment_at_top(struct parser_params *p)
{
    const char *ptr = p->lex.pbeg, *ptr_end = p->lex.pcur - 1;
    if (p->line_count != (p->has_shebang ? 2 : 1)) return 0;
    while (ptr < ptr_end) {
        if (!ISSPACE(*ptr)) return 0;
        ptr++;
    }
    return 1;
}

typedef long (*rb_magic_comment_length_t)(struct parser_params *p, const char *name, long len);
typedef void (*rb_magic_comment_setter_t)(struct parser_params *p, const char *name, const char *val);

static int parser_invalid_pragma_value(struct parser_params *p, const char *name, const char *val);

static void
magic_comment_encoding(struct parser_params *p, const char *name, const char *val)
{
    if (!comment_at_top(p)) {
        return;
    }
    parser_set_encode(p, val);
}

static int
parser_get_bool(struct parser_params *p, const char *name, const char *val)
{
    switch (*val) {
      case 't': case 'T':
        if (STRCASECMP(val, "true") == 0) {
            return TRUE;
        }
        break;
      case 'f': case 'F':
        if (STRCASECMP(val, "false") == 0) {
            return FALSE;
        }
        break;
    }
    return parser_invalid_pragma_value(p, name, val);
}

static int
parser_invalid_pragma_value(struct parser_params *p, const char *name, const char *val)
{
    rb_warning2("invalid value for %s: %s", WARN_S(name), WARN_S(val));
    return -1;
}

static void
parser_set_token_info(struct parser_params *p, const char *name, const char *val)
{
    int b = parser_get_bool(p, name, val);
    if (b >= 0) p->token_info_enabled = b;
}

static void
parser_set_compile_option_flag(struct parser_params *p, const char *name, const char *val)
{
    int b;

    if (p->token_seen) {
        rb_warning1("`%s' is ignored after any tokens", WARN_S(name));
        return;
    }

    b = parser_get_bool(p, name, val);
    if (b < 0) return;

    if (!p->compile_option)
        p->compile_option = rb_obj_hide(rb_ident_hash_new());
    rb_hash_aset(p->compile_option, ID2SYM(rb_intern(name)),
                 RBOOL(b));
}

static void
parser_set_shareable_constant_value(struct parser_params *p, const char *name, const char *val)
{
    for (const char *s = p->lex.pbeg, *e = p->lex.pcur; s < e; ++s) {
        if (*s == ' ' || *s == '\t') continue;
        if (*s == '#') break;
        rb_warning1("`%s' is ignored unless in comment-only line", WARN_S(name));
        return;
    }

    switch (*val) {
      case 'n': case 'N':
        if (STRCASECMP(val, "none") == 0) {
            p->ctxt.shareable_constant_value = shareable_none;
            return;
        }
        break;
      case 'l': case 'L':
        if (STRCASECMP(val, "literal") == 0) {
            p->ctxt.shareable_constant_value = shareable_literal;
            return;
        }
        break;
      case 'e': case 'E':
        if (STRCASECMP(val, "experimental_copy") == 0) {
            p->ctxt.shareable_constant_value = shareable_copy;
            return;
        }
        if (STRCASECMP(val, "experimental_everything") == 0) {
            p->ctxt.shareable_constant_value = shareable_everything;
            return;
        }
        break;
    }
    parser_invalid_pragma_value(p, name, val);
}

# if WARN_PAST_SCOPE
static void
parser_set_past_scope(struct parser_params *p, const char *name, const char *val)
{
    int b = parser_get_bool(p, name, val);
    if (b >= 0) p->past_scope_enabled = b;
}
# endif

struct magic_comment {
    const char *name;
    rb_magic_comment_setter_t func;
    rb_magic_comment_length_t length;
};

static const struct magic_comment magic_comments[] = {
    {"coding", magic_comment_encoding, parser_encode_length},
    {"encoding", magic_comment_encoding, parser_encode_length},
    {"frozen_string_literal", parser_set_compile_option_flag},
    {"shareable_constant_value", parser_set_shareable_constant_value},
    {"warn_indent", parser_set_token_info},
# if WARN_PAST_SCOPE
    {"warn_past_scope", parser_set_past_scope},
# endif
};

static const char *
magic_comment_marker(const char *str, long len)
{
    long i = 2;

    while (i < len) {
        switch (str[i]) {
          case '-':
            if (str[i-1] == '*' && str[i-2] == '-') {
                return str + i + 1;
            }
            i += 2;
            break;
          case '*':
            if (i + 1 >= len) return 0;
            if (str[i+1] != '-') {
                i += 4;
            }
            else if (str[i-1] != '-') {
                i += 2;
            }
            else {
                return str + i + 2;
            }
            break;
          default:
            i += 3;
            break;
        }
    }
    return 0;
}

static int
parser_magic_comment(struct parser_params *p, const char *str, long len)
{
    int indicator = 0;
    VALUE name = 0, val = 0;
    const char *beg, *end, *vbeg, *vend;
#define str_copy(_s, _p, _n) ((_s) \
        ? (void)(rb_str_resize((_s), (_n)), \
           MEMCPY(RSTRING_PTR(_s), (_p), char, (_n)), (_s)) \
        : (void)((_s) = STR_NEW((_p), (_n))))

    if (len <= 7) return FALSE;
    if (!!(beg = magic_comment_marker(str, len))) {
        if (!(end = magic_comment_marker(beg, str + len - beg)))
            return FALSE;
        indicator = TRUE;
        str = beg;
        len = end - beg - 3;
    }

    /* %r"([^\\s\'\":;]+)\\s*:\\s*(\"(?:\\\\.|[^\"])*\"|[^\"\\s;]+)[\\s;]*" */
    while (len > 0) {
        const struct magic_comment *mc = magic_comments;
        char *s;
        int i;
        long n = 0;

        for (; len > 0 && *str; str++, --len) {
            switch (*str) {
              case '\'': case '"': case ':': case ';':
                continue;
            }
            if (!ISSPACE(*str)) break;
        }
        for (beg = str; len > 0; str++, --len) {
            switch (*str) {
              case '\'': case '"': case ':': case ';':
                break;
              default:
                if (ISSPACE(*str)) break;
                continue;
            }
            break;
        }
        for (end = str; len > 0 && ISSPACE(*str); str++, --len);
        if (!len) break;
        if (*str != ':') {
            if (!indicator) return FALSE;
            continue;
        }

        do str++; while (--len > 0 && ISSPACE(*str));
        if (!len) break;
        if (*str == '"') {
            for (vbeg = ++str; --len > 0 && *str != '"'; str++) {
                if (*str == '\\') {
                    --len;
                    ++str;
                }
            }
            vend = str;
            if (len) {
                --len;
                ++str;
            }
        }
        else {
            for (vbeg = str; len > 0 && *str != '"' && *str != ';' && !ISSPACE(*str); --len, str++);
            vend = str;
        }
        if (indicator) {
            while (len > 0 && (*str == ';' || ISSPACE(*str))) --len, str++;
        }
        else {
            while (len > 0 && (ISSPACE(*str))) --len, str++;
            if (len) return FALSE;
        }

        n = end - beg;
        str_copy(name, beg, n);
        s = RSTRING_PTR(name);
        for (i = 0; i < n; ++i) {
            if (s[i] == '-') s[i] = '_';
        }
        do {
            if (STRNCASECMP(mc->name, s, n) == 0 && !mc->name[n]) {
                n = vend - vbeg;
                if (mc->length) {
                    n = (*mc->length)(p, vbeg, n);
                }
                str_copy(val, vbeg, n);
                (*mc->func)(p, mc->name, RSTRING_PTR(val));
                break;
            }
        } while (++mc < magic_comments + numberof(magic_comments));
#ifdef RIPPER
        str_copy(val, vbeg, vend - vbeg);
        dispatch2(magic_comment, name, val);
#endif
    }

    return TRUE;
}

static void
set_file_encoding(struct parser_params *p, const char *str, const char *send)
{
    int sep = 0;
    const char *beg = str;
    VALUE s;

    for (;;) {
        if (send - str <= 6) return;
        switch (str[6]) {
          case 'C': case 'c': str += 6; continue;
          case 'O': case 'o': str += 5; continue;
          case 'D': case 'd': str += 4; continue;
          case 'I': case 'i': str += 3; continue;
          case 'N': case 'n': str += 2; continue;
          case 'G': case 'g': str += 1; continue;
          case '=': case ':':
            sep = 1;
            str += 6;
            break;
          default:
            str += 6;
            if (ISSPACE(*str)) break;
            continue;
        }
        if (STRNCASECMP(str-6, "coding", 6) == 0) break;
        sep = 0;
    }
    for (;;) {
        do {
            if (++str >= send) return;
        } while (ISSPACE(*str));
        if (sep) break;
        if (*str != '=' && *str != ':') return;
        sep = 1;
        str++;
    }
    beg = str;
    while ((*str == '-' || *str == '_' || ISALNUM(*str)) && ++str < send);
    s = rb_str_new(beg, parser_encode_length(p, beg, str - beg));
    parser_set_encode(p, RSTRING_PTR(s));
    rb_str_resize(s, 0);
}

static void
parser_prepare(struct parser_params *p)
{
    int c = nextc0(p, FALSE);
    p->token_info_enabled = !compile_for_eval && RTEST(ruby_verbose);
    switch (c) {
      case '#':
        if (peek(p, '!')) p->has_shebang = 1;
        break;
      case 0xef:		/* UTF-8 BOM marker */
        if (p->lex.pend - p->lex.pcur >= 2 &&
            (unsigned char)p->lex.pcur[0] == 0xbb &&
            (unsigned char)p->lex.pcur[1] == 0xbf) {
            p->enc = rb_utf8_encoding();
            p->lex.pcur += 2;
#ifndef RIPPER
            if (p->debug_lines) {
                rb_enc_associate(p->lex.lastline, p->enc);
            }
#endif
            p->lex.pbeg = p->lex.pcur;
            return;
        }
        break;
      case EOF:
        return;
    }
    pushback(p, c);
    p->enc = rb_enc_get(p->lex.lastline);
}

#ifndef RIPPER
#define ambiguous_operator(tok, op, syn) ( \
    rb_warning0("`"op"' after local variable or literal is interpreted as binary operator"), \
    rb_warning0("even though it seems like "syn""))
#else
#define ambiguous_operator(tok, op, syn) \
    dispatch2(operator_ambiguous, TOKEN2VAL(tok), rb_str_new_cstr(syn))
#endif
#define warn_balanced(tok, op, syn) ((void) \
    (!IS_lex_state_for(last_state, EXPR_CLASS|EXPR_DOT|EXPR_FNAME|EXPR_ENDFN) && \
     space_seen && !ISSPACE(c) && \
     (ambiguous_operator(tok, op, syn), 0)), \
     (enum yytokentype)(tok))

static VALUE
parse_rational(struct parser_params *p, char *str, int len, int seen_point)
{
    VALUE v;
    char *point = &str[seen_point];
    size_t fraclen = len-seen_point-1;
    memmove(point, point+1, fraclen+1);
    v = rb_cstr_to_inum(str, 10, FALSE);
    return rb_rational_new(v, rb_int_positive_pow(10, fraclen));
}

static enum yytokentype
no_digits(struct parser_params *p)
{
    yyerror0("numeric literal without digits");
    if (peek(p, '_')) nextc(p);
    /* dummy 0, for tUMINUS_NUM at numeric */
    return set_integer_literal(p, INT2FIX(0), 0);
}

static enum yytokentype
parse_numeric(struct parser_params *p, int c)
{
    int is_float, seen_point, seen_e, nondigit;
    int suffix;

    is_float = seen_point = seen_e = nondigit = 0;
    SET_LEX_STATE(EXPR_END);
    newtok(p);
    if (c == '-' || c == '+') {
        tokadd(p, c);
        c = nextc(p);
    }
    if (c == '0') {
        int start = toklen(p);
        c = nextc(p);
        if (c == 'x' || c == 'X') {
            /* hexadecimal */
            c = nextc(p);
            if (c != -1 && ISXDIGIT(c)) {
                do {
                    if (c == '_') {
                        if (nondigit) break;
                        nondigit = c;
                        continue;
                    }
                    if (!ISXDIGIT(c)) break;
                    nondigit = 0;
                    tokadd(p, c);
                } while ((c = nextc(p)) != -1);
            }
            pushback(p, c);
            tokfix(p);
            if (toklen(p) == start) {
                return no_digits(p);
            }
            else if (nondigit) goto trailing_uc;
            suffix = number_literal_suffix(p, NUM_SUFFIX_ALL);
            return set_integer_literal(p, rb_cstr_to_inum(tok(p), 16, FALSE), suffix);
        }
        if (c == 'b' || c == 'B') {
            /* binary */
            c = nextc(p);
            if (c == '0' || c == '1') {
                do {
                    if (c == '_') {
                        if (nondigit) break;
                        nondigit = c;
                        continue;
                    }
                    if (c != '0' && c != '1') break;
                    nondigit = 0;
                    tokadd(p, c);
                } while ((c = nextc(p)) != -1);
            }
            pushback(p, c);
            tokfix(p);
            if (toklen(p) == start) {
                return no_digits(p);
            }
            else if (nondigit) goto trailing_uc;
            suffix = number_literal_suffix(p, NUM_SUFFIX_ALL);
            return set_integer_literal(p, rb_cstr_to_inum(tok(p), 2, FALSE), suffix);
        }
        if (c == 'd' || c == 'D') {
            /* decimal */
            c = nextc(p);
            if (c != -1 && ISDIGIT(c)) {
                do {
                    if (c == '_') {
                        if (nondigit) break;
                        nondigit = c;
                        continue;
                    }
                    if (!ISDIGIT(c)) break;
                    nondigit = 0;
                    tokadd(p, c);
                } while ((c = nextc(p)) != -1);
            }
            pushback(p, c);
            tokfix(p);
            if (toklen(p) == start) {
                return no_digits(p);
            }
            else if (nondigit) goto trailing_uc;
            suffix = number_literal_suffix(p, NUM_SUFFIX_ALL);
            return set_integer_literal(p, rb_cstr_to_inum(tok(p), 10, FALSE), suffix);
        }
        if (c == '_') {
            /* 0_0 */
            goto octal_number;
        }
        if (c == 'o' || c == 'O') {
            /* prefixed octal */
            c = nextc(p);
            if (c == -1 || c == '_' || !ISDIGIT(c)) {
                return no_digits(p);
            }
        }
        if (c >= '0' && c <= '7') {
            /* octal */
          octal_number:
            do {
                if (c == '_') {
                    if (nondigit) break;
                    nondigit = c;
                    continue;
                }
                if (c < '0' || c > '9') break;
                if (c > '7') goto invalid_octal;
                nondigit = 0;
                tokadd(p, c);
            } while ((c = nextc(p)) != -1);
            if (toklen(p) > start) {
                pushback(p, c);
                tokfix(p);
                if (nondigit) goto trailing_uc;
                suffix = number_literal_suffix(p, NUM_SUFFIX_ALL);
                return set_integer_literal(p, rb_cstr_to_inum(tok(p), 8, FALSE), suffix);
            }
            if (nondigit) {
                pushback(p, c);
                goto trailing_uc;
            }
        }
        if (c > '7' && c <= '9') {
          invalid_octal:
            yyerror0("Invalid octal digit");
        }
        else if (c == '.' || c == 'e' || c == 'E') {
            tokadd(p, '0');
        }
        else {
            pushback(p, c);
            suffix = number_literal_suffix(p, NUM_SUFFIX_ALL);
            return set_integer_literal(p, INT2FIX(0), suffix);
        }
    }

    for (;;) {
        switch (c) {
          case '0': case '1': case '2': case '3': case '4':
          case '5': case '6': case '7': case '8': case '9':
            nondigit = 0;
            tokadd(p, c);
            break;

          case '.':
            if (nondigit) goto trailing_uc;
            if (seen_point || seen_e) {
                goto decode_num;
            }
            else {
                int c0 = nextc(p);
                if (c0 == -1 || !ISDIGIT(c0)) {
                    pushback(p, c0);
                    goto decode_num;
                }
                c = c0;
            }
            seen_point = toklen(p);
            tokadd(p, '.');
            tokadd(p, c);
            is_float++;
            nondigit = 0;
            break;

          case 'e':
          case 'E':
            if (nondigit) {
                pushback(p, c);
                c = nondigit;
                goto decode_num;
            }
            if (seen_e) {
                goto decode_num;
            }
            nondigit = c;
            c = nextc(p);
            if (c != '-' && c != '+' && !ISDIGIT(c)) {
                pushback(p, c);
                nondigit = 0;
                goto decode_num;
            }
            tokadd(p, nondigit);
            seen_e++;
            is_float++;
            tokadd(p, c);
            nondigit = (c == '-' || c == '+') ? c : 0;
            break;

          case '_':	/* `_' in number just ignored */
            if (nondigit) goto decode_num;
            nondigit = c;
            break;

          default:
            goto decode_num;
        }
        c = nextc(p);
    }

  decode_num:
    pushback(p, c);
    if (nondigit) {
      trailing_uc:
        literal_flush(p, p->lex.pcur - 1);
        YYLTYPE loc = RUBY_INIT_YYLLOC();
        compile_error(p, "trailing `%c' in number", nondigit);
        parser_show_error_line(p, &loc);
    }
    tokfix(p);
    if (is_float) {
        enum yytokentype type = tFLOAT;
        VALUE v;

        suffix = number_literal_suffix(p, seen_e ? NUM_SUFFIX_I : NUM_SUFFIX_ALL);
        if (suffix & NUM_SUFFIX_R) {
            type = tRATIONAL;
            v = parse_rational(p, tok(p), toklen(p), seen_point);
        }
        else {
            double d = strtod(tok(p), 0);
            if (errno == ERANGE) {
                rb_warning1("Float %s out of range", WARN_S(tok(p)));
                errno = 0;
            }
            v = DBL2NUM(d);
        }
        return set_number_literal(p, v, type, suffix);
    }
    suffix = number_literal_suffix(p, NUM_SUFFIX_ALL);
    return set_integer_literal(p, rb_cstr_to_inum(tok(p), 10, FALSE), suffix);
}

static enum yytokentype
parse_qmark(struct parser_params *p, int space_seen)
{
    rb_encoding *enc;
    register int c;
    VALUE lit;

    if (IS_END()) {
        SET_LEX_STATE(EXPR_VALUE);
        return '?';
    }
    c = nextc(p);
    if (c == -1) {
        compile_error(p, "incomplete character syntax");
        return 0;
    }
    if (rb_enc_isspace(c, p->enc)) {
        if (!IS_ARG()) {
            int c2 = escaped_control_code(c);
            if (c2) {
                WARN_SPACE_CHAR(c2, "?");
            }
        }
      ternary:
        pushback(p, c);
        SET_LEX_STATE(EXPR_VALUE);
        return '?';
    }
    newtok(p);
    enc = p->enc;
    if (!parser_isascii(p)) {
        if (tokadd_mbchar(p, c) == -1) return 0;
    }
    else if ((rb_enc_isalnum(c, p->enc) || c == '_') &&
             p->lex.pcur < p->lex.pend && is_identchar(p->lex.pcur, p->lex.pend, p->enc)) {
        if (space_seen) {
            const char *start = p->lex.pcur - 1, *ptr = start;
            do {
                int n = parser_precise_mbclen(p, ptr);
                if (n < 0) return -1;
                ptr += n;
            } while (ptr < p->lex.pend && is_identchar(ptr, p->lex.pend, p->enc));
            rb_warn2("`?' just followed by `%.*s' is interpreted as" \
                     " a conditional operator, put a space after `?'",
                     WARN_I((int)(ptr - start)), WARN_S_L(start, (ptr - start)));
        }
        goto ternary;
    }
    else if (c == '\\') {
        if (peek(p, 'u')) {
            nextc(p);
            enc = rb_utf8_encoding();
            tokadd_utf8(p, &enc, -1, 0, 0);
        }
        else if (!lex_eol_p(p) && !(c = *p->lex.pcur, ISASCII(c))) {
            nextc(p);
            if (tokadd_mbchar(p, c) == -1) return 0;
        }
        else {
            c = read_escape(p, 0, &enc);
            tokadd(p, c);
        }
    }
    else {
        tokadd(p, c);
    }
    tokfix(p);
    lit = STR_NEW3(tok(p), toklen(p), enc, 0);
    set_yylval_str(lit);
    SET_LEX_STATE(EXPR_END);
    return tCHAR;
}

static enum yytokentype
parse_percent(struct parser_params *p, const int space_seen, const enum lex_state_e last_state)
{
    register int c;
    const char *ptok = p->lex.pcur;

    if (IS_BEG()) {
        int term;
        int paren;

        c = nextc(p);
      quotation:
        if (c == -1) goto unterminated;
        if (!ISALNUM(c)) {
            term = c;
            if (!ISASCII(c)) goto unknown;
            c = 'Q';
        }
        else {
            term = nextc(p);
            if (rb_enc_isalnum(term, p->enc) || !parser_isascii(p)) {
              unknown:
                pushback(p, term);
                c = parser_precise_mbclen(p, p->lex.pcur);
                if (c < 0) return 0;
                p->lex.pcur += c;
                yyerror0("unknown type of %string");
                return 0;
            }
        }
        if (term == -1) {
          unterminated:
            compile_error(p, "unterminated quoted string meets end of file");
            return 0;
        }
        paren = term;
        if (term == '(') term = ')';
        else if (term == '[') term = ']';
        else if (term == '{') term = '}';
        else if (term == '<') term = '>';
        else paren = 0;

        p->lex.ptok = ptok-1;
        switch (c) {
          case 'Q':
            p->lex.strterm = NEW_STRTERM(str_dquote, term, paren);
            return tSTRING_BEG;

          case 'q':
            p->lex.strterm = NEW_STRTERM(str_squote, term, paren);
            return tSTRING_BEG;

          case 'W':
            p->lex.strterm = NEW_STRTERM(str_dword, term, paren);
            return tWORDS_BEG;

          case 'w':
            p->lex.strterm = NEW_STRTERM(str_sword, term, paren);
            return tQWORDS_BEG;

          case 'I':
            p->lex.strterm = NEW_STRTERM(str_dword, term, paren);
            return tSYMBOLS_BEG;

          case 'i':
            p->lex.strterm = NEW_STRTERM(str_sword, term, paren);
            return tQSYMBOLS_BEG;

          case 'x':
            p->lex.strterm = NEW_STRTERM(str_xquote, term, paren);
            return tXSTRING_BEG;

          case 'r':
            p->lex.strterm = NEW_STRTERM(str_regexp, term, paren);
            return tREGEXP_BEG;

          case 's':
            p->lex.strterm = NEW_STRTERM(str_ssym, term, paren);
            SET_LEX_STATE(EXPR_FNAME|EXPR_FITEM);
            return tSYMBEG;

          default:
            yyerror0("unknown type of %string");
            return 0;
        }
    }
    if ((c = nextc(p)) == '=') {
        set_yylval_id('%');
        SET_LEX_STATE(EXPR_BEG);
        return tOP_ASGN;
    }
    if (IS_SPCARG(c) || (IS_lex_state(EXPR_FITEM) && c == 's')) {
        goto quotation;
    }
    SET_LEX_STATE(IS_AFTER_OPERATOR() ? EXPR_ARG : EXPR_BEG);
    pushback(p, c);
    return warn_balanced('%', "%%", "string literal");
}

static int
tokadd_ident(struct parser_params *p, int c)
{
    do {
        if (tokadd_mbchar(p, c) == -1) return -1;
        c = nextc(p);
    } while (parser_is_identchar(p));
    pushback(p, c);
    return 0;
}

static ID
tokenize_ident(struct parser_params *p)
{
    ID ident = TOK_INTERN();

    set_yylval_name(ident);

    return ident;
}

static int
parse_numvar(struct parser_params *p)
{
    size_t len;
    int overflow;
    unsigned long n = ruby_scan_digits(tok(p)+1, toklen(p)-1, 10, &len, &overflow);
    const unsigned long nth_ref_max =
        ((FIXNUM_MAX < INT_MAX) ? FIXNUM_MAX : INT_MAX) >> 1;
    /* NTH_REF is left-shifted to be ORed with back-ref flag and
     * turned into a Fixnum, in compile.c */

    if (overflow || n > nth_ref_max) {
        /* compile_error()? */
        rb_warn1("`%s' is too big for a number variable, always nil", WARN_S(tok(p)));
        return 0;		/* $0 is $PROGRAM_NAME, not NTH_REF */
    }
    else {
        return (int)n;
    }
}

static enum yytokentype
parse_gvar(struct parser_params *p, const enum lex_state_e last_state)
{
    const char *ptr = p->lex.pcur;
    register int c;

    SET_LEX_STATE(EXPR_END);
    p->lex.ptok = ptr - 1; /* from '$' */
    newtok(p);
    c = nextc(p);
    switch (c) {
      case '_':		/* $_: last read line string */
        c = nextc(p);
        if (parser_is_identchar(p)) {
            tokadd(p, '$');
            tokadd(p, '_');
            break;
        }
        pushback(p, c);
        c = '_';
        /* fall through */
      case '~':		/* $~: match-data */
      case '*':		/* $*: argv */
      case '$':		/* $$: pid */
      case '?':		/* $?: last status */
      case '!':		/* $!: error string */
      case '@':		/* $@: error position */
      case '/':		/* $/: input record separator */
      case '\\':		/* $\: output record separator */
      case ';':		/* $;: field separator */
      case ',':		/* $,: output field separator */
      case '.':		/* $.: last read line number */
      case '=':		/* $=: ignorecase */
      case ':':		/* $:: load path */
      case '<':		/* $<: reading filename */
      case '>':		/* $>: default output handle */
      case '\"':		/* $": already loaded files */
        tokadd(p, '$');
        tokadd(p, c);
        goto gvar;

      case '-':
        tokadd(p, '$');
        tokadd(p, c);
        c = nextc(p);
        if (parser_is_identchar(p)) {
            if (tokadd_mbchar(p, c) == -1) return 0;
        }
        else {
            pushback(p, c);
            pushback(p, '-');
            return '$';
        }
      gvar:
        set_yylval_name(TOK_INTERN());
        return tGVAR;

      case '&':		/* $&: last match */
      case '`':		/* $`: string before last match */
      case '\'':		/* $': string after last match */
      case '+':		/* $+: string matches last paren. */
        if (IS_lex_state_for(last_state, EXPR_FNAME)) {
            tokadd(p, '$');
            tokadd(p, c);
            goto gvar;
        }
        set_yylval_node(NEW_BACK_REF(c, &_cur_loc));
        return tBACK_REF;

      case '1': case '2': case '3':
      case '4': case '5': case '6':
      case '7': case '8': case '9':
        tokadd(p, '$');
        do {
            tokadd(p, c);
            c = nextc(p);
        } while (c != -1 && ISDIGIT(c));
        pushback(p, c);
        if (IS_lex_state_for(last_state, EXPR_FNAME)) goto gvar;
        tokfix(p);
        c = parse_numvar(p);
        set_yylval_node(NEW_NTH_REF(c, &_cur_loc));
        return tNTH_REF;

      default:
        if (!parser_is_identchar(p)) {
            YYLTYPE loc = RUBY_INIT_YYLLOC();
            if (c == -1 || ISSPACE(c)) {
                compile_error(p, "`$' without identifiers is not allowed as a global variable name");
            }
            else {
                pushback(p, c);
                compile_error(p, "`$%c' is not allowed as a global variable name", c);
            }
            parser_show_error_line(p, &loc);
            set_yylval_noname();
            return tGVAR;
        }
        /* fall through */
      case '0':
        tokadd(p, '$');
    }

    if (tokadd_ident(p, c)) return 0;
    SET_LEX_STATE(EXPR_END);
    tokenize_ident(p);
    return tGVAR;
}

#ifndef RIPPER
static bool
parser_numbered_param(struct parser_params *p, int n)
{
    if (n < 0) return false;

    if (DVARS_TERMINAL_P(p->lvtbl->args) || DVARS_TERMINAL_P(p->lvtbl->args->prev)) {
        return false;
    }
    if (p->max_numparam == ORDINAL_PARAM) {
        compile_error(p, "ordinary parameter is defined");
        return false;
    }
    struct vtable *args = p->lvtbl->args;
    if (p->max_numparam < n) {
        p->max_numparam = n;
    }
    while (n > args->pos) {
        vtable_add(args, NUMPARAM_IDX_TO_ID(args->pos+1));
    }
    return true;
}
#endif

static enum yytokentype
parse_atmark(struct parser_params *p, const enum lex_state_e last_state)
{
    const char *ptr = p->lex.pcur;
    enum yytokentype result = tIVAR;
    register int c = nextc(p);
    YYLTYPE loc;

    p->lex.ptok = ptr - 1; /* from '@' */
    newtok(p);
    tokadd(p, '@');
    if (c == '@') {
        result = tCVAR;
        tokadd(p, '@');
        c = nextc(p);
    }
    SET_LEX_STATE(IS_lex_state_for(last_state, EXPR_FNAME) ? EXPR_ENDFN : EXPR_END);
    if (c == -1 || !parser_is_identchar(p)) {
        pushback(p, c);
        RUBY_SET_YYLLOC(loc);
        if (result == tIVAR) {
            compile_error(p, "`@' without identifiers is not allowed as an instance variable name");
        }
        else {
            compile_error(p, "`@@' without identifiers is not allowed as a class variable name");
        }
        parser_show_error_line(p, &loc);
        set_yylval_noname();
        SET_LEX_STATE(EXPR_END);
        return result;
    }
    else if (ISDIGIT(c)) {
        pushback(p, c);
        RUBY_SET_YYLLOC(loc);
        if (result == tIVAR) {
            compile_error(p, "`@%c' is not allowed as an instance variable name", c);
        }
        else {
            compile_error(p, "`@@%c' is not allowed as a class variable name", c);
        }
        parser_show_error_line(p, &loc);
        set_yylval_noname();
        SET_LEX_STATE(EXPR_END);
        return result;
    }

    if (tokadd_ident(p, c)) return 0;
    tokenize_ident(p);
    return result;
}

static enum yytokentype
parse_ident(struct parser_params *p, int c, int cmd_state)
{
    enum yytokentype result;
    int mb = ENC_CODERANGE_7BIT;
    const enum lex_state_e last_state = p->lex.state;
    ID ident;
    int enforce_keyword_end = 0;

    do {
        if (!ISASCII(c)) mb = ENC_CODERANGE_UNKNOWN;
        if (tokadd_mbchar(p, c) == -1) return 0;
        c = nextc(p);
    } while (parser_is_identchar(p));
    if ((c == '!' || c == '?') && !peek(p, '=')) {
        result = tFID;
        tokadd(p, c);
    }
    else if (c == '=' && IS_lex_state(EXPR_FNAME) &&
             (!peek(p, '~') && !peek(p, '>') && (!peek(p, '=') || (peek_n(p, '>', 1))))) {
        result = tIDENTIFIER;
        tokadd(p, c);
    }
    else {
        result = tCONSTANT;	/* assume provisionally */
        pushback(p, c);
    }
    tokfix(p);

    if (IS_LABEL_POSSIBLE()) {
        if (IS_LABEL_SUFFIX(0)) {
            SET_LEX_STATE(EXPR_ARG|EXPR_LABELED);
            nextc(p);
            set_yylval_name(TOK_INTERN());
            return tLABEL;
        }
    }

#ifndef RIPPER
    if (!NIL_P(peek_end_expect_token_locations(p))) {
        VALUE end_loc;
        int lineno, column;
        int beg_pos = (int)(p->lex.ptok - p->lex.pbeg);

        end_loc = peek_end_expect_token_locations(p);
        lineno = NUM2INT(rb_ary_entry(end_loc, 0));
        column = NUM2INT(rb_ary_entry(end_loc, 1));

        if (p->debug) {
            rb_parser_printf(p, "enforce_keyword_end check. current: (%d, %d), peek: (%d, %d)\n",
                                p->ruby_sourceline, beg_pos, lineno, column);
        }

        if ((p->ruby_sourceline > lineno) && (beg_pos <= column)) {
            const struct kwtable *kw;

            if ((IS_lex_state(EXPR_DOT)) && (kw = rb_reserved_word(tok(p), toklen(p))) && (kw && kw->id[0] == keyword_end)) {
                if (p->debug) rb_parser_printf(p, "enforce_keyword_end is enabled\n");
                enforce_keyword_end = 1;
            }
        }
    }
#endif

    if (mb == ENC_CODERANGE_7BIT && (!IS_lex_state(EXPR_DOT) || enforce_keyword_end)) {
        const struct kwtable *kw;

        /* See if it is a reserved word.  */
        kw = rb_reserved_word(tok(p), toklen(p));
        if (kw) {
            enum lex_state_e state = p->lex.state;
            if (IS_lex_state_for(state, EXPR_FNAME)) {
                SET_LEX_STATE(EXPR_ENDFN);
                set_yylval_name(rb_intern2(tok(p), toklen(p)));
                return kw->id[0];
            }
            SET_LEX_STATE(kw->state);
            if (IS_lex_state(EXPR_BEG)) {
                p->command_start = TRUE;
            }
            if (kw->id[0] == keyword_do) {
                if (lambda_beginning_p()) {
                    p->lex.lpar_beg = -1; /* make lambda_beginning_p() == FALSE in the body of "-> do ... end" */
                    return keyword_do_LAMBDA;
                }
                if (COND_P()) return keyword_do_cond;
                if (CMDARG_P() && !IS_lex_state_for(state, EXPR_CMDARG))
                    return keyword_do_block;
                return keyword_do;
            }
            if (IS_lex_state_for(state, (EXPR_BEG | EXPR_LABELED | EXPR_CLASS)))
                return kw->id[0];
            else {
                if (kw->id[0] != kw->id[1])
                    SET_LEX_STATE(EXPR_BEG | EXPR_LABEL);
                return kw->id[1];
            }
        }
    }

    if (IS_lex_state(EXPR_BEG_ANY | EXPR_ARG_ANY | EXPR_DOT)) {
        if (cmd_state) {
            SET_LEX_STATE(EXPR_CMDARG);
        }
        else {
            SET_LEX_STATE(EXPR_ARG);
        }
    }
    else if (p->lex.state == EXPR_FNAME) {
        SET_LEX_STATE(EXPR_ENDFN);
    }
    else {
        SET_LEX_STATE(EXPR_END);
    }

    ident = tokenize_ident(p);
    if (result == tCONSTANT && is_local_id(ident)) result = tIDENTIFIER;
    if (!IS_lex_state_for(last_state, EXPR_DOT|EXPR_FNAME) &&
        (result == tIDENTIFIER) && /* not EXPR_FNAME, not attrasgn */
        lvar_defined(p, ident)) {
        SET_LEX_STATE(EXPR_END|EXPR_LABEL);
    }
    return result;
}

static void
warn_cr(struct parser_params *p)
{
    if (!p->cr_seen) {
        p->cr_seen = TRUE;
        /* carried over with p->lex.nextline for nextc() */
        rb_warn0("encountered \\r in middle of line, treated as a mere space");
    }
}

static enum yytokentype
parser_yylex(struct parser_params *p)
{
    register int c;
    int space_seen = 0;
    int cmd_state;
    int label;
    enum lex_state_e last_state;
    int fallthru = FALSE;
    int token_seen = p->token_seen;

    if (p->lex.strterm) {
        if (p->lex.strterm->flags & STRTERM_HEREDOC) {
            token_flush(p);
            return here_document(p, &p->lex.strterm->u.heredoc);
        }
        else {
            token_flush(p);
            return parse_string(p, &p->lex.strterm->u.literal);
        }
    }
    cmd_state = p->command_start;
    p->command_start = FALSE;
    p->token_seen = TRUE;
#ifndef RIPPER
    token_flush(p);
#endif
  retry:
    last_state = p->lex.state;
    switch (c = nextc(p)) {
      case '\0':		/* NUL */
      case '\004':		/* ^D */
      case '\032':		/* ^Z */
      case -1:			/* end of script. */
        p->eofp  = 1;
#ifndef RIPPER
        if (!NIL_P(p->end_expect_token_locations) && RARRAY_LEN(p->end_expect_token_locations) > 0) {
            pop_end_expect_token_locations(p);
            RUBY_SET_YYLLOC_OF_DUMMY_END(*p->yylloc);
            return tDUMNY_END;
        }
#endif
        /* Set location for end-of-input because dispatch_scan_event is not called. */
        RUBY_SET_YYLLOC(*p->yylloc);
        return END_OF_INPUT;

        /* white spaces */
      case '\r':
        warn_cr(p);
        /* fall through */
      case ' ': case '\t': case '\f':
      case '\13': /* '\v' */
        space_seen = 1;
        while ((c = nextc(p))) {
            switch (c) {
              case '\r':
                warn_cr(p);
                /* fall through */
              case ' ': case '\t': case '\f':
              case '\13': /* '\v' */
                break;
              default:
                goto outofloop;
            }
        }
      outofloop:
        pushback(p, c);
        dispatch_scan_event(p, tSP);
#ifndef RIPPER
        token_flush(p);
#endif
        goto retry;

      case '#':		/* it's a comment */
        p->token_seen = token_seen;
        /* no magic_comment in shebang line */
        if (!parser_magic_comment(p, p->lex.pcur, p->lex.pend - p->lex.pcur)) {
            if (comment_at_top(p)) {
                set_file_encoding(p, p->lex.pcur, p->lex.pend);
            }
        }
        lex_goto_eol(p);
        dispatch_scan_event(p, tCOMMENT);
        fallthru = TRUE;
        /* fall through */
      case '\n':
        p->token_seen = token_seen;
        VALUE prevline = p->lex.lastline;
        c = (IS_lex_state(EXPR_BEG|EXPR_CLASS|EXPR_FNAME|EXPR_DOT) &&
             !IS_lex_state(EXPR_LABELED));
        if (c || IS_lex_state_all(EXPR_ARG|EXPR_LABELED)) {
            if (!fallthru) {
                dispatch_scan_event(p, tIGNORED_NL);
            }
            fallthru = FALSE;
            if (!c && p->ctxt.in_kwarg) {
                goto normal_newline;
            }
            goto retry;
        }
        while (1) {
            switch (c = nextc(p)) {
              case ' ': case '\t': case '\f': case '\r':
              case '\13': /* '\v' */
                space_seen = 1;
                break;
              case '#':
                pushback(p, c);
                if (space_seen) {
                    dispatch_scan_event(p, tSP);
                    token_flush(p);
                }
                goto retry;
              case '&':
              case '.': {
                dispatch_delayed_token(p, tIGNORED_NL);
                if (peek(p, '.') == (c == '&')) {
                    pushback(p, c);
                    dispatch_scan_event(p, tSP);
                    goto retry;
                }
              }
              default:
                p->ruby_sourceline--;
                p->lex.nextline = p->lex.lastline;
                set_lastline(p, prevline);
              case -1:		/* EOF no decrement*/
                lex_goto_eol(p);
                if (c != -1) {
                    token_flush(p);
                    RUBY_SET_YYLLOC(*p->yylloc);
                }
                goto normal_newline;
            }
        }
      normal_newline:
        p->command_start = TRUE;
        SET_LEX_STATE(EXPR_BEG);
        return '\n';

      case '*':
        if ((c = nextc(p)) == '*') {
            if ((c = nextc(p)) == '=') {
                set_yylval_id(idPow);
                SET_LEX_STATE(EXPR_BEG);
                return tOP_ASGN;
            }
            pushback(p, c);
            if (IS_SPCARG(c)) {
                rb_warning0("`**' interpreted as argument prefix");
                c = tDSTAR;
            }
            else if (IS_BEG()) {
                c = tDSTAR;
            }
            else {
                c = warn_balanced((enum ruby_method_ids)tPOW, "**", "argument prefix");
            }
        }
        else {
            if (c == '=') {
                set_yylval_id('*');
                SET_LEX_STATE(EXPR_BEG);
                return tOP_ASGN;
            }
            pushback(p, c);
            if (IS_SPCARG(c)) {
                rb_warning0("`*' interpreted as argument prefix");
                c = tSTAR;
            }
            else if (IS_BEG()) {
                c = tSTAR;
            }
            else {
                c = warn_balanced('*', "*", "argument prefix");
            }
        }
        SET_LEX_STATE(IS_AFTER_OPERATOR() ? EXPR_ARG : EXPR_BEG);
        return c;

      case '!':
        c = nextc(p);
        if (IS_AFTER_OPERATOR()) {
            SET_LEX_STATE(EXPR_ARG);
            if (c == '@') {
                return '!';
            }
        }
        else {
            SET_LEX_STATE(EXPR_BEG);
        }
        if (c == '=') {
            return tNEQ;
        }
        if (c == '~') {
            return tNMATCH;
        }
        pushback(p, c);
        return '!';

      case '=':
        if (was_bol(p)) {
            /* skip embedded rd document */
            if (word_match_p(p, "begin", 5)) {
                int first_p = TRUE;

                lex_goto_eol(p);
                dispatch_scan_event(p, tEMBDOC_BEG);
                for (;;) {
                    lex_goto_eol(p);
                    if (!first_p) {
                        dispatch_scan_event(p, tEMBDOC);
                    }
                    first_p = FALSE;
                    c = nextc(p);
                    if (c == -1) {
                        compile_error(p, "embedded document meets end of file");
                        return END_OF_INPUT;
                    }
                    if (c == '=' && word_match_p(p, "end", 3)) {
                        break;
                    }
                    pushback(p, c);
                }
                lex_goto_eol(p);
                dispatch_scan_event(p, tEMBDOC_END);
                goto retry;
            }
        }

        SET_LEX_STATE(IS_AFTER_OPERATOR() ? EXPR_ARG : EXPR_BEG);
        if ((c = nextc(p)) == '=') {
            if ((c = nextc(p)) == '=') {
                return tEQQ;
            }
            pushback(p, c);
            return tEQ;
        }
        if (c == '~') {
            return tMATCH;
        }
        else if (c == '>') {
            return tASSOC;
        }
        pushback(p, c);
        return '=';

      case '<':
        c = nextc(p);
        if (c == '<' &&
            !IS_lex_state(EXPR_DOT | EXPR_CLASS) &&
            !IS_END() &&
            (!IS_ARG() || IS_lex_state(EXPR_LABELED) || space_seen)) {
            int token = heredoc_identifier(p);
            if (token) return token < 0 ? 0 : token;
        }
        if (IS_AFTER_OPERATOR()) {
            SET_LEX_STATE(EXPR_ARG);
        }
        else {
            if (IS_lex_state(EXPR_CLASS))
                p->command_start = TRUE;
            SET_LEX_STATE(EXPR_BEG);
        }
        if (c == '=') {
            if ((c = nextc(p)) == '>') {
                return tCMP;
            }
            pushback(p, c);
            return tLEQ;
        }
        if (c == '<') {
            if ((c = nextc(p)) == '=') {
                set_yylval_id(idLTLT);
                SET_LEX_STATE(EXPR_BEG);
                return tOP_ASGN;
            }
            pushback(p, c);
            return warn_balanced((enum ruby_method_ids)tLSHFT, "<<", "here document");
        }
        pushback(p, c);
        return '<';

      case '>':
        SET_LEX_STATE(IS_AFTER_OPERATOR() ? EXPR_ARG : EXPR_BEG);
        if ((c = nextc(p)) == '=') {
            return tGEQ;
        }
        if (c == '>') {
            if ((c = nextc(p)) == '=') {
                set_yylval_id(idGTGT);
                SET_LEX_STATE(EXPR_BEG);
                return tOP_ASGN;
            }
            pushback(p, c);
            return tRSHFT;
        }
        pushback(p, c);
        return '>';

      case '"':
        label = (IS_LABEL_POSSIBLE() ? str_label : 0);
        p->lex.strterm = NEW_STRTERM(str_dquote | label, '"', 0);
        p->lex.ptok = p->lex.pcur-1;
        return tSTRING_BEG;

      case '`':
        if (IS_lex_state(EXPR_FNAME)) {
            SET_LEX_STATE(EXPR_ENDFN);
            return c;
        }
        if (IS_lex_state(EXPR_DOT)) {
            if (cmd_state)
                SET_LEX_STATE(EXPR_CMDARG);
            else
                SET_LEX_STATE(EXPR_ARG);
            return c;
        }
        p->lex.strterm = NEW_STRTERM(str_xquote, '`', 0);
        return tXSTRING_BEG;

      case '\'':
        label = (IS_LABEL_POSSIBLE() ? str_label : 0);
        p->lex.strterm = NEW_STRTERM(str_squote | label, '\'', 0);
        p->lex.ptok = p->lex.pcur-1;
        return tSTRING_BEG;

      case '?':
        return parse_qmark(p, space_seen);

      case '&':
        if ((c = nextc(p)) == '&') {
            SET_LEX_STATE(EXPR_BEG);
            if ((c = nextc(p)) == '=') {
                set_yylval_id(idANDOP);
                SET_LEX_STATE(EXPR_BEG);
                return tOP_ASGN;
            }
            pushback(p, c);
            return tANDOP;
        }
        else if (c == '=') {
            set_yylval_id('&');
            SET_LEX_STATE(EXPR_BEG);
            return tOP_ASGN;
        }
        else if (c == '.') {
            set_yylval_id(idANDDOT);
            SET_LEX_STATE(EXPR_DOT);
            return tANDDOT;
        }
        pushback(p, c);
        if (IS_SPCARG(c)) {
            if ((c != ':') ||
                (c = peekc_n(p, 1)) == -1 ||
                !(c == '\'' || c == '"' ||
                  is_identchar((p->lex.pcur+1), p->lex.pend, p->enc))) {
                rb_warning0("`&' interpreted as argument prefix");
            }
            c = tAMPER;
        }
        else if (IS_BEG()) {
            c = tAMPER;
        }
        else {
            c = warn_balanced('&', "&", "argument prefix");
        }
        SET_LEX_STATE(IS_AFTER_OPERATOR() ? EXPR_ARG : EXPR_BEG);
        return c;

      case '|':
        if ((c = nextc(p)) == '|') {
            SET_LEX_STATE(EXPR_BEG);
            if ((c = nextc(p)) == '=') {
                set_yylval_id(idOROP);
                SET_LEX_STATE(EXPR_BEG);
                return tOP_ASGN;
            }
            pushback(p, c);
            if (IS_lex_state_for(last_state, EXPR_BEG)) {
                c = '|';
                pushback(p, '|');
                return c;
            }
            return tOROP;
        }
        if (c == '=') {
            set_yylval_id('|');
            SET_LEX_STATE(EXPR_BEG);
            return tOP_ASGN;
        }
        SET_LEX_STATE(IS_AFTER_OPERATOR() ? EXPR_ARG : EXPR_BEG|EXPR_LABEL);
        pushback(p, c);
        return '|';

      case '+':
        c = nextc(p);
        if (IS_AFTER_OPERATOR()) {
            SET_LEX_STATE(EXPR_ARG);
            if (c == '@') {
                return tUPLUS;
            }
            pushback(p, c);
            return '+';
        }
        if (c == '=') {
            set_yylval_id('+');
            SET_LEX_STATE(EXPR_BEG);
            return tOP_ASGN;
        }
        if (IS_BEG() || (IS_SPCARG(c) && arg_ambiguous(p, '+'))) {
            SET_LEX_STATE(EXPR_BEG);
            pushback(p, c);
            if (c != -1 && ISDIGIT(c)) {
                return parse_numeric(p, '+');
            }
            return tUPLUS;
        }
        SET_LEX_STATE(EXPR_BEG);
        pushback(p, c);
        return warn_balanced('+', "+", "unary operator");

      case '-':
        c = nextc(p);
        if (IS_AFTER_OPERATOR()) {
            SET_LEX_STATE(EXPR_ARG);
            if (c == '@') {
                return tUMINUS;
            }
            pushback(p, c);
            return '-';
        }
        if (c == '=') {
            set_yylval_id('-');
            SET_LEX_STATE(EXPR_BEG);
            return tOP_ASGN;
        }
        if (c == '>') {
            SET_LEX_STATE(EXPR_ENDFN);
            return tLAMBDA;
        }
        if (IS_BEG() || (IS_SPCARG(c) && arg_ambiguous(p, '-'))) {
            SET_LEX_STATE(EXPR_BEG);
            pushback(p, c);
            if (c != -1 && ISDIGIT(c)) {
                return tUMINUS_NUM;
            }
            return tUMINUS;
        }
        SET_LEX_STATE(EXPR_BEG);
        pushback(p, c);
        return warn_balanced('-', "-", "unary operator");

      case '.': {
        int is_beg = IS_BEG();
        SET_LEX_STATE(EXPR_BEG);
        if ((c = nextc(p)) == '.') {
            if ((c = nextc(p)) == '.') {
                if (p->ctxt.in_argdef) {
                    SET_LEX_STATE(EXPR_ENDARG);
                    return tBDOT3;
                }
                if (p->lex.paren_nest == 0 && looking_at_eol_p(p)) {
                    rb_warn0("... at EOL, should be parenthesized?");
                }
                else if (p->lex.lpar_beg >= 0 && p->lex.lpar_beg+1 == p->lex.paren_nest) {
                    if (IS_lex_state_for(last_state, EXPR_LABEL))
                        return tDOT3;
                }
                return is_beg ? tBDOT3 : tDOT3;
            }
            pushback(p, c);
            return is_beg ? tBDOT2 : tDOT2;
        }
        pushback(p, c);
        if (c != -1 && ISDIGIT(c)) {
            char prev = p->lex.pcur-1 > p->lex.pbeg ? *(p->lex.pcur-2) : 0;
            parse_numeric(p, '.');
            if (ISDIGIT(prev)) {
                yyerror0("unexpected fraction part after numeric literal");
            }
            else {
                yyerror0("no .<digit> floating literal anymore; put 0 before dot");
            }
            SET_LEX_STATE(EXPR_END);
            p->lex.ptok = p->lex.pcur;
            goto retry;
        }
        set_yylval_id('.');
        SET_LEX_STATE(EXPR_DOT);
        return '.';
      }

      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
        return parse_numeric(p, c);

      case ')':
        COND_POP();
        CMDARG_POP();
        SET_LEX_STATE(EXPR_ENDFN);
        p->lex.paren_nest--;
        return c;

      case ']':
        COND_POP();
        CMDARG_POP();
        SET_LEX_STATE(EXPR_END);
        p->lex.paren_nest--;
        return c;

      case '}':
        /* tSTRING_DEND does COND_POP and CMDARG_POP in the yacc's rule */
        if (!p->lex.brace_nest--) return tSTRING_DEND;
        COND_POP();
        CMDARG_POP();
        SET_LEX_STATE(EXPR_END);
        p->lex.paren_nest--;
        return c;

      case ':':
        c = nextc(p);
        if (c == ':') {
            if (IS_BEG() || IS_lex_state(EXPR_CLASS) || IS_SPCARG(-1)) {
                SET_LEX_STATE(EXPR_BEG);
                return tCOLON3;
            }
            set_yylval_id(idCOLON2);
            SET_LEX_STATE(EXPR_DOT);
            return tCOLON2;
        }
        if (IS_END() || ISSPACE(c) || c == '#') {
            pushback(p, c);
            c = warn_balanced(':', ":", "symbol literal");
            SET_LEX_STATE(EXPR_BEG);
            return c;
        }
        switch (c) {
          case '\'':
            p->lex.strterm = NEW_STRTERM(str_ssym, c, 0);
            break;
          case '"':
            p->lex.strterm = NEW_STRTERM(str_dsym, c, 0);
            break;
          default:
            pushback(p, c);
            break;
        }
        SET_LEX_STATE(EXPR_FNAME);
        return tSYMBEG;

      case '/':
        if (IS_BEG()) {
            p->lex.strterm = NEW_STRTERM(str_regexp, '/', 0);
            return tREGEXP_BEG;
        }
        if ((c = nextc(p)) == '=') {
            set_yylval_id('/');
            SET_LEX_STATE(EXPR_BEG);
            return tOP_ASGN;
        }
        pushback(p, c);
        if (IS_SPCARG(c)) {
            arg_ambiguous(p, '/');
            p->lex.strterm = NEW_STRTERM(str_regexp, '/', 0);
            return tREGEXP_BEG;
        }
        SET_LEX_STATE(IS_AFTER_OPERATOR() ? EXPR_ARG : EXPR_BEG);
        return warn_balanced('/', "/", "regexp literal");

      case '^':
        if ((c = nextc(p)) == '=') {
            set_yylval_id('^');
            SET_LEX_STATE(EXPR_BEG);
            return tOP_ASGN;
        }
        SET_LEX_STATE(IS_AFTER_OPERATOR() ? EXPR_ARG : EXPR_BEG);
        pushback(p, c);
        return '^';

      case ';':
        SET_LEX_STATE(EXPR_BEG);
        p->command_start = TRUE;
        return ';';

      case ',':
        SET_LEX_STATE(EXPR_BEG|EXPR_LABEL);
        return ',';

      case '~':
        if (IS_AFTER_OPERATOR()) {
            if ((c = nextc(p)) != '@') {
                pushback(p, c);
            }
            SET_LEX_STATE(EXPR_ARG);
        }
        else {
            SET_LEX_STATE(EXPR_BEG);
        }
        return '~';

      case '(':
        if (IS_BEG()) {
            c = tLPAREN;
        }
        else if (!space_seen) {
            /* foo( ... ) => method call, no ambiguity */
        }
        else if (IS_ARG() || IS_lex_state_all(EXPR_END|EXPR_LABEL)) {
            c = tLPAREN_ARG;
        }
        else if (IS_lex_state(EXPR_ENDFN) && !lambda_beginning_p()) {
            rb_warning0("parentheses after method name is interpreted as "
                        "an argument list, not a decomposed argument");
        }
        p->lex.paren_nest++;
        COND_PUSH(0);
        CMDARG_PUSH(0);
        SET_LEX_STATE(EXPR_BEG|EXPR_LABEL);
        return c;

      case '[':
        p->lex.paren_nest++;
        if (IS_AFTER_OPERATOR()) {
            if ((c = nextc(p)) == ']') {
                p->lex.paren_nest--;
                SET_LEX_STATE(EXPR_ARG);
                if ((c = nextc(p)) == '=') {
                    return tASET;
                }
                pushback(p, c);
                return tAREF;
            }
            pushback(p, c);
            SET_LEX_STATE(EXPR_ARG|EXPR_LABEL);
            return '[';
        }
        else if (IS_BEG()) {
            c = tLBRACK;
        }
        else if (IS_ARG() && (space_seen || IS_lex_state(EXPR_LABELED))) {
            c = tLBRACK;
        }
        SET_LEX_STATE(EXPR_BEG|EXPR_LABEL);
        COND_PUSH(0);
        CMDARG_PUSH(0);
        return c;

      case '{':
        ++p->lex.brace_nest;
        if (lambda_beginning_p())
            c = tLAMBEG;
        else if (IS_lex_state(EXPR_LABELED))
            c = tLBRACE;      /* hash */
        else if (IS_lex_state(EXPR_ARG_ANY | EXPR_END | EXPR_ENDFN))
            c = '{';          /* block (primary) */
        else if (IS_lex_state(EXPR_ENDARG))
            c = tLBRACE_ARG;  /* block (expr) */
        else
            c = tLBRACE;      /* hash */
        if (c != tLBRACE) {
            p->command_start = TRUE;
            SET_LEX_STATE(EXPR_BEG);
        }
        else {
            SET_LEX_STATE(EXPR_BEG|EXPR_LABEL);
        }
        ++p->lex.paren_nest;  /* after lambda_beginning_p() */
        COND_PUSH(0);
        CMDARG_PUSH(0);
        return c;

      case '\\':
        c = nextc(p);
        if (c == '\n') {
            space_seen = 1;
            dispatch_scan_event(p, tSP);
            goto retry; /* skip \\n */
        }
        if (c == ' ') return tSP;
        if (ISSPACE(c)) return c;
        pushback(p, c);
        return '\\';

      case '%':
        return parse_percent(p, space_seen, last_state);

      case '$':
        return parse_gvar(p, last_state);

      case '@':
        return parse_atmark(p, last_state);

      case '_':
        if (was_bol(p) && whole_match_p(p, "__END__", 7, 0)) {
            p->ruby__end__seen = 1;
            p->eofp = 1;
#ifdef RIPPER
            lex_goto_eol(p);
            dispatch_scan_event(p, k__END__);
#endif
            return END_OF_INPUT;
        }
        newtok(p);
        break;

      default:
        if (!parser_is_identchar(p)) {
            compile_error(p, "Invalid char `\\x%02X' in expression", c);
            token_flush(p);
            goto retry;
        }

        newtok(p);
        break;
    }

    return parse_ident(p, c, cmd_state);
}

static enum yytokentype
yylex(YYSTYPE *lval, YYLTYPE *yylloc, struct parser_params *p)
{
    enum yytokentype t;

    p->lval = lval;
    lval->val = Qundef;
    p->yylloc = yylloc;

    t = parser_yylex(p);

    if (has_delayed_token(p))
        dispatch_delayed_token(p, t);
    else if (t != END_OF_INPUT)
        dispatch_scan_event(p, t);

    return t;
}

#define LVAR_USED ((ID)1 << (sizeof(ID) * CHAR_BIT - 1))

static NODE*
node_new_temporal(struct parser_params *p, enum node_type type, VALUE a0, VALUE a1, VALUE a2)
{
    NODE *n = rb_ast_newnode(p->ast, type);

    rb_node_init(n, type, a0, a1, a2);
    return n;
}

static NODE*
node_newnode(struct parser_params *p, enum node_type type, VALUE a0, VALUE a1, VALUE a2, const rb_code_location_t *loc)
{
    NODE *n = node_new_temporal(p, type, a0, a1, a2);

    nd_set_loc(n, loc);
    nd_set_node_id(n, parser_get_node_id(p));
    return n;
}

static NODE *
nd_set_loc(NODE *nd, const YYLTYPE *loc)
{
    nd->nd_loc = *loc;
    nd_set_line(nd, loc->beg_pos.lineno);
    return nd;
}

#ifndef RIPPER
static enum node_type
nodetype(NODE *node)			/* for debug */
{
    return (enum node_type)nd_type(node);
}

static int
nodeline(NODE *node)
{
    return nd_line(node);
}

static NODE*
newline_node(NODE *node)
{
    if (node) {
        node = remove_begin(node);
        node->flags |= NODE_FL_NEWLINE;
    }
    return node;
}

static void
fixpos(NODE *node, NODE *orig)
{
    if (!node) return;
    if (!orig) return;
    nd_set_line(node, nd_line(orig));
}

static void
parser_warning(struct parser_params *p, NODE *node, const char *mesg)
{
    rb_compile_warning(p->ruby_sourcefile, nd_line(node), "%s", mesg);
}

static void
parser_warn(struct parser_params *p, NODE *node, const char *mesg)
{
    rb_compile_warn(p->ruby_sourcefile, nd_line(node), "%s", mesg);
}

static NODE*
block_append(struct parser_params *p, NODE *head, NODE *tail)
{
    NODE *end, *h = head, *nd;

    if (tail == 0) return head;

    if (h == 0) return tail;
    switch (nd_type(h)) {
      case NODE_LIT:
      case NODE_STR:
      case NODE_SELF:
      case NODE_TRUE:
      case NODE_FALSE:
      case NODE_NIL:
        parser_warning(p, h, "unused literal ignored");
        return tail;
      default:
        h = end = NEW_BLOCK(head, &head->nd_loc);
        end->nd_end = end;
        head = end;
        break;
      case NODE_BLOCK:
        end = h->nd_end;
        break;
    }

    nd = end->nd_head;
    switch (nd_type(nd)) {
      case NODE_RETURN:
      case NODE_BREAK:
      case NODE_NEXT:
      case NODE_REDO:
      case NODE_RETRY:
        if (RTEST(ruby_verbose)) {
            parser_warning(p, tail, "statement not reached");
        }
        break;

      default:
        break;
    }

    if (!nd_type_p(tail, NODE_BLOCK)) {
        tail = NEW_BLOCK(tail, &tail->nd_loc);
        tail->nd_end = tail;
    }
    end->nd_next = tail;
    h->nd_end = tail->nd_end;
    nd_set_last_loc(head, nd_last_loc(tail));
    return head;
}

/* append item to the list */
static NODE*
list_append(struct parser_params *p, NODE *list, NODE *item)
{
    NODE *last;

    if (list == 0) return NEW_LIST(item, &item->nd_loc);
    if (list->nd_next) {
        last = list->nd_next->nd_end;
    }
    else {
        last = list;
    }

    list->nd_alen += 1;
    last->nd_next = NEW_LIST(item, &item->nd_loc);
    list->nd_next->nd_end = last->nd_next;

    nd_set_last_loc(list, nd_last_loc(item));

    return list;
}

/* concat two lists */
static NODE*
list_concat(NODE *head, NODE *tail)
{
    NODE *last;

    if (head->nd_next) {
        last = head->nd_next->nd_end;
    }
    else {
        last = head;
    }

    head->nd_alen += tail->nd_alen;
    last->nd_next = tail;
    if (tail->nd_next) {
        head->nd_next->nd_end = tail->nd_next->nd_end;
    }
    else {
        head->nd_next->nd_end = tail;
    }

    nd_set_last_loc(head, nd_last_loc(tail));

    return head;
}

static int
literal_concat0(struct parser_params *p, VALUE head, VALUE tail)
{
    if (NIL_P(tail)) return 1;
    if (!rb_enc_compatible(head, tail)) {
        compile_error(p, "string literal encodings differ (%s / %s)",
                      rb_enc_name(rb_enc_get(head)),
                      rb_enc_name(rb_enc_get(tail)));
        rb_str_resize(head, 0);
        rb_str_resize(tail, 0);
        return 0;
    }
    rb_str_buf_append(head, tail);
    return 1;
}

static VALUE
string_literal_head(enum node_type htype, NODE *head)
{
    if (htype != NODE_DSTR) return Qfalse;
    if (head->nd_next) {
        head = head->nd_next->nd_end->nd_head;
        if (!head || !nd_type_p(head, NODE_STR)) return Qfalse;
    }
    const VALUE lit = head->nd_lit;
    ASSUME(lit != Qfalse);
    return lit;
}

/* concat two string literals */
static NODE *
literal_concat(struct parser_params *p, NODE *head, NODE *tail, const YYLTYPE *loc)
{
    enum node_type htype;
    VALUE lit;

    if (!head) return tail;
    if (!tail) return head;

    htype = nd_type(head);
    if (htype == NODE_EVSTR) {
        head = new_dstr(p, head, loc);
        htype = NODE_DSTR;
    }
    if (p->heredoc_indent > 0) {
        switch (htype) {
          case NODE_STR:
            nd_set_type(head, NODE_DSTR);
          case NODE_DSTR:
            return list_append(p, head, tail);
          default:
            break;
        }
    }
    switch (nd_type(tail)) {
      case NODE_STR:
        if ((lit = string_literal_head(htype, head)) != Qfalse) {
            htype = NODE_STR;
        }
        else {
            lit = head->nd_lit;
        }
        if (htype == NODE_STR) {
            if (!literal_concat0(p, lit, tail->nd_lit)) {
              error:
                rb_discard_node(p, head);
                rb_discard_node(p, tail);
                return 0;
            }
            rb_discard_node(p, tail);
        }
        else {
            list_append(p, head, tail);
        }
        break;

      case NODE_DSTR:
        if (htype == NODE_STR) {
            if (!literal_concat0(p, head->nd_lit, tail->nd_lit))
                goto error;
            tail->nd_lit = head->nd_lit;
            rb_discard_node(p, head);
            head = tail;
        }
        else if (NIL_P(tail->nd_lit)) {
          append:
            head->nd_alen += tail->nd_alen - 1;
            if (!head->nd_next) {
                head->nd_next = tail->nd_next;
            }
            else if (tail->nd_next) {
                head->nd_next->nd_end->nd_next = tail->nd_next;
                head->nd_next->nd_end = tail->nd_next->nd_end;
            }
            rb_discard_node(p, tail);
        }
        else if ((lit = string_literal_head(htype, head)) != Qfalse) {
            if (!literal_concat0(p, lit, tail->nd_lit))
                goto error;
            tail->nd_lit = Qnil;
            goto append;
        }
        else {
            list_concat(head, NEW_NODE(NODE_LIST, NEW_STR(tail->nd_lit, loc), tail->nd_alen, tail->nd_next, loc));
        }
        break;

      case NODE_EVSTR:
        if (htype == NODE_STR) {
            nd_set_type(head, NODE_DSTR);
            head->nd_alen = 1;
        }
        list_append(p, head, tail);
        break;
    }
    return head;
}

static NODE *
evstr2dstr(struct parser_params *p, NODE *node)
{
    if (nd_type_p(node, NODE_EVSTR)) {
        node = new_dstr(p, node, &node->nd_loc);
    }
    return node;
}

static NODE *
new_evstr(struct parser_params *p, NODE *node, const YYLTYPE *loc)
{
    NODE *head = node;

    if (node) {
        switch (nd_type(node)) {
          case NODE_STR:
            nd_set_type(node, NODE_DSTR);
            return node;
          case NODE_DSTR:
            break;
          case NODE_EVSTR:
            return node;
        }
    }
    return NEW_EVSTR(head, loc);
}

static NODE *
new_dstr(struct parser_params *p, NODE *node, const YYLTYPE *loc)
{
    VALUE lit = STR_NEW0();
    NODE *dstr = NEW_DSTR(lit, loc);
    RB_OBJ_WRITTEN(p->ast, Qnil, lit);
    return list_append(p, dstr, node);
}

static NODE *
call_bin_op(struct parser_params *p, NODE *recv, ID id, NODE *arg1,
                const YYLTYPE *op_loc, const YYLTYPE *loc)
{
    NODE *expr;
    value_expr(recv);
    value_expr(arg1);
    expr = NEW_OPCALL(recv, id, NEW_LIST(arg1, &arg1->nd_loc), loc);
    nd_set_line(expr, op_loc->beg_pos.lineno);
    return expr;
}

static NODE *
call_uni_op(struct parser_params *p, NODE *recv, ID id, const YYLTYPE *op_loc, const YYLTYPE *loc)
{
    NODE *opcall;
    value_expr(recv);
    opcall = NEW_OPCALL(recv, id, 0, loc);
    nd_set_line(opcall, op_loc->beg_pos.lineno);
    return opcall;
}

static NODE *
new_qcall(struct parser_params* p, ID atype, NODE *recv, ID mid, NODE *args, const YYLTYPE *op_loc, const YYLTYPE *loc)
{
    NODE *qcall = NEW_QCALL(atype, recv, mid, args, loc);
    nd_set_line(qcall, op_loc->beg_pos.lineno);
    return qcall;
}

static NODE*
new_command_qcall(struct parser_params* p, ID atype, NODE *recv, ID mid, NODE *args, NODE *block, const YYLTYPE *op_loc, const YYLTYPE *loc)
{
    NODE *ret;
    if (block) block_dup_check(p, args, block);
    ret = new_qcall(p, atype, recv, mid, args, op_loc, loc);
    if (block) ret = method_add_block(p, ret, block, loc);
    fixpos(ret, recv);
    return ret;
}

#define nd_once_body(node) (nd_type_p((node), NODE_ONCE) ? (node)->nd_body : node)
static NODE*
match_op(struct parser_params *p, NODE *node1, NODE *node2, const YYLTYPE *op_loc, const YYLTYPE *loc)
{
    NODE *n;
    int line = op_loc->beg_pos.lineno;

    value_expr(node1);
    value_expr(node2);
    if (node1 && (n = nd_once_body(node1)) != 0) {
        switch (nd_type(n)) {
          case NODE_DREGX:
            {
                NODE *match = NEW_MATCH2(node1, node2, loc);
                nd_set_line(match, line);
                return match;
            }

          case NODE_LIT:
            if (RB_TYPE_P(n->nd_lit, T_REGEXP)) {
                const VALUE lit = n->nd_lit;
                NODE *match = NEW_MATCH2(node1, node2, loc);
                match->nd_args = reg_named_capture_assign(p, lit, loc);
                nd_set_line(match, line);
                return match;
            }
        }
    }

    if (node2 && (n = nd_once_body(node2)) != 0) {
        NODE *match3;

        switch (nd_type(n)) {
          case NODE_LIT:
            if (!RB_TYPE_P(n->nd_lit, T_REGEXP)) break;
            /* fallthru */
          case NODE_DREGX:
            match3 = NEW_MATCH3(node2, node1, loc);
            return match3;
        }
    }

    n = NEW_CALL(node1, tMATCH, NEW_LIST(node2, &node2->nd_loc), loc);
    nd_set_line(n, line);
    return n;
}

# if WARN_PAST_SCOPE
static int
past_dvar_p(struct parser_params *p, ID id)
{
    struct vtable *past = p->lvtbl->past;
    while (past) {
        if (vtable_included(past, id)) return 1;
        past = past->prev;
    }
    return 0;
}
# endif

static int
numparam_nested_p(struct parser_params *p)
{
    struct local_vars *local = p->lvtbl;
    NODE *outer = local->numparam.outer;
    NODE *inner = local->numparam.inner;
    if (outer || inner) {
        NODE *used = outer ? outer : inner;
        compile_error(p, "numbered parameter is already used in\n"
                      "%s:%d: %s block here",
                      p->ruby_sourcefile, nd_line(used),
                      outer ? "outer" : "inner");
        parser_show_error_line(p, &used->nd_loc);
        return 1;
    }
    return 0;
}

static NODE*
gettable(struct parser_params *p, ID id, const YYLTYPE *loc)
{
    ID *vidp = NULL;
    NODE *node;
    switch (id) {
      case keyword_self:
        return NEW_SELF(loc);
      case keyword_nil:
        return NEW_NIL(loc);
      case keyword_true:
        return NEW_TRUE(loc);
      case keyword_false:
        return NEW_FALSE(loc);
      case keyword__FILE__:
        {
            VALUE file = p->ruby_sourcefile_string;
            if (NIL_P(file))
                file = rb_str_new(0, 0);
            else
                file = rb_str_dup(file);
            node = NEW_STR(file, loc);
            RB_OBJ_WRITTEN(p->ast, Qnil, file);
        }
        return node;
      case keyword__LINE__:
        return NEW_LIT(INT2FIX(p->tokline), loc);
      case keyword__ENCODING__:
        node = NEW_LIT(rb_enc_from_encoding(p->enc), loc);
        RB_OBJ_WRITTEN(p->ast, Qnil, node->nd_lit);
        return node;

    }
    switch (id_type(id)) {
      case ID_LOCAL:
        if (dyna_in_block(p) && dvar_defined_ref(p, id, &vidp)) {
            if (NUMPARAM_ID_P(id) && numparam_nested_p(p)) return 0;
            if (id == p->cur_arg) {
                compile_error(p, "circular argument reference - %"PRIsWARN, rb_id2str(id));
                return 0;
            }
            if (vidp) *vidp |= LVAR_USED;
            node = NEW_DVAR(id, loc);
            return node;
        }
        if (local_id_ref(p, id, &vidp)) {
            if (id == p->cur_arg) {
                compile_error(p, "circular argument reference - %"PRIsWARN, rb_id2str(id));
                return 0;
            }
            if (vidp) *vidp |= LVAR_USED;
            node = NEW_LVAR(id, loc);
            return node;
        }
        if (dyna_in_block(p) && NUMPARAM_ID_P(id) &&
            parser_numbered_param(p, NUMPARAM_ID_TO_IDX(id))) {
            if (numparam_nested_p(p)) return 0;
            node = NEW_DVAR(id, loc);
            struct local_vars *local = p->lvtbl;
            if (!local->numparam.current) local->numparam.current = node;
            return node;
        }
# if WARN_PAST_SCOPE
        if (!p->ctxt.in_defined && RTEST(ruby_verbose) && past_dvar_p(p, id)) {
            rb_warning1("possible reference to past scope - %"PRIsWARN, rb_id2str(id));
        }
# endif
        /* method call without arguments */
        return NEW_VCALL(id, loc);
      case ID_GLOBAL:
        return NEW_GVAR(id, loc);
      case ID_INSTANCE:
        return NEW_IVAR(id, loc);
      case ID_CONST:
        return NEW_CONST(id, loc);
      case ID_CLASS:
        return NEW_CVAR(id, loc);
    }
    compile_error(p, "identifier %"PRIsVALUE" is not valid to get", rb_id2str(id));
    return 0;
}

static NODE *
opt_arg_append(NODE *opt_list, NODE *opt)
{
    NODE *opts = opt_list;
    opts->nd_loc.end_pos = opt->nd_loc.end_pos;

    while (opts->nd_next) {
        opts = opts->nd_next;
        opts->nd_loc.end_pos = opt->nd_loc.end_pos;
    }
    opts->nd_next = opt;

    return opt_list;
}

static NODE *
kwd_append(NODE *kwlist, NODE *kw)
{
    if (kwlist) {
        opt_arg_append(kwlist, kw);
    }
    return kwlist;
}

static NODE *
new_defined(struct parser_params *p, NODE *expr, const YYLTYPE *loc)
{
    return NEW_DEFINED(remove_begin_all(expr), loc);
}

static NODE*
symbol_append(struct parser_params *p, NODE *symbols, NODE *symbol)
{
    enum node_type type = nd_type(symbol);
    switch (type) {
      case NODE_DSTR:
        nd_set_type(symbol, NODE_DSYM);
        break;
      case NODE_STR:
        nd_set_type(symbol, NODE_LIT);
        RB_OBJ_WRITTEN(p->ast, Qnil, symbol->nd_lit = rb_str_intern(symbol->nd_lit));
        break;
      default:
        compile_error(p, "unexpected node as symbol: %s", ruby_node_name(type));
    }
    return list_append(p, symbols, symbol);
}

static NODE *
new_regexp(struct parser_params *p, NODE *node, int options, const YYLTYPE *loc)
{
    NODE *list, *prev;
    VALUE lit;

    if (!node) {
        node = NEW_LIT(reg_compile(p, STR_NEW0(), options), loc);
        RB_OBJ_WRITTEN(p->ast, Qnil, node->nd_lit);
        return node;
    }
    switch (nd_type(node)) {
      case NODE_STR:
        {
            VALUE src = node->nd_lit;
            nd_set_type(node, NODE_LIT);
            nd_set_loc(node, loc);
            RB_OBJ_WRITTEN(p->ast, Qnil, node->nd_lit = reg_compile(p, src, options));
        }
        break;
      default:
        lit = STR_NEW0();
        node = NEW_NODE(NODE_DSTR, lit, 1, NEW_LIST(node, loc), loc);
        RB_OBJ_WRITTEN(p->ast, Qnil, lit);
        /* fall through */
      case NODE_DSTR:
        nd_set_type(node, NODE_DREGX);
        nd_set_loc(node, loc);
        node->nd_cflag = options & RE_OPTION_MASK;
        if (!NIL_P(node->nd_lit)) reg_fragment_check(p, node->nd_lit, options);
        for (list = (prev = node)->nd_next; list; list = list->nd_next) {
            NODE *frag = list->nd_head;
            enum node_type type = nd_type(frag);
            if (type == NODE_STR || (type == NODE_DSTR && !frag->nd_next)) {
                VALUE tail = frag->nd_lit;
                if (reg_fragment_check(p, tail, options) && prev && !NIL_P(prev->nd_lit)) {
                    VALUE lit = prev == node ? prev->nd_lit : prev->nd_head->nd_lit;
                    if (!literal_concat0(p, lit, tail)) {
                        return NEW_NIL(loc); /* dummy node on error */
                    }
                    rb_str_resize(tail, 0);
                    prev->nd_next = list->nd_next;
                    rb_discard_node(p, list->nd_head);
                    rb_discard_node(p, list);
                    list = prev;
                }
                else {
                    prev = list;
                }
            }
            else {
                prev = 0;
            }
        }
        if (!node->nd_next) {
            VALUE src = node->nd_lit;
            nd_set_type(node, NODE_LIT);
            RB_OBJ_WRITTEN(p->ast, Qnil, node->nd_lit = reg_compile(p, src, options));
        }
        if (options & RE_OPTION_ONCE) {
            node = NEW_NODE(NODE_ONCE, 0, node, 0, loc);
        }
        break;
    }
    return node;
}

static NODE *
new_kw_arg(struct parser_params *p, NODE *k, const YYLTYPE *loc)
{
    if (!k) return 0;
    return NEW_KW_ARG(0, (k), loc);
}

static NODE *
new_xstring(struct parser_params *p, NODE *node, const YYLTYPE *loc)
{
    if (!node) {
        VALUE lit = STR_NEW0();
        NODE *xstr = NEW_XSTR(lit, loc);
        RB_OBJ_WRITTEN(p->ast, Qnil, lit);
        return xstr;
    }
    switch (nd_type(node)) {
      case NODE_STR:
        nd_set_type(node, NODE_XSTR);
        nd_set_loc(node, loc);
        break;
      case NODE_DSTR:
        nd_set_type(node, NODE_DXSTR);
        nd_set_loc(node, loc);
        break;
      default:
        node = NEW_NODE(NODE_DXSTR, Qnil, 1, NEW_LIST(node, loc), loc);
        break;
    }
    return node;
}

static void
check_literal_when(struct parser_params *p, NODE *arg, const YYLTYPE *loc)
{
    VALUE lit;

    if (!arg || !p->case_labels) return;

    lit = rb_node_case_when_optimizable_literal(arg);
    if (UNDEF_P(lit)) return;
    if (nd_type_p(arg, NODE_STR)) {
        RB_OBJ_WRITTEN(p->ast, Qnil, arg->nd_lit = lit);
    }

    if (NIL_P(p->case_labels)) {
        p->case_labels = rb_obj_hide(rb_hash_new());
    }
    else {
        VALUE line = rb_hash_lookup(p->case_labels, lit);
        if (!NIL_P(line)) {
            rb_warning1("duplicated `when' clause with line %d is ignored",
                        WARN_IVAL(line));
            return;
        }
    }
    rb_hash_aset(p->case_labels, lit, INT2NUM(p->ruby_sourceline));
}

#else  /* !RIPPER */
static int
id_is_var(struct parser_params *p, ID id)
{
    if (is_notop_id(id)) {
        switch (id & ID_SCOPE_MASK) {
          case ID_GLOBAL: case ID_INSTANCE: case ID_CONST: case ID_CLASS:
            return 1;
          case ID_LOCAL:
            if (dyna_in_block(p)) {
                if (NUMPARAM_ID_P(id) || dvar_defined(p, id)) return 1;
            }
            if (local_id(p, id)) return 1;
            /* method call without arguments */
            return 0;
        }
    }
    compile_error(p, "identifier %"PRIsVALUE" is not valid to get", rb_id2str(id));
    return 0;
}

static VALUE
new_regexp(struct parser_params *p, VALUE re, VALUE opt, const YYLTYPE *loc)
{
    VALUE src = 0, err;
    int options = 0;
    if (ripper_is_node_yylval(re)) {
        src = RNODE(re)->nd_cval;
        re = RNODE(re)->nd_rval;
    }
    if (ripper_is_node_yylval(opt)) {
        options = (int)RNODE(opt)->nd_tag;
        opt = RNODE(opt)->nd_rval;
    }
    if (src && NIL_P(parser_reg_compile(p, src, options, &err))) {
        compile_error(p, "%"PRIsVALUE, err);
    }
    return dispatch2(regexp_literal, re, opt);
}
#endif /* !RIPPER */

static inline enum lex_state_e
parser_set_lex_state(struct parser_params *p, enum lex_state_e ls, int line)
{
    if (p->debug) {
        ls = rb_parser_trace_lex_state(p, p->lex.state, ls, line);
    }
    return p->lex.state = ls;
}

#ifndef RIPPER
static const char rb_parser_lex_state_names[][8] = {
    "BEG",    "END",    "ENDARG", "ENDFN",  "ARG",
    "CMDARG", "MID",    "FNAME",  "DOT",    "CLASS",
    "LABEL",  "LABELED","FITEM",
};

static VALUE
append_lex_state_name(enum lex_state_e state, VALUE buf)
{
    int i, sep = 0;
    unsigned int mask = 1;
    static const char none[] = "NONE";

    for (i = 0; i < EXPR_MAX_STATE; ++i, mask <<= 1) {
        if ((unsigned)state & mask) {
            if (sep) {
                rb_str_cat(buf, "|", 1);
            }
            sep = 1;
            rb_str_cat_cstr(buf, rb_parser_lex_state_names[i]);
        }
    }
    if (!sep) {
        rb_str_cat(buf, none, sizeof(none)-1);
    }
    return buf;
}

static void
flush_debug_buffer(struct parser_params *p, VALUE out, VALUE str)
{
    VALUE mesg = p->debug_buffer;

    if (!NIL_P(mesg) && RSTRING_LEN(mesg)) {
        p->debug_buffer = Qnil;
        rb_io_puts(1, &mesg, out);
    }
    if (!NIL_P(str) && RSTRING_LEN(str)) {
        rb_io_write(p->debug_output, str);
    }
}

enum lex_state_e
rb_parser_trace_lex_state(struct parser_params *p, enum lex_state_e from,
                          enum lex_state_e to, int line)
{
    VALUE mesg;
    mesg = rb_str_new_cstr("lex_state: ");
    append_lex_state_name(from, mesg);
    rb_str_cat_cstr(mesg, " -> ");
    append_lex_state_name(to, mesg);
    rb_str_catf(mesg, " at line %d\n", line);
    flush_debug_buffer(p, p->debug_output, mesg);
    return to;
}

VALUE
rb_parser_lex_state_name(enum lex_state_e state)
{
    return rb_fstring(append_lex_state_name(state, rb_str_new(0, 0)));
}

static void
append_bitstack_value(stack_type stack, VALUE mesg)
{
    if (stack == 0) {
        rb_str_cat_cstr(mesg, "0");
    }
    else {
        stack_type mask = (stack_type)1U << (CHAR_BIT * sizeof(stack_type) - 1);
        for (; mask && !(stack & mask); mask >>= 1) continue;
        for (; mask; mask >>= 1) rb_str_cat(mesg, stack & mask ? "1" : "0", 1);
    }
}

void
rb_parser_show_bitstack(struct parser_params *p, stack_type stack,
                        const char *name, int line)
{
    VALUE mesg = rb_sprintf("%s: ", name);
    append_bitstack_value(stack, mesg);
    rb_str_catf(mesg, " at line %d\n", line);
    flush_debug_buffer(p, p->debug_output, mesg);
}

void
rb_parser_fatal(struct parser_params *p, const char *fmt, ...)
{
    va_list ap;
    VALUE mesg = rb_str_new_cstr("internal parser error: ");

    va_start(ap, fmt);
    rb_str_vcatf(mesg, fmt, ap);
    va_end(ap);
    yyerror0(RSTRING_PTR(mesg));
    RB_GC_GUARD(mesg);

    mesg = rb_str_new(0, 0);
    append_lex_state_name(p->lex.state, mesg);
    compile_error(p, "lex.state: %"PRIsVALUE, mesg);
    rb_str_resize(mesg, 0);
    append_bitstack_value(p->cond_stack, mesg);
    compile_error(p, "cond_stack: %"PRIsVALUE, mesg);
    rb_str_resize(mesg, 0);
    append_bitstack_value(p->cmdarg_stack, mesg);
    compile_error(p, "cmdarg_stack: %"PRIsVALUE, mesg);
    if (p->debug_output == rb_ractor_stdout())
        p->debug_output = rb_ractor_stderr();
    p->debug = TRUE;
}

static YYLTYPE *
rb_parser_set_pos(YYLTYPE *yylloc, int sourceline, int beg_pos, int end_pos)
{
    yylloc->beg_pos.lineno = sourceline;
    yylloc->beg_pos.column = beg_pos;
    yylloc->end_pos.lineno = sourceline;
    yylloc->end_pos.column = end_pos;
    return yylloc;
}

YYLTYPE *
rb_parser_set_location_from_strterm_heredoc(struct parser_params *p, rb_strterm_heredoc_t *here, YYLTYPE *yylloc)
{
    int sourceline = here->sourceline;
    int beg_pos = (int)here->offset - here->quote
        - (rb_strlen_lit("<<-") - !(here->func & STR_FUNC_INDENT));
    int end_pos = (int)here->offset + here->length + here->quote;

    return rb_parser_set_pos(yylloc, sourceline, beg_pos, end_pos);
}

YYLTYPE *
rb_parser_set_location_of_delayed_token(struct parser_params *p, YYLTYPE *yylloc)
{
    yylloc->beg_pos.lineno = p->delayed.beg_line;
    yylloc->beg_pos.column = p->delayed.beg_col;
    yylloc->end_pos.lineno = p->delayed.end_line;
    yylloc->end_pos.column = p->delayed.end_col;

    return yylloc;
}

YYLTYPE *
rb_parser_set_location_of_heredoc_end(struct parser_params *p, YYLTYPE *yylloc)
{
    int sourceline = p->ruby_sourceline;
    int beg_pos = (int)(p->lex.ptok - p->lex.pbeg);
    int end_pos = (int)(p->lex.pend - p->lex.pbeg);
    return rb_parser_set_pos(yylloc, sourceline, beg_pos, end_pos);
}

YYLTYPE *
rb_parser_set_location_of_dummy_end(struct parser_params *p, YYLTYPE *yylloc)
{
    yylloc->end_pos = yylloc->beg_pos;

    return yylloc;
}

YYLTYPE *
rb_parser_set_location_of_none(struct parser_params *p, YYLTYPE *yylloc)
{
    int sourceline = p->ruby_sourceline;
    int beg_pos = (int)(p->lex.ptok - p->lex.pbeg);
    int end_pos = (int)(p->lex.ptok - p->lex.pbeg);
    return rb_parser_set_pos(yylloc, sourceline, beg_pos, end_pos);
}

YYLTYPE *
rb_parser_set_location(struct parser_params *p, YYLTYPE *yylloc)
{
    int sourceline = p->ruby_sourceline;
    int beg_pos = (int)(p->lex.ptok - p->lex.pbeg);
    int end_pos = (int)(p->lex.pcur - p->lex.pbeg);
    return rb_parser_set_pos(yylloc, sourceline, beg_pos, end_pos);
}
#endif /* !RIPPER */

static int
assignable0(struct parser_params *p, ID id, const char **err)
{
    if (!id) return -1;
    switch (id) {
      case keyword_self:
        *err = "Can't change the value of self";
        return -1;
      case keyword_nil:
        *err = "Can't assign to nil";
        return -1;
      case keyword_true:
        *err = "Can't assign to true";
        return -1;
      case keyword_false:
        *err = "Can't assign to false";
        return -1;
      case keyword__FILE__:
        *err = "Can't assign to __FILE__";
        return -1;
      case keyword__LINE__:
        *err = "Can't assign to __LINE__";
        return -1;
      case keyword__ENCODING__:
        *err = "Can't assign to __ENCODING__";
        return -1;
    }
    switch (id_type(id)) {
      case ID_LOCAL:
        if (dyna_in_block(p)) {
            if (p->max_numparam > NO_PARAM && NUMPARAM_ID_P(id)) {
                compile_error(p, "Can't assign to numbered parameter _%d",
                              NUMPARAM_ID_TO_IDX(id));
                return -1;
            }
            if (dvar_curr(p, id)) return NODE_DASGN;
            if (dvar_defined(p, id)) return NODE_DASGN;
            if (local_id(p, id)) return NODE_LASGN;
            dyna_var(p, id);
            return NODE_DASGN;
        }
        else {
            if (!local_id(p, id)) local_var(p, id);
            return NODE_LASGN;
        }
        break;
      case ID_GLOBAL: return NODE_GASGN;
      case ID_INSTANCE: return NODE_IASGN;
      case ID_CONST:
        if (!p->ctxt.in_def) return NODE_CDECL;
        *err = "dynamic constant assignment";
        return -1;
      case ID_CLASS: return NODE_CVASGN;
      default:
        compile_error(p, "identifier %"PRIsVALUE" is not valid to set", rb_id2str(id));
    }
    return -1;
}

#ifndef RIPPER
static NODE*
assignable(struct parser_params *p, ID id, NODE *val, const YYLTYPE *loc)
{
    const char *err = 0;
    int node_type = assignable0(p, id, &err);
    switch (node_type) {
      case NODE_DASGN: return NEW_DASGN(id, val, loc);
      case NODE_LASGN: return NEW_LASGN(id, val, loc);
      case NODE_GASGN: return NEW_GASGN(id, val, loc);
      case NODE_IASGN: return NEW_IASGN(id, val, loc);
      case NODE_CDECL: return NEW_CDECL(id, val, 0, loc);
      case NODE_CVASGN: return NEW_CVASGN(id, val, loc);
    }
    if (err) yyerror1(loc, err);
    return NEW_BEGIN(0, loc);
}
#else
static VALUE
assignable(struct parser_params *p, VALUE lhs)
{
    const char *err = 0;
    assignable0(p, get_id(lhs), &err);
    if (err) lhs = assign_error(p, err, lhs);
    return lhs;
}
#endif

static int
is_private_local_id(ID name)
{
    VALUE s;
    if (name == idUScore) return 1;
    if (!is_local_id(name)) return 0;
    s = rb_id2str(name);
    if (!s) return 0;
    return RSTRING_PTR(s)[0] == '_';
}

static int
shadowing_lvar_0(struct parser_params *p, ID name)
{
    if (dyna_in_block(p)) {
        if (dvar_curr(p, name)) {
            if (is_private_local_id(name)) return 1;
            yyerror0("duplicated argument name");
        }
        else if (dvar_defined(p, name) || local_id(p, name)) {
            vtable_add(p->lvtbl->vars, name);
            if (p->lvtbl->used) {
                vtable_add(p->lvtbl->used, (ID)p->ruby_sourceline | LVAR_USED);
            }
            return 0;
        }
    }
    else {
        if (local_id(p, name)) {
            if (is_private_local_id(name)) return 1;
            yyerror0("duplicated argument name");
        }
    }
    return 1;
}

static ID
shadowing_lvar(struct parser_params *p, ID name)
{
    shadowing_lvar_0(p, name);
    return name;
}

static void
new_bv(struct parser_params *p, ID name)
{
    if (!name) return;
    if (!is_local_id(name)) {
        compile_error(p, "invalid local variable - %"PRIsVALUE,
                      rb_id2str(name));
        return;
    }
    if (!shadowing_lvar_0(p, name)) return;
    dyna_var(p, name);
}

#ifndef RIPPER
static NODE *
aryset(struct parser_params *p, NODE *recv, NODE *idx, const YYLTYPE *loc)
{
    return NEW_ATTRASGN(recv, tASET, idx, loc);
}

static void
block_dup_check(struct parser_params *p, NODE *node1, NODE *node2)
{
    if (node2 && node1 && nd_type_p(node1, NODE_BLOCK_PASS)) {
        compile_error(p, "both block arg and actual block given");
    }
}

static NODE *
attrset(struct parser_params *p, NODE *recv, ID atype, ID id, const YYLTYPE *loc)
{
    if (!CALL_Q_P(atype)) id = rb_id_attrset(id);
    return NEW_ATTRASGN(recv, id, 0, loc);
}

static void
rb_backref_error(struct parser_params *p, NODE *node)
{
    switch (nd_type(node)) {
      case NODE_NTH_REF:
        compile_error(p, "Can't set variable $%ld", node->nd_nth);
        break;
      case NODE_BACK_REF:
        compile_error(p, "Can't set variable $%c", (int)node->nd_nth);
        break;
    }
}
#else
static VALUE
backref_error(struct parser_params *p, NODE *ref, VALUE expr)
{
    VALUE mesg = rb_str_new_cstr("Can't set variable ");
    rb_str_append(mesg, ref->nd_cval);
    return dispatch2(assign_error, mesg, expr);
}
#endif

#ifndef RIPPER
static NODE *
arg_append(struct parser_params *p, NODE *node1, NODE *node2, const YYLTYPE *loc)
{
    if (!node1) return NEW_LIST(node2, &node2->nd_loc);
    switch (nd_type(node1))  {
      case NODE_LIST:
        return list_append(p, node1, node2);
      case NODE_BLOCK_PASS:
        node1->nd_head = arg_append(p, node1->nd_head, node2, loc);
        node1->nd_loc.end_pos = node1->nd_head->nd_loc.end_pos;
        return node1;
      case NODE_ARGSPUSH:
        node1->nd_body = list_append(p, NEW_LIST(node1->nd_body, &node1->nd_body->nd_loc), node2);
        node1->nd_loc.end_pos = node1->nd_body->nd_loc.end_pos;
        nd_set_type(node1, NODE_ARGSCAT);
        return node1;
      case NODE_ARGSCAT:
        if (!nd_type_p(node1->nd_body, NODE_LIST)) break;
        node1->nd_body = list_append(p, node1->nd_body, node2);
        node1->nd_loc.end_pos = node1->nd_body->nd_loc.end_pos;
        return node1;
    }
    return NEW_ARGSPUSH(node1, node2, loc);
}

static NODE *
arg_concat(struct parser_params *p, NODE *node1, NODE *node2, const YYLTYPE *loc)
{
    if (!node2) return node1;
    switch (nd_type(node1)) {
      case NODE_BLOCK_PASS:
        if (node1->nd_head)
            node1->nd_head = arg_concat(p, node1->nd_head, node2, loc);
        else
            node1->nd_head = NEW_LIST(node2, loc);
        return node1;
      case NODE_ARGSPUSH:
        if (!nd_type_p(node2, NODE_LIST)) break;
        node1->nd_body = list_concat(NEW_LIST(node1->nd_body, loc), node2);
        nd_set_type(node1, NODE_ARGSCAT);
        return node1;
      case NODE_ARGSCAT:
        if (!nd_type_p(node2, NODE_LIST) ||
            !nd_type_p(node1->nd_body, NODE_LIST)) break;
        node1->nd_body = list_concat(node1->nd_body, node2);
        return node1;
    }
    return NEW_ARGSCAT(node1, node2, loc);
}

static NODE *
last_arg_append(struct parser_params *p, NODE *args, NODE *last_arg, const YYLTYPE *loc)
{
    NODE *n1;
    if ((n1 = splat_array(args)) != 0) {
        return list_append(p, n1, last_arg);
    }
    return arg_append(p, args, last_arg, loc);
}

static NODE *
rest_arg_append(struct parser_params *p, NODE *args, NODE *rest_arg, const YYLTYPE *loc)
{
    NODE *n1;
    if ((nd_type_p(rest_arg, NODE_LIST)) && (n1 = splat_array(args)) != 0) {
        return list_concat(n1, rest_arg);
    }
    return arg_concat(p, args, rest_arg, loc);
}

static NODE *
splat_array(NODE* node)
{
    if (nd_type_p(node, NODE_SPLAT)) node = node->nd_head;
    if (nd_type_p(node, NODE_LIST)) return node;
    return 0;
}

static void
mark_lvar_used(struct parser_params *p, NODE *rhs)
{
    ID *vidp = NULL;
    if (!rhs) return;
    switch (nd_type(rhs)) {
      case NODE_LASGN:
        if (local_id_ref(p, rhs->nd_vid, &vidp)) {
            if (vidp) *vidp |= LVAR_USED;
        }
        break;
      case NODE_DASGN:
        if (dvar_defined_ref(p, rhs->nd_vid, &vidp)) {
            if (vidp) *vidp |= LVAR_USED;
        }
        break;
#if 0
      case NODE_MASGN:
        for (rhs = rhs->nd_head; rhs; rhs = rhs->nd_next) {
            mark_lvar_used(p, rhs->nd_head);
        }
        break;
#endif
    }
}

static NODE *
const_decl_path(struct parser_params *p, NODE **dest)
{
    NODE *n = *dest;
    if (!nd_type_p(n, NODE_CALL)) {
        const YYLTYPE *loc = &n->nd_loc;
        VALUE path;
        if (n->nd_vid) {
             path = rb_id2str(n->nd_vid);
        }
        else {
            n = n->nd_else;
            path = rb_ary_new();
            for (; n && nd_type_p(n, NODE_COLON2); n = n->nd_head) {
                rb_ary_push(path, rb_id2str(n->nd_mid));
            }
            if (n && nd_type_p(n, NODE_CONST)) {
                // Const::Name
                rb_ary_push(path, rb_id2str(n->nd_vid));
            }
            else if (n && nd_type_p(n, NODE_COLON3)) {
                // ::Const::Name
                rb_ary_push(path, rb_str_new(0, 0));
            }
            else {
                // expression::Name
                rb_ary_push(path, rb_str_new_cstr("..."));
            }
            path = rb_ary_join(rb_ary_reverse(path), rb_str_new_cstr("::"));
            path = rb_fstring(path);
        }
        *dest = n = NEW_LIT(path, loc);
        RB_OBJ_WRITTEN(p->ast, Qnil, n->nd_lit);
    }
    return n;
}

extern VALUE rb_mRubyVMFrozenCore;

static NODE *
make_shareable_node(struct parser_params *p, NODE *value, bool copy, const YYLTYPE *loc)
{
    NODE *fcore = NEW_LIT(rb_mRubyVMFrozenCore, loc);

    if (copy) {
        return NEW_CALL(fcore, rb_intern("make_shareable_copy"),
                        NEW_LIST(value, loc), loc);
    }
    else {
        return NEW_CALL(fcore, rb_intern("make_shareable"),
                        NEW_LIST(value, loc), loc);
    }
}

static NODE *
ensure_shareable_node(struct parser_params *p, NODE **dest, NODE *value, const YYLTYPE *loc)
{
    NODE *fcore = NEW_LIT(rb_mRubyVMFrozenCore, loc);
    NODE *args = NEW_LIST(value, loc);
    args = list_append(p, args, const_decl_path(p, dest));
    return NEW_CALL(fcore, rb_intern("ensure_shareable"), args, loc);
}

static int is_static_content(NODE *node);

static VALUE
shareable_literal_value(NODE *node)
{
    if (!node) return Qnil;
    enum node_type type = nd_type(node);
    switch (type) {
      case NODE_TRUE:
        return Qtrue;
      case NODE_FALSE:
        return Qfalse;
      case NODE_NIL:
        return Qnil;
      case NODE_LIT:
        return node->nd_lit;
      default:
        return Qundef;
    }
}

#ifndef SHAREABLE_BARE_EXPRESSION
#define SHAREABLE_BARE_EXPRESSION 1
#endif

static NODE *
shareable_literal_constant(struct parser_params *p, enum shareability shareable,
                           NODE **dest, NODE *value, const YYLTYPE *loc, size_t level)
{
# define shareable_literal_constant_next(n) \
    shareable_literal_constant(p, shareable, dest, (n), &(n)->nd_loc, level+1)
    VALUE lit = Qnil;

    if (!value) return 0;
    enum node_type type = nd_type(value);
    switch (type) {
      case NODE_TRUE:
      case NODE_FALSE:
      case NODE_NIL:
      case NODE_LIT:
        return value;

      case NODE_DSTR:
        if (shareable == shareable_literal) {
            value = NEW_CALL(value, idUMinus, 0, loc);
        }
        return value;

      case NODE_STR:
        lit = rb_fstring(value->nd_lit);
        nd_set_type(value, NODE_LIT);
        RB_OBJ_WRITE(p->ast, &value->nd_lit, lit);
        return value;

      case NODE_ZLIST:
        lit = rb_ary_new();
        OBJ_FREEZE_RAW(lit);
        NODE *n = NEW_LIT(lit, loc);
        RB_OBJ_WRITTEN(p->ast, Qnil, n->nd_lit);
        return n;

      case NODE_LIST:
        lit = rb_ary_new();
        for (NODE *n = value; n; n = n->nd_next) {
            NODE *elt = n->nd_head;
            if (elt) {
                elt = shareable_literal_constant_next(elt);
                if (elt) {
                    n->nd_head = elt;
                }
                else if (RTEST(lit)) {
                    rb_ary_clear(lit);
                    lit = Qfalse;
                }
            }
            if (RTEST(lit)) {
                VALUE e = shareable_literal_value(elt);
                if (!UNDEF_P(e)) {
                    rb_ary_push(lit, e);
                }
                else {
                    rb_ary_clear(lit);
                    lit = Qnil;	/* make shareable at runtime */
                }
            }
        }
        break;

      case NODE_HASH:
        if (!value->nd_brace) return 0;
        lit = rb_hash_new();
        for (NODE *n = value->nd_head; n; n = n->nd_next->nd_next) {
            NODE *key = n->nd_head;
            NODE *val = n->nd_next->nd_head;
            if (key) {
                key = shareable_literal_constant_next(key);
                if (key) {
                    n->nd_head = key;
                }
                else if (RTEST(lit)) {
                    rb_hash_clear(lit);
                    lit = Qfalse;
                }
            }
            if (val) {
                val = shareable_literal_constant_next(val);
                if (val) {
                    n->nd_next->nd_head = val;
                }
                else if (RTEST(lit)) {
                    rb_hash_clear(lit);
                    lit = Qfalse;
                }
            }
            if (RTEST(lit)) {
                VALUE k = shareable_literal_value(key);
                VALUE v = shareable_literal_value(val);
                if (!UNDEF_P(k) && !UNDEF_P(v)) {
                    rb_hash_aset(lit, k, v);
                }
                else {
                    rb_hash_clear(lit);
                    lit = Qnil;	/* make shareable at runtime */
                }
            }
        }
        break;

      default:
        if (shareable == shareable_literal &&
            (SHAREABLE_BARE_EXPRESSION || level > 0)) {
            return ensure_shareable_node(p, dest, value, loc);
        }
        return 0;
    }

    /* Array or Hash */
    if (!lit) return 0;
    if (NIL_P(lit)) {
        // if shareable_literal, all elements should have been ensured
        // as shareable
        value = make_shareable_node(p, value, false, loc);
    }
    else {
        value = NEW_LIT(rb_ractor_make_shareable(lit), loc);
        RB_OBJ_WRITTEN(p->ast, Qnil, value->nd_lit);
    }

    return value;
# undef shareable_literal_constant_next
}

static NODE *
shareable_constant_value(struct parser_params *p, enum shareability shareable,
                         NODE *lhs, NODE *value, const YYLTYPE *loc)
{
    if (!value) return 0;
    switch (shareable) {
      case shareable_none:
        return value;

      case shareable_literal:
        {
            NODE *lit = shareable_literal_constant(p, shareable, &lhs, value, loc, 0);
            if (lit) return lit;
            return value;
        }
        break;

      case shareable_copy:
      case shareable_everything:
        {
            NODE *lit = shareable_literal_constant(p, shareable, &lhs, value, loc, 0);
            if (lit) return lit;
            return make_shareable_node(p, value, shareable == shareable_copy, loc);
        }
        break;

      default:
        UNREACHABLE_RETURN(0);
    }
}

static NODE *
node_assign(struct parser_params *p, NODE *lhs, NODE *rhs, struct lex_context ctxt, const YYLTYPE *loc)
{
    if (!lhs) return 0;

    switch (nd_type(lhs)) {
      case NODE_CDECL:
        rhs = shareable_constant_value(p, ctxt.shareable_constant_value, lhs, rhs, loc);
        /* fallthru */

      case NODE_GASGN:
      case NODE_IASGN:
      case NODE_LASGN:
      case NODE_DASGN:
      case NODE_MASGN:
      case NODE_CVASGN:
        lhs->nd_value = rhs;
        nd_set_loc(lhs, loc);
        break;

      case NODE_ATTRASGN:
        lhs->nd_args = arg_append(p, lhs->nd_args, rhs, loc);
        nd_set_loc(lhs, loc);
        break;

      default:
        /* should not happen */
        break;
    }

    return lhs;
}

static NODE *
value_expr_check(struct parser_params *p, NODE *node)
{
    NODE *void_node = 0, *vn;

    if (!node) {
        rb_warning0("empty expression");
    }
    while (node) {
        switch (nd_type(node)) {
          case NODE_RETURN:
          case NODE_BREAK:
          case NODE_NEXT:
          case NODE_REDO:
          case NODE_RETRY:
            return void_node ? void_node : node;

          case NODE_CASE3:
            if (!node->nd_body || !nd_type_p(node->nd_body, NODE_IN)) {
                compile_error(p, "unexpected node");
                return NULL;
            }
            if (node->nd_body->nd_body) {
                return NULL;
            }
            /* single line pattern matching */
            return void_node ? void_node : node;

          case NODE_BLOCK:
            while (node->nd_next) {
                node = node->nd_next;
            }
            node = node->nd_head;
            break;

          case NODE_BEGIN:
            node = node->nd_body;
            break;

          case NODE_IF:
          case NODE_UNLESS:
            if (!node->nd_body) {
                return NULL;
            }
            else if (!node->nd_else) {
                return NULL;
            }
            vn = value_expr_check(p, node->nd_body);
            if (!vn) return NULL;
            if (!void_node) void_node = vn;
            node = node->nd_else;
            break;

          case NODE_AND:
          case NODE_OR:
            node = node->nd_1st;
            break;

          case NODE_LASGN:
          case NODE_DASGN:
          case NODE_MASGN:
            mark_lvar_used(p, node);
            return NULL;

          default:
            return NULL;
        }
    }

    return NULL;
}

static int
value_expr_gen(struct parser_params *p, NODE *node)
{
    NODE *void_node = value_expr_check(p, node);
    if (void_node) {
        yyerror1(&void_node->nd_loc, "void value expression");
        /* or "control never reach"? */
        return FALSE;
    }
    return TRUE;
}
static void
void_expr(struct parser_params *p, NODE *node)
{
    const char *useless = 0;

    if (!RTEST(ruby_verbose)) return;

    if (!node || !(node = nd_once_body(node))) return;
    switch (nd_type(node)) {
      case NODE_OPCALL:
        switch (node->nd_mid) {
          case '+':
          case '-':
          case '*':
          case '/':
          case '%':
          case tPOW:
          case tUPLUS:
          case tUMINUS:
          case '|':
          case '^':
          case '&':
          case tCMP:
          case '>':
          case tGEQ:
          case '<':
          case tLEQ:
          case tEQ:
          case tNEQ:
            useless = rb_id2name(node->nd_mid);
            break;
        }
        break;

      case NODE_LVAR:
      case NODE_DVAR:
      case NODE_GVAR:
      case NODE_IVAR:
      case NODE_CVAR:
      case NODE_NTH_REF:
      case NODE_BACK_REF:
        useless = "a variable";
        break;
      case NODE_CONST:
        useless = "a constant";
        break;
      case NODE_LIT:
      case NODE_STR:
      case NODE_DSTR:
      case NODE_DREGX:
        useless = "a literal";
        break;
      case NODE_COLON2:
      case NODE_COLON3:
        useless = "::";
        break;
      case NODE_DOT2:
        useless = "..";
        break;
      case NODE_DOT3:
        useless = "...";
        break;
      case NODE_SELF:
        useless = "self";
        break;
      case NODE_NIL:
        useless = "nil";
        break;
      case NODE_TRUE:
        useless = "true";
        break;
      case NODE_FALSE:
        useless = "false";
        break;
      case NODE_DEFINED:
        useless = "defined?";
        break;
    }

    if (useless) {
        rb_warn1L(nd_line(node), "possibly useless use of %s in void context", WARN_S(useless));
    }
}

static NODE *
void_stmts(struct parser_params *p, NODE *node)
{
    NODE *const n = node;
    if (!RTEST(ruby_verbose)) return n;
    if (!node) return n;
    if (!nd_type_p(node, NODE_BLOCK)) return n;

    while (node->nd_next) {
        void_expr(p, node->nd_head);
        node = node->nd_next;
    }
    return n;
}

static NODE *
remove_begin(NODE *node)
{
    NODE **n = &node, *n1 = node;
    while (n1 && nd_type_p(n1, NODE_BEGIN) && n1->nd_body) {
        *n = n1 = n1->nd_body;
    }
    return node;
}

static NODE *
remove_begin_all(NODE *node)
{
    NODE **n = &node, *n1 = node;
    while (n1 && nd_type_p(n1, NODE_BEGIN)) {
        *n = n1 = n1->nd_body;
    }
    return node;
}

static void
reduce_nodes(struct parser_params *p, NODE **body)
{
    NODE *node = *body;

    if (!node) {
        *body = NEW_NIL(&NULL_LOC);
        return;
    }
#define subnodes(n1, n2) \
    ((!node->n1) ? (node->n2 ? (body = &node->n2, 1) : 0) : \
     (!node->n2) ? (body = &node->n1, 1) : \
     (reduce_nodes(p, &node->n1), body = &node->n2, 1))

    while (node) {
        int newline = (int)(node->flags & NODE_FL_NEWLINE);
        switch (nd_type(node)) {
          end:
          case NODE_NIL:
            *body = 0;
            return;
          case NODE_RETURN:
            *body = node = node->nd_stts;
            if (newline && node) node->flags |= NODE_FL_NEWLINE;
            continue;
          case NODE_BEGIN:
            *body = node = node->nd_body;
            if (newline && node) node->flags |= NODE_FL_NEWLINE;
            continue;
          case NODE_BLOCK:
            body = &node->nd_end->nd_head;
            break;
          case NODE_IF:
          case NODE_UNLESS:
            if (subnodes(nd_body, nd_else)) break;
            return;
          case NODE_CASE:
            body = &node->nd_body;
            break;
          case NODE_WHEN:
            if (!subnodes(nd_body, nd_next)) goto end;
            break;
          case NODE_ENSURE:
            if (!subnodes(nd_head, nd_resq)) goto end;
            break;
          case NODE_RESCUE:
            if (node->nd_else) {
                body = &node->nd_resq;
                break;
            }
            if (!subnodes(nd_head, nd_resq)) goto end;
            break;
          default:
            return;
        }
        node = *body;
        if (newline && node) node->flags |= NODE_FL_NEWLINE;
    }

#undef subnodes
}

static int
is_static_content(NODE *node)
{
    if (!node) return 1;
    switch (nd_type(node)) {
      case NODE_HASH:
        if (!(node = node->nd_head)) break;
      case NODE_LIST:
        do {
            if (!is_static_content(node->nd_head)) return 0;
        } while ((node = node->nd_next) != 0);
      case NODE_LIT:
      case NODE_STR:
      case NODE_NIL:
      case NODE_TRUE:
      case NODE_FALSE:
      case NODE_ZLIST:
        break;
      default:
        return 0;
    }
    return 1;
}

static int
assign_in_cond(struct parser_params *p, NODE *node)
{
    switch (nd_type(node)) {
      case NODE_MASGN:
      case NODE_LASGN:
      case NODE_DASGN:
      case NODE_GASGN:
      case NODE_IASGN:
        break;

      default:
        return 0;
    }

    if (!node->nd_value) return 1;
    if (is_static_content(node->nd_value)) {
        /* reports always */
        parser_warn(p, node->nd_value, "found `= literal' in conditional, should be ==");
    }
    return 1;
}

enum cond_type {
    COND_IN_OP,
    COND_IN_COND,
    COND_IN_FF
};

#define SWITCH_BY_COND_TYPE(t, w, arg) \
    switch (t) { \
      case COND_IN_OP: break; \
      case COND_IN_COND: rb_##w##0(arg "literal in condition"); break; \
      case COND_IN_FF: rb_##w##0(arg "literal in flip-flop"); break; \
    }

static NODE *cond0(struct parser_params*,NODE*,enum cond_type,const YYLTYPE*);

static NODE*
range_op(struct parser_params *p, NODE *node, const YYLTYPE *loc)
{
    enum node_type type;

    if (node == 0) return 0;

    type = nd_type(node);
    value_expr(node);
    if (type == NODE_LIT && FIXNUM_P(node->nd_lit)) {
        if (!e_option_supplied(p)) parser_warn(p, node, "integer literal in flip-flop");
        ID lineno = rb_intern("$.");
        return NEW_CALL(node, tEQ, NEW_LIST(NEW_GVAR(lineno, loc), loc), loc);
    }
    return cond0(p, node, COND_IN_FF, loc);
}

static NODE*
cond0(struct parser_params *p, NODE *node, enum cond_type type, const YYLTYPE *loc)
{
    if (node == 0) return 0;
    if (!(node = nd_once_body(node))) return 0;
    assign_in_cond(p, node);

    switch (nd_type(node)) {
      case NODE_DSTR:
      case NODE_EVSTR:
      case NODE_STR:
        SWITCH_BY_COND_TYPE(type, warn, "string ")
        break;

      case NODE_DREGX:
        if (!e_option_supplied(p)) SWITCH_BY_COND_TYPE(type, warning, "regex ")

        return NEW_MATCH2(node, NEW_GVAR(idLASTLINE, loc), loc);

      case NODE_AND:
      case NODE_OR:
        node->nd_1st = cond0(p, node->nd_1st, COND_IN_COND, loc);
        node->nd_2nd = cond0(p, node->nd_2nd, COND_IN_COND, loc);
        break;

      case NODE_DOT2:
      case NODE_DOT3:
        node->nd_beg = range_op(p, node->nd_beg, loc);
        node->nd_end = range_op(p, node->nd_end, loc);
        if (nd_type_p(node, NODE_DOT2)) nd_set_type(node,NODE_FLIP2);
        else if (nd_type_p(node, NODE_DOT3)) nd_set_type(node, NODE_FLIP3);
        break;

      case NODE_DSYM:
      warn_symbol:
        SWITCH_BY_COND_TYPE(type, warning, "symbol ")
        break;

      case NODE_LIT:
        if (RB_TYPE_P(node->nd_lit, T_REGEXP)) {
            if (!e_option_supplied(p)) SWITCH_BY_COND_TYPE(type, warn, "regex ")
            nd_set_type(node, NODE_MATCH);
        }
        else if (node->nd_lit == Qtrue ||
                 node->nd_lit == Qfalse) {
            /* booleans are OK, e.g., while true */
        }
        else if (SYMBOL_P(node->nd_lit)) {
            goto warn_symbol;
        }
        else {
            SWITCH_BY_COND_TYPE(type, warning, "")
        }
      default:
        break;
    }
    return node;
}

static NODE*
cond(struct parser_params *p, NODE *node, const YYLTYPE *loc)
{
    if (node == 0) return 0;
    return cond0(p, node, COND_IN_COND, loc);
}

static NODE*
method_cond(struct parser_params *p, NODE *node, const YYLTYPE *loc)
{
    if (node == 0) return 0;
    return cond0(p, node, COND_IN_OP, loc);
}

static NODE*
new_nil_at(struct parser_params *p, const rb_code_position_t *pos)
{
    YYLTYPE loc = {*pos, *pos};
    return NEW_NIL(&loc);
}

static NODE*
new_if(struct parser_params *p, NODE *cc, NODE *left, NODE *right, const YYLTYPE *loc)
{
    if (!cc) return right;
    cc = cond0(p, cc, COND_IN_COND, loc);
    return newline_node(NEW_IF(cc, left, right, loc));
}

static NODE*
new_unless(struct parser_params *p, NODE *cc, NODE *left, NODE *right, const YYLTYPE *loc)
{
    if (!cc) return right;
    cc = cond0(p, cc, COND_IN_COND, loc);
    return newline_node(NEW_UNLESS(cc, left, right, loc));
}

static NODE*
logop(struct parser_params *p, ID id, NODE *left, NODE *right,
          const YYLTYPE *op_loc, const YYLTYPE *loc)
{
    enum node_type type = id == idAND || id == idANDOP ? NODE_AND : NODE_OR;
    NODE *op;
    value_expr(left);
    if (left && nd_type_p(left, type)) {
        NODE *node = left, *second;
        while ((second = node->nd_2nd) != 0 && nd_type_p(second, type)) {
            node = second;
        }
        node->nd_2nd = NEW_NODE(type, second, right, 0, loc);
        nd_set_line(node->nd_2nd, op_loc->beg_pos.lineno);
        left->nd_loc.end_pos = loc->end_pos;
        return left;
    }
    op = NEW_NODE(type, left, right, 0, loc);
    nd_set_line(op, op_loc->beg_pos.lineno);
    return op;
}

static void
no_blockarg(struct parser_params *p, NODE *node)
{
    if (nd_type_p(node, NODE_BLOCK_PASS)) {
        compile_error(p, "block argument should not be given");
    }
}

static NODE *
ret_args(struct parser_params *p, NODE *node)
{
    if (node) {
        no_blockarg(p, node);
        if (nd_type_p(node, NODE_LIST)) {
            if (node->nd_next == 0) {
                node = node->nd_head;
            }
            else {
                nd_set_type(node, NODE_VALUES);
            }
        }
    }
    return node;
}

static NODE *
new_yield(struct parser_params *p, NODE *node, const YYLTYPE *loc)
{
    if (node) no_blockarg(p, node);

    return NEW_YIELD(node, loc);
}

static VALUE
negate_lit(struct parser_params *p, VALUE lit)
{
    if (FIXNUM_P(lit)) {
        return LONG2FIX(-FIX2LONG(lit));
    }
    if (SPECIAL_CONST_P(lit)) {
#if USE_FLONUM
        if (FLONUM_P(lit)) {
            return DBL2NUM(-RFLOAT_VALUE(lit));
        }
#endif
        goto unknown;
    }
    switch (BUILTIN_TYPE(lit)) {
      case T_BIGNUM:
        BIGNUM_NEGATE(lit);
        lit = rb_big_norm(lit);
        break;
      case T_RATIONAL:
        RATIONAL_SET_NUM(lit, negate_lit(p, RRATIONAL(lit)->num));
        break;
      case T_COMPLEX:
        RCOMPLEX_SET_REAL(lit, negate_lit(p, RCOMPLEX(lit)->real));
        RCOMPLEX_SET_IMAG(lit, negate_lit(p, RCOMPLEX(lit)->imag));
        break;
      case T_FLOAT:
        lit = DBL2NUM(-RFLOAT_VALUE(lit));
        break;
      unknown:
      default:
        rb_parser_fatal(p, "unknown literal type (%s) passed to negate_lit",
                        rb_builtin_class_name(lit));
        break;
    }
    return lit;
}

static NODE *
arg_blk_pass(NODE *node1, NODE *node2)
{
    if (node2) {
        if (!node1) return node2;
        node2->nd_head = node1;
        nd_set_first_lineno(node2, nd_first_lineno(node1));
        nd_set_first_column(node2, nd_first_column(node1));
        return node2;
    }
    return node1;
}

static bool
args_info_empty_p(struct rb_args_info *args)
{
    if (args->pre_args_num) return false;
    if (args->post_args_num) return false;
    if (args->rest_arg) return false;
    if (args->opt_args) return false;
    if (args->block_arg) return false;
    if (args->kw_args) return false;
    if (args->kw_rest_arg) return false;
    return true;
}

static NODE*
new_args(struct parser_params *p, NODE *pre_args, NODE *opt_args, ID rest_arg, NODE *post_args, NODE *tail, const YYLTYPE *loc)
{
    int saved_line = p->ruby_sourceline;
    struct rb_args_info *args = tail->nd_ainfo;

    if (args->forwarding) {
        if (rest_arg) {
            yyerror1(&tail->nd_loc, "... after rest argument");
            return tail;
        }
        rest_arg = idFWD_REST;
    }

    args->pre_args_num   = pre_args ? rb_long2int(pre_args->nd_plen) : 0;
    args->pre_init       = pre_args ? pre_args->nd_next : 0;

    args->post_args_num  = post_args ? rb_long2int(post_args->nd_plen) : 0;
    args->post_init      = post_args ? post_args->nd_next : 0;
    args->first_post_arg = post_args ? post_args->nd_pid : 0;

    args->rest_arg       = rest_arg;

    args->opt_args       = opt_args;

#ifdef FORWARD_ARGS_WITH_RUBY2_KEYWORDS
    args->ruby2_keywords = args->forwarding;
#else
    args->ruby2_keywords = 0;
#endif

    p->ruby_sourceline = saved_line;
    nd_set_loc(tail, loc);

    return tail;
}

static NODE*
new_args_tail(struct parser_params *p, NODE *kw_args, ID kw_rest_arg, ID block, const YYLTYPE *kw_rest_loc)
{
    int saved_line = p->ruby_sourceline;
    NODE *node;
    VALUE tmpbuf = rb_imemo_tmpbuf_auto_free_pointer();
    struct rb_args_info *args = ZALLOC(struct rb_args_info);
    rb_imemo_tmpbuf_set_ptr(tmpbuf, args);
    args->imemo = tmpbuf;
    node = NEW_NODE(NODE_ARGS, 0, 0, args, &NULL_LOC);
    RB_OBJ_WRITTEN(p->ast, Qnil, tmpbuf);
    if (p->error_p) return node;

    args->block_arg      = block;
    args->kw_args        = kw_args;

    if (kw_args) {
        /*
         * def foo(k1: 1, kr1:, k2: 2, **krest, &b)
         * variable order: k1, kr1, k2, &b, internal_id, krest
         * #=> <reorder>
         * variable order: kr1, k1, k2, internal_id, krest, &b
         */
        ID kw_bits = internal_id(p), *required_kw_vars, *kw_vars;
        struct vtable *vtargs = p->lvtbl->args;
        NODE *kwn = kw_args;

        if (block) block = vtargs->tbl[vtargs->pos-1];
        vtable_pop(vtargs, !!block + !!kw_rest_arg);
        required_kw_vars = kw_vars = &vtargs->tbl[vtargs->pos];
        while (kwn) {
            if (!NODE_REQUIRED_KEYWORD_P(kwn->nd_body))
                --kw_vars;
            --required_kw_vars;
            kwn = kwn->nd_next;
        }

        for (kwn = kw_args; kwn; kwn = kwn->nd_next) {
            ID vid = kwn->nd_body->nd_vid;
            if (NODE_REQUIRED_KEYWORD_P(kwn->nd_body)) {
                *required_kw_vars++ = vid;
            }
            else {
                *kw_vars++ = vid;
            }
        }

        arg_var(p, kw_bits);
        if (kw_rest_arg) arg_var(p, kw_rest_arg);
        if (block) arg_var(p, block);

        args->kw_rest_arg = NEW_DVAR(kw_rest_arg, kw_rest_loc);
        args->kw_rest_arg->nd_cflag = kw_bits;
    }
    else if (kw_rest_arg == idNil) {
        args->no_kwarg = 1;
    }
    else if (kw_rest_arg) {
        args->kw_rest_arg = NEW_DVAR(kw_rest_arg, kw_rest_loc);
    }

    p->ruby_sourceline = saved_line;
    return node;
}

static NODE *
args_with_numbered(struct parser_params *p, NODE *args, int max_numparam)
{
    if (max_numparam > NO_PARAM) {
        if (!args) {
            YYLTYPE loc = RUBY_INIT_YYLLOC();
            args = new_args_tail(p, 0, 0, 0, 0);
            nd_set_loc(args, &loc);
        }
        args->nd_ainfo->pre_args_num = max_numparam;
    }
    return args;
}

static NODE*
new_array_pattern(struct parser_params *p, NODE *constant, NODE *pre_arg, NODE *aryptn, const YYLTYPE *loc)
{
    struct rb_ary_pattern_info *apinfo = aryptn->nd_apinfo;

    aryptn->nd_pconst = constant;

    if (pre_arg) {
        NODE *pre_args = NEW_LIST(pre_arg, loc);
        if (apinfo->pre_args) {
            apinfo->pre_args = list_concat(pre_args, apinfo->pre_args);
        }
        else {
            apinfo->pre_args = pre_args;
        }
    }
    return aryptn;
}

static NODE*
new_array_pattern_tail(struct parser_params *p, NODE *pre_args, int has_rest, NODE *rest_arg, NODE *post_args, const YYLTYPE *loc)
{
    int saved_line = p->ruby_sourceline;
    NODE *node;
    VALUE tmpbuf = rb_imemo_tmpbuf_auto_free_pointer();
    struct rb_ary_pattern_info *apinfo = ZALLOC(struct rb_ary_pattern_info);
    rb_imemo_tmpbuf_set_ptr(tmpbuf, apinfo);
    node = NEW_NODE(NODE_ARYPTN, 0, tmpbuf, apinfo, loc);
    RB_OBJ_WRITTEN(p->ast, Qnil, tmpbuf);

    apinfo->pre_args = pre_args;

    if (has_rest) {
        apinfo->rest_arg = rest_arg ? rest_arg : NODE_SPECIAL_NO_NAME_REST;
    }
    else {
        apinfo->rest_arg = NULL;
    }

    apinfo->post_args = post_args;

    p->ruby_sourceline = saved_line;
    return node;
}

static NODE*
new_find_pattern(struct parser_params *p, NODE *constant, NODE *fndptn, const YYLTYPE *loc)
{
    fndptn->nd_pconst = constant;

    return fndptn;
}

static NODE*
new_find_pattern_tail(struct parser_params *p, NODE *pre_rest_arg, NODE *args, NODE *post_rest_arg, const YYLTYPE *loc)
{
    int saved_line = p->ruby_sourceline;
    NODE *node;
    VALUE tmpbuf = rb_imemo_tmpbuf_auto_free_pointer();
    struct rb_fnd_pattern_info *fpinfo = ZALLOC(struct rb_fnd_pattern_info);
    rb_imemo_tmpbuf_set_ptr(tmpbuf, fpinfo);
    node = NEW_NODE(NODE_FNDPTN, 0, tmpbuf, fpinfo, loc);
    RB_OBJ_WRITTEN(p->ast, Qnil, tmpbuf);

    fpinfo->pre_rest_arg = pre_rest_arg ? pre_rest_arg : NODE_SPECIAL_NO_NAME_REST;
    fpinfo->args = args;
    fpinfo->post_rest_arg = post_rest_arg ? post_rest_arg : NODE_SPECIAL_NO_NAME_REST;

    p->ruby_sourceline = saved_line;
    return node;
}

static NODE*
new_hash_pattern(struct parser_params *p, NODE *constant, NODE *hshptn, const YYLTYPE *loc)
{
    hshptn->nd_pconst = constant;
    return hshptn;
}

static NODE*
new_hash_pattern_tail(struct parser_params *p, NODE *kw_args, ID kw_rest_arg, const YYLTYPE *loc)
{
    int saved_line = p->ruby_sourceline;
    NODE *node, *kw_rest_arg_node;

    if (kw_rest_arg == idNil) {
        kw_rest_arg_node = NODE_SPECIAL_NO_REST_KEYWORD;
    }
    else if (kw_rest_arg) {
        kw_rest_arg_node = assignable(p, kw_rest_arg, 0, loc);
    }
    else {
        kw_rest_arg_node = NULL;
    }

    node = NEW_NODE(NODE_HSHPTN, 0, kw_args, kw_rest_arg_node, loc);

    p->ruby_sourceline = saved_line;
    return node;
}

static NODE*
dsym_node(struct parser_params *p, NODE *node, const YYLTYPE *loc)
{
    VALUE lit;

    if (!node) {
        return NEW_LIT(ID2SYM(idNULL), loc);
    }

    switch (nd_type(node)) {
      case NODE_DSTR:
        nd_set_type(node, NODE_DSYM);
        nd_set_loc(node, loc);
        break;
      case NODE_STR:
        lit = node->nd_lit;
        RB_OBJ_WRITTEN(p->ast, Qnil, node->nd_lit = ID2SYM(rb_intern_str(lit)));
        nd_set_type(node, NODE_LIT);
        nd_set_loc(node, loc);
        break;
      default:
        node = NEW_NODE(NODE_DSYM, Qnil, 1, NEW_LIST(node, loc), loc);
        break;
    }
    return node;
}

static int
append_literal_keys(st_data_t k, st_data_t v, st_data_t h)
{
    NODE *node = (NODE *)v;
    NODE **result = (NODE **)h;
    node->nd_alen = 2;
    node->nd_next->nd_end = node->nd_next;
    node->nd_next->nd_next = 0;
    if (*result)
        list_concat(*result, node);
    else
        *result = node;
    return ST_CONTINUE;
}

static bool
hash_literal_key_p(VALUE k)
{
    switch (OBJ_BUILTIN_TYPE(k)) {
      case T_NODE:
        return false;
      default:
        return true;
    }
}

static int
literal_cmp(VALUE val, VALUE lit)
{
    if (val == lit) return 0;
    if (!hash_literal_key_p(val) || !hash_literal_key_p(lit)) return -1;
    return rb_iseq_cdhash_cmp(val, lit);
}

static st_index_t
literal_hash(VALUE a)
{
    if (!hash_literal_key_p(a)) return (st_index_t)a;
    return rb_iseq_cdhash_hash(a);
}

static const struct st_hash_type literal_type = {
    literal_cmp,
    literal_hash,
};

static NODE *
remove_duplicate_keys(struct parser_params *p, NODE *hash)
{
    st_table *literal_keys = st_init_table_with_size(&literal_type, hash->nd_alen / 2);
    NODE *result = 0;
    NODE *last_expr = 0;
    rb_code_location_t loc = hash->nd_loc;
    while (hash && hash->nd_head && hash->nd_next) {
        NODE *head = hash->nd_head;
        NODE *value = hash->nd_next;
        NODE *next = value->nd_next;
        st_data_t key = (st_data_t)head;
        st_data_t data;
        value->nd_next = 0;
        if (nd_type_p(head, NODE_LIT) &&
            st_delete(literal_keys, (key = (st_data_t)head->nd_lit, &key), &data)) {
            NODE *dup_value = ((NODE *)data)->nd_next;
            rb_compile_warn(p->ruby_sourcefile, nd_line((NODE *)data),
                            "key %+"PRIsVALUE" is duplicated and overwritten on line %d",
                            head->nd_lit, nd_line(head));
            if (dup_value == last_expr) {
                value->nd_head = block_append(p, dup_value->nd_head, value->nd_head);
            }
            else {
                last_expr->nd_head = block_append(p, dup_value->nd_head, last_expr->nd_head);
            }
        }
        st_insert(literal_keys, (st_data_t)key, (st_data_t)hash);
        last_expr = nd_type_p(head, NODE_LIT) ? value : head;
        hash = next;
    }
    st_foreach(literal_keys, append_literal_keys, (st_data_t)&result);
    st_free_table(literal_keys);
    if (hash) {
        if (!result) result = hash;
        else list_concat(result, hash);
    }
    result->nd_loc = loc;
    return result;
}

static NODE *
new_hash(struct parser_params *p, NODE *hash, const YYLTYPE *loc)
{
    if (hash) hash = remove_duplicate_keys(p, hash);
    return NEW_HASH(hash, loc);
}
#endif

static void
error_duplicate_pattern_variable(struct parser_params *p, ID id, const YYLTYPE *loc)
{
    if (is_private_local_id(id)) {
        return;
    }
    if (st_is_member(p->pvtbl, id)) {
        yyerror1(loc, "duplicated variable name");
    }
    else {
        st_insert(p->pvtbl, (st_data_t)id, 0);
    }
}

static void
error_duplicate_pattern_key(struct parser_params *p, VALUE key, const YYLTYPE *loc)
{
    if (!p->pktbl) {
        p->pktbl = st_init_numtable();
    }
    else if (st_is_member(p->pktbl, key)) {
        yyerror1(loc, "duplicated key name");
        return;
    }
    st_insert(p->pktbl, (st_data_t)key, 0);
}

#ifndef RIPPER
static NODE *
new_unique_key_hash(struct parser_params *p, NODE *hash, const YYLTYPE *loc)
{
    return NEW_HASH(hash, loc);
}
#endif /* !RIPPER */

#ifndef RIPPER
static NODE *
new_op_assign(struct parser_params *p, NODE *lhs, ID op, NODE *rhs, struct lex_context ctxt, const YYLTYPE *loc)
{
    NODE *asgn;

    if (lhs) {
        ID vid = lhs->nd_vid;
        YYLTYPE lhs_loc = lhs->nd_loc;
        int shareable = ctxt.shareable_constant_value;
        if (shareable) {
            switch (nd_type(lhs)) {
              case NODE_CDECL:
              case NODE_COLON2:
              case NODE_COLON3:
                break;
              default:
                shareable = 0;
                break;
            }
        }
        if (op == tOROP) {
            rhs = shareable_constant_value(p, shareable, lhs, rhs, &rhs->nd_loc);
            lhs->nd_value = rhs;
            nd_set_loc(lhs, loc);
            asgn = NEW_OP_ASGN_OR(gettable(p, vid, &lhs_loc), lhs, loc);
            if (is_notop_id(vid)) {
                switch (id_type(vid)) {
                  case ID_GLOBAL:
                  case ID_INSTANCE:
                  case ID_CLASS:
                    asgn->nd_aid = vid;
                }
            }
        }
        else if (op == tANDOP) {
            if (shareable) {
                rhs = shareable_constant_value(p, shareable, lhs, rhs, &rhs->nd_loc);
            }
            lhs->nd_value = rhs;
            nd_set_loc(lhs, loc);
            asgn = NEW_OP_ASGN_AND(gettable(p, vid, &lhs_loc), lhs, loc);
        }
        else {
            asgn = lhs;
            rhs = NEW_CALL(gettable(p, vid, &lhs_loc), op, NEW_LIST(rhs, &rhs->nd_loc), loc);
            if (shareable) {
                rhs = shareable_constant_value(p, shareable, lhs, rhs, &rhs->nd_loc);
            }
            asgn->nd_value = rhs;
            nd_set_loc(asgn, loc);
        }
    }
    else {
        asgn = NEW_BEGIN(0, loc);
    }
    return asgn;
}

static NODE *
new_ary_op_assign(struct parser_params *p, NODE *ary,
                  NODE *args, ID op, NODE *rhs, const YYLTYPE *args_loc, const YYLTYPE *loc)
{
    NODE *asgn;

    args = make_list(args, args_loc);
    if (nd_type_p(args, NODE_BLOCK_PASS)) {
        args = NEW_ARGSCAT(args, rhs, loc);
    }
    else {
        args = arg_concat(p, args, rhs, loc);
    }
    asgn = NEW_OP_ASGN1(ary, op, args, loc);
    fixpos(asgn, ary);
    return asgn;
}

static NODE *
new_attr_op_assign(struct parser_params *p, NODE *lhs,
                   ID atype, ID attr, ID op, NODE *rhs, const YYLTYPE *loc)
{
    NODE *asgn;

    asgn = NEW_OP_ASGN2(lhs, CALL_Q_P(atype), attr, op, rhs, loc);
    fixpos(asgn, lhs);
    return asgn;
}

static NODE *
new_const_op_assign(struct parser_params *p, NODE *lhs, ID op, NODE *rhs, struct lex_context ctxt, const YYLTYPE *loc)
{
    NODE *asgn;

    if (lhs) {
        rhs = shareable_constant_value(p, ctxt.shareable_constant_value, lhs, rhs, loc);
        asgn = NEW_OP_CDECL(lhs, op, rhs, loc);
    }
    else {
        asgn = NEW_BEGIN(0, loc);
    }
    fixpos(asgn, lhs);
    return asgn;
}

static NODE *
const_decl(struct parser_params *p, NODE *path, const YYLTYPE *loc)
{
    if (p->ctxt.in_def) {
        yyerror1(loc, "dynamic constant assignment");
    }
    return NEW_CDECL(0, 0, (path), loc);
}
#else
static VALUE
const_decl(struct parser_params *p, VALUE path)
{
    if (p->ctxt.in_def) {
        path = assign_error(p, "dynamic constant assignment", path);
    }
    return path;
}

static VALUE
assign_error(struct parser_params *p, const char *mesg, VALUE a)
{
    a = dispatch2(assign_error, ERR_MESG(), a);
    ripper_error(p);
    return a;
}

static VALUE
var_field(struct parser_params *p, VALUE a)
{
    return ripper_new_yylval(p, get_id(a), dispatch1(var_field, a), 0);
}
#endif

#ifndef RIPPER
static NODE *
new_bodystmt(struct parser_params *p, NODE *head, NODE *rescue, NODE *rescue_else, NODE *ensure, const YYLTYPE *loc)
{
    NODE *result = head;
    if (rescue) {
        NODE *tmp = rescue_else ? rescue_else : rescue;
        YYLTYPE rescue_loc = code_loc_gen(&head->nd_loc, &tmp->nd_loc);

        result = NEW_RESCUE(head, rescue, rescue_else, &rescue_loc);
        nd_set_line(result, rescue->nd_loc.beg_pos.lineno);
    }
    else if (rescue_else) {
        result = block_append(p, result, rescue_else);
    }
    if (ensure) {
        result = NEW_ENSURE(result, ensure, loc);
    }
    fixpos(result, head);
    return result;
}
#endif

static void
warn_unused_var(struct parser_params *p, struct local_vars *local)
{
    int cnt;

    if (!local->used) return;
    cnt = local->used->pos;
    if (cnt != local->vars->pos) {
        rb_parser_fatal(p, "local->used->pos != local->vars->pos");
    }
#ifndef RIPPER
    ID *v = local->vars->tbl;
    ID *u = local->used->tbl;
    for (int i = 0; i < cnt; ++i) {
        if (!v[i] || (u[i] & LVAR_USED)) continue;
        if (is_private_local_id(v[i])) continue;
        rb_warn1L((int)u[i], "assigned but unused variable - %"PRIsWARN, rb_id2str(v[i]));
    }
#endif
}

static void
local_push(struct parser_params *p, int toplevel_scope)
{
    struct local_vars *local;
    int inherits_dvars = toplevel_scope && compile_for_eval;
    int warn_unused_vars = RTEST(ruby_verbose);

    local = ALLOC(struct local_vars);
    local->prev = p->lvtbl;
    local->args = vtable_alloc(0);
    local->vars = vtable_alloc(inherits_dvars ? DVARS_INHERIT : DVARS_TOPSCOPE);
#ifndef RIPPER
    if (toplevel_scope && compile_for_eval) warn_unused_vars = 0;
    if (toplevel_scope && e_option_supplied(p)) warn_unused_vars = 0;
    local->numparam.outer = 0;
    local->numparam.inner = 0;
    local->numparam.current = 0;
#endif
    local->used = warn_unused_vars ? vtable_alloc(0) : 0;

# if WARN_PAST_SCOPE
    local->past = 0;
# endif
    CMDARG_PUSH(0);
    COND_PUSH(0);
    p->lvtbl = local;
}

static void
local_pop(struct parser_params *p)
{
    struct local_vars *local = p->lvtbl->prev;
    if (p->lvtbl->used) {
        warn_unused_var(p, p->lvtbl);
        vtable_free(p->lvtbl->used);
    }
# if WARN_PAST_SCOPE
    while (p->lvtbl->past) {
        struct vtable *past = p->lvtbl->past;
        p->lvtbl->past = past->prev;
        vtable_free(past);
    }
# endif
    vtable_free(p->lvtbl->args);
    vtable_free(p->lvtbl->vars);
    CMDARG_POP();
    COND_POP();
    ruby_sized_xfree(p->lvtbl, sizeof(*p->lvtbl));
    p->lvtbl = local;
}

#ifndef RIPPER
static rb_ast_id_table_t *
local_tbl(struct parser_params *p)
{
    int cnt_args = vtable_size(p->lvtbl->args);
    int cnt_vars = vtable_size(p->lvtbl->vars);
    int cnt = cnt_args + cnt_vars;
    int i, j;
    rb_ast_id_table_t *tbl;

    if (cnt <= 0) return 0;
    tbl = rb_ast_new_local_table(p->ast, cnt);
    MEMCPY(tbl->ids, p->lvtbl->args->tbl, ID, cnt_args);
    /* remove IDs duplicated to warn shadowing */
    for (i = 0, j = cnt_args; i < cnt_vars; ++i) {
        ID id = p->lvtbl->vars->tbl[i];
        if (!vtable_included(p->lvtbl->args, id)) {
            tbl->ids[j++] = id;
        }
    }
    if (j < cnt) {
        tbl = rb_ast_resize_latest_local_table(p->ast, j);
    }

    return tbl;
}

static NODE*
node_newnode_with_locals(struct parser_params *p, enum node_type type, VALUE a1, VALUE a2, const rb_code_location_t *loc)
{
    rb_ast_id_table_t *a0;
    NODE *n;

    a0 = local_tbl(p);
    n = NEW_NODE(type, a0, a1, a2, loc);
    return n;
}

#endif

static void
numparam_name(struct parser_params *p, ID id)
{
    if (!NUMPARAM_ID_P(id)) return;
    compile_error(p, "_%d is reserved for numbered parameter",
        NUMPARAM_ID_TO_IDX(id));
}

static void
arg_var(struct parser_params *p, ID id)
{
    numparam_name(p, id);
    vtable_add(p->lvtbl->args, id);
}

static void
local_var(struct parser_params *p, ID id)
{
    numparam_name(p, id);
    vtable_add(p->lvtbl->vars, id);
    if (p->lvtbl->used) {
        vtable_add(p->lvtbl->used, (ID)p->ruby_sourceline);
    }
}

static int
local_id_ref(struct parser_params *p, ID id, ID **vidrefp)
{
    struct vtable *vars, *args, *used;

    vars = p->lvtbl->vars;
    args = p->lvtbl->args;
    used = p->lvtbl->used;

    while (vars && !DVARS_TERMINAL_P(vars->prev)) {
        vars = vars->prev;
        args = args->prev;
        if (used) used = used->prev;
    }

    if (vars && vars->prev == DVARS_INHERIT) {
        return rb_local_defined(id, p->parent_iseq);
    }
    else if (vtable_included(args, id)) {
        return 1;
    }
    else {
        int i = vtable_included(vars, id);
        if (i && used && vidrefp) *vidrefp = &used->tbl[i-1];
        return i != 0;
    }
}

static int
local_id(struct parser_params *p, ID id)
{
    return local_id_ref(p, id, NULL);
}

static int
check_forwarding_args(struct parser_params *p)
{
    if (local_id(p, idFWD_ALL)) return TRUE;
    compile_error(p, "unexpected ...");
    return FALSE;
}

static void
add_forwarding_args(struct parser_params *p)
{
    arg_var(p, idFWD_REST);
#ifndef FORWARD_ARGS_WITH_RUBY2_KEYWORDS
    arg_var(p, idFWD_KWREST);
#endif
    arg_var(p, idFWD_BLOCK);
    arg_var(p, idFWD_ALL);
}

#ifndef RIPPER
static NODE *
new_args_forward_call(struct parser_params *p, NODE *leading, const YYLTYPE *loc, const YYLTYPE *argsloc)
{
    NODE *rest = NEW_LVAR(idFWD_REST, loc);
#ifndef FORWARD_ARGS_WITH_RUBY2_KEYWORDS
    NODE *kwrest = list_append(p, NEW_LIST(0, loc), NEW_LVAR(idFWD_KWREST, loc));
#endif
    NODE *block = NEW_BLOCK_PASS(NEW_LVAR(idFWD_BLOCK, loc), loc);
    NODE *args = leading ? rest_arg_append(p, leading, rest, argsloc) : NEW_SPLAT(rest, loc);
#ifndef FORWARD_ARGS_WITH_RUBY2_KEYWORDS
    args = arg_append(p, args, new_hash(p, kwrest, loc), loc);
#endif
    return arg_blk_pass(args, block);
}
#endif

static NODE *
numparam_push(struct parser_params *p)
{
#ifndef RIPPER
    struct local_vars *local = p->lvtbl;
    NODE *inner = local->numparam.inner;
    if (!local->numparam.outer) {
        local->numparam.outer = local->numparam.current;
    }
    local->numparam.inner = 0;
    local->numparam.current = 0;
    return inner;
#else
    return 0;
#endif
}

static void
numparam_pop(struct parser_params *p, NODE *prev_inner)
{
#ifndef RIPPER
    struct local_vars *local = p->lvtbl;
    if (prev_inner) {
        /* prefer first one */
        local->numparam.inner = prev_inner;
    }
    else if (local->numparam.current) {
        /* current and inner are exclusive */
        local->numparam.inner = local->numparam.current;
    }
    if (p->max_numparam > NO_PARAM) {
        /* current and outer are exclusive */
        local->numparam.current = local->numparam.outer;
        local->numparam.outer = 0;
    }
    else {
        /* no numbered parameter */
        local->numparam.current = 0;
    }
#endif
}

static const struct vtable *
dyna_push(struct parser_params *p)
{
    p->lvtbl->args = vtable_alloc(p->lvtbl->args);
    p->lvtbl->vars = vtable_alloc(p->lvtbl->vars);
    if (p->lvtbl->used) {
        p->lvtbl->used = vtable_alloc(p->lvtbl->used);
    }
    return p->lvtbl->args;
}

static void
dyna_pop_vtable(struct parser_params *p, struct vtable **vtblp)
{
    struct vtable *tmp = *vtblp;
    *vtblp = tmp->prev;
# if WARN_PAST_SCOPE
    if (p->past_scope_enabled) {
        tmp->prev = p->lvtbl->past;
        p->lvtbl->past = tmp;
        return;
    }
# endif
    vtable_free(tmp);
}

static void
dyna_pop_1(struct parser_params *p)
{
    struct vtable *tmp;

    if ((tmp = p->lvtbl->used) != 0) {
        warn_unused_var(p, p->lvtbl);
        p->lvtbl->used = p->lvtbl->used->prev;
        vtable_free(tmp);
    }
    dyna_pop_vtable(p, &p->lvtbl->args);
    dyna_pop_vtable(p, &p->lvtbl->vars);
}

static void
dyna_pop(struct parser_params *p, const struct vtable *lvargs)
{
    while (p->lvtbl->args != lvargs) {
        dyna_pop_1(p);
        if (!p->lvtbl->args) {
            struct local_vars *local = p->lvtbl->prev;
            ruby_sized_xfree(p->lvtbl, sizeof(*p->lvtbl));
            p->lvtbl = local;
        }
    }
    dyna_pop_1(p);
}

static int
dyna_in_block(struct parser_params *p)
{
    return !DVARS_TERMINAL_P(p->lvtbl->vars) && p->lvtbl->vars->prev != DVARS_TOPSCOPE;
}

static int
dvar_defined_ref(struct parser_params *p, ID id, ID **vidrefp)
{
    struct vtable *vars, *args, *used;
    int i;

    args = p->lvtbl->args;
    vars = p->lvtbl->vars;
    used = p->lvtbl->used;

    while (!DVARS_TERMINAL_P(vars)) {
        if (vtable_included(args, id)) {
            return 1;
        }
        if ((i = vtable_included(vars, id)) != 0) {
            if (used && vidrefp) *vidrefp = &used->tbl[i-1];
            return 1;
        }
        args = args->prev;
        vars = vars->prev;
        if (!vidrefp) used = 0;
        if (used) used = used->prev;
    }

    if (vars == DVARS_INHERIT && !NUMPARAM_ID_P(id)) {
        return rb_dvar_defined(id, p->parent_iseq);
    }

    return 0;
}

static int
dvar_defined(struct parser_params *p, ID id)
{
    return dvar_defined_ref(p, id, NULL);
}

static int
dvar_curr(struct parser_params *p, ID id)
{
    return (vtable_included(p->lvtbl->args, id) ||
            vtable_included(p->lvtbl->vars, id));
}

static void
reg_fragment_enc_error(struct parser_params* p, VALUE str, int c)
{
    compile_error(p,
        "regexp encoding option '%c' differs from source encoding '%s'",
        c, rb_enc_name(rb_enc_get(str)));
}

#ifndef RIPPER
int
rb_reg_fragment_setenc(struct parser_params* p, VALUE str, int options)
{
    int c = RE_OPTION_ENCODING_IDX(options);

    if (c) {
        int opt, idx;
        rb_char_to_option_kcode(c, &opt, &idx);
        if (idx != ENCODING_GET(str) &&
            !is_ascii_string(str)) {
            goto error;
        }
        ENCODING_SET(str, idx);
    }
    else if (RE_OPTION_ENCODING_NONE(options)) {
        if (!ENCODING_IS_ASCII8BIT(str) &&
            !is_ascii_string(str)) {
            c = 'n';
            goto error;
        }
        rb_enc_associate(str, rb_ascii8bit_encoding());
    }
    else if (rb_is_usascii_enc(p->enc)) {
        if (!is_ascii_string(str)) {
            /* raise in re.c */
            rb_enc_associate(str, rb_usascii_encoding());
        }
        else {
            rb_enc_associate(str, rb_ascii8bit_encoding());
        }
    }
    return 0;

  error:
    return c;
}

static void
reg_fragment_setenc(struct parser_params* p, VALUE str, int options)
{
    int c = rb_reg_fragment_setenc(p, str, options);
    if (c) reg_fragment_enc_error(p, str, c);
}

static int
reg_fragment_check(struct parser_params* p, VALUE str, int options)
{
    VALUE err;
    reg_fragment_setenc(p, str, options);
    err = rb_reg_check_preprocess(str);
    if (err != Qnil) {
        err = rb_obj_as_string(err);
        compile_error(p, "%"PRIsVALUE, err);
        return 0;
    }
    return 1;
}

typedef struct {
    struct parser_params* parser;
    rb_encoding *enc;
    NODE *succ_block;
    const YYLTYPE *loc;
} reg_named_capture_assign_t;

static int
reg_named_capture_assign_iter(const OnigUChar *name, const OnigUChar *name_end,
          int back_num, int *back_refs, OnigRegex regex, void *arg0)
{
    reg_named_capture_assign_t *arg = (reg_named_capture_assign_t*)arg0;
    struct parser_params* p = arg->parser;
    rb_encoding *enc = arg->enc;
    long len = name_end - name;
    const char *s = (const char *)name;
    ID var;
    NODE *node, *succ;

    if (!len) return ST_CONTINUE;
    if (rb_enc_symname_type(s, len, enc, (1U<<ID_LOCAL)) != ID_LOCAL)
        return ST_CONTINUE;

    var = intern_cstr(s, len, enc);
    if (len < MAX_WORD_LENGTH && rb_reserved_word(s, (int)len)) {
        if (!lvar_defined(p, var)) return ST_CONTINUE;
    }
    node = node_assign(p, assignable(p, var, 0, arg->loc), NEW_LIT(ID2SYM(var), arg->loc), NO_LEX_CTXT, arg->loc);
    succ = arg->succ_block;
    if (!succ) succ = NEW_BEGIN(0, arg->loc);
    succ = block_append(p, succ, node);
    arg->succ_block = succ;
    return ST_CONTINUE;
}

static NODE *
reg_named_capture_assign(struct parser_params* p, VALUE regexp, const YYLTYPE *loc)
{
    reg_named_capture_assign_t arg;

    arg.parser = p;
    arg.enc = rb_enc_get(regexp);
    arg.succ_block = 0;
    arg.loc = loc;
    onig_foreach_name(RREGEXP_PTR(regexp), reg_named_capture_assign_iter, &arg);

    if (!arg.succ_block) return 0;
    return arg.succ_block->nd_next;
}

static VALUE
parser_reg_compile(struct parser_params* p, VALUE str, int options)
{
    reg_fragment_setenc(p, str, options);
    return rb_parser_reg_compile(p, str, options);
}

VALUE
rb_parser_reg_compile(struct parser_params* p, VALUE str, int options)
{
    return rb_reg_compile(str, options & RE_OPTION_MASK, p->ruby_sourcefile, p->ruby_sourceline);
}

static VALUE
reg_compile(struct parser_params* p, VALUE str, int options)
{
    VALUE re;
    VALUE err;

    err = rb_errinfo();
    re = parser_reg_compile(p, str, options);
    if (NIL_P(re)) {
        VALUE m = rb_attr_get(rb_errinfo(), idMesg);
        rb_set_errinfo(err);
        compile_error(p, "%"PRIsVALUE, m);
        return Qnil;
    }
    return re;
}
#else
static VALUE
parser_reg_compile(struct parser_params* p, VALUE str, int options, VALUE *errmsg)
{
    VALUE err = rb_errinfo();
    VALUE re;
    str = ripper_is_node_yylval(str) ? RNODE(str)->nd_cval : str;
    int c = rb_reg_fragment_setenc(p, str, options);
    if (c) reg_fragment_enc_error(p, str, c);
    re = rb_parser_reg_compile(p, str, options);
    if (NIL_P(re)) {
        *errmsg = rb_attr_get(rb_errinfo(), idMesg);
        rb_set_errinfo(err);
    }
    return re;
}
#endif

#ifndef RIPPER
void
rb_parser_set_options(VALUE vparser, int print, int loop, int chomp, int split)
{
    struct parser_params *p;
    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, p);
    p->do_print = print;
    p->do_loop = loop;
    p->do_chomp = chomp;
    p->do_split = split;
}

static NODE *
parser_append_options(struct parser_params *p, NODE *node)
{
    static const YYLTYPE default_location = {{1, 0}, {1, 0}};
    const YYLTYPE *const LOC = &default_location;

    if (p->do_print) {
        NODE *print = NEW_FCALL(rb_intern("print"),
                                NEW_LIST(NEW_GVAR(idLASTLINE, LOC), LOC),
                                LOC);
        node = block_append(p, node, print);
    }

    if (p->do_loop) {
        NODE *irs = NEW_LIST(NEW_GVAR(rb_intern("$/"), LOC), LOC);

        if (p->do_split) {
            ID ifs = rb_intern("$;");
            ID fields = rb_intern("$F");
            NODE *args = NEW_LIST(NEW_GVAR(ifs, LOC), LOC);
            NODE *split = NEW_GASGN(fields,
                                    NEW_CALL(NEW_GVAR(idLASTLINE, LOC),
                                             rb_intern("split"), args, LOC),
                                    LOC);
            node = block_append(p, split, node);
        }
        if (p->do_chomp) {
            NODE *chomp = NEW_LIT(ID2SYM(rb_intern("chomp")), LOC);
            chomp = list_append(p, NEW_LIST(chomp, LOC), NEW_TRUE(LOC));
            irs = list_append(p, irs, NEW_HASH(chomp, LOC));
        }

        node = NEW_WHILE(NEW_FCALL(idGets, irs, LOC), node, 1, LOC);
    }

    return node;
}

void
rb_init_parse(void)
{
    /* just to suppress unused-function warnings */
    (void)nodetype;
    (void)nodeline;
}

static ID
internal_id(struct parser_params *p)
{
    return rb_make_temporary_id(vtable_size(p->lvtbl->args) + vtable_size(p->lvtbl->vars));
}
#endif /* !RIPPER */

static void
parser_initialize(struct parser_params *p)
{
    /* note: we rely on TypedData_Make_Struct to set most fields to 0 */
    p->command_start = TRUE;
    p->ruby_sourcefile_string = Qnil;
    p->lex.lpar_beg = -1; /* make lambda_beginning_p() == FALSE at first */
    p->node_id = 0;
    p->delayed.token = Qnil;
#ifdef RIPPER
    p->result = Qnil;
    p->parsing_thread = Qnil;
#else
    p->error_buffer = Qfalse;
    p->end_expect_token_locations = Qnil;
    p->token_id = 0;
    p->tokens = Qnil;
#endif
    p->debug_buffer = Qnil;
    p->debug_output = rb_ractor_stdout();
    p->enc = rb_utf8_encoding();
}

#ifdef RIPPER
#define parser_mark ripper_parser_mark
#define parser_free ripper_parser_free
#endif

static void
parser_mark(void *ptr)
{
    struct parser_params *p = (struct parser_params*)ptr;

    rb_gc_mark(p->lex.input);
    rb_gc_mark(p->lex.lastline);
    rb_gc_mark(p->lex.nextline);
    rb_gc_mark(p->ruby_sourcefile_string);
    rb_gc_mark((VALUE)p->lex.strterm);
    rb_gc_mark((VALUE)p->ast);
    rb_gc_mark(p->case_labels);
    rb_gc_mark(p->delayed.token);
#ifndef RIPPER
    rb_gc_mark(p->debug_lines);
    rb_gc_mark(p->compile_option);
    rb_gc_mark(p->error_buffer);
    rb_gc_mark(p->end_expect_token_locations);
    rb_gc_mark(p->tokens);
#else
    rb_gc_mark(p->value);
    rb_gc_mark(p->result);
    rb_gc_mark(p->parsing_thread);
#endif
    rb_gc_mark(p->debug_buffer);
    rb_gc_mark(p->debug_output);
#ifdef YYMALLOC
    rb_gc_mark((VALUE)p->heap);
#endif
}

static void
parser_free(void *ptr)
{
    struct parser_params *p = (struct parser_params*)ptr;
    struct local_vars *local, *prev;

    if (p->tokenbuf) {
        ruby_sized_xfree(p->tokenbuf, p->toksiz);
    }
    for (local = p->lvtbl; local; local = prev) {
        if (local->vars) xfree(local->vars);
        prev = local->prev;
        xfree(local);
    }
    {
        token_info *ptinfo;
        while ((ptinfo = p->token_info) != 0) {
            p->token_info = ptinfo->next;
            xfree(ptinfo);
        }
    }
    xfree(ptr);
}

static size_t
parser_memsize(const void *ptr)
{
    struct parser_params *p = (struct parser_params*)ptr;
    struct local_vars *local;
    size_t size = sizeof(*p);

    size += p->toksiz;
    for (local = p->lvtbl; local; local = local->prev) {
        size += sizeof(*local);
        if (local->vars) size += local->vars->capa * sizeof(ID);
    }
    return size;
}

static const rb_data_type_t parser_data_type = {
#ifndef RIPPER
    "parser",
#else
    "ripper",
#endif
    {
        parser_mark,
        parser_free,
        parser_memsize,
    },
    0, 0, RUBY_TYPED_FREE_IMMEDIATELY
};

#ifndef RIPPER
#undef rb_reserved_word

const struct kwtable *
rb_reserved_word(const char *str, unsigned int len)
{
    return reserved_word(str, len);
}

VALUE
rb_parser_new(void)
{
    struct parser_params *p;
    VALUE parser = TypedData_Make_Struct(0, struct parser_params,
                                         &parser_data_type, p);
    parser_initialize(p);
    return parser;
}

VALUE
rb_parser_set_context(VALUE vparser, const struct rb_iseq_struct *base, int main)
{
    struct parser_params *p;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, p);
    p->error_buffer = main ? Qfalse : Qnil;
    p->parent_iseq = base;
    return vparser;
}

void
rb_parser_keep_script_lines(VALUE vparser)
{
    struct parser_params *p;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, p);
    p->keep_script_lines = 1;
}

void
rb_parser_error_tolerant(VALUE vparser)
{
    struct parser_params *p;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, p);
    p->error_tolerant = 1;
    p->end_expect_token_locations = rb_ary_new();
}

void
rb_parser_keep_tokens(VALUE vparser)
{
    struct parser_params *p;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, p);
    p->keep_tokens = 1;
    p->tokens = rb_ary_new();
}

#endif

#ifdef RIPPER
#define rb_parser_end_seen_p ripper_parser_end_seen_p
#define rb_parser_encoding ripper_parser_encoding
#define rb_parser_get_yydebug ripper_parser_get_yydebug
#define rb_parser_set_yydebug ripper_parser_set_yydebug
#define rb_parser_get_debug_output ripper_parser_get_debug_output
#define rb_parser_set_debug_output ripper_parser_set_debug_output
static VALUE ripper_parser_end_seen_p(VALUE vparser);
static VALUE ripper_parser_encoding(VALUE vparser);
static VALUE ripper_parser_get_yydebug(VALUE self);
static VALUE ripper_parser_set_yydebug(VALUE self, VALUE flag);
static VALUE ripper_parser_get_debug_output(VALUE self);
static VALUE ripper_parser_set_debug_output(VALUE self, VALUE output);

/*
 *  call-seq:
 *    ripper.error?   -> Boolean
 *
 *  Return true if parsed source has errors.
 */
static VALUE
ripper_error_p(VALUE vparser)
{
    struct parser_params *p;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, p);
    return RBOOL(p->error_p);
}
#endif

/*
 *  call-seq:
 *    ripper.end_seen?   -> Boolean
 *
 *  Return true if parsed source ended by +\_\_END\_\_+.
 */
VALUE
rb_parser_end_seen_p(VALUE vparser)
{
    struct parser_params *p;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, p);
    return RBOOL(p->ruby__end__seen);
}

/*
 *  call-seq:
 *    ripper.encoding   -> encoding
 *
 *  Return encoding of the source.
 */
VALUE
rb_parser_encoding(VALUE vparser)
{
    struct parser_params *p;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, p);
    return rb_enc_from_encoding(p->enc);
}

#ifdef RIPPER
/*
 *  call-seq:
 *    ripper.yydebug   -> true or false
 *
 *  Get yydebug.
 */
VALUE
rb_parser_get_yydebug(VALUE self)
{
    struct parser_params *p;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, p);
    return RBOOL(p->debug);
}
#endif

/*
 *  call-seq:
 *    ripper.yydebug = flag
 *
 *  Set yydebug.
 */
VALUE
rb_parser_set_yydebug(VALUE self, VALUE flag)
{
    struct parser_params *p;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, p);
    p->debug = RTEST(flag);
    return flag;
}

/*
 *  call-seq:
 *    ripper.debug_output   -> obj
 *
 *  Get debug output.
 */
VALUE
rb_parser_get_debug_output(VALUE self)
{
    struct parser_params *p;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, p);
    return p->debug_output;
}

/*
 *  call-seq:
 *    ripper.debug_output = obj
 *
 *  Set debug output.
 */
VALUE
rb_parser_set_debug_output(VALUE self, VALUE output)
{
    struct parser_params *p;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, p);
    return p->debug_output = output;
}

#ifndef RIPPER
#ifdef YYMALLOC
#define HEAPCNT(n, size) ((n) * (size) / sizeof(YYSTYPE))
/* Keep the order; NEWHEAP then xmalloc and ADD2HEAP to get rid of
 * potential memory leak */
#define NEWHEAP() rb_imemo_tmpbuf_parser_heap(0, p->heap, 0)
#define ADD2HEAP(new, cnt, ptr) ((p->heap = (new))->ptr = (ptr), \
                           (new)->cnt = (cnt), (ptr))

void *
rb_parser_malloc(struct parser_params *p, size_t size)
{
    size_t cnt = HEAPCNT(1, size);
    rb_imemo_tmpbuf_t *n = NEWHEAP();
    void *ptr = xmalloc(size);

    return ADD2HEAP(n, cnt, ptr);
}

void *
rb_parser_calloc(struct parser_params *p, size_t nelem, size_t size)
{
    size_t cnt = HEAPCNT(nelem, size);
    rb_imemo_tmpbuf_t *n = NEWHEAP();
    void *ptr = xcalloc(nelem, size);

    return ADD2HEAP(n, cnt, ptr);
}

void *
rb_parser_realloc(struct parser_params *p, void *ptr, size_t size)
{
    rb_imemo_tmpbuf_t *n;
    size_t cnt = HEAPCNT(1, size);

    if (ptr && (n = p->heap) != NULL) {
        do {
            if (n->ptr == ptr) {
                n->ptr = ptr = xrealloc(ptr, size);
                if (n->cnt) n->cnt = cnt;
                return ptr;
            }
        } while ((n = n->next) != NULL);
    }
    n = NEWHEAP();
    ptr = xrealloc(ptr, size);
    return ADD2HEAP(n, cnt, ptr);
}

void
rb_parser_free(struct parser_params *p, void *ptr)
{
    rb_imemo_tmpbuf_t **prev = &p->heap, *n;

    while ((n = *prev) != NULL) {
        if (n->ptr == ptr) {
            *prev = n->next;
            break;
        }
        prev = &n->next;
    }
}
#endif

void
rb_parser_printf(struct parser_params *p, const char *fmt, ...)
{
    va_list ap;
    VALUE mesg = p->debug_buffer;

    if (NIL_P(mesg)) p->debug_buffer = mesg = rb_str_new(0, 0);
    va_start(ap, fmt);
    rb_str_vcatf(mesg, fmt, ap);
    va_end(ap);
    if (RSTRING_END(mesg)[-1] == '\n') {
        rb_io_write(p->debug_output, mesg);
        p->debug_buffer = Qnil;
    }
}

static void
parser_compile_error(struct parser_params *p, const char *fmt, ...)
{
    va_list ap;

    rb_io_flush(p->debug_output);
    p->error_p = 1;
    va_start(ap, fmt);
    p->error_buffer =
        rb_syntax_error_append(p->error_buffer,
                               p->ruby_sourcefile_string,
                               p->ruby_sourceline,
                               rb_long2int(p->lex.pcur - p->lex.pbeg),
                               p->enc, fmt, ap);
    va_end(ap);
}

static size_t
count_char(const char *str, int c)
{
    int n = 0;
    while (str[n] == c) ++n;
    return n;
}

/*
 * strip enclosing double-quotes, same as the default yytnamerr except
 * for that single-quotes matching back-quotes do not stop stripping.
 *
 *  "\"`class' keyword\"" => "`class' keyword"
 */
RUBY_FUNC_EXPORTED size_t
rb_yytnamerr(struct parser_params *p, char *yyres, const char *yystr)
{
    if (*yystr == '"') {
        size_t yyn = 0, bquote = 0;
        const char *yyp = yystr;

        while (*++yyp) {
            switch (*yyp) {
              case '`':
                if (!bquote) {
                    bquote = count_char(yyp+1, '`') + 1;
                    if (yyres) memcpy(&yyres[yyn], yyp, bquote);
                    yyn += bquote;
                    yyp += bquote - 1;
                    break;
                }
                goto default_char;

              case '\'':
                if (bquote && count_char(yyp+1, '\'') + 1 == bquote) {
                    if (yyres) memcpy(yyres + yyn, yyp, bquote);
                    yyn += bquote;
                    yyp += bquote - 1;
                    bquote = 0;
                    break;
                }
                if (yyp[1] && yyp[1] != '\'' && yyp[2] == '\'') {
                    if (yyres) memcpy(yyres + yyn, yyp, 3);
                    yyn += 3;
                    yyp += 2;
                    break;
                }
                goto do_not_strip_quotes;

              case ',':
                goto do_not_strip_quotes;

              case '\\':
                if (*++yyp != '\\')
                    goto do_not_strip_quotes;
                /* Fall through.  */
              default_char:
              default:
                if (yyres)
                    yyres[yyn] = *yyp;
                yyn++;
                break;

              case '"':
              case '\0':
                if (yyres)
                    yyres[yyn] = '\0';
                return yyn;
            }
        }
      do_not_strip_quotes: ;
    }

    if (!yyres) return strlen(yystr);

    return (YYSIZE_T)(yystpcpy(yyres, yystr) - yyres);
}
#endif

#ifdef RIPPER
#ifdef RIPPER_DEBUG
/* :nodoc: */
static VALUE
ripper_validate_object(VALUE self, VALUE x)
{
    if (x == Qfalse) return x;
    if (x == Qtrue) return x;
    if (NIL_P(x)) return x;
    if (UNDEF_P(x))
        rb_raise(rb_eArgError, "Qundef given");
    if (FIXNUM_P(x)) return x;
    if (SYMBOL_P(x)) return x;
    switch (BUILTIN_TYPE(x)) {
      case T_STRING:
      case T_OBJECT:
      case T_ARRAY:
      case T_BIGNUM:
      case T_FLOAT:
      case T_COMPLEX:
      case T_RATIONAL:
        break;
      case T_NODE:
        if (!nd_type_p((NODE *)x, NODE_RIPPER)) {
            rb_raise(rb_eArgError, "NODE given: %p", (void *)x);
        }
        x = ((NODE *)x)->nd_rval;
        break;
      default:
        rb_raise(rb_eArgError, "wrong type of ruby object: %p (%s)",
                 (void *)x, rb_obj_classname(x));
    }
    if (!RBASIC_CLASS(x)) {
        rb_raise(rb_eArgError, "hidden ruby object: %p (%s)",
                 (void *)x, rb_builtin_type_name(TYPE(x)));
    }
    return x;
}
#endif

#define validate(x) ((x) = get_value(x))

static VALUE
ripper_dispatch0(struct parser_params *p, ID mid)
{
    return rb_funcall(p->value, mid, 0);
}

static VALUE
ripper_dispatch1(struct parser_params *p, ID mid, VALUE a)
{
    validate(a);
    return rb_funcall(p->value, mid, 1, a);
}

static VALUE
ripper_dispatch2(struct parser_params *p, ID mid, VALUE a, VALUE b)
{
    validate(a);
    validate(b);
    return rb_funcall(p->value, mid, 2, a, b);
}

static VALUE
ripper_dispatch3(struct parser_params *p, ID mid, VALUE a, VALUE b, VALUE c)
{
    validate(a);
    validate(b);
    validate(c);
    return rb_funcall(p->value, mid, 3, a, b, c);
}

static VALUE
ripper_dispatch4(struct parser_params *p, ID mid, VALUE a, VALUE b, VALUE c, VALUE d)
{
    validate(a);
    validate(b);
    validate(c);
    validate(d);
    return rb_funcall(p->value, mid, 4, a, b, c, d);
}

static VALUE
ripper_dispatch5(struct parser_params *p, ID mid, VALUE a, VALUE b, VALUE c, VALUE d, VALUE e)
{
    validate(a);
    validate(b);
    validate(c);
    validate(d);
    validate(e);
    return rb_funcall(p->value, mid, 5, a, b, c, d, e);
}

static VALUE
ripper_dispatch7(struct parser_params *p, ID mid, VALUE a, VALUE b, VALUE c, VALUE d, VALUE e, VALUE f, VALUE g)
{
    validate(a);
    validate(b);
    validate(c);
    validate(d);
    validate(e);
    validate(f);
    validate(g);
    return rb_funcall(p->value, mid, 7, a, b, c, d, e, f, g);
}

static ID
ripper_get_id(VALUE v)
{
    NODE *nd;
    if (!RB_TYPE_P(v, T_NODE)) return 0;
    nd = (NODE *)v;
    if (!nd_type_p(nd, NODE_RIPPER)) return 0;
    return nd->nd_vid;
}

static VALUE
ripper_get_value(VALUE v)
{
    NODE *nd;
    if (UNDEF_P(v)) return Qnil;
    if (!RB_TYPE_P(v, T_NODE)) return v;
    nd = (NODE *)v;
    if (!nd_type_p(nd, NODE_RIPPER)) return Qnil;
    return nd->nd_rval;
}

static void
ripper_error(struct parser_params *p)
{
    p->error_p = TRUE;
}

static void
ripper_compile_error(struct parser_params *p, const char *fmt, ...)
{
    VALUE str;
    va_list args;

    va_start(args, fmt);
    str = rb_vsprintf(fmt, args);
    va_end(args);
    rb_funcall(p->value, rb_intern("compile_error"), 1, str);
    ripper_error(p);
}

static VALUE
ripper_lex_get_generic(struct parser_params *p, VALUE src)
{
    VALUE line = rb_funcallv_public(src, id_gets, 0, 0);
    if (!NIL_P(line) && !RB_TYPE_P(line, T_STRING)) {
        rb_raise(rb_eTypeError,
                 "gets returned %"PRIsVALUE" (expected String or nil)",
                 rb_obj_class(line));
    }
    return line;
}

static VALUE
ripper_lex_io_get(struct parser_params *p, VALUE src)
{
    return rb_io_gets(src);
}

static VALUE
ripper_s_allocate(VALUE klass)
{
    struct parser_params *p;
    VALUE self = TypedData_Make_Struct(klass, struct parser_params,
                                       &parser_data_type, p);
    p->value = self;
    return self;
}

#define ripper_initialized_p(r) ((r)->lex.input != 0)

/*
 *  call-seq:
 *    Ripper.new(src, filename="(ripper)", lineno=1) -> ripper
 *
 *  Create a new Ripper object.
 *  _src_ must be a String, an IO, or an Object which has #gets method.
 *
 *  This method does not starts parsing.
 *  See also Ripper#parse and Ripper.parse.
 */
static VALUE
ripper_initialize(int argc, VALUE *argv, VALUE self)
{
    struct parser_params *p;
    VALUE src, fname, lineno;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, p);
    rb_scan_args(argc, argv, "12", &src, &fname, &lineno);
    if (RB_TYPE_P(src, T_FILE)) {
        p->lex.gets = ripper_lex_io_get;
    }
    else if (rb_respond_to(src, id_gets)) {
        p->lex.gets = ripper_lex_get_generic;
    }
    else {
        StringValue(src);
        p->lex.gets = lex_get_str;
    }
    p->lex.input = src;
    p->eofp = 0;
    if (NIL_P(fname)) {
        fname = STR_NEW2("(ripper)");
        OBJ_FREEZE(fname);
    }
    else {
        StringValueCStr(fname);
        fname = rb_str_new_frozen(fname);
    }
    parser_initialize(p);

    p->ruby_sourcefile_string = fname;
    p->ruby_sourcefile = RSTRING_PTR(fname);
    p->ruby_sourceline = NIL_P(lineno) ? 0 : NUM2INT(lineno) - 1;

    return Qnil;
}

static VALUE
ripper_parse0(VALUE parser_v)
{
    struct parser_params *p;

    TypedData_Get_Struct(parser_v, struct parser_params, &parser_data_type, p);
    parser_prepare(p);
    p->ast = rb_ast_new();
    ripper_yyparse((void*)p);
    rb_ast_dispose(p->ast);
    p->ast = 0;
    return p->result;
}

static VALUE
ripper_ensure(VALUE parser_v)
{
    struct parser_params *p;

    TypedData_Get_Struct(parser_v, struct parser_params, &parser_data_type, p);
    p->parsing_thread = Qnil;
    return Qnil;
}

/*
 *  call-seq:
 *    ripper.parse
 *
 *  Start parsing and returns the value of the root action.
 */
static VALUE
ripper_parse(VALUE self)
{
    struct parser_params *p;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, p);
    if (!ripper_initialized_p(p)) {
        rb_raise(rb_eArgError, "method called for uninitialized object");
    }
    if (!NIL_P(p->parsing_thread)) {
        if (p->parsing_thread == rb_thread_current())
            rb_raise(rb_eArgError, "Ripper#parse is not reentrant");
        else
            rb_raise(rb_eArgError, "Ripper#parse is not multithread-safe");
    }
    p->parsing_thread = rb_thread_current();
    rb_ensure(ripper_parse0, self, ripper_ensure, self);

    return p->result;
}

/*
 *  call-seq:
 *    ripper.column   -> Integer
 *
 *  Return column number of current parsing line.
 *  This number starts from 0.
 */
static VALUE
ripper_column(VALUE self)
{
    struct parser_params *p;
    long col;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, p);
    if (!ripper_initialized_p(p)) {
        rb_raise(rb_eArgError, "method called for uninitialized object");
    }
    if (NIL_P(p->parsing_thread)) return Qnil;
    col = p->lex.ptok - p->lex.pbeg;
    return LONG2NUM(col);
}

/*
 *  call-seq:
 *    ripper.filename   -> String
 *
 *  Return current parsing filename.
 */
static VALUE
ripper_filename(VALUE self)
{
    struct parser_params *p;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, p);
    if (!ripper_initialized_p(p)) {
        rb_raise(rb_eArgError, "method called for uninitialized object");
    }
    return p->ruby_sourcefile_string;
}

/*
 *  call-seq:
 *    ripper.lineno   -> Integer
 *
 *  Return line number of current parsing line.
 *  This number starts from 1.
 */
static VALUE
ripper_lineno(VALUE self)
{
    struct parser_params *p;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, p);
    if (!ripper_initialized_p(p)) {
        rb_raise(rb_eArgError, "method called for uninitialized object");
    }
    if (NIL_P(p->parsing_thread)) return Qnil;
    return INT2NUM(p->ruby_sourceline);
}

/*
 *  call-seq:
 *    ripper.state   -> Integer
 *
 *  Return scanner state of current token.
 */
static VALUE
ripper_state(VALUE self)
{
    struct parser_params *p;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, p);
    if (!ripper_initialized_p(p)) {
        rb_raise(rb_eArgError, "method called for uninitialized object");
    }
    if (NIL_P(p->parsing_thread)) return Qnil;
    return INT2NUM(p->lex.state);
}

/*
 *  call-seq:
 *    ripper.token   -> String
 *
 *  Return the current token string.
 */
static VALUE
ripper_token(VALUE self)
{
    struct parser_params *p;
    long pos, len;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, p);
    if (!ripper_initialized_p(p)) {
        rb_raise(rb_eArgError, "method called for uninitialized object");
    }
    if (NIL_P(p->parsing_thread)) return Qnil;
    pos = p->lex.ptok - p->lex.pbeg;
    len = p->lex.pcur - p->lex.ptok;
    return rb_str_subseq(p->lex.lastline, pos, len);
}

#ifdef RIPPER_DEBUG
/* :nodoc: */
static VALUE
ripper_assert_Qundef(VALUE self, VALUE obj, VALUE msg)
{
    StringValue(msg);
    if (UNDEF_P(obj)) {
        rb_raise(rb_eArgError, "%"PRIsVALUE, msg);
    }
    return Qnil;
}

/* :nodoc: */
static VALUE
ripper_value(VALUE self, VALUE obj)
{
    return ULONG2NUM(obj);
}
#endif

/*
 *  call-seq:
 *    Ripper.lex_state_name(integer)   -> string
 *
 *  Returns a string representation of lex_state.
 */
static VALUE
ripper_lex_state_name(VALUE self, VALUE state)
{
    return rb_parser_lex_state_name(NUM2INT(state));
}

void
Init_ripper(void)
{
    ripper_init_eventids1();
    ripper_init_eventids2();
    id_warn = rb_intern_const("warn");
    id_warning = rb_intern_const("warning");
    id_gets = rb_intern_const("gets");
    id_assoc = rb_intern_const("=>");

    (void)yystpcpy; /* may not used in newer bison */

    InitVM(ripper);
}

void
InitVM_ripper(void)
{
    VALUE Ripper;

    Ripper = rb_define_class("Ripper", rb_cObject);
    /* version of Ripper */
    rb_define_const(Ripper, "Version", rb_usascii_str_new2(RIPPER_VERSION));
    rb_define_alloc_func(Ripper, ripper_s_allocate);
    rb_define_method(Ripper, "initialize", ripper_initialize, -1);
    rb_define_method(Ripper, "parse", ripper_parse, 0);
    rb_define_method(Ripper, "column", ripper_column, 0);
    rb_define_method(Ripper, "filename", ripper_filename, 0);
    rb_define_method(Ripper, "lineno", ripper_lineno, 0);
    rb_define_method(Ripper, "state", ripper_state, 0);
    rb_define_method(Ripper, "token", ripper_token, 0);
    rb_define_method(Ripper, "end_seen?", rb_parser_end_seen_p, 0);
    rb_define_method(Ripper, "encoding", rb_parser_encoding, 0);
    rb_define_method(Ripper, "yydebug", rb_parser_get_yydebug, 0);
    rb_define_method(Ripper, "yydebug=", rb_parser_set_yydebug, 1);
    rb_define_method(Ripper, "debug_output", rb_parser_get_debug_output, 0);
    rb_define_method(Ripper, "debug_output=", rb_parser_set_debug_output, 1);
    rb_define_method(Ripper, "error?", ripper_error_p, 0);
#ifdef RIPPER_DEBUG
    rb_define_method(Ripper, "assert_Qundef", ripper_assert_Qundef, 2);
    rb_define_method(Ripper, "rawVALUE", ripper_value, 1);
    rb_define_method(Ripper, "validate_object", ripper_validate_object, 1);
#endif

    rb_define_singleton_method(Ripper, "dedent_string", parser_dedent_string, 2);
    rb_define_private_method(Ripper, "dedent_string", parser_dedent_string, 2);

    rb_define_singleton_method(Ripper, "lex_state_name", ripper_lex_state_name, 1);

    /* ignore newline, +/- is a sign. */
    rb_define_const(Ripper, "EXPR_BEG", INT2NUM(EXPR_BEG));
    /* newline significant, +/- is an operator. */
    rb_define_const(Ripper, "EXPR_END", INT2NUM(EXPR_END));
    /* ditto, and unbound braces. */
    rb_define_const(Ripper, "EXPR_ENDARG", INT2NUM(EXPR_ENDARG));
    /* ditto, and unbound braces. */
    rb_define_const(Ripper, "EXPR_ENDFN", INT2NUM(EXPR_ENDFN));
    /* newline significant, +/- is an operator. */
    rb_define_const(Ripper, "EXPR_ARG", INT2NUM(EXPR_ARG));
    /* newline significant, +/- is an operator. */
    rb_define_const(Ripper, "EXPR_CMDARG", INT2NUM(EXPR_CMDARG));
    /* newline significant, +/- is an operator. */
    rb_define_const(Ripper, "EXPR_MID", INT2NUM(EXPR_MID));
    /* ignore newline, no reserved words. */
    rb_define_const(Ripper, "EXPR_FNAME", INT2NUM(EXPR_FNAME));
    /* right after `.', `&.' or `::', no reserved words. */
    rb_define_const(Ripper, "EXPR_DOT", INT2NUM(EXPR_DOT));
    /* immediate after `class', no here document. */
    rb_define_const(Ripper, "EXPR_CLASS", INT2NUM(EXPR_CLASS));
    /* flag bit, label is allowed. */
    rb_define_const(Ripper, "EXPR_LABEL", INT2NUM(EXPR_LABEL));
    /* flag bit, just after a label. */
    rb_define_const(Ripper, "EXPR_LABELED", INT2NUM(EXPR_LABELED));
    /* symbol literal as FNAME. */
    rb_define_const(Ripper, "EXPR_FITEM", INT2NUM(EXPR_FITEM));
    /* equals to +EXPR_BEG+ */
    rb_define_const(Ripper, "EXPR_VALUE", INT2NUM(EXPR_VALUE));
    /* equals to <tt>(EXPR_BEG | EXPR_MID | EXPR_CLASS)</tt> */
    rb_define_const(Ripper, "EXPR_BEG_ANY", INT2NUM(EXPR_BEG_ANY));
    /* equals to <tt>(EXPR_ARG | EXPR_CMDARG)</tt> */
    rb_define_const(Ripper, "EXPR_ARG_ANY", INT2NUM(EXPR_ARG_ANY));
    /* equals to <tt>(EXPR_END | EXPR_ENDARG | EXPR_ENDFN)</tt> */
    rb_define_const(Ripper, "EXPR_END_ANY", INT2NUM(EXPR_END_ANY));
    /* equals to +0+ */
    rb_define_const(Ripper, "EXPR_NONE", INT2NUM(EXPR_NONE));

    ripper_init_eventids1_table(Ripper);
    ripper_init_eventids2_table(Ripper);

# if 0
    /* Hack to let RDoc document SCRIPT_LINES__ */

    /*
     * When a Hash is assigned to +SCRIPT_LINES__+ the contents of files loaded
     * after the assignment will be added as an Array of lines with the file
     * name as the key.
     */
    rb_define_global_const("SCRIPT_LINES__", Qnil);
#endif

}
#endif /* RIPPER */

/*
 * Local variables:
 * mode: c
 * c-file-style: "ruby"
 * End:
 */
