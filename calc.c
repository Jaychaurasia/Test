/*
 * ============================================================
 *   INTERMEDIATE C CALCULATOR
 * ============================================================
 *  Features:
 *   - Basic arithmetic: +  -  *  /  %
 *   - Power (^) and square root
 *   - Trigonometric functions: sin, cos, tan (degrees or radians)
 *   - Logarithms: log (base-10), ln (natural)
 *   - Memory: store (MS), recall (MR), clear (MC), add (M+)
 *   - Calculation history (last 10 results)
 *   - Expression evaluator with operator precedence
 *   - Error handling (div-by-zero, domain errors, etc.)
 * ============================================================
 */
 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <errno.h>
 
/* ── Constants ─────────────────────────────────────────────── */
#define HISTORY_SIZE   10
#define INPUT_MAX      256
#define PI             3.14159265358979323846
 
/* ── Global state ───────────────────────────────────────────── */
double  memory       = 0.0;
double  history[HISTORY_SIZE];
int     history_count = 0;
int     use_degrees   = 1;   /* 1 = degrees, 0 = radians */
 
/* ── Colour helpers (ANSI) ──────────────────────────────────── */
#define CLR_RESET  "\033[0m"
#define CLR_BOLD   "\033[1m"
#define CLR_CYAN   "\033[36m"
#define CLR_GREEN  "\033[32m"
#define CLR_YELLOW "\033[33m"
#define CLR_RED    "\033[31m"
#define CLR_BLUE   "\033[34m"
 
/* ── Utility ────────────────────────────────────────────────── */
static double to_rad(double deg) { return deg * (PI / 180.0); }
 
static void push_history(double val) {
    if (history_count < HISTORY_SIZE)
        history[history_count++] = val;
    else {
        memmove(history, history + 1, (HISTORY_SIZE - 1) * sizeof(double));
        history[HISTORY_SIZE - 1] = val;
    }
}
 
static void print_history(void) {
    if (history_count == 0) {
        printf(CLR_YELLOW "  No history yet.\n" CLR_RESET);
        return;
    }
    for (int i = 0; i < history_count && i < HISTORY_SIZE; i++)
        printf(CLR_CYAN "  [%2d]  %g\n" CLR_RESET, i + 1, history[i]);
}
 
/* ── Expression Evaluator (recursive-descent) ─────────────── */
/*
 * Grammar:
 *   expr   → term   { ('+' | '-') term }
 *   term   → factor { ('*' | '/' | '%') factor }
 *   factor → base   { '^' factor }       (right-associative)
 *   base   → '-' base | '(' expr ')' | func '(' expr ')' | number
 */
 
typedef struct { const char *p; int err; } Parser;
 
static double parse_expr(Parser *ps);
 
static void skip_ws(Parser *ps) {
    while (isspace((unsigned char)*ps->p)) ps->p++;
}
 
/* Try to match a function name; return 1 on success */
static int match_func(Parser *ps, const char *name) {
    size_t n = strlen(name);
    if (strncmp(ps->p, name, n) == 0 && !isalpha((unsigned char)ps->p[n])) {
        ps->p += n;
        return 1;
    }
    return 0;
}
 
static double parse_base(Parser *ps) {
    skip_ws(ps);
    if (ps->err) return 0;
 
    /* Unary minus */
    if (*ps->p == '-') { ps->p++; return -parse_base(ps); }
 
    /* Parenthesised sub-expression */
    if (*ps->p == '(') {
        ps->p++;
        double v = parse_expr(ps);
        skip_ws(ps);
        if (*ps->p != ')') { fprintf(stderr, CLR_RED "  Error: missing ')'\n" CLR_RESET); ps->err = 1; return 0; }
        ps->p++;
        return v;
    }
 
    /* Named functions */
    double arg;
 
#define FUNC1(name, expr) \
    if (match_func(ps, name)) { \
        skip_ws(ps); \
        if (*ps->p != '(') { fprintf(stderr, CLR_RED "  Error: expected '(' after " name "\n" CLR_RESET); ps->err=1; return 0; } \
        ps->p++; arg = parse_expr(ps); skip_ws(ps); \
        if (*ps->p != ')') { fprintf(stderr, CLR_RED "  Error: missing ')'\n" CLR_RESET); ps->err=1; return 0; } \
        ps->p++; return (expr); \
    }
 
    FUNC1("sqrt", (arg < 0 ? (ps->err=1, fprintf(stderr, CLR_RED "  Error: sqrt of negative number\n" CLR_RESET), 0.0) : sqrt(arg)))
    FUNC1("sin",  sin(use_degrees ? to_rad(arg) : arg))
    FUNC1("cos",  cos(use_degrees ? to_rad(arg) : arg))
    FUNC1("tan",  tan(use_degrees ? to_rad(arg) : arg))
    FUNC1("asin", (fabs(arg)>1 ? (ps->err=1, fprintf(stderr, CLR_RED "  Error: asin domain\n" CLR_RESET), 0.0) : (use_degrees ? (asin(arg)*180.0/PI) : asin(arg))))
    FUNC1("acos", (fabs(arg)>1 ? (ps->err=1, fprintf(stderr, CLR_RED "  Error: acos domain\n" CLR_RESET), 0.0) : (use_degrees ? (acos(arg)*180.0/PI) : acos(arg))))
    FUNC1("atan", (use_degrees ? (atan(arg)*180.0/PI) : atan(arg)))
    FUNC1("log",  (arg <= 0 ? (ps->err=1, fprintf(stderr, CLR_RED "  Error: log of non-positive\n" CLR_RESET), 0.0) : log10(arg)))
    FUNC1("ln",   (arg <= 0 ? (ps->err=1, fprintf(stderr, CLR_RED "  Error: ln of non-positive\n" CLR_RESET), 0.0) : log(arg)))
    FUNC1("abs",  fabs(arg))
    FUNC1("ceil", ceil(arg))
    FUNC1("floor",floor(arg))
 
    /* Named constants */
    if (match_func(ps, "pi") || match_func(ps, "PI")) return PI;
    if (match_func(ps, "e")  || match_func(ps, "E"))  return M_E;
    if (match_func(ps, "ans")) return (history_count ? history[history_count-1] : 0.0);
    if (match_func(ps, "mem") || match_func(ps, "MR")) return memory;
 
    /* Numeric literal */
    char *end;
    errno = 0;
    double val = strtod(ps->p, &end);
    if (end == ps->p) {
        fprintf(stderr, CLR_RED "  Error: unexpected token '%c'\n" CLR_RESET, *ps->p ? *ps->p : '?');
        ps->err = 1;
        return 0;
    }
    ps->p = end;
    return val;
}
 
static double parse_factor(Parser *ps) {
    double base = parse_base(ps);
    skip_ws(ps);
    if (!ps->err && *ps->p == '^') {
        ps->p++;
        double exp = parse_factor(ps);   /* right-assoc */
        return pow(base, exp);
    }
    return base;
}
 
static double parse_term(Parser *ps) {
    double left = parse_factor(ps);
    while (!ps->err) {
        skip_ws(ps);
        char op = *ps->p;
        if (op != '*' && op != '/' && op != '%') break;
        ps->p++;
        double right = parse_factor(ps);
        if (ps->err) break;
        if (op == '*') left *= right;
        else if (op == '/') {
            if (right == 0) { fprintf(stderr, CLR_RED "  Error: division by zero\n" CLR_RESET); ps->err=1; break; }
            left /= right;
        } else {
            if (right == 0) { fprintf(stderr, CLR_RED "  Error: modulo by zero\n" CLR_RESET); ps->err=1; break; }
            left = fmod(left, right);
        }
    }
    return left;
}
 
static double parse_expr(Parser *ps) {
    double left = parse_term(ps);
    while (!ps->err) {
        skip_ws(ps);
        char op = *ps->p;
        if (op != '+' && op != '-') break;
        ps->p++;
        double right = parse_term(ps);
        if (ps->err) break;
        left = (op == '+') ? left + right : left - right;
    }
    return left;
}
 
static double evaluate(const char *expr, int *ok) {
    Parser ps = { expr, 0 };
    double result = parse_expr(&ps);
    skip_ws(&ps);
    if (!ps.err && *ps.p != '\0') {
        fprintf(stderr, CLR_RED "  Error: unexpected '%s'\n" CLR_RESET, ps.p);
        ps.err = 1;
    }
    *ok = !ps.err;
    return result;
}
 
/* ── Banner & help ──────────────────────────────────────────── */
static void print_banner(void) {
    printf(CLR_BOLD CLR_BLUE
        "\n"
        "  ╔══════════════════════════════════════╗\n"
        "  ║    INTERMEDIATE C CALCULATOR  v1.0  ║\n"
        "  ╚══════════════════════════════════════╝\n"
        CLR_RESET);
    printf(CLR_CYAN "  Type 'help' for usage, 'quit' to exit.\n\n" CLR_RESET);
}
 
static void print_help(void) {
    printf(CLR_BOLD "\n  ── Operators ──────────────────────────────────\n" CLR_RESET);
    printf("   +  -  *  /  %%  ^   (standard arithmetic)\n");
    printf("   Grouping with parentheses, e.g. (3+4)*2\n");
 
    printf(CLR_BOLD "\n  ── Functions ──────────────────────────────────\n" CLR_RESET);
    printf("   sqrt(x)  abs(x)  ceil(x)  floor(x)\n");
    printf("   sin(x)   cos(x)  tan(x)\n");
    printf("   asin(x)  acos(x) atan(x)\n");
    printf("   log(x)   ln(x)              log=base-10, ln=natural\n");
 
    printf(CLR_BOLD "\n  ── Constants ──────────────────────────────────\n" CLR_RESET);
    printf("   pi  e   ans (last result)   mem / MR (memory)\n");
 
    printf(CLR_BOLD "\n  ── Commands ───────────────────────────────────\n" CLR_RESET);
    printf("   history    show last %d results\n", HISTORY_SIZE);
    printf("   MS  <val>  store value in memory (e.g. MS 42)\n");
    printf("   MR         recall memory\n");
    printf("   MC         clear memory\n");
    printf("   M+ <val>   add value to memory\n");
    printf("   deg        switch trig to DEGREES (current)\n");
    printf("   rad        switch trig to RADIANS\n");
    printf("   help       show this help\n");
    printf("   quit / exit\n\n");
}
 
/* ── Main ───────────────────────────────────────────────────── */
int main(void) {
    char input[INPUT_MAX];
 
    print_banner();
 
    while (1) {
        /* Prompt */
        printf(CLR_GREEN CLR_BOLD "  calc [%s] > " CLR_RESET,
               use_degrees ? "DEG" : "RAD");
        fflush(stdout);
 
        if (!fgets(input, sizeof(input), stdin)) {
            printf("\n");
            break;
        }
 
        /* Strip trailing newline/spaces */
        int len = (int)strlen(input);
        while (len > 0 && (input[len-1] == '\n' || input[len-1] == '\r' || input[len-1] == ' '))
            input[--len] = '\0';
 
        if (len == 0) continue;
 
        /* ── Built-in commands ── */
        if (strcmp(input, "quit") == 0 || strcmp(input, "exit") == 0) {
            printf(CLR_CYAN "  Goodbye!\n\n" CLR_RESET);
            break;
        }
        if (strcmp(input, "help") == 0) { print_help(); continue; }
        if (strcmp(input, "history") == 0) { print_history(); continue; }
        if (strcmp(input, "deg") == 0)  { use_degrees = 1; printf(CLR_YELLOW "  Mode: DEGREES\n" CLR_RESET); continue; }
        if (strcmp(input, "rad") == 0)  { use_degrees = 0; printf(CLR_YELLOW "  Mode: RADIANS\n" CLR_RESET); continue; }
        if (strcmp(input, "MR")  == 0)  { printf(CLR_CYAN "  Memory = %g\n" CLR_RESET, memory); continue; }
        if (strcmp(input, "MC")  == 0)  { memory = 0; printf(CLR_CYAN "  Memory cleared.\n" CLR_RESET); continue; }
        if (strncmp(input, "MS ", 3) == 0) {
            int ok; double v = evaluate(input + 3, &ok);
            if (ok) { memory = v; printf(CLR_CYAN "  Memory = %g\n" CLR_RESET, memory); }
            continue;
        }
        if (strncmp(input, "M+ ", 3) == 0) {
            int ok; double v = evaluate(input + 3, &ok);
            if (ok) { memory += v; printf(CLR_CYAN "  Memory = %g\n" CLR_RESET, memory); }
            continue;
        }
 
        /* ── Evaluate expression ── */
        int ok = 0;
        double result = evaluate(input, &ok);
        if (ok) {
            push_history(result);
            printf(CLR_BOLD CLR_GREEN "  = %g\n" CLR_RESET, result);
        }
        printf("\n");
    }
 
    return 0;
}
 
