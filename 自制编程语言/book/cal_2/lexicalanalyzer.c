#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include "token.h"

static char *st_line;
static int st_line_pos;

typedef enum
{
    INITIAL_STATUS,
    IN_INT_PART_STATUS,
    DOT_STATUS,
    IN_FRAC_PART_STATUS
} LexerStatus;

void get_token(Token *token)
{
    int out_pos = 0;
    LexerStatus status = INITIAL_STATUS;
    char current_char;

    token->kind = BAD_TOKEN;
    while (st_line[st_line_pos] != '\0') {
        current_char = st_line[st_line_pos];
        if ((status == IN_INT_PART_STATUS || status == IN_FRAC_PART_STATUS)
            && !isdigit(current_char) && current_char != '.') {
            token->kind = NUMBER_TOKEN;
            sscanf(token->str, "%lf", &token->value);
            return;
        }

        if (isspace(current_char)) {
            if (current_char == '\n') {
                token->kind = END_OF_LINE_TOKEN;
                return;
            }
            st_line_pos++;
            continue;
        }

        if (out_pos >= MAX_TOKEN_SIZE - 1) {
            fprintf(stderr, "token too long.\n");
            exit(-1);
        }

        token->str[out_pos] = st_line[st_line_pos];
        st_line_pos++;
        out_pos++;
        token->str[out_pos] = '\0';

        if (current_char == '+') {
            token->kind = ADD_OPERATOR_TOKEN;
            return;
        } else if (current_char == '-') {
            token->kind = SUB_OPERATOR_TOKEN;
            return;
        } else if (current_char == '*') {
            token->kind = MUL_OPERATOR_TOKEN;
            return;
        } else if (current_char == '/') {
            token->kind = DIV_OPERATOR_TOKEN;
            return;
        } else if (current_char == '(') {
            token->kind = LEFT_PAREN_TOKEN;
            return;
        } else if (current_char == ')') {
            token->kind = RIGHT_PAREN_TOKEN;
            return;
        } else if (isdigit(current_char)) {
            if (status == INITIAL_STATUS) {
                status = IN_INT_PART_STATUS;
            } else if (status == DOT_STATUS) {
                status = IN_FRAC_PART_STATUS;
            }
        } else if (current_char == '.') {
            if (status == IN_INT_PART_STATUS) {
                status = DOT_STATUS;
            } else {
                fprintf(stderr, "syntax error.\n");
                exit(1);
            }
        } else {
            fprintf(stderr, "bad character(%c)\n", current_char);
            exit(1);
        }
    }
}

void set_line(char *line)
{
    st_line = line;
    st_line_pos = 0;
}

#if 0
void parse_line(char *buf)
{
    Token token;
    set_line(buf);

    for(;;)
    {
        get_token(&token);
        if (token.kind == END_OF_LINE_TOKEN) {
            break;
        } else {
            printf("kind..%d, str..%s\n", token.kind, token.str);
        }
    }
}

int main(int argc, char const *argv[])
{
    char buf[1024];
    while (fgets(buf, 1024, stdin) != NULL) {
        parse_line(buf);
    }
    return 0;
}
#endif