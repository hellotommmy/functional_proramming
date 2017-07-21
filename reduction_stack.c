#include<assert.h>
#include<stdio.h>
#define EOINTSTR 10000000
struct stack{
	int top;
	int body[100];
};
int l_paren = '(';
int r_paren = ')';
int special_char(int c);
void stack_copy(struct stack *S, int *Str);
int len(int *Str);
void int_copy(int *src, int *dst, int len);
void Action_square(struct stack *S, int head);
void print_stack(struct stack *S);
void replace(struct stack *S, int start, int end, int *new);
void test_case();

int main(){
	test_case();
	return 0;
}
int special_char(int c){
	if(c == l_paren || c == r_paren || c == 's' || c == '+' || c == '*')
		return 1;
	else
		return 0;
}
void stack_copy(struct stack *S, int *Str){
	int length = len(Str);
	if(length <= 0)
		return;
	int i;
	for(i = 0; i < length; i++){
		//copy to stack
		S->body[i] = Str[i];
	}
	S->body[i] = EOINTSTR;
	S->top = length - 1;
}
int len(int *Str){
	int i = 0;
	while(Str[i]!=EOINTSTR)
		i++;
}
void int_copy(int *src, int *dst, int length){
	assert(length > 0);
	int i = 0;
	while(i <= length - 1){
		dst[i] = src[i];
		i++;
	}
}
void Action_square(struct stack *S, int head){
	assert(S->body[head+1] == l_paren);
	int i = head + 1;
	int l_paren_count = 1;
	int temp[100];
	temp[0] = l_paren;
	while(l_paren_count != 0){
		i++;
		if(S->body[i] == l_paren)
			l_paren_count++;
		else if(S->body[i] == r_paren)
			l_paren_count--;
		else
			;
		temp[i - head - 1] = S->body[i];
	}
	int content_len = i - head;
	int_copy(temp, temp + content_len, content_len);
	replace(S, head, i, temp);
	
}
void print_stack(struct stack *S){
	int i;
	for(i = 0; i <= S->top; i++){
		if(special_char(S->body[i]))
			printf("%c",(char)S->body[i]);
		else
			printf("%d",S->body[i]);
	}
}
void test_case(){
	int Str[100] = {'s','(','s','(','3','+','7',')',')',EOINTSTR};
	struct stack ST;
	stack_copy(&ST, Str);
	print_stack(&ST);
	Action_square(&ST, 1);
}
void replace(struct stack *S, int start, int end, int *new){//if the stack shrinks, end-start>len(new); otherwise end-start=len(new),
	int length = len(new);
	int i;
	if(length <=0)
		return;
	if(end - start + 1 == length){
		for( i = start; i <= end; i++)
			S->body[i] = new[i - start];
		S->top = end;
	}
	else if(end - start - length + 1 > 0){//shrinks
		for( i = start; i <= start + length - 1; i++)
			S->body[i] = new[i - start];
		for( ; i <= end; i++)
			S->body[i] = S->body[i + end - start - length + 1];
		S->top = i - 1;
	}
	else{//expands
		for( i = end + 1; i <=S->top; i++)
			S->body[i + length - (end - start + 1)] = S->body[i];
		S->top = i + length - (end - start + 1);
		for(i = start; i <=start + length - 1; i++)
			S->body[i] = new[i - start];	
	}
	S->body[S->top + 1] = EOINTSTR;
}

