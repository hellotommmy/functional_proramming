#include<assert.h>
#include<stdio.h>
#include<string.h>
#define EOINTSTR 10000000
struct stack{
	int top;
	int body[200];
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
void Action_plus(struct stack *S, int head);
void Action_mult(struct stack *S, int head);
void Action_remove_parens(struct stack *S, int head);
int count_red(struct stack *expr);
int main(){
	test_case();
	return 0;
}
int count_red(struct stack *expr){
	print_stack(expr);
	int i;
	int head = 0;
	int count = 0;
	struct stack expr1;
	if(expr->top == 0){//a single number
		return 1;
	}
	while(head <= expr->top){
		memcpy(&expr1, expr, sizeof(struct stack));
		switch(expr->body[head]){
			case 's':
				Action_square(&expr1, head);
				goto here;
			case '+':
				if((!special_char(expr->body[head + 1])) &&(!special_char(expr->body[head - 1]))){
					Action_plus(&expr1, head);
					goto here;
				}
				else
					break;
			case '*':
				if((!special_char(expr->body[head + 1])) &&(!special_char(expr->body[head - 1]))){
					Action_mult(&expr1, head);
					goto here;
				}
				else 
					break;
			default:
				break;
			here:
			count += count_red(&expr1);
		}	
		head++;	
		//eliminate redundant parenthesis
		i = 0;
		while(i <= expr->top){
			if(i>=1 && expr->body[i-1] == 's'){
				i++;
				continue;
			}
			if(expr->body[i] == l_paren && expr->body[i + 2] == r_paren )//e.g. (2)
			{
				Action_remove_parens(expr, i);
			}
			i++;
		}			
	}
	return count;
}
void Action_remove_parens(struct stack *S, int head){
	assert(S->body[head] == l_paren);
	assert(S->body[head + 2] == r_paren);
	int number[2] = { S->body[head + 1], EOINTSTR};
	replace(S, head, head + 2, number);
}
void Action_plus(struct stack *S, int head){
	assert(S->body[head] == '+');
	int number[2] = {S->body[head - 1] + S->body[head + 1], EOINTSTR};
	replace(S, head - 1, head + 1, number);
}
void Action_mult(struct stack *S, int head){
	assert(S->body[head] == '*');
	int number[2] = {S->body[head - 1] * S->body[head + 1], EOINTSTR};
	replace(S, head - 1, head + 1, number);
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
	return i;
}
void int_copy(int *src, int *dst, int length){
	assert(length > 0);
	int i = 0;
	while(i <= length - 1){
		dst[i] = src[i];
		i++;
	}
	dst[i] = EOINTSTR;
}
void Action_square(struct stack *S, int head){
	int i = head + 1;
	assert(S->body[i] == l_paren);
	int l_paren_count = 1;
	int temp[200];
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
	temp[content_len] = '*';
	int_copy(temp, temp + content_len + 1, content_len);
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
	printf("\n");
}
void test_case(){
	//int Str[100] = {'s','(','s','(','s','(',3,'+',7,')',')',')',EOINTSTR};
	int Str[100] = {'s','(',3,'+',7,')','*','s','(',3,'+',7,')',EOINTSTR};
	struct stack ST;
	stack_copy(&ST, Str);
	print_stack(&ST);
	printf("%d\n",count_red(&ST));
/*	Action_square(&ST, 0);
	print_stack(&ST);
	Action_plus(&ST, 4);
	print_stack(&ST);
	Action_square(&ST, 1);
	print_stack(&ST);	
	Action_remove_parens(&ST, 1);
	print_stack(&ST);*/
}
void replace(struct stack *S, int start, int end, int *new){//if the stack shrinks, end-start>len(new); otherwise end-start=len(new),
	int length = len(new);
	//printf("before replacement:%d,%d,%d\n",start,end,length);
	//print_stack(S);
	int i;
	int temp[200];
	if(length <=0)
		return;
	if(end - start + 1 == length ){
		for( i = start; i <= end; i++)
			S->body[i] = new[i - start];
		S->top = end;
	}
	else{//shrinks or expands
		if(S->top > end){//need to move elems
			for( i = end + 1; i <= S->top; i++){
				temp[i - end - 1] = S->body[i];
			}
			for( i = end + 1; i <= S->top; i++){
				S->body[i + length - (end - start + 1)] = temp[i - end - 1];
			}
		}
		S->top += length - (end - start + 1);	
		for(i = start; i <=start + length - 1; i++)
			S->body[i] = new[i - start];	
	}
	S->body[S->top + 1] = EOINTSTR;
	//printf("after replacement:\n");
	//print_stack(S);
}

