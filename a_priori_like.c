char a = 's';
char b = '(';
char c = ')';
char d = '+';
char e = '*';
int count_reductions(char *s);
int transible(char *s, int head, char * temp);
int main(){
	char expr[50];
	const_cpy(expr,"s(s(3+7))");
	printf("%d\n",count_reductions(expr));
}
int count_reductions(char *s){
	int head = 0;
	int count = 0;
	char temp[50];
	while(head < strlen(s)){
		//transform a step further
		if(transible(s, head, temp))
			count += count_reductions(temp);
		head++;
	}
	return count;
}
int transible(char *s, int head, char * temp){
	if(s[head] == '(' || s[head] == ')')
		return  0;
	if(s[head] >= '0' && s[head] <= '9')
		return 0;
	if(s[head] == 's'){
		
	}
	else{
		if(s[head] == '*')
			
		else
	}
}