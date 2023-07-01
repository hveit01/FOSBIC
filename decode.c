#include <stdio.h>
#include <stdlib.h>

char alph[] = {
	0,
	'A','B','C','D','E','F','G','H','I','J','K','L','M',
	'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
	'0','1','2','3','4','5','6','7','8','9',
	'*',' ',',','.','=',')','(','+','\'','$','-','/'
};

int main(int argc, char* argv[]) 
{
	int num, i;
	char str[10] = { 0, };

	if (argc != 2) {
		printf("Usage: %s number\n", argv[0]); 
		exit(1);
	}
	
	num = atol(argv[1]);
	for (i=0; num; i++) {
		str[i] = alph[(num % 48)+1];
		printf("num %% 48 = %d\n",(num%48)+1);
		num /= 48;
	}
	
	printf("%ld -> '%s'\n", atol(argv[1]), str);
	exit(0);
}