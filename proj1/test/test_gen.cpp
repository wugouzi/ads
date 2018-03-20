#include <stdio.h>

int main()
{
    FILE* fp;
    int n;
    scanf("%d", &n);
    fp = fopen("testcase.txt", "w+");
    for (int i = 1; i <= n; i++)
    {
        fprintf(fp, "0 %d ", i);
    }
    for (int i = n; i >= 1; i--)
    {
        fprintf(fp, "1 %d ", i);
    }
    
    //fprintf(fp, "Only a %dtest\n", 100);
    //fputs("1 2 3 4 5 6 7 8 9\n", fp);
    fclose(fp);
    return 0;
}