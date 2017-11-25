#include<stdio.h>
#include<stdlib.h>

//#include<string.h>

int countlines(char *filename);
void getlines(char **filename, int *result);
char* readlines(char *filename/*, int lines[]*/);


int main(int argc, char *argv[])
{
  //int res = countlines(argv[1]);
  //int *res = malloc(sizeof *res);
  //getlines(&argv[1], res);
  //printf("LINES: %d\n", *res);
  //return *res;
  readlines(argv[1]);
  
  
}

void getlines(char **filename, int *result){
  //*result = strlen(filename);
  *result = countlines(*filename);
  //return *result;
}

char* readlines(char *filename/*, int linesToRead[]*/){
  FILE *fp = fopen(filename,"r");
  int ch=0;
  int lines=0;
  char result[1024]; //= malloc(sizeof(char[1024]));
  
  if (fp == NULL)
    return "";
  
  lines++;
  while (fgets(result, 1024, fp)) {
    printf("%s", result); 
  }
  fclose(fp);
  return "";
}

int countlines(char *filename)
{
  // count the number of lines in the file called filename                                    
  FILE *fp = fopen(filename,"r");
  int ch=0;
  int lines=0;
  
  if (fp == NULL)
  return -1;
  
  lines++;
  while ((ch = fgetc(fp)) != EOF)
  {
    if (ch == '\n')
      lines++;
  }
  fclose(fp);
  return lines;
}