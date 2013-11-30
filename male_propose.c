#include <stdio.h>
#include <stdlib.h>



#define N      3                /* number of people on each side */
#define ERROR -1


/* linked list implementation */
struct node {
  int x;
  struct node *next;
};

typedef struct node nodelist;


/* Return the more preffered node */
int morePreferred(struct node *list, int x, int y){
  struct node *conductor = list;
  while(conductor){
    if(conductor->x == x) return x;
    if(conductor->x == y) return y;
    conductor = conductor -> next;
  }
  return ERROR;
}

nodelist* addToList(struct node* ll, int a) 
{
  nodelist * curr = (struct node *) malloc(sizeofdelist));
  



}



void go_through(nodelist *ll) 
{
     if(ll)
       {
         printf("%d\n", ll->x);
         go_through(ll->next);
       }   
}


int main(int argc, char* arg[]){

  /* This will be the male proposing version */
  struct node * curr, * head;

  int i; 

  for(i = 0; i< 10; i++){
    curr = (struct node *) malloc(sizeof(nodelist));
    curr-> x= i;
    curr-> next = head;
    head = curr;
  }

  curr = head;


  go_through(curr);





}
