/***************************************************************************
  Course    : Programming Languages 1 - Assignment 1 - Exercise 1
  Author(s) : Nikolaos Papadis (nikpapadis@gmail.com), Chris Mark (chrs.markx86@gmail.com)
  Date      : June 25, 2014
  -----------
  School of ECE, National Technical University of Athens.
****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <limits.h>
#include <string.h>

void Init();
void Insert(double element,int thesi_ston_pinaka);
int DeleteMin();

struct body
{
    double diff;
    int position_in_array;
};

struct point
{
    double speed;
    double position;
    int next,previous;
};

//set these as GLOBAL to be accessible from all functions which have to "pass" their pointers
struct point *pointInfo;
struct body *heap;
int *results;

int heapSize=0;
  
int main(int argc, char *argv[]) {
    
   
    int N=0,cycle_perimeter=0; 

    int i;
    FILE* fp;
    double d,r;
    int c;
       
    if( argc != 2 ){    
        return 1;
    }

    fp = fopen(argv[1],"rb");

    if( fp == NULL ){
        perror("fopen");
        return 1;
    }

    i = 0;
    c = fscanf(fp,"%lf",&d);   //file descriptor -> in buffer d is stored the number of candidates (N)
    c = fscanf(fp,"%lf",&r);   //file descriptor -> in buffer r is stored the perimeter length (L)
      
    while ( c != EOF ) {

        if ( i == 0 ) {
          			
            N = d;                                          // here is stored N through the buffer of file descriptor
            pointInfo = malloc(d * sizeof(struct point));
          	cycle_perimeter = r;                            // here is stored the perimeter length through the buffer of file descriptor
          			
            results =  malloc((d-1) * sizeof(int));
            heap =  malloc(d * sizeof(struct body));
                                
        }
       	else {
          		
            pointInfo[i-1].position = d;
      		pointInfo[i-1].speed = r;
                
            if (i==N) {
                pointInfo[i-1].next = 0;
            }
            else {
                pointInfo[i-1].next = i; 
            }

            if (i==1) {
                pointInfo[i-1].previous = N-1;
            }  
            else {
                pointInfo[i-1].previous = i-2; 
            }		
        }

    i++;
    c = fscanf(fp,"%lf",&d);
    c = fscanf(fp,"%lf",&r);
    }


    int iter;
    double element;

    Init();

    double diff= INT_MAX;
    int myNext=0;
    int Candidates=N;
    int leavers = 0;
    int toLeave;
    int current;

    //  mia proti prosomoiosi
    for (iter = 0; iter < N; iter++)
            { 
                myNext=pointInfo[iter].next;
                if (pointInfo[iter].speed!=pointInfo[myNext].speed) {
                           
                    if (iter==N-1) {
                                  
                        diff = (cycle_perimeter+pointInfo[myNext].position- pointInfo[iter].position)/(pointInfo[iter].speed-pointInfo[myNext].speed);
                        
                    }
                    
                    else {

                        diff = (pointInfo[myNext].position- pointInfo[iter].position)/(pointInfo[iter].speed-pointInfo[myNext].speed);
                    
                    }  
                          
                    if (diff>0) {
                        element = diff;
                        Insert(element,myNext);
                    }
                }
             
            }

    Candidates--;

    toLeave = DeleteMin();
    pointInfo[toLeave].speed=-1;

    results[leavers]=toLeave+1;

    leavers++;

    current = pointInfo[toLeave].previous;
    int tempNext = pointInfo[toLeave].next;

    pointInfo[current].next=pointInfo[toLeave].next;
    pointInfo[tempNext].previous=current;

    myNext=pointInfo[current].next;


    while (Candidates!=1) {
          
        if (pointInfo[current].speed!=pointInfo[myNext].speed) {
                    if (current==N-1) {
                      
                        diff = (cycle_perimeter+pointInfo[myNext].position- pointInfo[current].position)/(pointInfo[current].speed-pointInfo[myNext].speed);
           
                    }
                      
                    else {
            
                        diff = (pointInfo[myNext].position- pointInfo[current].position)/(pointInfo[current].speed-pointInfo[myNext].speed);
                    }  


                   if (diff>0) {
                        element = diff;
                        Insert(element,myNext);
                    }
            }
        
            toLeave = DeleteMin();

            if (pointInfo[toLeave].speed!=-1) {
                results[leavers]=toLeave+1;
                leavers++;
                pointInfo[toLeave].speed=-1;
                Candidates--;
            }
         

            if (heapSize==0){break;}
         
            current=pointInfo[toLeave].previous;
            tempNext = pointInfo[toLeave].next;
            pointInfo[current].next=tempNext;
            pointInfo[tempNext].previous=current;
            myNext=pointInfo[current].next;
        }
              
     
        for (i =0; i<leavers; i++) {

            printf("%d ",results[i]);

        }

     
    free(heap);          //!!!!!remember to release the memory when you have finished!!!!!
    free(pointInfo);
    free(results);
    return 0;

}   

/*Initialize Heap*/
void Init()
{
        heapSize = 0;
        heap[0].diff = -INT_MAX;
}

/*Insert an element into the heap */
void Insert(double element,int thesi_ston_pinaka)
{
        heapSize++;
        heap[heapSize].diff = element; /*Insert in the last place*/
      
        /*Adjust its position*/

        heap[heapSize].position_in_array = thesi_ston_pinaka; 
        
        int now = heapSize;
        
        while (heap[now/2].diff > element) 
        {
                heap[now] = heap[now/2];
                
                now /= 2;
        }
        
        heap[now].diff = element;
        heap[now].position_in_array = thesi_ston_pinaka;    
}

int DeleteMin()         //delete the minimum item form the heap
{
        /* heap[1] is the minimum element. So we remove heap[1]. Size of the heap is decreased. 
           Now heap[1] has to be filled. We put the last element in its place and see if it fits.
           If it does not fit, take minimum element among both its children and replaces parent with it.
           Again See if the last element fits in that place.*/

        double lastElement;
        int child,now,myThesis;
        
        myThesis = heap[1].position_in_array; 
        lastElement = heap[heapSize].diff;
        int last =  heap[heapSize].position_in_array;
         
        /* now refers to the index at which we are now */

        heapSize--;

        for (now = 1; now*2 <= heapSize ; now = child)
        {
                /* child is the index of the element which is minimum among both the children */ 
                /* Indexes of children are i*2 and i*2 + 1*/
                child = now*2;
                /*child!=heapSize beacuse heap[heapSize+1] does not exist, which means it has only one child */
                if(child != heapSize && heap[child+1].diff < heap[child].diff ) 
                {
                        child++;
                }
                /* To check if the last element fits ot not it suffices to check if the last element
                   is less than the minimum element among both the children*/
                if (lastElement > heap[child].diff)
                {
                        heap[now] = heap[child];
                        heap[now].position_in_array = heap[child].position_in_array;
                }
                else /* It fits there */
                {
                        break;
                }
        }

        heap[now].diff = lastElement;
        heap[now].position_in_array = last;

        return myThesis;
}