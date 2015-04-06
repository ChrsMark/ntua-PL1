/***************************************************************************
  Course    : Programming Languages 1 - Assignment 1 - Exercise 2
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


int myBlackBox (int k);

struct point
{
    int point_in_road;
    char type_of_point;   // it is starting or finishing point of a job 
 }; 
void quickSort( struct point [], int, int);
int partition( struct point [], int, int);

struct point *points;  // we have k starting points and k finishing points total is 2K+2 points

struct job
{
    int Start;
    int End;
};

//set these as GLOBAL to be accessible from all function with having to "pass" their pointers
struct job *days;

int N,L,X;

int main(int argc, char *argv[]) {
    
    N=L=X=0;
    int i;
    FILE* fp;
    int d,r,q;
    int c;
    int result;
       
    if( argc != 2 ){    
        return 1;
    }

    fp = fopen(argv[1],"rb");

    if( fp == NULL ){
        perror("fopen");
        return 1;
    }

    i = 0;
    c = fscanf(fp,"%d",&d);   //file descriptor -> in buffer d is stored the number of days (N)
    c = fscanf(fp,"%d",&r);   //file descriptor -> in buffer r is stored the perimeter length (L)
    c = fscanf(fp,"%d",&q);   //file descriptor -> in buffer r is stored the perimeter length (X)
      
    while ( c != EOF ) {

        if ( i == 0 ) {
          			
            N = d;
	        L = r; 
            X = q;                                         // here is stored N through the buffer of file descriptor
            days = malloc(d * sizeof(struct job));
                       
        }
       	else {
          		
            days[i-1].Start = d;
      	    days[i-1].End = r;
  
        }

    i++;
    c = fscanf(fp,"%d",&d);
    c = fscanf(fp,"%d",&r);
    }


    int Stop = 0;
    if(X==L){

      return 0;  
    }
    
    result = L; 
    
    
    int up=N;
    int down=0;
    int k = (up+down)/2;

    int myBest = -1; 

    while(!Stop){
        result = myBlackBox(k);
        if (result>X){
            down=k;
            k = (up+down)/2;
        }
        else {

            myBest=k;
            up=k;
            k = (up+down)/2;
        }
        
        

        if(abs(up-down)==1){
            break;
        }
        
        
    }
    printf("%d\n", myBest);
    
    free(days);          //!!!!!remember to release the memory when you have finished!!!!!
    
    return 0;

}   

/*  This function takes an integer which is a pointer for our 
    initial array of days-jobs. It returns the bigest uncompleted 
    road part that has been left from these days-jobs.
*/
int myBlackBox (int k){

    int Bound = 2*k;
    
    points = malloc(Bound * sizeof(struct point));
    int myMax,i;
    
    myMax = 0;
    
    int j=0;
    for (i=0; i<Bound; i+=2){
        points[i].point_in_road = days[j].Start;
        points[i].type_of_point = 'S';
        points[i+1].point_in_road = days[j].End;
        points[i+1].type_of_point = 'F';
        j++;

    }

    quickSort( points, 0, Bound-1);

    
    myMax = points[0].point_in_road;
    int temp =0 ;
    
    int Starts =1;

    
    for( i=1; i<Bound-1; i++){
        if (myMax>X){
                    	free (points);
                    	return myMax;
                    }

        if (points[i].type_of_point== 'F'){
            Starts--;
         } 
        else {
            Starts++;
        } 
        if (Starts==0){
            temp=points[i+1].point_in_road-points[i].point_in_road;
                if (temp>myMax){
                    myMax=temp;

                }
        }



    }
    if((L-points[Bound-1].point_in_road)>myMax){
        myMax = L-points[Bound-1].point_in_road ;
    }
    ;
    free(points);
return myMax;


}


void quickSort( struct point a[], int l, int r)
{
   int j;

   if( l < r ) 
   {
    // divide and conquer
        j = partition( a, l, r);
       quickSort( a, l, j-1);
       quickSort( a, j+1, r);
   }
    
}



int partition( struct point a[], int l, int r) {
   int pivot, i, j;
   struct point t;
   pivot = a[l].point_in_road;
   i = l; j = r+1;
        
   while( 1)
   {
    do ++i; while( a[i].point_in_road <= pivot && i <= r );
    do --j; while( a[j].point_in_road > pivot );
    if( i >= j ) break;
    t = a[i]; a[i] = a[j]; a[j] = t;
   }
   t = a[l]; a[l] = a[j]; a[j] = t;
   return j;
}