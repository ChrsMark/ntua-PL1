package myButs;


/** here we implement the simplePoint class which shows 
	if a point's  position(x,y).
**/
public class simplePoint {
	private double x;
	private double y;
	
public simplePoint() {
		x = 0;
		y = 0;
		
}

public simplePoint(double myX, double myY) {
	x = myX;
	y = myY;
	
}
void setX(double myX){
	x = myX;
}
void setY(double myY){
	y = myY;
}


double getX(){
	return x;
}
double  getY(){
	return y ;
}
	


}

