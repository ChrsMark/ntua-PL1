//package myButs;

/** here we implement the interestingPoint class which shows 
	if a point is Bat or Spider or Wall and what is its position(x,y).
**/

public class interestingPoint {
	private double x;
	private double y;
	private char kind;

	public interestingPoint(double myX, double myY, char myKind) {
		x = myX;
		y = myY;
		kind = myKind;
	}
	void setX(double myX){
		x = myX;
	}
	void setY(double myY){
		y = myY;
	}
	void setKind(char myKind){
		kind = myKind;
	}

	double getX(){
		return x;
	}
	double  getY(){
		return y ;
	}
	char getKind(){
		return kind ;
	}

}