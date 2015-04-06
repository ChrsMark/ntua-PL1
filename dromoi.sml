(***************************************************************************
  Course    : Programming Languages 1 - Assignment 1 - Exercise 2
  Author(s) : Nikolaos Papadis (nikpapadis@gmail.com), Chris Mark (chrs.markx86@gmail.com)
  Date      : June 25, 2014
  -----------
  School of ECE, National Technical University of Athens.
****************************************************************************)

fun parse file =
    let
	(* a function to read an integer from an input stream *)
        fun next_int input =
	    Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
	(* a function to read a real that spans till the end of line *)
        fun next_real input =
	    Option.valOf (TextIO.inputLine input)
	(* open input file and read the two integers in the first line *)
        val stream = TextIO.openIn file
        val n = next_int stream
        val l = next_int stream
        val x = next_int stream
	val _ = TextIO.inputLine stream
	(* a function to read the pair of integer & real in subsequent lines *)
        fun scanner 0 acc = acc
          | scanner i acc =
            let
                val s = next_int stream
                val f = next_int stream
            in
                scanner (i - 1) ((s, f) :: acc)
            end
    in
        (n, l, x ,  rev(scanner n []))
    end


(*Sort from shmmy.ntua.gr *)
local
   fun mergeTR [] ys acc = List.revAppend (acc, ys)
   |   mergeTR xs [] acc = List.revAppend (acc, xs)
   |   mergeTR (lx as (sX:int,fX:string) :: xs ) (ly as (sY:int,fY:string) :: ys) acc =
         if sX < sY then mergeTR xs ly ((sX,fX) :: acc)
         else mergeTR lx ys ((sY,fY) :: acc)
         
   fun mergepairs [] [] = []
   |   mergepairs [s] [] = s
   |   mergepairs l [] = mergepairs [] l
   |   mergepairs l [s] = mergepairs [] (s :: l)
   |   mergepairs l (s1 :: s2 :: ss) = mergepairs ((mergeTR s1 s2 []) :: l) ss
in
   fun fsort l = mergepairs [] (map (fn x => [x]) l)
end


(*This function produces tuples of type (int,"c"), where "c" represents start or finish*)
fun mySplit [] = []
  |mySplit [(s:int,f:int)] = [(s,"s"),(f,"f")]    
  |mySplit ((s,f)::xs ) = (s,"s")::(f,"f"):: mySplit(xs)

    
(*This function finds the bigest gap in the road for a specific number of days*)          
fun bigestGap (gap,semaphore,[],l) = 42
  | bigestGap (gap,semaphore,(point,type1)::[],l) = l + 1
  | bigestGap (gap,semaphore,((point1,myType1)::(head,"f")::[]),l) = if  l - head > gap then l - head else gap    
  | bigestGap (gap,semaphore,((point1,myType1)::(point2,myType2)::xs),l) =
      let
        val mySem = if  myType1="s" then semaphore+1  else semaphore-1         
      in  
        let     
          val myGap =  if mySem = 0 then point2 - point1 else 0        
          val myGGap = if myGap > gap  then myGap else gap
        in
          bigestGap(myGGap, mySem,((point2,myType2)::xs),l )
        end    
      end
      

(*This function finds the bigest gap (by calling bigestGap) in the road for a specific number of days*)
fun doTheJob ([],r) = r
  | doTheJob (hd::[],r) = r
  | doTheJob (myList,road_length) = 
    let
      val (head,"s")::xs = fsort(myList)
      val inGap =   head 
    in 
      bigestGap (inGap,1,xs,road_length)
    end


(*This function returns a part of a list, up to point*)
fun take (myList:((int*string) list),point) = 
  let
    val n = point 
    fun walk i acc [] = ([])
      | walk i acc (y :: ys) =
          if i   < n then walk (i+1) (y :: acc) ys
                      else (rev acc)
  in
      walk 0 [] myList
  end;


(* This is the binary function that returns the best Day or -1 *)
fun mybinary (down,up,myBest,candidates,x,road_l) = 
    let
      val target_day = (up+down) div 2
      val helplist = take (candidates, target_day*2) 
      val myCurrBest = doTheJob (helplist,road_l) 
      val myD = if  myCurrBest>x then target_day else down
      val myT = if  myCurrBest>x then up else target_day
      val myB = if  myCurrBest>x then myBest else target_day 
      val myListttt = if  myCurrBest>x then candidates else helplist 
    in 
      if myT-myD = 1 then
        myB
      else  
        mybinary (myD,myT,myB,myListttt,x,road_l)
    end


(*This is the main function*)     
fun my_solution (0, l, x, []) = 0 
  | my_solution (n ,l , x , candidates) =  
    let
      val myDay = ~1
      val up = n
      val down = 0
      val myList = mySplit(candidates)
    in
      if x = l then 
        0
      else
        mybinary(down,up,myDay,myList,x,l)
    end


fun dromoi fileName = my_solution (parse fileName)