(***************************************************************************
  Course    : Programming Languages 1 - Assignment 1 - Exercise 1
  Author(s) : Nikolaos Papadis (nikpapadis@gmail.com), Chris Mark (chrs.markx86@gmail.com)
  Date      : June 25, 2014
  Note      : We implemented an O(n^2) algorithm.
              We modified a little the parse function, so as to return (n,l,candidates),
              where candidates is a list of tuples (d, v, index), where index is the index
              the candidate (the line in which he is represented in the file).
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
  val _ = TextIO.inputLine stream
  (* a function to read the pair of integer & real in subsequent lines *)
        fun scanner 0 acc = acc
          | scanner i acc =
            let
                val d = next_int stream
                val (SOME v) = Real.fromString (next_real stream)
            in
                scanner (i - 1) ((d, v, n-i+1) :: acc)
            end
    in
        (n, l,  rev(scanner n []))
    end


(* This function sorts a list of tuples (_,real) *)
local
   fun mergeTR [] ys acc = List.revAppend (acc, ys)
   |   mergeTR xs [] acc = List.revAppend (acc, xs)
   |   mergeTR (lx as (sX:int,fX:real) :: xs ) (ly as (sY:int,fY:real) :: ys) acc =
         if fX < fY then mergeTR xs ly ((sX,fX) :: acc)
         else mergeTR lx ys ((sY,fY) :: acc)
         
   fun mergepairs [] [] = []
   |   mergepairs [s] [] = s
   |   mergepairs l [] = mergepairs [] l
   |   mergepairs l [s] = mergepairs [] (s :: l)
   |   mergepairs l (s1 :: s2 :: ss) = mergepairs ((mergeTR s1 s2 []) :: l) ss
in
   fun fsort l = mergepairs [] (map (fn x => [x]) l)
end


(* This function sorts a lists of tuples (_,real,_) *)
local
   fun mergeTR [] ys acc = List.revAppend (acc, ys)
   |   mergeTR xs [] acc = List.revAppend (acc, xs)
   |   mergeTR (lx as (sX:int,fX:real,index1:int) :: xs ) (ly as (sY:int,fY:real,index2:int) :: ys) acc =
         if fX > fY then mergeTR xs ly ((sX,fX,index1) :: acc)
         else mergeTR lx ys ((sY,fY,index2) :: acc)
         
   fun mergepairs [] [] = []
   |   mergepairs [s] [] = s
   |   mergepairs l [] = mergepairs [] l
   |   mergepairs l [s] = mergepairs [] (s :: l)
   |   mergepairs l (s1 :: s2 :: ss) = mergepairs ((mergeTR s1 s2 []) :: l) ss
in
   fun fsort2 l = mergepairs [] (map (fn x => [x]) l)
end


(* This function calculates the time needed before a candidate reaches another *)
fun timeCalc (p1:int, s1:real, i1:int) (p2:int, s2:real, i2:int) = ((Real.fromInt(p2-p1))/(s1-s2))


(* This function compares the second elements of two tuples *)
fun compTuples (x1:int,y1:real) (x2:int,y2:real) = if (y1 < y2) then true else false


(* This function updates the times at which each candidate reaches his next *)
fun loopInt2 j [] myArray n = myArray
    | loopInt2 j (_::[]) myArray n = myArray
    | loopInt2 j (hd1::hd2::xs) myArray n = 
        if (j <> n) then
            let
                val time = (#3(hd2):int, (timeCalc hd1 hd2))
                val k = (#2 time > 0.0)
                val m = compTuples time (Array.sub((!(myArray)),j))
                val r = if (k andalso m) then Array.update((!(myArray)), j, time) else ()
            in
                loopInt2 (j+1) (hd1::xs) myArray n
            end
        else
            myArray


(* This function calls loopInt2 with the next candidate as head of the list at each call *)
fun loopExt i [] myArray0 n = myArray0
    | loopExt i (l as hd::myList) myArray0 n =
        let
            val myArray = loopInt2 (i+1) l myArray0 n
        in
            loopExt (i+1) myList myArray n
        end


(* This function converts an array to a list *)
fun array_to_list (myArray:(int*real) array) n = 
    let
        fun walk i arr l = if i < n then walk (i+1) arr ((Array.sub(arr,i))::l)
                                    else (rev l)
    in
        walk 0 myArray []
    end


(* This function orders the candidates according to the time they are eliminated *)
fun order [] n = []
    | order myList n =
        let
            val myL as h1::tl1 = fsort2 myList
            val myArray0 = Array.array (n, (#3(h1), Real.posInf))
            val finalArray = loopExt 0 myL (ref myArray0) n
            val retList = array_to_list (!finalArray) n
            val almost = fsort retList
            fun walk i acc [] = (rev acc)
              | walk i acc ((y,z) :: ys) = if (i < n) then 
                                                            if (z < Real.posInf) then walk (i+1) (y::acc) ys
                                                                                 else walk (i+1) (acc) ys
                                                      else (rev acc)
        in
            walk 0 [] almost
        end


fun my_solution (0, l, candidates) = []
  | my_solution (n, 0, candidates) = []
  | my_solution (n, l, candidates) = order candidates n


fun agonas fileName = my_solution (parse fileName)