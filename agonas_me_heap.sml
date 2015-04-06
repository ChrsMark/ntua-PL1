(***************************************************************************
  Course    : Programming Languages 1 - Assignment 1 - Exercise 1
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
  val _ = TextIO.inputLine stream
  (* a function to read the pair of integer & real in subsequent lines *)
        fun scanner 0 acc = acc
          | scanner i acc =
            let
                val d = next_int stream
                val (SOME v) = Real.fromString (next_real stream)
            in
                scanner (i - 1) ((d, v) :: acc)
            end
    in
        (n, l,  rev(scanner n []))
    end


(* The SML Program: heap.sml *)

datatype tree = empty | node of tree * (real*int) * tree;
 
fun inorder empty s = s
 |  inorder (node(l,n,r)) s = inorder l (n :: (inorder r s))
 ;
(*
fun take_root empty s = s
  | take_root (node(l,n,r)) s = *)

local
    fun max (x:int) y = if x > y then x else y
    in
       fun height empty = 0
        |  height (node(l,_,r)) = 1 + max (height l) (height r)
    end;
 
    fun isfull empty = true
     |  isfull (node(l,(_,_),r)) = height l = height r  andalso isfull l andalso isfull r
      ;
  
    fun is_complete empty = true
     |  is_complete (t as node(l,_,r)) =
            isfull t orelse 
            ((height l) = (height r) + 1 andalso isfull r andalso is_complete l) orelse
            ((height l) = (height r) andalso isfull l andalso is_complete r)
 ;
(*
fun max n empty = true
     |  max n (node (l,m,r)) = n>=m andalso (is_prioritized l) andalso (is_prioritized r)
    and
        is_prioritized (empty) = true
     |  is_prioritized (node (l,n,r)) = (max n l) andalso (max n r)
 ;

fun isheap t = is_complete t andalso is_prioritized t
 *)

fun CBTinsert item empty = node (empty,item,empty)
  | CBTinsert item (t as node(l,n,r)) =
       if isfull t orelse 
          (height l = height r + 1 andalso isfull r andalso is_complete l andalso not (isfull l))
       then node (CBTinsert item l,n,r)
       else node (l,n,CBTinsert item r)
  ;
 
fun LISTtoCBT [] t = t
 |  LISTtoCBT (x::xs) t = LISTtoCBT xs (CBTinsert x t)
 ;

 
fun heapify empty = empty
 |  heapify (t as node(empty,n,r)) = t   (* assumes r=empty! *)
 |  heapify (t as node(node(l,m as (diff1,index1),r),n as (diff2,index2),empty)) =   (* assumes l,r=empty! *)
      if diff1<=diff2 then t else node (node (empty,n,empty),m,empty)
 |  heapify (t as node(l as node(l1,m as (diff1,index1),r1),n as (diff2,index2),r as node(l2,q as (diff3,index3),r2))) = 
      if diff2<=diff1 andalso diff2<=diff3 then t
      else if diff1<=diff2 andalso diff1<=diff3 then node (heapify (node(l1,n,r1)),m,r)
      else node (l,q,heapify(node(l2,n,r2)))
 ;

fun CBTtoHeap empty = empty
 |  CBTtoHeap (node(l,n,r)) = heapify (node (CBTtoHeap l, n, CBTtoHeap r));
 
fun HEAPbuild l = CBTtoHeap (LISTtoCBT l empty) :tree;
 
(*val h = HEAPbuild [503,087,512,061,908,170,897,275,653,426,154,509,612,677,765,703];
 *)

fun HEAPinsert item t = CBTtoHeap (CBTinsert item t);
 

fun HEAPmerge empty empty = empty
 |  HEAPmerge empty t = t
 |  HEAPmerge t empty = t
 |  HEAPmerge (l as node(l1,m as (diff1,index1),r1)) (r as node(l2,q as (diff2,index2),r2)) = 
      if (diff1 < diff2) then node (heapify (node(l1,q,r1)),m,r)
      else node (l,q,(heapify (node(l2,m,r2))))
 ;

(*
fun HEAPmerge t s = CBTtoHeap (LISTtoCBT (inorder t []) s);
*)
(*
fun HEAPsort empty = []
 |  HEAPsort (node(l,n,r)) = n :: HEAPsort (HEAPmerge l r);

HEAPsort h;

*)


(* A dummy solver and the function with the requested interface below *)
(*
fun walk i acc [] = ([])
  | walk i acc (y :: ys) =
        if i < n then walk (i+1) (y :: acc) ys
                 else (rev acc)


fun order [] n = []
    | order myList n =
        let
            val myArray0 = Array.array (n, (0, Real.posInf))
            val myL = fsort2 myList
            val finalArray = loopExt 0 myList (ref myArray0) n
            val retList = array_to_list (!finalArray) n
            val almost = fsort retList
            fun walk i acc [] = ([])
              | walk i acc ((y,z) :: ys) = if i < n then walk (i+1) (y::acc) ys
                                                    else (rev acc)
        in
            walk 0 [] almost
        end        
*)
fun timeCalc (p1:int, s1:real) (p2:int, s2:real) = ((Real.fromInt(p2-p1))/(s1-s2))

fun loop j [] n = []
    | loop j (_::[]) n = []
    | loop j (hd1::hd2::xs) n = 
        if (j < n) then
            let
                val time = ((timeCalc hd1 hd2), (j+2))
                val valid_time = if (#1 time > 0.0) then time else (Real.posInf, (j+2))
            in
                valid_time::(loop (j+1) (hd2::xs) n)
            end
        else
            []
            
fun b j [] n = []
    | b j (_::[]) n = []
    | b j (hd1::hd2::xs) n = 
        if (j > 0) then
            let
                val time = ((timeCalc hd1 hd2), (n-j+2))
                val valid_time = if (#1 time > 0.0) then time else (Real.posInf, (n-j+2))
            in
                valid_time::(b (j-1) (hd2::xs) n)
            end
        else
            []

fun he [] n l = []
  | he (candidates as hd::tail) n l = let
                               val li = loop 0 candidates n
                               val last_time = ((timeCalc ((#1 hd) + l, (#2 hd)) (List.last(candidates))), 1)
                               val myheap = if (#1 last_time > 0.0) then (last_time::li)
                                                                    else ((Real.posInf, 1)::li)
                             in
                               myheap
                             end 

fun heapsolve [] n l = HEAPbuild []
  | heapsolve (candidates as hd::tail) n l = let
                               val li = loop 0 candidates n
                               val last_time = ((timeCalc ((#1 hd) + l, (#2 hd)) (List.last(candidates))), 1)
                               val myheap = if (#1 last_time > 0.0) then HEAPbuild (last_time::li)
                                                                    else HEAPbuild ((Real.posInf, 1)::li)
                             in
                               myheap
                             end 
(*)
fun my_solution (0, l, candidates) = []
  | my_solution (n, 0, candidates) = []
  | my_solution (n, l, candidates) = heapsolve candidates n

fun agonas fileName = my_solution (parse fileName)

*)
  