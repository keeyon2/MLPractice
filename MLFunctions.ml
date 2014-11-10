fun Partician x [] (ListLess : int list, ListMore : int list) = 
    (ListLess, ListMore)
 | Partician x (origFirst::origRest) (ListLess : int list, ListMore : int list) = 
     if (x > origFirst) then (Partician x origRest ([origFirst] @ ListLess,
     ListMore))
     else (Partician x origRest (ListLess, ([origFirst] @ ListMore))); 

fun SwitchFirstLast [] = [] 
 | SwitchFirstLast (x::xs) = xs @ [x];


val TestList = [5, 1, 6, 13, 5, 6, 7, 76, 4];
val Tup = Partician 6 TestList ([],[]);

fun intPartitionSort (x::xs) = 
     let val TupTup = Partician x xs ([],[])
         val (Tup1 : int list) = #1TupTup
         val (Tup2 : int list) = #2TupTup
         val (LowerList : int list) = intPartitionSort Tup1
         val (HigherList : int list)  = intPartitionSort Tup2
         in ((LowerList @ [x]) @ HigherList)
     end
 | intPartitionSort _ = [];

