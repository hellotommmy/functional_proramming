
val a=new();;
val b=new();;
a:=3;;
b:=4;;
!a;;
!b;;
val swap(a,b) = let val temp = new() in temp := !a; a:=!b; b:=!temp;;
swap(a,b);;
!a;;
!b;; 
