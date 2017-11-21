rec swap(a,b) = 
  let val temp = *a in a := *b; b := temp;;
val z = 3;;
val w = 4;;
swap(&z,&w);;
z:w:nil;;
