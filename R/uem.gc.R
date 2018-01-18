

## iterative garbage control:

# iterates until the selected RAM value remains constant at each gc()

uem.gc = function(){
  gc = gc()
  while(gc()[2,4]<gc[2,4]) gc = gc()
}
