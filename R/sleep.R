sleep<-function(x){
# Let's the system sleep for x seconds (to slow down plotting of points, for example)
  start.time<-Sys.time()
  while((Sys.time()-start.time)<x){}
}