# pre_link1.csv predicted link function values for cross-validation

#Mar3,2012, use another setting of parameters
c(rep(c(1,0,-1),40),rep(c(1,0,0),40),rep(c(0,0,0),40))->s1
as.matrix(dummy.sim1)%*%s1-40/3->link1