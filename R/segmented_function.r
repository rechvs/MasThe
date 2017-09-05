im Modell sieht der Aufruf so aus: (z.B. für lm und bei c=20 vorgegeben )

rhs=function(x,c) ifelse(x>c,x-c,0)

lhs=function (x,c) ifelse (x<=c,c-x,0)

lhs_15=function (x,c) ifelse (x<=c,(c-x)^1.5,0)

 

Ips.glm<-glm(Befall ~ lhs_15(Alter,120) + rhs(Miproz,20) + lhs (Miproz,20) + NFK_MODELL + Tempsum, family = binomial, data = Fi_insekt3)  # 

    
plot ( Ips.glm, residuals=T, all.term=T)
