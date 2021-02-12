p1 <- as.double(readline(prompt = "please enter the sensitivity of test 1: "))
q1 <- as.double(readline(prompt = "please enter the specifcity of test 1: "))
p2 <- as.double(readline(prompt = "please enter the sensitivity of test 2: "))
q2 <- as.double(readline(prompt = "please enter the specificity of test 2: "))
ran<- as.double(readline(prompt = "please enter the range of prevalence:"))
plan<-as.integer(readline(prompt = "please indicate which plan you would like to use: "))

if(plan==0){
  pt<-p1+p2*(1-p1)
  qt<-q1*q2
  print(c(pt,qt))
}
if(plan==1){
  pt<-2*p1-p1^2
  qt<-q1^2
  print(c(pt,qt))
}
if(plan==2){
  pt<-p1*p2
  qt<-q1+q2*(1-q1)
  print(c(pt,qt))
}
if(plan==3){
  pt<-p1^2
  qt<-2*q1-q1^2
  print(c(pt,qt))
}

#clear
ppvt1<-function(pi){
 pi*(p1+p2*(1-p1))/(pi*(p1+p2*(1-p1))+(1-pi)*(1-q1*q2))
}
#clear
ppvt2<-function(pi){
  pi*(2*p1-p1^2)/(pi*(2*p1-p1^2)+(1-pi)*(1-q1^2))
}
#wrong
ppvt3<-function(pi){
  pi*p1*p2/(pi*p1*p2+(1-pi)*(1-(q1+q2*(1-q1))))
}
#wrong
ppvt4<-function(pi){
  pi*p1^2/(pi*p1^2+(1-pi)*(1-(2*q1-q1^2)))
}
ppv_curves<-function(pi){
  if(plan==0){
  curve(ppvt1, from = 0, to = ran, n = 101, add = FALSE, 
        type = "l", xname = "x", xlab = "prevalence",  log = NULL, 
        xlim = NULL, ylim = c(0,1), main = "Positive predictive values (combined) 
          as a function of prevalence")
  mtext("For scenario 1, two tests are different")}
  if(plan==1){
    curve(ppvt2, from = 0, to = ran, n = 101, add = FALSE, 
          type = "l", xname = "x", xlab = "prevalence",  log = NULL, 
          xlim = NULL, ylim = c(0,1), main = "Positive predictive values (combined) 
          as a function of prevalence")
  mtext("For scenario 1, two tests are the same")}
  if(plan==2){
    curve(ppvt3, from = 0, to = ran, n = 101, add = FALSE, 
          type = "l", xname = "x", xlab = "prevalence",  log = NULL, 
          xlim = NULL, ylim = c(0,1), main = "Positive predictive values (combined) 
          as a function of prevalence")
  mtext("For scenario 2, two tests are different")
  }
  if(plan==3){
    curve(ppvt4, from = 0, to = ran, n = 101, add = FALSE, 
          type = "l", xname = "x", xlab = "prevalence",  log = NULL, 
          xlim = NULL, ylim = c(0,1), main = "Positive predictive values (combined) 
          as a function of prevalence")
  mtext("For scenario 2, two tests are the same")
  }
}
ppv_curves(pi)
#wrong
npvt1<-function(pi){
  (1-pi)*q1*q2/((1-pi)*q1*q2+pi*(1-p1-p2+p1*p2))
}
#wrong
npvt2<-function(pi){
 (1-pi)*q1^2/((1-pi)*q1^2+pi*(1-(2*p1-p1^2)))
}
npvt3<-function(pi){
  (1-pi)*(q1+q2*(1-q1))/((1-pi)*(q1+q2*(1-q1))+pi*(1-p1*p2))
}
npvt4<-function(pi){
  (1-pi)*(2*q1-q1^2)/((1-pi)*(2*q1-q1^2)+pi*(1-p1^2))
}
npv_curves<-function(pi){
  if(plan==0){
    curve(npvt1, from = 0, to = ran, n = 101, add = FALSE, 
          type = "l", xname = "x", xlab = "prevalence",  log = NULL, 
          xlim = NULL, ylim = c(0,1), main = "Negative predictive values (combined)
          as a function of prevalence")
    mtext("For scenario 1, two tests are different")}
  if(plan==1){
    curve(npvt2, from = 0, to = ran, n = 101, add = FALSE, 
          type = "l", xname = "x", xlab = "prevalence",  log = NULL, 
          xlim = NULL, ylim = c(0,1), main = "Negative predictive values (combined)
          as a function of prevalence")
    mtext("For scenario 1, two tests are the same")}
  if(plan==2){
    curve(npvt3, from = 0, to = ran, n = 101, add = FALSE, 
          type = "l", xname = "x", xlab = "prevalence",  log = NULL, 
          xlim = NULL, ylim = c(0,1), main = "Negative predictive values (combined)
          as a function of prevalence")
    mtext("For scenario 2, two tests are different")
  }
  if(plan==3){
    curve(npvt4, from = 0, to = ran, n = 101, add = FALSE, 
          type = "l", xname = "x", xlab = "prevalence",  log = NULL, 
          xlim = NULL, ylim = c(0,1), main = "Negative predictive values (combined)
          as a function of prevalence")
    mtext("For scenario 2, two tests are the same")
  }
}

#npv_curves(pi)
