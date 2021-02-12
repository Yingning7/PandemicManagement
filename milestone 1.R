p <- as.double(readline(prompt = "please enter the sensitivity: "))
q <- as.double(readline(prompt = "please enter the specificity: "))
ran<- as.double(readline(prompt = "please enter the range of prevalence:"))

fn <- function(pi){
  ((1-p)*pi) / ((1-p)*pi+q*(1-pi))
}
fp <- function(pi){
  ((1-q)*(1-pi)) / (p*pi+(1-q)*(1-pi))
}
tp <- as.double(1 - fn(pi))
tn <- as.double(1 - fp(pi))
fn_curve<-function(pi){
  curve(((1-p)*pi) / ((1-p)*pi+q*(1-pi)), from = 0, to = ran, n = 101, add = FALSE, 
        type = "l", xname = "x", xlab = "prevalence",  log = NULL, 
        xlim = NULL, ylim = c(0,1), main = "False negative rate as 
        a function of prevalence")
  mtext(sprintf("for sensitivity= %.3f and specificity = %.3f", p, q))
  
}

fp_curve<-function(pi){
  curve(fp, from = 0, to = ran, n = 101, add = FALSE, 
        type = "l", xname = "x", xlab = "prevalence",  log = NULL, 
        xlim = NULL, ylim = c(0,1), main = "False positive rate as 
        a function of prevalence")
  mtext(sprintf("for sensitivity= %.3f and specificity = %.3f", p, q))
}
fn_curve(pi)

