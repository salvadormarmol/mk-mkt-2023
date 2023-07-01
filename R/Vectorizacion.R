
# Necesidad de vectorización

a <- rnorm(1000000)
b <- rnorm(1000000)

ini <- Sys.time()
c=a+b
fin <- Sys.time()
print(t1<-fin-ini)

ini <- Sys.time()
for (i in 1:length(a)) {
  c[i]=a[i]+b[i]
}
fin <- Sys.time()
print(t2<-fin-ini)

print(as.numeric(t2, units = "secs")/as.numeric(t1, units = "secs"))