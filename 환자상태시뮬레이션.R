#####
#github test!
######
num_p = 5 #number of people
num_m = 36 #months 
num_simu = 100 #number of simulations

state_of_p <- array(data=NA, dim=c(num_p, num_simu,num_m)) #empty array
normal_random <- array(data=NA, dim=c(num_p,num_simu,num_m))

#fill distribution with random number
#k=number of people, i=number of simulation
for(k in 1:5){
  for(i in 1:num_simu){
    normal_random[k,i,]<-rnorm(num_m, 0, 1)} 
}

#normal_random[1,1,10]
#set patient's score in first month
state_of_p[1,,1]<-38
state_of_p[2,,1]<-37
state_of_p[3,,1]<-40
state_of_p[4,,1]<-39
state_of_p[5,,1]<-36

#run as number of people, simulation, months(35)
for (m in 1:num_p) {
  for (i in 1:num_simu) {
    for (k in 1:(num_m-1)) {
      state_of_p[m,i,k+1] <- state_of_p[m,i,k]-(0.34*1+0.86*normal_random[m,i,k])
      if(state_of_p[m,i,k+1] <= 0){
        state_of_p[m,i,k+1] <- 0}
    }
  }
}

state_of_p[1,,]

p1 <- state_of_p[1,,]
p2 <- state_of_p[2,,]
p3 <- state_of_p[3,,]
p4 <- state_of_p[4,,]
p5 <- state_of_p[5,,]

#patient 1 case
col.mean <- (apply(p1, 2, sum)/100)
col.mean<-as.data.frame(col.mean)
col.mean<-within(col.mean,{
  p_state=as.integer(col.mean)
  p_state[col.mean>=40] ="mild"
  p_state[col.mean>=20 & col.mean<40] = "moderate"
  p_state[col.mean>=10 & col.mean<20] = "severe"
  p_state[col.mean>0 & col.mean<10] = "vegetative"
  p_state[col.mean=0] = "dead"
})
p1_stat <- as.data.frame(col.mean$p_state)

#patient 2 case
col.mean <- (apply(p2, 2, sum)/100)
col.mean<-as.data.frame(col.mean)
col.mean<-within(col.mean,{
  p_state=as.integer(col.mean)
  p_state[col.mean>=40] ="mild"
  p_state[col.mean>=20 & col.mean<40] = "moderate"
  p_state[col.mean>=10 & col.mean<20] = "severe"
  p_state[col.mean>0 & col.mean<10] = "vegetative"
  p_state[col.mean=0] = "dead"
})

p2_stat <- as.data.frame(col.mean$p_state)


#patient 3 case
col.mean <- (apply(p3, 2, sum)/100)
col.mean<-as.data.frame(col.mean)
col.mean<-within(col.mean,{
  p_state=as.integer(col.mean)
  p_state[col.mean>=40] ="mild"
  p_state[col.mean>=20 & col.mean<40] = "moderate"
  p_state[col.mean>=10 & col.mean<20] = "severe"
  p_state[col.mean>0 & col.mean<10] = "vegetative"
  p_state[col.mean=0] = "dead"
})

p3_stat <- as.data.frame(col.mean$p_state)

#patient 4 case
col.mean <- (apply(p4, 2, sum)/100)
col.mean<-as.data.frame(col.mean)
col.mean<-within(col.mean,{
  p_state=as.integer(col.mean)
  p_state[col.mean>=40] ="mild"
  p_state[col.mean>=20 & col.mean<40] = "moderate"
  p_state[col.mean>=10 & col.mean<20] = "severe"
  p_state[col.mean>0 & col.mean<10] = "vegetative"
  p_state[col.mean=0] = "dead"
})

p4_stat <- as.data.frame(col.mean$p_state)

#patient 5 case
col.mean <- (apply(p5, 2, sum)/100)
col.mean<-as.data.frame(col.mean)
col.mean<-within(col.mean,{
  p_state=as.integer(col.mean)
  p_state[col.mean>=40] ="mild"
  p_state[col.mean>=20 & col.mean<40] = "moderate"
  p_state[col.mean>=10 & col.mean<20] = "severe"
  p_state[col.mean>0 & col.mean<10] = "vegetative"
  p_state[col.mean=0] = "dead"
})

p5_stat <- as.data.frame(col.mean$p_state)

stat_res <- cbind(p1_stat,p2_stat,p3_stat,p4_stat,p5_stat)#bind each patient's status table

count<- array(data=0, dim=c(num_m,num_stat))#empty array for counting


for (j in 1:36){
  for (i in 1:5){
    if (stat_res[j,i] == 'mild'){
      count[j,1] = count[j,1] + 1
      print(i)
    }
    if (stat_res[j,i] == 'moderate'){
      count[j,2] = count[j,2] + 1
      print(i)
    }
    if (stat_res[j,i] == 'severe'){
      count[j,3] = count[j,3] + 1
      print(i)
    }
    if (stat_res[j,i] == 'vegetative'){
      count[j,4] = count[j,4] + 1
      print(i)
    }
    if (stat_res[j,i] == 'dead'){
      count[j,5] = count[j,5] + 1
      print(i)
    }
  }
}

colnames(count) <- c("mild","moderate","severe","vegetative","dead")
result <- count
result


#write.csv(state_of_p[1,,],"C:\\Users\\yubin\\Desktop\\시뮬레이션\\첫번째환자.csv")
#write.csv(state_of_p[2,,],"C:\\Users\\yubin\\Desktop\\시뮬레이션\\두번째환자.csv")
#write.csv(state_of_p[3,,],"C:\\Users\\yubin\\Desktop\\시뮬레이션\\세번째환자.csv")
#write.csv(state_of_p[4,,],"C:\\Users\\yubin\\Desktop\\시뮬레이션\\네번째환자.csv")
#write.csv(state_of_p[5,,],"C:\\Users\\yubin\\Desktop\\시뮬레이션\\다섯번째환자.csv")
