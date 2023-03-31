#### Dados ####
totalprec<-readr::read_delim("dados/dados.csv", delim = ";",
                          escape_double = FALSE, trim_ws = TRUE)

#### 1 dia ####

### Índice de concordância de Willmott modificado
mdch<- totalprec |>
  dplyr::group_by(estacao) |>
  dplyr::summarise(md_chirps= hydroGOF::md(Precipitacao,chirps))

mdps<- na.omit(totalprec) |>
  dplyr::group_by(estacao) |>
  dplyr::summarise(md_persiann= hydroGOF::md(Precipitacao,persiann))

cor(totalprec$Precipitacao, totalprec$persiann,
    method = "spearman"
)
## teste t
t.test(mdch$md_chirps,mdps$md_persiann)

### Correlação de Spearman###

corch<- na.omit(totalprec) |>
  dplyr::group_by(estacao) |>
  dplyr::summarise(cor_chirps= cor(Precipitacao,chirps,
                            method = "spearman"
  ))

corps<- na.omit(totalprec) |>
  dplyr::group_by(estacao) |>
  dplyr::summarise(cor_persiann= cor(Precipitacao,persiann,
                            method = "spearman"))
t.test(corch$cor_chirps,corps$cor_persiann)

p1d<-dplyr::full_join(mdch,mdps,corch,corps)

#### 2 dias####
## Função##
p2d = function (PA009) {

  prec = as.numeric()
  for (i in 1461:2){
    temp = 0
    for (j in 1:0){
      if (is.na(PA009[i-j,'Precipitacao'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'Precipitacao']
      }
    }
    prec[i] = temp
    print(prec[i])
  }

  chirps<-as.numeric()
  for (i in 1461:2){
    temp = 0
    for (j in 1:0){
      if (is.na(PA009[i-j,'chirps'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'chirps']
      }
    }
    chirps[i] = temp
    print(chirps[i])
  }

  persiann<-as.numeric()
  for (i in 1461:2){
    temp = 0
    for (j in 1:0){
      if (is.na(PA009[i-j,'persiann'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'persiann']
      }
    }
    persiann[i] = temp
    print(persiann[i])
  }

  P2dA009<-as.data.frame(cbind(PA009$Data, as.numeric(unlist(prec)),as.numeric(unlist(chirps)), as.numeric(unlist(persiann))))
  names(P2dA009) <- c("Data", "P_est", "chirps", "persiann")
  return(P2dA009)
}

est = unique(totalprec$estacao)
df = p2d(totalprec[totalprec$estacao==est[1],])
df$est = est[1]
for(i in 2:42){
  temp = p2d(totalprec[totalprec$estacao==est[i],])
  temp$est = est[i]
  df = rbind(df,temp)
}

### Índice de concordância de Willmott modificado
mdch<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(md_chirps= hydroGOF::md(as.numeric(P_est),as.numeric(chirps)))

mdps<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(md_persiann= hydroGOF::md(as.numeric(P_est),as.numeric(persiann)))


## teste t
t.test(mdch$md_chirps,mdps$md_persiann)

### Correlação de Spearman###

corch<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(cor_chirps= cor(as.numeric(P_est),as.numeric(chirps),
                                   method = "spearman"
  ))

corps<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(cor_persiann= cor(as.numeric(P_est),as.numeric(persiann),
                                     method = "spearman"))
t.test(corch$cor_chirps,corps$cor_persiann)



#### 3 dias####
## Função##
p3d = function (PA009) {

  prec = as.numeric()
  for (i in 1461:3){
    temp = 0
    for (j in 2:0){
      if (is.na(PA009[i-j,'Precipitacao'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'Precipitacao']
      }
    }
    prec[i] = temp
    print(prec[i])
  }

  chirps<-as.numeric()
  for (i in 1461:3){
    temp = 0
    for (j in 2:0){
      if (is.na(PA009[i-j,'chirps'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'chirps']
      }
    }
    chirps[i] = temp
    print(chirps[i])
  }

  persiann<-as.numeric()
  for (i in 1461:3){
    temp = 0
    for (j in 2:0){
      if (is.na(PA009[i-j,'persiann'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'persiann']
      }
    }
    persiann[i] = temp
    print(persiann[i])
  }

  P2dA009<-as.data.frame(cbind(PA009$Data, as.numeric(unlist(prec)),as.numeric(unlist(chirps)), as.numeric(unlist(persiann))))
  names(P2dA009) <- c("Data", "P_est", "chirps", "persiann")
  return(P2dA009)
}

est = unique(totalprec$estacao)
df = p3d(totalprec[totalprec$estacao==est[1],])
df$est = est[1]
for(i in 2:42){
  temp = p3d(totalprec[totalprec$estacao==est[i],])
  temp$est = est[i]
  df = rbind(df,temp)
}

### Índice de concordância de Willmott modificado
mdch<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(md_chirps= hydroGOF::md(as.numeric(P_est),as.numeric(chirps)))

mdps<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(md_persiann= hydroGOF::md(as.numeric(P_est),as.numeric(persiann)))


## teste t
t.test(mdch$md_chirps,mdps$md_persiann)

### Correlação de Spearman###

corch<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(cor_chirps= cor(as.numeric(P_est),as.numeric(chirps),
                                   method = "spearman"
  ))

corps<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(cor_persiann= cor(as.numeric(P_est),as.numeric(persiann),
                                     method = "spearman"))
t.test(corch$cor_chirps,corps$cor_persiann)

#### 4 dias####
## Função##
p4d = function (PA009) {

  prec = as.numeric()
  for (i in 1461:4){
    temp = 0
    for (j in 3:0){
      if (is.na(PA009[i-j,'Precipitacao'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'Precipitacao']
      }
    }
    prec[i] = temp
    print(prec[i])
  }

  chirps<-as.numeric()
  for (i in 1461:4){
    temp = 0
    for (j in 3:0){
      if (is.na(PA009[i-j,'chirps'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'chirps']
      }
    }
    chirps[i] = temp
    print(chirps[i])
  }

  persiann<-as.numeric()
  for (i in 1461:4){
    temp = 0
    for (j in 3:0){
      if (is.na(PA009[i-j,'persiann'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'persiann']
      }
    }
    persiann[i] = temp
    print(persiann[i])
  }

  P2dA009<-as.data.frame(cbind(PA009$Data, as.numeric(unlist(prec)),as.numeric(unlist(chirps)), as.numeric(unlist(persiann))))
  names(P2dA009) <- c("Data", "P_est", "chirps", "persiann")
  return(P2dA009)
}

est = unique(totalprec$estacao)
df = p4d(totalprec[totalprec$estacao==est[1],])
df$est = est[1]
for(i in 2:42){
  temp = p4d(totalprec[totalprec$estacao==est[i],])
  temp$est = est[i]
  df = rbind(df,temp)
}

### Índice de concordância de Willmott modificado
mdch<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(md_chirps= hydroGOF::md(as.numeric(P_est),as.numeric(chirps)))

mdps<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(md_persiann= hydroGOF::md(as.numeric(P_est),as.numeric(persiann)))


## teste t
t.test(mdch$md_chirps,mdps$md_persiann)

### Correlação de Spearman###

corch<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(cor_chirps= cor(as.numeric(P_est),as.numeric(chirps),
                                   method = "spearman"
  ))

corps<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(cor_persiann= cor(as.numeric(P_est),as.numeric(persiann),
                                     method = "spearman"))
t.test(corch$cor_chirps,corps$cor_persiann)

#### 5 dias####
## Função##
p5d = function (PA009) {

  prec = as.numeric()
  for (i in 1461:5){
    temp = 0
    for (j in 4:0){
      if (is.na(PA009[i-j,'Precipitacao'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'Precipitacao']
      }
    }
    prec[i] = temp
    print(prec[i])
  }

  chirps<-as.numeric()
  for (i in 1461:5){
    temp = 0
    for (j in 4:0){
      if (is.na(PA009[i-j,'chirps'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'chirps']
      }
    }
    chirps[i] = temp
    print(chirps[i])
  }

  persiann<-as.numeric()
  for (i in 1461:5){
    temp = 0
    for (j in 4:0){
      if (is.na(PA009[i-j,'persiann'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'persiann']
      }
    }
    persiann[i] = temp
    print(persiann[i])
  }

  P2dA009<-as.data.frame(cbind(PA009$Data, as.numeric(unlist(prec)),as.numeric(unlist(chirps)), as.numeric(unlist(persiann))))
  names(P2dA009) <- c("Data", "P_est", "chirps", "persiann")
  return(P2dA009)
}

est = unique(totalprec$estacao)
df = p5d(totalprec[totalprec$estacao==est[1],])
df$est = est[1]
for(i in 2:42){
  temp = p5d(totalprec[totalprec$estacao==est[i],])
  temp$est = est[i]
  df = rbind(df,temp)
}

### Índice de concordância de Willmott modificado
mdch<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(md_chirps= hydroGOF::md(as.numeric(P_est),as.numeric(chirps)))

mdps<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(md_persiann= hydroGOF::md(as.numeric(P_est),as.numeric(persiann)))


## teste t
t.test(mdch$md_chirps,mdps$md_persiann)

### Correlação de Spearman###

corch<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(cor_chirps= cor(as.numeric(P_est),as.numeric(chirps),
                                   method = "spearman"
  ))

corps<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(cor_persiann= cor(as.numeric(P_est),as.numeric(persiann),
                                     method = "spearman"))
t.test(corch$cor_chirps,corps$cor_persiann)

#### 10 dias####
## Função##
p10d = function (PA009) {

  prec = as.numeric()
  for (i in 1461:10){
    temp = 0
    for (j in 9:0){
      if (is.na(PA009[i-j,'Precipitacao'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'Precipitacao']
      }
    }
    prec[i] = temp
    print(prec[i])
  }

  chirps<-as.numeric()
  for (i in 1461:10){
    temp = 0
    for (j in 9:0){
      if (is.na(PA009[i-j,'chirps'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'chirps']
      }
    }
    chirps[i] = temp
    print(chirps[i])
  }

  persiann<-as.numeric()
  for (i in 1461:10){
    temp = 0
    for (j in 9:0){
      if (is.na(PA009[i-j,'persiann'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'persiann']
      }
    }
    persiann[i] = temp
    print(persiann[i])
  }

  P2dA009<-as.data.frame(cbind(PA009$Data, as.numeric(unlist(prec)),as.numeric(unlist(chirps)), as.numeric(unlist(persiann))))
  names(P2dA009) <- c("Data", "P_est", "chirps", "persiann")
  return(P2dA009)
}

est = unique(totalprec$estacao)
df = p10d(totalprec[totalprec$estacao==est[1],])
df$est = est[1]
for(i in 2:42){
  temp = p10d(totalprec[totalprec$estacao==est[i],])
  temp$est = est[i]
  df = rbind(df,temp)
}

### Índice de concordância de Willmott modificado
mdch<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(md_chirps= hydroGOF::md(as.numeric(P_est),as.numeric(chirps)))

mdps<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(md_persiann= hydroGOF::md(as.numeric(P_est),as.numeric(persiann)))


## teste t
t.test(mdch$md_chirps,mdps$md_persiann)

### Correlação de Spearman###

corch<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(cor_chirps= cor(as.numeric(P_est),as.numeric(chirps),
                                   method = "spearman"
  ))

corps<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(cor_persiann= cor(as.numeric(P_est),as.numeric(persiann),
                                     method = "spearman"))
t.test(corch$cor_chirps,corps$cor_persiann)


#### 15 dias####
## Função##
p15d = function (PA009) {

  prec = as.numeric()
  for (i in 1461:15){
    temp = 0
    for (j in 14:0){
      if (is.na(PA009[i-j,'Precipitacao'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'Precipitacao']
      }
    }
    prec[i] = temp
    print(prec[i])
  }

  chirps<-as.numeric()
  for (i in 1461:15){
    temp = 0
    for (j in 14:0){
      if (is.na(PA009[i-j,'chirps'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'chirps']
      }
    }
    chirps[i] = temp
    print(chirps[i])
  }

  persiann<-as.numeric()
  for (i in 1461:15){
    temp = 0
    for (j in 14:0){
      if (is.na(PA009[i-j,'persiann'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'persiann']
      }
    }
    persiann[i] = temp
    print(persiann[i])
  }

  P2dA009<-as.data.frame(cbind(PA009$Data, as.numeric(unlist(prec)),as.numeric(unlist(chirps)), as.numeric(unlist(persiann))))
  names(P2dA009) <- c("Data", "P_est", "chirps", "persiann")
  return(P2dA009)
}

est = unique(totalprec$estacao)
df = p15d(totalprec[totalprec$estacao==est[1],])
df$est = est[1]
for(i in 2:42){
  temp = p15d(totalprec[totalprec$estacao==est[i],])
  temp$est = est[i]
  df = rbind(df,temp)
}

### Índice de concordância de Willmott modificado
mdch<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(md_chirps= hydroGOF::md(as.numeric(P_est),as.numeric(chirps)))

mdps<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(md_persiann= hydroGOF::md(as.numeric(P_est),as.numeric(persiann)))


## teste t
t.test(mdch$md_chirps,mdps$md_persiann)

### Correlação de Spearman###

corch<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(cor_chirps= cor(as.numeric(P_est),as.numeric(chirps),
                                   method = "spearman"
  ))

corps<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(cor_persiann= cor(as.numeric(P_est),as.numeric(persiann),
                                     method = "spearman"))
t.test(corch$cor_chirps,corps$cor_persiann)


#### 30 dias####
## Função##
p30d = function (PA009) {

  prec = as.numeric()
  for (i in 1461:30){
    temp = 0
    for (j in 29:0){
      if (is.na(PA009[i-j,'Precipitacao'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'Precipitacao']
      }
    }
    prec[i] = temp
    print(prec[i])
  }

  chirps<-as.numeric()
  for (i in 1461:30){
    temp = 0
    for (j in 29:0){
      if (is.na(PA009[i-j,'chirps'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'chirps']
      }
    }
    chirps[i] = temp
    print(chirps[i])
  }

  persiann<-as.numeric()
  for (i in 1461:30){
    temp = 0
    for (j in 29:0){
      if (is.na(PA009[i-j,'persiann'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'persiann']
      }
    }
    persiann[i] = temp
    print(persiann[i])
  }

  P2dA009<-as.data.frame(cbind(PA009$Data, as.numeric(unlist(prec)),as.numeric(unlist(chirps)), as.numeric(unlist(persiann))))
  names(P2dA009) <- c("Data", "P_est", "chirps", "persiann")
  return(P2dA009)
}

est = unique(totalprec$estacao)
df = p30d(totalprec[totalprec$estacao==est[1],])
df$est = est[1]
for(i in 2:42){
  temp = p30d(totalprec[totalprec$estacao==est[i],])
  temp$est = est[i]
  df = rbind(df,temp)
}

### Índice de concordância de Willmott modificado
mdch<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(md_chirps= hydroGOF::md(as.numeric(P_est),as.numeric(chirps)))

mdps<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(md_persiann= hydroGOF::md(as.numeric(P_est),as.numeric(persiann)))


## teste t
t.test(mdch$md_chirps,mdps$md_persiann)

### Correlação de Spearman###

corch<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(cor_chirps= cor(as.numeric(P_est),as.numeric(chirps),
                                   method = "spearman"
  ))

corps<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(cor_persiann= cor(as.numeric(P_est),as.numeric(persiann),
                                     method = "spearman"))
t.test(corch$cor_chirps,corps$cor_persiann)

#### 60 dias####
## Função##
p60d = function (PA009) {

  prec = as.numeric()
  for (i in 1461:60){
    temp = 0
    for (j in 59:0){
      if (is.na(PA009[i-j,'Precipitacao'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'Precipitacao']
      }
    }
    prec[i] = temp
    print(prec[i])
  }

  chirps<-as.numeric()
  for (i in 1461:60){
    temp = 0
    for (j in 59:0){
      if (is.na(PA009[i-j,'chirps'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'chirps']
      }
    }
    chirps[i] = temp
    print(chirps[i])
  }

  persiann<-as.numeric()
  for (i in 1461:60){
    temp = 0
    for (j in 59:0){
      if (is.na(PA009[i-j,'persiann'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'persiann']
      }
    }
    persiann[i] = temp
    print(persiann[i])
  }

  P2dA009<-as.data.frame(cbind(PA009$Data, as.numeric(unlist(prec)),as.numeric(unlist(chirps)), as.numeric(unlist(persiann))))
  names(P2dA009) <- c("Data", "P_est", "chirps", "persiann")
  return(P2dA009)
}

est = unique(totalprec$estacao)
df = p60d(totalprec[totalprec$estacao==est[1],])
df$est = est[1]
for(i in 2:42){
  temp = p60d(totalprec[totalprec$estacao==est[i],])
  temp$est = est[i]
  df = rbind(df,temp)
}

### Índice de concordância de Willmott modificado
mdch<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(md_chirps= hydroGOF::md(as.numeric(P_est),as.numeric(chirps)))

mdps<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(md_persiann= hydroGOF::md(as.numeric(P_est),as.numeric(persiann)))


## teste t
t.test(mdch$md_chirps,mdps$md_persiann)

### Correlação de Spearman###

corch<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(cor_chirps= cor(as.numeric(P_est),as.numeric(chirps),
                                   method = "spearman"
  ))

corps<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(cor_persiann= cor(as.numeric(P_est),as.numeric(persiann),
                                     method = "spearman"))
t.test(corch$cor_chirps,corps$cor_persiann)

#### 120 dias####
## Função##
p120d = function (PA009) {

  prec = as.numeric()
  for (i in 1461:120){
    temp = 0
    for (j in 119:0){
      if (is.na(PA009[i-j,'Precipitacao'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'Precipitacao']
      }
    }
    prec[i] = temp
    print(prec[i])
  }

  chirps<-as.numeric()
  for (i in 1461:120){
    temp = 0
    for (j in 119:0){
      if (is.na(PA009[i-j,'chirps'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'chirps']
      }
    }
    chirps[i] = temp
    print(chirps[i])
  }

  persiann<-as.numeric()
  for (i in 1461:120){
    temp = 0
    for (j in 119:0){
      if (is.na(PA009[i-j,'persiann'])){
        temp = NA
        break
      }
      else{
        temp = temp + PA009[(i-j), 'persiann']
      }
    }
    persiann[i] = temp
    print(persiann[i])
  }

  P2dA009<-as.data.frame(cbind(PA009$Data, as.numeric(unlist(prec)),as.numeric(unlist(chirps)), as.numeric(unlist(persiann))))
  names(P2dA009) <- c("Data", "P_est", "chirps", "persiann")
  return(P2dA009)
}

est = unique(totalprec$estacao)
df = p120d(totalprec[totalprec$estacao==est[1],])
df$est = est[1]
for(i in 2:42){
  temp = p120d(totalprec[totalprec$estacao==est[i],])
  temp$est = est[i]
  df = rbind(df,temp)
}

### Índice de concordância de Willmott modificado
mdch<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(md_chirps= hydroGOF::md(as.numeric(P_est),as.numeric(chirps)))

mdps<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(md_persiann= hydroGOF::md(as.numeric(P_est),as.numeric(persiann)))


## teste t
t.test(mdch$md_chirps,mdps$md_persiann)

### Correlação de Spearman###

corch<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(cor_chirps= cor(as.numeric(P_est),as.numeric(chirps),
                                   method = "spearman"
  ))

corps<- na.omit(df) |>
  dplyr::group_by(est) |>
  dplyr::summarise(cor_persiann= cor(as.numeric(P_est),as.numeric(persiann),
                                     method = "spearman"))
t.test(corch$cor_chirps,corps$cor_persiann)


