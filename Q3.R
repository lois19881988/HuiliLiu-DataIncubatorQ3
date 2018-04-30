library(rvest)
library(plyr)
library(stringr)
library(MASS) 
library(lattice)
library(ggplot2)
library(devtools)
library(Rcpp)
library(ggplot2)
library(gridExtra)
library(RCurl)
library(jpeg)

# 0. Func for img brightness                  

calculate_img_brt <- function(img_url) {
  if (is.na(img_url)){
    brt_mean <- NA
  } else {
    tmp_file = tempfile()
    download.file(img_url, tmp_file, mode="wb")
    img = readJPEG(tmp_file)
    file.remove(tmp_file)
    dim_img <- dim(img)
    if (is.na(dim_img[3]) | dim_img[3] != 3 | dim_img[1] == 0 | dim_img[2] == 0){
      brt_mean <- NA
      print(paste0("pic wrong: dim of"))
      print(dim_img)
    } else {
      brt_sum <- 0
      
      for (j in 1:dim_img[1]){
        per_row <- NULL
        for (k in 1:dim_img[2]){
          r <- img[j, k, 1]
          g <- img[j, k, 2]
          b <- img[j, k, 3]
          brt_per_pic <- max(r, g, b)
          
          if (brt_sum == 0){
            brt_sum <- brt_per_pic
          } else {
            brt_sum <- sum(brt_sum, brt_per_pic)
          }
        }
      }
      brt_mean <- brt_sum / (dim_img[1] * dim_img[2])
    }
  }
  return(brt_mean)
}


# 1. Basice parameters                  

totl_num <- 9953
num_p_pg <- 100
pg_num <- ceiling(totl_num/num_p_pg)
num_last_pg <- totl_num - num_p_pg * (pg_num - 1)
url_base <- "https://www.imdb.com/list/ls057823854/"

object_name_list <- c("title", "year", "type","gross","rate","meta_score","img_url","audiance")
CSS_selector_list <- c(".lister-item-header a",
                       ".lister-item-year.text-muted.unbold",
                       ".genre",
                       ".text-muted .ghost~ .text-muted+ span",
                       ".ipl-rating-star.small .ipl-rating-star__rating",
                       ".inline-block.ratings-metascore",
                       ".loadlate",
                       ".certificate")


# 2. Scrape IMDB

all_data <- NULL
for(p in 1:pg_num){
  if(p == 1){
    url <- paste0(url_base)
  }else{
    url <- paste0(url_base, "?page=", p, sep="")
  }
  
  webpage <- read_html(url)
  movie_box_all <- html_nodes(webpage, ".lister-item.mode-detail")
  
  if (length(movie_box_all) != num_p_pg & length(movie_box_all) != num_last_pg ){
    print(paste("Something wrong with the url of ",url,
                ". There are ",length(movie_box_all), " movies on this page.", sep=""))
  }
  
  each_page <- NULL
  for (b in 1:length(movie_box_all)) {
    each_movie <- NULL
    movie_box <- movie_box_all[b]
    
    for (o in 1:length(object_name_list)){
      object_name <- object_name_list[o]
      CSS_selector <- CSS_selector_list[o]
      data_point_html <- html_nodes(movie_box, CSS_selector)
      if (object_name != "img_url"){
        text <- html_text(data_point_html)
      } else {
        
        # structure is different 
        all_text <- as.character(data_point_html)
        text <- str_match(all_text, 'loadlate=\"(https.*\\.jpg)\"')[2]
      }
      
      if(length(text) == 0){
        text <- NA
      }
      
      data_point <- as.data.frame(text)
      names(data_point)[1] <- object_name
      
      if (is.null(each_movie)){
        each_movie <- data_point
      } else {
        each_movie <- data.frame(each_movie, data_point)
      }
    }
    
    img_url_per_movie <- as.character(each_movie$img_url[1])
    img_brt <- calculate_img_brt(img_url = img_url_per_movie)
    
    each_movie$img_brt <- img_brt
    
    if (is.null(each_page)){
      each_page <- each_movie
    } else {
      each_page <- rbind(each_page, each_movie)
    }
  }
  
  if (is.null(all_data)){
    all_data <- each_page
  } else {
    all_data <- rbind(all_data, each_page)
  }
}
dim(all_data)

all_data$year <- as.numeric(str_match(all_data$year, "\\(([0-9]*)\\)")[,2])
all_data$type <- gsub(" ", "", str_match(all_data$type, "\\n([[[:alpha:]]*,[[:blank:]]]*)")[,2])
all_data$gross <- as.numeric(gsub("\\$|M","",all_data$gross))
all_data$rate <- as.numeric(as.character(all_data$rate))
all_data$meta_score <- as.numeric(str_match(all_data$meta_score, "\\n([0-9]{1,3})")[,2])


# 3. data cleaning

all_no_miss <- subset(all_data, !is.na(year) & !is.na(img_brt) & !is.na(type) & !is.na(gross) & !is.na(audiance) & !is.na(rate))

dup_img_url <- ddply(all_no_miss,.(img_url),nrow)
names(dup_img_url)[2]<-"dup_img_url"
dup_img_url[which(dup_img_url$dup_img_url>1),]

clean_data <- all_no_miss

# 4. indicator variables

# 4.1 type indicators
type_list <- c()
for (i  in 1:length(clean_data$type)) {
  type_single_row <- unlist(strsplit(clean_data$type[i], "[,]"))
  if (is.null(type_list)) {
    type_list <- type_single_row
  } else {
    type_list <- c(type_list, type_single_row)
  }
}
uniq_type_list <- unique(type_list)

for (t in 1:length(uniq_type_list)){
  type <- uniq_type_list[t]
  type_indi_name <- paste0(type, "_indi", sep = "")
  clean_data[, type_indi_name] <- ifelse(grepl(type, clean_data$type) == T, 1, 0)
}


# 4.2. brightness deviance

mean_brt_per_type_list <- c()
for (t in 1:length(uniq_type_list)){
  type <- uniq_type_list[t]
  type_indi_name <- paste0(type, "_indi", sep = "")
  mean_brt_per_type <- mean(subset(clean_data, eval(parse(text = type_indi_name)) == 1)$img_brt,na.rm = T)
  names(mean_brt_per_type) <- type
  mean_brt_per_type_list <- c(mean_brt_per_type_list,mean_brt_per_type)
}

all_type_indicators <- clean_data[, paste(uniq_type_list, "_indi", sep = "")]
clean_data$exp_brt <- rowSums(as.data.frame(mapply("*",all_type_indicators,mean_brt_per_type_list))/rowSums(all_type_indicators) )
clean_data$brt_bias <- clean_data$img_brt - clean_data$exp_brt
clean_data$abs_brt_bias <- abs(clean_data$img_brt - clean_data$exp_brt)

# 5. Plots and Regression

# 1st RQ

all_ty_means <- NULL
for (t in 1:length(uniq_type_list)){
  type <- uniq_type_list[t]
  type_indi_name <- paste0(type, "_indi", sep = "")
  type_means <- aggregate(img_brt ~ eval(parse(text = type_indi_name)), data = clean_data, FUN = "mean")
  names(type_means)[1] <- "genre_indi"
  type_means$genre <- type
  
  type_means$genre_indi <- ifelse(is.na(type_means$genre_indi),NA,
                             ifelse(type_means$genre_indi == 1, "Yes", "No"))
  
  lm_result <- lm(clean_data[, "img_brt"] ~ clean_data[,type_indi_name])
  pvalue <- summary(lm_result)$coefficients[,"Pr(>|t|)"][2]
  
  type_means$sig_indi_1_from_0 <- ifelse(is.na(pvalue), NA,
                                         ifelse(pvalue >= 0.05, 0, 1))
  if(is.null(all_ty_means)){
    all_ty_means <- type_means
  } else {
    all_ty_means <- rbind(all_ty_means, type_means)
  }
}

all_ty_means$rgb <- paste("#", substr(as.character(round(all_ty_means$img_brt,4)), 3, 4) ,
                            substr(as.character(round(all_ty_means$img_brt,4)), 3, 4) ,
                            substr(as.character(round(all_ty_means$img_brt,4)), 3, 4) , sep = "")

all_ty_means$img_brt_sig <- ifelse(all_ty_means$sig_indi_1_from_0 == 1,
                                     paste(round(all_ty_means$img_brt,2), "*", sep = ""),
                                     paste(round(all_ty_means$img_brt,2), sep = ""))

all_ty_means <- all_ty_means[order(all_ty_means$genre, all_ty_means$genre_indi),]

# Plot
plot_1 <- ggplot(data = all_ty_means, aes(y = img_brt), 
       ylim = c((min(all_ty_means$img_brt)-0.05),(max(all_ty_means$img_brt)+0.05))) +
  geom_bar(aes(genre_indi), stat = "identity", fill = all_ty_means$rgb, 
           width = 0.7) +  
  facet_wrap(~ genre, nrow = 4) +
  geom_text(aes(x = genre_indi,label = img_brt_sig), vjust=1.6, color="white", size=3) + 
  xlab("") + 
  ylab("Mean Brightness") + 
  ggtitle("Figure1. Comparative Brightness Averages") 

# 2nd RQ
RQ2_list <- list()
for (t in 1:length(uniq_type_list)) {
  type <- uniq_type_list[t]
  type_indi <- paste(type, "_indi", sep="")
  
  g <- ggplot(clean_data, aes(x=abs_brt_bias, y=rate, 
                                  shape=as.factor(eval(parse(text = type_indi))), 
                                  color=as.factor(eval(parse(text = type_indi))))) +
    geom_point() + 
    geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
    scale_shape_manual(values=c(3, 17))+ 
    scale_color_manual(values=c("gray70","darkorange"))+
    xlab(type) + 
    ylab("IMDB Rating") + 
    ggtitle("") +
    theme(legend.title=element_blank(),legend.position = "none",
          axis.title.x = element_text(size=7, face="bold"),
          axis.title.y = element_text( size=7, face="bold"),
          axis.text.x = element_text(size=7),
          axis.text.y = element_text( size=7)
    )
 
    RQ2_list [[t]] <- g
}

#Extract Legend 
g_legend<-function(g_plot){ 
  tmp <- ggplot_gtable(ggplot_build(g_plot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)
  } 

g_plot_w_legend <- ggplot(clean_data, aes(x=abs_brt_bias, y=rate, 
                                                      shape=as.factor(eval(parse(text = type_indi))), 
                                                      color=as.factor(eval(parse(text = type_indi))))) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_shape_manual(values=c(3, 17))+ 
  scale_color_manual(values=c("gray70","darkorange"))+
  xlab(paste("Brightness Deviation: ", type, sep = "")) + 
  ylab("IMDB Rating") + 
  ggtitle("") +
  theme(legend.title=element_blank(),legend.position = "top",
        axis.title.x = element_text(size=7, face="bold"),
        axis.title.y = element_text( size=7, face="bold"),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text( size=7)
  )
legend <- g_legend(g_plot_w_legend) 
RQ2_list [[length(uniq_type_list)+1]] <- legend
plot_2 <- grid.arrange(grobs = RQ2_list , 
                       widths = rep(1,6), 
                       layout_matrix = matrix(c(seq(1, length(uniq_type_list)+1, by = 1), NA),
                                                nrow= 4, 
                                                ncol = 6, byrow = T))


