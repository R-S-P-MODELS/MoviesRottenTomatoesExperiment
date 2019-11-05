ReviewRottenTomatoes<-function(link){

 data=readLines(link)
#grepl("#audience_reviews",data)
filme=unlist(strsplit(link,split="/"))
filme=filme[length(filme)]
#ind=which(grepl("#audience_reviews",data)) +3
require(tm)

Reviewers=stripWhitespace(data[3+which(grepl("#contentReviews",data))])
Reviewers=as.numeric(unlist(strsplit(Reviewers,"%")))

RatingCandidates=which(grepl("Rating",data))

RatingCandidates=RatingCandidates[which.min(abs(RatingCandidates-which(grepl("genres=",data))[1]))]
if(length(RatingCandidates)>0){
RatingCandidates=data[RatingCandidates+1]

RatingCandidates=unlist(strsplit(RatingCandidates,">"))

RatingCandidates=RatingCandidates[length(RatingCandidates)]
RatingCandidates=unlist(strsplit(RatingCandidates," "))[1]
} else{
RatingCandidates=NA
}


 Genero=unlist(strsplit(unlist(strsplit(data[which(grepl("genres=",data))[1] ],c("<"))),">"))
Genero=Genero[which(grepl("genres",Genero)) +1]

Users=stripWhitespace(data[3+which(grepl("#audience_reviews",data))])
Users=as.numeric(unlist(strsplit(Users,"%")))
if(length(Users)==0)
	Users=NA
if(length(Reviewers)==0)
	Reviewers=NA
frame=data.frame(filme,Reviewers,Users,Genero,RatingCandidates)

return(frame)

}


FindRottenLinks<-function(link){
 source("WebScrape.R")
links=ObtainLinks2(url=link)
links=unlist(links)
Candidatos=unique(unlist(links)[which(grepl("/m/",unlist(links)))])
Candidatos=Candidatos[!grepl("href",Candidatos)]
Candidatos=Candidatos[!grepl("rottentomatoes",Candidatos)]
Candidatos=paste("https://rottentomatoes.com",Candidatos,sep="")
return(Candidatos)
}
