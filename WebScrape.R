ScrapeTable<-function(url){ # dado um link obtenha as tabelas associadas como lista
require(XML)
library(RCurl)
teste=getURL(url)
return(readHTMLTable(teste) )
}

ObtainLinks<-function(url){ # dado um link que contem sublinks obter estes links
 
doc <- htmlParse(url)
links <- xpathSApply(doc, "//a/@href")
free(doc)
return(links)
}

ObtainLinks2<-function(url){ # Caso a primeira funcao nao rode, implementação alternativa
html <- paste(readLines(url), collapse="\n")
library(stringr)
matched <- str_match_all(html, "<a href=\"(.*?)\"")
return(matched)
}

LinksParaDownload<-function(url1){ # Retorno Limpo das funcoes acima onde teremos um vetor de strings onde cada string e o link completo de acesso
z<-ObtainLinks2(url1)
linksFim=paste(url1,z[[1]][,2],sep="")
return(linksFim)
}
