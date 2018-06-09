
library("gmapsdistance")
library("stringr")


print(paste("Qual a quantidade de Eixos do veiculo?"))
axis = scan(what=double(), nmax = 1)

print(paste("Qual a modalidade da carga?"))
print(paste("(1) Carga Geral (2) Carga Granel (3) Carga Neogranel (4) Carga Frigorificada (5) Carga Perigosa"))
modelo = scan(what=double(), nmax = 1)

if(modelo == 1){
	tabela_antt = read.table("tabela_antt_geral.csv",header = TRUE, sep=";")
}
if(modelo == 2){
	tabela_antt = read.table("tabela_antt_granel.csv",header = TRUE, sep=";")	
}
if(modelo == 3){
	tabela_antt = read.table("tabela_antt_neogranel.csv",header = TRUE, sep=";")	
}
if(modelo == 4){
	tabela_antt = read.table("tabela_antt_frigorificada.csv",header = TRUE, sep=";")	
}
if(modelo == 5){
	tabela_antt = read.table("tabela_antt_perigosa.csv",header = TRUE, sep=";")	
}
#Importação Tabela ANTT
#tabela_antt = read.table("tabela_antt.csv",header = TRUE, sep=";")
tabela_antt$Custo.KM.Eixo = gsub(",",".",tabela_antt[,3])
tabela_antt$Custo.KM.Eixo = gsub(" ","",tabela_antt[,3])
tabela_antt = data.frame(lapply(tabela_antt,as.numeric))


#Importação Mapas Cidades
dados_mapa = read.csv("mapa_cidades.csv", header = TRUE, sep=";",stringsAsFactors = FALSE)
dados_base = dados_mapa

#Gerar Distância
for(i in 1:length(dados_mapa[1,])){
	for(j in 1:length(dados_mapa[,1])){
		if(dados_mapa[j,i] != ""){	
			dados_mapa[j,i] = paste(iconv(dados_mapa[j,i], to="ASCII//TRANSLIT"),"+Brasil")
			dados_mapa[j,i] = gsub(" ","",dados_mapa[j,i])
		}
	}
}

origem = dados_mapa[(which(dados_mapa[,1]!= "")),1]
destino = dados_mapa[,2]
distancia = data.frame()

for (j in 1:length(origem)){
	for(i in 1:length(destino)){
		texto = toString(destino[i])
		planta = toString(origem[j])
		padrao_origem = toString(dados_base[j,1])
		padrao_destino = toString(dados_base[i,2])
		d = gmapsdistance(planta,texto,mode="driving")
		d = merge(d,padrao_origem)
		colnames(d) = c("Time","Distance","Status","Origem")
		d = merge(d,padrao_destino)
		colnames(d) = c("Time","Distance","Status","Origem","Destino")
		distancia = rbind(distancia,d)
	}
}
# distancia(index,Time, Distance, Status, Cidade Destino, Cidade Origem)

for(i in 1:length(distancia[,1])){
	#definir valor de Km aplicado na rota
	conversao = floor(distancia[i,2]/1000/100)
	if(conversao > 29){
		conversao = 29
	}
	#Buscar valor da taxa na tabela ANTT
	taxa = tabela_antt[(which(conversao==floor(tabela_antt[,1]/100))),3]
	frete_minimo = taxa * axis * (distancia[i,2]/1000)
	distancia$Frete_Minimo[i] = frete_minimo
}

write.csv(distancia,"Frete_Minimo.csv")