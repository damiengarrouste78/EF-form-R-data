#########################
sigmoide = function(x){return(1/(1+exp(-x)))}
logit = function(x){return(log(x/(1-x)))}



################################
# Comptage taux de cible
comptage = function(data)
{
	effectifs=dim(data)[1];
	effectifs_Cible = table(data$cible);
	freq_Cible = effectifs_Cible/effectifs;
	return(data.frame(cbind(effectifs, effectifs_Cible,freq_Cible)))
}

################################
# Significativite globale
pValue_global = function(reg)
{
	return(pchisq(reg$null.deviance - reg$deviance,reg$df.null - reg$df.residual,lower.tail=FALSE));

}


################################
# Calcul du Lift pour alpha
lift = function(cible, score, alpha)
	{     
  		index = order(score, decreasing=T)
		n_data=length(cible)
  		m = trunc(alpha*n_data)
            cible_sort=cible[index[1:m]];
  		return(length(cible_sort[cible_sort==1])/length(cible_sort)/(length(cible[cible==1])/n_data))
	}


####################### 
# Courbe lift 
courbe_lift = function(cible,score,nbre_point)
	{     
	      cible = as.numeric(as.character(cible));
		courbe = vector();courbe[1]= 0;
            score_dec = sort(score,decreasing = TRUE,index.return = TRUE)
		cible_dec = cible[score_dec$ix]
		n = round(length(cible)/nbre_point)+1;
		for (i in 1:(nbre_point))
		{ 
		  cible_restrict = cible_dec[1:(i*n)]
		  courbe[i+1] = length(which(cible_restrict ==1))/length(which(cible==1))
		}
		return(courbe)
      }



courbe_ideale = function(cible,modY,nbre_point)
	{
	 courbe = vector();
	 courbe[1] = 0;
       abs = length(which(cible == modY))/length(cible);
       numero_point_abs = round(abs*nbre_point)
	 for (i in (numero_point_abs):(nbre_point)){courbe[i+1] =1}
       a = 1/abs;
	 for (i in 1:(numero_point_abs-1)){courbe[i+1] = a*i/nbre_point}
	return(courbe)
      }

courbe_hasard = function(nbre_point)
	{
	return(seq(from=0,to=1,length=(nbre_point+1)));
      }



