
pnd=function(base,inter){
	base=base
	inter=inter
	max_base=max(base)
	length_inter=length(inter)
	percent=length(subset(inter,inter>max_base))/length_inter
	percent=percent*100
	print('pnd')
	print(percent)
}

pem=function(base,inter){
	base=base
	inter=inter
	median_base=median(base)
	length_inter=length(inter)
	percent=length(subset(inter,inter>median_base))/length_inter
	percent=percent*100
	print('pem')
	print(percent)
}


pemt=function(base,inter){
	base=base
	inter=inter
	x=seq(1,length(base),1)
	trend=lm(base~x)
	critical=trend$coefficients[1]+trend$coefficients[2]*x[length(x)]
	length_inter=length(inter)
	percent=length(subset(inter,inter>critical))/length_inter
	percent=percent*100
	print('pemt')
	print(percent)
}

pand=function(base,inter){
	base=base
	inter=inter
	max_base=max(base)
	length_inter=length(inter)
	removed=(length_inter-length(subset(inter,inter>max_base)))
	percent=(length_inter+length(base)-removed)/(length_inter+length(base))
	percent=percent*100
	print('pand')
	print(percent)
}

ird=function(base,inter){
	base=base
	inter=inter
	length_inter=length(inter)
	length_base=length(base)
	
	base_overlap=0
	for (i in 1:length_base){
		check=0
		for(j in 1:length_inter){
			if(base[i]>=inter[j]) check=1	
		}
		if (check==1) base_overlap=base_overlap+1
	}

	inter_overlap=0
	for (i in 1:length_inter){
		check=0
		for(j in 1:length_base){
			if(inter[i]<=base[j]) check=1
		}
		if (check==1) inter_overlap=inter_overlap+1
	}
	
	if(inter_overlap<=base_overlap)
		percent=(length_inter-inter_overlap)/length_inter
	else
		percent=1-(base_overlap/length_base)

	percent=abs(percent)
	
	percent=percent*100
	print('ird')
	print(percent)	
}

pdo=function(base,inter){
	base=base
	inter=inter
	max_base=max(base)
	length_inter=length(inter)
	length_base=length(base)
	comb=length_inter*length_base
	
	num=NULL
	for (i in 1:length_inter){
		count=0
		for(j in 1:length_base){
			if(inter[i]>base[j]) count=count+1	
		}
		num[i]=count
	}
	
	percent=sum(num)/comb
	percent=percent*100
	percent2=(percent/100)^2*100
	print('pdo')
	print(percent)
	print('pdo2')
	print(percent2)
}

nap=function(base,inter){
	base=base
	inter=inter
	length_inter=length(inter)
	length_base=length(base)
	comb=length_inter*length_base

	num=NULL
	for (i in 1:length_base){
		count=0
		for(j in 1:length_inter){
			if(base[i]>inter[j]) count=count+1	
			if(base[i]==inter[j]) count=count+.5	
		}
		num[i]=count
	}
	
	percent=(comb-sum(num))/comb
	percent=percent*100
	print('nap')
	print(percent)
}


mbr=function(base,inter){
	base=base
	inter=inter
	length_inter=length(inter)
	length_base=length(base)
	m3_base=mean(base[(length_base-2):length_base])
	m3_inter=mean(inter[(length_inter-2):length_inter])
	percent=(m3_base-m3_inter)/m3_base*100
	percent=percent
	print('mbr')
	print(percent)
}


pzd=function(base,inter){
	base=base
	inter=inter
	length_inter=length(inter)
	for (i in 1:length_inter){
		if (inter[i]==0){
		 place=i
		 break
		}
	}
	count=0
	for (i in place:length_inter){
		if (inter[i]==0) count=count+1
	}
	percent=count/(length_inter-place+1)*100
	print('pzd')
	print(percent)
}

run=function(base,inter,method){
	base=base
	inter=inter
	method=method
	
	if (method==1){
		pnd(base,inter)
		pem(base,inter)
		pemt(base,inter)
		pand(base,inter)
		ird(base,inter)
		pdo(base,inter)
		nap(base,inter)
	}
	else{
		mbr(base,inter)
		pzd(base,inter)	
	}
	x=c(base,inter)
	col = rep("blue",length(x))
	col[1:length(base)]='red'
	plot(x,col=col,ylab='performance',xlab='time',main='A-B Phase Contrast')
	abline(v=length(base)+.5)
}

base=c(0,0,0,0,0)
inter=c(20,110,80)

run(base,inter,1)



