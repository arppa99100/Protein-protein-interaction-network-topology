from slice_timepoints import slice_timepoints,cumulative_sma,running_sma,powersumavg,running_var,run

data_fl=open('summary.txt','r')

data_fl.readline()

data_lst=[[],[],[],[],[]]
for line in data_fl:
	contents=line.strip().split('\t')
	for i in range(5):
		data_lst[i].append(float(contents[i]))

variances=[]
running_var=run(data_lst[0],data_lst[1])
variances+=running_var
for i in range(2,len(data_lst)):
	running_var=run(data_lst[0],data_lst[i])
	variances.append(running_var[1])


outfl=open('running_variance.txt','w')

outfl.write('Year\tInteractions\tTransitivity\tCPL\tAssortativity\n')
for i in range(len(variances[0])):
	outfl.write(str(variances[0][i])+'\t'+str(variances[1][i])+'\t'+str(variances[2][i])+'\t'+str(variances[3][i])+'\t'+str(variances[4][i])+'\n')
