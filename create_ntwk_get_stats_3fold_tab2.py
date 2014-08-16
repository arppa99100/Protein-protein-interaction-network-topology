#!/usr/bin/python

import networkx as nx
import os

def num_pubs(data_dir):

	'''
	Parses tab2 files in data_diretory returning a
	dictionary with an interaction pair mapping to
	a set of reference publications. For use in
	determining 3-fold validation.
	'''

	os.chdir(data_dir)
	files=[fl for fl in os.listdir('.') if not fl.startswith('.')]
	studies={}

	for fl in files:
		data=open(fl,'r')
		data.readline()

		for line in data:
			contents=line.strip().split('\t')
			try:
				if contents[15]==contents[16]=='559292':
					pub=contents[13]
					int_a=contents[5]
					int_b=contents[6]
					if not int_a==int_b:
						if studies.get(int_a+int_b)==None:
							studies[int_a+int_b]=set([pub])
							studies[int_b+int_a]=set([pub])
						else:
							studies[int_a+int_b].add(pub)
							studies[int_b+int_a].add(pub)
			except:
				pass
	return studies



def create_network(year,data_dir,pubs):

	'''
	Builds Networkx graph using data from files in data_dir.
	Uses only 3-fold validated interactions (specified by pubs,
	a dictionary made by num_pubs().
	'''

	os.chdir(data_dir)
	files=[fl for fl in os.listdir('.') if not fl.startswith('.')]
	network=nx.Graph()

	for fl in files:
		data=open(fl,'r')
		data.readline()

		for line in data:
			contents=line.strip().split('\t')
			try:
				current_year=int(contents[13][-5:-1])
				taxid_1=contents[15]
				taxid_2=contents[16]
				int_a=contents[5]
				int_b=contents[6]
				if not int_a==int_b and taxid_1==taxid_2 and taxid_1=='559292' and current_year<=year and len(pubs[int_a+int_b])>2:
					network.add_edge(int_a,int_b)
			except:
				pass
				

	return network


def cpl(graph):

	'''
	Calculates the characteristic path length (CPL),
	equivalent to the average shortest path between
	nodes.
	'''

	shortest_paths=[]
	graph_nodes=graph.nodes()
	n=len(graph_nodes)
	for i in range(n-1):
		for j in range(i+1,n):
			try:
				shortest_paths.append(nx.shortest_path_length(graph,source=graph_nodes[i],target=graph_nodes[j]))
			except:
				pass

	return sum(shortest_paths)/len(shortest_paths)




def network_stats(data_dir,yr_start,yr_end):
	
	'''
	Returns a nested list of stats (year,interaction count,transitivity,
	characteristic path length, and assortativity.
	'''

	pubs=num_pubs(data_dir)
	stats=[[],[],[],[],[]]

	for year in range(yr_start,yr_end+1):
		ntwk=create_network(year,data_dir,pubs)
		stats[0].append(year)
		stats[1].append(len(ntwk.edges()))
		stats[2].append(nx.transitivity(ntwk))
		stats[3].append(cpl(ntwk))
		stats[4].append(nx.degree_assortativity_coefficient(ntwk))

	return stats


graph_stats=network_stats('.',1999,2014)

outfl=open('summary.txt','w')

outfl.write('Year\tInteractions\tTransitivity\tCPL\tAssortativity\n')
for i in range(len(graph_stats[0])):
	outfl.write(str(graph_stats[0][i])+'\t'+str(graph_stats[1][i])+'\t'+str(graph_stats[2][i])+'\t'+str(graph_stats[3][i])+'\t'+str(graph_stats[4][i])+'\n')