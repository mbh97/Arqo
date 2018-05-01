import random
import itertools
import subprocess
import sys, os
from random import uniform

def main():

	p = [random.random() for x in range(0,12)]
	#p = [uniform(0, 3) for x in range(0,3)]
	perms = itertools.permutations(p)
	ponderaciones = ''

	for perm in perms:
		#String de llamada al script
		args = str(perm[0]) + ' ' + str(perm[1]) + ' ' + str(perm[2]) + str(perm[3]) + ' ' + str(perm[4]) + ' ' + str(perm[5]) + str(perm[6]) + ' ' + str(perm[7]) + ' ' + str(perm[8]) + str(perm[9]) + ' ' + str(perm[10]) + ' ' + str(perm[11]) + ' \t'
		ponderaciones += args

		#Llamamos loop veces al script guardand la media de los resultados en una variable        
		loop = 10
		res = subprocess.run(['./script.sh', str(perm[0]), str(perm[1]), str(perm[2]),str(perm[3]), str(perm[4]), str(perm[5]),str(perm[6]), str(perm[7]), str(perm[8]),str(perm[9]), str(perm[10]), str(perm[11])], stdout=subprocess.PIPE)
		fl_res = float(res.stdout.decode('utf-8'))
		media = fl_res/loop
		
		ponderaciones += str(media)
		ponderaciones += '\n'

	outputName = 'salida_' + sys.argv[1]
	outputFile = open(outputName, 'w')
	outputFile.write(ponderaciones)

main()
