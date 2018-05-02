import random
import itertools
import subprocess
import sys, os
from random import uniform

def main():

	outputName = 'salida_' + sys.argv[1]
	outputFile = open(outputName, 'a')
	
	results = ''
	while 1:
	
		perm = [random.random() for x in range(0,12)]
		
		#args = str(perm[0]) + ' ' + str(perm[1]) + ' ' + str(perm[2]) + str(perm[3]) + ' ' + str(perm[4]) + ' ' + str(perm[5]) + str(perm[6]) + ' ' + str(perm[7]) + ' ' + str(perm[8]) + str(perm[9]) + ' ' + str(perm[10]) + ' ' + str(perm[11]) + ' \t'
		args = '(defvar *ponderaciones* \'((' + str(perm[0]) + ' ' + str(perm[1]) + ' ' + str(perm[2]) +  ' ' + str(perm[3]) + ' ' + str(perm[4]) + ' ' + str(perm[5]) + ')(' + str(perm[6]) + ' ' + str(perm[7]) + ' ' + str(perm[8]) + ' ' +  str(perm[9]) + ' ' + str(perm[10]) + ' ' + str(perm[11]) + ')))\t\t'        


		res = subprocess.run(['./script.sh', str(perm[0]), str(perm[1]), str(perm[2]),str(perm[3]), str(perm[4]), str(perm[5]),str(perm[6]), str(perm[7]), str(perm[8]),str(perm[9]), str(perm[10]), str(perm[11])], stdout=subprocess.PIPE)
		fl_res = float(res.stdout.decode('utf-8'))
		
		if fl_res > 0.9:
			results = args
			results += str(fl_res)
			results += '\n\n'
			outputFile.write(results)
			print('\t\t Hemos conseguido uno de ' + str(fl_res))

main()
