#!/usr/local/bin/python3
import os
import subprocess
import numpy as np
import matplotlib.pyplot as plt


def run_and_plot(name, numTerms, numThreads, numSamples):
	print(f"Running {name} experiment with 2^{numTerms} terms, up to 2^{numThreads} threads, {numSamples} samples...")
	with open(f"results/{name}.csv", 'w') as csv:
		process = subprocess.Popen(['./invoke_docker.sh', str(numTerms), str(numThreads), str(numSamples)], stdout=csv)
		process.communicate()

	data = np.loadtxt(f"results/{name}.csv", delimiter=',')
	# print(data)
	threads = data[:,0]
	times = data[:,1:] / 1000000000.0
	mean_times = times.mean(axis=1)
	std_times = times.std(axis=1)

	plt.errorbar(threads, mean_times, marker='.', yerr=std_times, label=name)


def main():
	os.system('mkdir -p results')
	
	run_and_plot("pi_native", 21, 19, 4)
	# run_and_plot("pi_native", 16, 10, 4)


	print("All done, plotting!")

	plot_path = 'results/plot.eps'
	plt.xscale('log', basex=2)
	plt.title('Execution time of concurrent Pi computation')
	plt.xlabel('Number of threads spawned')
	plt.ylabel('Total execution time (s)')
	plt.savefig(plot_path, format='eps')
	plt.show()
	
	print(f"Plot saved at: {plot_path}")

if __name__ == "__main__":
	main()