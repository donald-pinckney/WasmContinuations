#!/usr/local/bin/python3
import os
import subprocess
import numpy as np
import matplotlib.pyplot as plt
import time


def run_and_plot(name, execPath, numTerms, numThreads, numSamples):
	print(f"Running {name} experiment with 2^{numTerms} terms, up to 2^{numThreads} threads, {numSamples} samples...")

	t0 = time.time()

	with open(f"results/{name}.csv", 'w') as csv:
		process = subprocess.Popen(['./invoke_docker.sh', execPath, str(numTerms), str(numThreads), str(numSamples)], stdout=csv)
		process.communicate()

	dt = time.time() - t0
	print(f"\tDone in {dt} seconds.")

	data = np.loadtxt(f"results/{name}.csv", delimiter=',')
	# print(data)
	threads = data[:,0]
	times = data[:,1:] / 1000000000.0
	mean_times = times.mean(axis=1)
	std_times = times.std(axis=1)

	plt.errorbar(threads, mean_times, marker='.', yerr=std_times, label=name)


def main():
	os.system('mkdir -p results')

	terms = 19
	threads = 19
	samples = 4

	terms_wasm = 19
	threads_wasm = 18
	samples_wasm = 4
	
	run_and_plot("pi_native", "bin/pi", terms, threads, samples)
	run_and_plot("pi_wasm_node", "./pi_wasm_wrapper.sh", terms_wasm, threads_wasm, samples_wasm)

	print("All done, plotting!")

	plot_path = 'results/plot.eps'
	plt.xscale('log', basex=2)
	plt.title(f'Execution time of concurrent computation of 2^{terms} terms of Pi series')
	plt.xlabel('Number of threads spawned')
	plt.ylabel('Total execution time (s)')
	plt.legend()
	plt.savefig(plot_path, format='eps')
	plt.show()
	
	print(f"Plot saved at: {plot_path}")

if __name__ == "__main__":
	main()