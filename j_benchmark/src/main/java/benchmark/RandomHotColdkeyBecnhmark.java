package benchmark;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.concurrent.CountDownLatch;

public class RandomHotColdkeyBecnhmark {

	private ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>> outerConcSkipListMap = new ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>>();
	private ConcurrentHashMap<Integer, ConcurrentHashMap<Integer, Integer>> outerConcHashMap = new ConcurrentHashMap<Integer, ConcurrentHashMap<Integer, Integer>>();
	private HashMap<String, TreeMap<Integer, Integer>> performanceData = new HashMap<String, TreeMap<Integer, Integer>>();
	private BufferedWriter writer;

	public RandomHotColdkeyBecnhmark(int numofInsertions, int runRepetitions,
			int maxNumberOfThreads, double threshold) throws IOException,
			InterruptedException {
		writer = new BufferedWriter(new FileWriter(new File(
				"random_hot_cold_key_selection_benchmark_" + threshold
						+ "_1000000_" + maxNumberOfThreads)));

		boolean warmUp = true;
		/* Warm-up starts here */
		benchmark(10, 10000, 64, threshold, warmUp, Util.SKIP_LIST_MAP);
		benchmark(10, 10000, 64, threshold, warmUp, Util.CONCURRENT_MAP);
		/* Warm-up ends here */

		warmUp = false;
		/* Benchmarking starts here */
		benchmark(runRepetitions, numofInsertions, maxNumberOfThreads,
				threshold, warmUp, Util.SKIP_LIST_MAP);
		benchmark(runRepetitions, numofInsertions, maxNumberOfThreads,
				threshold, warmUp, Util.CONCURRENT_MAP);

		writer.close();

	}

	private void benchmark(int runRepetitions, int numofInsertions,
			int maxNumberOfThreads, double threshold, boolean warmUp,
			String concurrencyType) throws InterruptedException, IOException {

		String mapValueType = "INT_TO_" + concurrencyType + "INT_TO_INT";
		String mapConfig = concurrencyType + "_" + mapValueType;
		performanceData.put(mapConfig, new TreeMap<Integer, Integer>());

		CountDownLatch startSignal, doneSignal;

		long startTime, endTime, elapsed = 0;
		int randomInsertRepetitions;
		Thread[] threads = new Thread[64];

		for (int numOfThreads = 1; numOfThreads <= maxNumberOfThreads; numOfThreads *= 2) {

			randomInsertRepetitions = numofInsertions / numOfThreads;

			startTime = System.currentTimeMillis();
			for (int i = 0; i < runRepetitions; i++) {
				outerConcSkipListMap = new ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>>();
				outerConcHashMap = new ConcurrentHashMap<Integer, ConcurrentHashMap<Integer, Integer>>();

				startSignal = new CountDownLatch(1);
				doneSignal = new CountDownLatch(numOfThreads);

				for (int j = 0; j < numOfThreads; j++) {
					switch (concurrencyType) {
					case Util.SKIP_LIST_MAP:
						threads[j] = new Inserter(outerConcSkipListMap,
								randomInsertRepetitions, numofInsertions,
								threshold, startSignal, doneSignal);
						break;
					case Util.CONCURRENT_MAP:
						threads[j] = new Inserter(outerConcHashMap,
								randomInsertRepetitions, numofInsertions,
								threshold, startSignal, doneSignal);
						break;
					default:
						break;
					}

					threads[j].start();
				}
				startSignal.countDown();
				doneSignal.await();
			}
			endTime = System.currentTimeMillis();
			elapsed = (endTime - startTime);
			performanceData.get(mapConfig).put(new Integer(numOfThreads),
					new Integer((int) (elapsed / runRepetitions)));
		}
		if (!warmUp) {
			writePerfData(numofInsertions, runRepetitions, threshold);
		}
	}

	private void writePerfData(int numofInsertions, int runRepetitions,
			double threshold) throws IOException {

		Integer numerOfThreads, timeTaken;
		TreeMap<Integer, Integer> perfDataPerMapType;
		Iterator<Integer> numberOfThreadsITR;
		Iterator<String> mapTypeITR = performanceData.keySet().iterator();

		String mapType = null;
		while (mapTypeITR.hasNext()) {
			mapType = mapTypeITR.next();
			perfDataPerMapType = performanceData.get(mapType);
			numberOfThreadsITR = perfDataPerMapType.keySet().iterator();
			while (numberOfThreadsITR.hasNext()) {
				numerOfThreads = numberOfThreadsITR.next();
				timeTaken = perfDataPerMapType.get(numerOfThreads);
				Util.writeLine(writer, "RANDOM_INSERTION,JAVA,-" + mapType
						+ " -inserts " + numofInsertions + " -threshold "
						+ threshold + " -threads " + numerOfThreads + " ,"
						+ timeTaken);
			}
		}
	}

	public static void main(String[] args) throws InterruptedException,
			NumberFormatException, IOException {

		try {
			for (double threshold = 0.1; threshold >= 0.1; threshold -= 0.1) {
				new RandomHotColdkeyBecnhmark(Integer.parseInt(args[0]),
						Integer.parseInt(args[1]), Integer.parseInt(args[2]),
						threshold);
			}
		} catch (Exception e) {
			System.out
					.println("Please enter the folowing input data:\n"
							+ " 1-Number of run repetitions\n"
							+ " 2-Maximum number of threads\n"
							+ " 3-Probablity of cold keys\n"
							+ "Number of run insertions is 1000000\n"
							+ "Output will be put in \"random_hot_cold_key_selection_benchmark_<Probablity of cold keys>_<Number of run insertions>_<Maximum number of threads>.csv\"");
		}

	}
}
