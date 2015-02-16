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
import java.util.concurrent.atomic.AtomicReference;

import org.pcollections.IntTreePMap;

public class RandomHotColdkeyBecnhmark {

	private ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>> outerConcSkipListMap = new ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>>();
	private ConcurrentHashMap<Integer, ConcurrentHashMap<Integer, Integer>> outerConcHashMap = new ConcurrentHashMap<Integer, ConcurrentHashMap<Integer, Integer>>();
	private AtomicReference<IntTreePMap<AtomicReference<IntTreePMap<Integer>>>> outerMutableIntTreeMap;
	private HashMap<String, TreeMap<Integer, Integer>> performanceData = new HashMap<String, TreeMap<Integer, Integer>>();
	private BufferedWriter writer;

	public RandomHotColdkeyBecnhmark(int numofInsertions,
			double hotKeyPercentage, int runRepetitions,
			int maxNumberOfThreads, double coldKeyProbability)
			throws IOException, InterruptedException {
		String outputFileName = String.format(
				"random_hot_cold_key_selection_benchmark_%f_%d_%f_%d.csv",
				coldKeyProbability, numofInsertions, hotKeyPercentage,
				maxNumberOfThreads);
		writer = new BufferedWriter(new FileWriter(new File(outputFileName)));

		warmUp(numofInsertions, hotKeyPercentage, runRepetitions,
				maxNumberOfThreads, coldKeyProbability);
		runBenchmark(numofInsertions, hotKeyPercentage, runRepetitions,
				maxNumberOfThreads, coldKeyProbability);
		writer.close();

	}

	private void runBenchmark(int numofInsertions, double hotKeyPercentage,
			int runRepetitions, int maxNumberOfThreads,
			double coldKeyProbability) throws InterruptedException, IOException {
		benchmark(Util.SKIP_LIST_MAP, runRepetitions, numofInsertions,
				hotKeyPercentage, maxNumberOfThreads, coldKeyProbability);
		benchmark(Util.CONCURRENT_MAP, runRepetitions, numofInsertions,
				hotKeyPercentage, maxNumberOfThreads, coldKeyProbability);
		benchmark(Util.MUTABLE_INT_TREE_MAP, runRepetitions, numofInsertions,
				hotKeyPercentage, maxNumberOfThreads, coldKeyProbability);
		
		writePerfData(numofInsertions, hotKeyPercentage, coldKeyProbability);
	}

	private void benchmark(String concurrencyType, int runRepetitions,
			int numofInsertions, double hotKeyPercentage,
			int maxNumberOfThreads, double coldKeyProbability)
			throws InterruptedException, IOException {

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
			for (int i = 1; i <= runRepetitions; i++) {

				outerConcSkipListMap = new ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>>();
				outerConcHashMap = new ConcurrentHashMap<Integer, ConcurrentHashMap<Integer, Integer>>();
				outerMutableIntTreeMap = new AtomicReference(
						IntTreePMap.empty());

				startSignal = new CountDownLatch(1);
				doneSignal = new CountDownLatch(numOfThreads);

				for (int j = 0; j < numOfThreads; j++) {
					switch (concurrencyType) {
					case Util.SKIP_LIST_MAP:
						threads[j] = new Inserter(outerConcSkipListMap,
								randomInsertRepetitions, numofInsertions,
								hotKeyPercentage, coldKeyProbability,
								startSignal, doneSignal);
						threads[j].start();
						break;
					case Util.CONCURRENT_MAP:
						threads[j] = new Inserter(outerConcHashMap,
								randomInsertRepetitions, numofInsertions,
								hotKeyPercentage, coldKeyProbability,
								startSignal, doneSignal);
						threads[j].start();
						break;
					case Util.MUTABLE_INT_TREE_MAP:
						threads[j] = new Inserter(outerMutableIntTreeMap,
								randomInsertRepetitions, numofInsertions,
								hotKeyPercentage, coldKeyProbability,
								startSignal, doneSignal, j);
						threads[j].start();
						break;
					default:
						break;
					}
				}
				startSignal.countDown();
				doneSignal.await();
			}
			endTime = System.currentTimeMillis();
			elapsed = (endTime - startTime);
			performanceData.get(mapConfig).put(new Integer(numOfThreads),
					new Integer((int) (elapsed / runRepetitions)));
		}
	}

	private void writePerfData(int numofInsertions, double hotKeyPercentage,
			double coldKeyProbability) throws IOException {

		Util.writeLine(writer, "PROGNAME,VARIANT,ARGS,AVERAGE_TIME");
		
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
						+ " -inserts " + numofInsertions
						+ " -hotKeyPercentage " + hotKeyPercentage
						+ " -coldKeyOperationProbability " + coldKeyProbability
						+ " -maxNumberOfThreads " + numerOfThreads + " ,"
						+ timeTaken);
			}
		}
	}

	private void warmUp(int numofInsertions, double hotKeyPercentage,
			int runRepetitions, int maxNumberOfThreads,
			double coldKeyProbability) throws InterruptedException, IOException {
		benchmark(Util.SKIP_LIST_MAP,
				((runRepetitions >= 10) ? runRepetitions / 10 : 1),
				numofInsertions, hotKeyPercentage, maxNumberOfThreads,
				coldKeyProbability);
		benchmark(Util.CONCURRENT_MAP,
				((runRepetitions >= 10) ? runRepetitions / 10 : 1),
				numofInsertions, hotKeyPercentage, maxNumberOfThreads,
				coldKeyProbability);
		benchmark(Util.MUTABLE_INT_TREE_MAP,
				((runRepetitions >= 10) ? runRepetitions / 10 : 1),
				numofInsertions, hotKeyPercentage, maxNumberOfThreads,
				coldKeyProbability);
	}

	public static void main(String[] args) {

		try {
 
			int numofInsertions = Integer.parseInt(args[0]);
			double hotKeyPercentage = Double.parseDouble(args[1]);
			int runRepetitions = Integer.parseInt(args[2]);
			int maxNumberOfThreads = Integer.parseInt(args[3]);
			double coldKeyProbability = Double.parseDouble(args[4]);
			new RandomHotColdkeyBecnhmark(numofInsertions, hotKeyPercentage,
					runRepetitions, maxNumberOfThreads, coldKeyProbability);

		} catch (InterruptedException | IOException e) {
			e.printStackTrace();
		} catch (NumberFormatException | ArrayIndexOutOfBoundsException e) {
			System.out
					.println("Please enter the folowing input data:\n"
							+ " 1-Number of insertions\n"
							+ " 2-Hot key percentage over key range from 0 to the number of insertions (double in range [0-100])\n"
							+ " 3-Number of run repetitions\n"
							+ " 4-Maximum number of threads\n"
							+ " 5-Probablity of cold key operation (double in range [0-1])\n"
							+ "Output will be put in \"random_hot_cold_key_selection_benchmark_<Probablity of cold keys>_<Number of run insertions>_<Hot key percentage>_<Maximum number of threads>.csv\"");
		}
	}
}
