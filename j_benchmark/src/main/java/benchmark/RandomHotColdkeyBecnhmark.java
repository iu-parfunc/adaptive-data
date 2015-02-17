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
	private boolean append = true;

	public RandomHotColdkeyBecnhmark(String dsTypeToBeBenchmarked,
			int numofInsertions, double hotKeyPercentage, int runRepetitions,
			int maxNumberOfThreads, double coldKeyProbability)
			throws IOException, InterruptedException {
		String outputFileName = "random_hot_cold_key.csv";
		if (!new File(outputFileName).exists()) {
			append = false;
		}
		writer = new BufferedWriter(new FileWriter(new File(outputFileName),
				append));

		String cuncorrencyType = null, valueType = null;

		switch (dsTypeToBeBenchmarked) {
		case "pure":
			cuncorrencyType = Util.MUTABLE_INT_TREE_MAP;
			break;
		case "scalable":
			cuncorrencyType = Util.SKIP_LIST_MAP;
			break;
		case "hybrid":
			cuncorrencyType = Util.HYBRID_MAP;
			System.out.print("TBD");
			System.exit(0);
			break;
		case "JavaConc":
			cuncorrencyType = Util.CONCURRENT_MAP;
			break;
		default:
			System.out
					.println("Please insert one of the following data structures be benchmarked\n"
							+ "pure|scalable|hybrid|JavaConc");
			System.exit(0);
			break;
		}

		warmUp(cuncorrencyType, valueType, numofInsertions, hotKeyPercentage,
				runRepetitions, maxNumberOfThreads, coldKeyProbability);
		runBenchmark(cuncorrencyType, valueType, numofInsertions,
				hotKeyPercentage, runRepetitions, maxNumberOfThreads,
				coldKeyProbability);
		writePerfData(dsTypeToBeBenchmarked, numofInsertions, hotKeyPercentage,
				coldKeyProbability);
		writer.close();
	}

	private void runBenchmark(String cuncorrencyType, String valueType,
			int numofInsertions, double hotKeyPercentage, int runRepetitions,
			int maxNumberOfThreads, double coldKeyProbability)
			throws InterruptedException, IOException {

		benchmark(cuncorrencyType, runRepetitions, numofInsertions,
				hotKeyPercentage, maxNumberOfThreads, coldKeyProbability);
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
			// System.out.println(numOfThreads + "*** >>> "
			// + outerMutableIntTreeMap);
		}
	}

	private void writePerfData(String dsTypeToBeBenchmarked,
			int numofInsertions, double hotKeyPercentage,
			double coldKeyProbability) throws IOException {

		if (!append) {
			Util.writeLine(writer, "PROGNAME,VARIANT,ARGS,AVERAGE_TIME");
		}

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
				Util.writeLine(writer, "RANDOM_INSERTION,JAVA,-"
						+ dsTypeToBeBenchmarked + " -#inserts "
						+ numofInsertions + " -hotKeyPercentage "
						+ hotKeyPercentage + " -coldKeyProbability "
						+ coldKeyProbability + " -#threads " + numerOfThreads
						+ " ," + timeTaken);
			}
		}
	}

	private void warmUp(String cuncorrencyType, String valueType,
			int numofInsertions, double hotKeyPercentage, int runRepetitions,
			int maxNumberOfThreads, double coldKeyProbability)
			throws InterruptedException, IOException {

		benchmark(cuncorrencyType,
				((runRepetitions >= 10) ? runRepetitions / 10 : 1),
				numofInsertions, hotKeyPercentage, maxNumberOfThreads,
				coldKeyProbability);
	}

	public static void main(String[] args) {

		try {

			String dsTypeToBeBenchmarked = args[0];
			int numofInsertions = Integer.parseInt(args[1]);
			double hotKeyPercentage = Double.parseDouble(args[2]);
			int runRepetitions = Integer.parseInt(args[3]);
			int maxNumberOfThreads = Integer.parseInt(args[4]);
			double coldKeyProbability = Double.parseDouble(args[5]);
			new RandomHotColdkeyBecnhmark(dsTypeToBeBenchmarked,
					numofInsertions, hotKeyPercentage, runRepetitions,
					maxNumberOfThreads, coldKeyProbability);

		} catch (InterruptedException | IOException e) {
			e.printStackTrace();
		} catch (NumberFormatException | ArrayIndexOutOfBoundsException e) {
			System.out
					.println("Please enter the folowing input data:\n"
							+ " 1-Data structure to bebenchmarked (pure|scalable|hybrid|JavaConc)\n"
							+ " 2-Number of insertions\n"
							+ " 3-Hot key percentage over key range from 0 to the number of insertions (double in range [0-100])\n"
							+ " 4-Number of run repetitions\n"
							+ " 5-Maximum number of threads\n"
							+ " 6-Probablity of cold key operation (double in range [0-1])\n"
							+ "Output will be put in \"random_hot_cold_key_<DS Type>_<Probablity of cold keys>_<Number of run insertions>_<Hot key percentage>_<Maximum number of threads>.csv\"");
		}
	}
}
