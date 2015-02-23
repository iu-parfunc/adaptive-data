package benchmark;

import hybrid_ds.HybridIntMap;
import hybrid_ds.PureIntMap;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicReference;

import org.pcollections.IntTreePMap;

public class RandomHotColdkeyBecnhmark {

	@SuppressWarnings("rawtypes")
	private ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>> outerConcSkipListMap = new ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>>();
	private ConcurrentHashMap<Integer, ConcurrentHashMap<Integer, Integer>> outerConcHashMap = new ConcurrentHashMap<Integer, ConcurrentHashMap<Integer, Integer>>();
	private HybridIntMap<HybridIntMap<Integer>> outerHybridIntMapInnrMap;
	private PureIntMap<PureIntMap<Integer>> outerPureIntTreeMap;

	private HashMap<String, TreeMap<Integer, ArrayList<Long>>> performanceData = new HashMap<String, TreeMap<Integer, ArrayList<Long>>>();

	private Thread[] threads = new Thread[64];

	public RandomHotColdkeyBecnhmark(String dsTypeToBeBenchmarked,
			int numofInsertions, double hotKeyPercentage, int runRepetitions,
			int maxNumberOfThreads, double coldKeyProbability)
			throws IOException, InterruptedException {

		String cuncorrencyType = null, valueType = null;
		switch (dsTypeToBeBenchmarked) {
		case "mutable":
			cuncorrencyType = Util.MUTABLE_INT_TREE_MAP;
			break;
		case "scalable":
			cuncorrencyType = Util.SKIP_LIST_MAP;
			break;
		case "hybrid":
			cuncorrencyType = Util.HYBRID_MAP;
			break;
		case "pure":
			cuncorrencyType = Util.PURE_MAP;
			break;
		default:

			Util.inputFormatError("random");
			System.exit(0);
			break;
		}

		warmUp(cuncorrencyType, numofInsertions, hotKeyPercentage,
				runRepetitions, maxNumberOfThreads, coldKeyProbability);
		runBenchmark(cuncorrencyType, valueType, numofInsertions,
				hotKeyPercentage, runRepetitions, maxNumberOfThreads,
				coldKeyProbability);
		Util.writePerfData(performanceData, "random", dsTypeToBeBenchmarked,
				numofInsertions, hotKeyPercentage, coldKeyProbability);
	}

	private void runBenchmark(String cuncorrencyType, String valueType,
			int numofInsertions, double hotKeyPercentage, int runRepetitions,
			int maxNumberOfThreads, double coldKeyProbability)
			throws InterruptedException, IOException {

		randomBenchmark(cuncorrencyType, runRepetitions, numofInsertions,
				hotKeyPercentage, maxNumberOfThreads, coldKeyProbability, false);
	}

	private void randomBenchmark(String concurrencyType, int runRepetitions,
			int insertionCount, double hotKeyPercentage,
			int maxNumberOfThreads, double coldKeyProbability, boolean warmUp)
			throws InterruptedException, IOException {

		CountDownLatch startSignal, doneSignal;
		long startTime, endTime, elapsed;

		for (int numOfThreads = 1; numOfThreads <= maxNumberOfThreads; numOfThreads *= 2) {

			// System.out.println(" *** " + numOfThreads + " *** ");

			int insretionsPerThread = insertionCount / numOfThreads;

			ArrayList<Long> runTimeRecods = new ArrayList<Long>();

			for (int i = 1; i <= runRepetitions; i++) {

				initiatlizeMap(concurrencyType);
				startTime = System.currentTimeMillis();
				startSignal = new CountDownLatch(1);
				doneSignal = new CountDownLatch(numOfThreads);
				for (int j = 0; j < numOfThreads; j++) {
					switch (concurrencyType) {
					case Util.SKIP_LIST_MAP:
						threads[j] = new InsertionThread(outerConcSkipListMap,
								j * insretionsPerThread, (j + 1)
										* insretionsPerThread, insertionCount,
								hotKeyPercentage, coldKeyProbability,
								startSignal, doneSignal);
						threads[j].start();
						break;
					case Util.CONCURRENT_MAP:
						threads[j] = new InsertionThread(outerConcHashMap, j
								* insretionsPerThread, (j + 1)
								* insretionsPerThread, insertionCount,
								hotKeyPercentage, coldKeyProbability,
								startSignal, doneSignal);
						threads[j].start();
						break;
					case Util.HYBRID_MAP:
						threads[j] = new InsertionThread(
								outerHybridIntMapInnrMap, j
										* insretionsPerThread, (j + 1)
										* insretionsPerThread, insertionCount,
								hotKeyPercentage, coldKeyProbability,
								startSignal, doneSignal);
						threads[j].start();
						break;

					case Util.PURE_MAP:
						threads[j] = new InsertionThread(outerPureIntTreeMap, j
								* insretionsPerThread, (j + 1)
								* insretionsPerThread, insertionCount,
								hotKeyPercentage, coldKeyProbability,
								startSignal, doneSignal);
						threads[j].start();
						break;
					}
				}
				startSignal.countDown();
				doneSignal.await();
				endTime = System.currentTimeMillis();
				elapsed = (endTime - startTime);
				if (!warmUp) {
					runTimeRecods.add(new Long(elapsed));
				}
			}// End of FOR loop over run repetitions

			// switch (concurrencyType) {
			// case Util.HYBRID_MAP:
			// System.out.println(outerHybridIntMapInnrMap);
			// break;
			//
			// case Util.PURE_MAP:
			// System.out.println(outerPureIntTreeMap);
			// break;
			//
			// case Util.SKIP_LIST_MAP:
			// System.out.println(outerConcSkipListMap);
			// break;
			// }

			if (!warmUp) {
				String mapConfig = concurrencyType;
				performanceData.putIfAbsent(mapConfig,
						new TreeMap<Integer, ArrayList<Long>>());
				performanceData.get(mapConfig).put(new Integer(numOfThreads),
						runTimeRecods);
			}
		}// End of FOR loop over numberOfThreads
	}

	private void initiatlizeMap(String ConcurrecyType) {

		switch (ConcurrecyType) {
		case Util.SYNCHRONIZED_MAP:
			break;
		case Util.CONCURRENT_MAP:
			outerConcHashMap = new ConcurrentHashMap<Integer, ConcurrentHashMap<Integer, Integer>>();
			break;
		case Util.SKIP_LIST_MAP:
			outerConcSkipListMap = new ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>>();
			break;
		case Util.HYBRID_MAP:
			outerHybridIntMapInnrMap = new HybridIntMap<HybridIntMap<Integer>>();
			break;
		case Util.PURE_MAP:
			outerPureIntTreeMap = new PureIntMap<PureIntMap<Integer>>();
			break;
		}
	}

	private void warmUp(String cuncorrencyType, int numofInsertions,
			double hotKeyPercentage, int runRepetitions,
			int maxNumberOfThreads, double coldKeyProbability)
			throws InterruptedException, IOException {

		randomBenchmark(cuncorrencyType,
				((runRepetitions >= 10) ? runRepetitions / 10 : 1),
				numofInsertions, hotKeyPercentage, maxNumberOfThreads,
				coldKeyProbability, true);
	}

	public static void main(String[] args) {

		try {
			int i = 0;
			String dsType = args[i++];
			int numofInsertions = Integer.parseInt(args[i++]);
			double hotKeyPercentage = Double.parseDouble(args[i++]);
			int runRepetitions = Integer.parseInt(args[i++]);
			int maxNumberOfThreads = Integer.parseInt(args[i++]);
			double coldKeyProbability = Double.parseDouble(args[i++]);
			new RandomHotColdkeyBecnhmark(dsType, numofInsertions,
					hotKeyPercentage, runRepetitions, maxNumberOfThreads,
					coldKeyProbability);

		} catch (InterruptedException | IOException e) {
			e.printStackTrace();
		} catch (NumberFormatException | ArrayIndexOutOfBoundsException e) {
			Util.inputFormatError("random");
		}
	}
}