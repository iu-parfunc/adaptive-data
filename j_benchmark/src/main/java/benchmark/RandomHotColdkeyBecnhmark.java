package benchmark;

import hybrid_ds.HybridIntMap;
import hybrid_ds.PureIntMap;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.concurrent.CountDownLatch;

public class RandomHotColdkeyBecnhmark {

	private ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>> outerConcSkipListMap;
	private ConcurrentSkipListMap<Integer, ConcurrentHashMap<Integer, Integer>> outerConcHashMap;
	private ConcurrentSkipListMap<Integer, HybridIntMap<Integer>> outerHybridIntMapInnrMap;
	private ConcurrentSkipListMap<Integer, PureIntMap<Integer>> outerPureIntTreeMap;

	private HashMap<String, TreeMap<Integer, ArrayList<Long>>> performanceData = new HashMap<String, TreeMap<Integer, ArrayList<Long>>>();

	private Thread[] threads = new Thread[64];

	public RandomHotColdkeyBecnhmark(String dsTypeToBeBenchmarked,
			int numInsertions, int numHotKey, int runRepetitions,
			int maxNumThreads, double hotRatio, long runStartTimestamp,
			int casTries) throws IOException, InterruptedException {

		String cuncorrencyType = null, valueType = null;
		Util.setCasTries(casTries);
		switch (dsTypeToBeBenchmarked) {
		case "scalable":
			cuncorrencyType = Util.SKIP_LIST_MAP;
			break;
		case "hybrid":
			cuncorrencyType = Util.HYBRID_MAP;
			break;
		case "pure":
			cuncorrencyType = Util.PURE_MAP;
			break;
		case "concurrent":
			cuncorrencyType = Util.CONCURRENT_MAP;
			break;
		default:

			Util.inputFormatError("random");
			System.exit(0);
			break;
		}

		warmUp(cuncorrencyType, numInsertions, numHotKey, runRepetitions,
				maxNumThreads, hotRatio, casTries);
		runBenchmark(cuncorrencyType, valueType, numInsertions, numHotKey,
				runRepetitions, maxNumThreads, hotRatio, casTries);
		Util.writePerfData(performanceData, "random", dsTypeToBeBenchmarked,
				numInsertions, numHotKey, hotRatio, runStartTimestamp, casTries);
	}

	private void runBenchmark(String cuncorrencyType, String valueType,
			int numInsertions, int numHotKey, int runRepetitions,
			int maxNumThreadsmaxNumThreadss, double hotRatio, int casTries)
			throws InterruptedException, IOException {

		randomBenchmark(cuncorrencyType, runRepetitions, numInsertions,
				numHotKey, maxNumThreadsmaxNumThreadss, hotRatio, false,
				casTries);
	}

	private void warmUp(String cuncorrencyType, int numInsertions,
			int numHotKey, int runRepetitions, int maxNumThreadsmaxNumThreadss,
			double hotRatio, int casTries) throws InterruptedException,
			IOException {

		randomBenchmark(cuncorrencyType,
				((runRepetitions >= 10) ? runRepetitions / 10 : 1),
				numInsertions, numHotKey, maxNumThreadsmaxNumThreadss,
				hotRatio, true, casTries);
	}

	private void randomBenchmark(String concurrencyType, int runRepetitions,
			int insertionCount, int numHotKey, int maxNumThreadsmaxNumThreadss,
			double hotRatio, boolean warmUp, int casTries)
			throws InterruptedException, IOException {

		CountDownLatch startSignal, doneSignal;
		long startTime, endTime, elapsed;

		for (int numThreads = 1; numThreads <= maxNumThreadsmaxNumThreadss; numThreads++) {

			int numInsretionsPerThread = insertionCount / numThreads;

			ArrayList<Long> timeTakenForRounds = new ArrayList<Long>();

			for (int i = 1; i <= runRepetitions; i++) {

				// System.out.println("\nnumThreads is " + numThreads);
				initiatlizeMap(concurrencyType, casTries);
				startTime = System.currentTimeMillis();
				startSignal = new CountDownLatch(1);
				doneSignal = new CountDownLatch(numThreads);
				for (int j = 0; j < numThreads; j++) {
					threads[j] = new InsertionThread(outerConcSkipListMap,
							outerConcHashMap, outerPureIntTreeMap,
							outerHybridIntMapInnrMap, concurrencyType, j
									* numInsretionsPerThread, (j + 1)
									* numInsretionsPerThread, insertionCount,
							numHotKey, hotRatio, casTries, startSignal,
							doneSignal);
					threads[j].start();
				}
				startSignal.countDown();
				doneSignal.await();
				endTime = System.currentTimeMillis();
				elapsed = (endTime - startTime);
				if (!warmUp) {
					timeTakenForRounds.add(new Long(elapsed));
				}
				// logLastMapping(concurrencyType);
			}// End of FOR loop over run repetitions

			if (!warmUp) {

				Util.recordTimeTaken(performanceData, timeTakenForRounds,
						concurrencyType, numThreads);
			}
		}// End of FOR loop over numThreads
	}

	private void logLastMapping(String concurrencyType) {
		switch (concurrencyType) {
		case Util.SKIP_LIST_MAP:
			System.out.println(outerConcSkipListMap);
			break;
		case Util.HYBRID_MAP:
			System.out
					.println(outerHybridIntMapInnrMap
							.get(outerHybridIntMapInnrMap.keySet().iterator()
									.next()).size());
			break;
		case Util.PURE_MAP:
			System.out.println(outerPureIntTreeMap);
			break;
		case Util.CONCURRENT_MAP:
			System.out.println(outerConcHashMap);
		}
		System.out.println();
	}

	private void initiatlizeMap(String ConcurrecyType, int casTries) {

		switch (ConcurrecyType) {
		case Util.SYNCHRONIZED_MAP:
			break;
		case Util.CONCURRENT_MAP:
			outerConcHashMap = new ConcurrentSkipListMap<Integer, ConcurrentHashMap<Integer, Integer>>();
			break;
		case Util.SKIP_LIST_MAP:
			outerConcSkipListMap = new ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>>();
			break;
		case Util.HYBRID_MAP:
			outerHybridIntMapInnrMap = new ConcurrentSkipListMap<Integer, HybridIntMap<Integer>>();
			break;
		case Util.PURE_MAP:
			outerPureIntTreeMap = new ConcurrentSkipListMap<Integer, PureIntMap<Integer>>();
			break;
		}
	}

	public static void main(String[] args) {

		try {
			int i = 0;
			String dsType = args[i++];
			int numInsertions = Integer.parseInt(args[i++]);
			int numHotKey = Integer.parseInt(args[i++]);
			int runRepetitions = Integer.parseInt(args[i++]);
			int maxNumThreadsmaxNumThreadss = Integer.parseInt(args[i++]);
			double hotRatio = Double.parseDouble(args[i++]);
			int casTries = Integer.parseInt(args[i++]);
			long runStartTimestamp = Long.parseLong(args[i++]);
			new RandomHotColdkeyBecnhmark(dsType, numInsertions, numHotKey,
					runRepetitions, maxNumThreadsmaxNumThreadss, hotRatio,
					runStartTimestamp, casTries);

		} catch (InterruptedException | IOException e) {
			e.printStackTrace();
			System.exit(1);
		} catch (NumberFormatException | ArrayIndexOutOfBoundsException e) {
			Util.inputFormatError("random");
			System.exit(1);
		}
	}
}