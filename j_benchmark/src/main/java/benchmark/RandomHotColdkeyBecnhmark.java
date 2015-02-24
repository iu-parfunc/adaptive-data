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

	private ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>> outerConcSkipListMap = new ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>>();
	private ConcurrentHashMap<Integer, ConcurrentHashMap<Integer, Integer>> outerConcHashMap = new ConcurrentHashMap<Integer, ConcurrentHashMap<Integer, Integer>>();
	private HybridIntMap<HybridIntMap<Integer>> outerHybridIntMapInnrMap;
	private PureIntMap<PureIntMap<Integer>> outerPureIntTreeMap;

	private HashMap<String, TreeMap<Integer, ArrayList<Long>>> performanceData = new HashMap<String, TreeMap<Integer, ArrayList<Long>>>();

	private Thread[] threads = new Thread[64];

	public RandomHotColdkeyBecnhmark(String dsTypeToBeBenchmarked,
			int numInsertions, int numHotKey, int runRepetitions,
			int maxNumThreads, double hotRatio, long runStartTimestamp)
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

		warmUp(cuncorrencyType, numInsertions, numHotKey, runRepetitions,
				maxNumThreads, hotRatio);
		runBenchmark(cuncorrencyType, valueType, numInsertions, numHotKey,
				runRepetitions, maxNumThreads, hotRatio);
		Util.writePerfData(performanceData, "random", dsTypeToBeBenchmarked,
				numInsertions, numHotKey, hotRatio, runStartTimestamp);
	}

	private void runBenchmark(String cuncorrencyType, String valueType,
			int numInsertions, int numHotKey, int runRepetitions,
			int maxNumThreadsmaxNumThreadss, double hotRatio)
			throws InterruptedException, IOException {

		randomBenchmark(cuncorrencyType, runRepetitions, numInsertions,
				numHotKey, maxNumThreadsmaxNumThreadss, hotRatio, false);
	}

	private void warmUp(String cuncorrencyType, int numInsertions,
			int numHotKey, int runRepetitions, int maxNumThreadsmaxNumThreadss,
			double hotRatio) throws InterruptedException, IOException {

		randomBenchmark(cuncorrencyType,
				((runRepetitions >= 10) ? runRepetitions / 10 : 1),
				numInsertions, numHotKey, maxNumThreadsmaxNumThreadss,
				hotRatio, true);
	}

	private void randomBenchmark(String concurrencyType, int runRepetitions,
			int insertionCount, int numHotKey, int maxNumThreadsmaxNumThreadss,
			double hotRatio, boolean warmUp) throws InterruptedException,
			IOException {

		CountDownLatch startSignal, doneSignal;
		long startTime, endTime, elapsed;

		for (int numThreads = 1; numThreads <= maxNumThreadsmaxNumThreadss; numThreads++) {

			int numInsretionsPerThread = insertionCount / numThreads;

			ArrayList<Long> timeTakenForRounds = new ArrayList<Long>();

			for (int i = 1; i <= runRepetitions; i++) {

				initiatlizeMap(concurrencyType);
				startTime = System.currentTimeMillis();
				startSignal = new CountDownLatch(1);
				doneSignal = new CountDownLatch(numThreads);
				for (int j = 0; j < numThreads; j++) {
					switch (concurrencyType) {
					case Util.SKIP_LIST_MAP:
						threads[j] = new InsertionThread(outerConcSkipListMap,
								j * numInsretionsPerThread, (j + 1)
										* numInsretionsPerThread,
								insertionCount, numHotKey, hotRatio,
								startSignal, doneSignal);
						threads[j].start();
						break;
					case Util.CONCURRENT_MAP:
						threads[j] = new InsertionThread(outerConcHashMap, j
								* numInsretionsPerThread, (j + 1)
								* numInsretionsPerThread, insertionCount,
								numHotKey, hotRatio, startSignal, doneSignal);
						threads[j].start();
						break;
					case Util.HYBRID_MAP:
						threads[j] = new InsertionThread(
								outerHybridIntMapInnrMap, j
										* numInsretionsPerThread, (j + 1)
										* numInsretionsPerThread,
								insertionCount, numHotKey, hotRatio,
								startSignal, doneSignal);
						threads[j].start();
						break;

					case Util.PURE_MAP:
						threads[j] = new InsertionThread(outerPureIntTreeMap, j
								* numInsretionsPerThread, (j + 1)
								* numInsretionsPerThread, insertionCount,
								numHotKey, hotRatio, startSignal, doneSignal);
						threads[j].start();
						break;
					}
				}
				startSignal.countDown();
				doneSignal.await();
				endTime = System.currentTimeMillis();
				elapsed = (endTime - startTime);
				if (!warmUp) {
					timeTakenForRounds.add(new Long(elapsed));
				}
			}// End of FOR loop over run repetitions

			if (!warmUp) {

				Util.recordTimeTaken(performanceData, timeTakenForRounds,
						concurrencyType, numThreads);
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

	public static void main(String[] args) {

		try {
			int i = 0;
			String dsType = args[i++];
			int numInsertions = Integer.parseInt(args[i++]);
			int numHotKey = Integer.parseInt(args[i++]);
			int runRepetitions = Integer.parseInt(args[i++]);
			int maxNumThreadsmaxNumThreadss = Integer.parseInt(args[i++]);
			double hotRatio = Double.parseDouble(args[i++]);
			long runStartTimestamp = Long.parseLong(args[i++]);
			new RandomHotColdkeyBecnhmark(dsType, numInsertions, numHotKey,
					runRepetitions, maxNumThreadsmaxNumThreadss, hotRatio,
					runStartTimestamp);

		} catch (InterruptedException | IOException e) {
			e.printStackTrace();
			System.exit(1);
		} catch (NumberFormatException | ArrayIndexOutOfBoundsException e) {
			Util.inputFormatError("random");
			System.exit(1);
		}
	}
}