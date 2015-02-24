package benchmark;

import hybrid_ds.HybridIntMap;
import hybrid_ds.PureIntMap;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.concurrent.CountDownLatch;

public class SimpleInsertionBenchmark {

	private Map<Integer, Integer> mapInt;
	private Map<Integer, Map<Integer, Integer>> mapInnerMap;
	private PureIntMap<Integer> pureIntMapInt;
	private PureIntMap<PureIntMap<Integer>> pureIntMapInnerMap;
	private HybridIntMap<Integer> hybridIntMapInt;
	private HybridIntMap<HybridIntMap<Integer>> hybridIntMapInnerMap;

	private HashMap<String, TreeMap<Integer, ArrayList<Long>>> performanceData = new HashMap<String, TreeMap<Integer, ArrayList<Long>>>();

	public SimpleInsertionBenchmark(String dsTypeToBeBenchmarked,
			int numInsertions, int runRepetitions,
			int maxNumThreadsmaxNumThreadss, long runStartTimestamp,
			int casTries) throws InterruptedException, IOException {

		String cuncorrencyType = null;
		switch (dsTypeToBeBenchmarked) {
		case "pure":
			cuncorrencyType = Util.PURE_MAP;
			break;
		case "scalable":
			cuncorrencyType = Util.SKIP_LIST_MAP;
			break;
		case "hybrid":
			cuncorrencyType = Util.HYBRID_MAP;
			break;
		case "JavaSynch":
			cuncorrencyType = Util.SYNCHRONIZED_MAP;
			break;
		case "JavaConc":
			cuncorrencyType = Util.CONCURRENT_MAP;
			break;
		default:
			Util.inputFormatError("simple");
			System.exit(0);
			break;
		}

		warmUp(cuncorrencyType, numInsertions, runRepetitions,
				maxNumThreadsmaxNumThreadss, casTries);
		runBenchmark(cuncorrencyType, numInsertions, runRepetitions,
				maxNumThreadsmaxNumThreadss, casTries);
		Util.writePerfData(performanceData, "simple", dsTypeToBeBenchmarked,
				numInsertions, 0, 0, runStartTimestamp, casTries);
	}

	private void runBenchmark(String cuncorrencyType, int numInsertions,
			int runRepetitions, int maxNumThreadsmaxNumThreadss, int casTries)
			throws InterruptedException, IOException {

		String valueType = Util.INT_TO_INT;
		benchmark(cuncorrencyType, valueType, runRepetitions, numInsertions,
				maxNumThreadsmaxNumThreadss, false, casTries);
		// valueType = Util.INT_TO_INNER_MAP;
		// benchmark(cuncorrencyType, valueType, runRepetitions,
		// numInsertions,
		// maxNumThreadsmaxNumThreadss, false);
	}

	private void warmUp(String cuncorrencyType, int numInsertions,
			int runRepetitions, int maxNumThreadsmaxNumThreadss, int casTries)
			throws InterruptedException, IOException {

		String valueType = Util.INT_TO_INT;
		benchmark(cuncorrencyType, valueType,
				((runRepetitions >= 10) ? runRepetitions / 10 : 1),
				numInsertions, maxNumThreadsmaxNumThreadss, true, casTries);
		valueType = Util.INT_TO_INNER_MAP;
		benchmark(cuncorrencyType, valueType,
				((runRepetitions >= 10) ? runRepetitions / 10 : 1),
				numInsertions, maxNumThreadsmaxNumThreadss, true, casTries);
	}

	private void benchmark(String concurrencyType, String mapValueType,
			int runRepetitions, int numInsertions,
			int maxNumThreadsmaxNumThreadss, boolean warmUp, int casTries)
			throws InterruptedException, IOException {

		CountDownLatch startSignal, doneSignal;

		long startTime, endTime, elapsed = 0;
		Thread[] threads = new Thread[64];

		for (int numThreads = 1; numThreads <= maxNumThreadsmaxNumThreadss; numThreads++) {

			int numInsretionsPerThread = numInsertions / numThreads;

			ArrayList<Long> timeTakenForRounds = new ArrayList<Long>();

			for (int i = 1; i <= runRepetitions; i++) {

				initiatlizeMap(concurrencyType, mapValueType, casTries);

				startTime = System.currentTimeMillis();
				startSignal = new CountDownLatch(1);
				doneSignal = new CountDownLatch(numThreads);

				switch (concurrencyType) {
				case Util.PURE_MAP:
					for (int j = 0; j < numThreads; j++) {
						threads[j] = new InsertionThread(pureIntMapInt,
								pureIntMapInnerMap, j * numInsretionsPerThread,
								(j + 1) * numInsretionsPerThread, mapValueType,
								startSignal, doneSignal);
						threads[j].start();
					}
					break;
				case Util.HYBRID_MAP:

					for (int j = 0; j < numThreads; j++) {
						threads[j] = new InsertionThread(hybridIntMapInt,
								hybridIntMapInnerMap, j
										* numInsretionsPerThread, (j + 1)
										* numInsretionsPerThread, mapValueType,
								casTries, startSignal, doneSignal);
						threads[j].start();
					}
					break;
				default:
					for (int j = 0; j < numThreads; j++) {
						threads[j] = new InsertionThread(mapInt, mapInnerMap, j
								* numInsretionsPerThread, (j + 1)
								* numInsretionsPerThread, concurrencyType,
								mapValueType, startSignal, doneSignal);
						threads[j].start();
					}
					break;
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
						mapValueType, numThreads);
			}
		}// End of FOR loop over numberOfThreads
	}

	private void initiatlizeMap(String cuncorrencyType, String mapValueType,
			int casTries) {

		switch (mapValueType) {
		case Util.INT_TO_INT:
			switch (cuncorrencyType) {
			case Util.SYNCHRONIZED_MAP:
				mapInt = Collections
						.synchronizedMap(new HashMap<Integer, Integer>());
				break;
			case Util.CONCURRENT_MAP:
				mapInt = new ConcurrentHashMap<Integer, Integer>();
				break;
			case Util.SKIP_LIST_MAP:
				mapInt = new ConcurrentSkipListMap<Integer, Integer>();
				break;
			case Util.HYBRID_MAP:
				hybridIntMapInt = new HybridIntMap<Integer>(casTries);
				break;
			case Util.PURE_MAP:
				pureIntMapInt = new PureIntMap<Integer>();
				break;
			}
			break;
		case Util.INT_TO_INNER_MAP:
			switch (cuncorrencyType) {
			case Util.SYNCHRONIZED_MAP:
				mapInnerMap = Collections
						.synchronizedMap(new HashMap<Integer, Map<Integer, Integer>>());
				break;
			case Util.CONCURRENT_MAP:
				mapInnerMap = new ConcurrentHashMap<Integer, Map<Integer, Integer>>();
				break;
			case Util.SKIP_LIST_MAP:
				mapInnerMap = new ConcurrentSkipListMap<Integer, Map<Integer, Integer>>();
				break;
			case Util.HYBRID_MAP:
				hybridIntMapInnerMap = new HybridIntMap<HybridIntMap<Integer>>(
						casTries);
				break;
			case Util.PURE_MAP:
				pureIntMapInnerMap = new PureIntMap<PureIntMap<Integer>>();
				break;
			}
			break;
		}
	}

	public static void main(String[] args) {

		try {
			int i = 0;
			String dsType = args[i++];
			int numInsertions = Integer.parseInt(args[i++]);
			int runRepetitions = Integer.parseInt(args[i++]);
			int maxNumThreadsmaxNumThreadss = Integer.parseInt(args[i++]);
			int casTries = Integer.parseInt(args[i++]);
			long runStartTimestamp = Long.parseLong(args[i++]);
			new SimpleInsertionBenchmark(dsType, numInsertions, runRepetitions,
					maxNumThreadsmaxNumThreadss, runStartTimestamp, casTries);
		} catch (InterruptedException | IOException e) {
			e.printStackTrace();
			System.exit(1);
		} catch (NumberFormatException | ArrayIndexOutOfBoundsException e) {
			Util.inputFormatError("simple");
			System.exit(1);
		}
	}
}