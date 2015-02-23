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

public class SimpleInsertionBenchmark {

	@SuppressWarnings("rawtypes")
	private Map threadSafeMap;
	private PureIntMap<Integer> pureIntMapInt;
	private PureIntMap<PureIntMap<Integer>> pureIntMapInnerMap;
	private HybridIntMap<Integer> hybridIntMapInt;
	private HybridIntMap<HybridIntMap<Integer>> hybridIntMapInnerMap;

	private HashMap<String, TreeMap<Integer, ArrayList<Long>>> performanceData = new HashMap<String, TreeMap<Integer, ArrayList<Long>>>();

	public SimpleInsertionBenchmark(String dsTypeToBeBenchmarked,
			int numofInsertions, int runRepetitions, int maxNumberOfThreads)
			throws InterruptedException, IOException {

		String cuncorrencyType = null, valueType = null;
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

		warmUp(cuncorrencyType, numofInsertions, runRepetitions,
				maxNumberOfThreads);
		runBenchmark(cuncorrencyType, numofInsertions, runRepetitions,
				maxNumberOfThreads);
		Util.writePerfData(performanceData, "simple", dsTypeToBeBenchmarked,
				numofInsertions, 0, 0);
	}

	private void runBenchmark(String cuncorrencyType, int numofInsertions,
			int runRepetitions, int maxNumberOfThreads)
			throws InterruptedException, IOException {

		String valueType = Util.INT_TO_INNER_MAP;
		benchmark(cuncorrencyType, valueType, runRepetitions, numofInsertions,
				maxNumberOfThreads, false);
		// valueType = Util.INT_TO_INT;
		// benchmark(cuncorrencyType, valueType, runRepetitions,
		// numofInsertions,
		// maxNumberOfThreads, false);
	}

	private void benchmark(String concurrencyType, String mapValueType,
			int runRepetitions, int numofInsertions, int maxNumberOfThreads,
			boolean warmUp) throws InterruptedException, IOException {

		CountDownLatch startSignal, doneSignal;

		long startTime, endTime, elapsed = 0;
		Thread[] threads = new Thread[64];

		for (int numOfThreads = 1; numOfThreads <= maxNumberOfThreads; numOfThreads *= 2) {

			// System.out.println(" *** " + numOfThreads + " " + mapValueType
			// + " *** ");
			int numOfInsretionsPerThread = numofInsertions / numOfThreads;

			ArrayList<Long> runTimeRecods = new ArrayList<Long>();

			for (int i = 1; i <= runRepetitions; i++) {

				initiatlizeMap(concurrencyType, mapValueType);
				
				startTime = System.currentTimeMillis();
				startSignal = new CountDownLatch(1);
				doneSignal = new CountDownLatch(numOfThreads);

				switch (concurrencyType) {
				case Util.PURE_MAP:
					for (int j = 0; j < numOfThreads; j++) {
						threads[j] = new InsertionThread(pureIntMapInt,
								pureIntMapInnerMap, j
										* numOfInsretionsPerThread, (j + 1)
										* numOfInsretionsPerThread,
								mapValueType, startSignal, doneSignal);
						threads[j].start();
					}
					break;
				case Util.HYBRID_MAP:

					for (int j = 0; j < numOfThreads; j++) {
						threads[j] = new InsertionThread(hybridIntMapInt,
								hybridIntMapInnerMap, j
										* numOfInsretionsPerThread, (j + 1)
										* numOfInsretionsPerThread,
								mapValueType, startSignal, doneSignal);
						threads[j].start();
					}
					break;
				default:
					for (int j = 0; j < numOfThreads; j++) {
						threads[j] = new InsertionThread(threadSafeMap, j
								* numOfInsretionsPerThread, (j + 1)
								* numOfInsretionsPerThread, concurrencyType,
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
					runTimeRecods.add(new Long(elapsed));
				}

			}// End of FOR loop over run repetitions

			if (!warmUp) {

				String mapConfig = mapValueType;
				performanceData.putIfAbsent(mapConfig,
						new TreeMap<Integer, ArrayList<Long>>());
				performanceData.get(mapConfig).put(new Integer(numOfThreads),
						runTimeRecods);
			}
		}// End of FOR loop over numberOfThreads
	}

	private void initiatlizeMap(String cuncorrencyType, String mapValueType) {

		switch (mapValueType) {
		case Util.INT_TO_INT:
			switch (cuncorrencyType) {
			case Util.SYNCHRONIZED_MAP:
				threadSafeMap = Collections
						.synchronizedMap(new HashMap<Integer, Integer>());
				break;
			case Util.CONCURRENT_MAP:
				threadSafeMap = new ConcurrentHashMap<Integer, Integer>();
				break;
			case Util.SKIP_LIST_MAP:
				threadSafeMap = new ConcurrentSkipListMap<Integer, Integer>();
				break;
			case Util.HYBRID_MAP:
				hybridIntMapInt = new HybridIntMap<Integer>();
				break;
			case Util.PURE_MAP:
				pureIntMapInt = new PureIntMap<Integer>();
				break;
			}
			break;
		case Util.INT_TO_INNER_MAP:
			switch (cuncorrencyType) {
			case Util.SYNCHRONIZED_MAP:
				threadSafeMap = Collections
						.synchronizedMap(new HashMap<Integer, Map<Integer, Integer>>());
				break;
			case Util.CONCURRENT_MAP:
				threadSafeMap = new ConcurrentHashMap<Integer, Map<Integer, Integer>>();
				break;
			case Util.SKIP_LIST_MAP:
				threadSafeMap = new ConcurrentSkipListMap<Integer, Map<Integer, Integer>>();
				break;
			case Util.HYBRID_MAP:
				hybridIntMapInnerMap = new HybridIntMap<HybridIntMap<Integer>>();
				break;
			case Util.PURE_MAP:
				pureIntMapInnerMap = new PureIntMap<PureIntMap<Integer>>();
				break;
			}
			break;
		}
	}

	private void warmUp(String cuncorrencyType, int numofInsertions,
			int runRepetitions, int maxNumberOfThreads)
			throws InterruptedException, IOException {

		String valueType = Util.INT_TO_INT;
		benchmark(cuncorrencyType, valueType,
				((runRepetitions >= 10) ? runRepetitions / 10 : 1),
				numofInsertions, maxNumberOfThreads, true);
		valueType = Util.INT_TO_INNER_MAP;
		benchmark(cuncorrencyType, valueType,
				((runRepetitions >= 10) ? runRepetitions / 10 : 1),
				numofInsertions, maxNumberOfThreads, true);
	}

	public static void main(String[] args) {

		try {
			String dsType = args[0];
			int numofInsertions = Integer.parseInt(args[1]);
			int runRepetitions = Integer.parseInt(args[2]);
			int maxNumberOfThreads = Integer.parseInt(args[3]);
			new SimpleInsertionBenchmark(dsType, numofInsertions,
					runRepetitions, maxNumberOfThreads);
		} catch (InterruptedException | IOException e) {
			e.printStackTrace();
		} catch (NumberFormatException | ArrayIndexOutOfBoundsException e) {
			Util.inputFormatError("simple");
		}
	}
}