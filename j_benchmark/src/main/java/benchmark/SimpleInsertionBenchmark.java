package benchmark;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
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
	private BufferedWriter writer;
	private HashMap<String, TreeMap<Integer, Integer>> performanceData = new HashMap<String, TreeMap<Integer, Integer>>();
	private AtomicReference<IntTreePMap<Integer>> mutableIntTreeMapInt;
	private AtomicReference<IntTreePMap<AtomicReference<IntTreePMap<Integer>>>> mutableIntTreeMapInnerMap;
	private boolean append = true;

	public SimpleInsertionBenchmark(String dsTypeToBeBenchmarked,
			int numofInsertions, int runRepetitions, int maxNumberOfThreads)
			throws InterruptedException, IOException {

		String outputFileName = "simple_insertion.csv";

		if (!new File(outputFileName).exists()) {
			append = false;
		}
		writer = new BufferedWriter(new FileWriter(new File(outputFileName),
				true));

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
		case "JavaSynch":
			cuncorrencyType = Util.SYNCHRONIZED_MAP;
			break;
		case "JavaConc":
			cuncorrencyType = Util.CONCURRENT_MAP;
			break;
		default:
			System.out
					.println("Please insert one of the following data structures be benchmarked\n"
							+ "pure|scalable|hybrid|JavaSynch|JavaConc");
			System.exit(0);
			break;
		}

		valueType = Util.INT_TO_INT;
		warmUp(cuncorrencyType, valueType, numofInsertions, runRepetitions,
				maxNumberOfThreads);
		runBenchmark(cuncorrencyType, valueType, numofInsertions,
				runRepetitions, maxNumberOfThreads);
		valueType = Util.INT_TO_INNER_MAP;
		warmUp(cuncorrencyType, valueType, numofInsertions, runRepetitions,
				maxNumberOfThreads);
		runBenchmark(cuncorrencyType, valueType, numofInsertions,
				runRepetitions, maxNumberOfThreads);
		writePerfData(dsTypeToBeBenchmarked, numofInsertions, runRepetitions);
		writer.close();
	}

	private void runBenchmark(String cuncorrencyType, String valueType,
			int numofInsertions, int runRepetitions, int maxNumberOfThreads)
			throws InterruptedException, IOException {
		benchmark(cuncorrencyType, valueType, runRepetitions, numofInsertions,
				maxNumberOfThreads, false);
	}

	private void benchmark(String concurrencyType, String mapValueType,
			int runRepetitions, int numofInsertions, int maxNumberOfThreads,
			boolean warmUp) throws InterruptedException, IOException {

		initiatlizeMap(concurrencyType, mapValueType);
		if (!warmUp) {
			performanceData.put(mapValueType, new TreeMap<Integer, Integer>());
		}

		CountDownLatch startSignal, doneSignal;

		long startTime, endTime, elapsed = 0;
		int numOfInsretionsPerThread;
		Thread[] threads = new Thread[64];

		for (int numOfThreads = 1; numOfThreads <= maxNumberOfThreads; numOfThreads *= 2) {

			numOfInsretionsPerThread = numofInsertions / numOfThreads;

			startTime = System.currentTimeMillis();
			for (int i = 1; i <= runRepetitions; i++) {

				startSignal = new CountDownLatch(1);
				doneSignal = new CountDownLatch(numOfThreads);

				switch (concurrencyType) {
				case Util.MUTABLE_INT_TREE_MAP:
					mutableIntTreeMapInt = new AtomicReference(
							IntTreePMap.empty());
					mutableIntTreeMapInnerMap = new AtomicReference(
							IntTreePMap.empty());
					for (int j = 0; j < numOfThreads; j++) {
						threads[j] = new Inserter(mutableIntTreeMapInt,
								mutableIntTreeMapInnerMap, j
										* numOfInsretionsPerThread, (j + 1)
										* numOfInsretionsPerThread,
								mapValueType, startSignal, doneSignal);
						threads[j].start();
					}
					break;
				default:
					threadSafeMap.clear();
					for (int j = 0; j < numOfThreads; j++) {
						threads[j] = new Inserter(threadSafeMap, j
								* numOfInsretionsPerThread, (j + 1)
								* numOfInsretionsPerThread, concurrencyType,
								mapValueType, startSignal, doneSignal);
						threads[j].start();
					}
					break;
				}
				startSignal.countDown();
				doneSignal.await();
			}
			// System.out.println(numOfThreads + " *** >>> " +
			// mutableIntTreeMap);
			// System.out.println(mutableIntTreeMap.get().size());
			// System.out.println(threadSafeMap.size());
			endTime = System.currentTimeMillis();
			elapsed = (endTime - startTime);
			if (!warmUp) {
				performanceData.get(mapValueType).put(
						new Integer(numOfThreads),
						new Integer((int) (elapsed / runRepetitions)));
			}
		}
	}

	private void initiatlizeMap(String ConcurrecyType, String mapValyeType) {

		switch (mapValyeType) {
		case Util.INT_TO_INT:
			switch (ConcurrecyType) {
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
			case Util.MUTABLE_INT_TREE_MAP:
				break;
			default:
				break;
			}
			break;
		case Util.INT_TO_INNER_MAP:
			switch (ConcurrecyType) {
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
			case Util.MUTABLE_INT_TREE_MAP:
				break;
			default:
				break;
			}
			break;
		default:
			break;
		}
	}

	private void writePerfData(String dsTypeToBeBenchmarked,
			int numofInsertions, int runRepetitions) throws IOException {

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
						+ dsTypeToBeBenchmarked + "_" + mapType + " -#inserts "
						+ numofInsertions + " -#threads " + numerOfThreads
						+ " ," + timeTaken);
			}
		}
	}

	private void warmUp(String cuncorrencyType, String valueType,
			int numofInsertions, int runRepetitions, int maxNumberOfThreads)
			throws InterruptedException, IOException {

		benchmark(cuncorrencyType, valueType,
				((runRepetitions >= 10) ? runRepetitions / 10 : 1),
				numofInsertions, maxNumberOfThreads, true);
	}

	public static void main(String[] args) {

		try {
			String dsTypeToBeBenchmarked = args[0];
			int numofInsertions = Integer.parseInt(args[1]);
			int runRepetitions = Integer.parseInt(args[2]);
			int maxNumberOfThreads = Integer.parseInt(args[3]);
			new SimpleInsertionBenchmark(dsTypeToBeBenchmarked,
					numofInsertions, runRepetitions, maxNumberOfThreads);
		} catch (InterruptedException | IOException e) {
			e.printStackTrace();
		} catch (NumberFormatException | ArrayIndexOutOfBoundsException e) {
			System.out
					.println("Please enter the folowing input data:\n"
							+ " 1-Data structure to bebenchmarked (pure|scalable|hybrid|JavaSynch|JavaConc)\n"
							+ " 2-Number of insertions\n"
							+ " 3-Number of run repetitions\n"
							+ " 4-Maximum number of threads\n"
							+ " Output will be put in "
							+ "\"simple_insertion_<DS Type>_<Number of insertions>_<Maximum number of threads>.csv\"");
		}
	}
}
