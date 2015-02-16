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
	private AtomicReference<IntTreePMap<?>> mutableIntTreeMap;
	private boolean append = true;

	public SimpleInsertionBenchmark(String dsTypeToBeBenchmarked,
			int numofInsertions, int runRepetitions, int maxNumberOfThreads)
			throws InterruptedException, IOException {

		String outputFileName = "simple_insertion.csv";
		// String outputFileName =
		// String.format("simple_insertion_%s_%d_%d.csv",
		// dsTypeToBeBenchmarked, numofInsertions, maxNumberOfThreads);

		if (!new File(outputFileName).exists()) {
			append = false;
		}
		writer = new BufferedWriter(new FileWriter(new File(outputFileName),
				true));

		String cuncorrencyType = null, valueType = null;
		switch (dsTypeToBeBenchmarked) {
		case "pure":
			cuncorrencyType = Util.MUTABLE_INT_TREE_MAP;
			valueType = Util.INT_TO_MUTABLE_INT_TREE_MAP;
			break;
		case "scalable":
			cuncorrencyType = Util.SKIP_LIST_MAP;
			valueType = Util.INT_TO_SYNCH_HASHMAP_INT_TO_INT;
			break;
		case "hybrid":
			cuncorrencyType = Util.HYBRID_MAP;
			valueType = Util.INT_TO_SYNCH_HASHMAP_INT_TO_INT;
			System.out.print("TBD");
			System.exit(0);
			break;
		case "JavaSynch":
			cuncorrencyType = Util.SYNCHRONIZED_MAP;
			valueType = Util.INT_TO_SYNCH_HASHMAP_INT_TO_INT;
			break;
		case "JavaConc":
			cuncorrencyType = Util.CONCURRENT_MAP;
			valueType = Util.INT_TO_SYNCH_HASHMAP_INT_TO_INT;
			break;
		default:
			System.out
					.println("Please insert one of the following data structures be benchmarked\n"
							+ "pure|scalable|hybrid|JavaSynch|JavaConc");
			System.exit(0);
			break;
		}

		warmUp(cuncorrencyType, valueType, numofInsertions, runRepetitions,
				maxNumberOfThreads);
		runBenchmark(cuncorrencyType, valueType, numofInsertions,
				runRepetitions, maxNumberOfThreads);

		writer.close();
	}

	private void runBenchmark(String cuncorrencyType, String valueType,
			int numofInsertions, int runRepetitions, int maxNumberOfThreads)
			throws InterruptedException, IOException {
		benchmark(cuncorrencyType, valueType, runRepetitions, numofInsertions,
				maxNumberOfThreads);
		writePerfData(numofInsertions, runRepetitions);
	}

	private void benchmark(String concurrencyType, String mapValueType,
			int runRepetitions, int numofInsertions, int maxNumberOfThreads)
			throws InterruptedException, IOException {

		initiatlizeMap(concurrencyType, mapValueType);
		String mapConfig = concurrencyType + "_" + mapValueType;
		performanceData.put(mapConfig, new TreeMap<Integer, Integer>());

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
					mutableIntTreeMap = new AtomicReference(IntTreePMap.empty());
					for (int j = 0; j < numOfThreads; j++) {
						threads[j] = new Inserter(mutableIntTreeMap, j
								* numOfInsretionsPerThread, (j + 1)
								* numOfInsretionsPerThread, mapValueType,
								startSignal, doneSignal);
						threads[j].start();
					}
					break;
				default:
					threadSafeMap.clear();
					for (int j = 0; j < numOfThreads; j++) {
						threads[j] = new Inserter(threadSafeMap, j
								* numOfInsretionsPerThread, (j + 1)
								* numOfInsretionsPerThread, mapValueType,
								startSignal, doneSignal);
						threads[j].start();
					}
					break;
				}
				startSignal.countDown();
				doneSignal.await();
			}
			// System.out.println(numOfThreads + " *** >>> " +
			// mutableIntTreeMap);
			endTime = System.currentTimeMillis();
			elapsed = (endTime - startTime);
			performanceData.get(mapConfig).put(new Integer(numOfThreads),
					new Integer((int) (elapsed / runRepetitions)));
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
			default:
				break;
			}
			break;
		case Util.INT_TO_SYNCH_HASHMAP_INT_TO_INT:
			switch (ConcurrecyType) {
			case Util.SYNCHRONIZED_MAP:
				threadSafeMap = Collections
						.synchronizedMap(new HashMap<Integer, Map<String, String>>());
				break;
			case Util.CONCURRENT_MAP:
				threadSafeMap = new ConcurrentHashMap<Integer, Map<String, String>>();
				break;
			case Util.SKIP_LIST_MAP:
				threadSafeMap = new ConcurrentSkipListMap<Integer, Map<String, String>>();
				break;
			default:
				break;
			}
			break;
		default:
			break;
		}
	}

	private void writePerfData(int numofInsertions, int runRepetitions)
			throws IOException {

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
				Util.writeLine(writer, "RANDOM_INSERTION,JAVA,-" + mapType
						+ " -#inserts " + numofInsertions + " -#threads "
						+ numerOfThreads + " ," + timeTaken);
			}
		}
	}

	private void warmUp(String cuncorrencyType, String valueType,
			int numofInsertions, int runRepetitions, int maxNumberOfThreads)
			throws InterruptedException, IOException {

		benchmark(cuncorrencyType, valueType,
				((runRepetitions >= 10) ? runRepetitions / 10 : 1),
				numofInsertions, maxNumberOfThreads);
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
