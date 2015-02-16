package benchmark;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.TreeMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicReference;

import org.pcollections.IntTreePMap;

public class MutablePersistentIntTreeMap {

	private AtomicReference<IntTreePMap<?>> mutableIntTreeMap;
	private BufferedWriter writer;
	private HashMap<String, TreeMap<Integer, Integer>> performanceData = new HashMap<String, TreeMap<Integer, Integer>>();

	public MutablePersistentIntTreeMap(int numofInsertions, int runRepetitions,
			int maxNumberOfThreads) throws IOException, InterruptedException {

		writer = new BufferedWriter(new FileWriter(new File(
				"simple_insertion_benchmark_" + numofInsertions + "_"
						+ maxNumberOfThreads + ".csv")));
		Util.writeLine(writer, "PROGNAME,VARIANT,ARGS,AVERAGE_TIME");

		boolean warmUp = true;
		/* Warm-up starts here */
		benchmark(Util.SYNCHRONIZED_MAP, Util.INT_TO_INT, 10, 10000, 64, warmUp);
		benchmark(Util.SYNCHRONIZED_MAP, Util.INT_TO_MUTABLE_INT_TREE_MAP, 10,
				10000, 64, warmUp);

		/* Warm-up ends here */

		warmUp = false;
		benchmark(Util.SYNCHRONIZED_MAP, Util.INT_TO_INT, runRepetitions,
				numofInsertions, maxNumberOfThreads, warmUp);
		benchmark(Util.SYNCHRONIZED_MAP, Util.INT_TO_MUTABLE_INT_TREE_MAP,
				runRepetitions, numofInsertions, maxNumberOfThreads, warmUp);

		writer.close();
	}

	private void benchmark(String concurrencyType, String mapValueType,
			int runRepetitions, int numofInsertions, int maxNumberOfThreads,
			boolean warmUp) throws InterruptedException, IOException {

		String mapConfig = concurrencyType + "_" + mapValueType;
		performanceData.put(mapConfig, new TreeMap<Integer, Integer>());

		CountDownLatch startSignal, doneSignal;

		long startTime, endTime, elapsed = 0;
		int numOfInsretionsPerThread;
		Thread[] threads = new Thread[64];

		for (int numOfThreads = 4; numOfThreads <= maxNumberOfThreads; numOfThreads *= 2) {

			numOfInsretionsPerThread = numofInsertions / numOfThreads;

			startTime = System.currentTimeMillis();
			for (int i = 0; i < runRepetitions; i++) {


				mutableIntTreeMap = new AtomicReference(IntTreePMap.empty());

				startSignal = new CountDownLatch(1);
				doneSignal = new CountDownLatch(numOfThreads);

				for (int j = 0; j < numOfThreads; j++) {
					threads[j] = new Inserter(mutableIntTreeMap, j
							* numOfInsretionsPerThread, (j + 1)
							* numOfInsretionsPerThread, mapValueType,
							startSignal, doneSignal);
					threads[j].start();
				}
				startSignal.countDown();
				doneSignal.await();
				System.out.println(mutableIntTreeMap);
			}
			endTime = System.currentTimeMillis();
			elapsed = (endTime - startTime);
			performanceData.get(mapConfig).put(new Integer(numOfThreads),
					new Integer((int) (elapsed / runRepetitions)));
		}
		if (!warmUp) {
			writePerfData(numofInsertions, runRepetitions);
		}
	}

	private void writePerfData(int numofInsertions, int runRepetitions)
			throws IOException {

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
						+ " -inserts " + numofInsertions + " -threads "
						+ numerOfThreads + " ," + timeTaken);
			}
		}
	}

	public static void main(String[] args) {
		try {
			new MutablePersistentIntTreeMap(Integer.parseInt(args[0]),
					Integer.parseInt(args[1]), Integer.parseInt(args[2]));
		} catch (Exception e) {
			System.out
					.println("Please enter the folowing input data:\n"
							+ " 1-Number of insertions\n"
							+ " 2-Number of run repetitions\n"
							+ " 3-Maximum number of threads\n"
							+ " Output will be put in "
							+ "\"simple_insertion_benchmark_<Number of insertions>_<Maximum number of threads>.csv\"");
		}
	}
}