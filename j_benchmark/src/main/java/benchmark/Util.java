package benchmark;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Random;
import java.util.TreeMap;

public class Util {

	public static final String SYNCHRONIZED_MAP = "SYNCHRONIZED_HASH_MAP";
	public static final String CONCURRENT_MAP = "CONCURRENT_HASH_MAP";
	public static final String SKIP_LIST_MAP = "SKIP_LIST_MAP";
	public static final String HYBRID_MAP = "HYBRID";
	public static final String MUTABLE_INT_TREE_MAP = "MUTABLE_INT_TREE_MAP";
	public static final String PURE_MAP = "PURE_MAP";

	public static final String INT_TO_INT = "<Int,Int>";
	public static final String INT_TO_INNER_MAP = "<Int,InnerMAP>";

	public static final String RANDOM_HOT_COLD = "RANDOM";
	public static final String SIMPLE_INSERTION = "SIMPLE";

	public static void writeLine(BufferedWriter writer, String line)
			throws IOException {

		writer.write(line);
		writer.newLine();
	}

	public static Integer nextHotKey(Random hotKeyGen, int coldKeyRangeMax,
			int numHotKey) {

		coldKeyRangeMax *= 10;
		return new Integer(hotKeyGen.nextInt(numHotKey) + coldKeyRangeMax);
	}

	public static void writePerfData(
			HashMap<String, TreeMap<Integer, ArrayList<Long>>> performanceData,
			String benchMarkType, String dsType, int numInsertions,
			double numHotKey, double hotRatio) throws IOException {

		BufferedWriter writer = initiaLizeOutputFile(benchMarkType);

		Integer numerThreads;
		ArrayList<Long> timeTaken;
		long medianTime, maxTime, minTime;
		TreeMap<Integer, ArrayList<Long>> perfDataPerMapType;
		Iterator<Integer> numberOfThreadsITR;
		Iterator<String> mapTypeITR = performanceData.keySet().iterator();

		String mapType = null;
		while (mapTypeITR.hasNext()) {
			mapType = mapTypeITR.next();
			perfDataPerMapType = performanceData.get(mapType);
			numberOfThreadsITR = perfDataPerMapType.keySet().iterator();
			while (numberOfThreadsITR.hasNext()) {
				numerThreads = numberOfThreadsITR.next();
				timeTaken = perfDataPerMapType.get(numerThreads);
				Collections.sort(timeTaken);
				minTime = timeTaken.get(0);
				maxTime = timeTaken.get(timeTaken.size() - 1);
				medianTime = ((timeTaken.size() % 2 == 1) ? timeTaken
						.get((timeTaken.size() / 2)) : (timeTaken
						.get((timeTaken.size() / 2) - 1) + timeTaken
						.get((timeTaken.size() / 2))) / 2);
				String outDataLine = null;
				switch (benchMarkType) {
				case "simple":
					outDataLine = "simple_insertion_map_int_int," + dsType
							+ "-java," + /* ARGS is empty */"," + numerThreads
							+ "," + numInsertions + "," + minTime + ","
							+ medianTime + "," + maxTime;
					break;
				case "random":
					outDataLine = "hotcold_map_int_inner_map," + dsType
							+ "-java," + /* ARGS is empty */"," + numerThreads
							+ "," + numInsertions + "," + numHotKey + ","
							+ hotRatio + "," + minTime + "," + medianTime + ","
							+ maxTime;
					break;
				}
				Util.writeLine(writer, outDataLine);
			}
		}
		writer.close();
	}

	public static void recordTimeTaken(
			HashMap<String, TreeMap<Integer, ArrayList<Long>>> performanceData,
			ArrayList<Long> timeTakenForRounds, String mapConfig,
			int numOfThreads) {
		performanceData.putIfAbsent(mapConfig,
				new TreeMap<Integer, ArrayList<Long>>());
		performanceData.get(mapConfig).put(new Integer(numOfThreads),
				timeTakenForRounds);
	}

	private static BufferedWriter initiaLizeOutputFile(String benchMarkType) {

		BufferedWriter writer = null;
		String outputFileName = "";
		switch (benchMarkType) {
		case "simple":
			outputFileName = System.currentTimeMillis()
					+ "_simple_insertion.csv";
			break;
		case "random":
			outputFileName = System.currentTimeMillis()
					+ "_random_hot_cold_key.csv";
			break;
		}

		try {
			writer = new BufferedWriter(
					new FileWriter(new File(outputFileName)));
		} catch (IOException e) {
			e.printStackTrace();
		}

		try {

			switch (benchMarkType) {
			case "random":
				Util.writeLine(
						writer,
						"PROGNAME,VARIANT,ARGS,THREADS,NUM_INSERTS,NUM_HOTKEYS,HOT_RATIO,MINTIME,MEDIANTIME,MAXTIME");
				break;
			case "simple":
				Util.writeLine(writer,
						"PROGNAME,VARIANT,ARGS,THREADS,NUM_INSERTS,MINTIME,MEDIANTIME,MAXTIME,THREADS");
				break;
			}
		} catch (IOException e) {
			e.printStackTrace();
		}

		return writer;
	}

	public static void inputFormatError(String benchMarkType) {
		int i = 0;

		switch (benchMarkType) {
		case "random":

			System.out
					.println("Please enter the folowing input data:\n"
							+ i++
							+ "-Data structure to bebenchmarked (pure|scalable|hybrid|JavaConc)\n"
							+ i++
							+ "-Number of insertions\n"
							+ i++
							+ "-Number of hot keys\n"
							+ i++
							+ "-Number of run repetitions\n"
							+ i++
							+ "-Maximum number of threads\n"
							+ i++
							+ "-Probablity of hot key operation (double in range [0-1])\n"
							+ "Output will be put in \"random_hot_cold_key_<DS Type>_<Probablity of cold keys>_<Number of run insertions>_<Hot key percentage>_<Maximum number of threads>.csv\"");
			break;

		case "simple":
			System.out
					.println("Please enter the folowing input data:\n"
							+ i++
							+ "-Data structure to bebenchmarked (pure|scalable|hybrid|JavaSynch|JavaConc)\n"
							+ i++
							+ "-Number of insertions\n"
							+ i++
							+ "-Number of run repetitions\n"
							+ i++
							+ "-Maximum number of threads\n"
							+ " Output will be put in "
							+ "\"simple_insertion_<DS Type>_<Number of insertions>_<Maximum number of threads>.csv\"");
			break;
		}
	}

}
