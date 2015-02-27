package benchmark;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
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

	private static int casTries;

	public static int getCasTries() {
		return casTries;
	}

	public static void setCasTries(int casTries) {
		Util.casTries = casTries;
	}

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
			double numHotKey, double hotRatio, long runStartTimestamp,
			int casTries) throws IOException {

		BufferedWriter writer = initiaLizeOutputFile(benchMarkType,
				runStartTimestamp);

		ArrayList<Long> timeTaken;
		long medianTime, maxTime, minTime;
		TreeMap<Integer, ArrayList<Long>> perfDataPerMapType;

		for (String mapType : performanceData.keySet()) {

			perfDataPerMapType = performanceData.get(mapType);
			for (Integer numerThreads : perfDataPerMapType.keySet()) {
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
					outDataLine = "simple_insertion_int_to_int," + dsType
							+ "-java," + /* ARGS is empty */"," + numerThreads
							+ "," + numInsertions + "," + casTries + ","
							+ (double) minTime / 1000 + ","
							+ (double) medianTime / 1000 + ","
							+ (double) maxTime / 1000;
					break;
				case "random":
					outDataLine = "hotcold_int_to_inner_map," + dsType
							+ "-java," + /* ARGS is empty */"," + numerThreads
							+ "," + numInsertions + "," + numHotKey + ","
							+ hotRatio + "," + casTries + ","
							+ (double) minTime / 1000 + ","
							+ (double) medianTime / 1000 + ","
							+ (double) maxTime / 1000;
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

	private static BufferedWriter initiaLizeOutputFile(String benchMarkType,
			long runStartTimestamp) {

		BufferedWriter writer = null;
		String outputFileName = "";
		switch (benchMarkType) {
		case "simple":
			outputFileName = runStartTimestamp + "_simple_insertion.csv";
			break;
		case "random":
			outputFileName = runStartTimestamp + "_random_hot_cold_key.csv";
			break;
		}

		boolean append = false;
		if (new File(outputFileName).exists()) {
			append = true;
		}

		try {
			writer = new BufferedWriter(new FileWriter(
					new File(outputFileName), append));
		} catch (IOException e) {
			e.printStackTrace();
		}

		try {

			if (!append) {
				switch (benchMarkType) {
				case "random":
					Util.writeLine(
							writer,
							"PROGNAME,VARIANT,ARGS,THREADS,NUM_INSERTS,NUM_HOTKEYS,HOT_RATIO,CAS_TRIES,MINTIME,MEDIANTIME,MAXTIME");
					break;
				case "simple":
					Util.writeLine(
							writer,
							"PROGNAME,VARIANT,ARGS,THREADS,NUM_INSERTS,CAS_TRIES,MINTIME,MEDIANTIME,MAXTIME");
					break;
				}
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
							+ i++
							+ "-Cas tries\n"
							+ i++
							+ "-Current time in milliseconds\n"
							+ " Output will be put in "
							+ "\"<Current time in milliseconds>_random_hot_cold_key.csv\"");
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
							+ i++
							+ "-Current time in milliseconds\n"
							+ " Output will be put in "
							+ "\"<Current time in milliseconds>_simple_insertion.csv\"");
			break;
		}
	}
}
