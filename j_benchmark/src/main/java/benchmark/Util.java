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

	public static final String RANDOM_HOT_COLD = "RANDOM_HOT_COLD";
	public static final String SIMPLE_INSERTION = "SIMPLE_INSERTION";

	public static void writeLine(BufferedWriter writer, String line)
			throws IOException {

		writer.write(line);
		writer.newLine();
	}

	public static Integer getNextHotKey(Random hotKeyGen,
			int hotKeyRangeStartsAt, int hotKeyRangeEndsAt,
			double hotKeyPercentage) {

		int hotKeyRangeLength = (hotKeyRangeEndsAt - hotKeyRangeStartsAt);
		int numberOfHotKeys = (int) (((double) hotKeyRangeLength / (double) 100) * hotKeyPercentage);
		int hotKeysInterval = (int) (100 / hotKeyPercentage);
		return new Integer(
				(int) (hotKeyRangeStartsAt + hotKeyGen.nextInt(numberOfHotKeys)
						* hotKeysInterval));
	}

	public static boolean isHotKey(int key, int hotKeyRangeStartsAt,
			int hotKeyRangeEndsAt, double hotKeyPercentage) {

		int hotKeyRangeLength = (hotKeyRangeEndsAt - hotKeyRangeStartsAt);
		int hotKeysInterval = (int) (100 / hotKeyPercentage);
		for (int i = hotKeyRangeStartsAt; i < hotKeyRangeEndsAt; i += hotKeysInterval) {
			if (key == i) {
				return true;
			}
		}
		return false;
	}

	public static void writePerfData(
			HashMap<String, TreeMap<Integer, ArrayList<Long>>> performanceData,
			String benchMarkType, String dsType, int numofInsertions,
			double hotKeyPercentage, double coldKeyProbability)
			throws IOException {

		BufferedWriter writer = initiaLizeOutputFile(benchMarkType);

		Integer numerOfThreads;
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
				numerOfThreads = numberOfThreadsITR.next();
				timeTaken = perfDataPerMapType.get(numerOfThreads);
				Collections.sort(timeTaken);
				minTime = timeTaken.get(0);
				maxTime = timeTaken.get(timeTaken.size() - 1);
				medianTime = ((timeTaken.size() % 2 == 1) ? timeTaken
						.get((timeTaken.size() / 2)) : (timeTaken
						.get((timeTaken.size() / 2) - 1) + timeTaken
						.get((timeTaken.size() / 2))) / 2);
				String outDataLine = null;
				switch (benchMarkType) {
				case "random":
					outDataLine = "RANDOM_INSERTION,JAVA,-" + dsType
							+ " -#inserts " + numofInsertions
							+ " -hotKeyPercentage " + hotKeyPercentage
							+ " -coldKeyProbability " + coldKeyProbability;
					break;
				case "simple":
					outDataLine = "RANDOM_INSERTION,JAVA,-" + dsType
							+ " -#inserts " + numofInsertions;
					break;
				}
				Util.writeLine(writer, outDataLine + "," + minTime + ","
						+ medianTime + "," + maxTime + "," + numerOfThreads);
			}
		}
		writer.close();
	}

	private static BufferedWriter initiaLizeOutputFile(String benchMarkType) {

		BufferedWriter writer = null;
		String outputFileName = "";
		switch (benchMarkType) {
		case "simple":
			outputFileName = "simple_insertion.csv";
			break;
		case "random":
			outputFileName = "random_hot_cold_key.csv";
			break;
		}

		boolean append = true;
		if (!new File(outputFileName).exists()) {
			append = false;
		}
		try {
			writer = new BufferedWriter(new FileWriter(
					new File(outputFileName), append));
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		if (!append) {
			try {
				Util.writeLine(writer,
						"PROGNAME,VARIANT,ARGS,MINTIME,MEDIANTIME,MAXTIME,THREADS");
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
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
							+ "-Hot key percentage over key range from 0 to the number of insertions (double in range [0-100])\n"
							+ i++
							+ "-Number of run repetitions\n"
							+ i++
							+ "-Maximum number of threads\n"
							+ i++
							+ "-Probablity of cold key operation (double in range [0-1])\n"
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
