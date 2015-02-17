package benchmark;

import java.io.BufferedWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Random;

public class Util {

	public static final String SYNCHRONIZED_MAP = "SYNCHRONIZED_HASH_MAP";
	public static final String CONCURRENT_MAP = "CONCURRENT_HASH_MAP";
	public static final String SKIP_LIST_MAP = "SKIP_LIST_MAP";
	public static final String HYBRID_MAP = "HYBRID";
	public static final String MUTABLE_INT_TREE_MAP = "MUTABLE_INT_TREE_MAP";

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
		return new Integer(
				(int) (hotKeyRangeStartsAt + hotKeyGen.nextInt(numberOfHotKeys)
						* (100 / hotKeyPercentage)));
	}

	public static void main(String[] args) {

		Random hotKeyGen = new Random();
		HashMap<Integer, Integer> hotKeys = new HashMap<Integer, Integer>();
		Integer nextHotKey;
		for (int i = 0; i < 10000; i++) {
			nextHotKey = Util.getNextHotKey(hotKeyGen, 0, 1000000, 0.001);
			hotKeys.putIfAbsent(nextHotKey, new Integer(0));
			hotKeys.replace(nextHotKey, new Integer(hotKeys.get(nextHotKey)
					.intValue() + 1));
		}

		Iterator<Integer> itr = hotKeys.keySet().iterator();
		while (itr.hasNext()) {
			nextHotKey = itr.next();
			System.out.println(nextHotKey + " >>> " + hotKeys.get(nextHotKey));
		}
	}
}
