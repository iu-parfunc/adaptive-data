package benchmark;

import java.io.BufferedWriter;
import java.io.IOException;
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
}
